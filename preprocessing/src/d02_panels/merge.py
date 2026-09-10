"""Merge the TRTH 5-minute metrics with the Datastream sample and write the
analysis-ready panels as Parquet.

Memory-safe by construction: TRTH files are processed one at a time (2-4 M rows
each) and appended to the output Parquet files; the daily panels are then
aggregated from the 5-minute Parquet with polars in streaming mode. Nothing is
ever held in RAM for the full 43 M-row panel.
"""
import datetime as _dt
import os
from glob import glob
from pathlib import Path

import numpy as np
import pandas as pd
import polars as pl
import pyarrow as pa
import pyarrow.parquet as pq
from tqdm import tqdm

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[3] / "analysis" / "src" / "d00_preprocessing"))
import sampling_log as slog  # noqa: E402

# ---------- PATHS ----------
DATA_ROOT            = Path(os.environ.get("GEO_DATA_ROOT", Path(__file__).resolve().parents[2] / "data"))
TRTH_DIR             = DATA_ROOT / "02_preprocessed" / "trth"
MARKETS              = DATA_ROOT / "01_raw" / "handcoded" / "markets.csv"
SAMPLE               = DATA_ROOT / "02_preprocessed" / "sample.csv"
OUT_DIR              = DATA_ROOT / "03_output"
OUT_INTRADAY         = OUT_DIR / "intraday.parquet"
OUT_DAILY            = OUT_DIR / "daily.parquet"
OUT_INTRADAY_RUSUKR  = OUT_DIR / "intraday_RUS_UKR.parquet"
OUT_INTRADAY_AERODEF = OUT_DIR / "intraday_AERO_DEF.parquet"
OUT_DAILY_RUSUKR     = OUT_DIR / "daily_RUS_UKR.parquet"
OUT_DAILY_AERODEF    = OUT_DIR / "daily_AERO_DEF.parquet"
OUT_DIR.mkdir(parents=True, exist_ok=True)

# measures retired from the analysis (still present in older TRTH files, dropped here):
# undirected/x2/weighted price impacts and their realized spreads, the signed
# effective spreads, the non-same-day JFE pair, and the price-level volatilities
DROP_COLS = ["priceimpact_mean", "priceimpact2_mean", "w_priceimpact2_mean",
             "rspread_mean", "rspread2_mean", "w_rspread2_mean",
             "espread2_mean", "w_espread2_mean",
             "pi_jfe_mean", "rspread_jfe_mean", "rspread_jfe_sd_mean",
             "intraday_vol_mean", "intraday_5m_vol_mean", "price_std"]
# paper names for the measures of TRTH files produced before 10 Sep 2026 (no-op on newer files)
RENAME = {"pi_jfe_sd_mean": "price_impact_mean", "pi_jfe_sd_fi_mean": "price_impact_fi_mean",
          "vol_jfe": "volatility", "vol_jfel": "log_volatility"}
# TRTH prices are in local currency (pence in London, cents in Johannesburg): these
# value measures are converted to USD with the firm-day rate implied by the
# Datastream USD close (see merge_with_sample_and_filter)
USD_COLS = ["dollar_volume_mean", "dollar_volume_sum", "dvolume_mean", "dvolume_sum",
            "depth_value_mean", "depth_value_sum"]
FX_DIAGNOSTICS = Path(__file__).resolve().parents[3] / "analysis" / "output" / "tables" / "fx_diagnostics.csv"

# daily aggregation: these are summed over the day, everything else numeric is averaged
DAILY_SUM_COLS = ["dollar_volume_sum", "volume_sum", "trades_count", "quotes_count",
                  "depth_sum", "depth_value_sum", "dvolume_sum"]

# ---------- READ MARKETS LEGEND ----------
markets = pd.read_csv(MARKETS)
markets["suffix"] = markets["suffix"].astype(str).str.strip().replace("nan", "")

for c in ["open_local", "close_local", "open_gmt", "close_gmt"]:
    parsed = pd.to_datetime(markets[c], format="%H:%M", errors="coerce")
    if c in ("open_local", "close_local"):
        markets[c + "_sec"] = parsed.dt.hour * 3600 + parsed.dt.minute * 60
    markets[c] = parsed.dt.time

holiday_cols = [c for c in markets.columns if c.startswith("holiday")]
markets_long = markets.melt(id_vars=["suffix"], value_vars=holiday_cols,
                            var_name="holiday_idx", value_name="holiday")
# holidays are hand-coded as DD.MM.YY; normalize to the ISO format used by local_date
# (blank/whitespace-only cells are empty, not failures)
holiday_str = markets_long["holiday"].str.strip().replace("", pd.NA)
holiday_parsed = pd.to_datetime(holiday_str, format="%d.%m.%y", errors="coerce")
n_bad_holidays = int((holiday_str.notna() & holiday_parsed.isna()).sum())
if n_bad_holidays:
    print(f"⚠️ {n_bad_holidays} non-empty holiday entries in markets.csv did not parse as DD.MM.YY and are ignored.")
holiday_keys = set(
    (markets_long["suffix"] + "|" + holiday_parsed.dt.strftime("%Y-%m-%d")).dropna()
)

# ---------- SAMPLE (Datastream, local trading dates, USD) ----------
sample = pd.read_csv(SAMPLE)
sample["date"] = pd.to_datetime(sample["date"], errors="coerce").dt.date


# row counts of the 5-minute panel filters, accumulated over TRTH files
TOTALS = {"rows_in": 0, "rows_no_hours": 0, "rows_after_hours": 0, "rows_weekend": 0, "rows_holiday": 0}


# ---------- HELPERS ----------
def build_weekend_index(local_dates_series: pd.Series) -> set:
    if local_dates_series.empty:
        return set()
    dt = pd.to_datetime(local_dates_series, errors="coerce").dropna()
    if dt.empty:
        return set()
    rng = pd.date_range(dt.min().normalize(), dt.max().normalize(), freq="D")
    weekend = rng[rng.dayofweek >= 5]
    return set(weekend.strftime("%Y-%m-%d"))


def to_arrow(df: pd.DataFrame) -> pa.Table:
    """Object columns of mixed python types (e.g. zip codes that are ints for
    some markets and strings for others) become nullable strings; dates, times
    and plain strings pass through."""
    df = df.copy()
    for c in df.columns[df.dtypes == object]:
        types = set(map(type, df[c].dropna().head(2000)))
        if types and not (types <= {str} or types <= {_dt.date} or types <= {_dt.time}):
            df[c] = df[c].astype("string")
    return pa.Table.from_pandas(df, preserve_index=False)


class ParquetAppender:
    """Append pandas chunks to one Parquet file with a schema fixed by the
    first chunk. Inferred types drift across files (e.g. gmt is integral in a
    US-only file but 5.5 for India), so integers are promoted to float64 and
    null-typed columns to string before fixing the schema."""
    def __init__(self, path: Path):
        self.path, self.writer, self.schema, self.rows = path, None, None, 0

    @staticmethod
    def _canonical(schema: pa.Schema) -> pa.Schema:
        fields = []
        for f in schema:
            t = f.type
            if pa.types.is_integer(t):
                t = pa.float64()
            elif pa.types.is_null(t):
                t = pa.string()
            fields.append(pa.field(f.name, t))
        return pa.schema(fields)

    def append(self, df: pd.DataFrame):
        if df.empty:
            return
        table = to_arrow(df)
        if self.writer is None:
            self.schema = self._canonical(table.schema)
            self.writer = pq.ParquetWriter(str(self.path), self.schema, compression="snappy")
        table = table.select(self.schema.names).cast(self.schema)
        # small row groups let downstream polars scans stream instead of decoding
        # a whole 4M-row TRTH file at once
        self.writer.write_table(table, row_group_size=250_000)
        self.rows += table.num_rows

    def close(self):
        if self.writer is not None:
            self.writer.close()


# ---------- PROCESSOR (one TRTH file) ----------
def process_trth_file(file_path: Path) -> pd.DataFrame:
    if file_path.suffix == ".parquet":
        df = pd.read_parquet(file_path)
    else:
        df = pd.read_csv(file_path, low_memory=False)
    if {"ric", "datetime", "gmt"} - set(df.columns):
        return pd.DataFrame()
    df = df.rename(columns={k: v for k, v in RENAME.items() if k in df.columns and v not in df.columns})

    # suffix from ric
    df["suffix"] = df["ric"].astype(str).str.extract(r"\.([A-Za-z0-9]+)$")[0].fillna("NY")

    # core datetime handling
    df = df.sort_values(["ric", "datetime"], kind="mergesort").reset_index(drop=True)
    df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
    df["gmt"] = pd.to_numeric(df["gmt"], errors="coerce")
    df["local_datetime"] = df["datetime"] + pd.to_timedelta(df["gmt"], unit="h")

    # keep explicit components; date/time are the LOCAL trading day and time
    # (Datastream dates are local trading days; UTC dates split APAC sessions at UTC midnight)
    df["date"]        = df["local_datetime"].dt.date
    df["time"]        = df["local_datetime"].dt.time
    df["local_date"]  = df["local_datetime"].dt.date.astype(str)
    df["local_time"]  = df["local_datetime"].dt.time

    # merge markets & sort
    merged = df.merge(markets, how="left", on="suffix", validate="m:1")
    merged = merged.sort_values(["ric", "local_datetime"], kind="mergesort").reset_index(drop=True)

    print(f"[{file_path.name}] Before dropping missing hours: {len(merged):,}")
    n_in = len(merged)
    TOTALS["rows_in"] += n_in
    merged = merged.dropna(subset=["open_local", "close_local"])
    TOTALS["rows_no_hours"] += n_in - len(merged)
    print(f"[{file_path.name}] After  dropping missing hours: {len(merged):,}")

    # trading-hours filter: open < t <= close, in seconds since local midnight (NaN rows drop out)
    local_sec = (merged["local_datetime"] - merged["local_datetime"].dt.normalize()).dt.total_seconds()
    merged = merged.loc[(local_sec > merged["open_local_sec"]) & (local_sec <= merged["close_local_sec"])]
    TOTALS["rows_after_hours"] += len(merged)
    print(f"[{file_path.name}] After trading-hours filter: {len(merged):,}")

    # weekend/holiday filters
    weekend_index = build_weekend_index(merged["local_datetime"])
    if weekend_index:
        n_w = len(merged)
        merged = merged.loc[~merged["local_date"].isin(weekend_index)]
        TOTALS["rows_weekend"] += n_w - len(merged)
    before_holiday = len(merged)
    holiday_row = (merged["suffix"] + "|" + merged["local_date"]).isin(holiday_keys)
    merged = merged.loc[~holiday_row]
    TOTALS["rows_holiday"] += before_holiday - len(merged)
    print(f"[{file_path.name}] Holiday rows removed: {before_holiday - len(merged):,}")

    # derived metrics
    if {"price_mean", "volume_mean", "volume_sum"} <= set(merged.columns):
        merged["dvolume_mean"] = merged["price_mean"] * merged["volume_mean"]
        merged["dvolume_sum"]  = merged["price_mean"] * merged["volume_sum"]
    else:
        merged["dvolume_mean"] = np.nan
        merged["dvolume_sum"]  = np.nan

    # realized spread = effective half-spread minus the directional price impact
    # (Lee-Ready direction; _fi = Jurkatis full-information direction)
    for sfx in ("", "_fi"):
        if {"espread_mean", f"price_impact{sfx}_mean"} <= set(merged.columns):
            merged[f"realized_spread{sfx}_mean"] = merged["espread_mean"] - merged[f"price_impact{sfx}_mean"]

    merged = merged.drop(columns=[c for c in DROP_COLS if c in merged.columns])
    merged = merged.sort_values(["ric", "datetime"], kind="mergesort").reset_index(drop=True)
    return merged


def merge_with_sample_and_filter(part: pd.DataFrame, counts: dict) -> pd.DataFrame:
    """Inner-join with the Datastream sample on (ric, local date), then the
    sample screens. `counts` accumulates the unique RICs at each step."""
    counts["before_merge"].update(part["ric"].unique())
    part = part.merge(sample, how="inner", on=["ric", "date"], validate="m:1")
    counts["after_merge"].update(part["ric"].unique())

    part = part.loc[part["suffix"] != "NXX"]
    counts["after_nxx"].update(part["ric"].unique())

    # penny stocks: Datastream USD price at or below $1 (the local price is not a
    # comparable threshold across currencies)
    part = part[pd.to_numeric(part["price"], errors="coerce") > 1]
    counts["after_price"].update(part["ric"].unique())

    # USD conversion: local units per USD for each firm-day = local close (last
    # 5-minute bucket with trades; `price_close` once the TRTH files carry it) over
    # the Datastream USD close of the same day. Firm-days without trades take the
    # firm's rate on the nearest day, then the median of the market-day.
    close_col = "price_close" if "price_close" in part.columns else "price_mean"
    fd = part.groupby(["ric", "date"], sort=True).agg(suffix=("suffix", "first"), price=("price", "first")).reset_index()
    traded = part.loc[pd.to_numeric(part["trades_count"], errors="coerce") > 0]
    close = (traded.sort_values(["ric", "date", "datetime"], kind="mergesort")
                   .groupby(["ric", "date"])[close_col].last().rename("close").reset_index())
    fd = fd.merge(close, on=["ric", "date"], how="left")
    fd["fx"] = pd.to_numeric(fd["close"], errors="coerce") / pd.to_numeric(fd["price"], errors="coerce")
    fd["fx_source"] = np.where(fd["fx"].notna(), "firm_close", "")
    nearest = fd.groupby("ric")["fx"].transform(lambda x: x.ffill().bfill())
    fd.loc[fd["fx"].isna() & nearest.notna(), "fx_source"] = "firm_nearest_day"
    fd["fx"] = nearest
    market = fd.groupby(["suffix", "date"])["fx"].transform("median")
    fd.loc[fd["fx"].isna() & market.notna(), "fx_source"] = "market_median"
    fd["fx"] = fd["fx"].fillna(market)
    part = part.merge(fd[["ric", "date", "fx", "fx_source"]].rename(columns={"fx": "fx_local_per_usd"}),
                      on=["ric", "date"], how="left")
    for c in USD_COLS:
        if c in part.columns:
            part[c] = part[c] / part["fx_local_per_usd"]
    return part.sort_values(["ric", "datetime"], kind="mergesort").reset_index(drop=True)


# ---------- MAIN PIPELINE ----------
# prefer the parquet TRTH outputs (trth.py --output-format parquet), else CSV
files = sorted(glob(str(TRTH_DIR / "ukraine*.parquet"))) or sorted(glob(str(TRTH_DIR / "ukraine*.csv")))
if not files:
    raise FileNotFoundError(f"No TRTH parquet/CSV files found in {TRTH_DIR}")
print(f"TRTH inputs: {len(files)} files ({Path(files[0]).suffix})")
slog.reset("merge")

counts = {k: set() for k in ["before_merge", "after_merge", "after_nxx", "after_price",
                             "final", "aerodef", "rusukr"]}
writers = {
    "final":   ParquetAppender(OUT_INTRADAY),
    "aerodef": ParquetAppender(OUT_INTRADAY_AERODEF),
    "rusukr":  ParquetAppender(OUT_INTRADAY_RUSUKR),
}
dropped_tq = 0

for f in tqdm(files, desc="Processing TRTH files", unit="file"):
    part = process_trth_file(Path(f))
    if part.empty:
        continue
    part = merge_with_sample_and_filter(part, counts)
    if part.empty:
        continue

    # --- Split Aerospace/Defense and RUS/UKR ---
    is_aerodef = part["indm"].isin(["Aerospace", "Defense"]) if "indm" in part.columns else pd.Series(False, index=part.index)
    is_rusukr = part["ctriso3"].isin(["RUS", "UKR"]) if "ctriso3" in part.columns else pd.Series(False, index=part.index)
    aerodef = part[is_aerodef]
    rusukr = part[~is_aerodef & is_rusukr]
    final = part[~is_aerodef & ~is_rusukr]

    # --- Filter trade/quote counts on the main sample ---
    if {"trades_count", "quotes_count"} <= set(final.columns):
        keep = ~((final["trades_count"] == 0) & (final["quotes_count"] == 1))
        dropped_tq += int((~keep).sum())
        final = final[keep]

    for name, sub in (("final", final), ("aerodef", aerodef), ("rusukr", rusukr)):
        counts[name].update(sub["ric"].unique())
        writers[name].append(sub)

for w in writers.values():
    w.close()

print("\n--- UNIQUE RIC SUMMARY ---")
print(f"Before merging:  {len(counts['before_merge']):,}")
print(f"After merging:   {len(counts['after_merge']):,}")
print(f"After removing NXX: {len(counts['after_nxx']):,}")
print(f"After removing Datastream price<=$1: {len(counts['after_price']):,}")
print(f"Main sample (excl. Aerospace/Defense, RUS/UKR): {len(counts['final']):,}")
print(f"Aerospace/Defense: {len(counts['aerodef']):,} | RUS/UKR: {len(counts['rusukr']):,}")
print("---------------------------\n")
print(f"Removed {dropped_tq:,} rows with trades_count==0 & quotes_count==1 from the main sample.")
for name, w in writers.items():
    print(f"✅ Saved intraday {name} → {w.path} ({w.rows:,} rows)")

# ---------- sample-selection log ----------
T = TOTALS
slog.log("merge", "5-minute rows from TRTH (ric x local trading day, 19 files)", firms=len(counts["before_merge"]), rows=T["rows_in"])
slog.log("merge", "- rows of venues without trading-hours information", rows=T["rows_no_hours"])
slog.log("merge", "- rows outside local continuous trading hours (open < t <= close)", rows=T["rows_in"] - T["rows_no_hours"] - T["rows_after_hours"])
slog.log("merge", "- rows on local Saturdays/Sundays", rows=T["rows_weekend"])
slog.log("merge", "- rows on local exchange holidays", rows=T["rows_holiday"])
slog.log("merge", "Firms matched to Datastream on (ric, local trading day)", firms=len(counts["after_merge"]))
slog.log("merge", "- firms listed on venue NXX", firms=len(counts["after_merge"]) - len(counts["after_nxx"]))
slog.log("merge", "- firms with Datastream price <= $1 (penny stocks)", firms=len(counts["after_nxx"]) - len(counts["after_price"]))
slog.log("merge", "- Aerospace/Defense firms (separate sample)", firms=len(counts["aerodef"]))
slog.log("merge", "- Russian/Ukrainian firms (separate sample)", firms=len(counts["rusukr"]))
slog.log("merge", "Main 5-minute panel", firms=len(counts["final"]), rows=writers["final"].rows,
         note=f"{dropped_tq:,} rows with trades_count=0 & quotes_count=1 removed")


# ---------- DAILY AGGREGATION (polars, streaming over the 5-minute parquet) ----------
def aggregate_daily(in_path: Path, out_path: Path, label: str):
    if not in_path.exists():
        return
    lf = pl.scan_parquet(in_path)
    schema = lf.collect_schema()
    aggs = []
    for name, dtype in schema.items():
        if name in ("ric", "date"):
            continue
        if name == "datetime":
            aggs.append(pl.col(name).last())
        elif name in DAILY_SUM_COLS:
            aggs.append(pl.col(name).sum())
        elif dtype.is_numeric():
            aggs.append(pl.col(name).mean())
        else:
            aggs.append(pl.col(name).last())
    daily = lf.group_by(["ric", "date"]).agg(aggs).sort(["ric", "date"]).collect(engine="streaming")
    daily.write_parquet(out_path)
    print(f"✅ Saved daily {label} → {out_path} ({daily.height:,} rows)")
    if out_path == OUT_DAILY:
        slog.log("merge", "Main daily panel (5-minute rows aggregated by ric x local trading day)",
                 firms=daily["ric"].n_unique(), rows=daily.height)


aggregate_daily(OUT_INTRADAY, OUT_DAILY, "(except RUS/UKR & Aerospace/Defense)")
aggregate_daily(OUT_INTRADAY_AERODEF, OUT_DAILY_AERODEF, "Aerospace/Defense")
aggregate_daily(OUT_INTRADAY_RUSUKR, OUT_DAILY_RUSUKR, "RUS/UKR")


# ---------- FX DIAGNOSTICS ----------
def fx_diagnostics(daily_path: Path, out_path: Path):
    """Firms whose implied rate deviates from their market's: USD-quoted lines
    (e.g. `...u.TO`), stocks quoted in a different unit than their exchange, or
    Datastream prices adjusted for later corporate actions."""
    if not daily_path.exists():
        return
    d = pd.read_parquet(daily_path, columns=["ric", "date", "fx_local_per_usd", "fx_source"])
    d["suffix"] = d["ric"].astype(str).str.extract(r"\.([A-Za-z0-9]+)$")[0].fillna("NY")
    d["market_fx"] = d.groupby(["suffix", "date"])["fx_local_per_usd"].transform("median")
    g = d.groupby("ric").agg(suffix=("suffix", "first"), n_days=("date", "size"),
                             fx_median=("fx_local_per_usd", "median"), market_fx_median=("market_fx", "median"),
                             share_firm_close=("fx_source", lambda x: float((x == "firm_close").mean())))
    g["deviation"] = g["fx_median"] / g["market_fx_median"] - 1
    g["flag"] = g["deviation"].abs() > 0.20
    out_path.parent.mkdir(parents=True, exist_ok=True)
    g.sort_values("deviation").to_csv(out_path)
    print(f"[fx] firms with |rate / market rate - 1| > 20%: {int(g['flag'].sum())} of {len(g):,} → {out_path}")


fx_diagnostics(OUT_DAILY, FX_DIAGNOSTICS)
