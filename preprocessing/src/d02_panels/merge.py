import pandas as pd
import numpy as np
from pathlib import Path
from glob import glob
from tqdm import tqdm

# ---------- PATHS ----------
DATA_ROOT            = Path(__file__).resolve().parents[2] / "data"
TRTH_DIR             = DATA_ROOT / "02_preprocessed" / "trth"
MARKETS              = DATA_ROOT / "01_raw" / "handcoded" / "markets.csv"
SAMPLE               = DATA_ROOT / "02_preprocessed" / "sample.csv"
OUT_INTRADAY         = DATA_ROOT / "03_output" / "intraday.csv"
OUT_DAILY            = DATA_ROOT / "03_output" / "daily.csv"
OUT_INTRADAY_RUSUKR  = DATA_ROOT / "03_output" / "intraday_RUS_UKR.csv"
OUT_INTRADAY_AERODEF = DATA_ROOT / "03_output" / "intraday_AERO_DEF.csv"
OUT_DAILY_RUSUKR     = DATA_ROOT / "03_output" / "daily_RUS_UKR.csv"
OUT_DAILY_AERODEF    = DATA_ROOT / "03_output" / "daily_AERO_DEF.csv"

# ---------- READ MARKETS LEGEND ----------
markets = pd.read_csv(MARKETS)
markets["suffix"] = markets["suffix"].astype(str).str.strip().replace("nan", "")

for c in ["open_local", "close_local", "open_gmt", "close_gmt"]:
    markets[c] = pd.to_datetime(markets[c], format="%H:%M", errors="coerce").dt.time

holiday_cols = [c for c in markets.columns if c.startswith("holiday")]
markets_long = markets.melt(id_vars=["suffix"], value_vars=holiday_cols,
                            var_name="holiday_idx", value_name="holiday")
markets_holidays = (
    markets_long.dropna(subset=["holiday"])
    .groupby("suffix")["holiday"].apply(set)
    .to_dict()
)

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

# ---------- PROCESSOR ----------
def process_trth_file(file_path: Path) -> pd.DataFrame:
    df = pd.read_csv(file_path, low_memory=False)
    if {"ric", "datetime", "gmt"} - set(df.columns):
        return pd.DataFrame()

    # suffix from ric
    df["suffix"] = df["ric"].astype(str).str.extract(r"\.([A-Za-z0-9]+)$")[0].fillna("NY")

    # core datetime handling
    df = df.sort_values(["ric", "datetime"], kind="mergesort").reset_index(drop=True)
    df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
    df["gmt"] = pd.to_numeric(df["gmt"], errors="coerce")
    df["local_datetime"] = df["datetime"] + pd.to_timedelta(df["gmt"], unit="h")

    # keep explicit components
    df["date"]        = df["datetime"].dt.date
    df["time"]        = df["datetime"].dt.time
    df["local_date"]  = df["local_datetime"].dt.date.astype(str)
    df["local_time"]  = df["local_datetime"].dt.time

    # merge markets & sort
    merged = df.merge(markets, how="left", on="suffix", validate="m:1")
    merged = merged.sort_values(["ric", "local_datetime"], kind="mergesort").reset_index(drop=True)

    print(f"[{file_path.name}] Before dropping missing hours: {len(merged):,}")
    merged = merged.dropna(subset=["open_local", "close_local"])
    print(f"[{file_path.name}] After  dropping missing hours: {len(merged):,}")

    # trading-hours filter
    merged = merged.loc[
        merged.apply(lambda r:
            pd.notna(r["open_local"]) and pd.notna(r["local_time"]) and
            (r["open_local"] < r["local_time"] <= r["close_local"]),
            axis=1
        )
    ]
    print(f"[{file_path.name}] After trading-hours filter: {len(merged):,}")

    # weekend/holiday filters
    weekend_index = build_weekend_index(merged["local_datetime"])
    if weekend_index:
        merged = merged.loc[~merged["local_date"].isin(weekend_index)]
    merged = merged.loc[
        ~merged["local_date"].isin(
            merged["suffix"].map(markets_holidays).apply(lambda s: s or set())
        )
    ]

    # derived metrics
    if {"price_mean","volume_mean","volume_sum"} <= set(merged.columns):
        merged["dvolume_mean"] = merged["price_mean"] * merged["volume_mean"]
        merged["dvolume_sum"]      = merged["price_mean"] * merged["volume_sum"]
    else:
        merged["dvolume_mean"] = np.nan
        merged["dvolume_sum"]  = np.nan

    if {"espread_mean","priceimpact_mean"} <= set(merged.columns):
        merged["rspread_mean"] = merged["espread_mean"] - merged["priceimpact_mean"]
    if {"espread2_mean","priceimpact2_mean"} <= set(merged.columns):
        merged["rspread2_mean"] = merged["espread2_mean"] - merged["priceimpact2_mean"]
    if {"w_espread2_mean","w_priceimpact2_mean"} <= set(merged.columns):
        merged["w_rspread2_mean"] = merged["w_espread2_mean"] - merged["w_priceimpact2_mean"]

    # final sort
    merged = merged.sort_values(["ric", "datetime"], kind="mergesort").reset_index(drop=True)
    return merged

# ---------- MAIN PIPELINE ----------
files = sorted(glob(str(TRTH_DIR / "ukraine*.csv")))
if not files:
    raise FileNotFoundError(f"No TRTH CSV files found in {TRTH_DIR}")

cleaned_parts = []
for f in tqdm(files, desc="Processing TRTH files", unit="file"):
    part = process_trth_file(Path(f))
    if not part.empty:
        cleaned_parts.append(part)

merged_all = pd.concat(cleaned_parts, ignore_index=True) if cleaned_parts else pd.DataFrame()
merged_all["date"] = pd.to_datetime(merged_all["datetime"], errors="coerce").dt.date
merged_all = merged_all.sort_values(["ric", "date"], kind="mergesort").reset_index(drop=True)
print(f"\n✓ Processed {len(files)} files → {len(merged_all):,} rows.")

rics_before_merge = merged_all["ric"].nunique()

# ---------- MERGE WITH SAMPLE ----------
sample = pd.read_csv(SAMPLE)
if "date" in sample.columns:
    sample["date"] = pd.to_datetime(sample["date"], errors="coerce").dt.date

print(f"Before merging with sample: {len(merged_all):,}")
final_all = merged_all.merge(sample, how="inner", on=["ric","date"], validate="m:1")
final_all = final_all.sort_values(["ric","datetime"], kind="mergesort").reset_index(drop=True)
print(f"After merging with sample: {len(final_all):,}")

rics_after_merge  = final_all["ric"].nunique()

# --- Drop NXX ---
before_nxx = len(final_all)
final_all = final_all.loc[final_all["suffix"] != "NXX"].copy()
print(f"Dropped NXX suffix rows: {before_nxx - len(final_all):,} (from {before_nxx:,} → {len(final_all):,})")

rics_after_nxx    = final_all["ric"].nunique()

# --- Drop price<=1 ---
if "price" in final_all.columns:
    final_all["price"] = (
        final_all.groupby("ric", group_keys=False)["price"]
        .apply(lambda s: pd.to_numeric(s, errors="coerce"))
    )
    final_all = final_all[pd.to_numeric(final_all["price"], errors="coerce") > 1].copy()
final_all = final_all[pd.to_numeric(final_all["price_mean"], errors="coerce") > 1].copy()
print(f"After deleting price<=1: {len(final_all):,}")

rics_after_price_drop    = final_all["ric"].nunique()

# --- Split RUS/UKR & Areospce/Defence Industry---
if "indm" in final_all.columns:
    aerodef   = final_all[final_all["indm"].isin(["Aerospace","Defense"])].copy()
    final_all = final_all[~final_all["indm"].isin(["Aerospace","Defense"])].copy()
else:
    aerodef   = pd.DataFrame(columns=final_all.columns)
    final_all = final_all.copy()

rics_after_aerodef  = final_all["ric"].nunique()

if "ctriso3" in final_all.columns:
    rusukr  = final_all[final_all["ctriso3"].isin(["RUS","UKR"])].copy()
    final   = final_all[~final_all["ctriso3"].isin(["RUS","UKR"])].copy()
else:
    rusukr  = pd.DataFrame(columns=final_all.columns)
    final   = final_all.copy()

rics_after_rusukr  = final["ric"].nunique()

# --- Unique RIC summary ---
print("\n--- UNIQUE RIC SUMMARY ---")
print(f"Before merging:  {rics_before_merge:,}")
print(f"After merging:   {rics_after_merge:,}")
print(f"After removing NXX: {rics_after_nxx:,}")
print(f"After removing price<=1: {rics_after_price_drop:,}")
print(f"After removing Aerospace/Defense: {rics_after_aerodef:,}")
print(f"After removing RUS/UKR: {rics_after_rusukr:,}")
print("---------------------------\n")

# --- Filter trade/quote counts ---
if {"trades_count","quotes_count"} <= set(final.columns):
    before = len(final)
    final = final.loc[~((final["trades_count"]==0) & (final["quotes_count"]==1))].copy()
    print(f"Removed {before - len(final):,} rows (from {before:,} → {len(final):,}) after trade/quote filter.")

# ---------- SAVE INTRADAY ----------
OUT_INTRADAY.parent.mkdir(parents=True, exist_ok=True)
final.to_csv(OUT_INTRADAY, index=False)
print(f"✅ Saved intraday (except RUS/UKR & Aerospace/Defense) → {OUT_INTRADAY} ({len(final):,} rows)")
if not aerodef.empty:
    aerodef.to_csv(OUT_INTRADAY_AERODEF, index=False)
    print(f"✅ Saved Aerospace/Defense intraday → {OUT_INTRADAY_AERODEF} ({len(aerodef):,} rows)")
if not rusukr.empty:
    rusukr.to_csv(OUT_INTRADAY_RUSUKR, index=False)
    print(f"✅ Saved RUS/UKR intraday → {OUT_INTRADAY_RUSUKR} ({len(rusukr):,} rows)")

# ---------- DAILY AGGREGATION ----------
def aggregate_daily(df, out_path, label):
    df = df.copy()
    df["date"] = df["datetime"].dt.date
    num_cols = df.select_dtypes(include=[np.number]).columns
    sum_cols = [c for c in ["dollar_volume_sum","volume_sum","trades_count","quotes_count",
                            "depth_sum","depth_value_sum","dvolume"] if c in df.columns]
    avg_cols = [c for c in num_cols if c not in sum_cols]
    agg_spec = {c:"mean" for c in avg_cols}
    agg_spec.update({c:"sum" for c in sum_cols})
    agg_spec["datetime"] = "last"  
    obj_cols = df.select_dtypes(include=["object"]).columns
    for c in obj_cols:
        if c not in ["ric"]:
            agg_spec[c] = "last"
    daily = df.groupby(["ric","date"], as_index=False).agg(agg_spec)
    daily = daily.sort_values(["ric","date"], kind="mergesort").reset_index(drop=True)
    daily.to_csv(out_path, index=False)
    print(f"✅ Saved daily {label} → {out_path} ({len(daily):,} rows)")

if not final.empty:
    aggregate_daily(final, OUT_DAILY, "(except RUS/UKR & Aerospace/Defense)")
if not aerodef.empty:
    aggregate_daily(aerodef, OUT_DAILY_AERODEF, "Aerospace/Defense")
if not rusukr.empty:
    aggregate_daily(rusukr, OUT_DAILY_RUSUKR, "RUS/UKR")