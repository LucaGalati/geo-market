"""Build the analysis panels from the merged TRTH×Datastream output.

Two samples per panel, written as parquet in analysis/data:
- *_main.parquet     : UNBALANCED panel — a firm enters with data on >= MIN_DAYS
                       trading days both before and after the event (max observations,
                       same firms on both sides). This is the main sample.
- *_balanced.parquet : PERFECTLY BALANCED panel — firms observed on every trading day
                       of the union window calendar (robustness; on a global panel
                       this drops markets with local holidays by construction).

The 5-minute panels are processed with polars in streaming mode (never fully in
RAM); the daily panels with pandas.
"""
from datetime import datetime, timezone
from pathlib import Path
import time

import numpy as np
import pandas as pd
import polars as pl

try:
    from . import sampling_log as slog
except ImportError:
    import sampling_log as slog

# ---------- PATHS ----------
ROOT = Path(__file__).resolve().parents[3]
ROOT_PRE = ROOT / "preprocessing" / "data" / "03_output"
ROOT_OUT = ROOT / "analysis" / "data"
ROOT_OUT.mkdir(parents=True, exist_ok=True)

# label -> (input file, output stem, kind)
FILES = {
    "intraday":         (ROOT_PRE / "intraday.parquet",          "intraday",         "intraday"),
    "daily":            (ROOT_PRE / "daily.parquet",             "daily",            "daily"),
    "rusukr_intraday":  (ROOT_PRE / "intraday_RUS_UKR.parquet",  "rusukr_intraday",  "intraday"),
    "aerodef_intraday": (ROOT_PRE / "intraday_AERO_DEF.parquet", "aerodef_intraday", "intraday"),
    "rusukr_daily":     (ROOT_PRE / "daily_RUS_UKR.parquet",     "rusukr_daily",     "daily"),
    "aerodef_daily":    (ROOT_PRE / "daily_AERO_DEF.parquet",    "aerodef_daily",    "daily"),
}

# ---------- TIME WINDOW ----------
CUTOFF = datetime(2022, 2, 24, tzinfo=timezone.utc)
START = datetime(2022, 1, 27, tzinfo=timezone.utc)
END_EXCL = datetime(2022, 3, 24, tzinfo=timezone.utc)  # keeps the WHOLE last day for 5-minute data
MIN_DAYS = 1  # main sample: >= MIN_DAYS trading days pre AND post

# rows must have these non-missing (the mechanism/volatility subsets are a
# dropna at analysis time, not separate files)
REQUIRED_VARS = [
    "qspread_mean", "espread_mean", "trades_count", "dollar_volume_mean",
    "mktval", "price_impact_mean", "realized_spread_mean",
]


# ---------- INTRADAY (polars, streaming) ----------
def process_intraday(path: Path, stem: str, label: str):
    lf = pl.scan_parquet(path, low_memory=True)
    schema = lf.collect_schema().names()
    missing = [c for c in REQUIRED_VARS if c not in schema]
    if missing:
        print(f" [{label}] Missing columns {missing}. Skipping.")
        return

    lf = (
        lf.filter((pl.col("datetime") >= START) & (pl.col("datetime") < END_EXCL))
          .filter(pl.all_horizontal([pl.col(c).is_not_null() for c in REQUIRED_VARS]))
          .with_columns(
              pl.when(pl.col("datetime") >= CUTOFF).then(pl.lit("after"))
                .otherwise(pl.lit("before")).alias("period")
          )
    )

    # trading days per (ric, period) on the LOCAL trading day
    days = (lf.group_by(["ric", "period"])
              .agg(pl.col("date_local").n_unique().alias("n_days"))
              .collect(engine="streaming"))
    if days.height == 0:
        print(f" [{label}] No rows in window.")
        return
    piv = days.pivot(index="ric", on="period", values="n_days").fill_null(0)
    for col in ("before", "after"):
        if col not in piv.columns:
            piv = piv.with_columns(pl.lit(0).alias(col))
    main_rics = piv.filter((pl.col("before") >= MIN_DAYS) & (pl.col("after") >= MIN_DAYS))["ric"]

    total_days = lf.select(pl.col("date_local").n_unique()).collect(engine="streaming").item()
    firm_days = lf.group_by("ric").agg(pl.col("date_local").n_unique().alias("n")).collect(engine="streaming")
    bal_rics = firm_days.filter(pl.col("n") == total_days)["ric"]

    n_all = piv.height
    n_rows_all = lf.select(pl.len()).collect(engine="streaming").item()
    n_firms_in = pl.scan_parquet(path).select(pl.col("ric").n_unique()).collect(engine="streaming").item()
    slog.log("gathering", f"{label}: firms in the merged panel", firms=n_firms_in)
    slog.log("gathering", f"{label}: rows in window 27 Jan-23 Mar 2022 with all required variables non-missing",
             firms=n_all, rows=n_rows_all, note="required: " + ", ".join(REQUIRED_VARS))
    print(f" [{label}] main (>= {MIN_DAYS} trading days pre & post): {len(main_rics):,} / {n_all:,} firms")
    print(f" [{label}] balanced (all {total_days} union trading days): {len(bal_rics):,} / {n_all:,} firms")

    for rics, suffix in ((main_rics, "main"), (bal_rics, "balanced")):
        out = ROOT_OUT / f"{stem}_{suffix}.parquet"
        lf.filter(pl.col("ric").is_in(rics.implode())).sink_parquet(out, engine="streaming")
        n_rows = pl.scan_parquet(out).select(pl.len()).collect().item()
        print(f"✅ Saved {label} | {suffix} → {out} ({n_rows:,} rows)")
        rule = (f">= {MIN_DAYS} trading day(s) with data both before and after 24 Feb 2022" if suffix == "main"
                else f"data on all {total_days} trading days of the union calendar")
        slog.log("gathering", f"{label}: {suffix} sample", firms=len(rics), rows=n_rows, note=rule)


# ---------- DAILY (pandas) ----------
def _period(df, date_col):
    return np.where(df[date_col] >= pd.Timestamp(CUTOFF), "after", "before")


def filter_panel(df, date_col, label, min_days=MIN_DAYS):
    """Main sample: firms with >= min_days trading days BOTH before and after the cutoff."""
    df = df.copy()
    df["period"] = _period(df, date_col)
    days = df.groupby(["ric", "period"], sort=False)[date_col].nunique().unstack(fill_value=0)
    for col in ["before", "after"]:
        if col not in days:
            days[col] = 0
    keep = days.index[(days["before"] >= min_days) & (days["after"] >= min_days)]
    kept = df[df["ric"].isin(keep)].sort_values(["ric", date_col], kind="mergesort").reset_index(drop=True)
    print(f" [{label}] main (>= {min_days} trading days pre & post): {len(keep):,} / {days.shape[0]:,} firms")
    return kept


def perfectly_balanced_panel(df, date_col, label):
    """Robustness sample: firms observed on EVERY trading day of the union window calendar."""
    df = df.copy()
    df["period"] = _period(df, date_col)
    total_days = df[date_col].nunique()
    firm_days = df.groupby("ric")[date_col].nunique()
    keep = firm_days.index[firm_days == total_days]
    kept = df[df["ric"].isin(keep)].sort_values(["ric", date_col], kind="mergesort").reset_index(drop=True)
    print(f" [{label}] balanced (all {total_days} union trading days): {len(keep):,} / {df['ric'].nunique():,} firms")
    return kept


def process_daily(path: Path, stem: str, label: str):
    df = pd.read_parquet(path)
    missing = [c for c in REQUIRED_VARS if c not in df.columns]
    if missing:
        print(f" [{label}] Missing columns {missing}. Skipping.")
        return
    df["date"] = pd.to_datetime(df["date"], errors="coerce", utc=True)
    df = df.dropna(subset=["date"])
    df = df[(df["date"] >= pd.Timestamp(START)) & (df["date"] < pd.Timestamp(END_EXCL))]
    n_firms_in = df["ric"].nunique()
    df = df.dropna(subset=REQUIRED_VARS)
    if df.empty:
        print(f" [{label}] No rows in window.")
        return
    print(f" [{label}] loaded {len(df):,} rows, {df['ric'].nunique():,} firms")
    slog.log("gathering", f"{label}: firms in the merged panel", firms=n_firms_in)
    slog.log("gathering", f"{label}: rows in window 27 Jan-23 Mar 2022 with all required variables non-missing",
             firms=df["ric"].nunique(), rows=len(df), note="required: " + ", ".join(REQUIRED_VARS))

    for build, suffix in ((filter_panel, "main"), (perfectly_balanced_panel, "balanced")):
        out = ROOT_OUT / f"{stem}_{suffix}.parquet"
        panel = build(df, "date", label)
        panel.to_parquet(out, index=False)
        print(f"✅ Saved {label} | {suffix} → {out} ({len(panel):,} rows)")
        rule = (f">= {MIN_DAYS} trading day(s) with data both before and after 24 Feb 2022" if suffix == "main"
                else f"data on all {df['date'].nunique()} trading days of the union calendar")
        slog.log("gathering", f"{label}: {suffix} sample", firms=panel["ric"].nunique(), rows=len(panel), note=rule)


# ---------- MAIN ----------
def main():
    print(f"Building panels: main (>= {MIN_DAYS} trading days pre & post) and perfectly balanced\n")
    slog.reset("gathering")
    for label, (path, stem, kind) in FILES.items():
        if not path.exists():
            print(f"⚠️ File not found: {path}")
            continue
        print(f"\n--- {label} ---")
        t0 = time.perf_counter()
        (process_intraday if kind == "intraday" else process_daily)(path, stem, label)
        print(f" [{label}] done in {time.perf_counter() - t0:.0f}s")
    print("\n✅ All datasets processed successfully.")


if __name__ == "__main__":
    main()
