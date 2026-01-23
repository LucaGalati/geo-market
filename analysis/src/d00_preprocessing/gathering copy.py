import sys
import argparse
import pandas as pd
import numpy as np
from pathlib import Path
from tqdm import tqdm
import multiprocessing as mp

# ---------- PATHS ----------
# This script lives at: analysis/src/d00_preprocessing/gathering.py
# So the project root is three levels up from here.
ROOT = Path(__file__).resolve().parents[3]
ROOT_PRE  = ROOT / "preprocessing" / "data" / "03_output"
ROOT_OUT  = ROOT / "analysis" / "data"
ROOT_OUT.mkdir(parents=True, exist_ok=True)

FILES = {
    "intraday"       : ROOT_PRE / "intraday.csv",
    "daily"          : ROOT_PRE / "daily.csv",
    "rusukr"         : ROOT_PRE / "intraday_RUS_UKR.csv",
    "aerodef"        : ROOT_PRE / "intraday_AERO_DEF.csv",
    "rusukr_daily"   : ROOT_PRE / "daily_RUS_UKR.csv",
    "aerodef_daily"  : ROOT_PRE / "daily_AERO_DEF.csv",
}

# ---------- TIME WINDOW (UTC) ----------
CUTOFF = pd.Timestamp("2022-02-24", tz="UTC")
START  = pd.Timestamp("2022-01-27", tz="UTC")
END    = pd.Timestamp("2022-03-23", tz="UTC")

# ---------- REQUIRED VARS ----------
# Intraday should NOT require price_std; Daily can include it.
REQUIRED_INTRADAY = [
    "qspread_mean", "espread_mean", "dollar_volume_mean",
    "priceimpact_mean", "intraday_vol_mean", "intraday_5m_vol_mean", "mktval"
]
REQUIRED_DAILY = [
    "qspread_mean", "espread_mean", "dollar_volume_mean",
    "priceimpact_mean", "intraday_vol_mean", "intraday_5m_vol_mean", "mktval",
    # include price_std here only if you truly need it; otherwise comment it out:
    # "price_std"
]

# ---------- CSV READER (robust, chunked) ----------
def fast_read_csv(path: Path, chunksize: int = 3_000_000) -> pd.DataFrame:
    """
    Read very large CSV in chunks, prefer fast C engine, auto-fallback to Python engine,
    and skip bad lines. Return a stably-sorted DataFrame by 'ric' if present.
    """
    def _read(engine: str) -> pd.DataFrame:
        chunks = []
        for chunk in pd.read_csv(
            path,
            low_memory=False,
            chunksize=chunksize,
            engine=engine,
            on_bad_lines="skip",     # avoid fatal parsing on bad rows
        ):
            chunks.append(chunk)
        if not chunks:
            return pd.DataFrame()
        dfc = pd.concat(chunks, ignore_index=True)
        if "ric" in dfc.columns:
            dfc = dfc.sort_values(["ric"], kind="mergesort").reset_index(drop=True)
        return dfc

    # try C first, then fallback to Python
    try:
        return _read(engine="c")
    except Exception as e_c:
        print(f"  ⚠️ C engine failed on {path.name} ({e_c}). Falling back to Python engine...")
        try:
            return _read(engine="python")
        except Exception as e_py:
            print(f"  ❌ Python engine also failed on {path.name}: {e_py}")
            raise

# ---------- DATETIME COLUMN DETECTION ----------
def detect_datetime_col(df: pd.DataFrame, label: str) -> str:
    """
    Prefer 'datetime', then fall back to other common names. Raise helpful error if none found.
    """
    candidates = ["datetime", "local_datetime", "date", "date_local", "time"]
    for c in candidates:
        if c in df.columns:
            return c
    raise ValueError(
        f"No datetime-like column found in {label}. "
        f"Tried {candidates}. Available columns: {list(df.columns)}"
    )

# ---------- BALANCING ----------
def balance_panel(df: pd.DataFrame, date_col: str, label: str) -> pd.DataFrame:
    """
    Keep only RICs with equal counts before and after cutoff date.
    Assumes df has been windowed and dropna() applied.
    """
    df = df.sort_values(["ric", date_col], kind="mergesort").reset_index(drop=True)
    df = df.copy()
    df["period"] = np.where(df[date_col] >= CUTOFF, "after", "before")

    counts = (
        df.groupby(["ric", "period"], sort=False)
          .size()
          .unstack(fill_value=0)
          .sort_index(kind="mergesort")
    )
    for col in ["before", "after"]:
        if col not in counts:
            counts[col] = 0

    keep_rics = counts.index[counts["before"] == counts["after"]]
    kept = df[df["ric"].isin(keep_rics)].copy()
    kept = kept.sort_values(["ric", date_col], kind="mergesort").reset_index(drop=True)
    print(f"  [{label}] balanced RICs {len(keep_rics):,} / total {df['ric'].nunique():,}")
    return kept

# ---------- PROCESS ONE FILE ----------
def process_file(path: Path, label: str, force: bool = False):
    if not path.exists():
        print(f"⚠️ File not found: {path}")
        return

    # Output file map (skip if exists unless --force)
    out_map = {
        "intraday"      : ROOT_OUT / "intraday_balanced.csv",
        "daily"         : ROOT_OUT / "daily_balanced.csv",
        "rusukr"        : ROOT_OUT / "rusukr_balanced.csv",
        "aerodef"       : ROOT_OUT / "aerodef_balanced.csv",
        "rusukr_daily"  : ROOT_OUT / "rusukr_daily_balanced.csv",
        "aerodef_daily" : ROOT_OUT / "aerodef_daily_balanced.csv",
    }
    out_path = out_map[label]
    # if out_path.exists() and not force:
    #     print(f"⏭️  {label} already processed → {out_path} (use --force to recompute)")
    #     return

    print(f"\n--- Processing {label} ---")
    df = fast_read_csv(path)
    if df.empty:
        print(f"  [{label}] Empty input. Skipping.")
        return

    total_rics = df["ric"].nunique() if "ric" in df.columns else 0
    print(f"Loaded {len(df):,} rows, {total_rics:,} RICs")

    # Choose required vars set
    if label in {"daily", "rusukr_daily", "aerodef_daily"}:
        required_vars = REQUIRED_DAILY
    else:
        required_vars = REQUIRED_INTRADAY

    # Check column presence (informative only; we still dropna on the intersection)
    missing_cols = [c for c in required_vars if c not in df.columns]
    if missing_cols:
        print(f"  [{label}] Missing columns {missing_cols}. We will drop rows where present columns are NaN.")

    # Detect & parse datetime safely in UTC
    date_col = detect_datetime_col(df, label)
    df[date_col] = pd.to_datetime(df[date_col], errors="coerce", utc=True)
    df = df.dropna(subset=[date_col])

    # Stable sort before filtering
    sort_keys = ["ric", date_col] if "ric" in df.columns else [date_col]
    df = df.sort_values(sort_keys, kind="mergesort").reset_index(drop=True)

    # Restrict to window [START, END] (UTC)
    df = df[(df[date_col] >= START) & (df[date_col] <= END)].copy()
    if df.empty:
        print(f"  [{label}] No rows in window {START.date()}–{END.date()}.")
        return

    # Require completeness on **present** required vars (ignore truly missing columns)
    present_required = [c for c in required_vars if c in df.columns]
    before_rows = len(df)
    if present_required:
        df = df.dropna(subset=present_required)
    df = df.sort_values(sort_keys, kind="mergesort").reset_index(drop=True)
    print(f"  [{label}] dropna({present_required}) → {before_rows:,} → {len(df):,}")

    if df.empty:
        print(f"  [{label}] No rows remain after completeness filter.")
        return

    # Balance before/after cutoff
    balanced = balance_panel(df, date_col, label)
    print(f"  [{label}] final {len(balanced):,} rows")

    # Final stable sort and save
    balanced = balanced.sort_values(sort_keys, kind="mergesort").reset_index(drop=True)
    balanced.to_csv(out_path, index=False)
    print(f"✅ Saved {label} → {out_path}")

# ---------- MAIN ----------
def main():
    parser = argparse.ArgumentParser(description="Balance intraday/daily panels with completeness + windowing.")
    parser.add_argument("--force", action="store_true", help="Recompute even if outputs already exist.")
    args = parser.parse_args()

    print("Starting balancing for intraday/daily/rusukr/aerodef with full var completeness...\n")
    ncpu = max(1, mp.cpu_count() - 1)
    print(f"Using up to {ncpu} CPU cores")

    for label, path in tqdm(FILES.items(), desc="Processing datasets", unit="file"):
        process_file(path, label, force=args.force)

    print("\n✅ All datasets processed successfully.")

if __name__ == "__main__":
    main()