import pandas as pd 
import numpy as np 
from pathlib import Path 
from tqdm import tqdm 
import multiprocessing as mp 

# ---------- PATHS ---------- 
ROOT = Path(__file__).resolve().parents[3] 
ROOT_PRE = ROOT / "preprocessing" / "data" / "03_output" 
ROOT_OUT = ROOT / "analysis" / "data" 
ROOT_OUT.mkdir(parents=True, exist_ok=True) 

FILES = { 
    "intraday": ROOT_PRE / "intraday.csv", 
    "daily" : ROOT_PRE / "daily.csv", 
    "rusukr" : ROOT_PRE / "intraday_RUS_UKR.csv", 
    "aerodef" : ROOT_PRE / "intraday_AERO_DEF.csv", 
    "rusukr_daily" : ROOT_PRE / "daily_RUS_UKR.csv", 
    "aerodef_daily": ROOT_PRE / "daily_AERO_DEF.csv", 
}

# ---------- TIME WINDOW ---------- 
CUTOFF = pd.Timestamp("2022-02-24", tz="UTC") 
START = pd.Timestamp("2022-01-27", tz="UTC") 
END = pd.Timestamp("2022-03-23", tz="UTC") 

# ---------- REQUIRED VARS ---------- 
REQUIRED_INTRADAY = ["qspread_mean", "espread_mean", "dollar_volume_mean", "priceimpact_mean", "intraday_vol_mean", "intraday_5m_vol_mean", "mktval"] 
REQUIRED_DAILY = ["qspread_mean", "espread_mean", "dollar_volume_mean", "priceimpact_mean", "intraday_vol_mean", "intraday_5m_vol_mean", "mktval"] 

# ---------- PARALLEL SAFE CHUNK READER ---------- 
def fast_read_csv(path, chunksize=3_000_000): 
    """Read very large CSV in smaller chunks quickly and keep order stable.""" 
    chunks = [] 
    for chunk in pd.read_csv(path, low_memory=False, chunksize=chunksize): 
        chunks.append(chunk) 
    df = pd.concat(chunks, ignore_index=True) 
    return df.sort_values(["ric"], kind="mergesort").reset_index(drop=True) 

# ---------- BALANCING UTILS ---------- 
def balance_panel(df, date_col, label): 
    """Keep only RICs with equal counts before and after cutoff date.""" 
    df = df.sort_values(["ric", date_col], 
kind="mergesort").reset_index(drop=True) 
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
    print(f" [{label}] balanced RICs {len(keep_rics):,} / total {df['ric'].nunique():,}") 
    return kept 

# ---------- DATE COLUMN DETECTION ---------- 
def detect_datetime_col(df, label): 
    if label in {"daily", "rusukr_daily", "aerodef_daily"}: 
        for c in ["date", "local_date"]: 
            if c in df.columns: 
                return c 
    else: 
        for c in ["datetime", "local_datetime"]: 
            if c in df.columns: 
                return c 
    raise ValueError(f"No datetime column found in {label}.") 

# ---------- PROCESSOR ---------- 
def process_file(path: Path, label: str): 
    if not path.exists(): 
        print(f"⚠️ File not found: {path}") 
        return 

    print(f"\n--- Processing {label} ---") 
    df = fast_read_csv(path) 
    total_rics = df["ric"].nunique() if "ric" in df.columns else 0 
    print(f"Loaded {len(df):,} rows, {total_rics:,} RICs") 

    # Select proper required variable set 
    if label in {"daily", "rusukr_daily", "aerodef_daily"}: 
        required_vars = REQUIRED_DAILY 
    else: 
        required_vars = REQUIRED_INTRADAY 

    missing_cols = [c for c in required_vars if c not in df.columns] 
    if missing_cols: 
        print(f" [{label}] Missing required columns {missing_cols}. Skipping.") 
        return 

    # Detect & parse datetime 
    date_col = detect_datetime_col(df, label) 
    df[date_col] = pd.to_datetime(df[date_col], errors="coerce", utc=True) 
    df = df.dropna(subset=[date_col]) 

    # ---- SORT FIX ---- 
    sort_keys = ["ric"] 
    if {"date", "time"} <= set(df.columns): 
        sort_keys += ["date", "time"] 
    else: 
        sort_keys.append(date_col) 
    df = df.sort_values(sort_keys, kind="mergesort").reset_index(drop=True) 
    # ------------------- 

    # Restrict to window [START, END] 
    df = df[(df[date_col] >= START) & (df[date_col] <= END)].copy() 
    if df.empty: 
        print(f" [{label}] No rows in window {START.date()}–{END.date()}.") 
        return 

    # Require completeness on required vars 
    before_rows = len(df) 
    df = df.dropna(subset=required_vars) 
    df = df.sort_values(["ric", date_col], kind="mergesort").reset_index(drop=True) 
    print(f" [{label}] dropna({required_vars}) → {before_rows:,} → {len(df):,}") 

        
    # Balance before/after 
    balanced = balance_panel(df, date_col, label) 
    print(f" [{label}] final {len(balanced):,} rows") 

    # Sort final output 
    balanced = balanced.sort_values(["ric", date_col], kind="mergesort").reset_index(drop=True) 

    # Save per dataset 
    out_map = { 
        "intraday" : ROOT_OUT / "intraday_balanced.csv", 
        "daily" : ROOT_OUT / "daily_balanced.csv", 
        "rusukr" : ROOT_OUT / "rusukr_balanced.csv", 
        "aerodef" : ROOT_OUT / "aerodef_balanced.csv", 
        "rusukr_daily" : ROOT_OUT / "rusukr_daily_balanced.csv", 
        "aerodef_daily" : ROOT_OUT / "aerodef_daily_balanced.csv", 
    } 
    out_path = out_map[label] 
    balanced.to_csv(out_path, index=False) 
    print(f"✅ Saved {label} → {out_path}") 

# ---------- MAIN ---------- 
def main(): 
    print("Starting balancing for intraday/daily/ruua with full var completeness...\n") 
    ncpu = max(1, mp.cpu_count() - 1) 
    print(f"Using up to {ncpu} CPU cores") 

    for label, path in tqdm(FILES.items(), desc="Processing datasets", unit="file"): 
        process_file(path, label) 

    print("\n✅ All datasets processed successfully.") 

if __name__ == "__main__": 
    main()