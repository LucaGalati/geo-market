from pathlib import Path
import pandas as pd
import numpy as np
from tqdm import tqdm
from multiprocessing import Pool, cpu_count

# ---------- FOLDER STRUCTURE ----------
DATA_ROOT = Path(__file__).resolve().parents[2] / "data"
RAW_DIR   = DATA_ROOT / "01_raw" / "trth"
OUT_5M    = DATA_ROOT / "02_preprocessed" / "trth" / "intraday5"
OUT_DAILY = DATA_ROOT / "02_preprocessed" / "trth" / "daily"
OUT_5M.mkdir(parents=True, exist_ok=True)
OUT_DAILY.mkdir(parents=True, exist_ok=True)

# ---------- HELPERS ----------
def parse_gmt_to_hours(gmt_value) -> float:
    # '-' => subtract; '+' => add; no sign => add; empty/invalid => NaN
    if pd.isna(gmt_value):
        return np.nan
    s = str(gmt_value).strip()
    if s == "":
        return np.nan
    sign = 1
    if s.startswith("-"):
        sign = -1
        s = s[1:]
    elif s.startswith("+"):
        s = s[1:]
    try:
        return sign * float(s)
    except Exception:
        return np.nan

def compute_direction(price, prev_mid):
    if pd.isna(price) or pd.isna(prev_mid): return np.nan
    if price > prev_mid: return 1
    if price < prev_mid: return -1
    return 0

def winsorize_series(s: pd.Series, lower_q=0.001, upper_q=0.9999):
    if s.empty: return s
    low, high = s.quantile([lower_q, upper_q])
    return s.clip(lower=low, upper=high)

# ---------- PER-FILE PROCESS ----------
def process_one_file(gz_file: Path):
    base = gz_file.stem.replace(".csv", "")
    try:
        print(f"▶ Processing {gz_file.name}")
        df = pd.read_csv(gz_file, compression="gzip", low_memory=False)

        # Normalize columns; drop Domain
        rename_map = {
            "#RIC": "ric", 
            "Date-Time": "datetime", "GMT Offset": "gmt",
            "Type": "type", "Price": "price", "Volume": "volume",
            "Bid Price": "bid", "Bid Size": "bid_size",
            "Ask Price": "ask", "Ask Size": "ask_size",
            "Exch Time": "time", "Domain": None,
        }
        if "Domain" in df.columns:
            df = df.drop(columns=["Domain"])
        df = df.rename(columns={k: v for k, v in rename_map.items() if k in df.columns and v})

        # Parse datetime and numerics
        df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
        for col in ["price","bid","ask"]:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors="coerce")
        for col in ["volume","bid_size","ask_size"]:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors="coerce", downcast="integer")

        # GMT -> local date (add for '+', subtract for '-', add for no sign)
        df["gmt_hours"] = df["gmt"].apply(parse_gmt_to_hours)
        local_offset = pd.to_timedelta(df["gmt_hours"], unit="h")
        df["local_dt"] = df["datetime"] + local_offset
        df["date_local"] = df["local_dt"].dt.date

        # Keep trades even if no quote; drop only invalid quotes (ask<bid)
        mask_invalid_quote = (
            (df["type"].astype(str).str.upper() == "QUOTE")
            & (df["ask"].notna()) & (df["bid"].notna())
            & (df["ask"] < df["bid"])
        )
        df = df.loc[~mask_invalid_quote].copy()

        # Drop invalid trades: negative price OR negative volume
        mask_invalid_trade = (
            (df["type"].astype(str).str.upper() == "TRADE")
            & ((df["price"] < 0) | (df["volume"] < 0))
        )
        df = df.loc[~mask_invalid_trade].copy()

        # Midpoint, direction
        df["mid"] = np.where(
            df["ask"].notna() & df["bid"].notna(),
            (df["ask"] + df["bid"]) / 2,
            np.nan
        )
        df = df.sort_values(["ric","date_local","datetime"], kind="mergesort")
        df["prev_mid"] = df.groupby(["ric","date_local"])["mid"].shift(1)
        df["direction"] = np.vectorize(compute_direction)(df["price"], df["prev_mid"])

        # Spreads
        df["q_spread"] = (df["ask"] - df["bid"]) / df["mid"]
        df["e_spread"] = (df["price"] - df["mid"]).abs() / df["mid"]

        # ---------- PRICE IMPACT (no merge_asof): vectorized search per group ----------
        def add_mid_future(group: pd.DataFrame) -> pd.DataFrame:
            # group is already sorted by datetime
            dt = group["datetime"].to_numpy(dtype="datetime64[ns]")
            mid = group["mid"].to_numpy(dtype="float64")
            tgt = (group["datetime"] + pd.Timedelta(minutes=5)).to_numpy(dtype="datetime64[ns]")
            # index of last rt_dt <= target_dt
            idx = np.searchsorted(dt, tgt, side="right") - 1
            valid = (idx >= 0)
            mid_future = np.full(len(group), np.nan, dtype="float64")
            mid_future[valid] = mid[idx[valid]]
            group["mid_future"] = mid_future
            return group

        df = (
            df.groupby(["ric","date_local"], group_keys=False)
              .apply(add_mid_future)
        )

        df["price_impact"] = ((df["mid_future"] - df["mid"]) / df["mid"]) * df["direction"]
        df.loc[df["price_impact"] == 0, "price_impact"] = np.nan  # keep zero-tick dir, NA impact

        # Winsorize per RIC
        for col in ["price","volume","q_spread","e_spread","price_impact"]:
            df[col] = (
                df.groupby("ric", group_keys=False)[col]
                  .transform(lambda x: winsorize_series(x))
            )

        # Keep only necessary columns
        df = df[["ric","datetime","gmt","price","volume","q_spread","e_spread","price_impact"]].copy()

        # 5-minute aggregation
        df["dt_5m"] = df["datetime"].dt.floor("5min")
        agg_5m = (
            df.groupby(["ric","dt_5m"], as_index=False)
              .agg(
                  price_mean=("price","mean"),
                  volume_mean=("volume","mean"),
                  qspread_mean=("q_spread","mean"),
                  espread_mean=("e_spread","mean"),
                  priceimpact_mean=("price_impact","mean"),
                  price_std=("price","std"),
                  gmt_first=("gmt","first"),
              )
              .rename(columns={"dt_5m":"datetime","gmt_first":"gmt"})
        )

        # Daily aggregation (by local day proxy: use datetime date for a stable key)
        df["day_local"] = df["datetime"].dt.date
        agg_daily = (
            df.groupby(["ric","day_local"], as_index=False)
              .agg(
                  price_mean=("price","mean"),
                  volume_mean=("volume","mean"),
                  qspread_mean=("q_spread","mean"),
                  espread_mean=("e_spread","mean"),
                  priceimpact_mean=("price_impact","mean"),
                  price_std=("price","std"),
                  gmt_first=("gmt","first"),
              )
        )
        agg_daily["datetime"] = pd.to_datetime(agg_daily["day_local"])
        agg_daily = agg_daily.drop(columns=["day_local"]).rename(columns={"gmt_first":"gmt"})

        # Save
        out_5m = OUT_5M / f"{base}.parquet"
        out_daily = OUT_DAILY / f"{base}.parquet"
        agg_5m.to_parquet(out_5m, engine="pyarrow", compression="snappy")
        agg_daily.to_parquet(out_daily, engine="pyarrow", compression="snappy")
        print(f"   ✓ {base}: 5m={len(agg_5m):,} | daily={len(agg_daily):,}")
        return True

    except Exception as e:
        print(f"⚠️ Error in {gz_file.name}: {e}")
        return False

# ---------- MAIN ----------
def main():
    gz_files = sorted(RAW_DIR.glob("*.gz"))
    if not gz_files:
        raise FileNotFoundError(f"No .gz files found in {RAW_DIR}")
    print(f"Found {len(gz_files)} .gz files in {RAW_DIR}")

    # Use all cores minus 2 (good balance for 32 GB RAM on Apple Silicon)
    nproc = max(1, cpu_count() - 2)
    print(f"Using {nproc} CPU cores ...")

    with Pool(processes=nproc) as pool:
        list(tqdm(pool.imap_unordered(process_one_file, gz_files),
                  total=len(gz_files), desc="Processing files", unit="file"))
    print("\n✅ All files processed successfully.\n")

if __name__ == "__main__":
    main()

