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
    """Parse GMT offset strings like '+2', '-03', '9'. '+' or no sign => add, '-' => subtract."""
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
    if pd.isna(price) or pd.isna(prev_mid):
        return np.nan
    if price > prev_mid:
        return 1
    elif price < prev_mid:
        return -1
    return 0


def winsorize_series(s: pd.Series, lower_q=0.001, upper_q=0.9999):
    """Clip extremes at given quantiles."""
    if s.empty:
        return s
    low, high = s.quantile([lower_q, upper_q])
    return s.clip(lower=low, upper=high)


# ---------- PRICE IMPACT (vectorized 5-min lookup) ----------
def add_mid_future(group: pd.DataFrame) -> pd.DataFrame:
    """For each (ric, local day) group, assign mid_ref_future = mid_ref(t+5min)."""
    dt = group["datetime"].to_numpy(dtype="datetime64[ns]")
    mid_ref = group["mid_ref"].to_numpy(dtype="float64")
    target = (group["datetime"] + pd.Timedelta(minutes=5)).to_numpy(dtype="datetime64[ns]")

    # index of last observation <= target
    idx = np.searchsorted(dt, target, side="right") - 1
    valid = idx >= 0
    mid_ref_future = np.full(len(group), np.nan)
    mid_ref_future[valid] = mid_ref[idx[valid]]
    group["mid_ref_future"] = mid_ref_future
    return group


# ---------- PER-FILE PROCESS ----------
def process_one_file(gz_file: Path):
    base = gz_file.stem.replace(".csv", "")
    try:
        print(f"▶ Processing {gz_file.name}")
        df = pd.read_csv(gz_file, compression="gzip", low_memory=False)

        # --- Rename & drop Domain if present ---
        rename_map = {
            "#RIC": "ric", "RIC": "ric",
            "Date-Time": "datetime", "GMT Offset": "gmt",
            "Type": "type", "Price": "price", "Volume": "volume",
            "Bid Price": "bid", "Bid Size": "bid_size",
            "Ask Price": "ask", "Ask Size": "ask_size",
            "Exch Time": "time", "Domain": None,
        }
        if "Domain" in df.columns:
            df = df.drop(columns=["Domain"])
        df = df.rename(columns={k: v for k, v in rename_map.items() if k in df.columns and v})

        # --- Parse datetime & numerics ---
        df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
        for col in ["price", "bid", "ask"]:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors="coerce")
        for col in ["volume", "bid_size", "ask_size"]:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors="coerce", downcast="integer")

        # --- Parse GMT offsets & local date ---
        df["gmt_hours"] = df["gmt"].apply(parse_gmt_to_hours)
        local_offset = pd.to_timedelta(df["gmt_hours"], unit="h")
        df["local_dt"] = df["datetime"] + local_offset
        df["date_local"] = df["local_dt"].dt.date

        # --- Drop invalid quotes/trades ---
        mask_bad_quote = (
            (df["type"].astype(str).str.upper() == "QUOTE")
            & (df["ask"].notna()) & (df["bid"].notna())
            & (df["ask"] < df["bid"])
        )
        df = df.loc[~mask_bad_quote].copy()

        mask_bad_trade = (
            (df["type"].astype(str).str.upper() == "TRADE")
            & ((df["price"] < 0) | (df["volume"] < 0))
        )
        df = df.loc[~mask_bad_trade].copy()

        # --- Compute quote midpoint & reference mid (carry forward last quote) ---
        df["mid"] = np.where(
            df["ask"].notna() & df["bid"].notna(),
            (df["ask"] + df["bid"]) / 2,
            np.nan
        )
        df["is_quote"] = df["type"].astype(str).str.upper().eq("QUOTE")
        df["mid_quote"] = np.where(df["is_quote"], df["mid"], np.nan)

        df = df.sort_values(["ric", "date_local", "datetime"], kind="mergesort")
        df["mid_ref"] = (
            df.groupby(["ric", "date_local"], group_keys=False)["mid_quote"].ffill()
        )

        # --- Direction vs previous mid_ref ---
        df["prev_mid_ref"] = df.groupby(["ric", "date_local"])["mid_ref"].shift(1)
        df["direction"] = np.vectorize(compute_direction)(df["price"], df["prev_mid_ref"])

        # --- Quoted & Effective spreads ---
        df["q_spread"] = np.where(
            df["is_quote"] & df["mid"].notna(),
            (df["ask"] - df["bid"]) / df["mid"],
            np.nan
        )

        is_trade = df["type"].astype(str).str.upper().eq("TRADE")
        df["e_spread"] = np.where(
            is_trade & df["mid_ref"].notna() & df["price"].notna(),
            (df["price"] - df["mid_ref"]).abs() / df["mid_ref"],
            np.nan
        )

        # --- Add signed 2×effective spread ---
        df["e_spread2"] = np.where(
            is_trade & df["e_spread"].notna() & df["direction"].notna(),
            2 * df["e_spread"] * df["direction"],
            np.nan
        )

        # --- 5-min future mid_ref & price impact ---
        df = df.groupby(["ric", "date_local"], group_keys=False).apply(add_mid_future)
        df["price_impact"] = np.where(
            is_trade & df["mid_ref"].notna() & df["mid_ref_future"].notna(),
            ((df["mid_ref_future"] - df["mid_ref"]) / df["mid_ref"]) * df["direction"],
            np.nan
        )
        df.loc[df["price_impact"] == 0, "price_impact"] = np.nan

        # --- Add signed 2×price impact ---
        df["price_impact2"] = np.where(
            df["price_impact"].notna(),
            2 * df["price_impact"],
            np.nan
        )

        # --- Winsorize per RIC ---
        for col in ["price", "volume", "q_spread", "e_spread", "e_spread2", "price_impact", "price_impact2"]:
            df[col] = (
                df.groupby("ric", group_keys=False)[col]
                  .transform(lambda x: winsorize_series(x))
            )

        # --- 5-minute aggregation ---
        df["dt_5m"] = df["datetime"].dt.floor("5min")
        agg_5m = (
            df.groupby(["ric", "dt_5m"], as_index=False)
              .agg(
                  price_mean=("price","mean"),
                  volume_mean=("volume","mean"),
                  qspread_mean=("q_spread","mean"),
                  espread_mean=("e_spread","mean"),
                  espread2_mean=("e_spread2","mean"),
                  priceimpact_mean=("price_impact","mean"),
                  priceimpact2_mean=("price_impact2","mean"),
                  price_std=("price","std"),
                  gmt_first=("gmt","first"),
              )
              .rename(columns={"dt_5m": "datetime", "gmt_first": "gmt"})
        )

        # --- Daily aggregation ---
        df["day_local"] = df["date_local"]
        agg_daily = (
            df.groupby(["ric", "day_local"], as_index=False)
              .agg(
                  price_mean=("price","mean"),
                  volume_mean=("volume","mean"),
                  qspread_mean=("q_spread","mean"),
                  espread_mean=("e_spread","mean"),
                  espread2_mean=("e_spread2","mean"),
                  priceimpact_mean=("price_impact","mean"),
                  priceimpact2_mean=("price_impact2","mean"),
                  price_std=("price","std"),
                  gmt_first=("gmt","first"),
              )
        )
        agg_daily["datetime"] = pd.to_datetime(agg_daily["day_local"])
        agg_daily = agg_daily.drop(columns=["day_local"]).rename(columns={"gmt_first": "gmt"})

        # --- Save to CSV ---
        out_5m = OUT_5M / f"{base}.csv"
        out_daily = OUT_DAILY / f"{base}.csv"
        agg_5m.to_csv(out_5m, index=False)
        agg_daily.to_csv(out_daily, index=False)

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

    nproc = max(1, cpu_count() - 2)
    print(f"Using {nproc} CPU cores ...")

    with Pool(processes=nproc) as pool:
        list(tqdm(pool.imap_unordered(process_one_file, gz_files),
                  total=len(gz_files), desc="Processing files", unit="file"))
    print("\n✅ All files processed successfully.\n")


if __name__ == "__main__":
    main()