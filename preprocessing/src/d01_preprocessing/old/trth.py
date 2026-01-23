from pathlib import Path
import pandas as pd
import numpy as np
from tqdm import tqdm
from multiprocessing import Pool, cpu_count

# ---------- FOLDER STRUCTURE ----------
DATA_ROOT = Path(__file__).resolve().parents[2] / "data"
RAW_DIR   = DATA_ROOT / "01_raw" / "trth"
OUT_5M    = DATA_ROOT / "02_preprocessed" / "trth"
OUT_5M.mkdir(parents=True, exist_ok=True)

# ---------- HELPERS ----------
def parse_gmt_to_hours(gmt_value) -> float:
    """Parse GMT offsets like '+2', '-03', '9'. '+' or no sign => add, '-' => subtract."""
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
    if price < prev_mid:
        return -1
    return 0

def winsorize_series(s: pd.Series, lower_q=0.001, upper_q=0.9999):
    """Clip extremes at given quantiles; handles all-NaN or constant series cleanly."""
    if s.empty or s.dropna().nunique() <= 1:
        return s
    try:
        low, high = s.quantile([lower_q, upper_q])
    except Exception:
        return s
    if pd.isna(low) or pd.isna(high):
        return s
    return s.clip(lower=low, upper=high)


# ---------- 5-MIN FUTURE LOOKUPS (t+5m, last <= target) ----------
def add_future_refs(group: pd.DataFrame) -> pd.DataFrame:
    """
    Within each (ric, local day) compute:
      mid_ref_future  = last mid_ref at or before t+5m
      w_mid_ref_future = last w_mid_ref at or before t+5m
    """
    # time vectors
    dt = group["datetime"].to_numpy(dtype="datetime64[ns]")
    target = (group["datetime"] + pd.Timedelta(minutes=5)).to_numpy(dtype="datetime64[ns]")

    # compute common index of last observation <= target
    idx = np.searchsorted(dt, target, side="right") - 1
    valid = (idx >= 0) & (idx < len(dt))

    # ensure both reference arrays exist and are same length
    mid_ref  = group["mid_ref"].to_numpy(dtype="float64", copy=False)
    w_mid_ref = group["w_mid_ref"].to_numpy(dtype="float64", copy=False)

    # allocate futures
    mid_ref_future = np.full(len(dt), np.nan)
    w_mid_ref_future = np.full(len(dt), np.nan)

    # fill where valid
    mid_ref_future[valid] = mid_ref[idx[valid]]
    w_mid_ref_future[valid] = w_mid_ref[idx[valid]]

    group["mid_ref_future"]  = mid_ref_future
    group["w_mid_ref_future"] = w_mid_ref_future
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

        # --- Parse GMT offsets & local (trading) day ---
        df["gmt_hours"] = df["gmt"].apply(parse_gmt_to_hours)
        local_offset = pd.to_timedelta(df["gmt_hours"], unit="h")
        df["local_dt"] = df["datetime"] + local_offset
        df["date_local"] = df["local_dt"].dt.date

        # --- Drop invalid quotes/trades ---
        df["is_quote"] = df["type"].astype(str).str.upper().eq("QUOTE")
        df["is_trade"] = df["type"].astype(str).str.upper().eq("TRADE")

        bad_quote = df["is_quote"] & df["ask"].notna() & df["bid"].notna() & (df["ask"] < df["bid"])
        df = df.loc[~bad_quote].copy()

        bad_trade = df["is_trade"] & ((df["price"] < 0) | (df["volume"] < 0))
        df = df.loc[~bad_trade].copy()

        # --- Quote midpoint, reference midpoint (carry-forward) ---
        df["mid"] = np.where(df["ask"].notna() & df["bid"].notna(), (df["ask"] + df["bid"]) / 2, np.nan)
        df["mid_quote"] = np.where(df["is_quote"], df["mid"], np.nan)

        # Weighted quote midpoint (only when sizes and prices are present)
        valid_sizes  = df["ask_size"].notna() & df["bid_size"].notna() & (df["ask_size"] > 0) & (df["bid_size"] > 0)
        valid_prices = df["ask"].notna() & df["bid"].notna()
        denom = (df["ask_size"] + df["bid_size"]).astype("float64")
        w_mid = np.where(
            valid_sizes & valid_prices & (denom > 0),
            (df["ask"] * df["ask_size"] + df["bid"] * df["bid_size"]) / denom,
            np.nan
        )
        df["w_mid_quote"] = np.where(df["is_quote"], w_mid, np.nan)

        # -- add market depth on quote rows --
        df["market_depth"] = np.where(
            df["is_quote"] & valid_sizes & valid_prices,
            df["ask_size"].astype("float64") + df["bid_size"].astype("float64"),
            np.nan
        )
        df["market_depth_value"] = np.where(
            df["is_quote"] & df["market_depth"].notna() & df["mid"].notna(),
            df["market_depth"] * df["mid"],
            np.nan
        )

        # Sort, then ffill reference midpoints by (ric, local day)
        df = df.sort_values(["ric", "date_local", "datetime"], kind="mergesort")
        df["mid_ref"]   = df.groupby(["ric", "date_local"], group_keys=False)["mid_quote"].ffill()
        df["w_mid_ref"] = df.groupby(["ric", "date_local"], group_keys=False)["w_mid_quote"].ffill()

        # --- Direction vs previous ref midpoint (only trades) ---
        df["prev_mid_ref"] = df.groupby(["ric", "date_local"])["mid_ref"].shift(1)
        df["direction"] = np.vectorize(compute_direction)(df["price"], df["prev_mid_ref"])

        # --- Quoted spread (only quotes w/ both sides) ---
        df["q_spread"] = np.where(
            df["is_quote"] & df["mid"].notna(),
            (df["ask"] - df["bid"]) / df["mid"],
            np.nan
        )

        # --- Effective spread (only trades, using ref midpoint) ---
        df["e_spread"] = np.where(
            df["is_trade"] & df["mid_ref"].notna() & df["price"].notna(),
            (df["price"] - df["mid_ref"]).abs() / df["mid_ref"],
            np.nan
        )
        # Signed 2× effective spread
        df["e_spread2"] = np.where(
            df["is_trade"] & df["e_spread"].notna() & df["direction"].notna(),
            2 * df["e_spread"] * df["direction"],
            np.nan
        )

        # --- 5-min FUTURE REFS (t+5m, last <= target) ---
        df = df.groupby(["ric", "date_local"], group_keys=False).apply(add_future_refs)

        # Price impact (signed) and 2× price impact
        df["price_impact"] = np.where(
            df["is_trade"] & df["mid_ref"].notna() & df["mid_ref_future"].notna(),
            ((df["mid_ref_future"] - df["mid_ref"]) / df["mid_ref"]) * df["direction"],
            np.nan
        )
        # Keep zero-ticks (no NaN override here)
        df["price_impact2"] = np.where(df["price_impact"].notna(), 2 * df["price_impact"], np.nan)

        # or exclude zero-tick trades (direction = 0)
        df.loc[df["direction"] == 0, ["direction","price_impact","price_impact2"]] = np.nan

        # --- Realized spreads (unweighted) ---
        df["realized_spread"]  = np.where(df["e_spread"].notna()  & df["price_impact"].notna(),  df["e_spread"]  - df["price_impact"],  np.nan)
        df["realized_spread2"] = np.where(df["e_spread2"].notna() & df["price_impact2"].notna(), df["e_spread2"] - df["price_impact2"], np.nan)

        # --- Weighted effective spread / price impact / realized spread ---
        df["w_e_spread2"] = np.where(
            df["is_trade"] & df["w_mid_ref"].notna() & df["price"].notna() & df["direction"].notna(),
            2 * df["direction"] * ((df["price"] - df["w_mid_ref"]) / df["w_mid_ref"]),
            np.nan
        )
        df["w_price_impact2"] = np.where(
            df["is_trade"] & df["w_mid_ref"].notna() & df["w_mid_ref_future"].notna() & df["direction"].notna(),
            2 * df["direction"] * ((df["w_mid_ref_future"] - df["w_mid_ref"]) / df["w_mid_ref"]),
            np.nan
        )
        df["w_realized_spread2"] = np.where(
            df["w_e_spread2"].notna() & df["w_price_impact2"].notna(),
            df["w_e_spread2"] - df["w_price_impact2"],
            np.nan
        )

        # --- 5-minute log return based on consecutive prices per ric ---
        df["log_return"] = np.log(df.groupby("ric")["price"].transform(lambda x: x / x.shift(1)))
        df["squared_log_return"] = df["log_return"] ** 2

        # --- Winsorize per RIC ---
        for col in [
            "price","volume","q_spread","e_spread","e_spread2",
            "price_impact","price_impact2","realized_spread","realized_spread2",
            "w_e_spread2","w_price_impact2","w_realized_spread2","log_return","squared_log_return"
        ]:
            df[col] = df.groupby("ric", group_keys=False)[col].transform(lambda x: winsorize_series(x))

        # ---------- 5-MIN AGGREGATION (keep UTC + local day) ----------
        df["dt_5m"] = df["datetime"].dt.floor("5min")

        agg_5m = (
            df.groupby(["ric","dt_5m","date_local"], as_index=False)
              .agg(
                  price_mean=("price","mean"),
                  volume_mean=("volume","mean"),
                  volume_sum=("volume", "sum"),
                  qspread_mean=("q_spread","mean"),
                  espread_mean=("e_spread","mean"),
                  espread2_mean=("e_spread2","mean"),
                  priceimpact_mean=("price_impact","mean"),
                  priceimpact2_mean=("price_impact2","mean"),
                  realized_spread_mean=("realized_spread","mean"),
                  realized_spread2_mean=("realized_spread2","mean"),
                  w_espread2_mean=("w_e_spread2","mean"),
                  w_priceimpact2_mean=("w_price_impact2","mean"),
                  w_realized_spread2_mean=("w_realized_spread2","mean"),
                  logret_mean=("log_return", "mean"),
                  sqr_logret_mean=("squared_log_return", "mean"),
                  trades_count=("is_trade","sum"),
                  quotes_count=("is_quote","sum"),
                  depth_mean=("market_depth","mean"),
                  depth_sum=("market_depth","sum"),
                  depth_value_mean=("market_depth_value","mean"),
                  depth_value_sum=("market_depth_value","sum"),
                  price_std=("price","std"),
                  gmt_first=("gmt","first"),
              )
              .rename(columns={"dt_5m":"datetime","gmt_first":"gmt"})
        )

        agg_5m["avg_trade_size"] = np.where(agg_5m["trades_count"] > 0,
                                    agg_5m["volume_sum"] / agg_5m["trades_count"], np.nan)
        agg_5m["avg_quote_size"] = np.where(agg_5m["quotes_count"] > 0,
                                    agg_5m["depth_sum"] / agg_5m["quotes_count"], np.nan)

        # ---------- SAVE (5-minute) ----------
        out_5m = OUT_5M / f"{base}.csv"
        agg_5m.to_csv(out_5m, index=False)

        print(f"   ✓ {base}: 5m={len(agg_5m):,}")
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