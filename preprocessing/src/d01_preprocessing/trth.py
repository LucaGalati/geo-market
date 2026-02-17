from pathlib import Path
import pandas as pd
import numpy as np
from tqdm import tqdm
from multiprocessing import cpu_count
import shutil
import sys
import os
import warnings, numpy as np
# silence benign NumPy runtime warnings (NaNs during vectorized ops)
warnings.filterwarnings(
    "ignore",
    message="invalid value encountered in subtract",
    category=RuntimeWarning,
    module="numpy.lib.function_base"
)
warnings.filterwarnings(
    "ignore",
    message="Degrees of freedom <= 0 for slice",
    category=RuntimeWarning,
    module="numpy.lib.nanfunctions"
)
np.seterr(invalid="ignore")

# ---------- FOLDER STRUCTURE ----------
DATA_ROOT = Path(__file__).resolve().parents[2] / "data"
RAW_DIR   = DATA_ROOT / "01_raw" / "trth"
OUT_DIR   = DATA_ROOT / "02_preprocessed" / "trth"
TMP_DIR   = DATA_ROOT / "02_preprocessed" / "tmp"
OUT_DIR.mkdir(parents=True, exist_ok=True)
TMP_DIR.mkdir(parents=True, exist_ok=True)

CHUNKSIZE = 3_000_000   # tune up/down; this is RAM-safe on 32 GB

# ---------- HELPERS ----------
def parse_gmt_to_hours(gmt_value) -> float:
    if pd.isna(gmt_value):
        return np.nan
    s = str(gmt_value).strip()
    if s == "":
        return np.nan
    sign = 1
    if s.startswith("-"):
        sign = -1; s = s[1:]
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
    s_nonan = s.dropna()
    if s_nonan.empty or s_nonan.nunique() <= 1:
        return s
    low, high = s_nonan.quantile([lower_q, upper_q])
    if pd.isna(low) or pd.isna(high):
        return s
    return s.clip(lower=low, upper=high)

def add_future_refs(group: pd.DataFrame) -> pd.DataFrame:
    """Per (ric, date_local): mid_ref_future & w_mid_ref_future = last ref at or before t+5m."""
    if not pd.api.types.is_datetime64_any_dtype(group["datetime"]):
        group["datetime"] = pd.to_datetime(group["datetime"], errors="coerce", utc=True)

    dt = group["datetime"].to_numpy(dtype="datetime64[ns]")
    target = (group["datetime"] + pd.Timedelta(minutes=5)).to_numpy(dtype="datetime64[ns]")

    idx = np.searchsorted(dt, target, side="right") + 1
    valid = (idx >= 0) & (idx < len(dt))

    mid_ref = group["mid_ref"].to_numpy(dtype="float64", copy=False)
    w_mid_ref = group["w_mid_ref"].to_numpy(dtype="float64", copy=False)

    mid_ref_future = np.full(len(dt), np.nan)
    w_mid_ref_future = np.full(len(dt), np.nan)

    mid_ref_future[valid] = mid_ref[idx[valid]]
    w_mid_ref_future[valid] = w_mid_ref[idx[valid]]

    group["mid_ref_future"]  = mid_ref_future
    group["w_mid_ref_future"] = w_mid_ref_future
    return group

# ---------- PHASE 1 : stream & shard (CSV.GZ) ----------
def stage_phase_one(gz_path: Path, tmp_root: Path):
    """
    Phase 1: read .csv.gz in chunks, normalize columns,
    compute flags/date, and write partitioned CSV.GZ shards:
    tmp_root/<base>/ric=<RIC>/date_local=<YYYY-MM-DD>/part-XXXX.csv.gz
    """
    base = gz_path.stem.replace(".csv", "")
    out_root = tmp_root / base

    # --- Resume logic ---
    existing_files = list(out_root.rglob("*.csv.gz")) if out_root.exists() else []
    if existing_files:
        print(f"   ↪ Phase 1 already completed or partially staged for {base} — skipping Phase 1.")
        return out_root  # reuse existing shards

    # if folder exists but empty, continue (do not delete)
    if not out_root.exists():
        out_root.mkdir(parents=True, exist_ok=True)

    rename_map = {
        "#RIC": "ric",
        "Date-Time": "datetime", "GMT Offset": "gmt",
        "Type": "type", "Price": "price", "Volume": "volume",
        "Bid Price": "bid", "Bid Size": "bid_size",
        "Ask Price": "ask", "Ask Size": "ask_size",
        "Exch Time": "time", "Domain": None,
    }
    usecols = [c for c in rename_map.keys() if c != "Domain"]

    it = pd.read_csv(
        gz_path, compression="gzip", low_memory=False,
        chunksize=CHUNKSIZE, usecols=usecols
    )

    for chunk in tqdm(it, desc=f"[Phase 1] staging {gz_path.name}", unit="chunk"):
        # rename
        chunk = chunk.rename(columns={k: v for k, v in rename_map.items() if v})

        # parse datatypes
        chunk["datetime"] = pd.to_datetime(
            chunk["datetime"],
            format="%Y-%m-%dT%H:%M:%S.%fZ",  # adjust if needed
            errors="coerce",
            utc=True
        )

        for col in ["price", "bid", "ask"]:
            if col in chunk.columns:
                chunk[col] = pd.to_numeric(chunk[col], errors="coerce")
        for col in ["volume", "bid_size", "ask_size"]:
            if col in chunk.columns:
                chunk[col] = pd.to_numeric(chunk[col], errors="coerce", downcast="integer")

        # flags
        chunk["is_quote"] = chunk["type"].astype(str).str.upper().eq("QUOTE")
        chunk["is_trade"] = chunk["type"].astype(str).str.upper().eq("TRADE")

        # local day
        gmt_hours = chunk["gmt"].apply(parse_gmt_to_hours)
        local_offset = pd.to_timedelta(gmt_hours, unit="h")
        chunk["local_dt"] = chunk["datetime"] + local_offset
        chunk["date_local"] = chunk["local_dt"].dt.date

        # keep only necessary columns
        keep = ["ric","datetime","gmt","type","is_quote","is_trade","price","volume",
                "bid","ask","bid_size","ask_size","date_local"]
        part = chunk[keep].copy()
        part["date_local"] = part["date_local"].astype(str)

        # write grouped shards as CSV.GZ
        for (ric, d), sub in part.groupby(["ric","date_local"]):
            sub_path = out_root / f"ric={ric}" / f"date_local={d}"
            sub_path.mkdir(parents=True, exist_ok=True)
            file_path = sub_path / f"part-{np.random.randint(1e12)}.csv.gz"
            sub.to_csv(file_path, index=False, compression="gzip")

    return out_root


# ---------- PHASE 2 ----------
def process_shard_folder(shard_dir: Path) -> pd.DataFrame:
    """Read all parts in this ric/date_local folder, compute metrics, return 5m aggregation DataFrame."""
    files = sorted(shard_dir.glob("*.csv.gz"))
    if not files:
        return pd.DataFrame()

    dfs = [pd.read_csv(f, compression="gzip", low_memory=False) for f in files]
    df = pd.concat(dfs, ignore_index=True)

    # parse datetimes back
    df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
    for col in ["price","bid","ask"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")
    for col in ["volume","bid_size","ask_size"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce", downcast="integer")

    # cleaning
    bad_quote = df["is_quote"] & df["ask"].notna() & df["bid"].notna() & (df["ask"] < df["bid"])
    df = df.loc[~bad_quote].copy()
    bad_trade = df["is_trade"] & ((df["price"] < 0) | (df["volume"] < 0))
    df = df.loc[~bad_trade].copy()

    # midpoints
    df["mid"] = np.where(df["ask"].notna() & df["bid"].notna(), (df["ask"] + df["bid"]) / 2, np.nan)
    df["mid_quote"] = np.where(df["is_quote"], df["mid"], np.nan)

    # weighted mid
    valid_sizes  = df["ask_size"].notna() & df["bid_size"].notna() & (df["ask_size"] > 0) & (df["bid_size"] > 0)
    valid_prices = df["ask"].notna() & df["bid"].notna()
    denom = (df["ask_size"] + df["bid_size"]).astype("float64")
    w_mid = np.where(valid_sizes & valid_prices & (denom > 0),
                     (df["ask"] * df["ask_size"] + df["bid"] * df["bid_size"]) / denom,
                     np.nan)
    df["w_mid_quote"] = np.where(df["is_quote"], w_mid, np.nan)

    # sort
    df = df.sort_values(["ric","date_local","datetime"], kind="mergesort")

    # reference mids
    df["mid_ref"]   = df.groupby(["ric","date_local"], group_keys=False)["mid_quote"].ffill()
    df["w_mid_ref"] = df.groupby(["ric","date_local"], group_keys=False)["w_mid_quote"].ffill()

    # direction
    df["prev_mid_ref"] = df.groupby(["ric","date_local"])["mid_ref"].shift(1)
    df["direction"] = np.vectorize(compute_direction)(df["price"], df["prev_mid_ref"])

    # quoted spread
    df["q_spread"] = np.where(df["is_quote"] & df["mid"].notna(),
                              (df["ask"] - df["bid"]) / df["mid"], np.nan)

    # effective spread
    df["e_spread"] = np.where(df["is_trade"] & df["mid_ref"].notna() & df["price"].notna(),
                              (df["price"] - df["mid_ref"]).abs() / df["mid_ref"], np.nan)
    df["e_spread2"] = np.where(df["is_trade"] & df["e_spread"].notna() & df["direction"].notna(),
                               2 * df["e_spread"] * df["direction"], np.nan)

    # future refs
    df = df.groupby(["ric","date_local"], group_keys=False).apply(add_future_refs)

    # price impacts
    df["price_impact"] = np.where(
        df["is_trade"] & df["mid_ref"].notna() & df["mid_ref_future"].notna(),
        (df["mid_ref_future"] - df["mid_ref"]) / df["mid_ref"],
        np.nan
    )
    df["price_impact2"] = np.where(df["price_impact"].notna(),
                                   2 * df["direction"] * df["price_impact"], np.nan)
    df.loc[df["direction"] == 0, ["direction","price_impact2"]] = np.nan

    # weighted versions
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

    # log returns (only compute when both current and previous prices are > 0)
    p = df.groupby("ric", group_keys=False)["price"].apply(
        lambda x: x.where(x > 0)  # mask non-positive
    )
    log_p = np.log(p)             # no log(<=0)
    df["log_return"] = log_p.groupby(df["ric"]).diff()
    df["squared_log_return"] = df["log_return"] ** 2

    # market depth/value
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

    # dollar volume
    df["dollar_volume"] = np.where(
        df["price"].notna() & df["volume"].notna(),
        df["price"] * df["volume"],
        np.nan
    )

    # intraday volatility (std of price within each local day) 
    df["intraday_volatility"] = (
        df.groupby(["ric", "date_local"], group_keys=False)["price"]
            .transform(lambda x: np.nanstd(x, ddof=1) if len(x.dropna()) > 1 else np.nan)
    )

    # Compute 5-minute intraday volatility explicitly before aggregation
    df["price_5m_volatility"] = (
        df[df["is_trade"]]
        .groupby(["ric", df["datetime"].dt.floor("5min")], group_keys=False)["price"]
            .transform(lambda x: np.nanstd(x, ddof=1) if len(x.dropna()) > 1 else np.nan)
    )

    # winsorize
    for col in [
        "price","volume","dollar_volume","q_spread","e_spread",
        "e_spread2","price_impact","price_impact2","w_e_spread2",
        "w_price_impact2","log_return","squared_log_return",
        "intraday_volatility","price_5m_volatility"
    ]:
        df[col] = df.groupby("ric", group_keys=False)[col].transform(lambda x: winsorize_series(x))

    # 5m aggregation
    df["dt_5m"] = df["datetime"].dt.floor("5min")
    agg = (
        df.groupby(["ric","dt_5m","date_local"], as_index=False)
          .agg(
              price_mean=("price","mean"),
              volume_mean=("volume","mean"),
              volume_sum=("volume","sum"),
              dollar_volume_mean=("dollar_volume","mean"),
              dollar_volume_sum=("dollar_volume","sum"),
              intraday_vol_mean=("intraday_volatility", "mean"),
              intraday_5m_vol_mean=("price_5m_volatility","mean"),
              qspread_mean=("q_spread","mean"),
              espread_mean=("e_spread","mean"),
              espread2_mean=("e_spread2","mean"),
              priceimpact_mean=("price_impact","mean"),
              priceimpact2_mean=("price_impact2","mean"),
              w_espread2_mean=("w_e_spread2","mean"),
              w_priceimpact2_mean=("w_price_impact2","mean"),
              logret_mean=("log_return","mean"),
              sqr_logret_mean=("squared_log_return","mean"),
              trades_count=("is_trade","sum"),
              quotes_count=("is_quote","sum"),
              depth_mean=("market_depth","mean"),
              depth_sum=("market_depth","sum"),
              depth_value_mean=("market_depth_value","mean"),
              depth_value_sum=("market_depth_value","sum"),
              price_std=("price","std"),
              gmt_first=("gmt","first"),
          )
    )
    agg = agg.rename(columns={"dt_5m": "datetime", "gmt_first": "gmt"})
    agg["avg_trade_size"] = np.where(agg["trades_count"] > 0,
                                     agg["volume_sum"] / agg["trades_count"], np.nan)
    agg["avg_quote_size"] = np.where(agg["quotes_count"] > 0,
                                     agg["depth_sum"] / agg["quotes_count"], np.nan)
    return agg


def consolidate_phase_two(staged_root: Path, out_file: Path):
    """Phase 2: for each ric/date_local shard, compute metrics & append to final CSV."""
    if out_file.exists():
        out_file.unlink()
    out_file.parent.mkdir(parents=True, exist_ok=True)

    shard_dirs = sorted(staged_root.glob("ric=*/date_local=*"))
    if not shard_dirs:
        shard_dirs = [p for p in staged_root.rglob("*")
                      if p.is_dir() and "ric=" in p.as_posix() and "date_local=" in p.as_posix()]

    for shard in tqdm(shard_dirs, desc=f"[Phase 2] computing {staged_root.name}", unit="shard"):
        agg = process_shard_folder(shard)
        if not agg.empty:
            header = not out_file.exists()
            agg.to_csv(out_file, mode="a", header=header, index=False)


def main():
    args = sys.argv[1:]
    if args:
        gz_files = [RAW_DIR / args[0]]
    else:
        gz_files = sorted(RAW_DIR.glob("*.gz"))

    if not gz_files:
        raise FileNotFoundError(f"No .gz files found in {RAW_DIR}")

    print(f"Found {len(gz_files)} .gz files in {RAW_DIR}")
    print(f"Using up to {max(1, cpu_count()-1)} CPU cores (shard-level work is I/O-bound).")

    for gz in gz_files:
        print(f"▶ Processing {gz.name} (streamed, two-phase)")

        # --- NEW: resume/skip logic handled in stage_phase_one ---
        staged_root = stage_phase_one(gz, TMP_DIR)

        out_csv = OUT_DIR / (gz.stem.replace(".csv", "") + ".csv")
        consolidate_phase_two(staged_root, out_csv)

        # Clean temp only after successful Phase 2
        #shutil.rmtree(staged_root, ignore_errors=True)
        print(f"   ✓ Wrote {out_csv}")

    print("\n✅ All files processed successfully.\n")


if __name__ == "__main__":
    main()