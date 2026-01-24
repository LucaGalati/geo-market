from pathlib import Path
import argparse
import cProfile
import io
import os
import shutil
import sys
import time
import gzip
from contextlib import contextmanager
from datetime import datetime
from multiprocessing import cpu_count
from concurrent.futures import ProcessPoolExecutor, as_completed

import numpy as np
import pandas as pd
from tqdm import tqdm
import warnings

try:
    import pyarrow as pa
    import pyarrow.dataset as ds
    import pyarrow.parquet as pq
    _HAS_PYARROW = True
except Exception:
    pa = None
    ds = None
    pq = None
    _HAS_PYARROW = False

try:
    import polars as pl
    _HAS_POLARS = True
except Exception:
    pl = None
    _HAS_POLARS = False
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

# ---------- DEFAULTS ----------
CHUNKSIZE = 3_000_000   # tune up/down; this is RAM-safe on 32 GB
DEFAULT_TEMP_FORMAT = "csv"
DEFAULT_OUTPUT_FORMAT = "csv"
DEFAULT_PARQUET_COMPRESSION = "snappy"
DEFAULT_PARQUET_MAX_ROWS = 1_000_000
LOWER_WINSOR_Q = 0.001
UPPER_WINSOR_Q = 0.9999

# ---------- FOLDER STRUCTURE ----------
DATA_ROOT = Path(__file__).resolve().parents[2] / "data"
RAW_DIR   = DATA_ROOT / "01_raw" / "trth"
OUT_DIR   = DATA_ROOT / "02_preprocessed" / "trth"
TMP_DIR   = DATA_ROOT / "02_preprocessed" / "tmp"
OUT_DIR.mkdir(parents=True, exist_ok=True)
TMP_DIR.mkdir(parents=True, exist_ok=True)

# ---------- LOGGING / TIMING ----------
def log(msg: str):
    ts = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"[{ts}] {msg}")

def _format_bytes(n: int | None) -> str:
    if n is None:
        return "unknown"
    units = ["B", "KB", "MB", "GB", "TB", "PB"]
    val = float(n)
    for unit in units:
        if val < 1024.0:
            return f"{val:.2f} {unit}"
        val /= 1024.0
    return f"{val:.2f} EB"

def _get_ram_info():
    total = None
    available = None
    try:
        import psutil  # optional
        vm = psutil.virtual_memory()
        total = int(vm.total)
        available = int(vm.available)
        return total, available
    except Exception:
        pass

    if sys.platform == "win32":
        try:
            import ctypes
            class MEMORYSTATUSEX(ctypes.Structure):
                _fields_ = [
                    ("dwLength", ctypes.c_ulong),
                    ("dwMemoryLoad", ctypes.c_ulong),
                    ("ullTotalPhys", ctypes.c_ulonglong),
                    ("ullAvailPhys", ctypes.c_ulonglong),
                    ("ullTotalPageFile", ctypes.c_ulonglong),
                    ("ullAvailPageFile", ctypes.c_ulonglong),
                    ("ullTotalVirtual", ctypes.c_ulonglong),
                    ("ullAvailVirtual", ctypes.c_ulonglong),
                    ("sullAvailExtendedVirtual", ctypes.c_ulonglong),
                ]
            status = MEMORYSTATUSEX()
            status.dwLength = ctypes.sizeof(MEMORYSTATUSEX)
            ctypes.windll.kernel32.GlobalMemoryStatusEx(ctypes.byref(status))
            total = int(status.ullTotalPhys)
            available = int(status.ullAvailPhys)
        except Exception:
            pass
    else:
        try:
            page_size = os.sysconf("SC_PAGE_SIZE")
            phys_pages = os.sysconf("SC_PHYS_PAGES")
            total = int(page_size * phys_pages)
        except Exception:
            pass
        # Try Linux /proc/meminfo for available memory
        try:
            with open("/proc/meminfo", "r", encoding="utf-8") as f:
                for line in f:
                    if line.startswith("MemAvailable:"):
                        parts = line.split()
                        if len(parts) >= 2:
                            available = int(parts[1]) * 1024
                        break
        except Exception:
            pass

    return total, available

def _generate_sample_trth_gz(
    out_path: Path,
    rows: int = 50_000,
    rics: int = 4,
    days: int = 3,
    start_date: str = "2023-01-02",
    seed: int = 42,
    overwrite: bool = False,
):
    if out_path.exists() and not overwrite:
        log(f"Sample file already exists: {out_path}")
        return out_path

    rng = np.random.default_rng(seed)
    rics_list = [f"RIC{i+1}" for i in range(rics)]
    base_ts = pd.Timestamp(f"{start_date}T09:30:00Z")
    day_offsets = rng.integers(0, max(days, 1), size=rows)
    seconds = rng.integers(0, 6 * 60 * 60, size=rows)
    datetimes = (base_ts + pd.to_timedelta(day_offsets, unit="D") + pd.to_timedelta(seconds, unit="s")).sort_values()
    types = rng.choice(["QUOTE", "TRADE"], size=rows, p=[0.6, 0.4])

    price = rng.normal(100, 5, size=rows).round(4)
    spread = rng.uniform(0.01, 0.05, size=rows)
    bid = price - spread / 2
    ask = price + spread / 2
    volume = rng.integers(1, 5_000, size=rows)
    bid_size = rng.integers(1, 2_000, size=rows)
    ask_size = rng.integers(1, 2_000, size=rows)
    gmt = rng.choice(["+0", "+1", "-5", "+2"], size=rows)
    exch_time = datetimes.strftime("%H:%M:%S.%f")

    df = pd.DataFrame({
        "#RIC": rng.choice(rics_list, size=rows),
        "Date-Time": datetimes.strftime("%Y-%m-%dT%H:%M:%S.%fZ"),
        "GMT Offset": gmt,
        "Type": types,
        "Price": price,
        "Volume": volume,
        "Bid Price": bid,
        "Bid Size": bid_size,
        "Ask Price": ask,
        "Ask Size": ask_size,
        "Exch Time": exch_time,
        "Domain": "TEST",
    })

    out_path.parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(out_path, "wt", encoding="utf-8") as f:
        df.to_csv(f, index=False)
    log(f"Generated sample TRTH gz: {out_path} ({rows:,} rows)")
    return out_path

@contextmanager
def timed(label: str, enabled: bool):
    start = time.perf_counter()
    yield
    if enabled:
        elapsed = time.perf_counter() - start
        log(f"{label} took {elapsed:.2f}s")

def run_profiled(fn, profile_out: str | None = None):
    pr = cProfile.Profile()
    pr.enable()
    result = fn()
    pr.disable()
    s = io.StringIO()
    import pstats
    stats = pstats.Stats(pr, stream=s).sort_stats("cumulative")
    stats.print_stats(40)
    report = s.getvalue()
    if profile_out:
        Path(profile_out).parent.mkdir(parents=True, exist_ok=True)
        Path(profile_out).write_text(report, encoding="utf-8")
        log(f"Profile written to {profile_out}")
    else:
        print(report)
    return result

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

def _require_pyarrow():
    if not _HAS_PYARROW:
        raise ImportError(
            "pyarrow is required for Parquet temp/output. "
            "Install it (e.g., pip install pyarrow) or use --temp-format csv."
        )

def _require_polars():
    if not _HAS_POLARS:
        raise ImportError(
            "polars is required for --engine polars. "
            "Install it (e.g., pip install polars)."
        )

def _parse_partition_cols(value: str):
    if not value:
        return []
    return [v.strip() for v in value.split(",") if v.strip()]

def _to_arrow_table(obj):
    if _HAS_PYARROW and isinstance(obj, pa.Table):
        return obj
    if _HAS_POLARS and isinstance(obj, pl.DataFrame):
        return obj.to_arrow()
    if isinstance(obj, pd.DataFrame):
        return pa.Table.from_pandas(obj, preserve_index=False)
    raise TypeError(f"Unsupported table type: {type(obj)}")

def _polars_scan_csv(path: str, usecols: list[str]):
    try:
        return pl.scan_csv(
            path,
            columns=usecols,
            try_parse_dates=False,
            ignore_errors=True,
            low_memory=True,
        )
    except TypeError:
        return pl.scan_csv(path, columns=usecols)

def _write_parquet_dataset(
    df,
    out_root: Path,
    partition_cols: list[str],
    compression: str | None,
    max_rows_per_file: int | None,
    basename_template: str,
):
    _require_pyarrow()
    table = _to_arrow_table(df)
    # Prefer dataset writer when available; fallback to parquet.write_to_dataset.
    try:
        file_format = ds.ParquetFileFormat()
        try:
            file_options = file_format.make_write_options(compression=compression or "snappy")
        except Exception:
            file_options = None
        try:
            partitioning = ds.partitioning(partition_cols, flavor="hive")
        except Exception:
            partitioning = partition_cols
        ds.write_dataset(
            table,
            base_dir=str(out_root),
            format=file_format,
            partitioning=partitioning,
            existing_data_behavior="overwrite_or_ignore",
            basename_template=basename_template,
            max_rows_per_file=max_rows_per_file or None,
            file_options=file_options,
        )
    except Exception:
        pq.write_to_dataset(
            table,
            root_path=str(out_root),
            partition_cols=partition_cols,
            compression=compression or "snappy",
        )

def _read_parquet_shard(shard_dir: Path) -> pd.DataFrame:
    _require_pyarrow()
    dataset = ds.dataset(str(shard_dir), format="parquet", partitioning="hive")
    return dataset.to_table().to_pandas()

def _process_shard_folder_worker(args):
    shard_dir, tmp_format = args
    return shard_dir, process_shard_folder(shard_dir, tmp_format)

def _process_shard_folder_polars_worker(args):
    shard_dir, tmp_format = args
    return shard_dir, process_shard_folder_polars(shard_dir, tmp_format)

def add_future_refs(group: pd.DataFrame) -> pd.DataFrame:
    """Per (ric, day): mid_ref_future & w_mid_ref_future = last ref at or before t+5m."""
    if not pd.api.types.is_datetime64_any_dtype(group["datetime"]):
        group["datetime"] = pd.to_datetime(group["datetime"], errors="coerce", utc=True)

    dt = group["datetime"].to_numpy(dtype="datetime64[ns]")
    target = (group["datetime"] + pd.Timedelta(minutes=5)).to_numpy(dtype="datetime64[ns]")

    idx = np.searchsorted(dt, target, side="right") - 1
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

# ---------- PHASE 1 : stream & shard (CSV.GZ or Parquet) ----------
def stage_phase_one(
    gz_path: Path,
    tmp_root: Path,
    *,
    chunksize: int,
    tmp_format: str,
    parquet_partition_cols: list[str],
    parquet_compression: str | None,
    parquet_max_rows: int | None,
    timings: bool,
):
    """
    Phase 1: read .csv.gz in chunks, normalize columns,
    compute flags/date, and write partitioned shards:
    tmp_root/<base>/ric=<RIC>/day=<YYYY-MM-DD>/part-XXXX.(csv.gz|parquet)
    """
    base = gz_path.stem.replace(".csv", "")
    out_root = tmp_root / base

    # --- Resume logic ---
    if tmp_format == "parquet":
        existing_files = list(out_root.rglob("*.parquet")) if out_root.exists() else []
    else:
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
        chunksize=chunksize, usecols=usecols
    )

    for chunk_idx, chunk in enumerate(tqdm(it, desc=f"[Phase 1] staging {gz_path.name}", unit="chunk")):
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
        chunk["day"] = chunk["datetime"].dt.date

        # keep only necessary columns
        keep = ["ric","datetime","gmt","type","is_quote","is_trade","price","volume",
                "bid","ask","bid_size","ask_size","date_local","day"]
        part = chunk[keep].copy()
        part["day"] = part["day"].astype(str)

        if tmp_format == "parquet":
            with timed(f"Phase 1 parquet write (chunk {chunk_idx})", timings):
                _write_parquet_dataset(
                    part,
                    out_root=out_root,
                    partition_cols=parquet_partition_cols,
                    compression=parquet_compression,
                    max_rows_per_file=parquet_max_rows,
                    basename_template=f"part-{chunk_idx}-{{i}}.parquet",
                )
        else:
            # write grouped shards as CSV.GZ
            for (ric, d), sub in part.groupby(["ric","day"]):
                sub_path = out_root / f"ric={ric}" / f"day={d}"
                sub_path.mkdir(parents=True, exist_ok=True)
                file_path = sub_path / f"part-{np.random.randint(1e12)}.csv.gz"
                sub.to_csv(file_path, index=False, compression="gzip")

    return out_root

def stage_phase_one_polars(
    gz_path: Path,
    tmp_root: Path,
    *,
    tmp_format: str,
    parquet_partition_cols: list[str],
    parquet_compression: str | None,
    parquet_max_rows: int | None,
    timings: bool,
):
    """
    Polars Phase 1: read .csv.gz (typically as a whole, streaming when available),
    normalize columns, compute flags/date, and write partitioned Parquet shards.
    """
    _require_polars()
    if tmp_format != "parquet":
        raise ValueError("Polars engine requires --temp-format parquet.")

    base = gz_path.stem.replace(".csv", "")
    out_root = tmp_root / base

    existing_files = list(out_root.rglob("*.parquet")) if out_root.exists() else []
    if existing_files:
        print(f"   ↪ Phase 1 already completed or partially staged for {base} — skipping Phase 1.")
        return out_root

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

    with timed(f"Phase 1 polars load ({gz_path.name})", timings):
        lf = _polars_scan_csv(str(gz_path), usecols)

        lf = lf.rename({k: v for k, v in rename_map.items() if v})

        gmt_str = pl.col("gmt").cast(pl.Utf8, strict=False).str.strip_chars()
        sign = pl.when(gmt_str.str.starts_with("-")).then(-1).otherwise(1)
        gmt_clean = pl.when(gmt_str.str.starts_with("+") | gmt_str.str.starts_with("-")) \
            .then(gmt_str.str.slice(1)).otherwise(gmt_str)
        gmt_hours = pl.when(gmt_str.is_null() | (gmt_str == "")) \
            .then(None) \
            .otherwise(gmt_clean.cast(pl.Float64, strict=False) * sign)

        local_offset_seconds = (gmt_hours * 3600.0).round(0).cast(pl.Int64, strict=False)

        lf = lf.with_columns(
            pl.col("datetime").str.strptime(
                pl.Datetime, format="%Y-%m-%dT%H:%M:%S.%fZ", strict=False
            ).alias("datetime"),
            pl.col("price").cast(pl.Float64, strict=False),
            pl.col("bid").cast(pl.Float64, strict=False),
            pl.col("ask").cast(pl.Float64, strict=False),
            pl.col("volume").cast(pl.Int64, strict=False),
            pl.col("bid_size").cast(pl.Int64, strict=False),
            pl.col("ask_size").cast(pl.Int64, strict=False),
            (pl.col("type").cast(pl.Utf8, strict=False).str.to_uppercase() == "QUOTE").alias("is_quote"),
            (pl.col("type").cast(pl.Utf8, strict=False).str.to_uppercase() == "TRADE").alias("is_trade"),
        )

        lf = lf.with_columns(
            (pl.col("datetime") + pl.duration(seconds=local_offset_seconds)).alias("datetime")
        )
        lf = lf.with_columns(
            pl.col("datetime").dt.date().cast(pl.Utf8).alias("day")
        )

        keep = [
            "ric", "datetime", "gmt", "type", "is_quote", "is_trade",
            "price", "volume", "bid", "ask", "bid_size", "ask_size", "date_local", "day"
        ]
        lf = lf.select(keep)

        df = lf.collect(streaming=True)

    with timed(f"Phase 1 polars parquet write ({gz_path.name})", timings):
        _write_parquet_dataset(
            df,
            out_root=out_root,
            partition_cols=parquet_partition_cols,
            compression=parquet_compression,
            max_rows_per_file=parquet_max_rows,
            basename_template="part-{i}.parquet",
        )

    return out_root


# ---------- PHASE 2 ----------
def process_shard_folder(shard_dir: Path, tmp_format: str) -> pd.DataFrame:
    """Read all parts in this ric/day folder, compute metrics, return 5m aggregation DataFrame."""
    if tmp_format == "parquet":
        if not list(shard_dir.rglob("*.parquet")):
            return pd.DataFrame()
        try:
            df = _read_parquet_shard(shard_dir)
        except Exception:
            return pd.DataFrame()
        if df.empty:
            return df
    else:
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
    df = df.sort_values(["ric","day","datetime"], kind="mergesort")

    # reference mids
    df["mid_ref"]   = df.groupby(["ric","day"], group_keys=False)["mid_quote"].ffill()
    df["w_mid_ref"] = df.groupby(["ric","day"], group_keys=False)["w_mid_quote"].ffill()

    # direction
    df["prev_mid_ref"] = df.groupby(["ric","day"])["mid_ref"].shift(1)
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
    df = df.groupby(["ric","day"], group_keys=False).apply(add_future_refs)

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
        df.groupby(["ric", "day"], group_keys=False)["price"]
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
        df.groupby(["ric","dt_5m","day"], as_index=False)
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

def _winsorize_expr(col: str):
    low = pl.col(col).quantile(LOWER_WINSOR_Q).over("ric")
    high = pl.col(col).quantile(UPPER_WINSOR_Q).over("ric")
    return pl.col(col).clip(low, high).alias(col)

def process_shard_folder_polars(shard_dir: Path, tmp_format: str) -> "pl.DataFrame":
    """Polars version of Phase 2 processing for one ric/day shard."""
    _require_polars()
    if tmp_format != "parquet":
        raise ValueError("Polars engine requires --temp-format parquet.")

    files = sorted(shard_dir.glob("*.parquet"))
    if not files:
        return pl.DataFrame()

    try:
        df = pl.read_parquet([str(f) for f in files])
    except Exception:
        df = pl.read_parquet(str(shard_dir / "*.parquet"))

    if df.height == 0:
        return df

    df = df.with_columns(
        pl.col("datetime").cast(pl.Datetime, strict=False),
        pl.col("price").cast(pl.Float64, strict=False),
        pl.col("bid").cast(pl.Float64, strict=False),
        pl.col("ask").cast(pl.Float64, strict=False),
        pl.col("volume").cast(pl.Int64, strict=False),
        pl.col("bid_size").cast(pl.Int64, strict=False),
        pl.col("ask_size").cast(pl.Int64, strict=False),
    )

    bad_quote = (
        (pl.col("is_quote") == True)
        & pl.col("ask").is_not_null()
        & pl.col("bid").is_not_null()
        & (pl.col("ask") < pl.col("bid"))
    )
    bad_trade = (
        (pl.col("is_trade") == True)
        & ((pl.col("price") < 0) | (pl.col("volume") < 0))
    )
    df = df.filter(~bad_quote & ~bad_trade)

    valid_sizes = (
        pl.col("ask_size").is_not_null()
        & pl.col("bid_size").is_not_null()
        & (pl.col("ask_size") > 0)
        & (pl.col("bid_size") > 0)
    )
    valid_prices = pl.col("ask").is_not_null() & pl.col("bid").is_not_null()

    df = df.with_columns(
        pl.when(valid_prices)
        .then((pl.col("ask") + pl.col("bid")) / 2)
        .otherwise(None)
        .alias("mid")
    )
    df = df.with_columns(
        pl.when(pl.col("is_quote") == True).then(pl.col("mid")).otherwise(None).alias("mid_quote")
    )

    denom = (pl.col("ask_size") + pl.col("bid_size")).cast(pl.Float64)
    w_mid = pl.when(valid_sizes & valid_prices & (denom > 0)).then(
        (pl.col("ask") * pl.col("ask_size") + pl.col("bid") * pl.col("bid_size")) / denom
    ).otherwise(None)
    df = df.with_columns(
        w_mid.alias("w_mid"),
        pl.when(pl.col("is_quote") == True).then(w_mid).otherwise(None).alias("w_mid_quote"),
    )

    df = df.sort(["ric", "day", "datetime"])

    df = df.with_columns(
        pl.col("mid_quote").forward_fill().over(["ric", "day"]).alias("mid_ref"),
        pl.col("w_mid_quote").forward_fill().over(["ric", "day"]).alias("w_mid_ref"),
    )
    df = df.with_columns(
        pl.col("mid_ref").shift(1).over(["ric", "day"]).alias("prev_mid_ref")
    )

    direction = (
        pl.when(pl.col("price").is_null() | pl.col("prev_mid_ref").is_null())
        .then(None)
        .when(pl.col("price") > pl.col("prev_mid_ref")).then(1)
        .when(pl.col("price") < pl.col("prev_mid_ref")).then(-1)
        .otherwise(0)
    )
    df = df.with_columns(direction.alias("direction"))

    df = df.with_columns(
        pl.when((pl.col("is_quote") == True) & pl.col("mid").is_not_null())
        .then((pl.col("ask") - pl.col("bid")) / pl.col("mid"))
        .otherwise(None)
        .alias("q_spread")
    )

    df = df.with_columns(
        pl.when((pl.col("is_trade") == True) & pl.col("mid_ref").is_not_null() & pl.col("price").is_not_null())
        .then((pl.col("price") - pl.col("mid_ref")).abs() / pl.col("mid_ref"))
        .otherwise(None)
        .alias("e_spread")
    )
    df = df.with_columns(
        pl.when((pl.col("is_trade") == True) & pl.col("e_spread").is_not_null() & pl.col("direction").is_not_null())
        .then(2 * pl.col("e_spread") * pl.col("direction"))
        .otherwise(None)
        .alias("e_spread2")
    )

    df = df.with_columns(
        (pl.col("datetime") + pl.duration(minutes=5)).alias("target_dt")
    )
    right = df.select(
        "ric", "day",
        pl.col("datetime").alias("ref_dt"),
        pl.col("mid_ref").alias("mid_ref_future"),
        pl.col("w_mid_ref").alias("w_mid_ref_future"),
    )
    df = df.join_asof(
        right,
        left_on="target_dt",
        right_on="ref_dt",
        by=["ric", "day"],
        strategy="backward",
    )

    df = df.with_columns(
        pl.when((pl.col("is_trade") == True) & pl.col("mid_ref").is_not_null() & pl.col("mid_ref_future").is_not_null())
        .then((pl.col("mid_ref_future") - pl.col("mid_ref")) / pl.col("mid_ref"))
        .otherwise(None)
        .alias("price_impact")
    )
    df = df.with_columns(
        pl.when(pl.col("price_impact").is_not_null())
        .then(2 * pl.col("direction") * pl.col("price_impact"))
        .otherwise(None)
        .alias("price_impact2")
    )
    df = df.with_columns(
        pl.when(pl.col("direction") == 0).then(None).otherwise(pl.col("direction")).alias("direction"),
        pl.when(pl.col("direction") == 0).then(None).otherwise(pl.col("price_impact2")).alias("price_impact2"),
    )

    df = df.with_columns(
        pl.when(
            (pl.col("is_trade") == True)
            & pl.col("w_mid_ref").is_not_null()
            & pl.col("price").is_not_null()
            & pl.col("direction").is_not_null()
        )
        .then(2 * pl.col("direction") * ((pl.col("price") - pl.col("w_mid_ref")) / pl.col("w_mid_ref")))
        .otherwise(None)
        .alias("w_e_spread2")
    )
    df = df.with_columns(
        pl.when(
            (pl.col("is_trade") == True)
            & pl.col("w_mid_ref").is_not_null()
            & pl.col("w_mid_ref_future").is_not_null()
            & pl.col("direction").is_not_null()
        )
        .then(2 * pl.col("direction") * ((pl.col("w_mid_ref_future") - pl.col("w_mid_ref")) / pl.col("w_mid_ref")))
        .otherwise(None)
        .alias("w_price_impact2")
    )

    p = pl.when(pl.col("price") > 0).then(pl.col("price")).otherwise(None)
    df = df.with_columns(
        p.log().alias("log_price")
    )
    df = df.with_columns(
        pl.col("log_price").diff().over("ric").alias("log_return")
    )
    df = df.with_columns((pl.col("log_return") ** 2).alias("squared_log_return"))

    df = df.with_columns(
        pl.when((pl.col("is_quote") == True) & valid_sizes & valid_prices)
        .then(pl.col("ask_size").cast(pl.Float64) + pl.col("bid_size").cast(pl.Float64))
        .otherwise(None)
        .alias("market_depth")
    )
    df = df.with_columns(
        pl.when(pl.col("market_depth").is_not_null() & pl.col("mid").is_not_null())
        .then(pl.col("market_depth") * pl.col("mid"))
        .otherwise(None)
        .alias("market_depth_value")
    )

    df = df.with_columns(
        pl.when(pl.col("price").is_not_null() & pl.col("volume").is_not_null())
        .then(pl.col("price") * pl.col("volume"))
        .otherwise(None)
        .alias("dollar_volume")
    )

    df = df.with_columns(
        pl.col("price").std(ddof=1).over(["ric", "day"]).alias("intraday_volatility")
    )

    df = df.with_columns(
        pl.col("datetime").dt.floor("5m").alias("dt_5m")
    )
    price_trade = pl.when(pl.col("is_trade") == True).then(pl.col("price")).otherwise(None)
    df = df.with_columns(
        price_trade.std(ddof=1).over(["ric", "dt_5m"]).alias("price_5m_volatility")
    )

    winsor_cols = [
        "price","volume","dollar_volume","q_spread","e_spread",
        "e_spread2","price_impact","price_impact2","w_e_spread2",
        "w_price_impact2","log_return","squared_log_return",
        "intraday_volatility","price_5m_volatility"
    ]
    df = df.with_columns([_winsorize_expr(c) for c in winsor_cols])

    agg = (
        df.groupby(["ric", "dt_5m", "date_local"])
          .agg(
              pl.col("price").mean().alias("price_mean"),
              pl.col("volume").mean().alias("volume_mean"),
              pl.col("volume").sum().alias("volume_sum"),
              pl.col("dollar_volume").mean().alias("dollar_volume_mean"),
              pl.col("dollar_volume").sum().alias("dollar_volume_sum"),
              pl.col("intraday_volatility").mean().alias("intraday_vol_mean"),
              pl.col("price_5m_volatility").mean().alias("intraday_5m_vol_mean"),
              pl.col("q_spread").mean().alias("qspread_mean"),
              pl.col("e_spread").mean().alias("espread_mean"),
              pl.col("e_spread2").mean().alias("espread2_mean"),
              pl.col("price_impact").mean().alias("priceimpact_mean"),
              pl.col("price_impact2").mean().alias("priceimpact2_mean"),
              pl.col("w_e_spread2").mean().alias("w_espread2_mean"),
              pl.col("w_price_impact2").mean().alias("w_priceimpact2_mean"),
              pl.col("log_return").mean().alias("logret_mean"),
              pl.col("squared_log_return").mean().alias("sqr_logret_mean"),
              pl.col("is_trade").sum().alias("trades_count"),
              pl.col("is_quote").sum().alias("quotes_count"),
              pl.col("market_depth").mean().alias("depth_mean"),
              pl.col("market_depth").sum().alias("depth_sum"),
              pl.col("market_depth_value").mean().alias("depth_value_mean"),
              pl.col("market_depth_value").sum().alias("depth_value_sum"),
              pl.col("price").std(ddof=1).alias("price_std"),
              pl.col("gmt").first().alias("gmt"),
          )
          .rename({"dt_5m": "datetime"})
    )

    agg = agg.with_columns(
        pl.when(pl.col("trades_count") > 0)
        .then(pl.col("volume_sum") / pl.col("trades_count"))
        .otherwise(None)
        .alias("avg_trade_size"),
        pl.when(pl.col("quotes_count") > 0)
        .then(pl.col("depth_sum") / pl.col("quotes_count"))
        .otherwise(None)
        .alias("avg_quote_size"),
    )

    return agg


def consolidate_phase_two(
    staged_root: Path,
    out_file: Path,
    *,
    tmp_format: str,
    out_format: str,
    phase2_workers: int,
    parquet_compression: str | None,
    timings: bool,
    engine: str = "pandas",
):
    """Phase 2: for each ric/day shard, compute metrics & append to final output."""
    if out_format == "parquet":
        _require_pyarrow()
    if out_file.exists():
        out_file.unlink()
    out_file.parent.mkdir(parents=True, exist_ok=True)

    shard_dirs = sorted(staged_root.glob("ric=*/day=*"))
    if not shard_dirs:
        shard_dirs = [p for p in staged_root.rglob("*")
                      if p.is_dir() and "ric=" in p.as_posix() and "day=" in p.as_posix()]

    phase2_workers = max(1, int(phase2_workers))
    parquet_writer = None
    header_written = False

    def _append_output(agg):
        nonlocal parquet_writer, header_written
        if agg is None:
            return
        if isinstance(agg, pd.DataFrame):
            is_empty = agg.empty
        elif _HAS_POLARS and isinstance(agg, pl.DataFrame):
            is_empty = agg.height == 0
        else:
            is_empty = True
        if is_empty:
            return
        if out_format == "parquet":
            table = _to_arrow_table(agg)
            if parquet_writer is None:
                parquet_writer = pq.ParquetWriter(
                    str(out_file),
                    table.schema,
                    compression=parquet_compression or "snappy",
                )
            parquet_writer.write_table(table)
        else:
            if isinstance(agg, pd.DataFrame):
                agg.to_csv(out_file, mode="a", header=not header_written, index=False)
            elif _HAS_POLARS and isinstance(agg, pl.DataFrame):
                agg.to_pandas().to_csv(out_file, mode="a", header=not header_written, index=False)
            else:
                raise TypeError(f"Unsupported output type: {type(agg)}")
            header_written = True

    if phase2_workers == 1:
        for shard in tqdm(shard_dirs, desc=f"[Phase 2] computing {staged_root.name}", unit="shard"):
            if engine == "polars":
                agg = process_shard_folder_polars(shard, tmp_format)
            else:
                agg = process_shard_folder(shard, tmp_format)
            _append_output(agg)
    else:
        log(f"Phase 2 multiprocessing enabled ({phase2_workers} workers). Output order may be non-deterministic.")
        with timed(f"Phase 2 parallel compute ({staged_root.name})", timings):
            with ProcessPoolExecutor(max_workers=phase2_workers) as ex:
                if engine == "polars":
                    futures = [ex.submit(_process_shard_folder_polars_worker, (shard, tmp_format)) for shard in shard_dirs]
                else:
                    futures = [ex.submit(_process_shard_folder_worker, (shard, tmp_format)) for shard in shard_dirs]
                with tqdm(total=len(futures), desc=f"[Phase 2] computing {staged_root.name}", unit="shard") as pbar:
                    for fut in as_completed(futures):
                        _, agg = fut.result()
                        _append_output(agg)
                        pbar.update(1)

    if parquet_writer is not None:
        parquet_writer.close()


def _build_arg_parser():
    p = argparse.ArgumentParser(description="Stream and preprocess TRTH .csv.gz files.")
    p.add_argument("gz_file", nargs="?", help="Optional single .gz filename under RAW_DIR.")
    p.add_argument("--engine", choices=["pandas", "polars"], default="pandas")
    p.add_argument("--auto-tune", action="store_true", help="Auto-tune chunk size and workers based on RAM/CPU.")
    p.add_argument("--chunksize", type=int, default=None)
    p.add_argument("--generate-sample", action="store_true", help="Generate a small sample .csv.gz in RAW_DIR.")
    p.add_argument("--sample-rows", type=int, default=50_000)
    p.add_argument("--sample-rics", type=int, default=4)
    p.add_argument("--sample-days", type=int, default=3)
    p.add_argument("--sample-start-date", default="2023-01-02")
    p.add_argument("--sample-overwrite", action="store_true")
    p.add_argument("--sample-file", default="sample_trth.csv.gz")
    p.add_argument("--temp-format", choices=["csv", "parquet"], default=DEFAULT_TEMP_FORMAT)
    p.add_argument("--temp-partition-cols", default="ric,day")
    p.add_argument("--temp-parquet-compression", default=DEFAULT_PARQUET_COMPRESSION)
    p.add_argument("--temp-parquet-max-rows", type=int, default=None)
    p.add_argument("--output-format", choices=["csv", "parquet"], default=DEFAULT_OUTPUT_FORMAT)
    p.add_argument("--output-parquet-compression", default=DEFAULT_PARQUET_COMPRESSION)
    p.add_argument("--phase2-workers", type=int, default=None)
    p.add_argument("--timings", action="store_true", help="Print phase timing logs.")
    p.add_argument("--profile", action="store_true", help="Enable cProfile for the main process.")
    p.add_argument("--profile-out", default="", help="Optional path for profile report.")
    return p

def main():
    parser = _build_arg_parser()
    args = parser.parse_args()
    def _run():
        if args.generate_sample:
            sample_path = RAW_DIR / args.sample_file
            _generate_sample_trth_gz(
                sample_path,
                rows=args.sample_rows,
                rics=args.sample_rics,
                days=args.sample_days,
                start_date=args.sample_start_date,
                overwrite=args.sample_overwrite,
            )
            gz_files = [sample_path]
        elif args.gz_file:
            gz_files = [RAW_DIR / args.gz_file]
        else:
            gz_files = sorted(RAW_DIR.glob("*.gz"))

        if not gz_files:
            raise FileNotFoundError(f"No .gz files found in {RAW_DIR}")

        if args.temp_format == "parquet":
            _require_pyarrow()
        if args.engine == "polars":
            _require_polars()
            _require_pyarrow()
            if args.temp_format != "parquet":
                raise ValueError("Polars engine requires --temp-format parquet.")

        parquet_partition_cols = _parse_partition_cols(args.temp_partition_cols)
        if args.temp_format == "parquet" and not parquet_partition_cols:
            raise ValueError("Parquet temp format requires at least one partition column.")

        total_ram, avail_ram = _get_ram_info()
        cpu_total = cpu_count() or 1
        log(f"CPU cores available: {cpu_total}")
        log(f"RAM total: {_format_bytes(total_ram)}; RAM available: {_format_bytes(avail_ram)}")
        log(f"Auto-tune: {args.auto_tune}")
        log(f"Found {len(gz_files)} .gz files in {RAW_DIR}")
        log(f"Engine: {args.engine}")
        # ---- auto-tuned defaults ----
        effective_phase2_workers = args.phase2_workers if args.phase2_workers is not None else 1
        effective_chunksize = args.chunksize if args.chunksize is not None else CHUNKSIZE
        effective_parquet_max_rows = (
            args.temp_parquet_max_rows if args.temp_parquet_max_rows is not None else DEFAULT_PARQUET_MAX_ROWS
        )

        if args.auto_tune:
            if args.phase2_workers is None:
                effective_phase2_workers = max(1, min(cpu_total - 1, 32))
            if args.chunksize is None and avail_ram:
                target_bytes = int(avail_ram * 0.015)
                target_bytes = max(200 * 1024**2, target_bytes)
                effective_chunksize = int(target_bytes / 256)
                effective_chunksize = max(1_000_000, effective_chunksize)
            if args.temp_parquet_max_rows is None:
                base = effective_chunksize if effective_chunksize else DEFAULT_PARQUET_MAX_ROWS
                effective_parquet_max_rows = max(250_000, min(int(base * 0.75), 5_000_000))

        log(f"Phase 2 workers: {max(1, effective_phase2_workers)}")
        log(f"Chunksize: {effective_chunksize}")
        log(f"Temp parquet max rows: {effective_parquet_max_rows}")

        for gz in gz_files:
            log(f"▶ Processing {gz.name} (streamed, two-phase)")

            with timed(f"Phase 1 total ({gz.name})", args.timings):
                if args.engine == "polars":
                    staged_root = stage_phase_one_polars(
                        gz,
                        TMP_DIR,
                        tmp_format=args.temp_format,
                        parquet_partition_cols=parquet_partition_cols,
                        parquet_compression=args.temp_parquet_compression,
                        parquet_max_rows=effective_parquet_max_rows,
                        timings=args.timings,
                    )
                else:
                    staged_root = stage_phase_one(
                        gz,
                        TMP_DIR,
                        chunksize=effective_chunksize,
                        tmp_format=args.temp_format,
                        parquet_partition_cols=parquet_partition_cols,
                        parquet_compression=args.temp_parquet_compression,
                        parquet_max_rows=effective_parquet_max_rows,
                        timings=args.timings,
                    )

            out_suffix = ".parquet" if args.output_format == "parquet" else ".csv"
            out_path = OUT_DIR / (gz.stem.replace(".csv", "") + out_suffix)

            with timed(f"Phase 2 total ({gz.name})", args.timings):
                consolidate_phase_two(
                    staged_root,
                    out_path,
                    tmp_format=args.temp_format,
                    out_format=args.output_format,
                    phase2_workers=effective_phase2_workers,
                    parquet_compression=args.output_parquet_compression,
                    timings=args.timings,
                    engine=args.engine,
                )

            # Clean temp only after successful Phase 2
            #shutil.rmtree(staged_root, ignore_errors=True)
            log(f"✓ Wrote {out_path}")

        log("✅ All files processed successfully.")

    if args.profile:
        run_profiled(_run, args.profile_out or None)
    else:
        _run()


if __name__ == "__main__":
    main()
