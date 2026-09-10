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
# tick-level winsorization (per ric within a shard) is OFF by default: the
# stub-quote / bad-print filters below remove the pathological ticks, and the
# figures winsorize once at the analysis level. --winsor-ticks turns it on
# (symmetric 0.01% / 99.99%).
LOWER_WINSOR_Q = 0.0001
UPPER_WINSOR_Q = 0.9999
WINSOR_TICKS = False
# trade direction: "tick" (Lee-Ready: quote rule vs prevailing mid + tick rule at the mid), "fi" (Jurkatis 2022
# full-information algorithm, DS3) or "both" (default). FI runs on integer
# ticks and millisecond-trimmed timestamps, per (ric, local day).
DIRECTION = "both"
FI_FREQ = 3
FI_BAR = 0.3
# quote-sanity filters (calibrated on real shards: 0.022% of quotes, 8 trades in 1.7M)
MAX_REL_SPREAD = 0.25   # (ask-bid)/mid above this = stub quote (e.g. bid 0.01 / ask 100) -> dropped
MAX_TRADE_DEV = 0.50    # trade more than 50% away from the prevailing mid = bad print -> dropped
# London (and any venue quoting in minor units): some trades are reported in pounds
# while quotes are in pence -> price / prevailing mid ~= 0.01. Rescaled x100 when the
# ratio is within +-5% of 0.01 (about 2% of London trades); the rest of the far-away
# prints are dropped as bad prints.
MINOR_UNIT_RATIO = 0.01
MINOR_UNIT_TOL = 0.05

# ---------- FOLDER STRUCTURE ----------
DATA_ROOT = Path(os.environ.get("GEO_DATA_ROOT", Path(__file__).resolve().parents[2] / "data"))
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

def winsorize_series(s: pd.Series, lower_q=LOWER_WINSOR_Q, upper_q=UPPER_WINSOR_Q):
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
    lf = pl.scan_csv(path, try_parse_dates=False, ignore_errors=True, low_memory=True)
    return lf.select(usecols)

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
    shard_dir, tmp_format, direction, winsor = args
    return shard_dir, process_shard_folder(shard_dir, tmp_format, direction, winsor)

def _process_shard_folder_polars_worker(args):
    shard_dir, tmp_format, direction, winsor = args
    return shard_dir, process_shard_folder_polars(shard_dir, tmp_format, direction, winsor)

def add_future_refs(df: pd.DataFrame) -> pd.DataFrame:
    """Compute future references on trades with merge_asof at target=t+5m by (ric, date_local)."""
    if df.empty:
        df["mid_ref_future"] = np.nan
        return df

    if not pd.api.types.is_datetime64_any_dtype(df["datetime"]):
        df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)

    df["mid_ref_future"] = np.nan

    trades = df.loc[df["is_trade"] & df["datetime"].notna(), ["ric", "date_local", "datetime"]].copy()
    if trades.empty:
        return df

    trades["target"] = trades["datetime"] + pd.Timedelta(minutes=5)
    trades["_row_id"] = trades.index.to_numpy(dtype=np.int64)

    mids = df.loc[df["datetime"].notna(), ["ric", "date_local", "datetime", "mid_ref"]].copy()
    mids = mids.rename(columns={"datetime": "datetime_ref", "mid_ref": "mid_ref_future"})

    trades = trades.sort_values(["ric", "date_local", "target", "_row_id"], kind="mergesort")
    mids = mids.sort_values(["ric", "date_local", "datetime_ref"], kind="mergesort")

    merged = pd.merge_asof(
        trades,
        mids,
        left_on="target",
        right_on="datetime_ref",
        by=["ric", "date_local"],
        direction="backward",
        allow_exact_matches=True,
    )
    row_ids = merged["_row_id"].to_numpy(dtype=np.int64)
    df.loc[row_ids, "mid_ref_future"] = merged["mid_ref_future"].to_numpy()

    return df

# ---------- TRADE DIRECTION: Jurkatis (2022) full-information algorithm ----------
def _price_scale(*arrays, max_dec: int = 8) -> int:
    """Smallest power of ten turning every price into an integer tick."""
    x = np.concatenate([np.asarray(a, dtype=float) for a in arrays])
    x = x[np.isfinite(x)]
    for k in range(max_dec + 1):
        y = x * 10 ** k
        if np.all(np.abs(y - np.round(y)) < 1e-6):
            return 10 ** k
    return 10 ** max_dec


def fi_direction(df: pd.DataFrame, freq: int = FI_FREQ, bar: float = FI_BAR):
    """Trade direction (+1 buy, -1 sell, 0 unclassified, NaN non-trade) from the
    full-information algorithm of Jurkatis (2022, JFM), version DS3 (quote
    updates aggregated at the timestamp precision, order within a timestamp
    unknown), with the tick rule as its final fallback. Returns two series:
    the direction and the classification step (1-3 = FI, 4 = tick fallback).
    Runs per (ric, local day) on the cleaned shard (stub quotes and bad prints
    already dropped); needs `datetime`, `price`, `volume`, `bid/ask(_size)`."""
    try:
        from tradeclass import TradeClassification  # vendored, see tradeclass/__init__.py
    except ImportError:
        sys.path.insert(0, str(Path(__file__).resolve().parent))
        from tradeclass import TradeClassification
    out = pd.Series(np.nan, index=df.index, dtype=float)
    step = pd.Series(np.nan, index=df.index, dtype=float)
    for _, g in df.groupby(["ric", "date_local"], sort=False):
        tr = g[g["is_trade"] & g["price"].notna() & g["volume"].notna()]
        ask = g[g["is_quote"] & g["ask"].notna() & g["ask_size"].notna() & (g["ask_size"] > 0)]
        bid = g[g["is_quote"] & g["bid"].notna() & g["bid_size"].notna() & (g["bid_size"] > 0)]
        if len(tr) == 0 or len(ask) == 0 or len(bid) == 0:
            continue
        t0 = g["datetime"].min().normalize()
        sec = lambda d: (d["datetime"] - t0).dt.total_seconds().to_numpy(dtype=float)
        scale = _price_scale(tr["price"].to_numpy(), ask["ask"].to_numpy(), bid["bid"].to_numpy())
        df_tr = pd.DataFrame({"time": sec(tr), "price": np.round(tr["price"].to_numpy() * scale),
                              "vol": tr["volume"].to_numpy(dtype=float)})
        Ask = pd.DataFrame({"time": sec(ask), "price": np.round(ask["ask"].to_numpy() * scale),
                            "vol": ask["ask_size"].to_numpy(dtype=float)})
        Bid = pd.DataFrame({"time": sec(bid), "price": np.round(bid["bid"].to_numpy() * scale),
                            "vol": bid["bid_size"].to_numpy(dtype=float)})
        try:
            tc = TradeClassification(df_tr, Ask, Bid)
            tc.classify(method="ds_3", freq=freq, bar=bar)
        except Exception as e:  # never lose a shard over the classifier
            log(f"FI classification failed for {g['ric'].iloc[0]} {g['date_local'].iloc[0]}: {e!r}")
            continue
        out.loc[tr.index] = tc.df_tr["Initiator"].to_numpy(dtype=float)
        step.loc[tr.index] = tc.df_tr["Step"].to_numpy(dtype=float)
    return out, step


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
        if (out_root / "_PHASE1_COMPLETE").exists():
            print(f"   ↪ Phase 1 already completed for {base} — reusing staged shards.")
        else:
            print(f"   ⚠️ Reusing staged shards for {base} without a completion marker "
                  f"(staged by an older run, or interrupted). Delete {out_root} to restage from raw.")
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
        # partition key = LOCAL day, so a session straddling UTC midnight stays in one shard
        chunk["day"] = chunk["local_dt"].dt.date

        # keep only necessary columns
        keep = ["ric","datetime","gmt","type","is_quote","is_trade","price","volume",
                "bid","ask","bid_size","ask_size","date_local","day"]
        part = chunk[keep].copy()
        part["date_local"] = part["date_local"].astype(str)

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
            for (ric, d), sub in part.groupby(["ric","date_local"]):
                sub_path = out_root / f"ric={ric}" / f"day={d}"
                sub_path.mkdir(parents=True, exist_ok=True)
                file_path = sub_path / f"part-{np.random.randint(1e12)}.csv.gz"
                sub.to_csv(file_path, index=False, compression="gzip")

    (out_root / "_PHASE1_COMPLETE").touch()
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
        if (out_root / "_PHASE1_COMPLETE").exists():
            print(f"   ↪ Phase 1 already completed for {base} — reusing staged shards.")
        else:
            print(f"   ⚠️ Reusing staged shards for {base} without a completion marker "
                  f"(staged by an older run, or interrupted). Delete {out_root} to restage from raw.")
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
            (pl.col("datetime") + pl.duration(seconds=local_offset_seconds)).alias("local_dt")
        )
        lf = lf.with_columns(
            pl.col("local_dt").dt.date().cast(pl.Utf8).alias("date_local"),
            # partition key = LOCAL day (also fixes the "day" column missing in this engine)
            pl.col("local_dt").dt.date().cast(pl.Utf8).alias("day"),
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

    (out_root / "_PHASE1_COMPLETE").touch()
    return out_root


# ---------- PHASE 2 ----------
def process_shard_folder(shard_dir: Path, tmp_format: str, direction: str = DIRECTION,
                         winsor: bool = WINSOR_TICKS, return_ticks: bool = False) -> pd.DataFrame:
    """Read all parts in this ric/day folder, compute metrics, return 5m aggregation DataFrame
    (with return_ticks also the cleaned tick-level frame, for validation)."""
    if tmp_format == "parquet":
        if not list(shard_dir.rglob("*.parquet")):
            return pd.DataFrame()
        try:
            df = _read_parquet_shard(shard_dir)
        except Exception:
            return pd.DataFrame()
        if df.empty:
            log(f"Empty shard after load (parquet): {shard_dir}")
            return df
    else:
        files = sorted(shard_dir.glob("*.csv.gz"))
        if not files:
            return pd.DataFrame()
        dfs = [pd.read_csv(f, compression="gzip", low_memory=False) for f in files]
        df = pd.concat(dfs, ignore_index=True)
        if df.empty:
            log(f"Empty shard after load (csv.gz): {shard_dir}")
            return df

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
    # stub quotes survive the ask<bid check and produce absurd midpoints
    stub_quote = (
        df["is_quote"] & df["ask"].notna() & df["bid"].notna()
        & ((df["ask"] - df["bid"]) / ((df["ask"] + df["bid"]) / 2) > MAX_REL_SPREAD)
    )
    df = df.loc[~stub_quote].copy()
    if df.empty:
        log(f"Empty shard after cleaning: {shard_dir}")
        return df

    # midpoints
    df["mid"] = np.where(df["ask"].notna() & df["bid"].notna(), (df["ask"] + df["bid"]) / 2, np.nan)
    df["mid_quote"] = np.where(df["is_quote"], df["mid"], np.nan)

    valid_sizes  = df["ask_size"].notna() & df["bid_size"].notna() & (df["ask_size"] > 0) & (df["bid_size"] > 0)
    valid_prices = df["ask"].notna() & df["bid"].notna()

    # sort
    df = df.sort_values(["ric","date_local","datetime"], kind="mergesort")

    # reference mids
    df["mid_ref"]   = df.groupby(["ric","date_local"], group_keys=False)["mid_quote"].ffill()

    # direction
    df["prev_mid_ref"] = df.groupby(["ric","date_local"])["mid_ref"].shift(1)
    # trades reported in the major currency unit while quotes are in the minor one
    ratio = df["price"] / df["prev_mid_ref"]
    minor_unit = (
        df["is_trade"] & ratio.notna()
        & ((ratio / MINOR_UNIT_RATIO - 1).abs() <= MINOR_UNIT_TOL)
    )
    if minor_unit.any():
        df.loc[minor_unit, "price"] = df.loc[minor_unit, "price"] / MINOR_UNIT_RATIO
    # trades printing far from the prevailing mid are bad prints, not information
    off_trade = (
        df["is_trade"] & df["price"].notna() & df["prev_mid_ref"].notna()
        & ((df["price"] / df["prev_mid_ref"] - 1).abs() > MAX_TRADE_DEV)
    )
    if off_trade.any():
        df = df.loc[~off_trade].copy()
        df["prev_mid_ref"] = df.groupby(["ric","date_local"])["mid_ref"].shift(1)
        valid_sizes = valid_sizes.loc[df.index]    # masks built before the drop
        valid_prices = valid_prices.loc[df.index]
    # Lee-Ready (1991): quote rule against the prevailing mid; trades at the mid
    # (or without a prevailing mid) take the sign of the last price change
    # between trades of the same firm-day (equal prices skipped; first trade unsigned)
    df["direction"] = np.select(
        [df["price"].isna() | df["prev_mid_ref"].isna(),
         df["price"] > df["prev_mid_ref"],
         df["price"] < df["prev_mid_ref"]],
        [np.nan, 1.0, -1.0],
        default=0.0,
    )
    keys = [df["ric"], df["date_local"]]
    p = df["price"].where(df["is_trade"])
    prev = p.groupby(keys).ffill().groupby(keys).shift(1)          # last trade price before this trade
    changed = df["is_trade"] & p.notna() & (prev.isna() | (p != prev))
    prev_diff = prev.where(changed).groupby(keys).ffill()          # last price different from the current one
    tick = np.sign(p - prev_diff).fillna(0.0)
    need_tick = df["is_trade"] & p.notna() & (df["direction"].isna() | (df["direction"] == 0))
    df["lr_step"] = np.where(df["is_trade"] & p.notna(), np.where(need_tick, 2.0, 1.0), np.nan)
    df.loc[need_tick, "direction"] = tick[need_tick]
    if direction in ("fi", "both"):
        df["direction_fi"], df["fi_step"] = fi_direction(df)
    if direction == "fi":
        df["direction"] = df["direction_fi"]

    # quoted spread
    df["q_spread"] = np.where(df["is_quote"] & df["mid"].notna(),
                              (df["ask"] - df["bid"]) / df["mid"], np.nan)

    # effective spread
    df["e_spread"] = np.where(df["is_trade"] & df["mid_ref"].notna() & df["price"].notna(),
                              (df["price"] - df["mid_ref"]).abs() / df["mid_ref"], np.nan)

    # future refs
    df = add_future_refs(df)

    # price impact (JFE): D_t * (M_t+5 - M_t) / M_t, no factor 2, only for
    # trades whose t+5 target falls inside the trade's local calendar day
    df["mid_change"] = np.where(
        df["is_trade"] & df["mid_ref"].notna() & df["mid_ref_future"].notna(),
        (df["mid_ref_future"] - df["mid_ref"]) / df["mid_ref"],
        np.nan
    )
    gmt_hours = df["gmt"].apply(parse_gmt_to_hours)
    target_local_day = (
        df["datetime"]
        + pd.to_timedelta(gmt_hours, unit="h")
        + pd.Timedelta(minutes=5)
    ).dt.date
    trade_local_day = pd.to_datetime(df["date_local"], errors="coerce").dt.date
    same_day = df["mid_change"].notna() & (target_local_day == trade_local_day)
    for suffix in (["", "_fi"] if direction == "both" else [""]):
        d = df["direction" + suffix]
        df["price_impact" + suffix] = np.where(same_day & d.notna() & d.ne(0), d * df["mid_change"], np.nan)

    # log returns between consecutive TRADES: interleaved quote rows carry no
    # trade price and must not break the pairs
    traded = df.loc[df["is_trade"] & (df["price"] > 0)]
    df["log_return"] = np.log(traded["price"]).groupby(traded["ric"]).diff()
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

    # JFE intraday volatility: daily standard deviation of 5-minute
    # midpoint returns, using both simple and log returns.
    df["dt_5m"] = df["datetime"].dt.floor("5min")
    mid_5m = (
        df.groupby(["ric", "date_local", "dt_5m"], as_index=False)["mid_ref"]
          .last()
          .sort_values(["ric", "date_local", "dt_5m"])
    )
    mid_5m["ret_mid5"] = mid_5m.groupby(["ric", "date_local"])["mid_ref"].pct_change(fill_method=None)
    mid_5m["lret_mid5"] = np.log(mid_5m["mid_ref"].where(mid_5m["mid_ref"] > 0)).groupby(
        [mid_5m["ric"], mid_5m["date_local"]]
    ).diff()
    vol_day = (
        mid_5m.groupby(["ric", "date_local"], as_index=False)
              .agg(volatility=("ret_mid5", "std"), log_volatility=("lret_mid5", "std"))
    )

    # optional tick-level winsorization (off by default, see WINSOR_TICKS)
    if winsor:
        for col in [c for c in WINSOR_COLS if c in df.columns]:
            df[col] = df.groupby("ric", group_keys=False)[col].transform(lambda x: winsorize_series(x))

    # 5m aggregation
    if "price_impact_fi" in df.columns:
        df["fi_classified"] = df["direction_fi"].isin([1.0, -1.0]) & (df["fi_step"] < 4)
    df["dt_5m"] = df["datetime"].dt.floor("5min")
    agg = (
        df.groupby(["ric","dt_5m","date_local"], as_index=False)
          .agg(
              price_mean=("price","mean"),
              price_close=("price","last"),
              volume_mean=("volume","mean"),
              volume_sum=("volume","sum"),
              dollar_volume_mean=("dollar_volume","mean"),
              dollar_volume_sum=("dollar_volume","sum"),
              qspread_mean=("q_spread","mean"),
              espread_mean=("e_spread","mean"),
              price_impact_mean=("price_impact","mean"),
              logret_mean=("log_return","mean"),
              sqr_logret_mean=("squared_log_return","mean"),
              trades_count=("is_trade","sum"),
              quotes_count=("is_quote","sum"),
              depth_mean=("market_depth","mean"),
              depth_sum=("market_depth","sum"),
              depth_value_mean=("market_depth_value","mean"),
              depth_value_sum=("market_depth_value","sum"),
              gmt_first=("gmt","first"),
              **({"price_impact_fi_mean": ("price_impact_fi", "mean"),
                  "trades_fi_count": ("fi_classified", "sum")} if "price_impact_fi" in df.columns else {}),
          )
    )
    agg = agg.rename(columns={"dt_5m": "datetime", "gmt_first": "gmt"})
    agg = agg.merge(vol_day, on=["ric", "date_local"], how="left")
    agg["avg_trade_size"] = np.where(agg["trades_count"] > 0,
                                     agg["volume_sum"] / agg["trades_count"], np.nan)
    agg["avg_quote_size"] = np.where(agg["quotes_count"] > 0,
                                     agg["depth_sum"] / agg["quotes_count"], np.nan)
    return (agg, df) if return_ticks else agg

WINSOR_COLS = [
    "price", "volume", "dollar_volume", "q_spread", "e_spread",
    "price_impact", "price_impact_fi", "log_return", "squared_log_return",
]


def _winsorize_expr(col: str):
    # cast to float and interpolation="linear" to match pandas clip/quantile exactly
    c = pl.col(col).cast(pl.Float64)
    low = c.quantile(LOWER_WINSOR_Q, interpolation="linear").over("ric")
    high = c.quantile(UPPER_WINSOR_Q, interpolation="linear").over("ric")
    return c.clip(low, high).alias(col)

def process_shard_folder_polars(shard_dir: Path, tmp_format: str, direction: str = DIRECTION,
                                winsor: bool = WINSOR_TICKS) -> "pl.DataFrame":
    """Polars version of Phase 2 processing for one ric/day shard."""
    _require_polars()
    direction_mode = direction  # `direction` is reused below for the tick-test expression
    if tmp_format == "parquet":
        files = sorted(shard_dir.glob("*.parquet"))
        if not files:
            return pl.DataFrame()
        try:
            df = pl.read_parquet([str(f) for f in files])
        except Exception:
            df = pl.read_parquet(str(shard_dir / "*.parquet"))
        # hive-partitioned files do not store the partition columns (ric, day):
        # recover them from the shard path
        for part in (shard_dir.name, shard_dir.parent.name):
            if "=" in part:
                key, val = part.split("=", 1)
                if key not in df.columns:
                    df = df.with_columns(pl.lit(val).alias(key))
        dt_expr = pl.col("datetime").cast(pl.Datetime, strict=False)
    elif tmp_format == "csv":
        files = sorted(shard_dir.glob("*.csv.gz"))
        if not files:
            return pl.DataFrame()
        try:
            df = pl.concat(
                [pl.read_csv(str(f), infer_schema_length=1000) for f in files],
                how="vertical_relaxed",
                rechunk=True,
            )
        except Exception:
            return pl.DataFrame()
        dt_expr = pl.col("datetime").cast(pl.Utf8, strict=False).str.strptime(
            pl.Datetime, format="%Y-%m-%d %H:%M:%S%.f%z", strict=False
        )
    else:
        raise ValueError(f"Unsupported temp format for polars: {tmp_format}")

    if df.height == 0:
        return df

    df = df.with_columns(
        dt_expr.alias("datetime"),
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
    stub_quote = (
        (pl.col("is_quote") == True)
        & pl.col("ask").is_not_null()
        & pl.col("bid").is_not_null()
        & ((pl.col("ask") - pl.col("bid")) / ((pl.col("ask") + pl.col("bid")) / 2) > MAX_REL_SPREAD)
    )
    df = df.filter(~stub_quote)

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


    df = df.sort(["ric", "date_local", "datetime"])

    df = df.with_columns(
        pl.col("mid_quote").forward_fill().over(["ric", "date_local"]).alias("mid_ref"),
    )
    df = df.with_columns(
        pl.col("mid_ref").shift(1).over(["ric", "date_local"]).alias("prev_mid_ref")
    )
    minor_unit = (
        (pl.col("is_trade") == True)
        & ((pl.col("price") / pl.col("prev_mid_ref") / MINOR_UNIT_RATIO - 1).abs() <= MINOR_UNIT_TOL)
    )
    df = df.with_columns(
        pl.when(minor_unit).then(pl.col("price") / MINOR_UNIT_RATIO).otherwise(pl.col("price")).alias("price")
    )
    off_trade = (
        (pl.col("is_trade") == True)
        & pl.col("price").is_not_null()
        & pl.col("prev_mid_ref").is_not_null()
        & ((pl.col("price") / pl.col("prev_mid_ref") - 1).abs() > MAX_TRADE_DEV)
    )
    df = df.filter(~off_trade)
    df = df.with_columns(
        pl.col("mid_ref").shift(1).over(["ric", "date_local"]).alias("prev_mid_ref")
    )

    direction = (
        pl.when(pl.col("price").is_null() | pl.col("prev_mid_ref").is_null())
        .then(None)
        .when(pl.col("price") > pl.col("prev_mid_ref")).then(1.0)
        .when(pl.col("price") < pl.col("prev_mid_ref")).then(-1.0)
        .otherwise(0.0)
    )
    df = df.with_columns(direction.alias("direction"))
    # Lee-Ready tick rule for trades at the mid / without a prevailing mid (see pandas version)
    keys = ["ric", "date_local"]
    is_tr = (pl.col("is_trade") == True) & pl.col("price").is_not_null()
    df = df.with_columns(pl.when(is_tr).then(pl.col("price")).otherwise(None).alias("_p"))
    df = df.with_columns(pl.col("_p").forward_fill().shift(1).over(keys).alias("_prev"))
    changed = is_tr & (pl.col("_prev").is_null() | (pl.col("_p") != pl.col("_prev")))
    df = df.with_columns(pl.when(changed).then(pl.col("_prev")).otherwise(None).forward_fill().over(keys).alias("_prev_diff"))
    need_tick = is_tr & (pl.col("direction").is_null() | (pl.col("direction") == 0))
    df = df.with_columns(
        pl.when(need_tick).then((pl.col("_p") - pl.col("_prev_diff")).sign().fill_null(0.0).cast(pl.Float64))
        .otherwise(pl.col("direction")).alias("direction"),
        pl.when(is_tr).then(pl.when(need_tick).then(2.0).otherwise(1.0)).otherwise(None).alias("lr_step"),
    ).drop(["_p", "_prev", "_prev_diff"])
    if direction_mode in ("fi", "both"):
        # the FI classifier is numpy/Cython: hand it the cleaned ticks as pandas
        pdf = df.select(["ric", "date_local", "datetime", "is_trade", "is_quote", "price", "volume",
                         "bid", "ask", "bid_size", "ask_size"]).to_pandas()
        fi_dir, fi_step = fi_direction(pdf)
        df = df.with_columns(pl.Series("direction_fi", fi_dir.to_numpy()),
                             pl.Series("fi_step", fi_step.to_numpy()))
    if direction_mode == "fi":
        df = df.with_columns(pl.col("direction_fi").alias("direction"))

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
        (pl.col("datetime") + pl.duration(minutes=5)).alias("target_dt")
    )
    right = df.select(
        "ric", "date_local",
        pl.col("datetime").alias("ref_dt"),
        pl.col("mid_ref").alias("mid_ref_future"),
    )
    df = df.join_asof(
        right,
        left_on="target_dt",
        right_on="ref_dt",
        by=["ric", "date_local"],
        strategy="backward",
    )

    df = df.with_columns(
        pl.when((pl.col("is_trade") == True) & pl.col("mid_ref").is_not_null() & pl.col("mid_ref_future").is_not_null())
        .then((pl.col("mid_ref_future") - pl.col("mid_ref")) / pl.col("mid_ref"))
        .otherwise(None)
        .alias("mid_change")
    )
    # JFE price impact D_t * (M_t+5 - M_t) / M_t, same local day only
    gmt_str = pl.col("gmt").cast(pl.Utf8, strict=False).str.strip_chars()
    gmt_sign = pl.when(gmt_str.str.starts_with("-")).then(-1).otherwise(1)
    gmt_clean = pl.when(gmt_str.str.starts_with("+") | gmt_str.str.starts_with("-")) \
        .then(gmt_str.str.slice(1)).otherwise(gmt_str)
    gmt_hours = gmt_clean.cast(pl.Float64, strict=False) * gmt_sign
    target_local_day = (
        pl.col("target_dt")
        + pl.duration(seconds=(gmt_hours * 3600).round(0).cast(pl.Int64, strict=False))
    ).dt.date().cast(pl.Utf8)
    same_day = pl.col("mid_change").is_not_null() & (target_local_day == pl.col("date_local"))
    df = df.with_columns([
        pl.when(same_day & pl.col("direction" + sfx).is_not_null() & (pl.col("direction" + sfx) != 0))
        .then(pl.col("direction" + sfx) * pl.col("mid_change"))
        .otherwise(None)
        .alias("price_impact" + sfx)
        for sfx in (["", "_fi"] if direction_mode == "both" else [""])
    ])
    # log returns between consecutive TRADES (see pandas version)
    log_price = (
        pl.when((pl.col("is_trade") == True) & (pl.col("price") > 0))
        .then(pl.col("price").log())
        .otherwise(None)
    )
    df = df.with_columns(log_price.alias("log_price"))
    df = df.with_columns(
        (pl.col("log_price") - pl.col("log_price").forward_fill().shift(1).over("ric")).alias("log_return")
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
        pl.col("datetime").dt.truncate("5m").alias("dt_5m")
    )

    # JFE intraday volatility from 5-minute midpoint returns.
    mid_5m = (
        df.group_by(["ric", "date_local", "dt_5m"])
          .agg(pl.col("mid_ref").drop_nulls().last().alias("mid_5m"))  # pandas .last() skips NaN
          .sort(["ric", "date_local", "dt_5m"])
    )
    mid_5m = mid_5m.with_columns(
        pl.col("mid_5m").shift(1).over(["ric", "date_local"]).alias("mid_5m_lag")
    )
    mid_5m = mid_5m.with_columns(
        ((pl.col("mid_5m") / pl.col("mid_5m_lag")) - 1).alias("ret_mid5"),
        (pl.col("mid_5m").log() - pl.col("mid_5m_lag").log()).alias("lret_mid5"),
    )
    vol_day = mid_5m.group_by(["ric", "date_local"]).agg(
        pl.col("ret_mid5").std(ddof=1).alias("volatility"),
        pl.col("lret_mid5").std(ddof=1).alias("log_volatility"),
    )

    if winsor:
        df = df.with_columns([_winsorize_expr(c) for c in WINSOR_COLS if c in df.columns])
    has_fi = "price_impact_fi" in df.columns
    if has_fi:
        df = df.with_columns(
            (pl.col("direction_fi").is_in([1.0, -1.0]) & (pl.col("fi_step") < 4)).alias("fi_classified")
        )

    agg = (
        df.group_by(["ric", "dt_5m", "date_local"])
          .agg(
              pl.col("price").mean().alias("price_mean"),
              pl.col("price").drop_nulls().last().alias("price_close"),
              pl.col("volume").mean().alias("volume_mean"),
              pl.col("volume").sum().alias("volume_sum"),
              pl.col("dollar_volume").mean().alias("dollar_volume_mean"),
              pl.col("dollar_volume").sum().alias("dollar_volume_sum"),
              pl.col("q_spread").mean().alias("qspread_mean"),
              pl.col("e_spread").mean().alias("espread_mean"),
              pl.col("price_impact").mean().alias("price_impact_mean"),
              pl.col("log_return").mean().alias("logret_mean"),
              pl.col("squared_log_return").mean().alias("sqr_logret_mean"),
              pl.col("is_trade").sum().alias("trades_count"),
              pl.col("is_quote").sum().alias("quotes_count"),
              pl.col("market_depth").mean().alias("depth_mean"),
              pl.col("market_depth").sum().alias("depth_sum"),
              pl.col("market_depth_value").mean().alias("depth_value_mean"),
              pl.col("market_depth_value").sum().alias("depth_value_sum"),
              pl.col("gmt").first().alias("gmt"),
              *([pl.col("price_impact_fi").mean().alias("price_impact_fi_mean"),
                 pl.col("fi_classified").sum().alias("trades_fi_count")] if has_fi else []),
          )
          .rename({"dt_5m": "datetime"})
    )

    agg = agg.join(vol_day, on=["ric", "date_local"], how="left")

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
    direction: str = DIRECTION,
    winsor: bool = WINSOR_TICKS,
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
    parquet_schema = None
    header_written = False
    empty_shards = 0
    total_shards = len(shard_dirs)

    def _canonical_schema(schema):
        """Shards differ in inferred types (int vs float, gmt int vs string):
        fix one schema for the whole file and cast every shard to it."""
        fields = []
        for f in schema:
            t = f.type
            if f.name == "datetime":          # polars engine yields naive µs; timestamps are UTC
                t = pa.timestamp("ns", tz="UTC")
            elif f.name == "gmt":
                t = pa.string()
            elif f.name in ("trades_count", "quotes_count", "trades_fi_count"):
                t = pa.int64()
            elif pa.types.is_integer(t) or pa.types.is_floating(t) or pa.types.is_null(t):
                t = pa.float64()
            fields.append(pa.field(f.name, t))
        return pa.schema(fields)

    def _is_empty(agg):
        if agg is None:
            return True
        if isinstance(agg, pd.DataFrame):
            return agg.empty
        if _HAS_POLARS and isinstance(agg, pl.DataFrame):
            return agg.height == 0
        return True

    def _append_output(agg):
        nonlocal parquet_writer, parquet_schema, header_written
        if _is_empty(agg):
            return
        if out_format == "parquet":
            table = _to_arrow_table(agg)
            if parquet_writer is None:
                parquet_schema = _canonical_schema(table.schema)
                parquet_writer = pq.ParquetWriter(
                    str(out_file),
                    parquet_schema,
                    compression=parquet_compression or "snappy",
                )
            table = table.select(parquet_schema.names).cast(parquet_schema)
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
                agg = process_shard_folder_polars(shard, tmp_format, direction, winsor)
            else:
                agg = process_shard_folder(shard, tmp_format, direction, winsor)
            if _is_empty(agg):
                empty_shards += 1
                continue
            _append_output(agg)
    else:
        log(f"Phase 2 multiprocessing enabled ({phase2_workers} workers). Output order may be non-deterministic.")
        with timed(f"Phase 2 parallel compute ({staged_root.name})", timings):
            with ProcessPoolExecutor(max_workers=phase2_workers) as ex:
                if engine == "polars":
                    futures = [ex.submit(_process_shard_folder_polars_worker, (shard, tmp_format, direction, winsor)) for shard in shard_dirs]
                else:
                    futures = [ex.submit(_process_shard_folder_worker, (shard, tmp_format, direction, winsor)) for shard in shard_dirs]
                with tqdm(total=len(futures), desc=f"[Phase 2] computing {staged_root.name}", unit="shard") as pbar:
                    for fut in as_completed(futures):
                        _, agg = fut.result()
                        if _is_empty(agg):
                            empty_shards += 1
                        else:
                            _append_output(agg)
                        pbar.update(1)

    if parquet_writer is not None:
        parquet_writer.close()
    if total_shards:
        log(f"Phase 2 empty shards skipped: {empty_shards}/{total_shards}")


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
    p.add_argument("--direction", choices=["tick", "fi", "both"], default=DIRECTION,
                   help="trade direction: Lee-Ready (quote rule + tick rule), Jurkatis (2022) full-information algorithm, or both (default).")
    p.add_argument("--winsor-ticks", action="store_true",
                   help=f"winsorize tick-level measures per ric at {LOWER_WINSOR_Q:.2%}/{UPPER_WINSOR_Q:.2%} (default off).")
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
            if args.temp_format == "parquet" or args.output_format == "parquet":
                _require_pyarrow()

        parquet_partition_cols = _parse_partition_cols(args.temp_partition_cols)
        if args.temp_format == "parquet" and not parquet_partition_cols:
            raise ValueError("Parquet temp format requires at least one partition column.")

        total_ram, avail_ram = _get_ram_info()
        cpu_total = cpu_count() or 1
        log(f"CPU cores available: {cpu_total}")
        log(f"RAM total: {_format_bytes(total_ram)}; RAM available: {_format_bytes(avail_ram)}")
        log(f"Auto-tune: {args.auto_tune}")
        log(f"Found {len(gz_files)} .gz files in {RAW_DIR}")
        log(f"Engine: {args.engine}; direction: {args.direction}; tick winsorization: {args.winsor_ticks}")
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
                if args.engine == "polars" and args.temp_format == "parquet":
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
                    direction=args.direction,
                    winsor=args.winsor_ticks,
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
