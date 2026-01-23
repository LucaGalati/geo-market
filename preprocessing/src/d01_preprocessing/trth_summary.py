from pathlib import Path
import csv
import gzip
import io
import os
import shutil
import subprocess
from multiprocessing import Pool, cpu_count

# ---------- Config: keep your folder structure exactly ----------
DATA_ROOT = Path(__file__).resolve().parents[2] / "data"
RAW_DIR   = DATA_ROOT / "01_raw" / "trth"
OUT_PATH  = DATA_ROOT / "02_preprocessed" / "trth" / "summary" / "trth_summary.csv"
OUT_PATH.parent.mkdir(parents=True, exist_ok=True)

# ---------- Helpers ----------
def _open_stream(path: Path):
    """
    Return a text file-like stream for the gz file.
    Prefer pigz -dc (multithreaded) when available; fallback to Python gzip.
    """
    pigz = shutil.which("pigz")
    if pigz:
        # pigz -dc <file>
        p = subprocess.Popen([pigz, "-dc", str(path)], stdout=subprocess.PIPE)
        # Wrap binary stdout as text stream
        return io.TextIOWrapper(p.stdout, encoding="utf-8", newline='')
    else:
        # Python gzip (single-threaded)
        return io.TextIOWrapper(gzip.open(path, "rb"), encoding="utf-8", newline='')

def _detect_cols(header_row):
    """
    Find indices for RIC and Type columns (robust to '#RIC' and case).
    """
    lower = [c.strip().lower() for c in header_row]
    ric_idx  = next((i for i, c in enumerate(lower) if c in {"#ric", "ric"}), None)
    type_idx = next((i for i, c in enumerate(lower) if c in {"type", "record type", "recordtype"}), None)
    return ric_idx, type_idx

def _process_one_file(path: Path):
    """
    Process a single gz file and return (n_rows, n_quote, n_trade, ric_set).
    """
    try:
        with _open_stream(path) as f:
            reader = csv.reader(f)
            header = next(reader, None)
            if not header:
                return (0, 0, 0, set())

            ric_idx, type_idx = _detect_cols(header)
            if ric_idx is None or type_idx is None:
                # Skip silently but return zeros
                return (0, 0, 0, set())

            n_rows = n_quote = n_trade = 0
            ric_set = set()

            # speed tricks: localize functions/vars
            add = ric_set.add
            for row in reader:
                n_rows += 1
                # guard for short/empty rows
                if not row or len(row) <= max(ric_idx, type_idx):
                    continue
                ric = row[ric_idx]
                rtyp = row[type_idx]
                if ric:
                    add(ric)
                # faster than .upper(): compare first char when possible
                if rtyp:
                    ch = rtyp[0]
                    if ch == 'Q' or ch == 'q':
                        n_quote += 1
                    elif ch == 'T' or ch == 't':
                        n_trade += 1

            return (n_rows, n_quote, n_trade, ric_set)
    except Exception:
        # If anything goes wrong with this file, don't fail the whole job
        return (0, 0, 0, set())

def main():
    gz_files = sorted(RAW_DIR.glob("*.gz"))
    if not gz_files:
        raise FileNotFoundError(f"No .gz files found in {RAW_DIR}")

    # Use up to N cores (leave 1 core free)
    procs = max(1, min(len(gz_files), cpu_count() - 1))

    # Parallel map
    with Pool(processes=procs) as pool:
        results = pool.map(_process_one_file, gz_files)

    # Reduce results
    total_rows = total_quote = total_trade = 0
    all_ric = set()
    for n_rows, n_quote, n_trade, ric_set in results:
        total_rows += n_rows
        total_quote += n_quote
        total_trade += n_trade
        # RIC cardinality is small relative to rows, set union is cheap
        all_ric |= ric_set

    summary = {
        "unique_RICs": len(all_ric),
        "total_rows": total_rows,
        "quote_rows": total_quote,
        "trade_rows": total_trade,
    }

    # Print
    print("\n================ OVERALL SUMMARY ================\n")
    for k, v in summary.items():
        print(f"{k:15s}: {v:,}")
    print("\n=================================================\n")

    # Save CSV (tiny)
    with OUT_PATH.open("w", encoding="utf-8") as out:
        out.write(",".join(summary.keys()) + "\n")
        out.write(",".join(str(v) for v in summary.values()) + "\n")
    print(f"Summary saved to: {OUT_PATH}")

if __name__ == "__main__":
    main()
