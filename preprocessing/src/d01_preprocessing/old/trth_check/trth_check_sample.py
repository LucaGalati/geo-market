from pathlib import Path
import pandas as pd
import numpy as np

DATA_ROOT = Path(__file__).resolve().parents[3] / "data"
RAW_DIR   = DATA_ROOT / "01_raw" / "trth"
OUT_DIR   = DATA_ROOT / "02_preprocessed" / "trth" / "checks"
OUT_DIR.mkdir(parents=True, exist_ok=True)

OUT_TICKS = OUT_DIR / "ticks_sample.csv"

PAIRS = [
    ("AKBNK.IS", "2022-02-04"),
    ("AKSA.IS",  "2022-01-26"),
]
RICS  = set([r for r, _ in PAIRS])
DAYS  = set([d for _, d in PAIRS])

def parse_gmt_to_hours(gmt_value) -> float:
    if pd.isna(gmt_value): return np.nan
    s = str(gmt_value).strip()
    if s == "": return np.nan
    sign = 1
    if s.startswith("-"):
        sign = -1; s = s[1:]
    elif s.startswith("+"):
        s = s[1:]
    try:
        return sign * float(s)
    except Exception:
        return np.nan

def main():
    kept = []
    gz_files = sorted(RAW_DIR.glob("*.gz"))
    if not gz_files:
        raise FileNotFoundError(f"No .gz files in {RAW_DIR}")

    for gz in gz_files:
        for chunk in pd.read_csv(gz, compression="gzip", low_memory=False, chunksize=250_000):
            # Normalize needed columns
            chunk = chunk.rename(columns={
                "#RIC":"ric", "Date-Time":"datetime", "GMT Offset":"gmt",
                "Type":"type", "Price":"price", "Volume":"volume",
                "Bid Price":"bid", "Ask Price":"ask",
            })
            if "ric" not in chunk or "datetime" not in chunk or "gmt" not in chunk:
                continue

            # Filter by RIC early
            chunk = chunk[chunk["ric"].isin(RICS)]
            if chunk.empty: 
                continue

            # Parse time + local day (like pipeline)
            chunk["datetime"] = pd.to_datetime(chunk["datetime"], utc=True, errors="coerce")
            gmt_hours = chunk["gmt"].apply(parse_gmt_to_hours)
            local_offset = pd.to_timedelta(gmt_hours, unit="h")
            chunk["date_local"] = (chunk["datetime"] + local_offset).dt.date.astype(str)

            # Keep only target local days
            sub = chunk[chunk["date_local"].isin(DAYS)]
            if not sub.empty:
                kept.append(sub[["ric","datetime","gmt","type","price","volume","bid","ask"]])

    if not kept:
        raise RuntimeError("No rows matched the requested RIC×day pairs.")

    sample = pd.concat(kept, ignore_index=True)
    sample.to_csv(OUT_TICKS, index=False)
    print(f"✅ Wrote sample ticks: {OUT_TICKS} ({len(sample):,} rows)")

if __name__ == "__main__":
    main()