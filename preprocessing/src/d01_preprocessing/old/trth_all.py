from pathlib import Path
import pyarrow as pa
import pyarrow.csv as pacsv
import pyarrow.parquet as pq
from tqdm import tqdm  # progress bar

def main():
    # Define paths
    here = Path(__file__).resolve()
    data_root = here.parents[2] / "data"
    raw_dir = data_root / "01_raw" / "trth"
    out_path = data_root / "02_preprocessed" / "trth.parquet"
    out_path.parent.mkdir(parents=True, exist_ok=True)

    # Find ONLY .gz files in raw directory
    gz_files = sorted([f for f in raw_dir.glob("*.gz") if f.is_file()])
    if not gz_files:
        raise FileNotFoundError(f"No .gz files found in {raw_dir}")

    print(f"Found {len(gz_files)} gzipped files in {raw_dir}")

    # Column renaming map (simplified)
    rename_map = {
        "RIC": "ric",
        "Date-Time": "datetime",
        "GMT Offset": "gmt",
        "Type": "type",
        "Price": "price",
        "Volume": "volume",
        "Bid Price": "bid",
        "Bid Size": "bid_size",
        "Ask Price": "ask",
        "Ask Size": "ask_size",
        "Exch Time": "time",  # nanosecond timestamp
    }

    # Arrow column schema
    column_types = {
    	"RIC": pa.string(),
    	"Date-Time": pa.timestamp("ns", tz="UTC"),
    	"GMT Offset": pa.string(),
    	"Type": pa.string(),
    	"Price": pa.float32(),
    	"Volume": pa.int64(),       
    	"Bid Price": pa.float32(),
    	"Bid Size": pa.int64(),     
    	"Ask Price": pa.float32(),
   	"Ask Size": pa.int64(),     
   	"Exch Time": pa.string(),   # keep as string (HH:MM:SS.nnnnnnnnn)
    	"Domain": pa.string(),
}


    # Reading options
    read_opts = pacsv.ReadOptions(block_size=1 << 20, use_threads=True)
    parse_opts = pacsv.ParseOptions(delimiter=",", quote_char='"')
    conv_opts = pacsv.ConvertOptions(column_types=column_types, strings_can_be_null=True)

    writer = None

    # Process files with progress bar
    for i, gz_file in enumerate(tqdm(gz_files, desc="Processing .gz files", unit="file")):
        reader = pacsv.open_csv(gz_file, read_options=read_opts,
                                parse_options=parse_opts, convert_options=conv_opts)

        for batch in reader:
            # Drop Domain column if present
            cols = [c for c in batch.schema.names if c != "Domain"]
            batch = batch.select(cols)

            # Rename columns to lowercase simpler names
            rename_pairs = [(old, rename_map.get(old, old.lower())) for old in batch.schema.names]
            batch = batch.rename_columns([new for _, new in rename_pairs])

            if writer is None:
                writer = pq.ParquetWriter(out_path, schema=batch.schema, compression="snappy")

            writer.write_batch(batch)

    if writer:
        writer.close()

    print(f"\n Done. Combined Parquet file written to: {out_path}")
    print("You can read it later with:")
    print(f"  import pandas as pd; df = pd.read_parquet('{out_path}')")

if __name__ == "__main__":
    main()
