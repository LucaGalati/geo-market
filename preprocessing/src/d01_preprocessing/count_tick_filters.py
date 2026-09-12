"""Count, over every staged shard (ric x local trading day), how many tick
records each cleaning rule of trth.py removes: crossed quotes, negative
prices/volumes, stub quotes ((ask-bid)/mid > MAX_REL_SPREAD), trades reported in the
major currency unit while quotes are in the minor one (price/mid within MINOR_UNIT_TOL of
MINOR_UNIT_RATIO: rescaled, not dropped) and bad prints (trade more than MAX_TRADE_DEV away
from the prevailing midpoint after the rescaling).
Writes preprocessing/data/02_preprocessed/trth/summary/tick_filter_counts.csv
(one row per TRTH file + TOTAL). Read-only on the shards; no metrics recomputed."""
import sys
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
from trth import MAX_REL_SPREAD, MAX_TRADE_DEV, MINOR_UNIT_RATIO, MINOR_UNIT_TOL, MINOR_UNIT_FACTOR, TMP_DIR, OUT_DIR  # noqa: E402

KEYS = ["quotes", "crossed_quotes", "stub_quotes", "trades", "neg_price_or_volume", "rescaled_prints", "bad_prints"]


def count_shard(shard_dir: Path) -> dict:
    cols = ["is_quote", "is_trade", "price", "volume", "bid", "ask", "datetime"]
    files = sorted(shard_dir.glob("*.parquet")) or sorted(shard_dir.glob("*.csv.gz"))
    if not files:
        return dict.fromkeys(KEYS, 0)
    if files[0].suffix == ".parquet":
        df = pd.concat([pd.read_parquet(f, columns=cols) for f in files], ignore_index=True)
    else:
        df = pd.concat([pd.read_csv(f, compression="gzip", low_memory=False, usecols=cols) for f in files],
                       ignore_index=True)
    for c in ["price", "volume", "bid", "ask"]:
        df[c] = pd.to_numeric(df[c], errors="coerce")
    q, t = df["is_quote"].astype(bool), df["is_trade"].astype(bool)
    out = {"quotes": int(q.sum()), "trades": int(t.sum())}
    crossed = q & df["ask"].notna() & df["bid"].notna() & (df["ask"] < df["bid"])
    out["crossed_quotes"] = int(crossed.sum())
    df = df[~crossed]
    q, t = df["is_quote"].astype(bool), df["is_trade"].astype(bool)
    neg = t & ((df["price"] < 0) | (df["volume"] < 0))
    out["neg_price_or_volume"] = int(neg.sum())
    df = df[~neg]
    q, t = df["is_quote"].astype(bool), df["is_trade"].astype(bool)
    mid = (df["ask"] + df["bid"]) / 2
    stub = q & df["ask"].notna() & df["bid"].notna() & ((df["ask"] - df["bid"]) / mid > MAX_REL_SPREAD)
    out["stub_quotes"] = int(stub.sum())
    df = df[~stub].copy()
    q, t = df["is_quote"].astype(bool), df["is_trade"].astype(bool)
    # prevailing midpoint before each row, within the shard (= one ric x local day)
    df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
    df = df.sort_values("datetime", kind="mergesort")
    mid_quote = ((df["ask"] + df["bid"]) / 2).where(q)
    prev_mid = mid_quote.ffill().shift(1)
    ratio = df["price"] / prev_mid
    minor_unit = t & ratio.notna() & ((ratio / MINOR_UNIT_RATIO - 1).abs() <= MINOR_UNIT_TOL)
    out["rescaled_prints"] = int(minor_unit.sum())
    price = df["price"].where(~minor_unit, df["price"] * MINOR_UNIT_FACTOR)
    bad = t & price.notna() & prev_mid.notna() & ((price / prev_mid - 1).abs() > MAX_TRADE_DEV)
    out["bad_prints"] = int(bad.sum())
    return out


def main(workers: int = 8):
    rows = []
    for staged in sorted(p for p in TMP_DIR.iterdir() if p.is_dir() and p.name.startswith("ukraine")):
        shards = sorted(staged.glob("ric=*/day=*"))
        tot = dict.fromkeys(KEYS, 0)
        with ProcessPoolExecutor(max_workers=workers) as ex:
            for res in ex.map(count_shard, shards, chunksize=64):
                for k in KEYS:
                    tot[k] += res[k]
        tot["file"] = staged.name; tot["shards"] = len(shards)
        rows.append(tot)
        print(f"[{staged.name}] shards={len(shards):,} quotes={tot['quotes']:,} stub={tot['stub_quotes']:,} "
              f"trades={tot['trades']:,} rescaled={tot['rescaled_prints']:,} bad_prints={tot['bad_prints']:,}", flush=True)
    df = pd.DataFrame(rows)
    total = df[KEYS + ["shards"]].sum(); total["file"] = "TOTAL"
    df = pd.concat([df, total.to_frame().T], ignore_index=True)
    for k in ["crossed_quotes", "stub_quotes"]:
        df[k + "_pct_of_quotes"] = 100 * df[k].astype(float) / df["quotes"].astype(float)
    for k in ["neg_price_or_volume", "rescaled_prints", "bad_prints"]:
        df[k + "_pct_of_trades"] = 100 * df[k].astype(float) / df["trades"].astype(float)
    out = OUT_DIR / "summary" / "tick_filter_counts.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out, index=False)
    print(df.tail(1).to_string(index=False))
    print(f"✅ {out}")


if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 8)
