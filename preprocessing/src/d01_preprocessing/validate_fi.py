"""Validate the Jurkatis (2022) FI trade classification against the tick test on a
random sample of real TRTH shards (no re-run of the pipeline). Writes
<OUT_DIR>/summary/fi_validation.csv (one row per shard) and prints the summary.
Usage: python validate_fi.py [--shards 200] [--workers 3] [--seed 7]"""
import argparse, random, time, warnings
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path
import numpy as np, pandas as pd
import trth

warnings.filterwarnings("ignore")


def one(shard: Path):
    t0 = time.perf_counter()
    agg, df = trth.process_shard_folder(shard, "csv", "both", False, return_ticks=True)
    dt = time.perf_counter() - t0
    if not isinstance(df, pd.DataFrame) or df.empty or "direction_fi" not in df.columns:
        return None
    tr = df[df["is_trade"] & df["price"].notna()]
    if tr.empty:
        return None
    tick = tr["direction"].isin([1.0, -1.0]); fi = tr["direction_fi"].isin([1.0, -1.0])
    quote_rule = (tr["lr_step"] == 1)
    both = tick & fi
    agree = float((tr.loc[both, "direction"] == tr.loc[both, "direction_fi"]).mean()) if both.any() else np.nan
    pi_t, pi_f = tr["price_impact"].dropna(), tr["price_impact_fi"].dropna()
    a = agg.dropna(subset=["price_impact_mean", "price_impact_fi_mean"])
    return dict(
        shard=f"{shard.parent.parent.name}/{shard.parent.name}/{shard.name}", ric=tr["ric"].iloc[0],
        suffix=str(tr["ric"].iloc[0]).rsplit(".", 1)[-1] if "." in str(tr["ric"].iloc[0]) else "NY",
        seconds=dt, n_trades=len(tr), n_quotes=int(df["is_quote"].sum()),
        tick_classified=float(tick.mean()), quote_rule_share=float(quote_rule.mean()), fi_classified=float(fi.mean()),
        fi_proper=float((tr["fi_step"] < 4).mean()), agreement=agree,
        buy_share_tick=float((tr.loc[tick, "direction"] == 1).mean()) if tick.any() else np.nan,
        buy_share_fi=float((tr.loc[fi, "direction_fi"] == 1).mean()) if fi.any() else np.nan,
        pi_tick_mean_bps=pi_t.mean() * 1e4, pi_fi_mean_bps=pi_f.mean() * 1e4,
        pi_tick_absmax_bps=pi_t.abs().max() * 1e4, pi_fi_absmax_bps=pi_f.abs().max() * 1e4,
        pi_tick_p9999_bps=pi_t.abs().quantile(0.9999) * 1e4 if len(pi_t) else np.nan,
        corr_5m=float(a["price_impact_mean"].corr(a["price_impact_fi_mean"])) if len(a) > 2 else np.nan,
        quotes_no_size=int(((df["is_quote"]) & ((df["ask"].notna() & df["ask_size"].isna()) | (df["bid"].notna() & df["bid_size"].isna()))).sum()),
    )


def main():
    ap = argparse.ArgumentParser(); ap.add_argument("--shards", type=int, default=200)
    ap.add_argument("--workers", type=int, default=3); ap.add_argument("--seed", type=int, default=7)
    args = ap.parse_args()
    rng = random.Random(args.seed)
    files = sorted(trth.TMP_DIR.glob("ukraine*"))
    pool = []
    for f in files:                      # a few random shards per TRTH file so every market is represented
        rics = sorted(f.glob("ric=*"))
        for r in rng.sample(rics, min(len(rics), max(1, args.shards // len(files) + 1))):
            days = sorted(r.glob("day=*"))
            if days:
                pool.append(rng.choice(days))
    shards = rng.sample(pool, min(args.shards, len(pool)))
    print(f"validating FI on {len(shards)} shards from {len(files)} files with {args.workers} workers")
    rows = []
    with ProcessPoolExecutor(max_workers=args.workers) as ex:
        futs = {ex.submit(one, s): s for s in shards}
        for i, fut in enumerate(as_completed(futs), 1):
            try:
                r = fut.result()
            except Exception as e:
                print(f"  ! {futs[fut]}: {e!r}"); r = None
            if r:
                rows.append(r)
            if i % 25 == 0:
                print(f"  {i}/{len(shards)} done")
    out = pd.DataFrame(rows)
    dest = trth.OUT_DIR / "summary"; dest.mkdir(parents=True, exist_ok=True)
    out.to_csv(dest / "fi_validation.csv", index=False)
    w = out["n_trades"]
    def wm(c): return float(np.average(out[c].fillna(0), weights=w)) if len(out) else np.nan
    print("\n=== FI validation (trade-weighted over shards with trades) ===")
    print(f"shards {len(out)} | trades {int(w.sum()):,} | exchanges {out['suffix'].nunique()}")
    print(f"seconds/shard: median {out['seconds'].median():.2f}  mean {out['seconds'].mean():.2f}  max {out['seconds'].max():.1f}")
    print(f"classified: Lee-Ready {wm('tick_classified'):.3f} (quote rule {wm('quote_rule_share'):.3f}, tick rule the rest) | FI (incl. tick fallback) {wm('fi_classified'):.3f} | FI-proper steps 1-3 {wm('fi_proper'):.3f}")
    print(f"agreement tick vs FI where both classified: {wm('agreement'):.3f}  (median shard {out['agreement'].median():.3f})")
    print(f"buy share: tick {wm('buy_share_tick'):.3f} | FI {wm('buy_share_fi'):.3f}")
    print(f"price impact (bps, same day): tick {wm('pi_tick_mean_bps'):.2f} | FI {wm('pi_fi_mean_bps'):.2f}; corr of 5-min means median {out['corr_5m'].median():.3f}")
    print(f"no winsor tails: |pi| max over shards tick {out['pi_tick_absmax_bps'].max():.0f} bps, FI {out['pi_fi_absmax_bps'].max():.0f} bps; median shard p99.99 {out['pi_tick_p9999_bps'].median():.0f} bps")
    print(f"quotes with price but no size (dropped from FI input): {int(out['quotes_no_size'].sum()):,} of {int(out['n_quotes'].sum()):,}")
    print(f"-> {dest / 'fi_validation.csv'}")


if __name__ == "__main__":
    main()
