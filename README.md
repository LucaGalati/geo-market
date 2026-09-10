# geo-market

Liquidity of global equities around the Russian invasion of Ukraine (24 February 2022):
firms headquartered in first- or second-degree neighbours of Ukraine ("Nearby") against the
rest of the world ("Distant"), with LSEG Tick History trades and quotes aggregated to five-minute
intervals and local trading days.

The repository has two stages:

- `preprocessing/` (Python): Datastream sample (USD prices, market values, headquarters and
  distances), TRTH tick data -> five-minute microstructure measures, ECB exchange rates,
  merged five-minute and daily panels (`preprocessing/data/03_output/*.parquet`).
- `analysis/` (Python + R): event-window panels (main = unbalanced, balanced = robustness),
  firm-level matching (Mahalanobis within a propensity caliper, entropy-balancing weights),
  figures, sample-selection tables and paper text, and the R regressions
  (difference-in-differences with firm and day fixed effects, standard errors clustered by
  firm and day; dose-response in distance; horse race with economic exposure; mechanism).

`CONTEXT.md` is the glossary of samples, groups and measures. Pipeline outputs live under
`analysis/output/{figures,tables}` and are copied into the paper by hand; `docs/` (paper drafts
and notes) is not part of the repository.

## Run

```bash
conda env create -f environment.yml && conda activate geo-market   # Python 3.10 + pandas/polars/pyarrow
Rscript analysis/R/requirements.R                                  # R >= 4.4: arrow, data.table, fixest, contdid, ggplot2
python run.py                                                      # everything except the tick processing
```

`run.py` executes the steps below in order as subprocesses from the repository root, passing the
root to every script (`GEO_MARKET_ROOT`), so the folder can live anywhere. Useful variants:

```bash
python run.py --list                 # the steps
python run.py --from gathering       # resume from a step
python run.py --only matching,docs   # a subset
python run.py --with-trth            # include the TRTH tick processing (needs the raw .gz files; ~8 hours)
python preprocessing/master.py       # one stage (same flags)
python analysis/main.py
```

| step | script | output |
|---|---|---|
| sample | `preprocessing/src/d00_gathering/sample.py` | `data/02_preprocessed/sample.csv` |
| trth (optional) | `preprocessing/src/d01_preprocessing/trth.py` | `data/02_preprocessed/trth/ukraine*.parquet` (Phase 1 shards in `tmp/` are reused) |
| tick_counts (optional) | `preprocessing/src/d01_preprocessing/count_tick_filters.py` | counts of the tick filters for the appendix |
| fx_rates | `preprocessing/src/d00_gathering/fx_rates.py` | `data/01_raw/handcoded/{fx_rates_ecb,venue_currency}.csv` |
| merge | `preprocessing/src/d02_panels/merge.py` | `data/03_output/{intraday,daily}*.parquet` |
| gathering | `analysis/src/d00_preprocessing/gathering.py` | `analysis/data/{daily,intraday}_{main,balanced}.parquet` |
| matching | `analysis/src/d00_preprocessing/matching.py` | `analysis/data/psm_assignments.parquet`, `psm_balance.csv` |
| fig_* | `analysis/src/d01_figures/*.py` | `analysis/output/figures/{daily,intraday,overnight,map}/{sample}/{group}/` |
| docs | `analysis/src/d03_docs/build_sampling_docs.py` | `analysis/output/tables/{sample_selection,currencies,matching/balance}.tex`, data-section text |
| regressions | `analysis/src/d02_results/run_all.R` | `analysis/output/tables/regressions/{matched,main,internal/*}/*.tex` |

Samples and groups: `main` (firms with data before and after the invasion) and `balanced`
(firms on every trading day); `full` (Nearby vs Distant), `matched` (pairs of the matching),
`eb` (entropy-balanced controls). The paper's headline is the matched sample; the main sample
is the appendix; balanced and entropy-balanced results are produced under `internal/` only.

The stage READMEs (`preprocessing/README`, `analysis/README`) describe each script and its flags.
Tested on 11 September 2026: MacBook Pro M4 Max (10 cores, 32 GB), macOS 26, R 4.5.3, Python 3.10.
