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
Rscript analysis/R/requirements.R                                  # R >= 4.4: arrow, data.table, fixest >= 0.14, contdid, ggplot2
python run.py                                                      # everything except the tick processing
```

`run.py` executes the steps below in order as subprocesses from the repository root, passing the
root to every script (`GEO_MARKET_ROOT`), so the folder can live anywhere. Useful variants:

```bash
python run.py --list                 # the steps
python run.py --from gathering       # resume from a step
python run.py --only matching,docs   # a subset
python run.py --with-trth            # include the TRTH tick processing (needs the raw .gz files; hours)
python preprocessing/master.py       # one stage (same flags)
python analysis/main.py
```

| step | script | output |
|---|---|---|
| sample | `preprocessing/src/d00_gathering/sample.py` | `data/02_preprocessed/sample.csv` |
| trth (optional) | `preprocessing/src/d01_preprocessing/trth.py` | `data/02_preprocessed/trth/ukraine*.parquet` (Phase 1 shards in `tmp/` are reused when they carry the `_PHASE1_COMPLETE` marker) |
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

## Rerun by hand

Same order as `run.py`, from the repository root with the conda env active (prefix long runs with
`caffeinate -s`). Each step reads the outputs of the steps above it, so after changing one step rerun
it and everything below; `python run.py --from <step>` does exactly that.

| # | command | produces | time |
|---|---|---|---|
| 1 | `python preprocessing/src/d00_gathering/sample.py` | `preprocessing/data/02_preprocessed/sample.csv` (Datastream universe, USD prices, distances, neighbour dummies) | minutes |
| 2 | `python preprocessing/src/d01_preprocessing/trth.py --timings` | `preprocessing/data/02_preprocessed/trth/ukraineNN.parquet` (five-minute measures). Needs the raw `.gz` files; Phase 1 is skipped for every `tmp/ukraineNN/` folder already staged, so a rerun after a change in the measures redoes only Phase 2 | 2-3 h with the default workers (cores - 2); 16 h with `--phase2-workers 1` |
| 3 | `python preprocessing/src/d01_preprocessing/count_tick_filters.py` | `.../trth/summary/tick_filter_counts.csv` (numbers of the tick filters in the appendix). Only when the tick filters change | 1.5 h |
| 4 | `python preprocessing/src/d00_gathering/fx_rates.py` | `preprocessing/data/01_raw/handcoded/{fx_rates_ecb,venue_currency}.csv` (ECB rates, currency and unit of every venue) | seconds |
| 5 | `python preprocessing/src/d02_panels/merge.py` | `preprocessing/data/03_output/{intraday,daily}{,_AERO_DEF,_RUS_UKR}.parquet` (TRTH x Datastream, USD conversion, screens; picks, per `ukraineNN`, the newest of `.parquet`/`.csv`) | 25 min |
| 6 | `python analysis/src/d00_preprocessing/gathering.py` | `analysis/data/{daily,intraday}_{main,balanced}.parquet` (event window, main and balanced panels) | minutes |
| 7 | `python analysis/src/d00_preprocessing/matching.py` | `analysis/data/psm_assignments.parquet`, `psm_balance.csv` (matched pairs, entropy-balancing weights) | minutes |
| 8 | `python analysis/src/d01_figures/daily.py`, `intraday.py`, `intraday_overnight.py`, `descriptives.py` | `analysis/output/figures/{daily,intraday,overnight,map}/{sample}/{group}/`, `analysis/output/tables/{firms_by_country,...}` | minutes |
| 9 | `python analysis/src/d03_docs/build_sampling_docs.py` | `analysis/output/tables/{sample_selection,currencies,matching/balance}.tex`, `docs/paper/sampling/{data_section,formulas_appendix}.tex` (numbers filled in) | seconds |
| 10 | `Rscript analysis/src/d02_results/run_all.R` | `analysis/output/tables/regressions/{matched,main,internal/*}/*.tex`, `analysis/output/figures/dose_response/` | 20 min |

Typical partial reruns: new Datastream file -> from 1; new measures in `trth.py` -> from 2 (3 only if
the tick filters changed); new screens or currency rules in `merge.py` -> `python run.py --from merge`;
new matching or figures -> `python run.py --from matching`; regressions only ->
`python run.py --only regressions`.

The stage READMEs (`preprocessing/README`, `analysis/README`) describe each script and its flags.
Tested on 11 September 2026: MacBook Pro M4 Max (10 cores, 32 GB), macOS 26, R 4.5.3, Python 3.10.
