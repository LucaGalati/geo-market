# Glossary — geo-market

Event study of the liquidity of stocks around the Russian invasion of Ukraine (24 February 2022), by geographic proximity of the firm to the conflict.

## Samples
- **Main sample** — the *unbalanced* panel: a firm is included if it has data on at least one trading day before **and** one after the event. Maximizes observations while keeping the same firms on both sides. Files `daily_main.parquet`, `intraday_main.parquet`.
- **Balanced sample** — the *perfectly balanced* panel: a firm is included only if it is observed on every trading day of the union calendar of the window. Internal robustness only (a footnote in the paper): on a global panel it excludes markets with local holidays by construction and is a product of selection. Files `daily_balanced.parquet`, `intraday_balanced.parquet`.
- **Mechanism subset** — rows of a sample where the volatility measures are non-missing. A filter applied at analysis time, not a stored file.
- **Window** — 27 January 2022 to 23 March 2022 inclusive (20 trading days before, 20 after).

## Groups
- **Nearby** — firm headquartered in a first- or second-degree neighbour of Ukraine (`nbr_1_or_2 == 1`). Also called *treated*.
- **Distant** — every other firm (`nbr_1_or_2 == 0`). Also called *control*.
- **Full** grouping — Nearby vs Distant over the whole sample.
- **Matched** grouping — Nearby vs Distant restricted to the pairs of the matching (`matched_group` in `psm_assignments.parquet`; unmatched firms excluded).
- **Matching** — one estimation on the pre-period firm-level medians of the daily main sample: Mahalanobis distance on log(market value), log(dollar volume), log(quoted spread), within a propensity caliper of 0.2·SD(logit PS), 1:1 without replacement, optimal assignment.
- **Entropy balancing weights** — `eb_weight`: robustness alternative to matching; control weights that reproduce the treated means of the covariates (treated weight = 1).

## Time
- **Trading day** — the *local* calendar day of the exchange (`date_local`), never the UTC day: Asia-Pacific sessions straddle UTC midnight.
- **Event day** — 24 February 2022 (local trading day). Real-time analyses use UTC and the 03:53 UTC missile strikes.
- **Pre / post** — before vs on-or-after the event day.

## Measures (all from TRTH tick data, 5-minute aggregation, then daily means)
- **Quoted spread** — `qspread_mean`: (ask − bid) / midpoint, full spread.
- **Effective spread** — `espread_mean`: |price − midpoint| / midpoint, a *half*-spread (the signed ×2 versions are retired).
- **Price impact** — `price_impact_mean`: D·(m₅ − m)/m with D the Lee–Ready direction and m₅ the prevailing midpoint 5 minutes later, computed only when both midpoints fall in the same local trading day. `price_impact_fi_mean` is the same measure with the FI direction (available after the next TRTH run).
- **Realized spread** — `realized_spread_mean` = effective spread − price impact (half-spread convention); `realized_spread_fi_mean` with the FI direction.
- **Trade direction** — the sign D of a trade: +1 buyer-initiated, −1 seller-initiated. *Lee–Ready*: quote rule (sign of the trade price minus the prevailing midpoint) and, for trades at the midpoint or without a prevailing quote, the tick rule (sign of the last price change between trades of the same firm-day); only the first trade of a day can stay unsigned. *FI direction* (`direction_fi`): the full-information algorithm of Jurkatis (2022), which reads the direction off the quote-size changes around the trade and falls back to the tick rule. The current TRTH files (Sept 2026) still carry the quote rule alone; Lee–Ready and FI arrive with the next TRTH run.
- **Volatility** — `volatility` / `log_volatility`: standard deviation over the local day of the simple / log returns of the last prevailing midpoint of consecutive 5-minute intervals. The price-level volatilities and the signed effective spreads are retired.
- **Dollar volume, depth value** — `dollar_volume_*`, `dvolume_*`, `depth_value_*`: in **U.S. dollars**, converted from the currency of the exchange with the daily ECB reference rate (`fx_local_per_usd`, local units per USD, times 100 for venues quoting in pence, cents or agorot); the few venues whose currency the ECB does not quote use the exchange-day rate implied by the Datastream USD close (`fx_source`). Listings quoted in a currency other than that of their exchange are excluded. Prices and quotes themselves (`price_mean`, spreads) stay in local currency; spreads and impacts are relative, so no conversion is needed.
- **Penny stock** — a firm-day whose Datastream price (USD) is at or below one dollar or whose local trade price is at or below one unit; excluded.
- **Stub quote** — a quote whose relative spread exceeds 25% of the midpoint (e.g. bid 0.01 / ask 100); removed at the source.
- **Winsorization** — 1st/99th percentile clipping of a measure across firm-days, applied once, at analysis time, in the daily figures and regressions; the intraday event study and the stored panels are raw. The current TRTH files also carry a tick-level clipping at 0.1/99.99 within firm and day; from the next TRTH run this is off by default.
- **Bad print** — a trade printed more than 50% away from the prevailing midpoint; removed at the source.

## Added September 2026
- **Matched main sample** — the main (unbalanced) sample restricted to the pairs of the matching; the headline specification of the paper. The balanced sample and the entropy-balanced weighting are robustness variants.
- **Entropy-balanced group** — Nearby = all treated firms (weight 1) vs Distant = all control firms weighted by `eb_weight`; a robustness alternative to the matched pairs.
- **Overnight window** — the fixed interval 23:00 UTC on 23 February to 07:00 UTC on 24 February 2022 over which the overnight figure is drawn for every sample; only Distant firms trade in it.
- **Sample-selection log** — `analysis/output/sampling/sampling_log.csv`, one row per selection step written by each pipeline stage; the source of the sample table and of the data-section numbers.

## Regressions (September 2026)
- **War** — the post-invasion indicator: 1 on and after 24 February 2022 (`post`). The difference-in-differences coefficient is the treatment × War interaction with firm and day fixed effects and standard errors clustered by firm and day.
- **Negative distance** — minus the great-circle distance of the headquarters from Ukraine in thousands of km (`negdist`); the linear continuous treatment, so that a positive coefficient means a larger effect for closer firms.
- **Dose** — proximity to Ukraine, 3,000 km minus the distance (in thousands of km), for the dose-response analysis; firms beyond 3,000 km are the untreated reference. **ATT(d)**: effect for the firms of a distance bin; **ACRT(d)**: change of the effect per 1,000 km of proximity between adjacent bins.
- **War-day return** — the firm's log return on 24 February 2022 (`ret_war`); the economic treatment.
- **Pre-war volatility** — the firm's average volatility before the invasion, standardized across the firms of the sample (`prevol_z`).
- **Intensity** — Neighbor₁,₂ × negative distance: the neighbour effect scaled by proximity.
- **Decomposition** — effective half-spread = realized half-spread + price impact; used to attribute the spread response to adverse selection (price impact) or to liquidity providers' revenues (realized spread).
