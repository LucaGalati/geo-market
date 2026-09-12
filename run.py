"""Run the pipeline end to end, one script per step, in order.

    python run.py                       # every step except the TRTH tick processing (hours) and the tick-filter counts
    python run.py --list                # show the steps
    python run.py --from gathering      # resume from a step
    python run.py --only matching,docs  # a subset
    python run.py --skip regressions
    python run.py --with-trth           # include trth_summary + trth (needs the raw .gz files, ~8 h)
    python run.py --stage analysis      # one stage only (preprocessing | analysis)

Each step is a subprocess started from the repository root with the Python of this
environment (or Rscript); the root is passed to the scripts as GEO_MARKET_ROOT, so
the repository can live anywhere (spaces in the path are fine). A failing step stops
the run. Data files are never overwritten by hand: rerun the step that produces them.
"""
import argparse
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent
PY = sys.executable
RSCRIPT = shutil.which("Rscript") or "/usr/local/bin/Rscript"

# name, stage, command (relative to ROOT), optional?, description
STEPS = [
    ("sample",       "preprocessing", [PY, "preprocessing/src/d00_gathering/sample.py"], False,
     "Datastream sample: USD prices and market values, headquarters, distances, neighbour dummies"),
    ("trth_summary", "preprocessing", [PY, "preprocessing/src/d01_preprocessing/trth_summary.py"], True,
     "row/quote/trade counts of the raw TRTH files (diagnostic; needs data/01_raw/trth/*.gz)"),
    ("trth",         "preprocessing", [PY, "preprocessing/src/d01_preprocessing/trth.py", "--timings"], True,
     "TRTH tick data -> 5-minute microstructure measures (hours; Phase 1 shards are reused if present)"),
    ("tick_counts",  "preprocessing", [PY, "preprocessing/src/d01_preprocessing/count_tick_filters.py"], True,
     "exact counts of the tick-level filters for the appendix (~1.5 h)"),
    ("fx_rates",     "preprocessing", [PY, "preprocessing/src/d00_gathering/fx_rates.py"], False,
     "ECB reference rates and the currency/unit of every venue"),
    ("merge",        "preprocessing", [PY, "preprocessing/src/d02_panels/merge.py"], False,
     "TRTH x Datastream panels (5-minute and daily), USD conversion, sample screens"),
    ("gathering",    "analysis", [PY, "analysis/src/d00_preprocessing/gathering.py"], False,
     "event-window panels: main (unbalanced) and balanced"),
    ("matching",     "analysis", [PY, "analysis/src/d00_preprocessing/matching.py"], False,
     "firm-level matching (Mahalanobis within a propensity caliper) and entropy-balancing weights"),
    ("fig_daily",    "analysis", [PY, "analysis/src/d01_figures/daily.py"], False, "daily event-study figures"),
    ("fig_intraday", "analysis", [PY, "analysis/src/d01_figures/intraday.py"], False, "open/close-hour figures and t-tests"),
    ("fig_overnight", "analysis", [PY, "analysis/src/d01_figures/intraday_overnight.py"], False, "overnight figure"),
    ("fig_map",      "analysis", [PY, "analysis/src/d01_figures/descriptives.py"], False, "world map and firms-by-country table"),
    ("docs",         "analysis", [PY, "analysis/src/d03_docs/build_sampling_docs.py"], False,
     "sample-selection, currency and balance tables; data-section text"),
    ("regressions",  "analysis", [RSCRIPT, "analysis/src/d02_results/run_all.R"], False,
     "R: descriptives, pre-trends, DiD, dose-response, horse race, mechanism, intraday tables"),
]
NAMES = [s[0] for s in STEPS]


def select(args):
    names = NAMES[:]
    if args.stage:
        names = [n for n, st, *_ in STEPS if st == args.stage]
    if args.only:
        names = [n for n in names if n in args.only.split(",")]
    if args.frm:
        names = names[names.index(args.frm):]
    if args.to:
        names = names[:names.index(args.to) + 1]
    optional = {n for n, _, _, opt, _ in STEPS if opt}
    if not args.with_trth:
        names = [n for n in names if n not in ("trth_summary", "trth") or (args.only and n in args.only.split(","))]
    if not args.with_tick_counts:
        names = [n for n in names if n != "tick_counts" or (args.only and "tick_counts" in args.only.split(","))]
    if args.skip:
        names = [n for n in names if n not in args.skip.split(",")]
    return names


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--list", action="store_true")
    p.add_argument("--stage", choices=["preprocessing", "analysis"])
    p.add_argument("--from", dest="frm", choices=NAMES)
    p.add_argument("--to", choices=NAMES)
    p.add_argument("--only", help="comma-separated step names")
    p.add_argument("--skip", help="comma-separated step names")
    p.add_argument("--with-trth", action="store_true")
    p.add_argument("--with-tick-counts", action="store_true")
    args = p.parse_args(argv)
    if args.list:
        for n, st, cmd, opt, d in STEPS:
            print(f"{n:13s} {st:13s} {'optional' if opt else '':9s} {d}")
        return
    names = select(args)
    env = {**os.environ, "GEO_MARKET_ROOT": str(ROOT)}
    print(f"repository: {ROOT}\nsteps: {', '.join(names)}\n")
    for name in names:
        _, stage, cmd, _, desc = STEPS[NAMES.index(name)]
        print(f"{'#' * 70}\n# {name} ({stage}): {desc}\n{'#' * 70}", flush=True)
        t0 = time.perf_counter()
        r = subprocess.run(cmd, cwd=ROOT, env=env)
        print(f"# {name} finished in {time.perf_counter() - t0:,.0f} s (exit {r.returncode})\n", flush=True)
        if r.returncode != 0:
            sys.exit(f"step '{name}' failed (exit {r.returncode}); fix it and resume with: python run.py --from {name}")
    print("all steps done")


if __name__ == "__main__":
    main()
