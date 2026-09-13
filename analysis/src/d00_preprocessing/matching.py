"""Estimate the firm-level matching on the daily main panel and write the ric-level
assignments used by every downstream script (figures, R): the main reference
window under the standard file names, and every candidate window under a tagged
name for the appendix table (matching_windows.R)."""
import argparse
from pathlib import Path
import warnings
warnings.filterwarnings("ignore")

try:
    from . import psm_core as core
except ImportError:
    import psm_core as core

ROOT = Path(__file__).resolve().parents[2]
IN_PANEL = ROOT / "data" / "daily_main.parquet"
DATA = ROOT / "data"


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--main-only", action="store_true", help="skip the candidate windows of the appendix table")
    args = p.parse_args()
    core.run_windows(IN_PANEL, DATA, main_only=args.main_only)


if __name__ == "__main__":
    main()
