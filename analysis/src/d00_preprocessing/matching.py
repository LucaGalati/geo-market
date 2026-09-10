"""Estimate the firm-level matching ONCE on the daily main panel and write the
ric-level assignments used by every downstream script (figures, R)."""
from pathlib import Path
import warnings
warnings.filterwarnings("ignore")

try:
    from . import psm_core as core
except ImportError:
    import psm_core as core

ROOT = Path(__file__).resolve().parents[2]
IN_PANEL = ROOT / "data" / "daily_main.parquet"
OUT_ASSIGN = ROOT / "data" / "psm_assignments.parquet"
OUT_BALANCE = ROOT / "data" / "psm_balance.csv"


def main():
    core.run(IN_PANEL, OUT_ASSIGN, OUT_BALANCE)


if __name__ == "__main__":
    main()
