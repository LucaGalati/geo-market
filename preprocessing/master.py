"""Preprocessing stage only (Datastream sample, ECB rates, panels). Same flags as run.py;
the TRTH tick processing is included with --with-trth."""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from run import main  # noqa: E402

if __name__ == "__main__":
    main(["--stage", "preprocessing", *sys.argv[1:]])
