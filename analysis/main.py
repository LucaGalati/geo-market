"""Analysis stage only (panels, matching, figures, tables, R regressions). Same flags as run.py."""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from run import main  # noqa: E402

if __name__ == "__main__":
    main(["--stage", "analysis", *sys.argv[1:]])
