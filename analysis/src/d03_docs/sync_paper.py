"""Copy the pipeline outputs used by the manuscript into docs/paper, so that
docs/paper/manuscript.tex compiles from a single folder: analysis/output/tables ->
docs/paper/tables and analysis/output/figures -> docs/paper/figures (mirror: files
that no longer exist in analysis/output are removed)."""
import shutil
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
SRC = ROOT / "analysis" / "output"
DST = ROOT / "docs" / "paper"


def main():
    if not (DST / "manuscript.tex").exists():
        raise FileNotFoundError(f"{DST} does not hold the manuscript; nothing copied")
    for kind in ("tables", "figures"):
        src, dst = SRC / kind, DST / kind
        if not src.exists():
            raise FileNotFoundError(f"{src} not found: run the pipeline first")
        if dst.exists():
            shutil.rmtree(dst)
        shutil.copytree(src, dst)
        n = sum(1 for p in dst.rglob("*") if p.is_file())
        print(f"✅ {src.relative_to(ROOT)} -> {dst.relative_to(ROOT)} ({n:,} files)")


if __name__ == "__main__":
    main()
