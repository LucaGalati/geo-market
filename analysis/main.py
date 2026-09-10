"""Run the analysis stage end to end: panels → matching → figures → R tables.
Inputs: preprocessing/data/03_output/*.parquet (from preprocessing/master.py)."""
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

from src.d00_preprocessing import gathering, matching  # noqa: E402

print('###################################################')
print('Build panels (main / perfectly balanced) ...')
print('###################################################\n')
gathering.main()

print('###################################################')
print('Firm-level matching (Mahalanobis-in-caliper + entropy balancing) ...')
print('###################################################\n')
matching.main()

print('###################################################')
print('Figures (2x2 grid: sample x group) ...')
print('###################################################\n')
import src.d01_figures.daily              # noqa: E402,F401  (runs on import)
import src.d01_figures.intraday           # noqa: E402,F401
import src.d01_figures.intraday_overnight  # noqa: E402,F401
import src.d01_figures.descriptives       # noqa: E402,F401

print('###################################################')
print('Sample-selection tables and paper text ...')
print('###################################################\n')
from src.d03_docs import build_sampling_docs  # noqa: E402
build_sampling_docs.main()

print('###################################################')
print('R tables ...')
print('###################################################\n')
for script in ["daily.R", "intraday.R", "descriptives.R"]:
    subprocess.run(["Rscript", str(HERE / "src" / "d02_results" / script)], check=False)
