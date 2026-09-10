"""Sample-selection log shared by every pipeline stage: one row per
(stage, step) in analysis/output/sampling/sampling_log.csv. A stage calls
reset() when it starts and log() after each cut, so the table always reflects
the last complete run of each stage."""
import csv
import datetime as dt
from pathlib import Path

LOG = Path(__file__).resolve().parents[2] / "output" / "sampling" / "sampling_log.csv"
FIELDS = ["stage", "order", "step", "firms", "rows", "note", "timestamp"]
STAGE_RANK = {"trth": 0, "merge": 1, "gathering": 2, "matching": 3, "figures": 4}


def _read():
    if not LOG.exists():
        return []
    with open(LOG, newline="") as f:
        return list(csv.DictReader(f))


def _write(rows):
    LOG.parent.mkdir(parents=True, exist_ok=True)
    rows.sort(key=lambda r: (STAGE_RANK.get(r["stage"], 99), int(r["order"])))
    with open(LOG, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)


def reset(stage: str):
    _write([r for r in _read() if r["stage"] != stage])


def log(stage: str, step: str, firms=None, rows=None, note: str = ""):
    existing = [r for r in _read() if not (r["stage"] == stage and r["step"] == step)]
    order = 1 + max([int(r["order"]) for r in existing if r["stage"] == stage], default=0)
    existing.append({
        "stage": stage, "order": order, "step": step,
        "firms": "" if firms is None else int(firms),
        "rows": "" if rows is None else int(rows),
        "note": note, "timestamp": dt.datetime.now().isoformat(timespec="seconds"),
    })
    _write(existing)
