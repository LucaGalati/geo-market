"""Shared helpers for the figure scripts: which panel to load, how to define
the two groups, and analysis-level winsorization.

sample : "main" (unbalanced panel) | "balanced" (perfectly balanced panel)
group  : "full" (Nearby = nbr_1_or_2 == 1, everyone else Distant)
         "matched" (Nearby/Distant = treated/control of the matching in
                    psm_assignments.parquet; unmatched firms dropped)
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "d00_preprocessing"))
import sampling_log as slog  # noqa: E402

DATA_ROOT = Path(__file__).resolve().parents[2] / "data"
OUTPUT_ROOT = Path(__file__).resolve().parents[2] / "output"
# pipeline outputs: analysis/output/{figures,tables}/{kind}/{sample}/{group}/...
# (copied by hand into docs/paper when the manuscript is compiled)
FIGURES_ROOT = OUTPUT_ROOT / "figures"
TABLES_ROOT = OUTPUT_ROOT / "tables"
ASSIGNMENTS = DATA_ROOT / "psm_assignments.parquet"

SAMPLES = ("main", "balanced")
GROUPS = ("full", "matched", "eb")
SAMPLE_TITLE = {"main": "Main sample (unbalanced)", "balanced": "Perfectly balanced sample"}
GROUP_TITLE = {"full": "Nearby vs Distant", "matched": "Matched pairs", "eb": "Entropy-balanced controls"}
WINSOR = (0.01, 0.99)  # per-variable, firm-day pooled, inside the plotted sample
LABEL_MAP = {1: "Nearby", 0: "Distant"}


def panel_path(kind: str, sample: str) -> Path:
    return DATA_ROOT / f"{kind}_{sample}.parquet"


def available_columns(path: Path):
    return set(pq.read_schema(path).names)


def load_panel(kind: str, sample: str, columns):
    """Read only `columns` (those present) from the panel."""
    path = panel_path(kind, sample)
    if not path.exists():
        raise FileNotFoundError(f"{path} not found — run gathering.py first.")
    cols = [c for c in dict.fromkeys(columns) if c in available_columns(path)]
    return pd.read_parquet(path, columns=cols)


def assign_groups(df: pd.DataFrame, group: str, label_col: str = "group",
                  stage: str = None, sample: str = None) -> pd.DataFrame:
    """Add `label_col` = Nearby/Distant and a weight column `w` according to
    `group`; drop unlabeled rows. With `stage`/`sample` the group sizes are
    written to the sample-selection log.
      full    : Nearby = nbr_1_or_2 == 1, everyone else Distant, w = 1
      matched : treated/control pairs of the matching, w = 1
      eb      : all treated (w = 1) vs all controls weighted by eb_weight"""
    if group == "full":
        df[label_col] = pd.to_numeric(df["nbr_1_or_2"], errors="coerce").map(LABEL_MAP)
        df["w"] = 1.0
    elif group in ("matched", "eb"):
        if not ASSIGNMENTS.exists():
            raise FileNotFoundError(f"{ASSIGNMENTS} not found — run matching.py first.")
        a = pd.read_parquet(ASSIGNMENTS, columns=["ric", "nbr_1_or_2", "matched_group", "eb_weight"])
        if group == "matched":
            a = a[a["matched_group"].isin([0, 1])]
            a[label_col] = a["matched_group"].map(LABEL_MAP)
            a["w"] = 1.0
        else:
            a = a[a["eb_weight"].notna()]
            a[label_col] = a["nbr_1_or_2"].map(LABEL_MAP)
            a["w"] = a["eb_weight"].astype(float)
        df = df.merge(a[["ric", label_col, "w"]], on="ric", how="inner")
    else:
        raise ValueError(f"Unknown group: {group}")
    df = df[df[label_col].notna()].copy()
    if stage:
        n = df.groupby(label_col)["ric"].nunique()
        slog.log(stage, f"{sample} | {group}", firms=df["ric"].nunique(), rows=len(df),
                 note=f"Nearby {int(n.get('Nearby', 0)):,}, Distant {int(n.get('Distant', 0)):,}")
    return df


def winsorize(df: pd.DataFrame, cols, limits=WINSOR) -> pd.DataFrame:
    """Clip each column at its pooled quantiles (NaN-aware). Applied at analysis
    time only; the stored panels stay raw."""
    lo, hi = limits
    for c in cols:
        s = df[c]
        if s.notna().sum() > 1:
            q = s.quantile([lo, hi])
            df[c] = s.clip(lower=q.iloc[0], upper=q.iloc[1])
    return df


def wmean_by(df: pd.DataFrame, by, cols, w: str = "w") -> pd.DataFrame:
    """Weighted mean of `cols` by `by`, NaN-aware per column (weights renormalized
    over the non-missing observations of each column)."""
    keys = [df[b] for b in by]
    out = {}
    for c in cols:
        m = df[c].notna()
        num = (df[c].fillna(0) * df[w] * m).groupby(keys, dropna=False).sum()
        den = (df[w] * m).groupby(keys, dropna=False).sum()
        out[c] = num / den.replace(0, np.nan)
    return pd.DataFrame(out).reset_index()


def wstats(x, w):
    """Weighted mean, unbiased (reliability-weights) variance and effective n.
    With unit weights this is exactly the sample mean, var(ddof=1) and n."""
    x = np.asarray(x, dtype=float); w = np.asarray(w, dtype=float)
    ok = np.isfinite(x) & np.isfinite(w)
    x, w = x[ok], w[ok]
    sw = w.sum()
    if sw <= 0:
        return np.nan, np.nan, 0.0
    m = np.sum(w * x) / sw
    denom = sw - np.sum(w ** 2) / sw
    v = np.sum(w * (x - m) ** 2) / denom if denom > 0 else np.nan
    n_eff = sw ** 2 / np.sum(w ** 2)
    return m, v, n_eff


def out_dir(kind: str, sample: str, group: str, *parts, root: Path = None) -> Path:
    d = (root or FIGURES_ROOT) / kind / sample / group
    for p in parts:
        d = d / p
    d.mkdir(parents=True, exist_ok=True)
    return d


def tab_dir(kind: str, sample: str, group: str, *parts) -> Path:
    return out_dir(kind, sample, group, *parts, root=TABLES_ROOT)
