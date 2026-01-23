from pathlib import Path
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler

# ---------- PATHS ----------
ROOT = Path(__file__).resolve().parents[2]
IN1  = ROOT / "data" / "intraday_balanced.csv"
OUT_MATCHED = ROOT / "data" / "intraday_matched_psm.csv"
OUT_UPDATED = ROOT / "data" / "intraday_balanced.csv"
OUT_MATCHED.parent.mkdir(parents=True, exist_ok=True)

# ---------- SETTINGS ----------
CUTOFF = pd.Timestamp("2022-02-24", tz="UTC")  # pre-period cutoff
TREATED_FLAG = "nbr_1_or_2"                    # 1=treated, 0=control
COVARS = ["mktval", "price"]                   # PSM covariates
TARGET_TREATED = 54                            # expected treated count

# ---------- LOAD DATA ----------
def load_intraday():
    df = pd.read_csv(IN1, low_memory=False)
    # Parse datetime
    if "datetime" in df.columns:
        df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
        dtcol = "datetime"
    elif "date" in df.columns:
        df["date"] = pd.to_datetime(df["date"], errors="coerce", utc=True)
        dtcol = "date"
    else:
        raise ValueError("No 'datetime' or 'date' column in input file.")
    return df, dtcol

# ---------- PRE-PERIOD COVARIATES ----------
def prep_preperiod_firm_level(df: pd.DataFrame, dtcol: str) -> pd.DataFrame:
    pre = df[df[dtcol] < CUTOFF].copy()
    if pre.empty:
        raise ValueError("No pre-period data before 2022-02-24.")

    needed = {"ric", TREATED_FLAG, *COVARS}
    missing = [c for c in needed if c not in pre.columns]
    if missing:
        raise KeyError(f"Missing required columns: {missing}")

    for c in COVARS:
        pre[c] = pd.to_numeric(pre[c], errors="coerce")

    # Aggregate median per firm
    agg = (
        pre.groupby("ric", as_index=False)
           .agg({
               TREATED_FLAG: "last",
               "mktval": "median",
               "price": "median"
           })
    )

    before = len(agg)
    agg = agg.dropna(subset=COVARS)
    for c in COVARS:
        agg = agg[agg[c] > 0]
    after = len(agg)
    print(f"[info] pre-period firms: {before:,} → {after:,} after valid covariates")

    n_treat = int((agg[TREATED_FLAG] == 1).sum())
    n_ctrl  = int((agg[TREATED_FLAG] == 0).sum())
    print(f"[info] treated: {n_treat:,} | control: {n_ctrl:,}")
    return agg

# ---------- PROPENSITY SCORE ----------
def estimate_propensity(agg: pd.DataFrame) -> pd.DataFrame:
    X = agg[COVARS].to_numpy(dtype="float64")
    y = agg[TREATED_FLAG].to_numpy()

    scaler = StandardScaler()
    Xs = scaler.fit_transform(X)

    logit = LogisticRegression(max_iter=2000)
    logit.fit(Xs, y)
    ps = logit.predict_proba(Xs)[:, 1]
    agg = agg.copy()
    agg["propensity_score"] = ps
    return agg

# ---------- MATCHING ----------
def greedy_nearest_without_replacement(treated_df: pd.DataFrame, control_df: pd.DataFrame) -> pd.DataFrame:
    """
    Greedy nearest neighbor matching without replacement on propensity score.
    """
    t_scores = treated_df["propensity_score"].to_numpy()
    c_scores = control_df["propensity_score"].to_numpy()
    c_used = np.zeros(len(control_df), dtype=bool)

    pairs = []
    for ti, ps in enumerate(t_scores):
        diffs = np.abs(c_scores - ps)
        diffs[c_used] = np.inf
        cj = int(np.argmin(diffs))
        if not np.isfinite(diffs[cj]):
            break
        c_used[cj] = True
        pairs.append((ti, cj))

    print(f"[match] formed pairs: {len(pairs)}")
    t_matched = treated_df.iloc[[i for i, _ in pairs]].copy().reset_index(drop=True)
    c_matched = control_df.iloc[[j for _, j in pairs]].copy().reset_index(drop=True)
    t_matched["matched_group"] = "treated"
    c_matched["matched_group"] = "control"
    if "ric" in treated_df.columns:
        t_matched["matched_partner"] = c_matched["ric"].values
        c_matched["matched_partner"] = t_matched["ric"].values
    matched = pd.concat([t_matched, c_matched], ignore_index=True)
    return matched

# ---------- MAIN ----------
def main():
    df, dtcol = load_intraday()
    agg = prep_preperiod_firm_level(df, dtcol)
    agg = estimate_propensity(agg)

    treated = agg[agg[TREATED_FLAG] == 1].copy()
    control = agg[agg[TREATED_FLAG] == 0].copy()

    # Keep 448 treated if available
    n_treat = min(TARGET_TREATED, len(treated))
    if len(treated) != n_treat:
        print(f"[warn] expected 54 treated, found {len(treated)}. Using {n_treat}.")
    treated = treated.head(n_treat)

    if len(control) < n_treat:
        raise ValueError(f"Not enough controls to match {n_treat} treated.")

    matched_pairs = greedy_nearest_without_replacement(treated, control)

    # Save matched subset
    keep_cols = ["ric", TREATED_FLAG, *COVARS, "propensity_score", "matched_group", "matched_partner"]
    keep_cols = [c for c in keep_cols if c in matched_pairs.columns]
    matched_pairs = matched_pairs[keep_cols].copy()
    matched_pairs.to_csv(OUT_MATCHED, index=False)
    print(f"[done] Matched pairs saved → {OUT_MATCHED}")

    # ---------- ADD DUMMY TO ORIGINAL daily_balanced.csv ----------
    # Create mapping of ric → dummy (1 treated, 0 control)
    psm_map = {}
    for _, row in matched_pairs.iterrows():
        if row["matched_group"] == "treated":
            psm_map[row["ric"]] = 1
        elif row["matched_group"] == "control":
            psm_map[row["ric"]] = 0

    # Add to full dataset
    df["psm_group"] = df["ric"].map(psm_map)
    df.to_csv(OUT_UPDATED, index=False)
    print(f"[done] Updated intraday dataset with 'psm_group' dummy → {OUT_UPDATED}")

if __name__ == "__main__":
    main()