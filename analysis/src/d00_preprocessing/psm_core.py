"""Firm-level matching for the event study. One estimation on the daily main
panel, applied everywhere by joining on `ric`.

Covariates : firm-level medians, over a reference window of pre-invasion trading
             days, of the Datastream market value (U.S. dollars) and of the daily
             quoted spread and dollar volume, all log-transformed (COVARS). The
             appendix repeats the matching on market value and share price only,
             the two characteristics of Davies and Kim (2009, JFM) (DK_COVARS).
Window     : trading days relative to 24 February 2022 (day 0). The main window
             is MAIN_WINDOW; the other candidates in WINDOWS are estimated as well
             and reported in the appendix (analysis/src/d02_results/matching_windows.R).
Method     : Mahalanobis matching on the standardized log covariates WITHIN a
             propensity caliper (Rosenbaum & Rubin 1985; Rubin & Thomas 2000), 1:1
             without replacement, optimal assignment (Hungarian); the caliper is the
             widest of a grid that leaves every post-matching |SMD| below 0.10.

Output: one ric-level table (`psm_assignments.parquet`, with the partner of every
matched firm) + a balance table (`psm_balance.csv`) for the main matching, the
same two files with a window tag (`psm_assignments_w20_1.parquet`, ...) for every
candidate window, and with the tag `dk` for the Davies-Kim matching. The panels
are never rewritten.
"""
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.optimize import linear_sum_assignment
from scipy.spatial.distance import cdist
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler

try:
    from . import sampling_log as slog
except ImportError:
    import sampling_log as slog

TREATED_FLAG = "nbr_1_or_2"
EVENT = pd.Timestamp("2022-02-24", tz="UTC")
COVARS = ["mktval", "qspread_mean", "dollar_volume_sum"]   # main matching; log-transformed
DK_COVARS = ["mktval", "price"]                              # appendix: Davies and Kim (2009)
ALL_COVARS = ["mktval", "price", "qspread_mean", "dollar_volume_sum"]
COVAR_LABELS = {"mktval": "market value", "price": "price", "dollar_volume_sum": "dollar volume",
                "qspread_mean": "quoted spread"}
# candidate reference windows (trading days relative to day 0); the main window leaves
# out the last week before the invasion, and the others are reported in the appendix
# (matching_windows.R)
WINDOWS = {"w20_1": (-20, -1), "w20_10": (-20, -10), "w20_6": (-20, -6), "w10_1": (-10, -1), "w5_1": (-5, -1)}
MAIN_WINDOW = (-20, -6)
# caliper (in SD of logit PS): the widest candidate that leaves every |SMD| below
# SMD_WARN is used; if none does, the one with the smallest max |SMD|
CALIPER_CANDIDATES = (0.20, 0.15, 0.10, 0.05, 0.025)
SMD_WARN = 0.10


def window_tag(window):
    return f"w{-window[0]}_{-window[1]}"


def window_text(window):
    return f"trading days {window[0]} to {window[1]}"


# ---------- DATA ----------
def load_daily(path: Path) -> pd.DataFrame:
    df = pd.read_parquet(path, columns=["ric", "date", TREATED_FLAG, *ALL_COVARS])
    day = pd.to_datetime(df["date"], errors="coerce", utc=True).dt.tz_convert(None).dt.normalize()
    days = np.sort(day.dropna().unique())
    k = int(np.searchsorted(days, np.datetime64(EVENT.tz_convert(None))))   # position of day 0
    df["day_rel"] = np.searchsorted(days, day.to_numpy()) - k
    return df


def firm_level_pre(df: pd.DataFrame, window=MAIN_WINDOW, covars=COVARS) -> pd.DataFrame:
    """Medians of the covariates per firm over the reference window; firms with
    missing or non-positive covariates are dropped."""
    pre = df[(df["day_rel"] >= window[0]) & (df["day_rel"] <= window[1])]
    if pre.empty:
        raise ValueError(f"No data in the reference window {window}.")
    agg = pre.groupby("ric", as_index=False).agg({TREATED_FLAG: "last", **{c: "median" for c in covars}})
    before = len(agg)
    agg = agg.dropna(subset=covars)
    for c in covars:
        agg = agg[agg[c] > 0]
    agg[TREATED_FLAG] = agg[TREATED_FLAG].astype(int)
    print(f"[info] window {window_text(window)}: {before:,} firms → {len(agg):,} with valid covariates")
    print(f"[info] treated: {int((agg[TREATED_FLAG] == 1).sum()):,} | control: {int((agg[TREATED_FLAG] == 0).sum()):,}")
    return agg.reset_index(drop=True)


def _smd(x_t, x_c, w_c=None):
    """Standardized mean difference with pooled SD (optionally weighted controls)."""
    if w_c is None:
        m_c, v_c = x_c.mean(), np.var(x_c, ddof=1)
    else:
        w = w_c / w_c.sum()
        m_c = np.sum(w * x_c)
        v_c = np.sum(w * (x_c - m_c) ** 2)
    s = np.sqrt((np.var(x_t, ddof=1) + v_c) / 2)
    return float((x_t.mean() - m_c) / s) if s > 0 else np.nan


# ---------- MAIN METHOD: Mahalanobis within propensity caliper ----------
def _assign(D, ps_gap, caliper):
    eligible = ps_gap <= caliper
    rows, cols = linear_sum_assignment(np.where(eligible, D, 1e6))
    ok = eligible[rows, cols]
    return rows[ok], cols[ok]


def match(agg: pd.DataFrame, covars=COVARS):
    X = np.log(agg[covars].to_numpy(dtype="float64"))
    y = agg[TREATED_FLAG].to_numpy()
    Xs = StandardScaler().fit_transform(X)

    logit = LogisticRegression(penalty=None, max_iter=5000).fit(Xs, y)
    ps = np.clip(logit.predict_proba(Xs)[:, 1], 1e-12, 1 - 1e-12)
    ps_logit = np.log(ps / (1 - ps))
    sd_logit = float(np.std(ps_logit, ddof=1))
    t, c = y == 1, y == 0

    VI = np.linalg.inv(np.cov(Xs, rowvar=False))
    D = cdist(Xs[t], Xs[c], metric="mahalanobis", VI=VI)
    ps_gap = np.abs(ps_logit[t][:, None] - ps_logit[c][None, :])
    cols_bal = np.column_stack([Xs, ps_logit])

    # adaptive caliper: widest candidate with every |SMD| < SMD_WARN, else the
    # candidate with the smallest max |SMD|
    trials = []
    for k in CALIPER_CANDIDATES:
        ti, ci = _assign(D, ps_gap, k * sd_logit)
        smds = [abs(_smd(cols_bal[np.flatnonzero(t)[ti], j], cols_bal[np.flatnonzero(c)[ci], j]))
                for j in range(cols_bal.shape[1])]
        trials.append((k, ti, ci, max(smds)))
        print(f"[match] caliper {k:>5}·SD -> {len(ti)} pairs, max |SMD| = {max(smds):.3f}")
    good = [tr for tr in trials if tr[3] < SMD_WARN]
    k, ti, ci, worst = good[0] if good else min(trials, key=lambda tr: tr[3])
    caliper = k * sd_logit
    print(f"[match] chosen caliper = {k} * SD(logit PS) = {caliper:.4f}; "
          f"pairs: {len(ti)} (treated unmatched: {int(t.sum()) - len(ti)}); max |SMD| = {worst:.3f}")

    t_idx, c_idx = np.flatnonzero(t)[ti], np.flatnonzero(c)[ci]
    out = pd.DataFrame(index=agg.index)
    out["propensity_score"] = ps
    out["ps_logit"] = ps_logit
    out["matched_group"] = np.nan
    out.loc[t_idx, "matched_group"] = 1
    out.loc[c_idx, "matched_group"] = 0
    out["matched_partner"] = None
    out.loc[t_idx, "matched_partner"] = agg.loc[c_idx, "ric"].to_numpy()
    out.loc[c_idx, "matched_partner"] = agg.loc[t_idx, "ric"].to_numpy()
    out["pair_distance"] = np.nan
    out.loc[t_idx, "pair_distance"] = D[ti, ci]
    out.loc[c_idx, "pair_distance"] = D[ti, ci]

    balance = []
    for j, name in enumerate([f"log({col}) " for col in covars] + ["ps_logit"]):
        col = cols_bal[:, j]
        balance.append({"method": f"matching (caliper {k}·SD)", "variable": name.strip(),
                        "smd_before": _smd(col[t], col[c]),
                        "smd_after": _smd(col[t_idx], col[c_idx])})
    info = {"caliper_sd": k, "caliper": caliper, "pairs": len(ti), "unmatched": int(t.sum()) - len(ti),
            "max_smd": worst}
    return out, pd.DataFrame(balance), Xs, info


# ---------- DRIVER ----------
def run(in_path: Path, out_assign: Path, out_balance: Path, window=MAIN_WINDOW, covars=COVARS, log=True) -> pd.DataFrame:
    """Match on one reference window and one set of characteristics and write the
    assignment and balance files. The sampling log is written only for the main matching."""
    df = load_daily(in_path)
    agg = firm_level_pre(df, window, covars)
    matched, balance, Xs, info = match(agg, covars)
    what = ", ".join(COVAR_LABELS[c] for c in covars)
    n_t, n_c = int((agg[TREATED_FLAG] == 1).sum()), int((agg[TREATED_FLAG] == 0).sum())
    if log:
        slog.reset("matching")
        slog.log("matching", f"Firms with valid pre-period covariates (median {what} > 0)",
                 firms=len(agg), note=f"treated {n_t:,}, control {n_c:,}; reference window: {window_text(window)}")
        slog.log("matching", f"Matched pairs (log {what}: Mahalanobis within propensity caliper, 1:1 without replacement)",
                 firms=info["pairs"], note=f"caliper {info['caliper_sd']}*SD(logit PS) = {info['caliper']:.3f}; max |SMD| {info['max_smd']:.3f}")
        slog.log("matching", "- treated firms without a control inside the caliper", firms=info["unmatched"])
        slog.log("matching", "Matched sample (treated + controls)", firms=2 * info["pairs"])

    assignments = pd.concat([agg, matched], axis=1)
    assignments["window"] = window_tag(window)
    assignments["covariates"] = "+".join(covars)
    assignments.to_parquet(out_assign, index=False)
    balance.insert(0, "window", window_tag(window))
    balance.insert(1, "covariates", "+".join(covars))
    balance.to_csv(out_balance, index=False)
    print("[balance]")
    print(balance.round(4).to_string(index=False))
    bad = balance[balance["smd_after"].abs() > SMD_WARN]
    if not bad.empty:
        print(f"⚠️ |SMD| > {SMD_WARN} after adjustment for: "
              + ", ".join(f"{r.variable} ({r.method})" for r in bad.itertuples()))
    print(f"[done] assignments → {out_assign} | balance → {out_balance}")
    return assignments


def run_windows(in_path: Path, data_dir: Path, main_only=False):
    """The main matching (standard file names, sampling log), the Davies-Kim matching
    of the appendix (tag `dk`) and every candidate window (tagged file names) used
    by matching_windows.R."""
    out = run(in_path, data_dir / "psm_assignments.parquet", data_dir / "psm_balance.csv", MAIN_WINDOW, log=True)
    print(f"\n[dk] market value and price, {window_text(MAIN_WINDOW)}")
    run(in_path, data_dir / "psm_assignments_dk.parquet", data_dir / "psm_balance_dk.csv", MAIN_WINDOW, DK_COVARS, log=False)
    if main_only:
        return out
    for tag, window in WINDOWS.items():
        print(f"\n[window] {tag}: {window_text(window)}")
        run(in_path, data_dir / f"psm_assignments_{tag}.parquet", data_dir / f"psm_balance_{tag}.csv", window, log=False)
    return out
