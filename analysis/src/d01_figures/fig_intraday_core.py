"""Intraday open/close-hour z-score figures and t-tests for the quoted spread.
`run(sample, group)` handles one sample × group; intraday.py loops the 2×2 grid.

- trading day = LOCAL trading day (date_local): UTC calendar days split the
  Sydney/NZ session at UTC midnight;
- open/close hour = first/last 12 five-minute buckets;
- std_bench = std across FIRM-DAY hour means in the benchmark window;
- no winsorization at the analysis level (tails are sane after the tick filters);
- ONE parameter set for every sample/group.
"""
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from scipy.stats import t as student_t

try:
    from . import fig_common as fc
except ImportError:
    import fig_common as fc

EVENT_LOCAL_DAY = "2022-02-24"
VAR = "qspread_mean"
BENCH_WIN = (-20, -6)   # estimation window, separate from the evaluation days
PRE_WIN = (-5, -1)      # closing hour
POST_WIN = (0, 5)       # opening hour
DAYS = list(range(-5, 6))
Z_WINSOR = (0, 100)     # no winsorization of z (and none on the hour means either)

plt.rcParams.update({"font.family": "serif", "font.serif": ["Times New Roman", "DejaVu Serif"],
                     "font.size": 11.5, "axes.linewidth": 0.9, "figure.dpi": 300})


def _style(ax):
    ax.grid(True, alpha=.25)
    for s in ax.spines.values():
        s.set_visible(True); s.set_linewidth(.9); s.set_color("black")


def _load(sample, group):
    df = fc.load_panel("intraday", sample, ["ric", "datetime", "date_local", "nbr_1_or_2", VAR])
    df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
    df = df.dropna(subset=["datetime", "date_local"]).sort_values(["ric", "datetime"], kind="mergesort")
    df = fc.assign_groups(df, group, stage="figures_intraday", sample=sample)

    days = np.sort(df["date_local"].unique())
    day_to_idx = {d: i for i, d in enumerate(days)}
    if EVENT_LOCAL_DAY not in day_to_idx:
        raise ValueError(f"Event day {EVENT_LOCAL_DAY} not in date_local values.")
    df["day_rel"] = df["date_local"].map(day_to_idx) - day_to_idx[EVENT_LOCAL_DAY]
    return df.reset_index(drop=True)


def _hourly_means(df):
    bounds = df.groupby(["ric", "date_local"])["datetime"].agg(first_dt="min", last_dt="max").reset_index()
    df = df.merge(bounds, on=["ric", "date_local"], how="left")
    is_open = (df["datetime"] >= df["first_dt"]) & (df["datetime"] < df["first_dt"] + pd.Timedelta(minutes=60))
    is_close = (df["datetime"] > df["last_dt"] - pd.Timedelta(minutes=60)) & (df["datetime"] <= df["last_dt"])

    def hourly(flag):  # w is constant within firm, so it can sit in the keys
        return (df[flag].groupby(["ric", "date_local", "group", "day_rel", "w"])[VAR]
                .mean(numeric_only=True).reset_index().rename(columns={VAR: "hour_mean"}))
    return hourly(is_open), hourly(is_close)


def _benchmarks(hr_df, lo, hi):
    """Weighted mean/std of the FIRM-DAY hour means over the benchmark window."""
    z = hr_df[hr_df["day_rel"].between(lo, hi)]
    rows = {}
    for grp, sub in z.groupby("group"):
        m, v, _ = fc.wstats(sub["hour_mean"], sub["w"])
        rows[grp] = {"mean_bench": m, "std_bench": np.sqrt(v)}
    m, v, _ = fc.wstats(z["hour_mean"], z["w"])
    rows["Overall"] = {"mean_bench": m, "std_bench": np.sqrt(v)}
    return pd.DataFrame(rows).T


def _standardize(hr_df, lo, hi, bench):
    z = hr_df[hr_df["day_rel"].between(lo, hi)].copy()
    z = z.merge(bench, left_on="group", right_index=True, how="left")
    for col in ["mean_bench", "std_bench"]:
        z[col] = z[col].fillna(bench.loc["Overall", col])
    z["z"] = (z["hour_mean"] - z["mean_bench"]) / z["std_bench"].replace(0, np.nan)
    return z[["ric", "group", "day_rel", "z", "w"]]


def _t_tests(z_all):
    rows = []
    for d in DAYS:
        sub = z_all[z_all["day_rel"] == d]
        near = sub[sub["group"] == "Nearby"]["z"].dropna()
        dist = sub[sub["group"] == "Distant"]["z"].dropna()
        if len(near) < 3 or len(dist) < 3:
            rows.append({"day_rel": d, "mean_near": np.nan, "mean_dist": np.nan, "diff": np.nan,
                         "t_stat": np.nan, "p_val": np.nan, "sig": ""})
            continue
        # weighted Welch t-test (unit weights reproduce scipy's ttest_ind(equal_var=False))
        m1, v1, n1 = fc.wstats(near, sub.loc[near.index, "w"])
        m2, v2, n2 = fc.wstats(dist, sub.loc[dist.index, "w"])
        se2 = v1 / n1 + v2 / n2
        t_stat = (m1 - m2) / np.sqrt(se2)
        dof = se2 ** 2 / ((v1 / n1) ** 2 / (n1 - 1) + (v2 / n2) ** 2 / (n2 - 1))
        p_val = 2 * student_t.sf(abs(t_stat), dof)
        sig = "***" if p_val < .01 else "**" if p_val < .05 else "*" if p_val < .10 else ""
        rows.append({"day_rel": d, "mean_near": m1, "mean_dist": m2, "diff": m1 - m2,
                     "t_stat": t_stat, "p_val": p_val, "sig": sig})
    return pd.DataFrame(rows)


def _write_latex(t_table, path, caption_suffix):
    def fmt(x):
        return f"{x:.2f}" if pd.notnull(x) else ""
    with open(path, "w") as f:
        f.write("\\begin{table}[ht]\n\\centering\n")
        f.write(f"\\caption{{Difference in Standardized Abnormal Quoted Spread: Nearby vs.\\ Distant ({caption_suffix})}}\n")
        f.write("\\label{tab:ttest}\n\\vspace{0.5em}\n\\begin{tabular}{lcccc}\n\\toprule\n")
        f.write("Day & Nearby & Distant & Difference & $t$-statistic \\\\\n\\midrule\n")
        for _, r in t_table.iterrows():
            f.write(f"{int(r['day_rel']):+d} & {fmt(r['mean_near'])} & {fmt(r['mean_dist'])} & "
                    f"{fmt(r['diff'])} & {fmt(r['t_stat'])}{r['sig']} \\\\\n")
        f.write("\\midrule\n\\multicolumn{5}{l}{\\footnotesize Notes: Welch unequal-variance $t$-tests.}\\\\\n")
        f.write("\\multicolumn{5}{l}{\\footnotesize $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.}\\\\\n")
        f.write("\\bottomrule\n\\end{tabular}\n\\end{table}\n")


def _marker(ax, x, m, sig, color="black", marker="o", s=35):
    if np.isfinite(m):
        ax.scatter([x], [m], facecolors=color if sig else "white", edgecolors=color, marker=marker, s=s, zorder=3)


def _axes(ax, ylabel):
    ax.axhline(0, color="gray", lw=1.0, ls="--")
    ax.axvline(0, color="green", ls="--", lw=1.0)
    ax.set_xticks(DAYS); ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS) - .8, max(DAYS) + .8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel(ylabel)
    _style(ax)


def _wagg(z_sub):
    """Weighted mean and 90% CI half-width of z by day_rel."""
    recs = []
    for d, s in z_sub.groupby("day_rel"):
        m, v, n = fc.wstats(s["z"], s["w"])
        recs.append({"day_rel": d, "mean": m, "ci": 1.645 * np.sqrt(v / n) if n > 0 else np.nan})
    return pd.DataFrame(recs).set_index("day_rel").reindex(DAYS).reset_index()


def _fig_group_ci(z_all, out_dir):
    fig, ax = plt.subplots(figsize=(5.4, 4.8))
    for grp, (marker, color, off) in {"Nearby": ("o", "red", .15), "Distant": ("^", "blue", -.15)}.items():
        g = _wagg(z_all[z_all["group"] == grp])
        x = np.array(DAYS) + off
        ax.errorbar(x, g["mean"], yerr=g["ci"], fmt="none", ecolor=color, elinewidth=1.1, capsize=1.5)
        for xi, m, ci in zip(x, g["mean"], g["ci"]):
            _marker(ax, xi, m, (m - ci > 0) | (m + ci < 0), color, marker, 20)
    _axes(ax, "Standardized Abnormal Quoted Spread (z)")
    ax.legend(handles=[Line2D([0], [0], marker="o", color="red", lw=0, label="Nearby", markerfacecolor="red"),
                       Line2D([0], [0], marker="^", color="blue", lw=0, label="Distant", markerfacecolor="blue")],
              frameon=True, facecolor="white", edgecolor="gray", framealpha=.8, loc="best")
    fig.tight_layout(); plt.savefig(out_dir / "ci_dist.png", dpi=300, bbox_inches="tight"); plt.close(fig)


def _fig_overall_ci(z_all, out_dir):
    g = _wagg(z_all)
    x = np.array(DAYS)
    fig, ax = plt.subplots(figsize=(5.4, 4.8))
    ax.fill_between(x, g["mean"] - g["ci"], g["mean"] + g["ci"], color="black", alpha=.12)
    ax.plot(x, g["mean"], color="black", lw=1.2, zorder=1)
    for xi, m, ci in zip(x, g["mean"], g["ci"]):
        _marker(ax, xi, m, (m - ci > 0) | (m + ci < 0))
    _axes(ax, "Standardized Abnormal Quoted Spread (z)")
    ax.legend(handles=[Line2D([0], [0], color="black", lw=1.1, label="mean"),
                       Patch(facecolor="black", alpha=.12, label="90% CI"),
                       Line2D([0], [0], marker="o", color="black", lw=0, label="significant", mfc="black"),
                       Line2D([0], [0], marker="o", mfc="white", mec="black", lw=0, label="not significant")],
              frameon=True, facecolor="white", edgecolor="gray", framealpha=.8, loc="best")
    fig.tight_layout(); plt.savefig(out_dir / "ci_all.png", dpi=300, bbox_inches="tight"); plt.close(fig)


def _fig_difference_ci(t_table, out_dir):
    g = t_table.copy()
    g["SE"] = np.abs(g["diff"] / g["t_stat"])
    g.loc[~np.isfinite(g["SE"]), "SE"] = np.nan
    g["CI"] = 1.645 * g["SE"]
    x = np.array(g["day_rel"])
    fig, ax = plt.subplots(figsize=(5.4, 4.8))
    ax.fill_between(x, g["diff"] - g["CI"], g["diff"] + g["CI"], color="black", alpha=.12)
    ax.plot(x, g["diff"], color="black", lw=1.2, zorder=2)
    for xi, m, sig in zip(x, g["diff"], g["sig"]):
        _marker(ax, xi, m, sig != "")
    _axes(ax, "Difference in Standardized Quoted Spread (z)")
    ax.legend(handles=[Line2D([0], [0], color="black", lw=1.1, label="Nearby − Distant"),
                       Patch(facecolor="black", alpha=.12, label="90% CI"),
                       Line2D([0], [0], marker="o", color="black", lw=0, label="significant", markerfacecolor="black"),
                       Line2D([0], [0], marker="o", lw=0, label="not significant", markerfacecolor="white", markeredgecolor="black")],
              frameon=True, facecolor="white", edgecolor="gray", framealpha=.8, loc="best")
    fig.tight_layout(); plt.savefig(out_dir / "ci_difference.png", dpi=300, bbox_inches="tight"); plt.close(fig)


def run(sample: str, group: str):
    fig_dir = fc.out_dir("intraday", sample, group)
    tab_dir = fc.tab_dir("intraday", sample, group)

    df = _load(sample, group)
    open_hr, close_hr = _hourly_means(df)

    z_pre = _standardize(close_hr, *PRE_WIN, _benchmarks(close_hr, *BENCH_WIN))
    z_post = _standardize(open_hr, *POST_WIN, _benchmarks(open_hr, *BENCH_WIN))
    z_all = pd.concat([z_pre, z_post], ignore_index=True)
    low, high = np.nanpercentile(z_all["z"], Z_WINSOR)
    z_all["z"] = z_all["z"].clip(lower=low, upper=high)

    t_table = _t_tests(z_all)
    _write_latex(t_table, tab_dir / "ttest_10.tex", f"{fc.SAMPLE_TITLE[sample]}, {fc.GROUP_TITLE[group]}")
    t_table.to_csv(tab_dir / "ttest_10.csv", index=False)
    print(f"\n===== T-TEST [{sample}|{group}] Nearby − Distant =====")
    print(t_table.to_string(index=False, formatters={c: "{:.4f}".format for c in ["mean_near", "mean_dist", "diff", "t_stat", "p_val"]}))

    _fig_group_ci(z_all, fig_dir)
    _fig_overall_ci(z_all, fig_dir)
    _fig_difference_ci(t_table, fig_dir)
    print(f"✅ intraday [{sample}|{group}] tables → {tab_dir}; figures → {fig_dir}")
