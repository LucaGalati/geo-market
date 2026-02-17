import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from pathlib import Path
import statsmodels.api as sm
from statsmodels.iolib.summary2 import summary_col

# ---------- PATHS ----------
DATA_ROOT   = Path(__file__).resolve().parents[2] / "data"
OUTPUT_ROOT = Path(__file__).resolve().parents[2] / "output"
INTRADAY    = DATA_ROOT / "intraday_balanced.csv"
OUT_DIR     = OUTPUT_ROOT / "intraday" / "figures" / "psm" / "ci_90_winsor_99" / "qspread"
OUT_DIR2    = OUTPUT_ROOT / "intraday" / "tables" / "psm" 
OUT_DIR.mkdir(parents=True, exist_ok=True)
OUT_DIR2.mkdir(parents=True, exist_ok=True)

# ---------- READ ----------
df = pd.read_csv(INTRADAY, low_memory=False)
df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
df.dropna(subset=["datetime"], inplace=True)
df.sort_values(["ric", "datetime"], kind="mergesort", inplace=True)

df["psm_group"] = pd.to_numeric(df["psm_group"], errors="coerce")
df = df[df["psm_group"].isin([0, 1])].copy()
df["group"] = df["psm_group"].map({1: "Nearby", 0: "Distant"})
df["date_utc"] = df["datetime"].dt.normalize()
EVENT_DAY = pd.Timestamp("2022-02-24", tz="UTC")
days = np.sort(df["date_utc"].unique())
day_to_idx = {d: i for i, d in enumerate(days)}
event_idx = day_to_idx[EVENT_DAY]
df["day_rel"] = df["date_utc"].map(lambda d: day_to_idx[d] - event_idx)

# ---------- HOUR WINDOWS ----------
mins = df.groupby(["ric","date_utc"])["datetime"].min().rename("first_dt")
maxs = df.groupby(["ric","date_utc"])["datetime"].max().rename("last_dt")
bounds = pd.concat([mins,maxs],axis=1).reset_index()
df = df.merge(bounds,on=["ric","date_utc"],how="left")

df["is_open_hour"]  = (df["datetime"] >= df["first_dt"]) & (df["datetime"] <= df["first_dt"] + pd.Timedelta(minutes=60))
df["is_close_hour"] = (df["datetime"] >= df["last_dt"] - pd.Timedelta(minutes=60)) & (df["datetime"] <= df["last_dt"])

def hourly_mean(flag):
    sub = df[df[flag]]
    return (sub.groupby(["ric","date_utc","group","day_rel"])["qspread_mean"]
              .mean(numeric_only=True)
              .reset_index()
              .rename(columns={"qspread_mean":"hour_mean"}))

open_hr, close_hr = hourly_mean("is_open_hour"), hourly_mean("is_close_hour")

BENCH_WIN = (-10, -6)
PRE_WIN   = (-5, -1)
POST_WIN  = (0, 5)
DAYS      = list(range(-5, 6))

# ---------- BENCHMARKS ----------
def compute_benchmarks(hr_df, lo, hi):
    z = hr_df[hr_df["day_rel"].between(lo, hi)].copy()
    gday = z.groupby(["group","day_rel"])["hour_mean"].mean().reset_index()
    gbench = gday.groupby("group")["hour_mean"].agg(mean_bench="mean", std_bench="std").reset_index()
    overall = (
        gday.groupby("day_rel")["hour_mean"].mean().agg(["mean","std"])
    ).rename({"mean":"mean_bench","std":"std_bench"}).to_frame().T
    overall["group"] = "Overall"
    return pd.concat([gbench, overall], ignore_index=True).set_index("group")

bench_close = compute_benchmarks(close_hr, *BENCH_WIN)
bench_open  = compute_benchmarks(open_hr,  *BENCH_WIN)

# ---------- STANDARDIZATION ----------
def standardize(hr_df, lo, hi, kind):
    z = hr_df[hr_df["day_rel"].between(lo, hi)].copy()
    b = bench_close if kind=="close" else bench_open
    z = z.merge(b, left_on="group", right_index=True, how="left")
    for col in ["mean_bench","std_bench"]:
        z[col] = z[col].fillna(b.loc["Overall", col])
    z["z"] = (z["hour_mean"] - z["mean_bench"]) / z["std_bench"].replace(0, np.nan)
    return z[["ric","group","day_rel","z"]]

z_pre  = standardize(close_hr, *PRE_WIN,  "close")
z_post = standardize(open_hr,  *POST_WIN, "open")
z_all  = pd.concat([z_pre, z_post], ignore_index=True)

# ---------- WINSORIZATION ----------
# Change limits here to adjust:
LOW_PCT, HIGH_PCT = 2.5, 97.5       # current = 0.1–99.9
# e.g. set to (1, 99) for 1–99 % or (0.5, 99.5) for 0.5–99.5 %
low, high = np.nanpercentile(z_all["z"], [LOW_PCT, HIGH_PCT])
z_all["z"] = z_all["z"].clip(lower=low, upper=high)


# ============================================================
# ===  T-TEST: Nearby vs Distant for each day_rel in DAYS  ===
# ============================================================

from scipy.stats import ttest_ind

t_results = []

for d in DAYS:
    sub = z_all[z_all["day_rel"] == d]

    near = sub[sub["group"] == "Nearby"]["z"].dropna()
    dist = sub[sub["group"] == "Distant"]["z"].dropna()

    if len(near) < 3 or len(dist) < 3:
        t_results.append({
            "day_rel": d,
            "mean_near": np.nan,
            "mean_dist": np.nan,
            "diff": np.nan,
            "t_stat": np.nan,
            "p_val": np.nan,
            "sig": ""
        })
        continue

    # Welch unequal-variance t-test
    t_stat, p_val = ttest_ind(near, dist, equal_var=False)

    diff = near.mean() - dist.mean()

    # significance stars
    if p_val < 0.01: sig = "***"
    elif p_val < 0.05: sig = "**"
    elif p_val < 0.10: sig = "*"
    else: sig = ""

    t_results.append({
        "day_rel": d,
        "mean_near": near.mean(),
        "mean_dist": dist.mean(),
        "diff": diff,
        "t_stat": t_stat,
        "p_val": p_val,
        "sig": sig
    })

# Put results in a table
t_table = pd.DataFrame(t_results)

# ============================================================
# = Create LaTeX Table 
# ============================================================

latex_path = OUT_DIR2 / "ttest_10.tex"

# Work on a copy
tab = t_table.copy()

# Round values
def fmt(x):
    return f"{x:.2f}" if pd.notnull(x) else ""

tab["mean_near_f"] = tab["mean_near"].apply(fmt)
tab["mean_dist_f"] = tab["mean_dist"].apply(fmt)
tab["diff_f"]      = tab["diff"].apply(fmt)
tab["t_stat_f"]    = tab["t_stat"].apply(fmt)

# Add stars to t-stat (based on p-val)
def add_stars(row):
    if pd.isnull(row["p_val"]):
        return ""
    if row["p_val"] < 0.01:
        return row["t_stat_f"] + "***"
    elif row["p_val"] < 0.05:
        return row["t_stat_f"] + "**"
    elif row["p_val"] < 0.10:
        return row["t_stat_f"] + "*"
    else:
        return row["t_stat_f"]

tab["t_stat_star"] = tab.apply(add_stars, axis=1)

# Build LaTeX
with open(latex_path, "w") as f:
    f.write("\\begin{table}[ht]\n")
    f.write("\\centering\n")
    f.write("\\caption{Difference in Standardized Abnormal Quoted Spread: Nearby vs.\ Distant}\n")
    f.write("\\label{tab:ttest_psm}\n")
    f.write("\\vspace{0.5em}\n")
    
    f.write("\\begin{tabular}{lcccc}\n")
    f.write("\\toprule\n")
    f.write("Day & Nearby & Distant & Difference & $t$-statistic \\\\\n")
    f.write("\\midrule\n")
    
    for _, r in tab.iterrows():
        f.write(
            f"{int(r['day_rel']):+d} & "
            f"{r['mean_near_f']} & "
            f"{r['mean_dist_f']} & "
            f"{r['diff_f']} & "
            f"{r['t_stat_star']} \\\\\n"
        )
    
    f.write("\\midrule\n")
    f.write("\\multicolumn{5}{l}{\\footnotesize Notes: Welch unequal-variance $t$-tests.}\\\\\n")
    f.write("\\multicolumn{5}{l}{\\footnotesize $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.}\\\\\n")
    f.write("\\bottomrule\n")
    f.write("\\end{tabular}\n")
    f.write("\\end{table}\n")

print(f"Saved LaTeX table → {latex_path}")

# Pretty print
print("\n================= T-TEST: Nearby − Distant =================")
print(t_table.to_string(index=False,
                        formatters={
                            "mean_near": "{:.4f}".format,
                            "mean_dist": "{:.4f}".format,
                            "diff": "{:.4f}".format,
                            "t_stat": "{:.3f}".format,
                            "p_val": "{:.3f}".format
                        }))
print("============================================================\n")

# Optionally save to CSV
t_table.to_csv(OUT_DIR2 / "ttest_10.csv", index=False)
print(f"Saved t-test table → {OUT_DIR2/'ttest_10.csv'}")


# ---------- STYLE ----------
plt.rcParams.update({
 "font.family":"serif",
 "font.serif":["Times New Roman","DejaVu Serif"],
 "font.size":11.5,
 "axes.linewidth":0.9,
 "figure.dpi":300
})
def style(ax):
    ax.grid(True, alpha=.25)
    for s in ax.spines.values():
        s.set_visible(True); s.set_linewidth(.9); s.set_color("black")

# ---------- GROUP CI (90%) ----------
def fig_group_ci_90():
    fig, ax = plt.subplots(figsize=(5.4,4.8))
    offset = 0.15
    crit = 1.645  # 90% CI
    marker_styles = {"Nearby":("o","red"), "Distant":("^","blue")}

    for grp,(marker,color) in marker_styles.items():
        g = (z_all[z_all["group"]==grp]
             .groupby("day_rel")["z"]
             .agg(mean="mean",std="std",n="count")
             .reindex(DAYS).reset_index())
        g["se"] = g["std"]/np.sqrt(g["n"].where(g["n"]>0,np.nan))
        g["ci"] = crit * g["se"]
        g["lo"], g["hi"] = g["mean"]-g["ci"], g["mean"]+g["ci"]
        g["sig"] = (g["lo"]>0)|(g["hi"]<0)
        x = np.array(DAYS) + (offset if grp=="Nearby" else -offset)
        ax.errorbar(x, g["mean"], yerr=g["ci"], fmt="none",
                    ecolor=color, elinewidth=1.1, capsize=1.5)
        for xi,m,sig in zip(x,g["mean"],g["sig"]):
            if np.isfinite(m):
                face=color if sig else "white"
                ax.scatter([xi],[m],facecolors=face,edgecolors=color,
                           marker=marker,s=20,zorder=3)

    # axis styling
    ax.axhline(0, color="gray", lw=1.0, ls="--")
    ax.axvline(0, color="green", ls="--", lw=1.0)
    ax.set_xticks(DAYS)
    ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS)-0.8, max(DAYS)+0.8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel("Standardized Abnormal Quoted Spread (z)")
    style(ax)

    # legend
    handles=[
        Line2D([0],[0],marker="o",color="red",lw=0,label="Nearby",markerfacecolor="red"),
        Line2D([0],[0],marker="^",color="blue",lw=0,label="Distant",markerfacecolor="blue")
    ]
    ax.legend(handles=handles,frameon=True,facecolor="white",
              edgecolor="gray",framealpha=.8,loc="best")

    fig.tight_layout()
    plt.savefig(OUT_DIR/"ci_dist.png",dpi=300,bbox_inches="tight")
    plt.close()

# ---------- OVERALL CI (shaded) ----------
def fig_overall_ci():
    g=(z_all.groupby("day_rel")["z"]
        .agg(mean="mean",std="std",n="count")
        .reindex(DAYS).reset_index())
    g["se"]=g["std"]/np.sqrt(g["n"].where(g["n"]>0,np.nan))
    g["ci"]=1.645*g["se"]
    g["lo"],g["hi"]=g["mean"]-g["ci"],g["mean"]+g["ci"]
    g["sig"]=(g["lo"]>0)|(g["hi"]<0)
    x=np.array(DAYS)
    fig,ax=plt.subplots(figsize=(5.4,4.8))
    ax.fill_between(x,g["lo"],g["hi"],color="black",alpha=.12)
    ax.plot(x,g["mean"],color="black",lw=1.2,zorder=1)
    for xi,m,s in zip(x,g["mean"],g["sig"]):
        if np.isfinite(m):
            if s: ax.scatter([xi],[m],color="black",s=35)
            else: ax.scatter([xi],[m],facecolors="white",edgecolors="black",s=35)
    ax.axhline(0,color="gray",lw=1.0,ls="--")
    ax.axvline(0,color="green",ls="--",lw=1.0)
    ax.set_xticks(DAYS)
    ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS)-0.8,max(DAYS)+0.8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel("Standardized Abnormal Quoted Spread (z)")
    style(ax)
    h=[
        Line2D([0],[0],color="black",lw=1.1,label="mean"),
        Patch(facecolor="black",alpha=.12,label="90% CI"),
        Line2D([0],[0],marker="o",color="black",lw=0,label="significant",mfc="black"),
        Line2D([0],[0],marker="o",mfc="white",mec="black",lw=0,label="not significant")
    ]
    ax.legend(handles=h,frameon=True,facecolor="white",edgecolor="gray",framealpha=.8,loc="best")
    fig.tight_layout()
    plt.savefig(OUT_DIR/"ci_all.png",dpi=300,bbox_inches="tight")
    plt.close()
# ================================================================
# === SHADED CI PLOT FOR NEARBY − DISTANT DIFFERENCE (OVERALL STYLE)
# ================================================================

def fig_difference_ci_90():
    # Welch SE from t-tests: SE = |diff / t_stat|
    g = t_table.copy()
    g["SE"] = np.abs(g["diff"] / g["t_stat"])
    g.loc[g["SE"].isna(), "SE"] = np.nan

    # 90% CI
    g["CI"] = 1.645 * g["SE"]
    g["lo"] = g["diff"] - g["CI"]
    g["hi"] = g["diff"] + g["CI"]

    x = np.array(g["day_rel"])

    fig, ax = plt.subplots(figsize=(5.4,4.8))

    # Shaded 90% CI (same as overall)
    ax.fill_between(x, g["lo"], g["hi"], color="black", alpha=.12)

    # Difference line
    ax.plot(x, g["diff"], color="black", lw=1.2, zorder=2)

    # Markers: filled if significant else hollow
    for xi, m, sig in zip(x, g["diff"], g["sig"]):
        if np.isfinite(m):
            if sig in ["*","**","***"]:
                ax.scatter([xi], [m], color="black", s=35, zorder=3)
            else:
                ax.scatter([xi], [m], facecolors="white", edgecolors="black",
                           s=35, zorder=3)

    # Baseline styling (same as overall)
    ax.axhline(0, color="gray", lw=1.0, ls="--")
    ax.axvline(0, color="green", lw=1.0, ls="--")

    ax.set_xticks(DAYS)
    ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS)-0.8, max(DAYS)+0.8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel("Difference in Standardized Quoted Spread (z)")

    style(ax)

    # Legend (same structure as overall plot)
    handles = [
        Line2D([0],[0], color="black", lw=1.1, label="Nearby − Distant"),
        Patch(facecolor="black", alpha=.12, label="90% CI"),
        Line2D([0],[0], marker="o", color="black", lw=0,
               label="significant", markerfacecolor="black"),
        Line2D([0],[0], marker="o", lw=0, label="not significant",
               markerfacecolor="white", markeredgecolor="black")
    ]
    ax.legend(handles=handles, frameon=True, facecolor="white",
              edgecolor="gray", framealpha=.8, loc="best")

    fig.tight_layout()
    plt.savefig(OUT_DIR / "ci_difference.png",
                dpi=300, bbox_inches="tight")
    plt.close()

    print("Saved difference shaded plot → ci_difference.png")


# ---------- RUN ----------
fig_group_ci_90()
fig_overall_ci()
fig_difference_ci_90()
print(f"✅ 90% CI figures saved to: {OUT_DIR}")
