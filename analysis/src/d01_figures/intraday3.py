import pandas as pd, numpy as np, matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from pathlib import Path

# ---------- PATHS ----------
DATA_ROOT   = Path(__file__).resolve().parents[2] / "data"
OUTPUT_ROOT = Path(__file__).resolve().parents[2] / "output"
INTRADAY    = DATA_ROOT / "intraday_balanced.csv"
OUT_DIR     = OUTPUT_ROOT / "intraday" / "figures" / "full"
OUT_DIR.mkdir(parents=True, exist_ok=True)

# ---------- READ ----------
df = pd.read_csv(INTRADAY, low_memory=False)
df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
df.dropna(subset=["datetime"], inplace=True)
df.sort_values(["ric", "datetime"], kind="mergesort", inplace=True)

df["nbr_1_or_2"] = pd.to_numeric(df["nbr_1_or_2"], errors="coerce")
df["group"] = df["nbr_1_or_2"].map({1: "Nearby", 0: "Distant"})
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

BENCH_WIN = (-20, -6)
PRE_WIN   = (-5, -1)
POST_WIN  = (0, 5)
DAYS      = list(range(-3, 4))

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

# ---------- STYLE ----------
plt.rcParams.update({
 "font.family":"serif","font.serif":["Times New Roman","DejaVu Serif"],
 "font.size":11.5,"axes.linewidth":0.9,"figure.dpi":300})
def style(ax):
    ax.grid(True,alpha=.25)
    for s in ax.spines.values(): s.set_visible(True); s.set_linewidth(.9); s.set_color("black")

# ---------- NEARBY vs DISTANT BOX PLOTS ----------
def fig_group_boxplots():
    fig, ax = plt.subplots(figsize=(4.6,4.4))
    off, width = 0.15, 0.20
    for grp, color, ls, marker in [("Nearby","red","-","o"),("Distant","blue","--","^")]:
        data = [z_all.loc[(z_all["group"]==grp)&(z_all["day_rel"]==d),"z"].dropna().values for d in DAYS]
        pos = [d-off if grp=="Nearby" else d+off for d in DAYS]
        ax.boxplot(
            data, positions=pos, widths=width, showfliers=False, patch_artist=False,
            boxprops=dict(color=color, linewidth=1.0, linestyle=ls),
            whiskerprops=dict(color=color, linewidth=0.9, linestyle=ls),
            capprops=dict(color=color, linewidth=0.9, linestyle=ls),
            medianprops=dict(color=color, linewidth=1.0)
        )
        means = [np.nanmean(v) if len(v) else np.nan for v in data]
        ax.scatter(pos, means, color=color, marker=marker, s=28, zorder=3, label=grp)
        # no connecting line here

    ax.set_xticks(DAYS)
    ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS)-0.8, max(DAYS)+0.8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel("Standardized Abnormal Quoted Spread (z)")
    style(ax)
    handles=[
        Line2D([0],[0],color='red',lw=1,label='Nearby',marker='o'),
        Line2D([0],[0],color='blue',lw=1,ls='--',label='Distant',marker='^')
    ]
    ax.legend(handles=handles,frameon=True,facecolor='white',edgecolor='gray',framealpha=.75,loc='best')
    fig.tight_layout()
    plt.savefig(OUT_DIR/"boxplots_dist.png",dpi=300,bbox_inches='tight')
    plt.close()

# ---------- GROUP CI PLOT ----------
def fig_group_ci():
    fig, ax = plt.subplots(figsize=(4.6,4.4))
    for grp, color, marker in [("Nearby","red","o"),("Distant","blue","^")]:
        g = (z_all[z_all["group"]==grp]
             .groupby("day_rel")["z"].agg(mean="mean",std="std",n="count")
             .reindex(DAYS).reset_index())
        g["se"]=g["std"]/np.sqrt(g["n"].where(g["n"]>0,np.nan))
        g["ci"]=1.96*g["se"]
        g["lo"],g["hi"]=g["mean"]-g["ci"],g["mean"]+g["ci"]
        g["sig"]=(g["lo"]>0)|(g["hi"]<0)
        x=np.array(DAYS)
        ax.fill_between(x,g["lo"],g["hi"],color=color,alpha=.15)
        ax.plot(x,g["mean"],color=color,ls='-' if grp=="Nearby" else '--',lw=1.1)
        for xi,m,sig in zip(x,g["mean"],g["sig"]):
            if np.isfinite(m):
                face=color if sig else "white"
                ax.scatter([xi],[m],facecolors=face,edgecolors=color,marker=marker,s=36,zorder=3)
    ax.set_xticks(DAYS)
    ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS)-0.8,max(DAYS)+0.8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel("Standardized Abnormal Quoted Spread (z)")
    style(ax)
    leg=[
        Line2D([0],[0],color="red",lw=1.1,label="Nearby"),
        Patch(facecolor="red",alpha=.15,label="95% CI"),
        Line2D([0],[0],color="blue",lw=1.1,ls="--",label="Distant"),
        Patch(facecolor="blue",alpha=.15,label="95% CI")
    ]
    ax.legend(handles=leg,frameon=True,facecolor="white",edgecolor="gray",framealpha=.8,loc="best")
    fig.tight_layout()
    plt.savefig(OUT_DIR/"ci_dist.png",dpi=300,bbox_inches='tight')
    plt.close()

# ---------- OVERALL BOX & CI ----------
def fig_overall_boxplots():
    data=[z_all[z_all["day_rel"]==d]["z"].dropna().values for d in DAYS]
    fig,ax=plt.subplots(figsize=(4.6,4.4))
    bp=ax.boxplot(data,positions=DAYS,widths=0.25,showfliers=False,patch_artist=False,
                  boxprops=dict(color="black",lw=1.0),
                  whiskerprops=dict(color="black",lw=0.9),
                  capprops=dict(color="black",lw=0.9),
                  medianprops=dict(color="black",lw=1.0))
    means=[np.nanmean(v) if len(v) else np.nan for v in data]
    ax.scatter(DAYS,means,color="black",s=28,zorder=3,label="mean")
    ax.set_xticks(DAYS)
    ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS)-0.8,max(DAYS)+0.8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel("Standardized Abnormal Quoted Spread (z)")
    style(ax)
    ax.legend(frameon=True,facecolor="white",edgecolor="gray",framealpha=0.8)
    fig.tight_layout()
    plt.savefig(OUT_DIR/"boxplots_all.png",dpi=300,bbox_inches='tight')
    plt.close()

def fig_overall_ci():
    g=(z_all.groupby("day_rel")["z"].agg(mean="mean",std="std",n="count").reindex(DAYS).reset_index())
    g["se"]=g["std"]/np.sqrt(g["n"].where(g["n"]>0,np.nan))
    g["ci"]=1.96*g["se"]; g["lo"]=g["mean"]-g["ci"]; g["hi"]=g["mean"]+g["ci"]; g["sig"]=(g["lo"]>0)|(g["hi"]<0)
    x=np.array(DAYS)
    fig,ax=plt.subplots(figsize=(4.6,4.4))
    ax.fill_between(x,g["lo"],g["hi"],color="black",alpha=.12)
    ax.plot(x,g["mean"],color="black",lw=1.1,zorder=1)
    for xi,m,s in zip(x,g["mean"],g["sig"]):
        if np.isfinite(m):
            if s: ax.scatter([xi],[m],color="black",s=30)
            else: ax.scatter([xi],[m],facecolors="white",edgecolors="black",s=30)
    ax.set_xticks(DAYS)
    ax.set_xticklabels([str(d) for d in DAYS])
    ax.set_xlim(min(DAYS)-0.8,max(DAYS)+0.8)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel("Standardized Abnormal Quoted Spread (z)")
    style(ax)
    h=[
        Line2D([0],[0],color="black",lw=1.1,label="mean"),
        Patch(facecolor="black",alpha=.12,label="95% CI"),
        Line2D([0],[0],marker="o",color="black",lw=0,label="significant"),
        Line2D([0],[0],marker="o",mfc="white",mec="black",lw=0,label="not significant")
    ]
    ax.legend(handles=h,frameon=True,facecolor="white",edgecolor="gray",framealpha=.8,loc="best")
    fig.tight_layout()
    plt.savefig(OUT_DIR/"ci_all.png",dpi=300,bbox_inches='tight')
    plt.close()

# ---------- RUN ----------
fig_group_boxplots()
fig_group_ci()
fig_overall_boxplots()
fig_overall_ci()

print(f"✅ Final 4 figures saved to: {OUT_DIR}")
