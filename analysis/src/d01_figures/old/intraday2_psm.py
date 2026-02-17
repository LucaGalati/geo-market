import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# ---------- PATHS ----------
DATA_ROOT    = Path(__file__).resolve().parents[2] / "data"
OUTPUT_ROOT  = Path(__file__).resolve().parents[2] / "output"
INTRADAY     = DATA_ROOT / "intraday_balanced_psm.csv"

OUT_BASE  = OUTPUT_ROOT / "intraday" / "figures" / "psm" / "mean"
OUT_FIVE  = OUT_BASE / "five_trading_days"
OUT_BOUND = OUT_BASE / "close_open"
OUT_24H   = OUT_BASE / "high_frequency"
for p in [OUT_BASE, OUT_FIVE, OUT_BOUND, OUT_24H]:
    p.mkdir(parents=True, exist_ok=True)

# ---------- READ DATA ----------
df = pd.read_csv(INTRADAY, low_memory=False)

# ---------- TARGET VARIABLES ----------
vars_to_plot = ["qspread_mean", "espread_mean", "dollar_volume_sum", "trades_count", "intraday_5m_vol_mean", "intraday_vol_mean"]
present_vars = [v for v in vars_to_plot if v in df.columns]
if not present_vars:
    raise ValueError(f"No target variables found. Expected one of: {vars_to_plot}")

# ---------- DATETIME ----------
if "datetime" not in df.columns:
    raise ValueError("Expected a 'datetime' column in intraday_balanced.csv")

df["datetime"] = pd.to_datetime(df["datetime"], errors="coerce", utc=True)
df = df.dropna(subset=["datetime"]).copy()

# Sort safely (does not drop any columns)
if "ric" in df.columns:
    df.sort_values(["ric", "datetime"], kind="mergesort", inplace=True, ignore_index=True)
else:
    df.sort_values("datetime", kind="mergesort", inplace=True, ignore_index=True)

# ---------- LABEL GROUPS ----------
label_map = {1: "Nearby", 0: "Distant"}
if "psm_group" in df.columns:
    df["psm_group"] = pd.to_numeric(df["psm_group"], errors="coerce")
    df["sample_label"] = df["psm_group"].map(label_map)
else:
    df["sample_label"] = "All"

# ---------- CONVERT NUMERIC ----------
for v in present_vars:
    df[v] = pd.to_numeric(df[v], errors="coerce")

# ---------- LOG TRANSFORM FOR VOLUME AND TRADES ----------
for v in ["dollar_volume_sum", "trades_count"]:
    if v in df.columns:
        df[v] = np.log(df[v].where(df[v] > 0))

# ---------- EVENT & TRADING DAY INDEX ----------
event_day = pd.Timestamp("2022-02-24", tz="UTC")
df["date_utc"] = df["datetime"].dt.normalize()

unique_days = np.sort(df["date_utc"].dropna().unique())
day_index = pd.Series(range(len(unique_days)), index=unique_days)
if event_day not in day_index.index:
    raise ValueError(f"Event day {event_day.date()} not present in data.")

event_idx = int(day_index.loc[event_day])
df["trading_day_index"] = df["date_utc"].map(day_index)
df["days_from_event"] = df["trading_day_index"] - event_idx

# ---------- MATPLOTLIB STYLE ----------
plt.rcParams.update({
    "text.usetex": False,
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Computer Modern Roman", "DejaVu Serif"],
    "font.size": 12,
    "axes.labelsize": 12,
    "legend.fontsize": 11,
    "xtick.labelsize": 11,
    "ytick.labelsize": 11,
    "axes.linewidth": 0.7,
    "figure.dpi": 300,
})

def _style_axes(ax):
    ax.grid(True, alpha=0.25)
    for sp in ax.spines.values():
        sp.set_visible(True); sp.set_linewidth(0.7); sp.set_color("black")

# ---------- A) FIVE TRADING DAYS ----------
def plot_five_trading_days(varname: str, ylabel: str):
    dsub = df[(df["days_from_event"] >= -5) & (df["days_from_event"] <= 5)].dropna(subset=[varname])
    if dsub.empty: return

    fig, ax = plt.subplots(figsize=(6.4, 3.8))
    distant = dsub[dsub["sample_label"] == "Distant"]
    nearby  = dsub[dsub["sample_label"] == "Nearby"]

    ax.scatter(distant["datetime"], distant[varname],
               s=5, color="blue", alpha=0.35, marker="^", label="Distant", zorder=1)
    ax.scatter(nearby["datetime"], nearby[varname],
               s=5, color="red", alpha=0.6, marker="o", label="Nearby", zorder=2)

    # Shade post-event region
    ev_start = event_day
    ev_end = dsub["datetime"].max()
    ax.axvspan(ev_start, ev_end, color="green", alpha=0.15, zorder=0)
    ax.axvline(ev_start, color="green", linestyle="--", linewidth=1)
    ax.set_title('First-/Second-degree Neighbours (54 firms)', fontsize=14, fontweight='bold')
    ax.set_xlabel("UTC Datetime")
    ax.set_ylabel(ylabel)
    ax.legend(frameon=True, facecolor="white", edgecolor="gray", framealpha=0.5, loc="best")
    _style_axes(ax)

    fig.autofmt_xdate()
    fig.tight_layout()
    plt.savefig(OUT_FIVE / f"{varname}.png", dpi=300, bbox_inches="tight")
    plt.close()

# ---------- B) BOUNDARY POINTS ----------
def plot_boundary_points(varname: str, ylabel: str):
    if "ric" not in df.columns: return
    prev_day = df[df["days_from_event"] == -1].dropna(subset=[varname])
    day0     = df[df["days_from_event"] == 0 ].dropna(subset=[varname])
    if prev_day.empty or day0.empty: return

    # Last obs of day –1, first of day 0
    idx_last  = prev_day.groupby("ric")["datetime"].idxmax()
    idx_first = day0.groupby("ric")["datetime"].idxmin()
    prev_last = prev_day.loc[idx_last]
    day0_first = day0.loc[idx_first]

    fig, ax = plt.subplots(figsize=(6.4, 3.8))

    # Distant (blue triangles)
    mask_d_prev = prev_last["sample_label"] == "Distant"
    mask_d_first = day0_first["sample_label"] == "Distant"
    ax.scatter(prev_last.loc[mask_d_prev, "datetime"], prev_last.loc[mask_d_prev, varname],
               s=12, marker="^", facecolors="none", edgecolors="blue", alpha=0.9,
               label="Distant: Last (-1d)", zorder=2)
    ax.scatter(day0_first.loc[mask_d_first, "datetime"], day0_first.loc[mask_d_first, varname],
               s=14, marker="^", color="blue", alpha=0.7,
               label="Distant: First (0d)", zorder=3)

    # Nearby (red circles)
    mask_n_prev = prev_last["sample_label"] == "Nearby"
    mask_n_first = day0_first["sample_label"] == "Nearby"
    ax.scatter(prev_last.loc[mask_n_prev, "datetime"], prev_last.loc[mask_n_prev, varname],
               s=12, marker="o", facecolors="none", edgecolors="red", alpha=0.9,
               label="Nearby: Last (-1d)", zorder=4)
    ax.scatter(day0_first.loc[mask_n_first, "datetime"], day0_first.loc[mask_n_first, varname],
               s=14, marker="o", color="red", alpha=0.7,
               label="Nearby: First (0d)", zorder=5)

    ax.axvline(event_day, color="green", linestyle="--", linewidth=1)
    ax.set_title('First-/Second-degree Neighbours (54 firms)', fontsize=14, fontweight='bold')
    ax.set_xlabel("UTC Datetime (5-min native ticks)")
    ax.set_ylabel(ylabel)
    ax.legend(frameon=True, facecolor="white", edgecolor="gray", framealpha=0.6, loc="best")
    _style_axes(ax)

    fig.autofmt_xdate()
    fig.tight_layout()
    plt.savefig(OUT_BOUND / f"{varname}.png", dpi=300, bbox_inches="tight")
    plt.close()

# ---------- C) ±24 HOURS AROUND 03:59 UTC ----------
def plot_around_0359(varname: str, ylabel: str):
    t0 = pd.Timestamp("2022-02-24 03:59:00+00:00")
    t1, t2 = t0 - pd.Timedelta(hours=24), t0 + pd.Timedelta(hours=24)
    dsub = df[(df["datetime"] >= t1) & (df["datetime"] <= t2)].dropna(subset=[varname])
    if dsub.empty: return

    fig, ax = plt.subplots(figsize=(7.2, 3.8))
    distant = dsub[dsub["sample_label"] == "Distant"]
    nearby  = dsub[dsub["sample_label"] == "Nearby"]

    ax.scatter(distant["datetime"], distant[varname],
               s=5, color="blue", alpha=0.35, marker="^", label="Distant", zorder=1)
    ax.scatter(nearby["datetime"], nearby[varname],
               s=5, color="red", alpha=0.6, marker="o", label="Nearby", zorder=2)

    ax.axvline(t0, color="green", linestyle="--", linewidth=1)
    ax.axvspan(t0, t2, color="green", alpha=0.15, zorder=0)
    ax.set_title('First-/Second-degree Neighbours (54 firms)', fontsize=14, fontweight='bold')
    ax.set_xlabel("UTC Datetime")
    ax.set_ylabel(ylabel)
    ax.legend(frameon=True, facecolor="white", edgecolor="gray", framealpha=0.5, loc="best")
    _style_axes(ax)

    fig.autofmt_xdate()
    fig.tight_layout()
    plt.savefig(OUT_24H / f"{varname}.png", dpi=300, bbox_inches="tight")
    plt.close()

# ---------- LABELS ----------
y_labels = {
    "qspread_mean": "Quoted Spread (%)",
    "espread_mean": "Effective Spread (%)",
    "trades_count": "Number of Trades (log)",
    "dollar_volume_sum": "Dollar Volume (log)",
    "intraday_5m_vol_mean": "Intraday Volatility (5-minute)",
    "intraday_vol_mean": "Intraday Volatility (open-close)"
}

# ---------- RUN ----------
for var in vars_to_plot:
    if var not in df.columns:
        print(f"Skipping missing variable: {var}")
        continue
    ylabel = y_labels.get(var, var)
    plot_five_trading_days(var, ylabel)
    plot_boundary_points(var, ylabel)
    plot_around_0359(var, ylabel)

print(f"\n✅ Plots saved:\n - {OUT_FIVE}\n - {OUT_BOUND}\n - {OUT_24H}")