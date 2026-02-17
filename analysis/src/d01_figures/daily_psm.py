import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from datetime import date

# ---------- PATHS ----------
DATA_ROOT   = Path(__file__).resolve().parents[2] / "data"
OUTPUT_ROOT = Path(__file__).resolve().parents[2] / "output"
DAILY       = DATA_ROOT / "daily_balanced.csv"
OUT_DAILY   = OUTPUT_ROOT / "daily" / "figures"
OUT_DAILY.mkdir(parents=True, exist_ok=True)

# ---------- READ DATA ----------
df = pd.read_csv(DAILY, low_memory=False)

# Usa 'date' e non 'datetime'
df["date"] = pd.to_datetime(df["date"], errors="coerce", utc=True).dt.normalize()
df = df.sort_values(["ric", "date"], kind="mergesort").reset_index(drop=True)

# ---------- EVENT DATE ----------
event_date = pd.Timestamp("2022-02-24", tz="UTC")

# ---------- TRADING DAY INDEX ----------
unique_dates = np.sort(df["date"].dropna().unique())
trading_day_index = pd.Series(range(len(unique_dates)), index=pd.to_datetime(unique_dates).normalize())

event_trading_index = trading_day_index.get(event_date, None)
if event_trading_index is None:
    raise ValueError(f"Event {event_date.date()} not found in the dataset! "
                     f"Available sample dates: {unique_dates[:5]}")

df["trading_day_index"] = df["date"].map(trading_day_index)
df["days_from_event"] = df["trading_day_index"] - event_trading_index

# ---------- LABEL GROUPS ----------
label_map = {1: "Nearby", 0: "Distant"}
df["psm_group"] = pd.to_numeric(df["psm_group"], errors="coerce")
df = df[df["psm_group"].isin([0, 1])].copy()
df["sample_label"] = df["psm_group"].map(label_map)

# ---------- VARIABLES ----------
vars_to_plot = [
    "volume_mean", "qspread_mean", "espread_mean", "espread2_mean",
    "priceimpact_mean", "priceimpact2_mean", "w_espread2_mean", "intraday_vol_mean",
    "w_priceimpact2_mean", "logret_mean", "sqr_logret_mean", "intraday_5m_vol_mean",
    "depth_mean", "depth_value_mean", "price_std", "avg_trade_size",
    "avg_quote_size", "dollar_volume_mean", "rspread_mean", "rspread2_mean",
    "w_rspread2_mean", "volume_sum", "trades_count", "quotes_count",
    "depth_sum", "depth_value_sum", "dollar_volume_sum", "dvolume_mean", "dvolume_sum"
]

# ---------- TRANSFORMATIONS ----------
# 1) Variables in percent
liquidity_vars = [v for v in vars_to_plot if any(k in v for k in ["spread", "impact", "realized", "logret"])]
df[liquidity_vars] = df[liquidity_vars] * 100

# 2) Log-transformation of other variables
exclude_patterns = ["spread", "impact", "ret", "std", "price", "volatility", "rspread"]
log_candidates = [v for v in vars_to_plot if not any(p in v for p in exclude_patterns)]
for v in log_candidates:
    if v in df.columns:
        df[v] = np.log(df[v].where(df[v] > 0))

# ---------- AGGREGATE DAILY ----------
daily = (
    df.groupby(["days_from_event", "sample_label"], dropna=False)[vars_to_plot]
      .mean(numeric_only=True)
      .reset_index()
      .sort_values("days_from_event")
)

# ---------- Y-AXIS LABELS ----------
y_labels = {
    "volume_mean": "Average Shares (log)",
    "qspread_mean": "Quoted Spread (%)",
    "espread_mean": "Effective Spread (%)",
    "espread2_mean": "Signed Effective Spread (%)",
    "priceimpact_mean": "Price Impact (%)",
    "priceimpact2_mean": "Signed Price Impact (%)",
    "w_espread2_mean": "Weighted Effective Spread (%)",
    "w_priceimpact2_mean": "Weighted Price Impact (%)",
    "logret_mean": "Average Log Returns (%)",
    "sqr_logret_mean": "Average Squared Log Returns (%)",
    "depth_mean": "Average Market Depth (log)",
    "depth_value_mean": "Average Market Depth Value (log)",
    "price_std": "Price STDEV",
    "avg_trade_size": "Average Trade Size (log)",
    "avg_quote_size": "Average Quote Size (log)",
    "avg_turnover": "Average Turnover (log)",
    "rspread_mean": "Realized Spread (%)",
    "rspread2_mean": "Signed Realized Spread (%)",
    "w_rspread2_mean": "Weighted Realized Spread (%)",
    "volume_sum": "Shares (log)",
    "trades_count": "Number of Trades (log)",
    "quotes_count": "Number of Quotes (log)",
    "depth_sum": "Market Depth (log)",
    "depth_value_sum": "Market Depth Value (log)",
    "turnover": "Turnover (log)",
    "dollar_volume_sum": "Dollar Volume (log)",
    "dollar_volume_mean": "Average Dollar Volume (log)",
    "dvolume_sum": "Dollar Volume 2 (log)",
    "dvolume_mean": "Average Dollar Volume 2 (log)",
    "intraday_5m_vol_mean": "Intraday Volatility (5-minute)",
    "intraday_vol_mean": "Intraday Volatility (open-close)"
}

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

# ---------- PLOT FUNCTION ----------
def plot_event_series(varname, ylabel, df_data, out_dir):
    pivoted = (
        df_data.pivot(index="days_from_event", columns="sample_label", values=varname)
        .sort_index()
    )
    if pivoted.isna().all().all():
        return

    fig, ax = plt.subplots(figsize=(4.3, 3.8))
    ax.plot(pivoted.index, pivoted.get("Nearby"), color="red", linewidth=1, label="Nearby")
    ax.plot(pivoted.index, pivoted.get("Distant"), color="blue", linestyle="--", linewidth=1, label="Distant")

    # Highlight post-event period
    ax.axvspan(0, pivoted.index.max(), color="green", alpha=0.2)
    ax.axvline(0, color="green", linestyle="--", linewidth=1)

    ax.set_title('First-/Second-degree Neighbours (448 firms)', fontsize=14, fontweight='bold')
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.25)

    for spine in ax.spines.values():
        spine.set_visible(True)
        spine.set_linewidth(0.7)
        spine.set_color("black")

    # Legend
    handles, labels = ax.get_legend_handles_labels()
    if any(h is not None for h in handles):
        ax.legend(frameon=True, facecolor="white", edgecolor="gray", framealpha=0.4, loc="best")

    fig.tight_layout()
    plt.savefig(out_dir / f"{varname}.png", dpi=300, bbox_inches="tight")
    plt.close()

# ---------- OUTPUT DIRECTORIES ----------
window10_dir  = OUT_DAILY / "psm" / "window_10days"
window15_dir  = OUT_DAILY / "psm" / "window_15days"
fullsample_dir = OUT_DAILY / "psm" / "full_sample"
window10_dir.mkdir(parents=True, exist_ok=True)
window15_dir.mkdir(parents=True, exist_ok=True)
fullsample_dir.mkdir(parents=True, exist_ok=True)

# ---------- PLOT SETS ----------
daily_10 = daily[(daily["days_from_event"] >= -10) & (daily["days_from_event"] <= 10)]
daily_15 = daily[(daily["days_from_event"] >= -15) & (daily["days_from_event"] <= 15)]

for var in vars_to_plot:
    ylabel = y_labels.get(var, var)
    plot_event_series(var, ylabel, daily_10, window10_dir)

for var in vars_to_plot:
    ylabel = y_labels.get(var, var)
    plot_event_series(var, ylabel, daily_15, window15_dir)

for var in vars_to_plot:
    ylabel = y_labels.get(var, var)
    plot_event_series(var, ylabel, daily, fullsample_dir)
