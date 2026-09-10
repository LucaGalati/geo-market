"""Daily event-study figures. `run(sample, group)` draws every variable for one
sample × group combination; daily.py loops over the 2×2 grid."""
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

try:
    from . import fig_common as fc
except ImportError:
    import fig_common as fc

EVENT_DATE = pd.Timestamp("2022-02-24", tz="UTC")

VARS_TO_PLOT = [
    "volume_mean", "qspread_mean", "espread_mean",
    "price_impact_mean", "realized_spread_mean", "price_impact_fi_mean", "realized_spread_fi_mean",
    "logret_mean", "sqr_logret_mean", "volatility", "log_volatility",
    "depth_mean", "depth_value_mean", "avg_trade_size", "avg_quote_size",
    "dollar_volume_mean", "volume_sum", "trades_count", "quotes_count",
    "depth_sum", "depth_value_sum", "dollar_volume_sum",   # dvolume_* (price_mean x volume) duplicate dollar_volume_*
]

PERCENT_PATTERNS = ["spread", "impact", "logret"]
PERCENT_EXACT = {"volatility", "log_volatility"}
SQUARED_PERCENT = {"sqr_logret_mean"}          # x1e4 -> %^2
NOLOG_PATTERNS = ["spread", "impact", "ret", "std", "price"]
NOLOG_EXACT = {"volatility", "log_volatility"}

# each impact measure and its realized spread are averaged over the SAME rows
IMPACT_REALIZED = {
    "price_impact_mean": ("price_impact_mean", "realized_spread_mean"),
    "realized_spread_mean": ("price_impact_mean", "realized_spread_mean"),
    "price_impact_fi_mean": ("price_impact_fi_mean", "realized_spread_fi_mean"),
    "realized_spread_fi_mean": ("price_impact_fi_mean", "realized_spread_fi_mean"),
}

Y_LABELS = {
    "volume_mean": "Average Shares (log)",
    "qspread_mean": "Quoted Spread (%)",
    "espread_mean": "Effective Spread (%)",
    "logret_mean": "Average Log Returns (%)",
    "sqr_logret_mean": "Average Squared Log Returns (%$^2$)",
    "depth_mean": "Average Market Depth (log)",
    "depth_value_mean": "Average Market Depth Value (log)",
    "avg_trade_size": "Average Trade Size (log)",
    "avg_quote_size": "Average Quote Size (log)",
    "volume_sum": "Shares (log)",
    "trades_count": "Number of Trades (log)",
    "quotes_count": "Number of Quotes (log)",
    "depth_sum": "Market Depth (log)",
    "depth_value_sum": "Market Depth Value (log)",
    "dollar_volume_sum": "Dollar Volume (USD, log)",
    "dollar_volume_mean": "Average Dollar Volume (USD, log)",
    "dvolume_sum": "Dollar Volume 2 (USD, log)",
    "dvolume_mean": "Average Dollar Volume 2 (USD, log)",
    "price_impact_mean": "Price Impact (%)",
    "realized_spread_mean": "Realized Spread (%)",
    "price_impact_fi_mean": "Price Impact, FI direction (%)",
    "realized_spread_fi_mean": "Realized Spread, FI direction (%)",
    "volatility": "Volatility (%)",
    "log_volatility": "Log Volatility (%)",
}

WINDOWS = {"window_10days": (-10, 10), "window_15days": (-15, 15), "full_sample": (None, None)}

plt.rcParams.update({
    "text.usetex": False, "font.family": "serif",
    "font.serif": ["Times New Roman", "Computer Modern Roman", "DejaVu Serif"],
    "font.size": 12, "axes.labelsize": 12, "legend.fontsize": 11,
    "xtick.labelsize": 11, "ytick.labelsize": 11, "axes.linewidth": 0.7, "figure.dpi": 300,
})


def _load(sample, group):
    df = fc.load_panel("daily", sample, ["ric", "date", "nbr_1_or_2", *VARS_TO_PLOT])
    df["date"] = pd.to_datetime(df["date"], errors="coerce", utc=True).dt.normalize()
    df = df.dropna(subset=["date"]).sort_values(["ric", "date"], kind="mergesort")
    df = fc.assign_groups(df, group, "sample_label", stage="figures_daily", sample=sample)

    unique_dates = np.sort(df["date"].unique())
    idx = pd.Series(range(len(unique_dates)), index=pd.to_datetime(unique_dates))
    if EVENT_DATE not in idx.index:
        raise ValueError(f"Event {EVENT_DATE.date()} not found in the panel dates.")
    df["days_from_event"] = df["date"].map(idx) - idx[EVENT_DATE]
    return df.reset_index(drop=True)


def _transform(df, available):
    pct = [v for v in available if (any(k in v for k in PERCENT_PATTERNS) or v in PERCENT_EXACT)
           and v not in SQUARED_PERCENT]
    df[pct] = df[pct] * 100
    sq = [v for v in available if v in SQUARED_PERCENT]
    df[sq] = df[sq] * 1e4
    for v in available:
        if (not any(p in v for p in NOLOG_PATTERNS) and v not in NOLOG_EXACT
                and v not in PERCENT_EXACT and v not in SQUARED_PERCENT):
            df[v] = np.log(df[v].where(df[v] > 0))
    return df


def _aggregate_pairwise(df_window, varname):
    impact_var, realized_var = IMPACT_REALIZED[varname]
    both = df_window[df_window[impact_var].notna() & df_window[realized_var].notna()]
    return (fc.wmean_by(both, ["days_from_event", "sample_label"], [impact_var, realized_var])
              .sort_values("days_from_event"))


def _plot(varname, ylabel, df_data, out_dir):
    pivoted = df_data.pivot(index="days_from_event", columns="sample_label", values=varname).sort_index()
    if pivoted.empty or pivoted.isna().all().all():
        return
    fig, ax = plt.subplots(figsize=(4.3, 3.8))
    if "Nearby" in pivoted:
        ax.plot(pivoted.index, pivoted["Nearby"], color="red", linewidth=1, label="Nearby")
    if "Distant" in pivoted:
        ax.plot(pivoted.index, pivoted["Distant"], color="blue", linestyle="--", linewidth=1, label="Distant")
    ax.axvspan(0, pivoted.index.max(), color="green", alpha=0.2)
    ax.axvline(0, color="green", linestyle="--", linewidth=1)
    ax.set_xlabel("Trading Days Around Conflict Onset")
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.25)
    for spine in ax.spines.values():
        spine.set_visible(True); spine.set_linewidth(0.7); spine.set_color("black")
    if ax.get_legend_handles_labels()[0]:
        ax.legend(frameon=True, facecolor="white", edgecolor="gray", framealpha=0.4, loc="best")
    fig.tight_layout()
    plt.savefig(out_dir / f"{varname}.png", dpi=300, bbox_inches="tight")
    plt.close(fig)


def run(sample: str, group: str):
    df = _load(sample, group)
    available = [v for v in VARS_TO_PLOT if v in df.columns]
    missing = [v for v in VARS_TO_PLOT if v not in df.columns]
    if missing:
        print(f"[daily|{sample}|{group}] columns not in data (skipped): {missing}")

    df = fc.winsorize(df, available)          # analysis-level 1/99, raw units
    df = _transform(df, available)

    daily = fc.wmean_by(df, ["days_from_event", "sample_label"], available).sort_values("days_from_event")

    for sub, (lo, hi) in WINDOWS.items():
        out = fc.out_dir("daily", sample, group, sub)
        if lo is None:
            d_win, df_win = daily, df
        else:
            d_win = daily[daily["days_from_event"].between(lo, hi)]
            df_win = df[df["days_from_event"].between(lo, hi)]
        for var in available:
            pair = IMPACT_REALIZED.get(var)
            if pair and pair[0] in df.columns and pair[1] in df.columns:
                data = _aggregate_pairwise(df_win, var)
            else:
                data = d_win
            _plot(var, fc_label(var), data, out)
    print(f"✅ daily figures [{sample}|{group}] → {fc.out_dir('daily', sample, group)}")


def fc_label(var):
    return Y_LABELS.get(var, var)
