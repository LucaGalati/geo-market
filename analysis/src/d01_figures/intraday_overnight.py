import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# ============================================================
# PATHS
# ============================================================

DATA_ROOT   = Path(__file__).resolve().parents[2] / "data"
OUTPUT_ROOT = Path(__file__).resolve().parents[2] / "output"
INTRADAY    = DATA_ROOT / "intraday_balanced.csv"

OUT_DIR     = OUTPUT_ROOT / "intraday" / "figures" / "full" / "overnight"
OUT_DIR.mkdir(parents=True, exist_ok=True)

# ============================================================
# SETTINGS
# ============================================================

VAR = "qspread_mean"                    # variable to plot
YLABEL = "Quoted Spread (%)"            # y-axis label

EVENT_DAY = pd.Timestamp("2022-02-24", tz="UTC")
PREV_DAY  = EVENT_DAY - pd.Timedelta(days=1)

# REAL-TIME EVENTS (UTC)
EVENTS = [
    #("03:37", "Explosions in multiple Ukrainian cities", False),
    ("03:53", "Missile strikes in Kyiv and Kharkiv (3:53-3:59)", True),
    #("04:55", "Zelensky introduces martial law", False),
    #("05:17", "Attacks on several oblasts; fighting begins", False),
    #("05:55", "Russian aircraft/helicopter shot down", False),
    #("06:36", "Strike with casualties", False),
]

# ============================================================
# READ AND PREP
# ============================================================

df = pd.read_csv(INTRADAY, low_memory=False)

df["datetime"] = pd.to_datetime(df["datetime"], utc=True, errors="coerce")
df = df.dropna(subset=["datetime"]).copy()

df = df.sort_values(["ric","datetime"])
df["date_utc"] = df["datetime"].dt.normalize()

df["nbr_1_or_2"] = pd.to_numeric(df["nbr_1_or_2"], errors="coerce")
df = df[df["nbr_1_or_2"].isin([0,1])]  # keep only matched firms
df["sample_label"] = df["nbr_1_or_2"].map({1:"Nearby", 0:"Distant"})

df[VAR] = pd.to_numeric(df[VAR], errors="coerce")

# ============================================================
# STYLE
# ============================================================

plt.rcParams.update({
    "font.family": "serif",
    "font.serif": ["Times New Roman"],
    "font.size": 12,
    "axes.linewidth": 0.8,
    "figure.dpi": 300
})

def style(ax):
    ax.grid(True, alpha=0.25)
    for sp in ax.spines.values():
        sp.set_linewidth(0.8)
        sp.set_color("black")

# ============================================================
# OVERNIGHT PLOT
# ============================================================

def plot_overnight():

    # Convert to percent
    df["val"] = df[VAR] * 100
    
    # Last Nearby print on Feb 23
    prev_near = df[(df["sample_label"] == "Nearby") &
                   (df["date_utc"] == PREV_DAY)]
    if prev_near.empty:
        print("No Nearby firms on previous day.")
        return
    last_prev = prev_near["datetime"].max()

    # First Nearby print on Feb 24
    day0_near = df[(df["sample_label"] == "Nearby") &
                   (df["date_utc"] == EVENT_DAY)]
    if day0_near.empty:
        print("No Nearby firms on event day.")
        return
    first_day0 = day0_near["datetime"].min()

    if last_prev >= first_day0:
        print("Invalid overnight interval.")
        return

    # Select distant firms in this interval
    overnight = df[
        (df["sample_label"] == "Distant") &
        (df["datetime"] > last_prev) &
        (df["datetime"] < first_day0)
    ].dropna(subset=["val"]).copy()

    if overnight.empty:
        print("NO OVERNIGHT DATA FOR DISTANT FIRMS")
        return

    # Aggregate to 5-minute bins
    overnight["bin"] = overnight["datetime"].dt.floor("5min")

    stats = (overnight.groupby("bin")["val"]
             .agg(["mean","std","count"])
             .reset_index()
             .sort_values("bin"))
    # Keep only bins with at least 2 observations
    stats = stats[stats["count"] >= 2]

    stats["se"]   = stats["std"] / np.sqrt(stats["count"])
    stats["ci"]   = 1.645 * stats["se"]
    stats["lo"]   = stats["mean"] - stats["ci"]
    stats["hi"]   = stats["mean"] + stats["ci"]

    # Clean numeric
    stats = stats.replace([np.inf, -np.inf], np.nan).dropna(subset=["mean"])
    
    # ===================== PLOT =====================

    fig, ax = plt.subplots(figsize=(10,4.8))

    # CI
    ax.fill_between(stats["bin"], stats["lo"], stats["hi"],
                    color="blue", alpha=0.20)

    # Mean line
    ax.plot(stats["bin"], stats["mean"], color="blue", lw=2.2)

    # Y-limits for positioning labels
    ymin, ymax = ax.get_ylim()

    # EVENT LINES (with numbering)
    for i, (tstr, desc, bold) in enumerate(EVENTS, start=1):
        ts = pd.Timestamp(f"2022-02-24 {tstr}+00:00")
        lw = 2.2 if bold else 0.9
        ax.axvline(ts, color="green", ls="--")

        # # number label
        # ax.text(
        #     ts - pd.Timedelta(minutes=6),     # shift label 6 minutes left of the line
        #     ymax - (ymax - ymin) * 0.05,      # place label 5% below the top
        #     f"({i})",
        #     color="green",
        #     fontsize=10,
        #     ha="right",                       # align to the right (toward the line)
        #     va="top",
        #     fontweight="bold" if bold else "normal"
        # )

    ax.axhline(0, color="gray", ls="--", lw=1)

    ax.set_title("Overnight Dynamics (Distant Firms Only)",
                 fontsize=16, fontweight="bold")
    ax.set_xlabel("UTC Time Overnight")
    ax.set_ylabel(YLABEL)

    style(ax)
    fig.autofmt_xdate()
    fig.tight_layout()

    out_file = OUT_DIR / f"{VAR}_overnight.png"
    fig.savefig(out_file, dpi=300, bbox_inches="tight")
    plt.close()

    print("Saved →", out_file)

# ============================================================
# LATEX TABLE
# ============================================================

def write_latex_table():

    path = OUT_DIR / "overnight_events_table.tex"

    with open(path, "w") as f:
        f.write("\\begin{table}[ht]\n\\centering\n")
        f.write("\\caption{Real-Time Overnight Events on 24 Feb 2022 (UTC)}\n")
        f.write("\\begin{tabular}{ccl}\\hline\n")
        f.write("No. & Time (UTC) & Event Description \\\\ \\hline\n")

        for i, (tstr, desc, bold) in enumerate(EVENTS, start=1):
            f.write(f"{i} & {tstr} & {desc} \\\\ \n")

        f.write("\\hline\n\\end{tabular}\n\\end{table}\n")

    print("Saved →", path)

# ============================================================
# RUN
# ============================================================

if __name__ == "__main__":
    plot_overnight()
    write_latex_table()
    print("DONE.")