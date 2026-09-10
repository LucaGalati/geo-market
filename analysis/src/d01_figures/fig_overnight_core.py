"""Overnight dynamics of the quoted spread for Distant firms on the night of the
invasion, on a FIXED window 23:00 UTC (23 Feb) -> 07:00 UTC (24 Feb) for every
sample/group, with the real-time event markers. Bins with fewer than MIN_FIRMS
firms are not drawn and the number of firms per bin is shown on the right axis,
so that changes in the composition of open markets are visible instead of
being read as dynamics. `run(sample, group)` draws one cell of the grid."""
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

try:
    from . import fig_common as fc
except ImportError:
    import fig_common as fc

VAR = "qspread_mean"
YLABEL = "Quoted Spread (%)"
WINDOW_START = pd.Timestamp("2022-02-23 23:00", tz="UTC")
WINDOW_END = pd.Timestamp("2022-02-24 07:00", tz="UTC")
MIN_FIRMS = 10

# real-time events (UTC); bold = headline event
EVENTS = [
    ("03:37", "Explosions in multiple Ukrainian cities", False),
    ("03:53", "Missile strikes in Kyiv and Kharkiv (3:53-3:59)", True),
    ("04:55", "Zelensky introduces martial law", False),
    ("05:17", "Attacks on several oblasts; fighting begins", False),
    ("05:55", "Russian aircraft/helicopter shot down", False),
    ("06:36", "Strike with casualties", False),
]

plt.rcParams.update({"font.family": "serif", "font.serif": ["Times New Roman", "DejaVu Serif"],
                     "font.size": 12, "axes.linewidth": 0.8, "figure.dpi": 300})


def _style(ax):
    ax.grid(True, alpha=0.25)
    for sp in ax.spines.values():
        sp.set_linewidth(0.8); sp.set_color("black")


def run(sample: str, group: str):
    out_dir = fc.out_dir("overnight", sample, group)
    tab_dir = fc.tab_dir("overnight", sample, group)
    df = fc.load_panel("intraday", sample, ["ric", "datetime", "nbr_1_or_2", "ctriso3", VAR])
    df["datetime"] = pd.to_datetime(df["datetime"], utc=True, errors="coerce")
    df = df.dropna(subset=["datetime"])
    df = df[(df["datetime"] >= WINDOW_START) & (df["datetime"] < WINDOW_END)]
    df = fc.assign_groups(df, group, "sample_label", stage="figures_overnight", sample=sample)
    df = fc.winsorize(df, [VAR])
    df["val"] = df[VAR] * 100

    overnight = df[df["sample_label"] == "Distant"].dropna(subset=["val"]).copy()
    if overnight.empty:
        print(f"[overnight|{sample}|{group}] no Distant data in the window — skipped.")
        return
    overnight["bin"] = overnight["datetime"].dt.floor("5min")
    recs = []
    for b, s in overnight.groupby("bin"):
        m, v, n_eff = fc.wstats(s["val"], s["w"])
        recs.append({"bin": b, "mean": m, "ci": 1.645 * np.sqrt(v / n_eff) if n_eff > 1 else np.nan,
                     "n_firms": s["ric"].nunique()})
    stats = pd.DataFrame(recs).sort_values("bin")
    stats.loc[stats["n_firms"] < MIN_FIRMS, ["mean", "ci"]] = np.nan
    if stats["mean"].notna().sum() == 0:
        print(f"[overnight|{sample}|{group}] no bin with at least {MIN_FIRMS} Distant firms in the window — skipped.")
        return

    fig, ax = plt.subplots(figsize=(10, 4.8))
    ax.fill_between(stats["bin"], stats["mean"] - stats["ci"], stats["mean"] + stats["ci"], color="blue", alpha=0.20)
    ax.plot(stats["bin"], stats["mean"], color="blue", lw=2.2)
    ax.set_xlim(WINDOW_START, WINDOW_END)
    ymin, ymax = ax.get_ylim()
    for i, (tstr, desc, bold) in enumerate(EVENTS, start=1):
        ts = pd.Timestamp(f"2022-02-24 {tstr}+00:00")
        ax.axvline(ts, color="green", ls="--", lw=2.2 if bold else 0.9)
        ax.text(ts - pd.Timedelta(minutes=6), ymax - (ymax - ymin) * 0.05, f"({i})", color="green",
                fontsize=10, ha="right", va="top", fontweight="bold" if bold else "normal")
    ax.axhline(0, color="gray", ls="--", lw=1)
    ax.set_xlabel("UTC time, night of 23-24 February 2022")
    ax.set_ylabel(YLABEL)
    _style(ax)
    ax2 = ax.twinx()
    ax2.step(stats["bin"], stats["n_firms"], where="post", color="0.45", lw=0.9)
    ax2.set_ylabel("Firms with quotes in the bin", color="0.35")
    ax2.tick_params(axis="y", colors="0.35")
    ax2.set_ylim(0, max(stats["n_firms"].max() * 1.15, MIN_FIRMS))
    n_dist = overnight["ric"].nunique()
    fig.autofmt_xdate(); fig.tight_layout()
    fig.savefig(out_dir / f"{VAR}_overnight.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    stats.to_csv(tab_dir / f"{VAR}_overnight_bins.csv", index=False)
    # composition of the plotted sample for the paper text
    comp = overnight.assign(sfx=overnight["ric"].str.extract(r"\.([A-Za-z0-9]+)$")[0].fillna("NY"))
    n_ctry = comp["ctriso3"].astype(str).str.upper().replace("NAN", np.nan).nunique() if "ctriso3" in comp else np.nan
    pd.DataFrame({"firms": [n_dist], "exchanges": [comp["sfx"].nunique()], "countries": [n_ctry],
                  "bins_total": [len(stats)], "bins_below_min_firms": [int((stats["n_firms"] < MIN_FIRMS).sum())],
                  "min_firms": [MIN_FIRMS]}).to_csv(tab_dir / "overnight_sample.csv", index=False)

    with open(tab_dir / "overnight_events_table.tex", "w") as f:
        f.write("\\begin{table}[ht]\n\\centering\n")
        f.write("\\caption{Real-Time Overnight Events on 24 Feb 2022 (UTC)}\n")
        f.write("\\begin{tabular}{ccl}\\hline\nNo. & Time (UTC) & Event Description \\\\ \\hline\n")
        for i, (tstr, desc, bold) in enumerate(EVENTS, start=1):
            f.write(f"{i} & {tstr} & {desc} \\\\ \n")
        f.write("\\hline\n\\end{tabular}\n\\end{table}\n")
    print(f"✅ overnight [{sample}|{group}] → {out_dir}")
