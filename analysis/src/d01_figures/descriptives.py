# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from shapely.geometry import MultiPolygon
from pathlib import Path
import matplotlib.patheffects as pe

# ---------------- PATHS ----------------

ROOT        = Path(__file__).resolve().parents[2]
DATA_ROOT   = ROOT / "data"
OUTPUT_ROOT = ROOT / "output"

DAILY       = DATA_ROOT / "daily_balanced.csv"
OUT_DIR     = OUTPUT_ROOT / "map"
OUT_DIR.mkdir(parents=True, exist_ok=True)

OUT_FIG     = OUT_DIR / "world_firms_hatched_final.png"

# ---------------- LOAD DATA ----------------

df = pd.read_csv(DAILY, low_memory=False)

required = ["ctriso3", "ric", "nbr_1", "nbr_1_or_2"]
for c in required:
    if c not in df.columns:
        raise ValueError(f"Required column missing: {c}")

df["ctriso3"] = df["ctriso3"].astype(str).str.upper()
df["ric"]     = df["ric"].astype(str)
df["nbr_1"]   = pd.to_numeric(df["nbr_1"], errors="coerce").fillna(0).astype(int)
df["nbr_1_or_2"] = pd.to_numeric(df["nbr_1_or_2"], errors="coerce").fillna(0).astype(int)

df = df[df["ctriso3"] != "nan"]

# ---------------- CONSTRUCT nbr_2 ----------------
df["nbr_2"] = ((df["nbr_1"] == 0) & (df["nbr_1_or_2"] == 1)).astype(int)

# ---------------- COUNTRY STATS ----------------

n_firms  = df.groupby("ctriso3")["ric"].nunique().rename("n_firms")
nbr1_cnt = df.groupby("ctriso3")["nbr_1"].sum().rename("n_nbr1")
nbr2_cnt = df.groupby("ctriso3")["nbr_2"].sum().rename("n_nbr2")
dist_cnt = df.groupby("ctriso3").apply(lambda x: ((x["nbr_1"] == 0) & (x["nbr_1_or_2"] == 0)).sum()).rename("n_dist")

country = pd.concat([n_firms, nbr1_cnt, nbr2_cnt, dist_cnt], axis=1).fillna(0)
country = country.reset_index().rename(columns={"ctriso3": "iso3"})

def classify(row):
    if row["n_firms"] == 0:
        return "none"
    if row["n_nbr1"] > row["n_nbr2"] and row["n_nbr1"] > row["n_dist"]:
        return "nbr1"
    if row["n_nbr2"] > row["n_nbr1"] and row["n_nbr2"] > row["n_dist"]:
        return "nbr2"
    return "distant"

country["cat"] = country.apply(classify, axis=1)

# ---------------- LOAD MAP ----------------

world = gpd.read_file(gpd.datasets.get_path("naturalearth_lowres"))
world = world.rename(columns={"iso_a3": "iso3"})

merged = world.merge(country, on="iso3", how="left")
merged["n_firms"] = merged["n_firms"].fillna(0)
merged["cat"]     = merged["cat"].fillna("none")

# ---------------- COLORS & HATCHES ----------------

RED  = "#FF0000"
BLUE = "#0000FF"
GREY = "#f0f0f0"

HATCH = {
    "nbr1": "ooooo",
    "nbr2": "xxxxx",
    "distant": "/////",
    "none": None
}

EDGECOLOR = {
    "nbr1": RED,
    "nbr2": RED,
    "distant": BLUE,
    "none": "white"
}

# ---------------- SMART LABEL PLACEMENT (original code) ----------------

def get_mainland_centroid(geom):
    if isinstance(geom, MultiPolygon):
        geom = max(geom.geoms, key=lambda g: g.area)
    c = geom.centroid
    if not geom.contains(c):
        c = geom.representative_point()
    return c.x, c.y

def place_labels(ax, gdf):
    placed = []
    for _, row in gdf[gdf["n_firms"] > 0].iterrows():
        geom = row.geometry
        try:
            x,y = get_mainland_centroid(geom)
        except:
            continue

        # avoid overlap
        for (px,py) in placed:
            if abs(px-x)<0.4 and abs(py-y)<0.4:
                x += 0.5
                y += 0.5
        placed.append((x,y))

        ax.text(
            x, y, str(int(row["n_firms"])),
            ha="center", va="center",
            fontsize=5,
            color="white",
            fontweight="bold",
            path_effects=[pe.withStroke(linewidth=1, foreground="black")]
        )

# ---------------- PLOT ----------------

plt.rcParams.update({
    "font.family": "serif",
    "font.size": 12,
    "axes.linewidth": 0.6,
    "figure.dpi": 300,
})

fig, ax = plt.subplots(figsize=(14,7))

# background: no-firm countries
merged[merged["cat"] == "none"].plot(
    ax=ax,
    color=GREY,
    edgecolor="white",
    linewidth=0.3
)

# layer each category WITH HATCHES
for cat in ["nbr1", "nbr2", "distant"]:
    subset = merged[merged["cat"] == cat]
    if len(subset) == 0:
        continue
    subset.plot(
        ax=ax,
        facecolor="white",
        edgecolor=EDGECOLOR[cat],
        hatch=HATCH[cat],
        linewidth=0.35
    )

place_labels(ax, merged)
ax.set_axis_off()

# legend
legend_items = [
    Patch(facecolor="white", edgecolor=RED,  hatch="ooooo", label="Nearby (1st-degree)"),
    Patch(facecolor="white", edgecolor=RED,  hatch="xxxxx", label="Nearby (2nd-degree)"),
    Patch(facecolor="white", edgecolor=BLUE, hatch="/////", label="Distant"),
    Patch(facecolor=GREY,  edgecolor="black", label="No firms")
]

ax.legend(handles=legend_items, loc="lower left",
          frameon=True, facecolor="white", edgecolor="gray", fontsize=10)

plt.tight_layout()
plt.savefig(OUT_FIG)
plt.close()

print(f"Saved map → {OUT_FIG}")