from pathlib import Path
import pandas as pd
import numpy as np

DATA_ROOT   = Path(__file__).resolve().parents[3] / "data"
OUT_DIR     = DATA_ROOT / "02_preprocessed" / "trth" / "checks"
OUT_DIR.mkdir(parents=True, exist_ok=True)

SAMPLE_TICKS = OUT_DIR / "ticks_sample.csv"
MAIN_5M      = DATA_ROOT / "02_preprocessed" / "trth" / "intraday5" / "ukraine02.csv"
MAIN_DAILY   = DATA_ROOT / "02_preprocessed" / "trth" / "daily"     / "ukraine02.csv"

OUT_COMP_5M  = OUT_DIR / "compare_5m.csv"
OUT_COMP_D   = OUT_DIR / "compare_daily.csv"

def parse_gmt_to_hours(gmt_value) -> float:
    if pd.isna(gmt_value): return np.nan
    s = str(gmt_value).strip()
    if s == "": return np.nan
    sign = 1
    if s.startswith("-"):
        sign = -1; s = s[1:]
    elif s.startswith("+"):
        s = s[1:]
    try:
        return sign * float(s)
    except Exception:
        return np.nan

def compute_direction(price, prev_mid):
    if pd.isna(price) or pd.isna(prev_mid): return np.nan
    if price > prev_mid: return 1
    if price < prev_mid: return -1
    return 0

def winsorize_series(s: pd.Series, lower_q=0.001, upper_q=0.9999):
    if s.empty: return s
    low, high = s.quantile([lower_q, upper_q])
    return s.clip(lower=low, upper=high)

def add_mid_future(group: pd.DataFrame) -> pd.DataFrame:
    dt = group["datetime"].to_numpy(dtype="datetime64[ns]")
    mid_ref = group["mid_ref"].to_numpy(dtype="float64")
    target = (group["datetime"] + pd.Timedelta(minutes=5)).to_numpy(dtype="datetime64[ns]")
    idx = np.searchsorted(dt, target, side="right") - 1
    valid = idx >= 0
    out = np.full(len(group), np.nan)
    out[valid] = mid_ref[idx[valid]]
    group["mid_ref_future"] = out
    return group

def main():
    # ----- read tiny sample -----
    df = pd.read_csv(SAMPLE_TICKS)
    if df.empty:
        raise RuntimeError("ticks_sample.csv is empty.")
    df["datetime"] = pd.to_datetime(df["datetime"], utc=True, errors="coerce")
    for col in ["price","volume","bid","ask"]:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")

    # ----- replicate pipeline logic on the small sample -----
    df["gmt_hours"] = df["gmt"].apply(parse_gmt_to_hours)
    local_offset = pd.to_timedelta(df["gmt_hours"], unit="h")
    df["local_dt"] = df["datetime"] + local_offset
    df["date_local"] = df["local_dt"].dt.date

    # invalid quotes: ask<bid; invalid trades: negative price or volume
    is_quote = df["type"].astype(str).str.upper().eq("QUOTE")
    is_trade = df["type"].astype(str).str.upper().eq("TRADE")
    bad_quote = is_quote & df["ask"].notna() & df["bid"].notna() & (df["ask"] < df["bid"])
    bad_trade = is_trade & ((df["price"] < 0) | (df["volume"] < 0))
    df = df.loc[~bad_quote & ~bad_trade].copy()

    # mid from quotes only, carry forward
    df["mid"] = np.where(df["ask"].notna() & df["bid"].notna(), (df["ask"] + df["bid"]) / 2, np.nan)
    df["mid_quote"] = np.where(is_quote, df["mid"], np.nan)

    df = df.sort_values(["ric","date_local","datetime"], kind="mergesort")
    # NOTE: matching production: forward-fill only (no backfill)
    df["mid_ref"] = df.groupby(["ric","date_local"], group_keys=False)["mid_quote"].ffill()

    df["prev_mid_ref"] = df.groupby(["ric","date_local"])["mid_ref"].shift(1)
    df["direction"] = np.vectorize(compute_direction)(df["price"], df["prev_mid_ref"])

    # spreads
    df["q_spread"] = np.where(is_quote & df["mid"].notna(), (df["ask"] - df["bid"]) / df["mid"], np.nan)
    df["e_spread"] = np.where(is_trade & df["mid_ref"].notna() & df["price"].notna(),
                              (df["price"] - df["mid_ref"]).abs() / df["mid_ref"], np.nan)
    df["e_spread2"] = np.where(is_trade & df["e_spread"].notna() & df["direction"].notna(),
                               2 * df["e_spread"] * df["direction"], np.nan)

    # price impact via 5-min future mid_ref
    df = df.groupby(["ric","date_local"], group_keys=False).apply(add_mid_future)
    df["price_impact"] = np.where(
        is_trade & df["mid_ref"].notna() & df["mid_ref_future"].notna(),
        ((df["mid_ref_future"] - df["mid_ref"]) / df["mid_ref"]) * df["direction"],
        np.nan
    )
    df.loc[df["price_impact"] == 0, "price_impact"] = np.nan
    df["price_impact2"] = np.where(df["price_impact"].notna(), 2 * df["price_impact"], np.nan)

    # winsorize per RIC like production
    for col in ["price","volume","q_spread","e_spread","e_spread2","price_impact","price_impact2"]:
        df[col] = df.groupby("ric", group_keys=False)[col].transform(lambda x: winsorize_series(x))

    # ----- aggregate from sample -----
    # 5-minute (UTC bins)
    df["dt_5m"] = df["datetime"].dt.floor("5min")
    agg5 = (
        df.groupby(["ric","dt_5m"], as_index=False)
          .agg(
              price_mean=("price","mean"),
              volume_mean=("volume","mean"),
              qspread_mean=("q_spread","mean"),
              espread_mean=("e_spread","mean"),
              espread2_mean=("e_spread2","mean"),
              priceimpact_mean=("price_impact","mean"),
              priceimpact2_mean=("price_impact2","mean"),
              price_std=("price","std"),
              gmt_first=("gmt","first"),
          )
          .rename(columns={"dt_5m":"datetime","gmt_first":"gmt"})
    )

    # daily (local day)
    df["day_local"] = df["date_local"]
    aggD = (
        df.groupby(["ric","day_local"], as_index=False)
          .agg(
              price_mean=("price","mean"),
              volume_mean=("volume","mean"),
              qspread_mean=("q_spread","mean"),
              espread_mean=("e_spread","mean"),
              espread2_mean=("e_spread2","mean"),
              priceimpact_mean=("price_impact","mean"),
              priceimpact2_mean=("price_impact2","mean"),
              price_std=("price","std"),
              gmt_first=("gmt","first"),
          )
    )
    aggD["datetime"] = pd.to_datetime(aggD["day_local"])
    aggD = aggD.drop(columns=["day_local"]).rename(columns={"gmt_first":"gmt"})

    # ----- load main outputs & subset to these RIC×days -----
    main5 = pd.read_csv(MAIN_5M, parse_dates=["datetime"])
    mainD = pd.read_csv(MAIN_DAILY, parse_dates=["datetime"])

    # restrict to the two RIC×days present in the sample
    days_by_ric = (
        df.groupby("ric")["date_local"]
          .apply(lambda s: sorted(set(s)))
          .to_dict()
    )

    sub5 = []
    for ric, days in days_by_ric.items():
        mask = (main5["ric"] == ric) & (main5["datetime"].dt.date.astype(str).isin(set(str(d) for d in days)))
        sub5.append(main5.loc[mask])
    sub5 = pd.concat(sub5, ignore_index=True) if sub5 else main5.iloc[0:0]

    subD = []
    for ric, days in days_by_ric.items():
        mask = (mainD["ric"] == ric) & (mainD["datetime"].dt.date.astype(str).isin(set(str(d) for d in days)))
        subD.append(mainD.loc[mask])
    subD = pd.concat(subD, ignore_index=True) if subD else mainD.iloc[0:0]

    # ----- compare -----
    comp5 = agg5.merge(
        sub5,
        on=["ric","datetime"],
        suffixes=("_recalc","_main"),
        how="outer",
        indicator=True
    )
    compD = aggD.merge(
        subD,
        on=["ric","datetime"],
        suffixes=("_recalc","_main"),
        how="outer",
        indicator=True
    )

    comp5.to_csv(OUT_COMP_5M, index=False)
    compD.to_csv(OUT_COMP_D,   index=False)

    print(f"✅ Wrote {OUT_COMP_5M} ({len(comp5):,} rows)")
    print(f"✅ Wrote {OUT_COMP_D} ({len(compD):,} rows)")

    # quick mismatch summary
    def summarize(comp, label):
        cols = ["price_mean","volume_mean","qspread_mean","espread_mean","espread2_mean",
                "priceimpact_mean","priceimpact2_mean","price_std"]
        print(f"\n=== {label} mismatches (mean abs diff) ===")
        for c in cols:
            a, b = f"{c}_recalc", f"{c}_main"
            if a in comp.columns and b in comp.columns:
                both = comp[a].notna() & comp[b].notna()
                if both.any():
                    mad = (comp.loc[both, a] - comp.loc[both, b]).abs().mean()
                    print(f"{c:20s}: {mad:.6g}  (n={both.sum()})")
                else:
                    print(f"{c:20s}: n/a (no overlapping non-nulls)")
    summarize(comp5, "5-minute")
    summarize(compD,  "Daily")

if __name__ == "__main__":
    main()