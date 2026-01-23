from pathlib import Path
import pandas as pd
from geopy.distance import geodesic
import numpy as np
from datetime import date

def main():
    here = Path(__file__).resolve()
    project_root = here.parents[2]
    data_root = project_root / "data"
    in_path = data_root / "01_raw" / "equities.csv"
    out_path = data_root / "02_preprocessed" / "sample.csv"
    out_path.parent.mkdir(parents=True, exist_ok=True)

    # 1) Read CSV safely
    df = pd.read_csv(in_path, low_memory=False)

    # 2) Parse date column and compute returns
    df["date"] = pd.to_datetime(df["date"], errors="coerce")
    
    # log returns (only compute when both current and previous prices are > 0)
    p = df.groupby("RIC", group_keys=False)["price"].apply(
        lambda x: x.where(x > 0)  # mask non-positive
    )
    log_p = np.log(p)             # no log(<=0)
    df["daily_log_return"] = log_p.groupby(df["RIC"]).diff()
    df["daily_squared_log_return"] = df["daily_log_return"] ** 2

    # 3) Define date range
    start_date = pd.Timestamp("2022-01-27")
    end_date = pd.Timestamp("2022-03-23")

    # 4) Filter for date range
    df = df.sort_values(by=["RIC", "date"]).reset_index(drop=True)
    mask = (df["date"] >= start_date) & (df["date"] <= end_date)
    df_filtered = df.loc[mask]

    # 5) Exclude weekends (Saturday=5, Sunday=6)
    df_filtered = df_filtered[df_filtered["date"].dt.dayofweek < 5]

    # 6) Drop 'Unnamed: 0' and all columns starting with 'msci_'
    cols_to_drop = [c for c in df_filtered.columns if c == "Unnamed: 0" or c.startswith("msci_")]
    df_filtered = df_filtered.drop(columns=cols_to_drop, errors="ignore")
    df_filtered["date"] = df_filtered["date"].dt.date
    df_filtered = df_filtered.sort_values(by=["RIC", "date"]).reset_index(drop=True)

    # 7) Ensure lat/long are numeric (in case they were read as strings)
    for col in ["lat", "long"]:
        if col in df_filtered.columns:
            df_filtered[col] = pd.to_numeric(df_filtered[col], errors="coerce")

    # 8) Drop missing or small market cap
    before_rows = len(df_filtered)
    before_rics = df_filtered["RIC"].nunique()
    df_filtered = df_filtered.dropna(subset=["mktval"])
    after_dropna_rics = df_filtered["RIC"].nunique()
    print(f"After dropping missing mktval: {len(df_filtered):,} rows, {after_dropna_rics:,} unique RICs "
          f"({before_rics - after_dropna_rics:,} RICs removed)")

    before_smallcap_rics = df_filtered["RIC"].nunique()
    df_filtered = df_filtered[df_filtered["mktval"] >= 10_000_000]
    after_smallcap_rics = df_filtered["RIC"].nunique()
    print(f"After dropping mktval < 10M: {len(df_filtered):,} rows, {after_smallcap_rics:,} unique RICs "
          f"({before_smallcap_rics - after_smallcap_rics:,} RICs removed)")
    
    # 9) Compute distance (in km) from HQ (lat, long) to invasion point (49.872, 36.935)
    def compute_distance(row):
        try:
            firm_coords = (row["lat"], row["long"])
            invasion_coords = (49.872, 36.935)
            if pd.isna(firm_coords[0]) or pd.isna(firm_coords[1]):
                return np.nan
            return geodesic(firm_coords, invasion_coords).kilometers
        except Exception:
            return np.nan

    df_filtered["dist_invasion"] = df_filtered.apply(compute_distance, axis=1)

    # 10) Binary treatment: 1 if dist_ukr < 1000 km
    df_filtered["treat_ukr"] = (df_filtered["dist_ukr"] < 1000).astype(int)

    # 11) Binary dummy for invasion proximity
    df_filtered["treat_invasion"] = (df_filtered["dist_invasion"] < 1000).astype(int)

    # 12) Define first- and second-degree neighbor sets (excluding Russia)
    first_deg_iso2 = {"BY", "PL", "SK", "HU", "RO", "MD"}           # Belarus, Poland, Slovakia, Hungary, Romania, Moldova
    first_deg_iso3 = {"BLR", "POL", "SVK", "HUN", "ROU", "MDA"}

    second_deg_iso2 = {"DE", "CZ", "LT", "AT", "RS", "HR", "SI", "BG", "LV"}        # Germany, Czechia, Lithuania, Austria, Serbia, Croatia, Slovenia, Bulgaria, Latvia
    second_deg_iso3 = {"DEU", "CZE", "LTU", "AUT", "SRB", "HRV", "SVN", "BGR", "LVA"}

    # 13) First-order neighbor dummy
    df_filtered["nbr_1"] = (
        df_filtered["ctriso2"].isin(first_deg_iso2) |
        df_filtered["ctriso3"].isin(first_deg_iso3)
    ).astype(int)

    # 14) First- OR second-order neighbor dummy (1 if either, 0 otherwise)
    df_filtered["nbr_1_or_2"] = np.where(
        (
            df_filtered["ctriso2"].isin(first_deg_iso2.union(second_deg_iso2)) |
            df_filtered["ctriso3"].isin(first_deg_iso3.union(second_deg_iso3))
        ),
        1, 0
    )

    # 15) Continuous treatment variables based on dist_ukr
    # a) Inverse-distance
    df_filtered["treat_ukr_inv"] = 1 / (1 + df_filtered["dist_ukr"])
    # b) Normalized distance (rescaled 0–1)
    max_dist = df_filtered["dist_ukr"].max()
    min_dist = df_filtered["dist_ukr"].min()
    df_filtered["treat_ukr_norm"] = 1 - (df_filtered["dist_ukr"] - min_dist) / (max_dist - min_dist) if max_dist != min_dist else 1.0
    # c) Gaussian decay
    sigma = 1000
    df_filtered["treat_ukr_gauss"] = np.exp(-(df_filtered["dist_ukr"] ** 2) / (2 * sigma ** 2))

    # 16) Continuous treatment variables based on dist_invasion
    # a) Inverse-distance
    df_filtered["treat_invasion_inv"] = 1 / (1 + df_filtered["dist_invasion"])
    # b) Normalized distance (rescaled 0–1)
    max_dist2 = df_filtered["dist_invasion"].max()
    min_dist2 = df_filtered["dist_invasion"].min()
    df_filtered["treat_invasion_norm"] = 1 - (df_filtered["dist_invasion"] - min_dist2) / (max_dist2 - min_dist2) if max_dist2 != min_dist2 else 1.0
    # c) Gaussian decay
    sigma2 = 1000
    df_filtered["treat_invasion_gauss"] = np.exp(-(df_filtered["dist_invasion"] ** 2) / (2 * sigma2 ** 2))

    # 17) Rename RIC column to ric
    df_filtered = df_filtered.rename(columns={"RIC": "ric"})

    # 18) Save cleaned dataset
    df_filtered.to_csv(out_path, index=False)
    print(f"\nSaved {len(df_filtered)} rows and {df_filtered.shape[1]} columns to '{out_path}'")

if __name__ == "__main__":
    main()