"""Daily exchange rates for the USD conversion of the value measures.

Writes two hand-checkable files in preprocessing/data/01_raw/handcoded/:
- fx_rates_ecb.csv    : ECB euro reference rates (public zip) turned into local currency
                        units per U.S. dollar, one row per currency and day of the window
- venue_currency.csv  : currency of each trading venue (RIC suffix) from the exchange
                        country in markets.csv, with the unit factor of the venue's prices
                        (1, or 100 for pence / cents / agorot) detected from the ratio of
                        the local price to the Datastream USD price in the daily panel
Run once (or whenever markets.csv changes) before merge.py."""
import io
import sys
import urllib.request
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
HAND = ROOT / "preprocessing" / "data" / "01_raw" / "handcoded"
DAILY = ROOT / "preprocessing" / "data" / "03_output" / "daily.parquet"
ECB_URL = "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-hist.zip"
START, END = "2022-01-20", "2022-03-31"
# venues whose country in markets.csv is wrong for the currency: NLB is the Canadian NEO exchange (CAD)
VENUE_OVERRIDE = {"NLB": "CAD"}
# ISO 3166-1 alpha-3 country of the exchange -> ISO 4217 currency (2022)
COUNTRY_CURRENCY = {
    "AUS": "AUD", "THA": "THB", "JPN": "JPY", "IND": "INR", "GBR": "GBP", "KOR": "KRW", "MYS": "MYR",
    "CAN": "CAD", "SWE": "SEK", "USA": "USD", "CHE": "CHF", "POL": "PLN", "TUR": "TRY", "MEX": "MXN",
    "ZAF": "ZAR", "PAK": "PKR", "PHL": "PHP", "DNK": "DKK", "NZL": "NZD", "SGP": "SGD", "CHL": "CLP",
    "ARG": "ARS", "ISR": "ILS", "HUN": "HUF", "CZE": "CZK", "LKA": "LKR", "SAU": "SAR", "ISL": "ISK",
    "IDN": "IDR", "BRA": "BRL", "ROU": "RON", "MKD": "MKD", "HKG": "HKD", "TWN": "TWD", "NOR": "NOK",
    "CHN": "CNY", "BGR": "BGN", "SRB": "RSD", "EGY": "EGP", "ARE": "AED", "QAT": "QAR", "KWT": "KWD",
    "BHR": "BHD", "OMN": "OMR", "JOR": "JOD", "MAR": "MAD", "NGA": "NGN", "KEN": "KES", "VNM": "VND",
    "PER": "PEN", "COL": "COP", "RUS": "RUB", "UKR": "UAH", "KAZ": "KZT", "MUS": "MUR", "HRV": "HRK",
    **{c: "EUR" for c in ["DEU", "FRA", "ITA", "ESP", "NLD", "BEL", "AUT", "FIN", "IRL", "PRT", "GRC", "LUX",
                          "SVN", "LVA", "LTU", "EST", "SVK", "MLT", "CYP"]},
}


def build_rates() -> pd.DataFrame:
    out = HAND / "fx_rates_ecb.csv"
    if out.exists():
        return pd.read_csv(out, parse_dates=["date"])
    local = HAND / "eurofxref-hist.zip"          # downloaded with curl if urllib cannot verify the certificate
    raw = local.read_bytes() if local.exists() else urllib.request.urlopen(ECB_URL, timeout=120).read()
    z = zipfile.ZipFile(io.BytesIO(raw))
    df = pd.read_csv(z.open([n for n in z.namelist() if n.endswith(".csv")][0]))
    df.columns = [c.strip() for c in df.columns]
    df = df.loc[:, ~df.columns.str.startswith("Unnamed")]
    df["Date"] = pd.to_datetime(df["Date"])
    df = df[(df["Date"] >= START) & (df["Date"] <= END)].sort_values("Date")
    long = df.melt(id_vars="Date", var_name="currency", value_name="per_eur")
    long["per_eur"] = pd.to_numeric(long["per_eur"], errors="coerce")
    usd = long[long["currency"] == "USD"].set_index("Date")["per_eur"]
    long["local_per_usd"] = long["per_eur"] / long["Date"].map(usd)
    eur = pd.DataFrame({"Date": usd.index, "currency": "EUR", "local_per_usd": 1.0 / usd.values})
    rates = (pd.concat([long[["Date", "currency", "local_per_usd"]], eur]).dropna()
               .rename(columns={"Date": "date"}).sort_values(["currency", "date"]))
    rates.to_csv(out, index=False)
    print(f"[fx] {out.name}: {rates['currency'].nunique()} currencies x {rates['date'].nunique()} days from {ECB_URL}")
    return rates


def build_venues(rates: pd.DataFrame) -> pd.DataFrame:
    markets = pd.read_csv(HAND / "markets.csv")
    markets["suffix"] = markets["suffix"].astype(str).str.strip().replace("nan", "")
    ven = markets[["suffix", "exchange_iso3"]].copy()
    ven["currency"] = ven["exchange_iso3"].map(COUNTRY_CURRENCY)
    # markets.csv writes the currency instead of the country for some venues (e.g. AX -> AUD)
    ven["currency"] = ven["currency"].fillna(ven["exchange_iso3"].where(ven["exchange_iso3"].isin(set(rates["currency"]))))
    for sfx, ccy in VENUE_OVERRIDE.items():
        ven.loc[ven["suffix"] == sfx, "currency"] = ccy
    unknown = ven[ven["currency"].isna()]["exchange_iso3"].unique()
    if len(unknown):
        print(f"⚠️ no currency for exchange countries {list(unknown)} -> IMPLIED")
    ven["currency"] = ven["currency"].fillna("IMPLIED")
    ven.loc[~ven["currency"].isin(set(rates["currency"])), "currency"] = "IMPLIED"

    # unit factor from the daily panel: local price / Datastream USD price vs the ECB rate
    ven["unit_factor"] = 1
    ven["check_ratio"] = np.nan
    if DAILY.exists():
        d = pd.read_parquet(DAILY, columns=["ric", "date", "price", "price_mean"])
        d["suffix"] = d["ric"].astype(str).str.extract(r"\.([A-Za-z0-9]+)$")[0].fillna("NY")
        d["date"] = pd.to_datetime(d["date"])
        d["implied"] = d["price_mean"] / d["price"]
        implied = d.groupby(["suffix", "date"])["implied"].median().reset_index()
        implied = implied.merge(ven[["suffix", "currency"]], on="suffix").merge(rates, on=["currency", "date"], how="left")
        implied["ratio"] = implied["implied"] / implied["local_per_usd"]
        r = implied.groupby("suffix")["ratio"].median()
        ven = ven.merge(r.rename("check_ratio_data"), left_on="suffix", right_index=True, how="left")
        ven["check_ratio"] = ven["check_ratio_data"]; ven = ven.drop(columns="check_ratio_data")
        ven.loc[(ven["check_ratio"] / 100 - 1).abs() < 0.15, "unit_factor"] = 100
        ven["check_ratio"] = ven["check_ratio"] / ven["unit_factor"]
        bad = ven[ven["currency"].ne("IMPLIED") & ven["check_ratio"].notna() & ((ven["check_ratio"] - 1).abs() > 0.15)]
        if not bad.empty:
            print("⚠️ venues whose implied rate disagrees with the ECB rate by more than 15% (check the country/currency):")
            print(bad.to_string(index=False))
    ven = ven[["suffix", "exchange_iso3", "currency", "unit_factor", "check_ratio"]]
    ven.to_csv(HAND / "venue_currency.csv", index=False)
    return ven


if __name__ == "__main__":
    rates = build_rates()
    ven = build_venues(rates)
    print(ven.sort_values(["currency", "suffix"]).to_string(index=False))
