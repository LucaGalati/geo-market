from pathlib import Path
import polars as pl

# -------- Paths --------
DATA_ROOT = Path(__file__).resolve().parents[2] / "data"
RAW_DIR   = DATA_ROOT / "01_raw" / "trth"
OUT_DIR   = DATA_ROOT / "02_preprocessed" / "trth"
OUT_DIR.mkdir(parents=True, exist_ok=True)

# -------- Winsorization config --------
WINSOR_COLS = [
    "price","volume","q_spread","e_spread","e_spread2",
    "price_impact","price_impact2","realized_spread","realized_spread2",
    "w_e_spread2","w_price_impact2","w_realized_spread2",
    "log_return","squared_log_return"
]
LOW_Q  = 0.001
HIGH_Q = 0.9999


def scan_trth_csv_gz(gz: Path) -> pl.LazyFrame:
    """
    Scan CSV.GZ lazily, normalize headers, cast to expected dtypes.
    """
    return (
        pl.scan_csv(
            gz,
            has_header=True,
            ignore_errors=True,
            null_values=["", "NA", "NaN"],
            try_parse_dates=False,
        )
        # normalize headers: lowercase + strip leading '#'
        .select([pl.all().name.map(lambda n: n.lower().lstrip("#"))])
        .rename({
            "ric":"ric",
            "date-time":"datetime",
            "gmt offset":"gmt",
            "type":"type",
            "price":"price",
            "volume":"volume",
            "bid price":"bid",
            "bid size":"bid_size",
            "ask price":"ask",
            "ask size":"ask_size",
            "exch time":"time",
        })
        .select([
            pl.col("ric").cast(pl.Utf8),
            pl.col("datetime").str.to_datetime(strict=False, time_unit="us", time_zone="UTC"),
            pl.col("gmt").cast(pl.Utf8),
            pl.col("type").cast(pl.Utf8),
            pl.col("price").cast(pl.Float64, strict=False),
            pl.col("volume").cast(pl.Int64, strict=False),
            pl.col("bid").cast(pl.Float64, strict=False),
            pl.col("bid_size").cast(pl.Int64, strict=False),
            pl.col("ask").cast(pl.Float64, strict=False),
            pl.col("ask_size").cast(pl.Int64, strict=False),
        ])
    )


def build_pipeline(gz: Path) -> pl.LazyFrame:
    df = scan_trth_csv_gz(gz)

    # --- flags early (used in many branches) ---
    df = df.with_columns([
        (pl.col("type").str.to_uppercase() == "QUOTE").alias("is_quote"),
        (pl.col("type").str.to_uppercase() == "TRADE").alias("is_trade"),
    ])

    # --- quote-side features FIRST (so reordering never breaks 'mid') ---
    valid_quote = (
        pl.col("is_quote")
        & pl.col("ask").is_not_null() & pl.col("bid").is_not_null()
        & pl.col("ask_size").is_not_null() & pl.col("bid_size").is_not_null()
        & (pl.col("ask_size") > 0) & (pl.col("bid_size") > 0)
    )

    df = df.with_columns([
        # simple midpoint (exists before any filter/spread calc)
        pl.when(pl.col("ask").is_not_null() & pl.col("bid").is_not_null())
          .then((pl.col("ask") + pl.col("bid")) / 2)
          .otherwise(None)
          .alias("mid"),

        # quote midpoint (only on quote rows)
        pl.when(pl.col("is_quote")).then(pl.col("mid")).otherwise(None).alias("mid_quote"),

        # weighted midpoint (no dependency on a temp column)
        pl.when(valid_quote)
          .then(
              (pl.col("ask") * pl.col("ask_size").cast(pl.Float64)
             +  pl.col("bid") * pl.col("bid_size").cast(pl.Float64))
              / (pl.col("ask_size") + pl.col("bid_size")).cast(pl.Float64)
          )
          .otherwise(None)
          .alias("w_mid_quote"),

        # market depth and value (from first principles to avoid ordering hazards)
        pl.when(valid_quote)
          .then((pl.col("ask_size") + pl.col("bid_size")).cast(pl.Float64))
          .otherwise(None)
          .alias("market_depth"),

        pl.when(valid_quote)
          .then(((pl.col("ask_size") + pl.col("bid_size")).cast(pl.Float64)) * ((pl.col("ask") + pl.col("bid")) / 2))
          .otherwise(None)
          .alias("market_depth_value"),
    ])

    # --- NOW do data cleaning filters (mid already exists) ---
    df = df.filter(
        ~(pl.col("is_quote") & pl.col("ask").is_not_null() & pl.col("bid").is_not_null() & (pl.col("ask") < pl.col("bid")))
    ).filter(
        ~(pl.col("is_trade") & ((pl.col("price") < 0) | (pl.col("volume") < 0)))
    )

    # --- GMT parsing (never write nulls inside replace) ---
    df = df.with_columns([
        pl.col("gmt").fill_null("").str.replace_all(r"^\+", "").alias("gmt_clean"),
    ])
    df = df.with_columns([
        pl.when(pl.col("gmt_clean").str.strip_chars().eq("") | pl.col("gmt").is_null())
          .then(None)
          .otherwise(pl.col("gmt_clean"))
          .cast(pl.Float64, strict=False)
          .alias("gmt_hours"),
    ]).drop("gmt_clean")

    # --- local datetime / date_local (split to satisfy lazy planner) ---
    df = df.with_columns([
        (pl.col("datetime") + (pl.col("gmt_hours") * pl.duration(hours=1))).alias("local_dt"),
    ])
    df = df.with_columns([
        pl.col("local_dt").dt.date().alias("date_local"),
    ])

    # --- sort & ffill reference mids per (ric, date_local) ---
    df = df.sort(["ric","date_local","datetime"])
    df = df.with_columns([
        pl.col("mid_quote").fill_null(strategy="forward").over(["ric","date_local"]).alias("mid_ref"),
        pl.col("w_mid_quote").fill_null(strategy="forward").over(["ric","date_local"]).alias("w_mid_ref"),
    ])

    # --- direction vs previous mid_ref (inline) ---
    prev_mid = pl.col("mid_ref").shift(1).over(["ric","date_local"])
    df = df.with_columns([
        pl.when(pl.col("price").is_null() | prev_mid.is_null())
          .then(None)
          .when(pl.col("price") > prev_mid).then(1)
          .when(pl.col("price") < prev_mid).then(-1)
          .otherwise(0)
          .alias("direction"),
    ])

    # --- quoted & effective spreads (mid exists already) ---
    df = df.with_columns([
        pl.when(pl.col("is_quote") & pl.col("mid").is_not_null())
          .then((pl.col("ask") - pl.col("bid")) / pl.col("mid"))
          .otherwise(None)
          .alias("q_spread"),
        pl.when(pl.col("is_trade") & pl.col("mid_ref").is_not_null() & pl.col("price").is_not_null())
          .then((pl.col("price") - pl.col("mid_ref")).abs() / pl.col("mid_ref"))
          .otherwise(None)
          .alias("e_spread"),
        pl.when(pl.col("is_trade") & pl.col("e_spread").is_not_null() & pl.col("direction").is_not_null())
          .then(2 * pl.col("e_spread") * pl.col("direction"))
          .otherwise(None)
          .alias("e_spread2"),
    ])

    # --- t+5m asof refs ---
    ref = (
        df.select(["ric","date_local","datetime","mid_ref","w_mid_ref"])
          .sort(["ric","date_local","datetime"])
    )
    df = df.with_columns([
        (pl.col("datetime") + pl.duration(minutes=5)).alias("t_plus_5")
    ])
    df = df.join_asof(
        ref,
        left_on="t_plus_5", right_on="datetime",
        by=["ric","date_local"],
        strategy="backward",
        suffix="_future"   # -> mid_ref_future, w_mid_ref_future
    )

    # --- price impact (your latest: unsigned, and signed 2x) + zero-tick exclusion ---
    df = df.with_columns([
        pl.when(pl.col("is_trade") & pl.col("mid_ref").is_not_null() & pl.col("mid_ref_future").is_not_null())
          .then((pl.col("mid_ref_future") - pl.col("mid_ref")) / pl.col("mid_ref"))
          .otherwise(None)
          .alias("price_impact"),
        pl.when(pl.col("price_impact").is_not_null() & pl.col("direction").is_not_null())
          .then(2 * pl.col("direction") * pl.col("price_impact"))
          .otherwise(None)
          .alias("price_impact2"),
    ])
    df = df.with_columns([
        pl.when(pl.col("direction") == 0).then(None).otherwise(pl.col("direction")).alias("direction"),
        pl.when(pl.col("direction").is_null()).then(None).otherwise(pl.col("price_impact2")).alias("price_impact2"),
    ])

    # --- realized spreads (unweighted & weighted) ---
    df = df.with_columns([
        pl.when(pl.col("e_spread").is_not_null() & pl.col("price_impact").is_not_null())
          .then(pl.col("e_spread") - pl.col("price_impact"))
          .otherwise(None)
          .alias("realized_spread"),
        pl.when(pl.col("e_spread2").is_not_null() & pl.col("price_impact2").is_not_null())
          .then(pl.col("e_spread2") - pl.col("price_impact2"))
          .otherwise(None)
          .alias("realized_spread2"),
        pl.when(pl.col("is_trade") & pl.col("w_mid_ref").is_not_null() & pl.col("price").is_not_null() & pl.col("direction").is_not_null())
          .then(2 * pl.col("direction") * ((pl.col("price") - pl.col("w_mid_ref")) / pl.col("w_mid_ref")))
          .otherwise(None)
          .alias("w_e_spread2"),
        pl.when(pl.col("is_trade") & pl.col("w_mid_ref").is_not_null() & pl.col("w_mid_ref_future").is_not_null() & pl.col("direction").is_not_null())
          .then(2 * pl.col("direction") * ((pl.col("w_mid_ref_future") - pl.col("w_mid_ref")) / pl.col("w_mid_ref")))
          .otherwise(None)
          .alias("w_price_impact2"),
        (pl.col("w_e_spread2") - pl.col("w_price_impact2")).alias("w_realized_spread2"),
    ])

    # --- log returns (per ric) ---
    df = df.with_columns([
        pl.col("price").log().diff().over("ric").alias("log_return"),
        (pl.col("price").log().diff().over("ric") ** 2).alias("squared_log_return"),
    ])

    # --- winsorize per RIC BEFORE aggregation ---
    qtbl = df.group_by("ric").agg(
        [pl.col(c).quantile(LOW_Q).alias(f"{c}_lo") for c in WINSOR_COLS] +
        [pl.col(c).quantile(HIGH_Q).alias(f"{c}_hi") for c in WINSOR_COLS]
    )
    df = df.join(qtbl, on="ric", how="left")

    for c in WINSOR_COLS:
        df = df.with_columns([
            pl.when(pl.col(f"{c}_lo").is_not_null() & pl.col(f"{c}_hi").is_not_null() & pl.col(c).is_not_null())
              .then(pl.max_horizontal([
                      pl.col(f"{c}_lo"),
                      pl.min_horizontal([pl.col(c), pl.col(f"{c}_hi")])
                  ])
              )
              .otherwise(pl.col(c))
              .alias(c)
        ])

    # --- 5-minute aggregation ---
    df = df.with_columns([pl.col("datetime").dt.truncate("5m").alias("dt_5m")])

    agg = (
        df.group_by(["ric","dt_5m","date_local"])
          .agg([
              pl.mean("price").alias("price_mean"),
              pl.mean("volume").alias("volume_mean"),
              pl.sum("volume").alias("volume_sum"),
              pl.mean("q_spread").alias("qspread_mean"),
              pl.mean("e_spread").alias("espread_mean"),
              pl.mean("e_spread2").alias("espread2_mean"),
              pl.mean("price_impact").alias("priceimpact_mean"),
              pl.mean("price_impact2").alias("priceimpact2_mean"),
              pl.mean("realized_spread").alias("realized_spread_mean"),
              pl.mean("realized_spread2").alias("realized_spread2_mean"),
              pl.mean("w_e_spread2").alias("w_espread2_mean"),
              pl.mean("w_price_impact2").alias("w_priceimpact2_mean"),
              pl.mean("w_realized_spread2").alias("w_realized_spread2_mean"),
              pl.mean("log_return").alias("logret_mean"),
              pl.mean("squared_log_return").alias("sqr_logret_mean"),
              pl.sum("is_trade").alias("trades_count"),
              pl.sum("is_quote").alias("quotes_count"),
              pl.mean("market_depth").alias("depth_mean"),
              pl.sum("market_depth").alias("depth_sum"),
              pl.mean("market_depth_value").alias("depth_value_mean"),
              pl.sum("market_depth_value").alias("depth_value_sum"),
              pl.std("price").alias("price_std"),
              pl.first("gmt").alias("gmt"),
          ])
          .with_columns([
              (pl.col("volume_sum") / pl.col("trades_count")).alias("avg_trade_size"),
              (pl.col("depth_sum") / pl.col("quotes_count")).alias("avg_quote_size"),
              pl.col("dt_5m").alias("datetime"),
          ])
          .drop("dt_5m")
    )

    return agg


def main():
    files = sorted(RAW_DIR.glob("*.gz"))
    if not files:
        raise FileNotFoundError(f"No .gz files found in {RAW_DIR}")

    for gz in files:
        base = gz.stem.replace(".csv", "")
        print(f"▶ Processing {gz.name} (Polars streaming)")
        agg = build_pipeline(gz).collect(engine="streaming")
        out_path = OUT_DIR / f"{base}.csv"
        agg.write_csv(out_path)
        print(f"   ✓ Wrote {out_path}  ({agg.shape[0]} rows)")


if __name__ == "__main__":
    main()
