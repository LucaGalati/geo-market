"""Daily event-study figures for the 2×2 grid: {main, balanced} × {full, matched}."""
try:
    from . import fig_daily_core as core, fig_common as fc
except ImportError:
    import fig_daily_core as core
    import fig_common as fc

fc.slog.reset("figures_daily")
for sample in fc.SAMPLES:
    for group in fc.GROUPS:
        try:
            core.run(sample, group)
        except (FileNotFoundError, ValueError) as e:
            print(f"⚠️ daily [{sample}|{group}] skipped: {e}")
