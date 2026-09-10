"""Intraday open/close-hour z-score figures and t-tests for the 2×2 grid."""
try:
    from . import fig_intraday_core as core, fig_common as fc
except ImportError:
    import fig_intraday_core as core
    import fig_common as fc

fc.slog.reset("figures_intraday")
for sample in fc.SAMPLES:
    for group in fc.GROUPS:
        try:
            core.run(sample, group)
        except (FileNotFoundError, ValueError) as e:
            print(f"⚠️ intraday [{sample}|{group}] skipped: {e}")
