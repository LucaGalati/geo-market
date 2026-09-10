"""Overnight quoted-spread dynamics (Distant firms) for the 2×2 grid."""
try:
    from . import fig_overnight_core as core, fig_common as fc
except ImportError:
    import fig_overnight_core as core
    import fig_common as fc

fc.slog.reset("figures_overnight")
for sample in fc.SAMPLES:
    for group in fc.GROUPS:
        try:
            core.run(sample, group)
        except (FileNotFoundError, ValueError) as e:
            print(f"⚠️ overnight [{sample}|{group}] skipped: {e}")
