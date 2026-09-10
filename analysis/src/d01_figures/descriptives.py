"""World map of firms by country for the 2×2 grid."""
try:
    from . import fig_descriptives_core as core, fig_common as fc
except ImportError:
    import fig_descriptives_core as core
    import fig_common as fc

for sample in fc.SAMPLES:
    for group in ("full", "matched"):  # entropy balancing keeps the full universe: same map as "full"
        try:
            core.run(sample, group)
        except (FileNotFoundError, ValueError) as e:
            print(f"⚠️ map [{sample}|{group}] skipped: {e}")
