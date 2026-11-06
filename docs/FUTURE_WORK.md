# Future work

Deliberately not in the first release (no non-functional controls exist for
them in the interface):

- Joint multi-dam mapping (`hmm_map_joint()`, multi-dam blocks) and
  heterogeneity tests across families.
- Known-sire / full-sib workflows.
- Phenotype association and QTL scans using the inheritance probabilities.
- Automatic pedigree reconstruction and automatic selfing detection.
- Interactive marker-order editing, ripple / `try_marker()` diagnostics, and
  framework-map construction (`diagnose_map_intervals()`,
  `collapse_framework_blocks()`).
- Marker-specific emission error (`epsilon_model = "marker_specific"`) with
  its own summaries.
- A crossover-calling module built on `xi` (posterior recombination).
- User accounts, shared multi-user hosting, Docker image, and a Windows
  executable. The current release is a local, single-user application.
- Streaming exports of very large posterior tables (currently capped at 2
  million rows per table; full posteriors are always available in the native
  RDS objects).
- An HTML analysis report.
