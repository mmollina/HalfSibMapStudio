# HalfSibMap Studio replay bundle

This folder recomputes the saved analysis from the included raw inputs with
the real HalfSibMap API, the recorded settings, and the recorded seeds. It does
not depend on the original Shiny session, a project directory, or temporary
files, and it does not merely reload final results.

## Contents

- `settings.json` – family, exclusions, stage settings, per-group order/phase/model recipes, seed, threads, HalfSibMap commit.
- `inputs/` – the original pedigree and genotype CSV files (checksums are verified).
- `metadata/identifier_mapping.csv` – reversible original-to-internal ID mapping (verified during replay).
- `helpers/` – the pure-R adapter code used by the app (generated copies; no Shiny).
- `expected/` – the corresponding outputs saved by HalfSibMap Studio.
- `setup.R`, `replay.R`, `compare.R`.

## Commands

Run from any working directory (paths are resolved relative to this folder):

```bash
Rscript replay/setup.R --install
Rscript replay/replay.R
Rscript replay/compare.R
```

`setup.R` without `--install` only checks that the recorded HalfSibMap commit
and helper packages are installed. HalfSibMap is installed from git, so `git`
and a C++ toolchain (Rtools on Windows) are needed for `--install`.

## Expected output files

`outputs/` will contain `two_point_pairs.csv`, `filtered_markers.csv`,
`groups.csv`, `orders.csv`, `phase.csv`, `positions.csv`, `intervals.csv`,
`fits.csv` (for the stages that were completed) and `replay_results.rds` with
the recomputed native objects. `compare.R` prints PASS/FAIL per file and exits
with status 1 on any mismatch. Numeric tolerance is `1e-6` absolute plus `1e-6`
relative; a whole-group marker order is accepted up to reversal. Results may
differ in the last digits across platforms or BLAS libraries.
