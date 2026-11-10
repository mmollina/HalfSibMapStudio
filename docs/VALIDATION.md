# Validation record

Everything below was run on 2026-11-06 in the development environment
described here. Outcomes are the real outputs; nothing is estimated unless
labelled so.

## Environment

| Item | Value |
| --- | --- |
| OS | Ubuntu 24.04.5 LTS (x86_64), container, 4 CPU cores, 15 GB RAM |
| Locale | `POSIX` (C) — Unicode handling was tested in this non-UTF-8 locale |
| R | 4.3.3 (Ubuntu `r-base`) |
| HalfSibMap | 0.2.0.9000, `RemoteSha b3e8b5fdd429c67a3feb7cd6c380dde746377f43` (installed from git) |
| Key packages | shiny 1.14.0, bslib 0.12.0, DT 0.34.0, callr (see `renv.lock`), shinytest2 0.5.1 |
| Browser | Playwright Chromium 1243 (`/opt/pw-browsers/chromium`) for chromote and manual inspection |

## Installation

| Command | Outcome |
| --- | --- |
| `Rscript scripts/bootstrap.R --init` (maintainer) | Installed 142 packages from CRAN sources (`cloud.r-project.org`) and HalfSibMap from git at the pinned commit; wrote `renv.lock`. |
| `git clone` into a folder whose path contains a space, then `Rscript scripts/bootstrap.R` | `renv::restore()` succeeded: 128 CRAN packages linked from the local renv cache, HalfSibMap rebuilt from git in 31 s; commit verified. |

Not verified: a restore that downloads every CRAN package on a machine with an
empty renv cache (the `--init` run did download them all from CRAN), Windows,
and macOS. The GitHub Actions workflow (`.github/workflows/checks.yaml`, Linux
and Windows) has not run yet.

## Automated checks

`Rscript scripts/test.R --browser` (with `CHROMOTE_CHROME=/opt/pw-browsers/chromium`):

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 385 ]
Tests: 12 files, 385 expectations, 0 failed, 0 skipped, 0 errors   (rerun 2026-11-09 after adding thread selection and the calibrated memory estimate)
```

| File | What it checks |
| --- | --- |
| `test-identifiers.R` | Leading zeros, numeric-looking, punctuation, space and Unicode IDs; `make.names` collisions; demonstrates that the bare upstream reader loses a numeric-looking mother; duplicate headers. |
| `test-validation.R` | Missing columns, duplicate markers, fractional dosages, `2.0`, `Inf`, out-of-range and text calls, missing/ungenotyped mother, unmatched samples, empty and all-missing data, multiple mothers, mother listed as offspring, duplicated offspring, known fathers requiring confirmation, wrong delimiter. |
| `test-analysis.R` | One family never pooled (compared with a pooled call); uninformative maternal markers excluded and reported; linkage-LOD 3 and 6 reach `tpt_filter()` and match a direct call; the `thresh.LOD.rf`-only trap; zero-pair filter refused; preview equals apply; invalid `k`; `ward.D2` switch recorded; singleton groups; MDS envelope removal and the package's skip warning; malformed supplied orders; reversal; direct/path/unresolved phase; source-aware blocked on unresolved phase; population blocks; posterior shapes, labels, bounds, sums; iteration-limited fit flagged; a failing block leaves other blocks intact (failure injected by mocking `hmm_map_blocks()` for one block). |
| `test-map-summary.R` | Hand-built inputs for gap rules: no-linkage and unresolved intervals have `NA` distance, positions restart per segment, length excludes gaps, singleton segments unestimated, map function changes distances only. |
| `test-state.R` | Every row of the invalidation table; signatures stable across save/reopen; a result finishing after settings changed is stored as historical, not current. |
| `test-jobs.R` | Real cancellation of a running 3,000-marker × 300-offspring two-point job (worker-start signal, process confirmed dead within seconds, nothing accepted, earlier results intact); worker error reported as failed and rerun succeeds; multi-group job keeps the good group when another fails; requests contain only paths/settings/signatures. |
| `test-persistence.R` | Save/reopen without recomputation; corrupt and missing artifacts set aside; different installed HalfSibMap commit makes results outdated; interrupted jobs; project lock; bundle round trip; inputs-only bundle; path traversal, absolute paths, `.R` files, checksum and schema mismatches rejected. |
| `test-settings.R` | `config/defaults.json` equals the `formals()` of the pinned functions; stage-specific lambdas; only the gametic paternal model; no fake `threads` for source-aware; setting domains; CSV export keeps full precision, `NA` and Unicode. |
| `test-parity.R` | Independent reference analysis with direct HalfSibMap calls (no app code) vs. artifacts produced through the app's workers: two-point `r`, `lod_r`, `lod_ph`, `logLik` (tol 1e-10), filtered set, groups, MDS order (up to reversal), phase vector and components, source-aware `r`, log-likelihood, alpha/beta, iterations, aligned `gammaM`, distances; population blocks vs `hmm_map_blocks()`/`get_block_map()`/`calc_haploprob()` on an unresolved order. |
| `test-replay-export.R` | Results ZIP contents and labels; outdated results excluded by default and labelled historical when included; non-converged fit exported with its warning; replay bundle unzipped into a path with spaces and run with `Rscript --vanilla` from another directory: `setup.R`, `replay.R` and `compare.R` all succeed and every output file matches. |
| `test-app-e2e.R` | shinytest2 in headless Chromium: Run demo workflow, server stays responsive while workers run, all stages complete, family shown, Group map and Maternal inheritance render, Save, project-bundle download, results package built in the background and downloaded (status `complete`). |

Tolerances: parity uses 1e-10 (same machine, same thread count, deterministic
code). The replay comparison uses `1e-6 + 1e-6·|x|` to allow platform and BLAS
differences; it matched exactly here.

## Manual browser inspection

The running app was inspected with Playwright screenshots at 1366×768,
1440×900 (2×), 1920×1080 and 900×900. Issues found and fixed: empty group
selectors after first fill, wrapped navigation labels at 1366 px, overlapping
offspring labels in the heatmap, an error rendering an empty validation table
after reopen, and two-column cards squeezing instead of stacking below 992 px.
No browser console errors were recorded. Screenshots of real results are in
`docs/screenshots/`.

## Benchmark (observed)

`Rscript scripts/benchmark_demo.R`, 1 worker thread unless stated:

| Stage (demo, MOM1: 60 offspring, 24 markers, 276 pairs) | Elapsed |
| --- | --- |
| Import (worker) | 3.7 s |
| Select family | 0.2 s |
| Two-point (worker) | 3.4 s |
| Filter (worker) | 3.5 s |
| Grouping (worker) | 3.3 s |
| MDS order (worker) | 3.2 s |
| Phase (worker) | 3.4 s |
| Source-aware map (worker) | 5.0 s |
| Whole demo through the job layer | 29 s |

About 3 s of each worker stage is R start-up and package loading in the new
process; the HalfSibMap calls themselves take well under a second on the demo.

| Two-point analysis (simulated, in-process) | 1 thread | 2 threads | Result object |
| --- | --- | --- | --- |
| 1,000 markers × 200 offspring | 1.47 s | 0.74 s | 47 MB |
| 3,000 markers × 300 offspring | 9.41 s | 5.25 s | 416 MB |

### Memory (measured, 2026-11-09)

Peak resident memory (VmHWM) of the app's real job workers, read from `/proc`
by the parent while each worker ran, compared with the app's estimate:

| Step | Markers × offspring | Measured peak | Estimate | Estimate / measured |
| --- | --- | --- | --- | --- |
| Two-point | 500 × 200 | 377 MB | 421 MB | 1.12 |
| Two-point | 1,000 × 200 | 437 MB | 481 MB | 1.10 |
| Two-point | 1,500 × 200 | 508 MB | 581 MB | 1.14 |
| Two-point | 2,000 × 300 | 637 MB | 722 MB | 1.13 |
| Two-point | 3,000 × 300 | 991 MB | 1,118 MB | 1.13 |
| Filter | 1,000 × 200 | 476 MB | 532 MB | 1.12 |
| Filter | 3,000 × 300 | 1,285 MB | 1,572 MB | 1.22 |
| Two-point, diagnostics on | 3,000 × 300 | 1,523 MB | 1,615 MB | 1.06 |
| Filter, diagnostics on | 3,000 × 300 | 1,869 MB | 2,045 MB | 1.09 |

(MB here = 2^20 bytes.) An R worker with HalfSibMap loaded starts at 375 MB.
Above that, memory grows with T² (about 75–90 bytes per T² for the two-point
step and 105–130 for the filter step); 1, 2 and 4 threads gave identical
peaks, and tripling the offspring count changed the peak by about 5%. The app
session needs about 83–103 bytes per T² to display the result, and the saved
result file is about 37 bytes per T². The previous formula (7 matrices × 8
bytes × 3 copies) gave 504 MB output / 1.5 GB peak for 3,000 markers and did
not separate the filter step.

## Known limitations

- Windows, macOS and the CI workflow are untested.
- Validation and import of very large files run in a worker but read the whole
  file into memory; no streaming parser.
- Exports of posterior tables are capped at 2 million rows per group (full
  posteriors remain in the native RDS).
- The hidden population pre-fit inside `hmm_map_source_aware()` suppresses its
  own warnings; its convergence cannot be reported.
- Session-end cleanup kills the owned worker (callr supervision); it was not
  exercised by an automated test of a closed browser tab.
