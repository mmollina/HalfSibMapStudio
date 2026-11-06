# Upstream API contract (HalfSibMap)

This document records the HalfSibMap API that HalfSibMap Studio relies on, as
verified against the installed package. Every statement below was checked by
reading the source at the pinned commit and by inspecting real objects with
`str()` / `formals()` (see `tests/testthat/test-settings.R`,
`test-analysis.R` and `test-parity.R`, which keep these facts under test).

## Dependency

| Item | Value |
| --- | --- |
| Package | `HalfSibMap` 0.2.0.9000 |
| Repository | <https://github.com/mmollina/HalfSibMap> |
| Pinned commit | `b3e8b5fdd429c67a3feb7cd6c380dde746377f43` (branch `main` head on 2026-11-06) |
| Install source | `git::https://github.com/mmollina/HalfSibMap.git@b3e8b5f…` (recorded in `renv.lock` as `RemoteType: xgit`, `RemoteSha` = pinned commit) |
| R requirement | R >= 4.2.0; compiled C++ (Rcpp, RcppParallel); `SystemRequirements: GNU make` |
| License | MIT + file LICENSE |

The app never tracks a moving branch. Workers refuse to run if the installed
`RemoteSha` differs from the project's recorded commit
(`hsms_worker_check_env()`), and reopening a project computed with another
commit is a *compatibility event* that marks every result outdated.

## Functions used and verified signatures

| Stage | Function (verified formals) | Returns |
| --- | --- | --- |
| Import | `read_HSMap_data(pedigree, genotypes, na_strings = c("NA",".",""))` | `HSMap.data`: `G_list` (named by family id; offspring × markers integer matrices), `M_list` (named integer vectors), `alleles`, `pedigree`, `stats` |
| Two-point | `pairwise_rf(x, snps = NULL, presence_threshold = 1, parent_label = NULL, threads = NULL, lambda = 20, q0 = 0.5, tol = 1e-6, maxit = 200, tiny = 1e-12, return_diagnostics = FALSE, return_input = FALSE, ...)` | `HSMap.tpt`: `fit` (`r`, `lod_r`, `lod_ph`, `logLik`, `no_linkage`, `mom_phase_list`, `lod_ph_list`, `q_list`, `optimizer`, `n_grid`, `markers`, `n_dams`), `markers`, `presence_threshold`, `parent_label`, `threads`, `time_sec` |
| Filter | `tpt_filter(x, order = NULL, thresh.LOD.ph = 5, thresh.LOD.rf = 5, thresh.rf = 0.15, probs = c(0.05, 1), diag.markers = NULL, diagnostic.plot = TRUE, breaks = 100)` | filtered `HSMap.tpt` with `filter_stats` (`n_in_matrix`, `n_kept`, `kept_markers`, `thresholds$LOD_used`, …) |
| Grouping | `group_markers(w, k = NULL, inter = TRUE, compare = FALSE, weight_by_lod = FALSE, chrom = NULL, method = "average", na_r = 0.5, palette = hs_pal)` | `hsmap_group`: `hc`, `k`, `groups` (named integer), `markers`, `method` (method actually used), `comp` |
| Order | `mds_order(x, tpt, lg = "all", plot_each = TRUE, itmax = 1e5, drop_outside_envelope = FALSE, envelope_k = 2, envelope_span = 0.25, min_keep = 3)` | `hsmap_mds`: named list of character orders, each with attribute `removed` |
| Phase | `phase_from_pairwise(tpt, order, dam = "all", min_phase_lod = 0, tie_tol = 1e-8, anchor_idx = 1, anchor_label = 0, max_passes = 50, tol = 1e-9, verbose = FALSE)` | single dam: `HSMap.phased` (`dam`, `order`, `clusters`, `component`, `phase_vec`, `resolved_interval`, `direct_lod`, `direct_supported`, `resolved_via`, `n_components`, `component_sizes`, `unresolved_intervals`, `objective`, `converged`, `fully_resolved`, …) |
| Source-aware map | `hmm_map_source_aware(x, phased, dam = 1, epsilon = 0.01, lambda = 2, alpha_start = 0.005, beta_start = 0.02, r_start = 0.05, tol = 1e-6, maxit = 2000, fit_mode = TRUE, q = NULL, K = NULL, nmin = 60, smooth = 0.5)` | `HSMap.sourceaware`: `r`, `pos`, `dist`, `alpha`, `beta`, `loglik`, `loglik_i`, `converged`, `iters`, `gammaM` [n × T], `xi` [n × (T−1)], `gammaH` [n × T], `q`, `K`, `rho`, `npairs`, `markers`, `phase_vec`, `dam`, `settings` |
| Population map | `hmm_map(x, phased, dam = 1, threads = NULL, epsilon = 0.01, …, paternal_mode = c("gametic","HWE","per_marker","two_locus"), r_start = 0.05, lambda = 20, maxit = 1000, …)` (called by `hmm_map_blocks`) | `HSMap.map`: `dam`, `order`, `phase_vec`, `fit` (`r`, `pi`, `q`, `logLik`, `penalized_obj`, `objective`, `converged`, `iters`, `conv_reason`, `objective_decreased`, traces, `epsilon`, `paternal_mode`) |
| Blocks | `hmm_map_blocks(x, phased, ..., min_block_markers = 2, gap_r = 0.499)` | `HSMap.map.blocks`: `blocks` (each `block`, `markers`, `contributing_dams`, `fit` or `NULL` + `note`), `block_id`, `unresolved_boundaries`, `n_blocks`, `order`, `interval_table`, `gap_r`, `dams` |
| Block summary | `get_block_map(x, map.function = c("haldane","kosambi","morgan"), gap_r = NULL)` | list: `positions` (`marker`, `phase_block`, `segment`, `pos`), `interval_table` (+ `dist_cM`), `segment_lengths`, `block_lengths`, `total_linked_length`, `n_segments`, `n_gaps`, `gap_intervals`, … |
| Inheritance | `calc_haploprob(dat, map, dam = NULL, epsilon = NULL)` | single map: `HSMap.gamma`: `gamma` [2 × T × n], `hap0_is_A`, `markers`, `samples`, `dam` |
| Map functions | `inv_haldane()`, `inv_kosambi()`, `inv_morgan()` (r → cM) | numeric |

## Behaviour that shapes the adapters

**Reader identifiers.** `read_HSMap_data()` uses `read.csv()` defaults:
`check.names = TRUE` renames sample columns (`"007"` → `"X007"`, `"A-1"` and
`"A.1"` → `"A.1"`), and type inference reads a pedigree id `"007"` as the
integer 7. A numeric-looking mother therefore silently loses her genotypes
(verified in `test-identifiers.R`). The app maps every sample, family and
marker to a syntactic internal ID (`S000001`, `F0001`, `M0000001`), writes
normalised CSVs and calls the reader on those, then verifies offspring order,
marker columns, every genotype value and the identified mother
(`hsms_verify_import()`). The reader also coerces genotype columns with
`storage.mode<- "integer"`, which would truncate a decimal dosage such as
`1.5` to `1`; the app rejects non-integer calls before the reader is called.
REF/ALT columns are also type-inferred (a column of `T`/`F` would become
logical), so the app keeps its own text copy of the annotation.

**Family membership.** For each `family_id`, the reader takes the unique
non-missing `mother` as the dam (falling back to the family id if there is not
exactly one) and the rows with `generation == 2` that have a genotype column as
offspring. The app validates exactly one genotyped mother per family before
calling it.

**Allele coding.** Calls are 0/1/2 allele counts (`0 = aa`, `1 = Aa`, `2 = AA`)
of the same allele across a marker; REF/ALT are not used by any analysis
function, and the app never recodes calls from them.

**Families are pooled if you let them.** `pairwise_rf()` on a multi-family
`HSMap.data` performs a pooled multi-dam analysis (grid optimiser, summed
objective). The app always passes an object containing exactly one family
(`hsms_select_family()`, `hsms_verify_selection()`) and checks that the
result has one `mom_phase_list` entry labelled with that family.

**Marker order in `pairwise_rf()`.** Markers are sorted with `sort()`; with
the app's zero-padded internal IDs this equals file order.

**Informative markers.** A marker enters `pairwise_rf()` if the mother is
non-missing and at least one offspring is observed; homozygous-mother markers
are accepted but carry no maternal phase information (all pairs `NA`). The app
sends only maternal heterozygous markers with at least one observed offspring
call (`snps =`), and reports the others as not informative.

**`tpt_filter()` compatibility argument.** `thresh.LOD.ph` is *not* a phase
filter: the mask uses `lod_r >= max(thresh.LOD.ph, thresh.LOD.rf)`. Lowering
only `thresh.LOD.rf` to 3 leaves an effective cutoff of 5 (verified). The app
exposes one linkage-LOD cutoff and passes it to both arguments, then checks
`filter_stats$thresholds$LOD_used` equals the request.
`diagnostic.plot = FALSE` is always passed.

**`tpt_filter()` with no surviving pairs.** If no pair passes the screen, all
row counts are 0, the quantile window is `[0, 0]`, and *every* marker is kept.
The app refuses to record that as a filtered result and reports why.

**Grouping.** `inter = FALSE` and an explicit `k` are always passed. With
`weight_by_lod = TRUE` and `method = "average"`, the package switches to
`ward.D2`; the returned `method` is recorded and shown.

**Ordering.** `plot_each = FALSE` is always passed. Groups with fewer than 3
markers are returned unchanged without MDS (recorded as "no MDS estimate").
With envelope removal, markers dropped are in `attr(order, "removed")`; if
removal would leave fewer than `min_keep` markers the package skips it with a
warning (captured). The app calls `mds_order()` with a character vector per
group (one group at a time) and checks every marker is accounted for.
SMACOF uses a deterministic Torgerson start; a seed is still set and recorded.

**Phase.** `dam = 1` on a single-family `HSMap.tpt` returns `HSMap.phased`
(`dam = "all"` would also return a single object for one family). The app
rejects `HSMap.phased.multi`. Intervals are `NA` in `phase_vec` when the two
markers lie in different components; `resolved_via` is `direct`, `path` or
`unresolved`; `direct_lod` is the raw adjacent edge LOD and is *not* the
support of a path-derived interval. A component can be non-contiguous along
the order, so the number of contiguous blocks is the number of `NA` intervals
plus one, not the number of components. For one family, the profiled phase
LOD equals the linkage LOD wherever a phase is called.

**Source-aware model.** `hmm_map_source_aware()` does not check that
`phase_vec` is fully resolved, and has no `threads` argument. The app blocks it
unless `fully_resolved` is `TRUE` and no `NA` is present. It runs an internal
population-mode pre-fit with `suppressWarnings()`; that pre-fit's convergence
is not returned, so the app does not claim to check it. `gammaM` is
`1 − P(M = 1)`, i.e. the posterior probability that homolog 1 was transmitted,
with rows in the order of `x$G_list[[dam]]` rows and columns in phased order
(the matrices have no dimnames; the app attaches them after checking
dimensions). Homolog 1 carries allele "1" at the first marker of the order.
`dist` sets `r >= 0.499` to `NA`, but `pos` simply skips them, so it does not
restart positions after a gap; the app builds its own segment-aware summary.
The default `lambda` is 2 (different from the other stages).

**Population model.** `paternal_mode`: `"gametic"` (identifiable, default);
`"HWE"` is a byte-identical alias; `"per_marker"` is deprecated and routed to
`"gametic"` with a warning; `"two_locus"` is disabled (error). Only
`"gametic"` is offered. `hmm_map()` refuses `NA` phase. `hmm_map_blocks()`
splits at `NA` intervals and calls `hmm_map()` per block; a failure in any
block aborts the whole call, so the app falls back to one `hmm_map_blocks()`
call per resolved block (using the same minimal phased object `dam`, `order`,
`phase_vec` the package builds internally) to keep valid blocks. With
`lambda > 0` the reported objective is the penalised objective
(`penalized_obj`); `logLik` is also returned.

**Inheritance (population).** `calc_haploprob()` accepts a single
`HSMap.map` (not the blocks container). `gamma[1, , ]` is the probability of
hap0, which carries allele "1" at the first marker of the block
(`hap0_is_A[1] == 1`), the same convention as source-aware `gammaM`
(correlation 0.999 on the demo, `test-analysis.R`); `gamma[1,,] + gamma[2,,] = 1`.

**Gap summaries.** `get_block_map()` classifies `linked`,
`no_linkage_boundary`, `unresolved_phase`, `between_blocks`,
`insufficient_information`, restarts positions per segment and sums finite
linked distances. The app's `hsms_summarize_map()` applies the same rules to
both models and is cross-checked against `get_block_map()` for every
population fit (`hsms_crosscheck_block_map()`, `test-parity.R`).

**Plot helpers.** `plot_phase()` maps an `NA` phase to a missing fill rather
than a labelled "unresolved" class, and `tpt_filter()`/`mds_order()` print
plots as side effects; the app draws its own figures and never calls
plotting side effects inside workers.

**Threads.** `pairwise_rf(threads=)` and `hmm_map(threads=)` call
`RcppParallel::setThreadOptions()`. The worker sets the thread count once per
job and records it; thread count is not part of result signatures
(per-pair estimation is independent and results are identical).

## Checks supporting each adapter

| Adapter | Test |
| --- | --- |
| identifier mapping / import verification | `test-identifiers.R`, `test-validation.R` |
| single-family selection | `test-analysis.R` ("never pooled") |
| pairwise and filter semantics | `test-analysis.R`, `test-parity.R` |
| grouping / ordering / supplied orders | `test-analysis.R`, `test-parity.R` |
| phase classification and routing | `test-analysis.R`, `test-parity.R` |
| source-aware and population fits, posteriors | `test-analysis.R`, `test-parity.R` |
| gap-aware summaries | `test-map-summary.R`, `test-parity.R` |
| defaults equal to `formals()` | `test-settings.R` |
