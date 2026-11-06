# Decisions

| # | Decision | Reason |
| --- | --- | --- |
| 1 | Pin HalfSibMap `b3e8b5f…`, installed from git (`remotes::install_git`), recorded in `renv.lock` as `xgit` | Reproducible baseline; the GitHub API/tarball hosts were not reachable in the build environment while `git clone` was. Workers refuse a different commit. |
| 2 | Map all IDs to internal syntactic IDs before `read_HSMap_data()` | The reader renames and retypes identifiers (verified). |
| 3 | Read and write CSV as raw UTF-8 bytes (custom writer) | Unicode IDs were corrupted under a C/POSIX locale with `read.csv(fileEncoding=)` / `write.csv`. |
| 4 | Staleness from recursive expected signatures, not manual invalidation | Every descendant becomes outdated automatically; display-only state is never part of a signature. |
| 5 | Signatures are SHA-256 of canonical JSON | Must survive a JSON save/reopen round trip exactly. |
| 6 | `callr::r_bg()` + 500 ms non-blocking polling; one heavy job per session | Process isolation and real cancellation of compiled code (default requested). |
| 7 | Multi-group jobs run items sequentially in one worker; finished items are accepted even if a later item fails or the job is cancelled | Bounded resources; a failed group must not erase others. |
| 8 | Import (validation + reader) runs in the worker; selection, exclusions, reversal and supplied orders run in-process | Selection is a cheap subset; import can be slow on large files. |
| 9 | Results ZIP and replay bundle are built by the worker; project bundles and single tables in-process | Large exports must not block the UI; bundles are file copies. |
| 10 | One linkage-LOD cutoff passed to both `thresh.LOD.ph` and `thresh.LOD.rf` | `tpt_filter()` uses their maximum as one linkage cutoff. |
| 11 | Refuse a filter in which no pair passes | `tpt_filter()` would then keep every marker. |
| 12 | Population model always via `hmm_map_blocks()` (also for resolved phase), with a per-block fallback when the whole-order call fails | One code path; block failures stay isolated. |
| 13 | Gap-aware summary is app-owned, applied to both models, cross-checked against `get_block_map()` | Source-aware `pos` does not restart after gaps. |
| 14 | Kosambi/Morgan offered for source-aware fits as a labelled display conversion of the fitted r | Conversion uses the package's own map functions; the model is not refitted. |
| 15 | Only `paternal_mode = "gametic"` is offered | `HWE` is an alias, `per_marker` deprecated, `two_locus` disabled. |
| 16 | Marker-specific emission error (`epsilon_model = "marker_specific"`) deferred | Not needed for the core workflow; summaries not yet implemented. |
| 17 | Thread count recorded but excluded from signatures | Per-pair estimation is independent; results are identical across thread counts. |
| 18 | Autosave of the project manifest after each accepted change | Avoids lost work; Save is also available explicitly. |
| 19 | A selected-family change resets exclusions | Exclusions refer to that family's samples and markers. |
| 20 | Figures drawn with ggplot2 in the app, not package plotting helpers | Package helpers print side effects and do not label unresolved intervals or segments. |
| 21 | Default CRAN repository for bootstrap is `cloud.r-project.org` (source on Linux, binaries on Windows/macOS); Posit Package Manager can be set via `HSMS_CRAN_REPO` | PPM's binary redirect host was blocked in the build environment. |
| 22 | R 4.3.3 is the tested version | Ubuntu 24.04 system R; HalfSibMap needs >= 4.2. |
