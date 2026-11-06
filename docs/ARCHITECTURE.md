# Architecture

HalfSibMap Studio is a plain Shiny-for-R project (`app.R` + `R/` + `www/`).
Shiny sources every file in `R/` automatically. There is no frontend build
step and no second framework.

## Four layers

```
UI modules (Shiny)        R/app_ui.R, R/app_server.R, R/mod_*.R, R/ui_*.R, www/
        |  read/write via a session-scoped context (ctx)
Project / state layer     R/project_state.R, R/project_actions.R, R/project_io.R, R/artifact_store.R
        |  immutable requests, payload files
Job layer                 R/job_manager.R (parent), R/worker_entry.R (child process)
        |  calls
Analysis adapters         R/identifier_mapping.R, R/input_validation.R, R/family_selection.R,
                          R/qc_summary.R, R/analysis_*.R, R/map_summary.R, R/analysis_inheritance.R
        |  calls the public API of
HalfSibMap (pinned commit)
```

`R/core_files.R` lists the pure-R files (everything except the Shiny modules).
Workers, `scripts/smoke_demo.R`, the tests and the replay bundle source these
files without Shiny, so every adapter can be run from a plain R script.

## Project state

A project is a plain, serialisable R list (`hsms_new_project()`): applied
settings, inputs and checksums, selected family, exclusions, per-group
specifications (`order_spec`, `phase_spec`, `map_spec`), an artifact registry,
current-artifact pointers, job records and last outcomes. It never contains
reactive values, process handles or environments.

**Signatures and staleness.** Each stage has an *expected signature* computed
recursively from applied settings and inputs only
(`hsms_expected_signature()`):

```
raw inputs -> import -> selection (family + exclusions) -> tpt_raw -> tpt_filtered
  -> grouping -> order[g] -> phase[g] -> map[g] -> (summaries, posteriors, exports)
```

An artifact is current iff its recorded signature equals the expected signature
now. Therefore an upstream change makes every descendant outdated without any
manual invalidation code, and display-only state (group labels, display map
function and gap threshold, table sorting, plot selections, thread count) never
enters a signature. Signatures are SHA-256 hashes of canonical JSON, so they
survive a save/reopen round trip exactly.

Statuses: `not_run`, `ready`, `blocked`, `running`, `cancelling`, `complete`,
`complete_with_warnings`, `partial`, `outdated`, `failed`, `cancelled`,
`interrupted`, `historical`.

**Draft vs applied.** Form inputs are drafts. A Run/Apply action commits the
draft into the project (applied settings or a per-group spec) and launches the
job against that signature. Badges show "Unapplied changes" when a form differs
from the applied values; "Discard edits" restores them.

## Artifacts and persistence

```
<project>/project.json            versioned manifest (schema_version 1)
<project>/inputs/                 raw uploaded files (copied, checksummed)
<project>/metadata/               settings.json, exclusions.csv, identifier_mapping.csv
<project>/artifacts/<id>.rds|json payload (raw HalfSibMap object + app summaries) and provenance
<project>/history/events.jsonl    applied changes and job outcomes; history/logs/ worker logs
<project>/exports/                built result packages and replay bundles
<project>/jobs/<job-id>/          temporary worker directories (removed after acceptance)
```

All writes are atomic (temporary file + rename). The manifest is written after
artifacts, and reopen validates checksums, drops pointers to missing or corrupt
payloads, treats a different installed HalfSibMap commit as a compatibility
event (all results outdated, kept for inspection), and marks running jobs as
interrupted. Project folders default to
`tools::R_user_dir("HalfSibMapStudio", "data")/projects` (override with
`HSMS_PROJECTS_DIR` or `config/defaults.json`). A `.lock` file prevents two
sessions writing one project.

Portable bundles (`.hsmap.zip`) carry a `bundle.json` with SHA-256 checksums.
Import rejects absolute paths, `..`, unknown top-level folders and file types
other than JSON/CSV/RDS, checks sizes, schema version and checksums, extracts
into a new folder in the project store, and never executes code. RDS files are
for trusted local workflows; this is not a security sandbox.

## Job layer

1. `hsms_build_request()` writes an immutable `request.rds` (input payload
   paths, applied settings, seed, threads, expected signature per item, pinned
   commit).
2. `hsms_job_launch()` starts `callr::r_bg()` (supervised) with the project
   library paths. The child sources the core files, verifies the HalfSibMap
   commit, sets the seed and RcppParallel threads, and runs items one at a time.
3. The child writes `status.json` at item boundaries, a payload RDS per item,
   and `result.json` after each item; stdout/stderr go to `worker.log`.
4. The Shiny server polls every 500 ms with `invalidateLater()`; it never
   calls a blocking wait. Logs are read with a bounded tail.
5. On completion each item is accepted only if its launch signature still equals
   the expected signature; otherwise it is stored as `historical`. Failed items
   are recorded as failed outcomes; other items are kept (partial completion).
6. Cancel kills the process tree, waits for exit on subsequent polls (shown as
   Cancelling), and records cancelled outcomes. Items completed before the
   cancel are accepted.
7. On session end the owned process is killed and the project is unlocked.

One heavy job runs per session. Progress percentages are shown only for
multi-item jobs (completed/total items); a single HalfSibMap call is shown as
indeterminate.

Exports (results ZIP, replay bundle) also run as jobs; project bundles and
individual tables are built in the request.

## Modules

| Stage | File |
| --- | --- |
| Shell, navigation, job lifecycle, demo chain | `app_ui.R`, `app_server.R` |
| 1 Project | `mod_project.R` |
| 2 Data | `mod_data.R` |
| 3 Quality review | `mod_qc.R` |
| 4 Two-point analysis | `mod_pairwise.R` |
| 5 Groups and order | `mod_group_order.R` |
| 6 Phase and maps | `mod_phase_map.R` |
| 7 Inheritance and results | `mod_results.R` |
| 8 Export | `mod_exports.R` |
| Jobs drawer | `mod_jobs.R` |
| Figures (app and export) | `export_figures.R` |
