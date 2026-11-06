# Development instructions (shared by all coding agents)

This is the single authoritative instruction file. `CLAUDE.md` and `CODEX.md`
only point here.

## What this repository is

HalfSibMap Studio: a local Shiny app that drives the HalfSibMap R package
(pinned commit `b3e8b5fdd429c67a3feb7cd6c380dde746377f43`). Read
`docs/ARCHITECTURE.md` and `docs/UPSTREAM_API_CONTRACT.md` before changing code.

## Rules

- Never reimplement HalfSibMap algorithms (two-point, phasing, grouping,
  ordering, HMM/EM). Call the public API through the adapters in `R/analysis_*.R`.
- Verify upstream behaviour with real objects (`str()`, `formals()`) before
  relying on it, and record it in `docs/UPSTREAM_API_CONTRACT.md`.
- Keep the pure-R core (files listed in `R/core_files.R`) free of Shiny. Workers,
  scripts, tests and the replay bundle depend on that.
- Project state is a plain serialisable list. No reactive values, process
  handles or environments in it. New computational inputs must enter
  `hsms_expected_signature()`; display-only state must not.
- Heavy work goes through the job layer (`hsms_build_request()` /
  `hsms_job_launch()`); never block the Shiny process with `wait()`.
- Never fabricate or simulate results when a real computation fails; record
  the failure. Never switch models automatically.
- Keep user data, project stores, logs and generated outputs out of git.
- Changing the HalfSibMap commit requires updating `renv.lock`,
  `hsms_pinned_halfsibmap_commit()` in `R/utils.R`, `DESCRIPTION`,
  `scripts/bootstrap.R`, the contract document, and rerunning the parity tests.

## Commands

- Install/restore: `Rscript scripts/bootstrap.R` (maintainers: `--init`).
- Run: `Rscript scripts/run_app.R`.
- Tests: `Rscript scripts/test.R` (`--fast`, `--browser`).
- Smoke test: `Rscript scripts/smoke_demo.R`.

Update `docs/VALIDATION.md` with the commands run and their real outcomes.
