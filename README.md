# HalfSibMap Studio

A local Shiny workbench for maternal linkage mapping in open-pollinated
(half-sib) diploid families, built on the
[HalfSibMap](https://github.com/mmollina/HalfSibMap) R package.

Load a pedigree and genotype file, choose **one** maternal family, review data
quality, then run the analysis in explicit stages: two-point recombination and
filtering, linkage groups and marker order, maternal phase assembly, map
fitting (source-aware HMM for fully resolved phase, population model with
phase blocks otherwise), and per-offspring maternal inheritance probabilities.
Every result stays tied to the settings that produced it, heavy steps run in a
background R process you can cancel, and you can export tables, figures, the
native HalfSibMap objects, a portable project bundle, and an R script that
replays the whole analysis.

All statistics are computed by HalfSibMap (pinned to commit
`b3e8b5fdd429c67a3feb7cd6c380dde746377f43`); the app does not reimplement
any of its algorithms.

## Requirements

- R >= 4.2 (tested with R 4.3.3 on Ubuntu 24.04).
- A C++ toolchain, because HalfSibMap contains compiled code and is installed
  from source:
  - **Windows:** the [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
    version matching your R, plus [Git for Windows](https://git-scm.com/download/win).
  - **Linux (Debian/Ubuntu):** `build-essential`, `git`, and the development
    libraries `libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev
    libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev
    libjpeg-dev`.
  - **macOS:** Xcode command line tools and git.
- Internet access for the one-time installation only. After that, the app and
  the demo run offline (no CDN, database or cloud account).

## Install

From the repository folder (paths with spaces are fine):

```bash
Rscript scripts/bootstrap.R
```

This installs `renv` if needed and restores the exact package versions in
`renv.lock` (including HalfSibMap at the pinned commit, from git) into a
project library. It never upgrades packages on its own. On Linux packages are
built from source, which can take 20–30 minutes the first time; set
`HSMS_CRAN_REPO` to a Posit Package Manager Linux URL to use binaries.

## Run

```bash
Rscript scripts/run_app.R
```

The app listens on <http://127.0.0.1:3838> and opens your browser. Options:
`--port=4000`, `--host=0.0.0.0` (exposes it to your network; not hardened for
public hosting), `--no-browser`. From an R session in this folder:
`shiny::runApp(".")`. On Windows you can double-click `run_app.bat`.

Projects are saved on the computer running R, by default in
`tools::R_user_dir("HalfSibMapStudio", "data")/projects` (override with the
`HSMS_PROJECTS_DIR` environment variable).

## Try it

Click **Run demo workflow** on the Project page. It analyses family MOM1 of the
simulated example shipped with HalfSibMap (24 markers, 60 offspring) through
every stage in about 40 seconds, then open **Inheritance and results**.

## Check

```bash
Rscript scripts/test.R
```

```bash
Rscript scripts/test.R --browser
```

```bash
Rscript scripts/smoke_demo.R
```

```bash
Rscript scripts/benchmark_demo.R
```

`--fast` runs only the quick unit tests. `--browser` adds the shinytest2
end-to-end test (needs Chrome or Chromium; set `CHROMOTE_CHROME` if it is not
found).

## Documentation

- `docs/USER_GUIDE.md` – how to use each stage.
- `docs/DATA_FORMATS.md` – input files, validation rules, exports.
- `docs/UPSTREAM_API_CONTRACT.md` – the verified HalfSibMap API and its caveats.
- `docs/ARCHITECTURE.md` – layers, state model, jobs, persistence.
- `docs/VALIDATION.md` – what was tested, how, and the results.
- `docs/DECISIONS.md`, `docs/FUTURE_WORK.md`.

## Scope

First release: one family at a time, unknown fathers. Joint multi-dam mapping,
known-sire crosses, QTL analysis, selfing detection and manual map editing are
future work (`docs/FUTURE_WORK.md`). This is a local, single-user
application, not a hardened multi-user web service.

## License

MIT (see `LICENSE`). HalfSibMap is distributed under its own MIT license.
