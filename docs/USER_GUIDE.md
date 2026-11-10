# User guide

HalfSibMap Studio runs the HalfSibMap analysis for one open-pollinated family
at a time: one known, genotyped mother, genotyped offspring, unknown fathers.
Each stage has an explicit Run/Apply button; nothing expensive runs because you
change a tab or a field. Heavy stages run in a background R process; open the
**Jobs** panel (top right) to follow progress or cancel.

## 1. Project

- **Run demo workflow** creates a project from the simulated example shipped
  with HalfSibMap and runs every stage on family MOM1 with recorded preset
  settings (package defaults; `k = 1` is a demo setting, not a general default).
  **Load demo data** stops after import and family selection.
- **New project**, **Open a saved project**, or **Import a project bundle**.
- The project manifest is saved automatically after each applied change and
  accepted result (header shows "Saved hh:mm:ss"; failures are shown in red).

## 2. Data

Upload the pedigree and genotype CSVs (see `DATA_FORMATS.md`) and click
**Validate files**. Blocking errors and warnings are listed separately and can
be downloaded. Then choose a family and click **Use this family**. Changing the
family resets exclusions and makes all analysis results outdated.

## 3. Quality review

Missingness by offspring and marker, maternal genotype classes, descriptive
call frequencies. Markers where the mother is homozygous or missing are not
informative for maternal mapping and are listed separately.

Exclusions are drafted first (missingness filter preview, or manual IDs with a
required reason) and take effect only with **Apply exclusions**. They are
reversible (remove rows from the draft and apply again). The app never removes
suspected selfs automatically; remove confirmed selfs here.

## 4. Two-point analysis

The run card shows retained counts and the number of pairs, a **Worker
threads** box, and a **memory estimate**.

- *Worker threads* (also in Settings; one project-wide value) sets how many CPU
  threads the background worker uses. It speeds up the two-point analysis and
  the population-model map fit; it changes neither results nor memory. The
  source-aware model is single-threaded. On the test machine, 3,000 markers ×
  300 offspring took 9.4 s with 1 thread and 5.3 s with 2.
- *Memory estimate* lists the peak memory of the two-point worker, of the
  filter step (which needs more, because it reloads the full result), what the
  app itself holds while showing the result, the saved file size, the total
  needed at most, and the memory available on this computer right now. The
  figures are calibrated from measured runs and are slightly pessimistic.
  Memory grows with the square of the number of markers.

If the analysis would need more than 80% of the available memory (or exceeds
the `memory_warning_gb` / `pairwise_marker_warning` limits in
`config/defaults.json`), you must confirm before it runs. Markers are never
subsampled. Results: RF and LOD distributions, a matrix view (aggregated for
large marker sets, labelled as such), and pair inspection.

**Filter**: one linkage-LOD cutoff (passed to both of `tpt_filter()`'s LOD
arguments), maximum r, and row-information quantiles (counts of surviving
informative pairs per marker, not missingness). **Preview filter** uses exactly
the settings Apply will use.

## 5. Groups and order

**Build groups** with an explicit `k`. The method actually used is shown (LOD
weighting changes `average` to `ward.D2`). Groups are inferred labels; you can
rename them for display only.

**Order selected groups** runs MDS per group. Groups with 1–2 markers get no MDS
estimate. Optional envelope removal reports removed markers. You can apply a
supplied order CSV (after a reconciliation report) or **Reverse order**; both
make that group's phase and map outdated, and the earlier order stays in the
history.

## 6. Phase and maps

**Assemble phase** per group. `min_phase_lod` (default 0, a compatibility
default, not a validated threshold) controls which pairwise edges count as phase
evidence. Intervals are labelled direct, path-derived, or unresolved;
unresolved intervals are never filled.

The route table shows, per group, which model is available:

- **Source-aware model**: fully resolved phase only.
- **Population model with phase-block handling**: fits each resolved block.

The model is your explicit choice; the app never switches models after an
error. Non-converged fits are flagged and stay flagged in exports.

## 7. Inheritance and results

Display settings (map function, no-linkage threshold) change only the summary,
never the fit. Views: map overview, group map (each segment drawn separately),
interval and position tables, maternal inheritance heatmap and profiles, and,
for source-aware fits, the paternal sharing-mode view.

Homolog 1 is a statistical label within a phase block. Sharing states describe
haplotype-sharing patterns; they do not identify fathers or prove identity by
descent.

## 8. Export

- **Results package**: tables, native HalfSibMap objects, figures (PNG/PDF),
  settings and provenance. Outdated results are excluded unless you include
  them (then labelled *historical*).
- **Replay bundle**: `replay.R` recomputes the analysis from the raw inputs;
  `compare.R` checks the result.
- **Project bundle**: portable copy for another machine.

## Status badges

Ready, Running, Cancelling, Complete, Warnings, Partial, Outdated, Failed,
Cancelled, Interrupted, Blocked. An outdated result stays visible for
inspection but is not exported as current.
