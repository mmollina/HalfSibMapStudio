# Fixtures. Real package data and real package simulations are used for
# integration tests; small hand-built objects are used only for app-owned
# summary logic and are clearly labelled.

demo_paths <- function() {
  list(ped = system.file("extdata", "example_pedigree.csv", package = "HalfSibMap"),
       geno = system.file("extdata", "example_genotypes.csv", package = "HalfSibMap"))
}

skip_if_fast <- function() {
  if (identical(Sys.getenv("HSMS_FAST_TESTS"), "true")) testthat::skip("slow test skipped (HSMS_FAST_TESTS=true)")
}

write_csv_lines <- function(lines, path) { writeLines(enc2utf8(lines), path, useBytes = TRUE); path }

# A simulated single-family dataset written with HalfSibMap's own writers.
sim_family_files <- function(T_markers, n, r_vec = NULL, r_const = 0.05, seed = 1, dir = tempfile("sim_"),
                             miss_rate = 0) {
  dir.create(dir, showWarnings = FALSE)
  sim <- HalfSibMap::sim_multi_pop(T_markers = T_markers, n_pops = 1, n_ind_per_pop = n, r_vec = r_vec,
                                   r_const = r_const, phase_mode = "random", maternal_geno_mode = "all_het",
                                   seed = seed, miss_rate = miss_rate)
  invisible(utils::capture.output({
    HalfSibMap::write_sim_pedigree(sim, file.path(dir, "pedigree.csv"))
    HalfSibMap::write_sim_genotypes(sim, file.path(dir, "genotypes.csv"))
  }))
  list(ped = file.path(dir, "pedigree.csv"), geno = file.path(dir, "genotypes.csv"), sim = sim)
}

# Import + select in-process (no workers) for adapter-level tests.
imported <- function(paths = demo_paths(), family = "MOM1", accept_unknown_father = FALSE) {
  imp <- hsms_import(paths$ped, paths$geno, tempfile("norm_"), accept_unknown_father)
  stopifnot(isTRUE(imp$ok))
  list(imp = imp, sel = hsms_select_family(imp, family))
}

# A complete project built through the real job layer, memoised per session.
.hsms_fixture_cache <- new.env()
demo_project <- function() {
  if (!is.null(.hsms_fixture_cache$demo)) return(.hsms_fixture_cache$demo)
  d <- hsms_defaults(hsms_test_app_dir)
  base <- tempfile("demo_proj_"); dir.create(base)
  cr <- hsms_create_project("Test demo", base, d)
  p <- cr$proj; dir <- cr$dir
  dp <- demo_paths()
  p <- hsms_action_set_inputs(p, dir, dp$ped, dp$geno, "example_pedigree.csv", "example_genotypes.csv")
  p <- hsms_run_stage_sync(p, dir, "import")
  p <- hsms_action_select_family(p, dir, "MOM1")
  p <- hsms_run_stage_sync(p, dir, "tpt_raw")
  p <- hsms_run_stage_sync(p, dir, "tpt_filtered")
  p <- hsms_run_stage_sync(p, dir, "grouping")
  p <- hsms_set_order_spec_mds(p, "1", d$order)
  p <- hsms_run_stage_sync(p, dir, "order", hsms_group_items(p, dir, "order", "1"), list(order_settings = d$order))
  p <- hsms_set_phase_spec(p, "1", d$phase)
  p <- hsms_run_stage_sync(p, dir, "phase", hsms_group_items(p, dir, "phase", "1"))
  p <- hsms_set_map_spec(p, "1", "source_aware", d$source_aware)
  p <- hsms_run_stage_sync(p, dir, "map", hsms_group_items(p, dir, "map", "1"))
  hsms_save_project(p, dir)
  .hsms_fixture_cache$demo <- list(proj = p, dir = dir, defaults = d)
  .hsms_fixture_cache$demo
}

# Copy a project directory so a test can mutate it freely.
copy_project <- function(dir) {
  dst <- tempfile("proj_copy_"); dir.create(dst)
  file.copy(list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)
  unlink(file.path(dst, ".lock"))
  dst
}
