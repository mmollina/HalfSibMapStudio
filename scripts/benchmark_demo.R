# Benchmark the real demo workflow (through the job layer) and one larger
# two-point analysis. Prints observed elapsed times; nothing is estimated.
#   Rscript scripts/benchmark_demo.R
local({
  if (file.exists("renv/activate.R") && !nzchar(Sys.getenv("RENV_PROJECT"))) source("renv/activate.R")
  res <- source("scripts/smoke_demo.R", local = new.env())$value
  cat("\nDemo dimensions: family MOM1, 60 offspring, 24 informative markers, 276 pairs\n")
  cat("Larger two-point analysis (simulated, in-process, 1 and 2 threads):\n")
  for (dims in list(c(1000, 200), c(3000, 300))) {
    sim <- HalfSibMap::sim_multi_pop(T_markers = dims[1], n_pops = 1, n_ind_per_pop = dims[2], r_const = 0.01,
                                     phase_mode = "random", maternal_geno_mode = "all_het", seed = 1)
    d <- tempfile(); dir.create(d)
    invisible(utils::capture.output({
      HalfSibMap::write_sim_pedigree(sim, file.path(d, "p.csv"))
      HalfSibMap::write_sim_genotypes(sim, file.path(d, "g.csv"))
    }))
    imp <- hsms_import(file.path(d, "p.csv"), file.path(d, "g.csv"), file.path(d, "n"))
    sel <- hsms_select_family(imp, "MOM1")
    for (th in 1:2) {
      t0 <- proc.time()[["elapsed"]]
      pw <- hsms_run_pairwise(sel, hsms_defaults()$pairwise, th)
      cat(sprintf("  %5d markers x %3d offspring, %d thread(s): %6.2f s (object %s)\n", dims[1], dims[2], th,
                  proc.time()[["elapsed"]] - t0, hsms_fmt_bytes(as.numeric(utils::object.size(pw$raw)))))
    }
  }
  cat(sprintf("CPU cores detected: %s\n", parallel::detectCores()))
})
