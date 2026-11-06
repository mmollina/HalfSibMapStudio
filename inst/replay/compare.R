# Compare replay outputs with the outputs saved by HalfSibMap Studio.
#   Rscript /path/to/replay/compare.R [--out=/path/to/outputs]
# Documented symmetry: a whole-group order may be compared up to reversal.
# Numeric tolerance: |a - b| <= 1e-6 + 1e-6 * |b|.
local({
  args <- commandArgs(trailingOnly = TRUE)
  # Rscript encodes spaces in the script path as "~+~".
  root <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)))
  out_arg <- sub("^--out=", "", grep("^--out=", args, value = TRUE))
  out_dir <- if (length(out_arg)) out_arg[1] else file.path(root, "outputs")
  exp_dir <- file.path(root, "expected")
  rd <- function(p) utils::read.csv(p, colClasses = "character", na.strings = "NA", check.names = FALSE, encoding = "UTF-8")
  close_enough <- function(a, b) {
    na <- is.na(a) | is.na(b)
    if (!identical(is.na(a), is.na(b))) return(FALSE)
    x <- suppressWarnings(as.numeric(a[!na])); y <- suppressWarnings(as.numeric(b[!na]))
    if (anyNA(x) || anyNA(y)) return(identical(a, b))
    all(abs(x - y) <= 1e-6 + 1e-6 * abs(y))
  }
  ok_all <- TRUE
  for (f in list.files(exp_dir, pattern = "\\.csv$")) {
    e <- rd(file.path(exp_dir, f)); p <- file.path(out_dir, f)
    if (!file.exists(p)) { cat("FAIL ", f, ": missing in outputs\n"); ok_all <- FALSE; next }
    o <- rd(p)
    if (f == "orders.csv") {
      same <- all(vapply(unique(e$group), function(g) {
        a <- e$marker_id[e$group == g]; b <- o$marker_id[o$group == g]
        identical(a, b) || identical(a, rev(b))
      }, logical(1)))
      cat(if (same) "PASS " else "FAIL ", f, "\n"); ok_all <- ok_all && same; next
    }
    good <- identical(dim(e), dim(o)) && identical(names(e), names(o)) &&
      all(vapply(names(e), function(cn) close_enough(e[[cn]], o[[cn]]), logical(1)))
    cat(if (good) "PASS " else "FAIL ", f, "\n")
    ok_all <- ok_all && good
  }
  if (!ok_all) quit(status = 1, save = "no")
  cat("All replay outputs match the saved analysis.\n")
})
