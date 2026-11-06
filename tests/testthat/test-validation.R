# Input validation before the upstream reader is called.

mk_inputs <- function(ped_lines, geno_lines) {
  d <- tempfile("val_"); dir.create(d)
  list(ped = write_csv_lines(ped_lines, file.path(d, "p.csv")), geno = write_csv_lines(geno_lines, file.path(d, "g.csv")))
}
base_ped <- c("id,mother,father,generation,family_id", "M,,,1,F", "o1,M,,2,F", "o2,M,,2,F", "o3,M,,2,F")
base_geno <- c("marker_id,REF,ALT,M,o1,o2,o3", "m1,A,G,1,0,1,2", "m2,A,G,1,1,1,0", "m3,C,T,1,2,NA,1")
codes <- function(f) { v <- hsms_validate_inputs(f$ped, f$geno); list(ok = v$ok, codes = v$issues$code, v = v) }

test_that("a clean small file validates", {
  r <- codes(mk_inputs(base_ped, base_geno))
  expect_true(r$ok)
  expect_true(r$v$families$analyzable)
})

test_that("missing required columns block import", {
  r <- codes(mk_inputs(c("id,mother,generation,family_id", "M,,1,F", "o1,M,2,F"), base_geno))
  expect_false(r$ok); expect_true("ped_missing_columns" %in% r$codes)
  r <- codes(mk_inputs(base_ped, sub("REF,", "", base_geno)))
  expect_false(r$ok); expect_true("geno_missing_columns" %in% r$codes)
})

test_that("duplicate markers, invalid calls, decimal dosages and infinities are rejected, never truncated", {
  expect_true("geno_duplicate_marker" %in% codes(mk_inputs(base_ped, c(base_geno, "m1,A,G,1,0,0,0")))$codes)
  r <- codes(mk_inputs(base_ped, c(base_geno[1:3], "m3,C,T,1,1.5,0,1")))
  expect_false(r$ok); expect_true(any(grepl("fractional", r$codes)))
  r <- codes(mk_inputs(base_ped, c(base_geno[1:3], "m3,C,T,1,2.0,0,1")))
  expect_false(r$ok); expect_true(any(grepl("decimal_formatted", r$codes)))
  r <- codes(mk_inputs(base_ped, c(base_geno[1:3], "m3,C,T,1,Inf,0,1")))
  expect_false(r$ok); expect_true(any(grepl("infinite", r$codes)))
  r <- codes(mk_inputs(base_ped, c(base_geno[1:3], "m3,C,T,1,3,0,1")))
  expect_false(r$ok); expect_true(any(grepl("out_of_range", r$codes)))
  r <- codes(mk_inputs(base_ped, c(base_geno[1:3], "m3,C,T,1,AA,0,1")))
  expect_false(r$ok); expect_true(any(grepl("non_numeric", r$codes)))
})

test_that("a family without a genotyped mother is not analysable", {
  r <- codes(mk_inputs(base_ped, c("marker_id,REF,ALT,o1,o2,o3", "m1,A,G,0,1,2", "m2,A,G,1,1,0")))
  expect_false(r$v$families$analyzable)
  expect_match(r$v$families$notes, "mother has no genotype column")
})

test_that("unmatched genotype columns and pedigree rows without genotypes are reported, not silently used", {
  r <- codes(mk_inputs(c(base_ped, "o4,M,,2,F"), sub("o3$", "o3,extra", base_geno) |>
                         (\(g) c(g[1], paste0(g[-1], ",1")))()))
  expect_true("geno_unmatched_columns" %in% r$codes)
  expect_true("ped_without_genotypes" %in% r$codes)
  expect_false("extra" %in% r$v$sample_cols)
})

test_that("empty and all-missing inputs are blocking", {
  expect_false(codes(mk_inputs(base_ped, "marker_id,REF,ALT,M,o1,o2,o3"))$ok)
  r <- codes(mk_inputs(base_ped, c("marker_id,REF,ALT,M,o1,o2,o3", "m1,A,G,NA,NA,NA,NA")))
  expect_false(r$ok); expect_true("geno_all_missing" %in% r$codes)
})

test_that("multiple mothers in one family, mother listed as offspring, and duplicated offspring are caught", {
  r <- codes(mk_inputs(c(base_ped, "N,,,1,F", "o9,N,,2,F"), sub("o3$", "o3,N,o9", base_geno) |>
                         (\(g) c(g[1], paste0(g[-1], ",1,1")))()))
  expect_false(r$v$families$analyzable[r$v$families$family_id == "F"])
  expect_true("family_multiple_mothers" %in% r$codes)
  expect_true("ped_duplicate_id" %in% codes(mk_inputs(c(base_ped, "o1,M,,2,F"), base_geno))$codes)
  expect_true("ped_mother_is_offspring" %in% codes(mk_inputs(c(base_ped, "M2,M,,2,F", "o5,M2,,2,G"), base_geno))$codes)
})

test_that("known fathers require an explicit, recorded unknown-father decision", {
  f <- mk_inputs(c(base_ped[1:2], "o1,M,S1,2,F", base_ped[4:5]), base_geno)
  r <- codes(f)
  expect_true("known_father" %in% r$codes)
  imp <- hsms_import(f$ped, f$geno, tempfile(), accept_unknown_father = FALSE)
  expect_false(imp$ok); expect_true(isTRUE(imp$needs_father_confirmation))
  imp2 <- hsms_import(f$ped, f$geno, tempfile(), accept_unknown_father = TRUE)
  expect_true(imp2$ok); expect_match(imp2$known_father_policy, "confirmed")
})

test_that("non-comma delimiters are rejected with a clear message", {
  d <- tempfile(); dir.create(d)
  p <- write_csv_lines(gsub(",", "\t", base_ped), file.path(d, "p.csv"))
  v <- hsms_validate_inputs(p, mk_inputs(base_ped, base_geno)$geno)
  expect_true("unsupported_delimiter" %in% v$issues$code)
})
