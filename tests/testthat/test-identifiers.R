# Identifier preservation through the upstream reader.

tricky_files <- function(dir = tempfile("ids_")) {
  dir.create(dir)
  ids <- c(mom = "007", o1 = "0012", o2 = "12", o3 = "A-1", o4 = "A.1", o5 = "A 1", o6 = "Ébène_ü", o7 = "x(1)")
  ped <- c("id,mother,father,generation,family_id",
           sprintf('"%s",,,1,"0001"', ids[["mom"]]),
           sprintf('"%s","%s",,2,"0001"', ids[-1], ids[["mom"]]))
  set.seed(3)
  mk <- c("001", "1e5", "snp-1", "snp.1", "Marker ä")
  calls <- matrix(sample(0:2, length(mk) * 7, TRUE), length(mk))
  geno <- c(paste(c("marker_id", "REF", "ALT", sprintf('"%s"', ids)), collapse = ","),
            vapply(seq_along(mk), function(i) paste(c(sprintf('"%s"', mk[i]), "A", "G", "1", calls[i, ]), collapse = ","), ""))
  list(ped = write_csv_lines(ped, file.path(dir, "p.csv")), geno = write_csv_lines(geno, file.path(dir, "g.csv")),
       ids = ids, markers = mk, calls = calls)
}

test_that("leading zeros, numeric, punctuation, spaces and Unicode IDs survive import exactly", {
  f <- tricky_files()
  imp <- hsms_import(f$ped, f$geno, tempfile())
  expect_true(imp$ok)
  expect_identical(imp$families$family_id, "0001")
  expect_identical(imp$families$mother_id, "007")
  sel <- hsms_select_family(imp, "0001")
  off <- hsms_to_original(imp$mapping, "sample", sel$offspring)
  expect_setequal(off, unname(f$ids[-1]))
  expect_identical(hsms_to_original(imp$mapping, "marker", colnames(imp$data$G_list[[1]])), f$markers)
  # values survive too (offspring order follows the pedigree)
  G <- imp$data$G_list[[1]]
  expect_equal(unname(G[hsms_to_internal(imp$mapping, "sample", "A-1"), ]), f$calls[, 3])
})

test_that("names that collide under make.names() stay distinct", {
  f <- tricky_files()
  expect_identical(make.names("A-1"), make.names("A.1"))  # the upstream hazard
  imp <- hsms_import(f$ped, f$geno, tempfile())
  int <- hsms_to_internal(imp$mapping, "sample", c("A-1", "A.1", "A 1"))
  expect_false(anyNA(int)); expect_false(anyDuplicated(int) > 0)
  expect_true(hsms_check_mapping(imp$mapping))
})

test_that("the upstream reader alone would lose a numeric-looking mother", {
  f <- tricky_files()
  raw <- HalfSibMap::read_HSMap_data(f$ped, f$geno)
  # read.csv renames "007" to "X007" in the genotype header and reads the pedigree id as 7
  expect_false("007" %in% colnames(raw$G_list[[1]]))
  expect_true(all(is.na(raw$M_list[[1]])))
})

test_that("duplicate genotype headers are a blocking error", {
  d <- tempfile(); dir.create(d)
  ped <- write_csv_lines(c("id,mother,father,generation,family_id", "M,,,1,F", "o1,M,,2,F", "o2,M,,2,F"), file.path(d, "p.csv"))
  geno <- write_csv_lines(c("marker_id,REF,ALT,M,o1,o1", "m1,A,G,1,0,1", "m2,A,G,1,1,1"), file.path(d, "g.csv"))
  v <- hsms_validate_inputs(ped, geno)
  expect_false(v$ok)
  expect_true("duplicate_header" %in% v$issues$code)
})
