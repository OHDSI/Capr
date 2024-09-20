source("R/isStandardCS.R")

test_that("isStandardCS works correctly with standard concepts only", {
  # Load test data
  dat <- Capr::readConceptSet("tests/testthat/resources/ihd_cad_S.json")

  res <- isStandardCS(dat)

  expect_equal(nrow(res), 0)
})

test_that("isStandardCS works correctly with non-standard concepts only", {
  # Load test data
  dat <- Capr::readConceptSet("tests/testthat/resources/ihd_cad_NS.json")

  res <- isStandardCS(dat)

  expect_equal(nrow(res), 3)
  expect_contains(res$standard_concept, "Non-standard")
  expect_contains(res$standard_concept, "C")
})

test_that("isStandardCS works correctly with all kinds of concepts", {
  # Load test data
  dat <- Capr::readConceptSet("tests/testthat/resources/ihd_cad_mix.json")

  res <- isStandardCS(dat)

  expect_equal(nrow(res), 2)
  expect_contains(res$standard_concept, "Non-standard")
  expect_contains(res$standard_concept, "C")
})
