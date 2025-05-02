test_that("Loading json definition to Capr works", {
  cd <- readInCirce(file.path("resources", "CohortTest.json"))
  checkmate::assertClass(cd, "Cohort")
})