test_that("cohort entry works", {
  # ch <- cohort() # empty cohort is valid in circe but not Capr

  # simplest possible cohort
  simpleCohort <- cohort(condition(cs(1)))
  expect_s4_class(simpleCohort, "Cohort")

  # shared concept set
  cs1 <- cs(descendants(exclude(436665),440383,442306,4175329))
  x <- cohort(entry(condition(cs1), drug(cs1)))
  expect_s4_class(x, "Cohort")
  expect_type(as.list(x), "list")
  as.json(x)

  # different concept sets
  cs1 <- cs(descendants(exclude(436665),440383,442306,4175329))
  cs2 <- cs(descendants(exclude(436665),440383,442306))
  x <- cohort(entry(condition(cs1), drug(cs2)))
  expect_s4_class(x, "Cohort")
  expect_type(as.list(x), "list")
  j <- as.json(x)
  class(j)

  e <- CirceR::cohortExpressionFromJson(as.json(x))

  gibleed <- cs(descendants(192671))
  gibleedCohort <- cohort(condition(gibleed))

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  con <- DatabaseConnector::connect(connectionDetails)
  gibleed <- getConceptSetDetails(gibleed, con, vocabularyDatabaseSchema = "main")
  DatabaseConnector::disconnect(con)



})
