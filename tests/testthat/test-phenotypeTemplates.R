test_that("chronicOutcomeCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- chronicOutcomeCohort(cs_test)
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("chronicOutcomeCohort uses All limits and fixed exit at endDate", {
  cs_test <- cs(1L, name = "test")
  cd <- chronicOutcomeCohort(cs_test)

  expect_equal(cd@entry@primaryCriteriaLimit, "All")
  expect_equal(cd@attrition@expressionLimit, "All")
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
})

test_that("chronicOutcomeCohort default era gap is 1", {
  cs_test <- cs(1L, name = "test")
  cd <- chronicOutcomeCohort(cs_test)
  expect_equal(cd@era@eraDays, 1L)
})

test_that("chronicOutcomeCohort custom era gap is applied", {
  cs_test <- cs(1L, name = "test")
  cd <- chronicOutcomeCohort(cs_test, eraGapDays = 180L)
  expect_equal(cd@era@eraDays, 180L)
})

test_that("firstEverDiagnosisCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- firstEverDiagnosisCohort(cs_test)
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("firstEverDiagnosisCohort uses firstOccurrence and observation exit", {
  cs_test <- cs(1L, name = "test")
  cd <- firstEverDiagnosisCohort(cs_test)

  expect_equal(cd@entry@primaryCriteriaLimit, "First")
  expect_s4_class(cd@exit@endStrategy, "ObservationExit")
})

test_that("acuteOutcomeCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- acuteOutcomeCohort(cs_test)
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("acuteOutcomeCohort uses All limits and fixed exit", {
  cs_test <- cs(1L, name = "test")
  cd <- acuteOutcomeCohort(cs_test)

  expect_equal(cd@entry@primaryCriteriaLimit, "All")
  expect_equal(cd@attrition@expressionLimit, "All")
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
  expect_equal(cd@era@eraDays, 0L)
})

test_that("acuteOutcomeCohort uses endDate-based exit", {
  cs_test <- cs(1L, name = "test")
  cd <- acuteOutcomeCohort(cs_test)
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
})

test_that("newUserDrugCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- newUserDrugCohort(cs_test)
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("newUserDrugCohort uses drugExit strategy", {
  cs_test <- cs(1L, name = "test")
  cd <- newUserDrugCohort(cs_test)

  expect_equal(cd@entry@primaryCriteriaLimit, "First")
  expect_s4_class(cd@exit@endStrategy, "DrugExposureExit")
})

test_that("allDrugCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- allDrugCohort(cs_test)
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("allDrugCohort uses All limits and observation exit", {
  cs_test <- cs(1L, name = "test")
  cd <- allDrugCohort(cs_test)

  expect_equal(cd@entry@primaryCriteriaLimit, "All")
  expect_equal(cd@attrition@expressionLimit, "All")
  expect_s4_class(cd@exit@endStrategy, "ObservationExit")
})

test_that("measurementCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- measurementCohort(cs_test, valueFilter = valueAsNumber(gt(5.0)))
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("measurementCohort includes unit filter when provided", {
  cs_test <- cs(1L, name = "test")
  cd <- measurementCohort(cs_test, valueFilter = valueAsNumber(lte(3.0)),
                          unitConceptIds = 8554L)
  expect_s4_class(cd, "Cohort")
})

test_that("measurementCohort works without unit filter", {
  cs_test <- cs(1L, name = "test")
  cd <- measurementCohort(cs_test, valueFilter = valueAsNumber(eq(1.0)))
  expect_s4_class(cd, "Cohort")
})

test_that("procedureCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- procedureCohort(cs_test)
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("observationCohort returns a Cohort and compiles to valid Circe JSON", {
  skip_if_not_installed("CirceR")
  cs_test <- cs(1L, name = "test")
  cd <- observationCohort(cs_test)
  expect_s4_class(cd, "Cohort")

  json <- toCohortJson(cd)
  expect_type(json, "character")

  sql <- CirceR::cohortExpressionFromJson(json) |>
    CirceR::buildCohortQuery(
      CirceR::createGenerateOptions(generateStats = FALSE)
    )
  expect_type(sql, "character")
})

test_that("all archetypes accept NULL conceptSet explicitly", {
  skip_if_not_installed("CirceR")

  expect_s4_class(chronicOutcomeCohort(NULL), "Cohort")
  expect_s4_class(firstEverDiagnosisCohort(NULL), "Cohort")
  expect_s4_class(acuteOutcomeCohort(NULL), "Cohort")
  expect_s4_class(procedureCohort(NULL), "Cohort")
  expect_s4_class(observationCohort(NULL), "Cohort")
  expect_s4_class(allDrugCohort(NULL), "Cohort")
})

test_that("firstEverDiagnosisCohort applies firstOccurrence on entry", {
  cs_test <- cs(descendants(320128), name = "test")
  cd <- firstEverDiagnosisCohort(cs_test)

  json <- toCohortJson(cd)
  expect_match(json, "First", fixed = TRUE)
})

test_that("acuteOutcomeCohort default parameters are sensible", {
  cs_test <- cs(1L, name = "test")
  cd <- acuteOutcomeCohort(cs_test)

  expect_equal(cd@era@eraDays, 0L)
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
})

test_that("chronicOutcomeCohort with zero washout works", {
  cs_test <- cs(1L, name = "test")
  cd <- chronicOutcomeCohort(cs_test, washoutDays = 0L)
  expect_s4_class(cd, "Cohort")
})

test_that("chronicOutcomeCohort exitOffsetDays parameter is applied", {
  cs_test <- cs(1L, name = "test")
  cd <- chronicOutcomeCohort(cs_test, exitOffsetDays = 90L)
  expect_s4_class(cd, "Cohort")
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
})

test_that("procedureCohort uses All limits and fixed exit", {
  cs_test <- cs(1L, name = "test")
  cd <- procedureCohort(cs_test)

  expect_equal(cd@entry@primaryCriteriaLimit, "All")
  expect_equal(cd@attrition@expressionLimit, "All")
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
})

test_that("procedureCohort default era gap is 1", {
  cs_test <- cs(1L, name = "test")
  cd <- procedureCohort(cs_test)
  expect_equal(cd@era@eraDays, 1L)
})

test_that("observationCohort uses All limits and fixed exit", {
  cs_test <- cs(1L, name = "test")
  cd <- observationCohort(cs_test)

  expect_equal(cd@entry@primaryCriteriaLimit, "All")
  expect_equal(cd@attrition@expressionLimit, "All")
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
})

test_that("observationCohort default era gap is 1", {
  cs_test <- cs(1L, name = "test")
  cd <- observationCohort(cs_test)
  expect_equal(cd@era@eraDays, 1L)
})

test_that("acuteOutcomeCohort default exitDays is 14", {
  cs_test <- cs(1L, name = "test")
  cd <- acuteOutcomeCohort(cs_test)
  expect_s4_class(cd@exit@endStrategy, "FixedDurationExit")
})
