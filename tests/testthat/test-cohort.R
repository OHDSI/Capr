test_that("cohort entry works", {
  # ch <- cohort() # empty cohort is valid in circe but not Capr
  skip_if_not_installed("CirceR")
  # simplest possible cohort
  simpleCohort <- cohort(condition(cs(1)))
  expect_s4_class(simpleCohort, "Cohort")

  # shared concept set
  cs1 <- cs(descendants(exclude(436665),440383,442306,4175329))
  x <- cohort(entry(condition(cs1), drug(cs1)))
  expect_s4_class(x, "Cohort")
  expect_type(as.list(x), "list") # TODO Do we keep as.list and as.json?
  expect_type(toCirce(x), "list")
  expect_type(as.json(x), "character")
  sql <- CirceR::cohortExpressionFromJson(as.json(x)) %>%
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))
  expect_type(sql, "character")

  # different concept sets
  cs1 <- cs(descendants(exclude(436665),440383,442306,4175329))
  cs2 <- cs(descendants(exclude(436665),440383,442306))
  x <- cohort(entry(condition(cs1), drug(cs2)))
  expect_s4_class(x, "Cohort")
  expect_type(as.list(x), "list") # TODO Do we keep as.list and as.json?
  expect_type(toCirce(x), "list")
  expect_type(as.json(x), "character")
  sql <- CirceR::cohortExpressionFromJson(as.json(x)) %>%
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))
  expect_type(sql, "character")

})

test_that("getConceptSetDetails works on Eunomia", {
  skip_if_not_installed("Eunomia")
  gibleed <- cs(descendants(192671))
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  suppressMessages({
    con <- DatabaseConnector::connect(connectionDetails)
  })
  suppressWarnings({ # DatabaseConnector will throw a warning when passing already translated SQL code to dbGetQuery
    gibleed <- getConceptSetDetails(gibleed, con, vocabularyDatabaseSchema = "main")
  })
  expect_equal(gibleed@Expression[[1]]@Concept@concept_name, "Gastrointestinal hemorrhage")
  DatabaseConnector::disconnect(con)
})


test_that("full cohort works", {
  skip_if_not_installed("CirceR")

  cd <- cohort(
    entry = entry(
      condition(cs(descendants(201826L)), male()),
      observationWindow = continuousObservation(365, 0)
    ),
    attrition = attrition(
      'no t1d' = withAll(
        exactly(0,
          condition(cs(descendants(201254L))),
          duringInterval(eventStarts(-Inf, -1))
        )
      ),
      'abnormal hba1c' = withAll(
        atLeast(1,
          measurement(
            cs(descendants(4184637L)),
            valueAsNumber(lt(13)),
            unit(8713L)
          ),
          duringInterval(eventStarts(-Inf, -1))
        )
      )
    )
  )

  conceptSets <- listConceptSets(cd)

  cohortList <- toCirce(cd)
  expect_type(cohortList, "list")

  cohortJson <- jsonlite::toJSON(cohortList, pretty = TRUE, auto_unbox = TRUE) %>%
    as.character()

  expect_type(cohortJson, "character")
  expect_true(nchar(cohortJson) > 1)

  sql <- CirceR::cohortExpressionFromJson(cohortJson) %>%
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)
})


test_that("full cohort works without group", {
  skip_if_not_installed("CirceR")

  cd <- cohort(
    entry = entry(
      condition(cs(descendants(201826L)), male()),
      observationWindow = continuousObservation(365, 0)
    ),
    attrition = attrition(
      'no t1d' = exactly(0,
          condition(cs(descendants(201254L))),
          duringInterval(eventStarts(-Inf, -1))
      ),
      'abnormal hba1c' = atLeast(1,
          measurement(
            cs(descendants(4184637L)),
            valueAsNumber(lt(13)),
            unit(8713L)),
          duringInterval(eventStarts(-Inf, -1))
      )
    )
  )

  conceptSets <- listConceptSets(cd)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))

  cohortList <- toCirce(cd)
  expect_type(cohortList, "list")

  cohortJson <- jsonlite::toJSON(cohortList, pretty = T, auto_unbox = TRUE) %>%
    as.character()

  expect_type(cohortJson, "character")
  expect_true(nchar(cohortJson) > 1)

  sql <- CirceR::cohortExpressionFromJson(cohortJson) %>%
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)
})




test_that("Capr cohort generates on synpuf", {

  # need simple cohort for synpuf
  cd <- cohort(
    entry = entry(
      drug(cs(descendants(1118084), name = "celecoxib"), male()),
      observationWindow = continuousObservation(365, 0)
    )
  )

  cohortList <- toCirce(cd)
  expect_type(cohortList, "list")

  cohortJson <- jsonlite::toJSON(cohortList, pretty = T, auto_unbox = TRUE) %>%
    as.character()

  expect_type(cohortJson, "character")
  expect_true(nchar(cohortJson) > 1)

  sql <- CirceR::cohortExpressionFromJson(cohortJson) %>%
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)

  cohortsToCreate <- tibble::tibble(
    cohortId = 999,
    cohortName = "CaprTest",
    sql = sql
  )

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()

  cohortTableNames <- CohortGenerator::getCohortTableNames("cohort")

  invisible(capture_output(suppressMessages({
    CohortGenerator::createCohortTables(connectionDetails,
                                        cohortDatabaseSchema = "main",
                                        cohortTableNames = cohortTableNames)


    CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                       cdmDatabaseSchema = "main" ,
                                       cohortTableNames = cohortTableNames,
                                       cohortDefinitionSet = cohortsToCreate,
                                       incremental = FALSE)

  df <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                         cohortDatabaseSchema = "main",
                                         cohortTable = "cohort",
                                         cohortIds = c(999),
                                         cohortDefinitionSet = cohortsToCreate)
  })))

  expect_true(df$cohortEntries > 1)

})


