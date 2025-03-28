test_that("cohort entry works", {
  # ch <- cohort() # empty cohort is valid in circe but not Capr
  skip_if_not_installed("CirceR")
  # simplest possible cohort
  simpleCohort <- cohort(conditionOccurrence(cs(1, name = "test")))
  expect_s4_class(simpleCohort, "Cohort")

  # shared concept set
  cs1 <- cs(descendants(exclude(436665),440383,442306,4175329), name = "test")
  x <- cohort(entry(conditionOccurrence(cs1), drugExposure(cs1)))
  expect_s4_class(x, "Cohort")
  expect_type(as.list(x), "list") # TODO Do we keep as.list and as.json?
  expect_type(toCirce(x), "list")
  expect_type(compile(x), "character")


  sql <- CirceR::cohortExpressionFromJson(compile(x)) |>
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))
  expect_type(sql, "character")

  # different concept sets
  cs1 <- cs(descendants(exclude(436665),440383,442306,4175329), name = "test")
  cs2 <- cs(descendants(exclude(436665),440383,442306), name = "test")
  x <- cohort(entry(conditionOccurrence(cs1), drugExposure(cs2)))
  expect_s4_class(x, "Cohort")
  expect_type(as.list(x), "list")
  expect_type(toCirce(x), "list")
  expect_type(compile(x), "character")

  sql <- CirceR::cohortExpressionFromJson(compile(x)) |>
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))
  expect_type(sql, "character")

})

# test_that("getConceptSetDetails works on Eunomia", {
#   skip_if_not_installed("Eunomia")
#   gibleed <- cs(descendants(192671), name = "test")
#   connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#   suppressMessages({
#     con <- DatabaseConnector::connect(connectionDetails)
#   })
#   suppressWarnings({ # DatabaseConnector will throw a warning when passing already translated SQL code to dbGetQuery
#     gibleed <- getConceptSetDetails(gibleed, con, vocabularyDatabaseSchema = "main")
#   })
#   expect_equal(gibleed@Expression[[1]]@Concept@concept_name, "Gastrointestinal hemorrhage")
#   DatabaseConnector::disconnect(con)
# })


test_that("full cohort works", {
  skip_if_not_installed("CirceR")

  cd <- cohort(
    entry = entry(
      conditionOccurrence(cs(descendants(201826L), name = "test"), male()),
      observationWindow = continuousObservation(365, 0)
    ),
    attrition = attrition(
      'no t1d' = withAll(
        exactly(0,
          conditionOccurrence(cs(descendants(201254L), name = "test")),
          duringInterval(eventStarts(-Inf, -1))
        )
      ),
      'abnormal hba1c' = withAll(
        atLeast(1,
          measurement(
            cs(descendants(4184637L), name = "test"),
            valueAsNumber(lt(13)),
            measurementUnit(8713L)
          ),
          duringInterval(eventStarts(-Inf, -1))
        )
      )
    )
  )

  conceptSets <- listConceptSets(cd)

  cohortList <- toCirce(cd)
  expect_type(cohortList, "list")

  cohortJson <- jsonlite::toJSON(cohortList, pretty = TRUE, auto_unbox = TRUE) |>
    as.character()

  expect_type(cohortJson, "character")
  expect_true(nchar(cohortJson) > 1)

  sql <- CirceR::cohortExpressionFromJson(cohortJson) |>
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)
})


test_that("full cohort works without group", {
  skip_if_not_installed("CirceR")

  cd <- cohort(
    entry = entry(
      conditionOccurrence(cs(descendants(201826L), name = "test"), male()),
      observationWindow = continuousObservation(365, 0)
    ),
    attrition = attrition(
      'no t1d' = exactly(0,
          conditionOccurrence(cs(descendants(201254L), name = "test")),
          duringInterval(eventStarts(-Inf, -1))
      ),
      'abnormal hba1c' = atLeast(1,
          measurement(
            cs(descendants(4184637L), name = "test"),
            valueAsNumber(lt(13)),
            measurementUnit(8713L)),
          duringInterval(eventStarts(-Inf, -1))
      )
    )
  )

  conceptSets <- listConceptSets(cd)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))

  cohortList <- toCirce(cd)
  expect_type(cohortList, "list")

  cohortJson <- jsonlite::toJSON(cohortList, pretty = T, auto_unbox = TRUE) |>
    as.character()

  expect_type(cohortJson, "character")
  expect_true(nchar(cohortJson) > 1)

  sql <- CirceR::cohortExpressionFromJson(cohortJson) |>
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)
})




test_that("full cohort works with domains without concepts", {
  skip_if_not_installed("CirceR")

  cd <- cohort(
    entry = entry(
      conditionOccurrence(cs(descendants(201826L), name = "test"), male())
    ),
    attrition = attrition(
      'no t1d' = exactly(0,
                         conditionOccurrence(cs(descendants(201254L), name = "test")),
                         duringInterval(eventStarts(-Inf, -1))
      ),
      '365d OP' = withAll(
        exactly(1,
                observationPeriod(),
                duringInterval(eventStarts(-Inf, -365), eventEnds(-1, Inf))
        )
      ),
      'abnormal hba1c' = atLeast(1,
                                 measurement(
                                   cs(descendants(4184637L), name = "test"),
                                   valueAsNumber(lt(13)),
                                   measurementUnit(8713L)),
                                 duringInterval(eventStarts(-Inf, -1))
      )
    ),
    exit = exit(
      endStrategy = observationExit(),
      censor = censoringEvents(
        death()
      )
    )
  )

  conceptSets <- listConceptSets(cd)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))

  cohortList <- toCirce(cd)
  expect_type(cohortList, "list")

  cohortJson <- jsonlite::toJSON(cohortList, pretty = T, auto_unbox = TRUE) |>
    as.character()

  expect_type(cohortJson, "character")
  expect_true(nchar(cohortJson) > 1)

  sql <- CirceR::cohortExpressionFromJson(cohortJson) |>
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)
})


# test_that("Capr cohort generates on synpuf", {
#   skip_if_not_installed("CirceR")
#   skip_if_not_installed("Eunomia")
#   # need simple cohort for synpuf
#   cd <- cohort(
#     entry = entry(
#       # observationWindow = continuousObservation(1, 0) # TODO this line causes an error.
#       drugExposure(cs(descendants(1118084), name = "celecoxib"), male()),
#       observationWindow = continuousObservation(365, 0)
#     )
#   )
#
#   cohortList <- toCirce(cd)
#   expect_type(cohortList, "list")
#
#   cohortJson <- jsonlite::toJSON(cohortList, pretty = T, auto_unbox = TRUE) |>
#     as.character()
#
#   expect_type(cohortJson, "character")
#   expect_true(nchar(cohortJson) > 1)
#
#   sql <- CirceR::cohortExpressionFromJson(cohortJson) |>
#     CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))
#
#   expect_type(sql, "character")
#   expect_true(nchar(sql) > 1)
#
#   cohortsToCreate <- tibble::tibble(
#     cohortId = 999,
#     cohortName = "CaprTest",
#     sql = sql
#   )
#
#   connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#   cohortTableNames <- CohortGenerator::getCohortTableNames("cohort")
#
#   invisible(capture_output(suppressMessages({
#     CohortGenerator::createCohortTables(connectionDetails,
#                                         cohortDatabaseSchema = "main",
#                                         cohortTableNames = cohortTableNames)
#
#
#     CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
#                                        cdmDatabaseSchema = "main" ,
#                                        cohortTableNames = cohortTableNames,
#                                        cohortDefinitionSet = cohortsToCreate,
#                                        incremental = FALSE)
#
#   df <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
#                                          cohortDatabaseSchema = "main",
#                                          cohortTable = "cohort",
#                                          cohortIds = c(999),
#                                          cohortDefinitionSet = cohortsToCreate)
#   })))
#
#   expect_true(df$cohortEntries > 1)
#
# })

test_that("compile generic works", {
  ch <- cohort(conditionOccurrence(cs(1,2, name = "test")))
  expect_gt(nchar(generics::compile(ch)), 10)
})


test_that("makeCohortSet works", {
  skip_if_not_installed("CirceR")
  #make concept set for celecoxib
  celecoxib <- cs(descendants(1118084), name = "celecoxib")

  #make cohort for celecoxib
  celecoxibCohort <- cohort(
    entry = entry(
      drugExposure(celecoxib)
    ),
    exit = exit(
      observationExit()
    )
  )

  #make concept set for diclofenac
  diclofenac <- cs(descendants(1124300), name = "diclofenac")

  #make cohort for diclofenac
  diclofenacCohort <- cohort(
    entry = entry(
      drugExposure(diclofenac)
    ),
    exit = exit(
      observationExit()
    )
  )


  kk <- makeCohortSet(celecoxibCohort, diclofenacCohort)
  expect_s3_class(kk, class = "data.frame")
  expect_type(kk$sql, "character")

})
