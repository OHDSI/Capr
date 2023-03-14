test_that("cohort with additional criteria works", {
  skip_if_not_installed("CirceR")
  # LEGEND stroke
  cd <- cohort(
    entry = entry(
      condition(cs(372924, 376713, 441874, 439847,
                   432923, 43530727, 4148906,
                   descendants(443454), name = "stroke")),
      observationWindow = continuousObservation(0L, 0L),
      primaryCriteriaLimit = "All",
      additionalCriteria = withAny(
        atLeast(1,
          visit(cs(descendants(9201, 9203, 262), name = "IP Visit")),
          duringInterval(startWindow = eventStarts(-Inf, 1),
                         endWindow = eventEnds(0, Inf, index = "startDate")
          )
        )
      ),
      qualifiedLimit = "All"
    ),
    exit = exit(
      endStrategy = fixedExit(index = "startDate", offsetDays = 7L)
    ),
    era = era(eraDays = 180L)
  )

  # debugonce(toCirce)
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
