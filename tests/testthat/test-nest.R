test_that("Nesting Criteria works", {

  query <- visit(cs(descendants(9201, 9203, 262)),
                 nestedWithAll(
                   atLeast(1,
                           condition(cs(descendants(316139), name = "heart failure")),
                           duringInterval(startWindow = eventStarts(0, Inf),
                                          endWindow = eventStarts(-Inf, 0, index = "endDate")
                           )
                   )
                 )
  )
  expect_s4_class(query@attributes[[1]], "nestedAttribute")
   #chekc coercion
  query_list <- Capr:::as.list(query)
  expect_equal(names(query_list$VisitOccurrence)[2], "CorrelatedCriteria")
})

test_that("Can build a nested cohort", {

  cd <- cohort(
    entry = entry(
      visit(cs(descendants(9201, 9203, 262)),
            nestedWithAll(
              atLeast(1,
                      condition(cs(descendants(316139), name = "heart failure")),
                      duringInterval(startWindow = eventStarts(0, Inf),
                                     endWindow = eventStarts(-Inf, 0, index = "endDate")
                      )
              )
            )
      ),
      observationWindow = continuousObservation(0L, 0L),
      primaryCriteriaLimit = "First"
    ),
    exit = exit(
      endStrategy = fixedExit(index = "endDate", offsetDays = 1L)
    ),
    era = era(eraDays = 7L)
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


})
