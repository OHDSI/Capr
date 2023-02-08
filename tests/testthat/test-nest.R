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

  query_list <- Capr:::as.list(query)
  expect_equal(names(query_list$VisitOccurrence)[2], "CorrelatedCriteria")

  l <- listConceptSets(query)
  expect_true(all(purrr::map_lgl(l, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("Can build a cohort with nested attribute", {

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
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = FALSE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)

  # Test generatation
  # sql <- SqlRender::render(sql,
  #                   vocabulary_database_schema = "main",
  #                   cdm_database_schema = "main",
  #                   target_database_schema = "main",
  #                   target_cohort_id = "1",
  #                   target_cohort_table = "cohort",
  #                   results_database_schema = "main")

  # expect_length(stringr::str_extract_all(sql, "@\\w+")[[1]], 0)

  # Error in cohort sql translation for sqlite
  # sql <- SqlRender::translate(sql, "sqlite")

  # sql <- SqlRender::translate(sql, "duckdb")

  # con <- DBI::dbConnect(duckdb::duckdb(), Eunomia::eunomiaDir("GIBleed", dbms = "duckdb")) # requires development version of Eunomia

  # DBI::dbExecute(con,
  #   "CREATE TABLE main.cohort (
  # 	cohort_definition_id BIGINT,
  # 	subject_id BIGINT,
  # 	cohort_start_date DATE,
  # 	cohort_end_date DATE
  # );")
  #
  # SqlRender::splitSql(sql) %>% purrr::walk(~DBI::dbExecute(con, .))
  #
  # df <- DBI::dbGetQuery(con, "select * from main.cohort")
  #
  # expect_s3_class(df, "data.frame")
  # DBI::dbDisconnect(con, shutdown = TRUE)
})


test_that("Can build a cohort with nested groups", {

  t2dDrug <- drug(cs(descendants(1502809,1502826,1503297,1510202,1515249,1516766,
                                 1525215,1529331,1530014,1547504,1559684,1560171,
                                 1580747,1583722,1594973,1597756)))

  t2d <- condition(cs(descendants(201826)))

  t1d <- condition(cs(descendants(201254)))


  t1dDrug <- drug(cs(descendants(1502905,1513876,1516976,1517998,
                                 1531601,1544838,1550023,1567198)))

  t1dDrugWT2Drug <- drug(cs(descendants(1502905,1513876,1516976,1517998,
                                        1531601,1544838,1550023,1567198)),
                         nestedWithAll(
                           atLeast(1, t2dDrug,
                                   duringInterval(startWindow = eventStarts(-Inf, -1))
                           )
                         )
  )

  abLabFast <- measurement(cs(descendants(3037110)),
                           valueAsNumber(gte(125)))
  abLabHb <- measurement(cs(descendants(3003309,3004410,3005673,3007263)),
                         valueAsNumber(gte(6)))
  abLabRan <- measurement(cs(descendants(3000483,3004501)),
                          valueAsNumber(gte(200)))


  cd <- cohort(
    entry = entry(
      t2d,
      t2dDrug,
      abLabHb,
      abLabRan,
      abLabFast,
      observationWindow = continuousObservation(0L, 0L),
      primaryCriteriaLimit = "All",
      additionalCriteria = withAll(
        exactly(0,
                t1d,
                duringInterval(startWindow = eventStarts(-Inf, 0)))
      ),
      qualifiedLimit = "First"
    ),
    attrition = attrition(
      't2dAlgo' = withAny(
        # Path 1
        withAll(
          exactly(0, t2d, duringInterval(startWindow = eventStarts(-Inf, 0))),
          atLeast(1, t2dDrug, duringInterval(startWindow = eventStarts(-Inf, 0))),
          withAny(
            atLeast(1, abLabHb, duringInterval(startWindow = eventStarts(-Inf, 0))),
            atLeast(1, abLabRan, duringInterval(startWindow = eventStarts(-Inf, 0))),
            atLeast(1, abLabFast, duringInterval(startWindow = eventStarts(-Inf, 0)))
          )
        ),
        #Path 2
        withAll(
          atLeast(1, t2d, duringInterval(startWindow = eventStarts(-Inf, 0))),
          exactly(0, t1dDrug, duringInterval(startWindow = eventStarts(-Inf, 0))),
          exactly(0, t2dDrug, duringInterval(startWindow = eventStarts(-Inf, 0))),
          withAny(
            atLeast(1, abLabHb, duringInterval(startWindow = eventStarts(-Inf, 0))),
            atLeast(1, abLabRan, duringInterval(startWindow = eventStarts(-Inf, 0))),
            atLeast(1, abLabFast, duringInterval(startWindow = eventStarts(-Inf, 0)))
          )
        ),
        #Path 3
        withAll(
          atLeast(1, t2d, duringInterval(startWindow = eventStarts(-Inf, 0))),
          exactly(0, t1dDrug, duringInterval(startWindow = eventStarts(-Inf, 0))),
          atLeast(0, t2dDrug, duringInterval(startWindow = eventStarts(-Inf, 0)))
        ),
        #Path 4
        withAll(
          atLeast(1, t2d, duringInterval(startWindow = eventStarts(-Inf, 0))),
          atLeast(1, t1dDrug, duringInterval(startWindow = eventStarts(-Inf, 0))),
          atLeast(1, t1dDrugWT2Drug, duringInterval(startWindow = eventStarts(-Inf, 0)))
        ),
        #Path 5
        withAll(
          atLeast(1, t2d, duringInterval(startWindow = eventStarts(-Inf, 0))),
          atLeast(1, t1dDrug, duringInterval(startWindow = eventStarts(-Inf, 0))),
          exactly(0, t2dDrug, duringInterval(startWindow = eventStarts(-Inf, 0))),
          atLeast(2, t2d, duringInterval(startWindow = eventStarts(-Inf, 0)))
        )
      )
    ),
    exit = exit(
      endStrategy = observationExit(),
      censor = censoringEvents(t1d)
    )
  )

  expect_s4_class(cd, "Cohort")

  cohortList <- toCirce(cd)
  expect_type(cohortList, "list")

  cohortJson <- jsonlite::toJSON(cohortList, pretty = T, auto_unbox = TRUE) %>%
    as.character()

  expect_type(cohortJson, "character")
  expect_true(nchar(cohortJson) > 1)

  sql <- CirceR::cohortExpressionFromJson(cohortJson) %>%
    CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = FALSE))

  expect_type(sql, "character")
  expect_true(nchar(sql) > 1)

})

test_that("listConceptSets works with nested Query", {
  a <- visit(cs(descendants(9201, 9203, 262)),
    nestedWithAll(
      atLeast(1,
        condition(cs(descendants(316139), name = "heart failure"))
      )
    )
  )

  conceptSets <- listConceptSets(a)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})


# extra code for generation

  # sql <- CirceR::cohortExpressionFromJson(cohortJson) %>%
  #   CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = FALSE))
  #
  # expect_type(sql, "character")
  # expect_true(nchar(sql) > 1)
  #
  # sql <- SqlRender::render(sql,
  #                          vocabulary_database_schema = "main",
  #                          cdm_database_schema = "main",
  #                          target_database_schema = "main",
  #                          target_cohort_id = "1",
  #                          target_cohort_table = "cohort",
  #                          results_database_schema = "main")
  #
  # expect_length(stringr::str_extract_all(sql, "@\\w+")[[1]], 0)
  #
  # # Error in cohort sql translation for sqlite
  # # sql <- SqlRender::translate(sql, "sqlite")
  #
  # sql <- SqlRender::translate(sql, "duckdb")
  #
  # con <- DBI::dbConnect(duckdb::duckdb(), Eunomia::eunomiaDir("GIBleed", dbms = "duckdb")) # requires development version of Eunomia
  #
  # DBI::dbExecute(con,
  #                "CREATE TABLE main.cohort (
  # 	cohort_definition_id BIGINT,
  # 	subject_id BIGINT,
  # 	cohort_start_date DATE,
  # 	cohort_end_date DATE
  # );")
  #
  # SqlRender::splitSql(sql) %>% purrr::walk(~DBI::dbExecute(con, .))
  #
  # df <- DBI::dbGetQuery(con, "select * from main.cohort")
  #
  # expect_s3_class(df, "data.frame")
  # DBI::dbDisconnect(con, shutdown = TRUE)



