test_that("customEra criteria serialize to Circe CustomEra", {
  ce <- customEra(
    criteriaList = list(
      conditionOccurrence(cs(1L, name = "condition")),
      drugExposure(cs(2L, name = "drug"))
    ),
    gapDays = 30L,
    first = TRUE,
    customEraStartDate(gte(as.Date("2010-01-01"))),
    customEraEndDate(lt(as.Date("2015-12-31"))),
    customEraDuration(bt(14L, 90L)),
    ageAtStart(gte(18L))
  )
  json <- jsonlite::fromJSON(toCohortJson(cohort(entry(ce))), simplifyVector = FALSE)
  crit <- json$PrimaryCriteria$CriteriaList[[1]]$CustomEra

  expect_length(crit$CriteriaList, 2)
  expect_equal(crit$GapDays, 30)
  expect_true(crit$First)
  expect_equal(crit$StartDate$Op, "gte")
  expect_equal(crit$EndDate$Op, "lt")
  expect_equal(crit$Duration$Op, "bt")
  expect_equal(crit$AgeAtStart$Op, "gte")
})

test_that("jsonToCapr round-trips CustomEra criteria", {
  ce <- customEra(
    criteriaList = list(
      conditionOccurrence(cs(1L, name = "condition")),
      drugExposure(cs(2L, name = "drug"))
    ),
    gapDays = 30L,
    first = TRUE,
    customEraDuration(bt(14L, 90L))
  )
  path <- tempfile(fileext = ".json")
  writeLines(toCohortJson(cohort(entry(ce))), path)

  code <- jsonToCapr(path)
  expect_match(code, "customEra\\(", all = FALSE)
  expect_match(code, "criteriaList = list\\(", all = FALSE)
  expect_match(code, "gapDays = 30L", all = FALSE)
})
