test_that("missing qualifiedLimit matches primaryCriteriaLimit when additionalCriteria is NULL", {
  e <- entry(
    conditionOccurrence(cs(1, name = "test")),
    primaryCriteriaLimit = "Last"
  )
  expect_equal(e@qualifiedLimit, "Last")

  e_all <- entry(
    conditionOccurrence(cs(1, name = "test")),
    primaryCriteriaLimit = "All"
  )
  expect_equal(e_all@qualifiedLimit, "All")
})

test_that("missing qualifiedLimit errors when additionalCriteria is set", {
  expect_error(
    entry(
      conditionOccurrence(cs(1, name = "test")),
      primaryCriteriaLimit = "All",
      additionalCriteria = withAll(
        atLeast(1, conditionOccurrence(cs(2, name = "test2")))
      )
    ),
    "qualifiedLimit must be provided when additionalCriteria is used"
  )
})

test_that("qualifiedLimit can be set explicitly when additionalCriteria is set", {
  e <- entry(
    conditionOccurrence(cs(1, name = "test")),
    primaryCriteriaLimit = "All",
    additionalCriteria = withAll(
      atLeast(1, conditionOccurrence(cs(2, name = "test2")))
    ),
    qualifiedLimit = "Last"
  )
  expect_equal(e@qualifiedLimit, "Last")
})
