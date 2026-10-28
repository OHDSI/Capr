test_that("episode query serializes to Circe Episode criteria", {
  ep <- episode(
    cs(1L, 2L, name = "episodes"),
    firstOccurrence(),
    episodeStartDate(gte(as.Date("2018-01-01"))),
    episodeEndDate(lte(as.Date("2020-01-01"))),
    episodeNumber(gte(1L))
  )
  json <- jsonlite::fromJSON(toCohortJson(cohort(entry(ep))), simplifyVector = FALSE)
  crit <- json$PrimaryCriteria$CriteriaList[[1]]$Episode

  expect_equal(crit$CodesetId, 0)
  expect_true(crit$First)
  expect_equal(crit$EpisodeStartDate$Op, "gte")
  expect_equal(crit$EpisodeEndDate$Op, "lte")
  expect_equal(crit$EpisodeNumber$Op, "gte")
})

test_that("Episode concept set selections serialize", {
  ep <- episode(
    cs(1L, name = "episodes"),
    episodeTypeCS(cs(2L, name = "type")),
    episodeObjectConceptCS(cs(3L, name = "object")),
    genderCS(cs(8507L, name = "male"))
  )
  json <- jsonlite::fromJSON(toCohortJson(cohort(entry(ep))), simplifyVector = FALSE)
  crit <- json$PrimaryCriteria$CriteriaList[[1]]$Episode

  expect_equal(crit$EpisodeTypeCS$CodesetId, 1)
  expect_equal(crit$EpisodeObjectConceptCS$CodesetId, 2)
  expect_equal(crit$GenderCS$CodesetId, 3)
})

test_that("jsonToCapr round-trips Episode criteria", {
  ch <- cohort(entry(episode(
    cs(1L, name = "episodes"),
    firstOccurrence(),
    episodeStartDate(gte(as.Date("2018-01-01"))),
    episodeNumber(gte(1L))
  )))
  path <- tempfile(fileext = ".json")
  writeLines(toCohortJson(ch), path)

  code <- jsonToCapr(path)
  expect_match(code, "episode\\(", all = FALSE)
  expect_match(code, "episodeStartDate\\(", all = FALSE)
  expect_match(code, "episodeNumber\\(", all = FALSE)
})
