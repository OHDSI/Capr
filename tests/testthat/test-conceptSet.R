test_that("cs helpers work", {
  concepts <- cs(3, descendants(4, mapped(5, exclude(6))))

  df <- tibble::tribble(
    ~conceptId, ~includeDescendants, ~isExcluded, ~includeMapped,
     3,          FALSE,               FALSE,      FALSE,
     4,          TRUE ,               FALSE,      FALSE,
     5,          TRUE ,               FALSE,      TRUE,
     6,          TRUE ,               TRUE  ,     TRUE)

  expect_equal(as.data.frame(concepts), df)

})

test_that("writeConceptSet works", {
  path <- tempfile("concepts", fileext = ".json")
  concepts <- cs(1, 2, descendants(4, 5), exclude(descendants(6, 7)))
  writeConceptSet(concepts, path = path)
  # cat(readr::read_file(path))
  concepts2 <- readConceptSet(path)
  expect_equal(concepts@Expression, concepts2@Expression)
  # name and id will be different if you write then read back in

})

test_that("dedupConceptSet works", {
  allConceptSets <- list(cs(2,1), cs(2,3), cs(1,2))
  r <- dedupConceptSets(allConceptSets)
  uniqueConceptSets <- r$uniqueConceptSets
  lookup <- r$lookup

  expect_equal(unname(lookup[allConceptSets[[1]]@id]), unname(lookup[allConceptSets[[3]]@id]))
})

test_that("equality works", {
  # order should not matter
  expect_true(cs(1,descendants(9),3) == cs(1,3,descendants(9)))
  expect_false(cs(2,descendants(9),3) == cs(1,3,descendants(9)))

  expect_false(cs(descendants(9), 3) == cs(9,3))
  expect_false(cs(mapped(9), 3) == cs(9,3))
  expect_false(cs(exclude(9), 3) == cs(9,3))
  expect_false(cs(exclude(9), 3) == cs(exclude(mapped(9)),3))
  expect_false(cs(exclude(9), 3) == cs(exclude(mapped(9)),3))

  expect_error(cs(3, 3), "duplicated") # duplicates are not allowed

  # name and id are ignored
  expect_true(cs(9, 3, name = "a") == cs(9, 3, name = "b"))
  expect_true(cs(9, 3, id = "a") == cs(9, 3, id = "b"))

})

test_that("uniqueConceptSets works", {
  x <- uniqueConceptSets(list(
    cs(1,2,3),
    cs(1,2,3),
    cs(1,2, exclude(3))
  ))

  expect_true(length(x) == 2)
  expect_true(x[[1]] == cs(1,2,3))
  expect_true(x[[2]] == cs(1,2, exclude(3)))
})

test_that("concept set id does not depend on order", {
  a <- cs(1,descendants(9),3)
  b <- cs(1,3,descendants(9))
  c <- cs(1,3,9)
  d <- cs(1,descendants(9),3)

  expect_type(a@id, "character")
  expect_true(a@id == b@id)
  expect_true(a@id == d@id)
  expect_false(b@id == c@id)
})



