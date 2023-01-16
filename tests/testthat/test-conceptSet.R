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


test_that("read/writeConceptSet works", {

  # read in a csv file exported from Atlas
  cs1 <- readConceptSet(system.file("extdata/conceptSetExpressionAtlasExportExample.csv", mustWork = TRUE, package = "Capr"))
  expect_s4_class(cs1, "ConceptSet")
  expect_true(nrow(as.data.frame(cs1)) == 10)

  # write and read concept set as csv
  f1 <- tempfile(fileext = ".csv")
  aceInhibitors <- cs(descendants(1335471, 1340128, 1341927, 1363749, 1308216, 1310756, 1373225, 1331235, 1334456, 1342439))
  writeConceptSet(aceInhibitors, f1)
  comparison1 <- readConceptSet(f1)
  expect_true(aceInhibitors == comparison1)
  # waldo::compare(aceInhibitors, comparison)

  # write and read concept set as json
  f2 <- tempfile(fileext = ".json")
  writeConceptSet(aceInhibitors, f2)
  comparison2 <- readConceptSet(f2)
  expect_true(aceInhibitors == comparison2)

  # read from a subset of the concept table
  cs3 <- readConceptSet(system.file("extdata/selectionFromConceptTable.csv", mustWork = TRUE, package = "Capr"))
  expect_s4_class(cs3, "ConceptSet")
  expect_true(nrow(as.data.frame(cs3)) == 1)

  # read from output of Codelist Generator
  cs4 <- readConceptSet(system.file("extdata/codelistGeneratorOutputExample.csv", mustWork = TRUE, package = "Capr"))
  expect_s4_class(cs4, "ConceptSet")
  expect_true(nrow(as.data.frame(cs4)) == 2)
})


