source("R/isStandard.R")

test_that("isStandard works expectedly with no data", {
  db_connection <- mockery::mock()
  data_concepts_path <- tempdir()
  vocab_schema <- "cdm"
  save_path <- tempdir()

  expect_error(
    isStandard(db_connection, data_concepts_path, vocab_schema, save_path)
  )
  unlink(data_concepts_path, recursive = TRUE, force = TRUE)
})


test_that("isStandard works correctly with only standard concepts", {
  db_connection <- mockery::mock()
  attr(db_connection, "dbms") <- "postgresql"
  data_concepts_path <- tempdir()
  vocab_schema <- "vocab_schema"

  # Mock the concept table query result
  concept_table <- tibble::tibble(
    CONCEPT_ID = c("1", "2"),
    CONCEPT_NAME = c("Standard1", "Standard2"),
    STANDARD_CONCEPT = c("S", "S"),
  )

  mockery::stub(isStandard, "DatabaseConnector::querySql", concept_table)

  # Mock source data
  mockdatapath <- tempfile(pattern = "mockdata", fileext = ".csv", tmpdir = data_concepts_path)
  mockdata <- tibble::tribble(
    ~sourceCode, ~concept_id,
    "A", "1",
    "B", "2"
  )

  # Create a mock CSV file with standard concepts
  write.csv(data.frame(mockdata), mockdatapath, row.names = FALSE)

  res <- isStandard(db_connection, data_concepts_path, vocab_schema)

  expect_equal(nrow(res), 0)

  unlink(data_concepts_path, recursive = TRUE, force = TRUE)
})

test_that("isStandard works correctly with non-standard concepts", {
  db_connection <- mockery::mock()
  attr(db_connection, "dbms") <- "postgresql"
  data_concepts_path <- tempdir()
  vocab_schema <- "vocab_schema"

  # Mock the concept table query result
  concept_table <- tibble::tibble(
    CONCEPT_ID = c("1", "2"),
    CONCEPT_NAME = c("Standard", "Non-standard"),
    STANDARD_CONCEPT = c("Standard", "Non-standard"),
  )

  mockery::stub(isStandard, "DatabaseConnector::querySql", concept_table)

  # Mock source data
  mockdatapath <- tempfile(pattern = "mockdata", fileext = ".csv", tmpdir = data_concepts_path)
  mockdata <- tibble::tribble(
    ~sourceCode, ~concept_id,
    "A", "1",
    "B", "2"
  )
  filepath <- sub(".*(mockdata.*)", "\\1", mockdatapath)

  # Create a mock CSV file with non-standard concepts
  write.csv(data.frame(mockdata), mockdatapath, row.names = FALSE)

  res <- isStandard(db_connection, data_concepts_path, vocab_schema)

  expect_equal(nrow(res), 1)
  expect_equal(res$concept_id, "2")
  expect_equal(res$concept_name, "Non-standard")
  expect_equal(res$source_code, "B")
  expect_contains(res$source_table, filepath)
  expect_equal(res$standard_concept, "Non-standard")

  unlink(data_concepts_path, recursive = TRUE, force = TRUE)
})

test_that("isStandard works correctly with all kinds of concepts", {
  db_connection <- mockery::mock()
  attr(db_connection, "dbms") <- "postgresql"
  data_concepts_path <- tempdir()
  vocab_schema <- "vocab_schema"

  # Mock the concept table query result
  concept_table <- tibble::tibble(
    CONCEPT_ID = c("1", "2", "3"),
    CONCEPT_NAME = c("Standard", "Non-Standard", "Classification"),
    STANDARD_CONCEPT = c("Standard", "Non-standard", "Classification"),
  )

  mockery::stub(isStandard, "DatabaseConnector::querySql", concept_table)

  # Mock source data
  mockdatapath <- tempfile(pattern = "mockdata", fileext = ".csv", tmpdir = data_concepts_path)
  mockdata <- tibble::tribble(
    ~sourceCode, ~concept_id,
    "A", "1",
    "B", "2",
    "C", "3"
  )

  filepath <- sub(".*(mockdata.*)", "\\1", mockdatapath)

  # Create a mock CSV file with all kinds of concepts
  write.csv(data.frame(mockdata), mockdatapath, row.names = FALSE)

  res <- isStandard(db_connection, data_concepts_path, vocab_schema)

  expect_equal(nrow(res), 2)
  expect_equal(res$concept_id, c("2", "3"))
  expect_equal(res$concept_name, c("Non-Standard", "Classification"))
  expect_equal(res$source_code, c("B", "C"))
  expect_contains(res$source_table, filepath)
  expect_equal(res$standard_concept, c("Non-standard", "Classification"))

  unlink(data_concepts_path, recursive = TRUE, force = TRUE)
})
