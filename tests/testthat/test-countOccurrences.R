source("R/countOccurrences.R")

# test_that("countOccurrences returns correct structure", {
#   mock_db <- mockery::mock()
#   attr(mock_db, "dbms") <- "postgresql"

#   # Mock the querySql function to return a predefined result
#   concept_id <- c("A" = 1, "B" = 2)
#   mockery::stub(countOccurrences, "DatabaseConnector::querySql", function(...) {
#     tibble::tibble(
#       concept_id = concept_id,
#       count_persons = c(1, 2),
#       count_records = c(3, 4),
#       desc_count_person = c(5, 6),
#       desc_count_record = c(7, 8)
#     )
#   })

#   v <- c("A" = 1, "B" = 2)
#   tables <- c("observation", "condition_occurrence")
#   links <- list(observation = "observation_concept_id", condition_occurrence = "condition_concept_id")
#   cdm_schema <- "main"
#   vocab_schema <- "main"

#   result <- countOccurrences(v, tables, links, mock_db, cdm_schema, vocab_schema)

#   expect_s3_class(result, "tbl_df")
#   expect_true(all(c("concept_id", "count_persons", "count_records", "desc_count_person", "desc_count_record", "concept_name") %in% colnames(result)))
# })


# test_that("countOccurrences handles empty input", {
#   mock_db <- mockery::mock()
#   attr(mock_db, "dbms") <- "postgresql"

#   # Mock the querySql function to return an empty result
#   mockery::stub(countOccurrences, "DatabaseConnector::querySql", function(...) {
#     tibble::tibble(
#       concept_id = integer(),
#       count_persons = integer(),
#       count_records = integer(),
#       desc_count_person = integer(),
#       desc_count_record = integer()
#     )
#   })

#   v <- c()
#   tables <- c("observation")
#   links <- list(observation = "observation_concept_id")
#   cdm_schema <- "main"
#   vocab_schema <- "main"

#   expect_error(countOccurrences(v, tables, links, mock_db, cdm_schema, vocab_schema))
# })


# test_that("countOccurrences handles missing concept ids", {
#   mock_db <- mockery::mock()
#   attr(mock_db, "dbms") <- "postgresql"

#   # Mock the querySql function to return a result with no matching concept ids
#   mockery::stub(countOccurrences, "DatabaseConnector::querySql", function(...) {
#     tibble::tibble(
#       concept_id = integer(),
#       count_persons = integer(),
#       count_records = integer(),
#       desc_count_person = integer(),
#       desc_count_record = integer()
#     )
#   })

#   v <- c(4, 5)
#   tables <- c("observation", "condition_occurrence")
#   links <- list(observation = "observation_concept_id", condition_occurrence = "condition_concept_id")
#   cdm_schema <- "main"
#   vocab_schema <- "main"

#   result <- countOccurrences(v, tables, links, mock_db, cdm_schema, vocab_schema)

#   expect_s3_class(result, "tbl_df")
#   expect_equal(nrow(result), length(v))
#   expect_true(all(result$count_persons == 0))
#   expect_true(all(result$count_records == 0))
#   expect_true(all(result$desc_count_person == 0))
#   expect_true(all(result$desc_count_record == 0))
# })


# test_that("countOccurrences saves results to file", {
#   mock_db <- mockery::mock()
#   attr(mock_db, "dbms") <- "postgresql"

#   # Mock the querySql function to return a predefined result
#   mockery::stub(countOccurrences, "DatabaseConnector::querySql", function(...) {
#     tibble::tibble(
#       concept_id = c(1, 2),
#       count_persons = c(2, 1),
#       count_records = c(3, 1),
#       desc_count_person = c(1, 0),
#       desc_count_record = c(1, 0)
#     )
#   })

#   v <- c(1, 2)
#   tables <- c("observation", "condition_occurrence")
#   links <- list(observation = "observation_concept_id", condition_occurrence = "condition_concept_id")
#   cdm_schema <- "main"
#   vocab_schema <- "main"
#   save_path <- tempdir()

#   result <- countOccurrences(v, tables, links, mock_db, cdm_schema, vocab_schema, save_path)

#   expect_true(file.exists(file.path(save_path, "count_occurrences.csv")))
# })


test_that("countOccurrences performs count query correctly", {
  # Create a connection to an in-memory SQLite database
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = ":memory:")
  conn <- DatabaseConnector::connect(connectionDetails)

  # Create the observation table
  DatabaseConnector::executeSql(conn, "
  CREATE TABLE main.observation (
    observation_id INTEGER PRIMARY KEY,
    person_id INTEGER,
    observation_concept_id INTEGER
  )
")

  # Create the concept_ancestor table
  DatabaseConnector::executeSql(conn, "
  CREATE TABLE main.concept_ancestor (
    ancestor_concept_id INTEGER,
    descendant_concept_id INTEGER
  )
")

  # Insert sample data into the observation table
  DatabaseConnector::executeSql(conn, "
  INSERT INTO main.observation (observation_id, person_id, observation_concept_id) VALUES
  (1, 101, 1001),
  (2, 102, 1002),
  (3, 103, 1003),
  (4, 101, 2004)
")

  # Insert sample data into the concept_ancestor table
  DatabaseConnector::executeSql(conn, "
  INSERT INTO main.concept_ancestor (ancestor_concept_id, descendant_concept_id) VALUES
  (1001, 1001),
  (1002, 1002),
  (1003, 1003),
  (1004, 1004),
  (1001, 2001),
  (1002, 2002),
  (1003, 2003),
  (1004, 2004)
")

  # Define the schema and tables
  vocab_schema <- "main"
  save_path <- tempdir()

  # Mock data for testing
  v <- c("A" = 1001, "B" = 1002, "C" = 1003, "D" = 1004)
  tables <- c("observation")
  links <- list(observation = "observation_concept_id")
  cdm_schema <- "main"
  vocab_schema <- "main"

  # Run the countOccurrences function
  result <- countOccurrences(v, tables, links, conn, cdm_schema, vocab_schema, save_path)

  # Verify the results
  expect_true(file.exists(file.path(save_path, "count_occurrences.csv")))

  expected_result <- tibble::tibble(
    concept_id = c(1001, 1002, 1003, 1004),
    count_persons = c(1, 1, 1, 0),
    count_records = c(1, 1, 1, 0),
    desc_count_person = c(1, 1, 1, 1),
    desc_count_record = c(1, 1, 1, 1),
    concept_name = c("A", "B", "C", "D")
  ) |>
    dplyr::arrange(dplyr::desc(count_records + desc_count_record))

  expect_equal(result, expected_result)
})
