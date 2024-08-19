# tests/testthat/test-isStandardDB.R

library(testthat)
library(mockery)
library(dplyr)
source('R/isStandardDB.R')

# Mock data
mock_db_connection <- mock()
attr(mock_db_connection, "dbms") <- "postgresql"
mock_querySql <- mock(
  data.frame(
    CONCEPT_ID = c(1, 2, 3),
    CONCEPT_NAME = c("Concept1", "Concept2", "Concept3"),
    SOURCE_CODE = c("Code1", "Code2", "Code3"),
    STANDARD_CONCEPT = c(NA, "S", NA)
  )
)

# Mock the DatabaseConnector::querySql function
stub(isStandardDB, "DatabaseConnector::querySql", mock_querySql)

test_that("isStandardDB identifies non-standard concepts", {
  links <- list(
    "table1" = c("concept_id_col1", "source_code_col1")
  )
  
  result <- isStandardDB(mock_db_connection, "cdm_schema", "vocab_schema", links)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$concept_id, c("1", "3"))
  expect_equal(result$concept_name, c("Concept1", "Concept3"))
  expect_equal(result$source_code, c("Code1", "Code3"))
  expect_equal(result$source_table, c("table1", "table1"))
})

test_that("isStandardDB returns empty tibble when no non-standard concepts", {
  mock_querySql_empty <- mock(
    data.frame(
      CONCEPT_ID = c(1, 2, 3),
      CONCEPT_NAME = c("Concept1", "Concept2", "Concept3"),
      SOURCE_CODE = c("Code1", "Code2", "Code3"),
      STANDARD_CONCEPT = c("S", "S", "S")
    )
  )
  
  stub(isStandardDB, "DatabaseConnector::querySql", mock_querySql_empty)
  
  links <- list(
    "table1" = c("concept_id_col1", "source_code_col1")
  )
  
  result <- isStandardDB(mock_db_connection, "cdm_schema", "vocab_schema", links)
  
  expect_equal(nrow(result), 0)
})

test_that("isStandardDB saves results when save_path is provided", {
  temp_dir <- tempdir()
  
  links <- list(
    "table1" = c("concept_id_col1", "source_code_col1")
  )

  mock_querySql <- mock(
    data.frame(
        CONCEPT_ID = c(1, 2, 3),
        CONCEPT_NAME = c("Concept1", "Concept2", "Concept3"),
        SOURCE_CODE = c("Code1", "Code2", "Code3"),
        STANDARD_CONCEPT = c(NA, "S", "C")
    )
  )

  stub(isStandardDB, "DatabaseConnector::querySql", mock_querySql)

  result <- isStandardDB(mock_db_connection, "cdm_schema", "vocab_schema", links, save_path = temp_dir)
  
  expect_true(file.exists(file.path(temp_dir, "table1")))
  saved_data <- readr::read_csv(file.path(temp_dir, "table1"))
  expect_equal(nrow(saved_data), 3)
  expect_equal(nrow(result), 1)
})

test_that("isStandardDB handles multiple tables", {
  links <- list(
    "table1" = c("concept_id_col1", "source_code_col1"),
    "table2" = c("concept_id_col2", "source_code_col2")
  )
  
  mock_querySql <- mock(
    data.frame(
        CONCEPT_ID = c(1, 2, 3),
        CONCEPT_NAME = c("Concept1", "Concept2", "Concept3"),
        SOURCE_CODE = c("Code1", "Code2", "Code3"),
        STANDARD_CONCEPT = c(NA, "S", NA)
    ),
    cycle = TRUE
  )

  stub(isStandardDB, "DatabaseConnector::querySql", mock_querySql)

  result <- isStandardDB(mock_db_connection, "cdm_schema", "vocab_schema", links)
  
  expect_equal(nrow(result), 4)
  expect_equal(result$concept_id, c("1", "3", "1", "3"))
  expect_equal(result$source_table, c("table1", "table1", "table2", "table2"))
})