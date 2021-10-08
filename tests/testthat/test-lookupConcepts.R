#context("Lookup Concepts") #depreciated testthat v3
library(Capr)
library(dplyr)
library(tibble)



test_that("Lookup concepts by id", {
  #set up connection details to test db
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    port = 5432
  )
  #connect to test db
  connection <- DatabaseConnector::connect(connectionDetails)
  #set schema for vocabulary
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")

  #lookup warfarin rxnorm concept id 1310149
  warfTest <- getConceptIdDetails(conceptIds = 1310149,
                      connection = connection,
                      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                      mapToStandard = TRUE)
  # check values in object are set
  expect_equal(warfTest$standardConcept, "S")
  expect_equal(warfTest$conceptId, 1310149L)
  expect_equal(warfTest$vocabularyId, "RxNorm")

})

test_that("Lookup concepts by code and vocabulary", {
  #set up connection details to test db
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    port = 5432
  )
  #connect to test db
  #connection <- DatabaseConnector::connect(connectionDetails)
  #set schema for vocabulary
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")

  #lookup non-standard warfarin snomed concept code 372756006
  #new connection using connection details instead of open connection
  warfTest <- getConceptCodeDetails(conceptCode = "372756006",
                                    vocabulary = "SNOMED",
                                    connectionDetails = connectionDetails,
                                    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                    mapToStandard = FALSE)
  # check values in object are set
  expect_true(is.na(warfTest$standardConcept))
  expect_equal(warfTest$conceptId, 4187015L)
  expect_equal(warfTest$vocabularyId, "SNOMED")

})

test_that("Lookup keywords", {
  #set up connection details to test db
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    port = 5432
  )
  #connect to test db
  connection <- DatabaseConnector::connect(connectionDetails)
  #set schema for vocabulary
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")

  #lookup non-standard warfarin snomed concept code 372756006
  warfTest <- lookupKeyword(keyword = "Warfarin",
                            searchType = "exact",
                            connection = connection,
                            vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  warfTestSingle <- warfTest %>%
    dplyr::filter(standardConcept == "S")

  # check values in object are set
  expect_true(tibble::is_tibble(warfTest))
  expect_equal(warfTestSingle$standardConcept, "S")
  expect_equal(warfTestSingle$conceptId, 1310149L)
  expect_equal(warfTestSingle$vocabularyId, "RxNorm")

})
