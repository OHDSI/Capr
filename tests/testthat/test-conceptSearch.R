# test-conceptSearch.R
# Tests for vocabulary search functions using Eunomia (SQLite) as the OMOP CDM.
# All tests skip automatically when Eunomia or DatabaseConnector are unavailable.
#
# Concept IDs used (present in every Eunomia release):
#   192671 - Gastrointestinal hemorrhage  (Condition, standard)
#   21600744 - Drugs for diabetes except insulin (Drug, standard)
#
# ICD-10 code used: "I48" (Atrial Fibrillation) - expected to map to standard concept 313217

# ---------- helpers -----------------------------------------------------------

.eunomia_skip <- function() {
  skip_if_not_installed("Eunomia")
  skip_if_not_installed("DatabaseConnector")
}

.eunomia_con <- function() {
  suppressMessages(
    DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
  )
}

# ---------- searchConcepts ----------------------------------------------------

test_that("searchConcepts returns a tibble with expected columns", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- searchConcepts("hemorrhage", con, vocabularyDatabaseSchema = "main")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                     "concept_class_id", "standard_concept", "concept_code") %in% names(result)))
})

test_that("searchConcepts finds a known concept by name", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- searchConcepts("gastrointestinal hemorrhage", con,
                            vocabularyDatabaseSchema = "main", standardOnly = FALSE)

  expect_true(192671L %in% result$concept_id)
})

test_that("searchConcepts standardOnly = TRUE returns only standard concepts", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- searchConcepts("hemorrhage", con, vocabularyDatabaseSchema = "main",
                            standardOnly = TRUE)

  if (nrow(result) > 0) {
    expect_true(all(result$standard_concept == "S"))
  }
})

test_that("searchConcepts domain filter restricts results", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- searchConcepts("hemorrhage", con, vocabularyDatabaseSchema = "main",
                            domain = "Condition", standardOnly = FALSE)

  if (nrow(result) > 0) {
    expect_true(all(toupper(result$domain_id) == "CONDITION"))
  }
})

test_that("searchConcepts respects limit", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- searchConcepts("a", con, vocabularyDatabaseSchema = "main",
                            standardOnly = FALSE, limit = 3L)

  expect_lte(nrow(result), 3L)
})

test_that("searchConcepts errors on bad arguments", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  expect_error(searchConcepts("", con, vocabularyDatabaseSchema = "main"))
  expect_error(searchConcepts("test", con, vocabularyDatabaseSchema = "main", limit = 0L))
})

# ---------- getConceptDescendants ---------------------------------------------

test_that("getConceptDescendants returns a tibble with level columns", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- getConceptDescendants(192671L, con, vocabularyDatabaseSchema = "main")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("concept_id", "concept_name", "min_levels_of_separation",
                     "max_levels_of_separation") %in% names(result)))
})

test_that("getConceptDescendants includes the seed concept at level 0", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- getConceptDescendants(192671L, con, vocabularyDatabaseSchema = "main",
                                   minLevels = 0L)

  expect_true(192671L %in% result$concept_id)
  seed_row <- result[result$concept_id == 192671L, ]
  expect_equal(seed_row$min_levels_of_separation, 0L)
})

test_that("getConceptDescendants minLevels = 1 excludes the seed", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- getConceptDescendants(192671L, con, vocabularyDatabaseSchema = "main",
                                   minLevels = 1L)

  expect_false(192671L %in% result$concept_id)
})

# ---------- mapSourceToStandard -----------------------------------------------

test_that("mapSourceToStandard returns a tibble with source and standard columns", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- mapSourceToStandard("K92.1", con, vocabularyDatabaseSchema = "main")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("source_concept_id", "source_code", "standard_concept_id",
                     "standard_concept_name") %in% names(result)))
})

test_that("mapSourceToStandard vocabularyId filter restricts source vocabulary", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- mapSourceToStandard("K92.1", con, vocabularyDatabaseSchema = "main",
                                 vocabularyId = "ICD10CM")

  if (nrow(result) > 0) {
    expect_true(all(toupper(result$source_vocabulary_id) == "ICD10CM"))
  }
})

# ---------- getConceptInfo ----------------------------------------------------

test_that("getConceptInfo returns full details for known concept IDs", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- getConceptInfo(c(192671L), con, vocabularyDatabaseSchema = "main")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("concept_id", "concept_name", "domain_id", "vocabulary_id",
                     "concept_class_id", "standard_concept", "concept_code",
                     "valid_start_date", "valid_end_date") %in% names(result)))
  expect_true(192671L %in% result$concept_id)
  expect_equal(
    result[result$concept_id == 192671L, ]$concept_name,
    "Gastrointestinal hemorrhage"
  )
})

test_that("getConceptInfo accepts multiple concept IDs", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  result <- getConceptInfo(c(192671L, 21600744L), con,
                            vocabularyDatabaseSchema = "main")

  expect_s3_class(result, "tbl_df")
  expect_true(192671L %in% result$concept_id)
})

test_that("getConceptInfo errors on non-integer input", {
  .eunomia_skip()
  con <- .eunomia_con()
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  expect_error(getConceptInfo("not_an_id", con, vocabularyDatabaseSchema = "main"))
})
