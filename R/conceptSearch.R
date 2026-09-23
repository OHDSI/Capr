# Vocabulary / Concept Search ----------------------------------------------------
#
# These functions require a live connection to an OMOP CDM vocabulary schema.
# SQL is stored in inst/sql/sql_server/ following the OHDSI/HADES convention.
# SqlRender::loadRenderTranslateSql() renders and translates in one call.
#
# searchConcepts()     - universal ILIKE search, works on all DBMS
# rankedSearchConcepts() - two-phase ranked search with dialect-specific similarity:
#                          postgresql -> pg_trgm similarity()
#                          snowflake  -> JAROWINKLER_SIMILARITY()
#                          all others -> positional boost only (same structure)

# Read a SQL file from inst/sql/{dbms}/ with fallback to inst/sql/sql_server/.
.readSql <- function(name, dbms = "sql_server") {
  dialectPath <- fs::path_package("Capr", "sql", dbms, paste0(name, ".sql"))
  if (fs::file_exists(dialectPath)) return(readr::read_file(dialectPath))
  readr::read_file(fs::path_package("Capr", "sql", "sql_server", paste0(name, ".sql")))
}

# Queries vocabulary.vocabulary_id = 'None' row for the OMOP vocabulary version string.
.informVocabularyVersion <- function(connection, vocabularyDatabaseSchema) {
  tryCatch({
    sql <- .readSql("getVocabularyVersion") |>
      SqlRender::render(schema = vocabularyDatabaseSchema) |>
      SqlRender::translate(targetDialect = DatabaseConnector::dbms(connection))
    result <- DatabaseConnector::querySql(connection, sql)
    version <- result[[1]][1]
    if (!is.na(version) && nzchar(version)) {
      cli::cli_inform("Vocabulary: {.val {version}}")
    }
  }, error = function(e) invisible(NULL))  # silently skip if vocabulary table unavailable
}

#' Search the OMOP vocabulary by keyword
#'
#' Returns concepts whose concept_name matches a keyword.
#' Search is case-insensitive. SQL lives in inst/sql/searchConcepts.sql.
#'
#' @param keyword Character string to search for (partial match).
#' @param connection A DatabaseConnector connection to an OMOP CDM.
#' @param vocabularyDatabaseSchema Schema containing the OMOP vocabulary tables. Required
#' @param domain Optional character vector restricting by domain_id (e.g. "Condition").
#' @param standardOnly If TRUE (default), return only standard concepts.
#' @param limit Maximum rows to return. Default 100.
#' @return A tibble with concept_id, concept_name, domain_id, vocabulary_id,
#'   concept_class_id, standard_concept, concept_code.
#' @seealso getConceptDescendants, mapSourceToStandard, getConceptSetDetails, cs
#' @examples
#' \dontrun{
#' connection <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#' searchConcepts("atrial fibrillation", connection, vocabularyDatabaseSchema = "main")
#' searchConcepts("metformin", connection, vocabularyDatabaseSchema = "main", domain = "Drug")
#' }
#' @export
searchConcepts <- function(keyword, connection, vocabularyDatabaseSchema,
                           domain = NULL, standardOnly = TRUE, limit = 100L) {
  checkmate::assertCharacter(keyword, len = 1, min.chars = 1)
  checkmate::assertTRUE(DBI::dbIsValid(connection))
  checkmate::assertCharacter(vocabularyDatabaseSchema, len = 1, null.ok = TRUE)
  checkmate::assertIntegerish(limit, len = 1, lower = 1L)

  if (!is.null(domain)) {
    domainFilter <- paste0("AND UPPER(domain_id) IN (", paste(paste0("'", toupper(domain), "'"), collapse = ", "), ")")
  } else {
    domainFilter <- ""
  }

  standardFilter <- if (standardOnly) "AND standard_concept = 'S'" else ""
  .informVocabularyVersion(connection, vocabularyDatabaseSchema)
  sql <- .readSql("searchConcepts") |>
    SqlRender::render(
      limit = as.integer(limit),
      schema = vocabularyDatabaseSchema,
      keyword = keyword,
      domainFilter = domainFilter,
      standardFilter = standardFilter
    ) |>
    SqlRender::translate(targetDialect = DatabaseConnector::dbms(connection))
  tb <- DatabaseConnector::querySql(connection, sql) |> 
    tibble::as_tibble() |> 
    dplyr::rename_all(tolower)
  return(tb)
}

#' Get all descendants of one or more standard concepts
#'
#' Traverses concept_ancestor. SQL lives in inst/sql/getConceptDescendants.sql.
#'
#' @param conceptIds Integer vector of standard concept IDs.
#' @param connection A DatabaseConnector connection to an OMOP CDM.
#' @param vocabularyDatabaseSchema Schema containing the OMOP vocabulary tables. Required.
#' @param minLevels Minimum levels of separation (default 0 includes seeds).
#' @param maxLevels Maximum levels of separation (default Inf = all descendants).
#' @return A tibble with concept details plus min/max_levels_of_separation.
#' @seealso searchConcepts, descendants, cs
#' @examples
#' \dontrun{
#' connection <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#' getConceptDescendants(313217L, connection, vocabularyDatabaseSchema = "main")
#' }
#' @export
getConceptDescendants <- function(conceptIds, connection, vocabularyDatabaseSchema,
                                  minLevels = 0L, maxLevels = Inf) {
  checkmate::assertIntegerish(conceptIds, min.len = 1)
  checkmate::assertTRUE(DBI::dbIsValid(connection))
  .informVocabularyVersion(connection, vocabularyDatabaseSchema)
  maxLevelsSql <- if (is.infinite(maxLevels)) 99999L else as.integer(maxLevels)
  sql <- .readSql("getConceptDescendants") |>
    SqlRender::render(
      schema = vocabularyDatabaseSchema,
      conceptIds = as.integer(conceptIds),
      minLevels = as.integer(minLevels),
      maxLevels = maxLevelsSql
    ) |>
    SqlRender::translate(targetDialect = DatabaseConnector::dbms(connection))
  tb <- DatabaseConnector::querySql(connection, sql) |>
    tibble::as_tibble() |>
    dplyr::rename_all(tolower)
  return(tb)
}

#' Map source (non-standard) codes to their standard OMOP concepts
#'
#' Uses the concept_relationship 'Maps to' relationship.
#' SQL lives in inst/sql/mapSourceToStandard.sql.
#'
#' @param sourceCodes Character vector of source codes (e.g. ICD-10 codes).
#' @param connection A DatabaseConnector connection to an OMOP CDM.
#' @param vocabularyDatabaseSchema Schema containing the OMOP vocabulary tables. Required.
#' @param vocabularyId Optional vocabulary IDs to restrict source lookup (e.g. "ICD10CM").
#' @return A tibble with source and mapped standard concept columns.
#' @seealso searchConcepts, cs, mapped
#' @examples
#' \dontrun{
#' connection <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#' mapSourceToStandard(c("I48", "I48.0"), connection,
#'   vocabularyDatabaseSchema = "main", vocabularyId = "ICD10CM")
#' }
#' @export
mapSourceToStandard <- function(sourceCodes, connection, vocabularyDatabaseSchema,
                                vocabularyId = NULL) {
  checkmate::assertCharacter(sourceCodes, min.len = 1)
  checkmate::assertTRUE(DBI::dbIsValid(connection))
  .informVocabularyVersion(connection, vocabularyDatabaseSchema)

  if (!is.null(vocabularyId)) {
    vocabFilter <- paste0("AND UPPER(sc.vocabulary_id) IN (", paste(paste0("'", toupper(vocabularyId), "'"), collapse = ", "), ")")
  } else {
    vocabFilter <- ""
  }

  sql <- .readSql("mapSourceToStandard") |>
    SqlRender::render(
      schema = vocabularyDatabaseSchema,
      sourceCodes = paste0("'", toupper(sourceCodes), "'"),
      vocabFilter = vocabFilter
    ) |>
    SqlRender::translate(targetDialect = DatabaseConnector::dbms(connection))
  tb <- DatabaseConnector::querySql(connection, sql) |>
    tibble::as_tibble() |>
    dplyr::rename_all(tolower)
  return(tb)
}

#' Look up concept details by concept ID
#'
#' SQL lives in inst/sql/getConceptInfo.sql.
#'
#' @param conceptIds Integer vector of concept IDs.
#' @param connection A DatabaseConnector connection to an OMOP CDM.
#' @param vocabularyDatabaseSchema Schema containing the OMOP vocabulary tables. Required.
#' @return A tibble with full concept table columns for the requested IDs.
#' @seealso getConceptSetDetails, searchConcepts
#' @examples
#' \dontrun{
#' connection <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#' getConceptInfo(c(313217L, 320128L), connection, vocabularyDatabaseSchema = "main")
#' }
#' @export
getConceptInfo <- function(conceptIds, connection, vocabularyDatabaseSchema) {
  checkmate::assertIntegerish(conceptIds, min.len = 1)
  checkmate::assertTRUE(DBI::dbIsValid(connection))
  .informVocabularyVersion(connection, vocabularyDatabaseSchema)
  sql <- .readSql("getConceptInfo") |>
    SqlRender::render(
      schema = vocabularyDatabaseSchema,
      conceptIds = as.integer(conceptIds)
    ) |>
    SqlRender::translate(targetDialect = DatabaseConnector::dbms(connection))
  tb <- DatabaseConnector::querySql(connection, sql) |>
    tibble::as_tibble() |>
    dplyr::rename_all(tolower)
  return(tb)
}

#' Ranked vocabulary search with dialect-specific similarity scoring
#'
#' A two-phase search that first narrows candidates with ILIKE, then ranks them
#' using the best similarity function available for the connected DBMS:
#' \itemize{
#'   \item \strong{PostgreSQL}: \code{similarity()} from \code{pg_trgm} (requires the extension).
#'   \item \strong{Snowflake}: \code{JAROWINKLER_SIMILARITY()}.
#'   \item \strong{Spark / Databricks}: normalized \code{levenshtein()}.
#'   \item \strong{All other dialects}: positional boost scoring (exact > prefix > contains).
#' }
#' Synonym matching and mapping-count enrichment are scoped to the candidate set,
#' avoiding a full-table synonym scan.
#'
#' For PostgreSQL the GIN trigram index dramatically improves performance but is
#' not required: \code{CREATE INDEX ON concept USING GIN (concept_name gin_trgm_ops);}.
#'
#' @param keyword Character string to search for.
#' @param connection A \code{DatabaseConnector} connection to an OMOP CDM.
#' @param vocabularyDatabaseSchema Schema containing the OMOP vocabulary tables. Required.
#' @param domain Optional character vector restricting by \code{domain_id} (e.g. \code{"Condition"}).
#' @param standardOnly If \code{TRUE} (default), return only standard concepts.
#' @param limit Maximum rows to return. Default \code{50L}.
#' @param offset Pagination offset. Default \code{0L}.
#' @return A tibble with columns: \code{concept_id}, \code{concept_name}, \code{concept_code},
#'   \code{vocabulary_id}, \code{domain_id}, \code{concept_class_id}, \code{standard_concept},
#'   \code{relevance} (0–1.5 float), \code{mapping_count}.
#' @seealso \code{\link{searchConcepts}}, \code{\link{cs}}
#' @examples
#' \dontrun{
#' connection <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
#' rankedSearchConcepts("atrial fibrillation", connection, vocabularyDatabaseSchema = "main")
#' rankedSearchConcepts("metformin", connection, vocabularyDatabaseSchema = "main",
#'                    domain = "Drug", limit = 20L)
#' }
#' @export
rankedSearchConcepts <- function(keyword, connection, vocabularyDatabaseSchema,
                               domain = NULL, standardOnly = TRUE,
                               limit = 50L, offset = 0L) {
  checkmate::assertCharacter(keyword, len = 1, min.chars = 1)
  checkmate::assertTRUE(DBI::dbIsValid(connection))
  checkmate::assertCharacter(vocabularyDatabaseSchema, len = 1)
  checkmate::assertIntegerish(limit, len = 1, lower = 1L)
  checkmate::assertIntegerish(offset, len = 1, lower = 0L)

  .informVocabularyVersion(connection, vocabularyDatabaseSchema)
  dbms <- DatabaseConnector::dbms(connection)
  optimized <- list(
    postgresql = "pg_trgm similarity()",
    snowflake  = "JAROWINKLER_SIMILARITY()",
    spark      = "normalized levenshtein()"
  )
  if (dbms %in% names(optimized)) {
    cli::cli_inform("rankedSearchConcepts: using {.strong {dbms}} route ({optimized[[dbms]]}).")
  } else {
    cli::cli_warn(c(
      "rankedSearchConcepts: no optimized SQL for dialect {.val {dbms}}.",
      "i" = "Falling back to positional-boost ranking (sql_server). For simple search use {.fn searchConcepts}."
    ))
  }

  if (!is.null(domain)) {
    domainFilter <- paste0("AND UPPER(domain_id) IN (", paste(paste0("'", toupper(domain), "'"), collapse = ", "), ")")
  } else {
    domainFilter <- ""
  }

  if (standardOnly) {
    standardFilter <- "AND standard_concept = 'S'"
  } else {
    standardFilter <- ""
  }

  sql <- .readSql("rankedSearchConcepts", dbms = dbms) |>
    SqlRender::render(
      schema = vocabularyDatabaseSchema,
      keyword = keyword,
      domainFilter = domainFilter,
      standardFilter = standardFilter,
      limit = as.integer(limit),
      offset = as.integer(offset)
    ) |>
    SqlRender::translate(targetDialect = dbms)
  tb <- DatabaseConnector::querySql(connection, sql) |>
    tibble::as_tibble() |>
    dplyr::rename_all(tolower)
  return(tb)
}
