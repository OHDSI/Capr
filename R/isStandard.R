#' Check for Non-Standard Concepts in Database Tables
#'
#' This function examines a set of tables for non-standard concepts by comparing them against a standard concepts table in a SQL database. It identifies non-standard concepts and optionally saves the joined tables that contain these non-standard concepts.
#'
#' @param db_connection A DBI database connection object to the SQL database containing the standard concepts table.
#' @param data_concepts_path The file path to the directory containing CSV files of tables to be checked against the standard concepts table. Each CSV file should contain at least the columns `sourceCode` and `concept_id`.
#' @param save_path (Optional) The file path where the joined tables containing non-standard concepts should be saved. If not provided, the tables will not be saved but will still be checked for non-standard concepts.
#' @param vocab_schema The schema name of the vocabulary database containing the standard concepts table.
#'
#' @return A tibble containing the columns `concept_id`, `concept_name`, `source_code`, and `source_table`, which represent the concept ID, concept name, source code, and source table name for each non-standard concept found.
#'
#' @details The function first queries the `cdm.concept` table from the provided SQL database to retrieve the standard concepts. It then reads each CSV file in the specified directory, preparing and joining it with the standard concepts table based on the concept ID. Non-standard concepts are identified based on the `standard_concept` column not being 'S' or 'C'. Information about these non-standard concepts is collected and returned as a tibble.
#'
#' If a `save_path` is provided, each joined table that contains at least one non-standard concept is saved to the specified directory with the same name as the original table file.
#'
#' @examples
#' # Assuming you have a valid DBI connection `db_conn` and your tables are located in "path/to/data_concepts":
#' non_standard_concepts <- isStandard(db_conn, "path/to/data_concepts", "path/to/save_non_standard/")
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate across filter select inner_join
#' @importFrom DatabaseConnector connect disconnect querySql
#' @export
isStandard <- function(db_connection, data_concepts_path, vocab_schema, save_path = NULL) {
  # Read concept table from SQL database
  concept_table_query <- SqlRender::render(
    "SELECT concept_id, CONCEPT_NAME, standard_concept FROM @vocab_schema.concept",
    vocab_schema = vocab_schema
  )

  concept_table_query_translated <- SqlRender::translate(
    sql = concept_table_query,
    targetDialect = attr(db_connection, "dbms")
  )

  concept_table <- DatabaseConnector::querySql(db_connection, concept_table_query_translated) |>
    dplyr::rename(concept_id = CONCEPT_ID) |>
    dplyr::mutate(concept_id = as.character(concept_id)) |>
    dplyr::mutate(concept_id = tolower(trimws(concept_id)))

  # Initialize vectors for non-standard concepts
  nonStandard <- c()
  conceptNameNonStandard <- c()
  sourceCodeNonStandard <- c()
  sourceTableNonStandard <- c()
  standard_concept <- c()

  # Get tables from data_concepts_path
  tables <- list.files(path = data_concepts_path, pattern = "\\.csv$", full.names = TRUE)

  # Stop if path does not point towards dir of CSVs
  if (length(tables) == 0) {
    stop("No CSV files found in the specified directory.")
  }

  for (table_path in tables) {
    table_name <- basename(table_path)

    # Read and prepare table
    tb <-
      readr::read_csv(table_path, col_types = readr::cols(
        sourceCode = readr::col_character(),
        concept_id = readr::col_character()
      )) |>
      dplyr::mutate(across(c(sourceCode, concept_id), ~ gsub("\u00A0", " ", .))) |>
      dplyr::mutate(dplyr::across(c(sourceCode, concept_id), ~ trimws(.))) |>
      dplyr::filter(!is.na(sourceCode), !is.na(concept_id)) |>
      dplyr::mutate(
        concept_id = tolower(concept_id),
        concept_id = as.character(concept_id)
      ) |>
      dplyr::select(sourceCode, concept_id)

    # Join tables
    joined <- dplyr::inner_join(concept_table, tb, by = "concept_id")|>
      dplyr::rename(standard_concept = STANDARD_CONCEPT, concept_name = CONCEPT_NAME) |>
      dplyr::mutate(standard_concept = ifelse(is.na(standard_concept), 'Non-standard', standard_concept)) |>
      dplyr::mutate(standard_concept = dplyr::recode(
        standard_concept,
        "S" = "Standard",
        "C" = "Classification"
      )) |>
      dplyr::filter(!(standard_concept == "Standard"))

    # Add non-standard concept info to vectors
    ind <- which(!(joined$standard_concept %in% c("S")))
    nonStandard <- append(nonStandard, joined$concept_id[ind])
    conceptNameNonStandard <- append(conceptNameNonStandard, joined$concept_name[ind])
    sourceCodeNonStandard <- append(sourceCodeNonStandard, joined$sourceCode[ind])
    sourceTableNonStandard <- append(
      sourceTableNonStandard,
      replicate(length(ind), table_name, simplify = "vector")
    )
    standard_concept <- append(standard_concept, joined$standard_concept[ind])

    # Save if not empty and save_path is provided
    if (!is.null(save_path) && nrow(joined) > 0) {
      message(paste0("saving file: ", table_name))
      readr::write_csv(joined, paste0(save_path, "/", table_name))
    } else if (is.null(save_path)) {
      next
    } else {
      message(paste("No matches found for concept set.\n"))
    }
  }

  # Create table of non-standard concepts
  res <- tibble::tibble(
    concept_id = nonStandard,
    concept_name = conceptNameNonStandard,
    source_code = sourceCodeNonStandard,
    source_table = unlist(sourceTableNonStandard),
    standard_concept = standard_concept
  )
  message(paste0("Finished checking for non-standard concepts.\n", nrow(res), " non-standard concepts found across tables."))

  return(res)
}
