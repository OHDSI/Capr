#' Identify Non-Standard Concepts in Database
#'
#' This function checks a set of tables for non-standard concepts by comparing them against the concept table in a SQL database. It identifies non-standard concepts and optionally saves the joined tables containing these non-standard concepts.
#'
#' @param db_connection A DBI database connection object to the SQL database containing the standard concepts table.
#' @param cdm_schema The schema name of the Common Data Model (CDM) database containing the source tables.
#' @param vocab_schema The schema name of the vocabulary database containing the standard concepts table.
#' @param links A named list where each name is a table name and each value is a vector of two strings: the column names for `concept_id` and `source_code` in that table.
#' @param save_path (Optional) The file path where the joined tables containing non-standard concepts should be saved. If not provided, the tables will not be saved but will still be checked for non-standard concepts.
#'
#' @return A tibble containing the columns `concept_id`, `concept_name`, `source_code`, and `source_table`, representing the concept ID, concept name, source code, and source table name for each non-standard concept found.
#'
#' @details The function first queries the `concept` table from the provided SQL database to retrieve the standard concepts. It then reads each specified table, preparing and joining it with the standard concepts table based on the concept ID. Non-standard concepts are identified based on the `standard_concept` column being `NA`. Information about these non-standard concepts is collected and returned as a tibble. The function also adds a `source_table` column to indicate the source table name.
#'
#' If a `save_path` is provided, each joined table (including both standard and non-standard concepts) is saved to the specified directory with the same name as the original table file.
#'
#' @examples
#' # Assuming you have a valid DBI connection `db_conn` and your tables are specified in the `links` list:
#' links <- list(
#'   "table1" = c("concept_id_col1", "source_code_col1"),
#'   "table2" = c("concept_id_col2", "source_code_col2")
#' )
#' non_standard_concepts <- isStandardDB(db_conn, "cdm_schema", "vocab_schema", links, "path/to/save_non_standard/")
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr mutate filter rename bind_rows
#' @importFrom DatabaseConnector querySql
#' @importFrom SqlRender render translate
#' @export
isStandardDB <- function(db_connection, cdm_schema, vocab_schema, links, save_path = NULL) {
  # get tables
  tables <- names(links)
  
  for (table in tables) {
    # Read concept table from SQL database
    concept_table_query <- SqlRender::render(sprintf(
      "
        WITH source_table AS (
        -- Select the unique concept_id and source code from the specified source table
        SELECT DISTINCT %s AS concept_id, %s AS source_code
        FROM %s.%s
        )
        SELECT a.concept_id, a.concept_name, source_table.source_code, a.standard_concept
        FROM %s.concept a
        JOIN source_table ON a.concept_id = source_table.concept_id
        ORDER BY source_table.concept_id
      ",
      links[[table]][1], links[[table]][2], cdm_schema, table, vocab_schema)
    )
    
    concept_table_query_translated <- SqlRender::translate(
      sql = concept_table_query,
      targetDialect = attr(db_connection, "dbms")
    )
    
    concept_table <- DatabaseConnector::querySql(db_connection, concept_table_query_translated) |>
      dplyr::rename(
        concept_id = CONCEPT_ID,
        concept_name = CONCEPT_NAME,
        source_code = SOURCE_CODE,
        standard_concept = STANDARD_CONCEPT) |>
      dplyr::mutate(concept_id = as.character(concept_id)) |>
      dplyr::mutate(concept_id = tolower(trimws(concept_id)))
    concept_table["source_table"] <- rep.int(table, nrow(concept_table))
    nonStandardDF <- concept_table |> dplyr::filter(is.na(standard_concept))
    
    # Initialize res on first loop iteration
    if (!exists("res")) {
      res <- nonStandardDF
    } else {
      res <- dplyr::bind_rows(res, nonStandardDF)
    }
  }
    
    # Save if not empty and save_path is provided
    if (!is.null(save_path) && nrow(res) > 0) {
      readr::write_csv(concept_table, paste0(save_path, "/", table))
      message(paste0("Saved results to ", save_path, "/", table, "\n"))
    } else if (is.null(save_path)) {
      message("No save path provided, only returning non-standard concepts DF")
    } else if (nrow(res) == 0) {
      message("No non-standard concepts found in DB; not saving any file.\n")
    }

  message(paste0("Finished checking for non-standard concepts.\n", nrow(res), " non-standard concepts found across tables."))

  return(res)
}
