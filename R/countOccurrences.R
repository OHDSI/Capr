#' This function counts the occurrences of specified concept ids within specified tables in an OMOP database,
#' including direct occurrences and occurrences through descendants, based on a provided cdm_schema and links.
#' It returns a tibble summarizing the counts across persons and across records.
#'
#' @param v A vector of concept_ids to count occurrences for.
#' @param tables A character vector of CDM table names to search within.
#' @param links A list linking each table to its respective concept_id field.
#' @param db_connection A database connection object through which queries will be executed.
#' @param cdm_schema The database cdm_schema in which the tables are located.
#' @param vocab_schema The database vocab_schema in which the concept tables are located.
#' @param save_path The path to save the results to as a CSV file. If NULL, the results are not saved.
#'
#' @return A tibble with columns for the number of times any concept from 'v' occurs: direct count of persons,
#'         direct count of records, descendant count of persons, and descendant count of records.
#'         The tibble also includes the concept names and domain ids derived from `v` and is arranged by the total
#'         record count (direct + descendant).
#'
#' @examples
#' # Assuming `db_connection` is a valid database connection, `cdm_schema` is set to "public",
#' # `tables` contains the names of the tables to search, `links` defines the relevant fields,
#' # and `v` contains the values to search for:
#' results <- countOccurrences(
#'   v = c(1, 2), tables = c("observation", "condition_occurrence"),
#'   links = list(observation = "observation_concept_id", condition_occurrence = "condition_concept_id"),
#'   db_connection = db_connection, cdm_schema = "public", vocab_schema = "vocabulary"
#' )
#'
#' @export
countOccurrences <- function(v, tables, links, db_connection, cdm_schema, vocab_schema, save_path = NULL) {
  stopifnot(is.vector(v))
  stopifnot(is.character(tables) & is.vector(tables))
  stopifnot(is.list(links))
  stopifnot(is.character(cdm_schema))

  results <- list()

  # Create a temporary table for concept IDs
  temp_query <- "CREATE TEMPORARY TABLE temp_concepts (concept_id INTEGER)"
  temp_query_translated <- SqlRender::translate(sql = temp_query, targetDialect = attr(db_connection, "dbms"))
  DatabaseConnector::dbExecute(db_connection, temp_query_translated)
  temp_query_insert <- sprintf("INSERT INTO temp_concepts (concept_id) VALUES %s", paste0("(", v, ")", collapse = ", "))
  temp_query_insert_translated <- SqlRender::translate(sql = temp_query_insert, targetDialect = attr(db_connection, "dbms"))
  DatabaseConnector::dbExecute(db_connection, temp_query_insert_translated)

  for (table in tables) {
    concept_id_field <- links[[table]][1]

    # Combined SQL query for direct and descendant counts
    combined_sql <- sprintf(
      "WITH direct_counts AS (
        SELECT %s AS concept_id, COUNT(DISTINCT person_id) AS count_persons, COUNT(*) AS count_records
        FROM %s.%s
        WHERE %s IN (SELECT concept_id FROM temp_concepts)
        GROUP BY %s
      ), desc_counts AS (
        SELECT b.ancestor_concept_id AS concept_id, COUNT(DISTINCT a.person_id) AS desc_count_person, COUNT(*) AS desc_count_record
        FROM %s.%s a
        JOIN %s.concept_ancestor b ON a.%s = b.descendant_concept_id
        WHERE b.ancestor_concept_id IN (SELECT concept_id FROM temp_concepts)
        GROUP BY b.ancestor_concept_id
      )
      SELECT c.concept_id, coalesce(d.count_persons, 0) AS count_persons, coalesce(d.count_records, 0) AS count_records, coalesce(dc.desc_count_person, 0) AS desc_count_person, coalesce(dc.desc_count_record, 0) AS desc_count_record,
             coalesce(concept.concept_name, '') AS concept_name, coalesce(concept.domain_id, '') AS domain_id
      FROM temp_concepts c
      LEFT JOIN direct_counts d ON c.concept_id = d.concept_id
      LEFT JOIN desc_counts dc ON c.concept_id = dc.concept_id
      LEFT JOIN %s.concept concept ON c.concept_id = concept.concept_id",
      concept_id_field, cdm_schema, table, concept_id_field, concept_id_field,
      cdm_schema, table, vocab_schema, concept_id_field,
      vocab_schema
    )
    
    sql_translated <- SqlRender::translate(sql = combined_sql, targetDialect = attr(db_connection, "dbms"))
    combined_res <- DatabaseConnector::dbGetQuery(db_connection, sql_translated)

    ind_not_in_data <- which(!(v %in% combined_res$concept_id))
    combined_res <- combined_res |>
      bind_rows(tibble(
        concept_id = v[ind_not_in_data],
        concept_name = "Unknown; concept_id not found in data",
        domain_id = "Unknown; concept_id not found in data",
        count_persons = 0,
        count_records = 0,
        desc_count_person = 0,
        desc_count_record = 0
      )) |>
      dplyr::filter(!is.na(concept_id))

    # Append results
    results[[table]] <- combined_res
  }

  # Combine all results into a single data frame and transform
  final_res <- bind_rows(results) |>
    group_by(concept_id, concept_name, domain_id) |>
    summarise(
      count_persons = sum(count_persons),
      count_records = sum(count_records),
      desc_count_person = sum(desc_count_person),
      desc_count_record = sum(desc_count_record)
    ) |>
    ungroup() |>
    arrange(desc(desc_count_record))

  if (!is.null(save_path)) {
    readr::write_csv(final_res, paste0(save_path, "/", "count_occurrences.csv"))
  }

  return(final_res)
}
