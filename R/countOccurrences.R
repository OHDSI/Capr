#' Count Occurrences of Values in Database Tables
#'
#' This function counts the occurrences of specified concept ids within specified tables in a OMOP database, 
#' including direct occurrences and occurrences through descendants, based on a provided cdm_schema and links.
#' It returns a tibble summarizing the counts across persons and across records.
#'
#' @param v A vector of concept_ids to count occurrences for.
#' @param tables A character vector of CDM table names to search within.
#' @param links A list linking each table to its respective concept_id field.
#' @param db_connection A database connection object through which queries will be executed.
#' @param cdm_schema The database cdm_schema in which the tables are located.
#' @param vocab_schema The database vocab_schema in which the concept tables are located.
#' @param save_path The path to save the results to. If NULL, the results are not saved.
#'
#' @return A tibble with columns for the number of times any concept from 'v' occurs: direct count of persons, 
#'         direct count of records, descendant count of persons, and descendant count of records. 
#'         The tibble also includes the concept names derived from `v` and is arranged by the total 
#'         record count (direct + descendant).
#'
#' @examples
#' # Assuming `db_connection` is a valid database connection, `cdm_schema` is set to "public",
#' # `tables` contains the names of the tables to search, `links` defines the relevant fields,
#' # and `v` contains the values to search for:
#' results <- countOccurrences(v = c(1, 2), tables = c("observation", "condition_occurrence"), 
#'                             links = list(observation = "observation_concept_id", condition_occurrence = "condition_concept_id", ...), 
#'                             db_connection = db_connection, cdm_schema = "public")
#'
#' @export
countOccurrences <- function(v, tables, links, db_connection, cdm_schema, vocab_schema, save_path = NULL) {
  library(DBI)
  library(dplyr)
  library(tibble)

  stopifnot(is.vector(v))
  stopifnot(is.character(tables) & is.vector(tables))
  stopifnot(is.list(links))
  stopifnot(is.character(cdm_schema))
  
  results <- list()
  
  for (table in tables) {
    concept_id_field <- links[[table]]
    
    # Combined SQL query for direct and descendant counts
    combined_sql <- sprintf(
      # WITH clause to define common table expressions (CTEs)
      "WITH direct_counts AS (
        -- Select the concept_id and count the distinct persons and total records for each concept_id
        SELECT %s AS concept_id, COUNT(DISTINCT person_id) AS count_persons, COUNT(*) AS count_records
        -- From the cdm schema and the specified table
        FROM %s.%s
        -- Where the concept_id is in the provided vector
        WHERE %s IN (%s)
        -- Group by the concept_id
        GROUP BY %s
        -- Define the second expression for descendants
      ), desc_counts AS (
        -- Select the ancestor_concept_id and count the distinct persons and total records for each concept_id
        SELECT b.ancestor_concept_id AS concept_id, COUNT(DISTINCT a.person_id) AS desc_count_person, COUNT(*) AS desc_count_record
        -- From the vocab schema and concept ancestor table (vocab schema must contain concept_ancestor table; this can be the same schema as the cdm)
        FROM %s.%s a
        -- Join the concept_ancestor table to get the ancestor_concept_id
        JOIN %s.concept_ancestor b ON a.%s = b.descendant_concept_id
        -- Where the descendant_concept_id is in the provided vector
        WHERE b.ancestor_concept_id IN (%s)
        -- Group by the ancestor_concept_id
        GROUP BY b.ancestor_concept_id
      )
      -- Combine the direct and descendant counts into one result set
      SELECT coalesce(d.concept_id, dc.concept_id) AS concept_id, coalesce(count_persons, 0) AS count_persons, coalesce(count_records, 0) AS count_records, coalesce(desc_count_person, 0) AS desc_count_person, coalesce(desc_count_record, 0) AS desc_count_record
      FROM direct_counts d
      FULL OUTER JOIN desc_counts dc ON d.concept_id = dc.concept_id",
      concept_id_field, cdm_schema, table, concept_id_field, paste(v, collapse = ","), concept_id_field,
      cdm_schema, table, vocab_schema, concept_id_field, paste(v, collapse = ",")
    )
    
    combined_res <- dbGetQuery(db_connection, combined_sql)

    not_in_data <- v[!(v %in% combined_res$concept_id)]
    combined_res <- combined_res |>
      bind_rows(tibble(
        concept_id = not_in_data,
        count_persons = 0,
        count_records = 0,
        desc_count_person = 0,
        desc_count_record = 0
       ))
    
    # Append results
    results[[table]] <- combined_res
  }
  
  # Combine all results into a single data frame and transform
  final_res <- bind_rows(results) %>%
    group_by(concept_id) %>%
    summarise(
      count_persons = sum(count_persons),
      count_records = sum(count_records),
      desc_count_person = sum(desc_count_person),
      desc_count_record = sum(desc_count_record)
    ) %>%
    ungroup() %>%
    mutate(concept_name = names(v)[match(concept_id, v)]) %>%
    arrange(desc(count_records + desc_count_record))

    if (!is.null(save_path)) {
      readr::write_csv(final_res, paste0(save_path, '/', 'count_occurrences.csv'))
    }
  
  return(final_res)
}