#' Count Occurrences of Values in Database Tables
#'
#' This function counts the occurrences of specified concept ids within specified tables in a OMOP database, 
#' including direct occurrences and occurrences through descendants, based on a provided schema and links.
#' It returns a tibble summarizing the counts across persons and across records.
#'
#' @param v A vector of concept_ids to count occurrences for.
#' @param tables A character vector of CDM table names to search within.
#' @param links A list linking each table to its respective concept_id field.
#' @param db_connection A database connection object through which queries will be executed.
#' @param schema The database schema in which the tables are located.
#'
#' @return A tibble with columns for the number of times any concept from 'v' occurs: direct count of persons, 
#'         direct count of records, descendant count of persons, and descendant count of records. 
#'         The tibble also includes the concept names derived from `v` and is arranged by the total 
#'         record count (direct + descendant).
#'
#' @examples
#' # Assuming `db_connection` is a valid database connection, `schema` is set to "public",
#' # `tables` contains the names of the tables to search, `links` defines the relevant fields,
#' # and `v` contains the values to search for:
#' results <- countOccurrences(v = c(1, 2), tables = c("observation", "condition_occurrence"), 
#'                             links = list(observation = "observation_concept_id", condition_occurrence = "condition_concept_id", ...), 
#'                             db_connection = db_connection, schema = "public")
#'
#' @export
countOccurrences <- function(v, tables, links, db_connection, schema) {
  library(DBI)
  library(dplyr)
  library(tibble)

  stopifnot(is.vector(v))
  stopifnot(is.character(tables) & is.vector(tables))
  stopifnot(is.list(links))
  stopifnot(is.character(schema))
  
  # Placeholder for results
  results <- list()
  
  for (table in tables) {
    concept_id_field <- links[[table]]
    
    for (i in seq_along(v)) {
      val <- v[i]
      
      # Direct counts SQL query
      direct_sql <- 
        sprintf(
          "SELECT COUNT(DISTINCT person_id) AS count_persons, COUNT(*) AS count_records FROM %s WHERE %s = %d",
          paste0(schema, ".", table),
          concept_id_field,
          val
        )
      direct_res <- dbGetQuery(db_connection, direct_sql)
      
      # Descendant counts SQL query
      descendant_sql <- sprintf(
        "SELECT COUNT(DISTINCT a.person_id) AS descendant_count_person, COUNT(*) AS descendant_count_record FROM %s a JOIN %s.concept_ancestor b ON a.%s = b.descendant_concept_id WHERE b.ancestor_concept_id = %d",
        paste0(schema, ".", table), # Reference the target table within the schema
        schema, # Explicitly reference the schema for the concept_ancestor table
        concept_id_field,
        val)
      descendant_res <- dbGetQuery(db_connection, descendant_sql)
      
      # Combine results
      results[[i]] <- tibble(
        concept_id = val,
        count_persons = direct_res$count_persons,
        count_records = direct_res$count_records,
        descendant_count_person = descendant_res$descendant_count_person,
        descendant_count_record = descendant_res$descendant_count_record
      )
    }
  }
  
  # Combine all results into a single data frame
  final_res <- bind_rows(results) %>%
    mutate(concept_name = names(v)) %>%
    arrange(desc(count_records + descendant_count_record))
  
  return(final_res)
}