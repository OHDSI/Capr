library(DBI)
library(dplyr)
library(tibble)

countOccurrences <- function(v, tables, links, db_connection, schema) {
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