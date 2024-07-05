countOccurrences <- function(v, tables, list_of_df, links) {
  stopifnot(is.vector(v))
  stopifnot(is.character(tables) & is.vector(tables))
  stopifnot(is.list(list_of_df))
  stopifnot(is.list(links))
  
  if (!any(tables %in% names(links))) {
    stop("Table is not one of the tables listed underneath with linked concept_id fields, \nPlease provide a different table or update the table -- concept_id links. \nTerminating")
  }
  
  unique_person_ids <- vector("list", length(v))
  records <- numeric(length(v))
  records_descendant <- numeric(length(v))  
  unique_person_ids_descendant <- vector("list", length(v))
  
  for (table in tables) {
    df <- list_of_df[[table]]
    concept_id_field <- links[[table]]
    if (!all(c("person_id", concept_id_field) %in% names(df))) {
      stop("Data frame does not contain expected columns.")
    }
    anc <- list_of_df$concept_ancestor$ancestor_concept_id
    desc <- list_of_df$concept_ancestor$descendant_concept_id
    
    if (!all(c("ancestor_concept_id", "descendant_concept_id") %in% names(list_of_df$concept_ancestor))) {
      stop("Concept ancestor data frame does not contain expected columns.")
    }
    
    for (i in seq_along(v)) {
      val <- v[i]
      direct_df <- df[df[, concept_id_field] == val, ]
      desc_val <- c(anc[which(desc == val)], desc[which(anc == val)]) %>% unique()
      descendant_df <- df[df[, concept_id_field] %in% desc_val, ]
      
      # Counting total records directly
      if (nrow(direct_df) > 0) {
        unique_person_ids[[i]] <- unique(c(unique_person_ids[[i]], direct_df$person_id))
        records[i] <- records[i] + nrow(direct_df)
      }
      
      # Counting total records for descendants
      if (nrow(descendant_df) > 0) {
        unique_person_ids_descendant[[i]] <- unique(c(unique_person_ids_descendant[[i]], descendant_df$person_id))
        records_descendant[i] <- records_descendant[i] + nrow(descendant_df)
      }
    }
  }
  
  count_persons <- sapply(unique_person_ids, length)
  count_records <- records
  descendant_count_person <- sapply(unique_person_ids_descendant, length)
  descendant_count_record <- records_descendant
  
  res <- tibble::tibble(
    concept_name = names(v),
    concept_id = v,
    count_persons = count_persons,
    count_records = count_records,
    descendant_count_person = descendant_count_person,
    descendant_count_record = descendant_count_record,
    included = (count_records + descendant_count_record) > 0
  ) %>%
    dplyr::arrange(dplyr::desc(count_records + descendant_count_record))
  
  return(res)
}