# Function to count occurrences of each value in v within df$value, unique by person_id
countOccurrences <- function(v, tables, list_of_df, links) {
  ## Checks -----
  # Argument input types
  stopifnot(is.vector(v))  # v = vector of concepts in concept set
  stopifnot(is.character(tables) & is.vector(tables))  # table = string with CDM table name
  stopifnot(is.list(list_of_df))  # list_of_df = list of CDM tables as dataframes
  stopifnot(is.list(links))  # links = links between table names and concept_id fields
  
  # Check the provided table is compatible with links
  if (!any(tables %in% names(links))) {
    warning("Table is not one of the tables listed underneath with linked concept_id fields, \nPlease provide a different table or update the table -- concept_id links. \nTerminating")
    writeLines(names(links))
    break
  }
  
  ## Function -----
  counts_persons <- c()
  counts <- c()
  i <- 0
  for (table in tables) {
    i <- i + 1
    # Get table of choice as dataframe
    df <- list_of_df[[table]]
    
    # Get concept_id field
    concept_id_field <- links[[table]]
    
    # Ensure unique person_id for each value
    unique_df <- df[!duplicated(df[c("person_id", concept_id_field)]), ]
    
    # Initialize a named vector to store counts_persons
    tmp_p <- setNames(numeric(length(v)), as.character(v))
    tmp_t <- setNames(numeric(length(v)), as.character(v))
    
    # Count persons and occurrences
    for (val in v) {
      tmp_p[as.character(val)] <- sum(unique_df[, concept_id_field] == as.character(val))
      tmp_t[as.character(val)] <- sum(df[, concept_id_field] == as.character(val))
    }
    if (i == 1) {
      counts_persons <- tmp_p
      counts <- tmp_t
    } else {
      counts_persons <- counts_persons + tmp_p
      counts <- counts + tmp_t
    }
  }
  
  # Tibble
  res <- tibble::tibble(
    concept_name = names(v),
    concept_id = v,
    count_persons = counts_persons,
    count_occurrences = counts,
    included = c(counts > 0)
  ) %>% dplyr::arrange(dplyr::desc(counts))  # Arrange tibble descendingly according to counts
  
  return(res)
}

cat("Sourced countOccurrences function")