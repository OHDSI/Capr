library(readr)
library(dplyr)

isStandard <- function(concept_table_path, data_concepts_path, save_path, write_tables = FALSE) {
  # Set working directory
  path <- rstudioapi::getSourceEditorContext()$path %>%
    dirname() %>%
    dirname() %>%
    dirname()
  setwd(path)

  # Read concept table
  concept_table <- read_delim(concept_table_path,
                              delim = '\t',
                              col_types = cols(concept_id = col_character())) %>%
    mutate(concept_id = as.character(concept_id)) %>% # Ensure concept_id is character
    mutate(concept_id = tolower(trimws(concept_id)))
  
  # Initialize vector of non-standard (or not in vocabularies) concept ids
  nonStandard <- c()
  # Initialize vector of non-standard concept names
  conceptNameNonStandard <- c()
  # Initialize vector of source codes/terms for non-standard concepts
  sourceCodeNonStandard <- c()
  # Initialize vector of source tables for non-standard concepts
  sourceTableNonStandard <- c()

  # Get tables
  tables <- list.files(path = data_concepts_path, pattern = "\\.csv$", full.names = TRUE)
  for (table_path in tables) {
    table_name <- basename(table_path)

    # Read and prepare table
    tb <- read_csv(table_path, col_types = cols(sourceCode = col_character(), concept_id = col_character())) %>%
      mutate(across(c(sourceCode, concept_id), ~gsub("\u00A0", " ", .))) %>% # Replace non-breaking space with regular space
      mutate(across(c(sourceCode, concept_id), ~trimws(.))) %>%
      filter(!is.na(sourceCode), !is.na(concept_id)) %>%
      mutate(concept_id = tolower(concept_id),
            concept_id = as.character(concept_id))

    # Join tables
    joined <- inner_join(concept_table, tb, by = "concept_id")

    # Add non-standard concepts to vector
    ind <- which(!(joined$standard_concept %in% c('S', 'C')))
    nonStandard <- append(nonStandard, joined$concept_id[ind])
    conceptNameNonStandard <- append(conceptNameNonStandard, joined$concept_name[ind])
    sourceCodeNonStandard <- append(sourceCodeNonStandard, joined$sourceCode[ind])
    sourceTableNonStandard <- append(sourceTableNonStandard, 
                                     replicate(length(ind), table_name, simplify="vector"))

    # Save if not empty
    if (write_tables == TRUE) {
      if(nrow(joined) > 0) {
        write_csv(joined, paste0(save_path, table_name))
      } else {
        cat("No matches found for table:", table_name, "\n")
      }
    }
  }
  # Create table of non-standard concepts
  res <- tibble::tibble(
    concept_id = nonStandard,
    concept_name = conceptNameNonStandard,
    source_code = sourceCodeNonStandard,
    source_table = sourceTableNonStandard
  )
  return(res)
}
