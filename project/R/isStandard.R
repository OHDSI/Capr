library(readr)
library(dplyr)

isStandard <- function(concept_table_path, data_concepts_path, save_path = NULL) {

# Filters CONCEPT.csv from Athena vocabulary download for included concepts per
# a table of source codes and provided concepts. The accepted format is CSV files with
# at least fields 'sourceCode' to store source codes or source terms and 'concept_id'
# to store concept_ids.
# These tables are read from data_concepts_path.
# 
# If a save_path is provided, results are written as filtered versions of the CONCEPT
# table to the directory save_path points to; with one table for each provided table 
# of concepts. It is recommended to provide one table of concepts per source table.
# If no save_path is provided, results are not saved.
# 
# The function will always return a tibble of non-standard concepts
# that can be inspected in the R environment or directly printed to the console.
# 
# Arguments:
#   concept_table_path:
#     path to CONCEPT.csv from Athena vocabulary download
#     
#   data_concepts_path:
#     path to directory of CSVs with fields sourceCode and concept_id
#     see ./project/data/phems_variable/list/*.csv for examples
#     
#   save_path:
#     path to directory to save filtered concept table with only included concepts
#     ! Can be null if write_tables = FALSE
#     
#   write_tables:
#     Boolean toggle for whether to save the results

  
  #  Set working directory
  path <- rstudioapi::getSourceEditorContext()$path %>%
    dirname() %>%
    dirname() %>%
    dirname()
  original_wd <- getwd()
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

    # Add non-standard concept info to vectors
    ind <- which(!(joined$standard_concept %in% c('S', 'C')))
    nonStandard <- append(nonStandard, joined$concept_id[ind])
    conceptNameNonStandard <- append(conceptNameNonStandard, joined$concept_name[ind])
    sourceCodeNonStandard <- append(sourceCodeNonStandard, joined$sourceCode[ind])
    sourceTableNonStandard <- append(sourceTableNonStandard, 
                                     replicate(length(ind), table_name, simplify="vector"))

    # Save if not empty
    if (!(is.null(save_path))) {
      if(nrow(joined) > 0) {
        message(paste("saving file: ", table_name))
        write_csv(joined, paste0(save_path, table_name))
      } else {
        message(paste("No matches found for table:", table_name, "\n"))
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
  
  # reset working directory
  setwd(original_wd)
  return(res)
}
