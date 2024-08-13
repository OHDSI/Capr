#' Check for Non-Standard Concepts in Concept Sets
#'
#' This function examines a concept set for non-standard concepts by comparing them against a standard concepts table in a SQL database. It identifies non-standard concepts and optionally saves the details of these non-standard concepts to a specified path.
#'
#' @param db_connection A DBI database connection object to the SQL database containing the standard concepts table. This parameter is currently not used in the function but intended for future use where database queries might be necessary.
#' @param conceptSet An object representing a set of concepts, containing details such as concept ID, concept name, and whether it is a standard concept. The structure of this object should support `@Expression` to access the individual concepts and their properties.
#' @param save_path (Optional) The file path where the details of non-standard concepts should be saved as a CSV file. If not provided, the information will not be saved but will still be checked for non-standard concepts.
#'
#' @return A tibble containing the columns `concept_name`, `concept_id`, `concept_set`, and `standard`, which represent the concept name, concept ID, the name of the concept set, and the standard status for each concept found to be non-standard or not classified as standard.
#'
#' @details The function processes a given concept set to identify non-standard concepts. Non-standard concepts are those not marked as 'S' (Standard) or 'C' (Classification) in their `standard_concept` attribute. The function creates a data frame with concept details, filters out standard and classification concepts, and if a `save_path` is provided, saves this information to a CSV file. If no non-standard concepts are found, appropriate messages are displayed.
#'
#' @examples
#' # Assuming you have a concept set `conceptSet`:
#' non_standard_concepts <- isStandardCS(db_conn, conceptSet, "path/to/save_standard_AND_non_standard/")
#'
#' @export
isStandardCS <- function(conceptSet, save_path = NULL) {
  # Initialize vectors for non-standard concepts
  nonStandard <- c()
  conceptNameNonStandard <- c()
  sourceCodeNonStandard <- c()
  sourceTableNonStandard <- c()
  standard_concept <- c()

  # Get concept set details
  cs <- conceptSet@Expression

  # initialize vectors
  concept_name <- c()
  concept_id <- c()
  concept_set <- c()
  standard_concept <- c()

  for (concept in cs) {
    concept_name <- append(concept_name, concept@Concept@concept_name)
    concept_id <- append(concept_id, concept@Concept@concept_id)
    standard_concept <- append(standard_concept, concept@Concept@standard_concept)
  }
  cs_name <- conceptSet@Name
  concept_set <- rep.int(cs_name, length(concept_id))

  # Replace NAs with non-standard
  standard_concept[standard_concept == ""] <- "Non-standard"

  # Filter out standard and classification concepts; keep non-standard and NA
  df <- data.frame(
    concept_name,
    concept_id,
    concept_set,
    standard_concept
  )

  # Save if not empty and save_path is provided
  if (!is.null(save_path) && nrow(df) > 0) {
    message(paste0("saving file: CONCEPTSET_", cs_name))
    readr::write_csv(df, paste0(save_path, "/CONCEPTSET_", cs_name, ".csv"))
  } else if (is.null(save_path)) {
    message("No save path specified; returning non-standard concepts\n")
  } else {
    message(paste0("No matches found for concept set: ", cs_name, "\n"))
  }


  # NonStandard concepts
  res <- df |>
    dplyr::filter(standard_concept != "S" | is.na(standard_concept)) |>
    tibble::tibble()
  if (nrow(res) == 0) {
    message("No non-standard concepts found in concept set: ", cs_name)
  } else {
    message(paste0("Found ", nrow(res), " non-standard concepts in concept set: ", cs_name))
  }
  return(res)
}
