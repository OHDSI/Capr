#' @name table_linked_to_concept_field
#' @title Link Table Names to Respective Concept ID Fields
#' 
#' @description
#' This script creates a list mapping various table names to their respective
#' concept ID fields. This mapping is useful for dynamically referencing
#' concept IDs across different tables in a database schema, particularly
#' within the context of healthcare data where tables represent different
#' aspects of patient information (e.g., conditions, drug exposures).
#'
#' @details The `links` list object contains key-value pairs where each key is
#' a table name and the corresponding value is the name of the field that
#' contains the concept ID within that table. This object can be used to
#' programmatically access concept ID fields by table name.
#'
#' The following tables and their concept ID fields are included:
#' - `condition_occurrence`: `condition_concept_id`
#' - `death`: `cause_concept_id`
#' - `device_exposure`: `device_concept_id`
#' - `drug_exposure`: `drug_concept_id`
#' - `measurement`: `measurement_concept_id`
#' - `observation`: `observation_concept_id`
#' - `procedure_occurrence`: `procedure_concept_id`
#' - `specimen`: `specimen_concept_id`
#' - `visit_occurrence`: `visit_concept_id`
#'
#' @examples
#' # Access the concept ID field for the condition_occurrence table
#' concept_field <- links$condition_occurrence
#' print(concept_field)
#'
#' @export
links <- list(
  condition_occurrence = c("condition_concept_id", "condition_source_value"),
  death = c("cause_concept_id", "cause_source_value"),
  device_exposure = c("device_concept_id", "device_source_value"),
  drug_exposure = c("drug_concept_id", "drug_source_value"),
  measurement = c("measurement_concept_id", "measurement_source_value"),
  observation = c("observation_concept_id", "observation_source_value"),
  procedure_occurrence = c("procedure_concept_id", "procedure_source_value"),
  specimen = c("specimen_concept_id", "specimen_source_value"),
  visit_occurrence = c("visit_concept_id", "visit_source_value")
)

cat("Sourced links object\n")