# Link table names to respective concept_id field
links <- list(
  condition_occurrence = "condition_concept_id",
  death = "cause_concept_id",
  device_exposure = "device_concept_id",
  drug_exposure = "drug_concept_id",
  measurement = "measurement_concept_id",
  observation = "observation_concept_id",
  procedure_occurrence = "procedure_concept_id",
  specimen = "specimen_concept_id",
  visit_occurrence = "visit_concept_id"
)

cat("Sourced links object")