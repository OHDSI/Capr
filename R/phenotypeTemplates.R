# Phenotype Archetype Templates -----------------------------------------------
#
# These functions encode common clinical/epidemiological cohort patterns as
# reusable, parameterized building blocks. Each returns a `Cohort` object.
# Concept sets are the only required inputs; all design defaults are explicit
# and can be overridden with named parameters.
#
# They are designed for use by both humans and LLM coding agents as starting
# points for cohort generation. For definitions that diverge from all archetypes,
# compose a custom cohort() call from the package primitives instead.

#' Chronic condition cohort
#'
#' Build a cohort that captures every qualified condition occurrence per person,
#' exiting at each event's end date. Nearby episodes can be collapsed into eras
#' via `eraGapDays`. Matches the OHDSI Phenotype Library convention: all qualifying
#' events are preserved, era collapse merges adjacent episodes.
#'
#' @param conditionConceptSet A `ConceptSet` for the chronic condition.
#' @param washoutDays Minimum days of continuous observation required before
#'   each index event. Default 365.
#' @param exitOffsetDays Days added after each event's end date before the
#'   cohort episode exits. Default 0 (exit at event end).
#' @param eraGapDays Maximum gap in days between episodes to collapse into one
#'   era. Default 1 (merge back-to-back episodes).
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' htnCs <- cs(descendants(320128), name = "Hypertension")
#' htnCohort <- chronicOutcomeCohort(htnCs, washoutDays = 365L, eraGapDays = 180L)
#' }
chronicOutcomeCohort <- function(conditionConceptSet,
                          washoutDays = 365L,
                          exitOffsetDays = 0L,
                          eraGapDays = 1L) {
  cohort(
    entry = entry(
      conditionOccurrence(conditionConceptSet),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "All"
    ),
    attrition = attrition(expressionLimit = "All"),
    exit = exit(endStrategy = fixedExit(index = "endDate", offsetDays = exitOffsetDays)),
    era = era(eraDays = eraGapDays)
  )
}

#' First-ever condition cohort
#'
#' Build a cohort for the first-ever occurrence of a condition in a person's
#' history, with a minimum prior observation requirement. Useful for new-onset
#' or new-user designs where prevalent cases must be excluded.
#'
#' @param conditionConceptSet A `ConceptSet` for the condition.
#' @param washoutDays Minimum days of continuous observation required before
#'   the index event. Default 365.
#' @param eraGapDays Maximum gap in days between episodes to collapse into one
#'   era. Default 0 (no collapsing).
#'
#' @details
#' Uses `firstOccurrence()` on the entry query to select only the first recorded
#' instance of the condition per person. Combined with the observation window,
#' this produces the standard incident phenotype: "first recorded diagnosis with
#' at least N days of prior observation."
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' afibCs <- cs(descendants(313217), name = "Atrial Fibrillation")
#' newOnsetAfib <- firstEverDiagnosisCohort(afibCs, washoutDays = 365L)
#' }
firstEverDiagnosisCohort <- function(conditionConceptSet,
                           washoutDays = 365L,
                           eraGapDays = 0L) {
  cohort(
    entry = entry(
      conditionOccurrence(conditionConceptSet, firstOccurrence()),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "First"
    ),
    attrition = attrition(expressionLimit = "First"),
    exit = exit(endStrategy = observationExit()),
    era = era(eraDays = eraGapDays)
  )
}

#' Acute event cohort
#'
#' Build a cohort for an acute event: short-duration condition episodes exiting
#' a fixed number of days after each event's end date. Each qualifying event
#' enters the cohort independently (multiple episodes per person are preserved)
#' and no era collapsing is applied. Useful for events like myocardial infarction,
#' ischemic stroke, or UTI.
#'
#' @param conditionConceptSet A `ConceptSet` for the acute condition.
#' @param washoutDays Minimum days of continuous observation required before
#'   each index event. Default 180.
#' @param exitDays Days after each event's end date that the episode exits.
#'   Default 14 (matches OHDSI Phenotype Library convention for acute events).
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' miCs <- cs(descendants(4329847), name = "Myocardial Infarction")
#' miCohort <- acuteOutcomeCohort(miCs, washoutDays = 180L, exitDays = 14L)
#' }
acuteOutcomeCohort <- function(conditionConceptSet,
                        washoutDays = 180L,
                        exitDays = 14L) {
  cohort(
    entry = entry(
      conditionOccurrence(conditionConceptSet),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "All"
    ),
    attrition = attrition(expressionLimit = "All"),
    exit = exit(endStrategy = fixedExit(index = "endDate", offsetDays = exitDays)),
    era = era(eraDays = 0L)
  )
}

#' New user drug cohort
#'
#' Build a cohort for first-time users of a drug: the first recorded drug
#' exposure per person with a minimum prior observation requirement, exiting at
#' end of continuous drug exposure. Useful for new-user pharmacoepidemiology
#' designs.
#'
#' @param drugConceptSet A `ConceptSet` for the drug of interest.
#' @param washoutDays Minimum days of continuous observation required before
#'   the index event. Default 365.
#' @param persistenceWindow Maximum gap in days between drug records when
#'   building the continuous exposure era. Default 30.
#' @param surveillanceWindow Days added to the end of the exposure era before
#'   exit. Default 0.
#'
#' @details
#' Uses `firstOccurrence()` on the entry query to select only the first recorded
#' exposure per person. The exit strategy is `drugExit()`, so a person's cohort
#' episode extends from first exposure through the end of their continuous
#' exposure era.
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' metforminCs <- cs(descendants(1503297), name = "Metformin")
#' metforminUsers <- newUserDrugCohort(metforminCs, washoutDays = 365L)
#' }
newUserDrugCohort <- function(drugConceptSet,
                          washoutDays = 365L,
                          persistenceWindow = 30L,
                          surveillanceWindow = 0L) {
  cohort(
    entry = entry(
      drugExposure(drugConceptSet, firstOccurrence()),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "First"
    ),
    attrition = attrition(expressionLimit = "First"),
    exit = exit(endStrategy = drugExit(
      conceptSet = drugConceptSet,
      persistenceWindow = persistenceWindow,
      surveillanceWindow = surveillanceWindow
    )),
    era = era(eraDays = 0L)
  )
}

#' All drug exposures cohort
#'
#' Build a cohort that captures every qualifying drug exposure episode per
#' person, exiting at end of continuous observation. Multiple episodes per
#' person are preserved. Useful for prevalence or utilization studies.
#'
#' @param drugConceptSet A `ConceptSet` for the drug of interest.
#' @param washoutDays Minimum days of continuous observation required before
#'   each index event. Default 0 (no minimum observation requirement).
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' aceiCs <- cs(descendants(1335471), name = "ACE Inhibitors")
#' aceiExposures <- allDrugCohort(aceiCs, washoutDays = 0L)
#' }
allDrugCohort <- function(drugConceptSet,
                          washoutDays = 0L) {
  cohort(
    entry = entry(
      drugExposure(drugConceptSet),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "All"
    ),
    attrition = attrition(expressionLimit = "All"),
    exit = exit(endStrategy = observationExit()),
    era = era(eraDays = 0L)
  )
}

#' Measurement threshold cohort
#'
#' Build a cohort based on a measurement value threshold: first qualifying
#' measurement per person with a minimum prior observation requirement, exiting
#' at end of continuous observation.
#'
#' @param measurementConceptSet A `ConceptSet` for the measurement of interest.
#' @param valueFilter A value attribute (from `valueAsNumber()`, `rangeHigh()`,
#'   or `rangeLow()`) wrapping a comparison operator such as `gt(5.7)` or
#'   `lte(3.0)`.
#' @param unitConceptIds Optional integer vector of unit concept IDs to filter
#'   the measurement unit (e.g. `8554L` for percent). Use `NULL` to accept any
#'   unit.
#' @param washoutDays Minimum days of continuous observation required before
#'   the index event. Default 365.
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' hba1cCs <- cs(descendants(3004410), name = "HbA1c")
#' uncontrolled <- measurementCohort(
#'   hba1cCs,
#'   valueFilter = valueAsNumber(gt(6.5)),
#'   unitConceptIds = 8554L
#' )
#' }
measurementCohort <- function(measurementConceptSet,
                              valueFilter,
                              unitConceptIds = NULL,
                              washoutDays = 365L) {
  measAttrs <- list(valueFilter)
  if (!is.null(unitConceptIds)) {
    measAttrs <- c(measAttrs, list(measurementUnit(unitConceptIds)))
  }

  cohort(
    entry = entry(
      do.call(measurement, c(list(conceptSet = measurementConceptSet), measAttrs)),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "First"
    ),
    attrition = attrition(expressionLimit = "First"),
    exit = exit(endStrategy = observationExit()),
    era = era(eraDays = 0L)
  )
}

#' Procedure-based cohort
#'
#' Build a cohort that captures every qualified procedure occurrence per person,
#' exiting at each event's end date. Nearby episodes can be collapsed into eras
#' via `eraGapDays`.
#'
#' @param procedureConceptSet A `ConceptSet` for the procedure of interest.
#' @param washoutDays Minimum days of continuous observation required before
#'   each index event. Default 365.
#' @param exitOffsetDays Days added after each event's end date before the
#'   cohort episode exits. Default 0 (exit at event end).
#' @param eraGapDays Maximum gap in days between episodes to collapse into one
#'   era. Default 1 (merge back-to-back episodes).
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' cabgCs <- cs(descendants(4032243), name = "CABG")
#' cabgCohort <- procedureCohort(cabgCs, washoutDays = 365L)
#' }
procedureCohort <- function(procedureConceptSet,
                            washoutDays = 365L,
                            exitOffsetDays = 0L,
                            eraGapDays = 1L) {
  cohort(
    entry = entry(
      procedure(procedureConceptSet),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "All"
    ),
    attrition = attrition(expressionLimit = "All"),
    exit = exit(endStrategy = fixedExit(index = "endDate", offsetDays = exitOffsetDays)),
    era = era(eraDays = eraGapDays)
  )
}

#' Observation-based cohort
#'
#' Build a cohort that captures every qualified observation record per person,
#' exiting at each event's end date. Nearby episodes can be collapsed into eras
#' via `eraGapDays`.
#'
#' @param observationConceptSet A `ConceptSet` for the observation of interest.
#' @param washoutDays Minimum days of continuous observation required before
#'   each index event. Default 365.
#' @param exitOffsetDays Days added after each event's end date before the
#'   cohort episode exits. Default 0 (exit at event end).
#' @param eraGapDays Maximum gap in days between episodes to collapse into one
#'   era. Default 1 (merge back-to-back episodes).
#'
#' @return A `Cohort` object.
#' @export
#'
#' @examples
#' \dontrun{
#' smokingCs <- cs(descendants(4052676), name = "Smoking Status")
#' smokers <- observationCohort(smokingCs, washoutDays = 365L)
#' }
observationCohort <- function(observationConceptSet,
                              washoutDays = 365L,
                              exitOffsetDays = 0L,
                              eraGapDays = 1L) {
  cohort(
    entry = entry(
      observation(observationConceptSet),
      observationWindow = continuousObservation(priorDays = washoutDays),
      primaryCriteriaLimit = "All"
    ),
    attrition = attrition(expressionLimit = "All"),
    exit = exit(endStrategy = fixedExit(index = "endDate", offsetDays = exitOffsetDays)),
    era = era(eraDays = eraGapDays)
  )
}
