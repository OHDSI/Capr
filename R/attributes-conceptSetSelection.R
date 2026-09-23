# ConceptSetSelection Attribute --------------------------------------------------
#
# Corresponds to circe-be ConceptSetSelection.java: { CodesetId, IsExclusion }
# Used for the *TypeCS fields on domain criteria, and GenderCS/RaceCS/EthnicityCS
# in DemographicCriteria.
#
# Extends conceptSetAttribute so collectGuid, replaceCodesetId, and listConceptSets
# all work via inheritance; only as.list is overridden to include IsExclusion.

#' @include attributes-concept.R
NULL

# S4 Class -------

setClass("conceptSetSelectionAttribute",
  contains = "conceptSetAttribute",
  slots = c(isExclusion = "logical"),
  prototype = list(isExclusion = FALSE)
)

# Coercion -------

setMethod("as.list", "conceptSetSelectionAttribute", function(x) {
  nm <- x@name
  tibble::lst(`:=`(!!nm, list(CodesetId = x@conceptSet@id, IsExclusion = x@isExclusion)))
})

# Internal constructor -------

newConceptSetSelection <- function(conceptSet, name, isExclusion = FALSE) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetSelectionAttribute",
    name = name, conceptSet = conceptSet, isExclusion = as.logical(isExclusion)[1L])
}

# User-facing constructors -------

#' ConditionType filter via concept set (ConceptSetSelection)
#' @param conceptSet A \code{ConceptSet} built with \code{cs()}.
#' @param isExclusion logical; if \code{TRUE} the types in the concept set are excluded.
#' @return An attribute for use in \code{conditionOccurrence()}.
#' @export
conditionTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "ConditionTypeCS", isExclusion)
}

#' DrugType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{drugExposure()}.
#' @export
drugTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "DrugTypeCS", isExclusion)
}

#' MeasurementType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{measurement()}.
#' @export
measurementTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "MeasurementTypeCS", isExclusion)
}

#' ObservationType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{observation()}.
#' @export
observationTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "ObservationTypeCS", isExclusion)
}

#' ProcedureType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{procedure()}.
#' @export
procedureTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "ProcedureTypeCS", isExclusion)
}

#' DeviceType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{deviceExposure()}.
#' @export
deviceTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "DeviceTypeCS", isExclusion)
}

#' DeathType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{death()}.
#' @export
deathTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "DeathTypeCS", isExclusion)
}

#' SpecimenType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{specimen()}.
#' @export
specimenTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "SpecimenTypeCS", isExclusion)
}

#' VisitType filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{visit()}.
#' @export
visitTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "VisitTypeCS", isExclusion)
}

#' ObservationPeriod period type filter via concept set (ConceptSetSelection)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{observationPeriod()}.
#' @export
periodTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "PeriodTypeCS", isExclusion)
}

#' Gender filter via concept set (ConceptSetSelection, for demographic criteria)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{withAll()}/\code{withAny()} attrition groups.
#' @export
genderCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "GenderCS", isExclusion)
}

#' Race filter via concept set (ConceptSetSelection, for demographic criteria)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{withAll()}/\code{withAny()} attrition groups.
#' @export
raceCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "RaceCS", isExclusion)
}

#' Ethnicity filter via concept set (ConceptSetSelection, for demographic criteria)
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{withAll()}/\code{withAny()} attrition groups.
#' @export
ethnicityCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "EthnicityCS", isExclusion)
}

#' Unit filter via concept set — Measurement, Observation, DoseEra, Specimen
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{measurement()}, \code{observation()}, \code{doseEra()}, \code{specimen()}.
#' @export
unitCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "UnitCS", isExclusion)
}

#' Dose unit filter via concept set — DrugExposure
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{drugExposure()}.
#' @export
doseUnitCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "DoseUnitCS", isExclusion)
}

#' Route concept filter via concept set — DrugExposure
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{drugExposure()}.
#' @export
routeConceptCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "RouteConceptCS", isExclusion)
}

#' Measurement operator filter via concept set
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{measurement()}.
#' @export
measurementOperatorCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "OperatorCS", isExclusion)
}

#' Observation qualifier filter via concept set
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{observation()}.
#' @export
observationQualifierCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "QualifierCS", isExclusion)
}

#' Procedure modifier filter via concept set
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{procedure()}.
#' @export
procedureModifierCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "ModifierCS", isExclusion)
}

#' Place of service filter via concept set — VisitOccurrence, VisitDetail
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{visit()} or \code{visitDetail()}.
#' @export
placeOfServiceCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "PlaceOfServiceCS", isExclusion)
}

#' Provider specialty filter via concept set — multiple domains
#' @inheritParams conditionTypeCS
#' @return An attribute for use in query functions that support provider specialty.
#' @export
providerSpecialtyCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "ProviderSpecialtyCS", isExclusion)
}

#' Condition status filter via concept set — ConditionOccurrence
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{conditionOccurrence()}.
#' @export
conditionStatusCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "ConditionStatusCS", isExclusion)
}

#' Specimen anatomic site filter via concept set
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{specimen()}.
#' @export
specimenAnatomicSiteCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "AnatomicSiteCS", isExclusion)
}

#' Specimen disease status filter via concept set
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{specimen()}.
#' @export
specimenDiseaseStatusCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "DiseaseStatusCS", isExclusion)
}

#' Visit detail type filter via concept set — VisitDetail
#' @inheritParams conditionTypeCS
#' @return An attribute for use in \code{visitDetail()}.
#' @export
visitDetailTypeCS <- function(conceptSet, isExclusion = FALSE) {
  newConceptSetSelection(conceptSet, "VisitDetailTypeCS", isExclusion)
}
