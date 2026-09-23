# Concept Set Attribute class ----------------------------

#' An S4 class for a concept set attribute that holds a reference to a concept set ID
#' @slot
#' name the name of the attribute
#' @slot
#' conceptSet a ConceptSet object that provides the ID reference
#' @include conceptSet.R
setClass("conceptSetAttribute",
         slots = c(name = "character",
                   conceptSet = "ConceptSet"),
  prototype = list(name = NA_character_, conceptSet = new("ConceptSet")))

setValidity("conceptSetAttribute", function(object) {
  stopifnot(is.character(object@name), length(object@name) == 1)
  TRUE
})

# Console Print ---------------

setMethod("show", "conceptSetAttribute", function(object) {
  cli::cat_bullet(paste("Capr Concept Set Attribute:", object@name, "- ID:", object@conceptSet@id), bullet = "sup_plus")
})

# Concept Attribute class ----------------------------

#' An S4 class for a concept attribute
#' @slot
#' name the name of the attribute
#' @slot
#' conceptSet a list representing the concepts for the attribute
#' @include conceptSet.R
setClass("conceptAttribute",
         slots = c(name = "character",
                   conceptSet = "list"  # TODO why is this a list and not a concept set object?
),
  prototype = list(name = NA_character_, conceptSet = list()))

setValidity("conceptAttribute", function(object) {
  # TODO check that each object in conceptSet list is of Concept Class
  stopifnot(is.list(object@conceptSet), is.character(object@name), length(object@name) == 1)
  TRUE
})

# Console Print ---------------

setMethod("show", "conceptAttribute", function(object) {

  tbl <- tibble::tibble(concept_id = purrr::map_int(object@conceptSet, ~as.integer(.x@concept_id)),
    concept_name = purrr::map_chr(object@conceptSet,
                                  ~as.character(.x@concept_name)), concept_code = purrr::map_chr(object@conceptSet,
      ~as.character(.x@concept_code)), domain_id = purrr::map_chr(object@conceptSet,
                                                                  ~as.character(.x@domain_id)),
    vocabulary_id = purrr::map_chr(object@conceptSet,
                                   ~as.character(.x@vocabulary_id)), concept_class_id = purrr::map_chr(object@conceptSet,
      ~as.character(.x@concept_class_id)))
  cli::cat_bullet(paste("Capr Concept Attribute:", object@name), bullet = "sup_plus")
  print(tbl)
})

# Constructors -------------

#' Add male attribute to a query
#'
#' @return
#' An attribute that can be used in a query function
#' @export
#'
#' @describeIn
#' attributes male demographic attribute
#'
#' @examples
#' # Create a cohort of males with Type 1 diabetes
#' t1dm <- cs(descendants(201254, 435216, 40484648), name = "type 1 diabetes")
#' t1dm_males <- cohort(conditionOccurrence(t1dm, male()))
male <- function() {

  methods::new("conceptAttribute",
               name = "Gender",
               conceptSet = list(methods::new("Concept", concept_id = 8507L,
    concept_name = "MALE", concept_code = "M", domain_id = "Gender", vocabulary_id = "Gender", concept_class_id = "Gender")))
}

#' Add female attribute to a query
#'
#' @return
#' An attribute that can be used in a query function
#' @export
#'
#' @describeIn
#' attributes female demographic attribute
#' @examples
#' # Create a cohort of males with Type 1 diabetes
#' t1dm <- cs(descendants(201254, 435216, 40484648), name = "type 1 diabetes")
#' t1dm_females <- cohort(conditionOccurrence(t1dm, female()))
female <- function() {

  methods::new("conceptAttribute",
               name = "Gender",
               conceptSet = list(methods::new("Concept", concept_id = 8532L,
    concept_name = "FEMALE", concept_code = "F", domain_id = "Gender", vocabulary_id = "Gender",
    concept_class_id = "Gender")))
}

#' Add gender attribute with one or more concept IDs (e.g. both sexes)
#'
#' Use for round-trip when JSON has \code{Gender: [8507, 8532]}; a single criterion
#' yields one \code{gender_concept_id in (...)} in SQL instead of separate branches.
#' @param ... Integer concept IDs (e.g. \code{8507L}, \code{8532L} for male, female).
#' @return A single conceptAttribute with name \code{Gender} for use in additional criteria.
#' @export
genderConcepts <- function(...) {
  ids <- as.integer(c(...))
  concepts <- lapply(ids, function(id) {
    methods::new("Concept", concept_id = id, concept_name = NA_character_)
  })
  methods::new("conceptAttribute", name = "Gender", conceptSet = concepts)
}

#' Add race filter to a demographic criteria group
#'
#' @param ... Integer OMOP race concept IDs (e.g. \code{8527L} White, \code{8516L} Black or African American,
#'   \code{8515L} Asian, \code{38003614L} Native Hawaiian or Other Pacific Islander, \code{8557L} Native American).
#' @return A conceptAttribute with name \code{Race} for use in \code{withAll()}/\code{withAny()} attrition groups.
#' @export
#' @describeIn attributes race demographic attribute
raceConcepts <- function(...) {
  ids <- as.integer(c(...))
  concepts <- lapply(ids, function(id) {
    methods::new("Concept", concept_id = id, concept_name = NA_character_)
  })
  methods::new("conceptAttribute", name = "Race", conceptSet = concepts)
}

#' Add ethnicity filter to a demographic criteria group
#'
#' @param ... Integer OMOP ethnicity concept IDs (e.g. \code{38003563L} Hispanic or Latino,
#'   \code{38003564L} Not Hispanic or Latino).
#' @return A conceptAttribute with name \code{Ethnicity} for use in \code{withAll()}/\code{withAny()} attrition groups.
#' @export
#' @describeIn attributes ethnicity demographic attribute
ethnicityConcepts <- function(...) {
  ids <- as.integer(c(...))
  concepts <- lapply(ids, function(id) {
    methods::new("Concept", concept_id = id, concept_name = NA_character_)
  })
  methods::new("conceptAttribute", name = "Ethnicity", conceptSet = concepts)
}

#' Add provider specialty filter to a visit (round-trip from Atlas JSON)
#'
#' When JSON has \code{VisitOccurrence.ProviderSpecialty: [{ CONCEPT_ID: ... }]}, use this
#' so round-trip preserves the \code{PR.specialty_concept_id in (...)} filter.
#' @param ... Integer concept IDs for provider specialty (e.g. \code{38004463L}).
#' @return A conceptAttribute with name \code{ProviderSpecialty} for use in \code{visit()}.
#' @export
providerSpecialtyConcepts <- function(...) {
  ids <- as.integer(c(...))
  concepts <- lapply(ids, function(id) {
    methods::new("Concept", concept_id = id, concept_name = NA_character_)
  })
  methods::new("conceptAttribute", name = "ProviderSpecialty", conceptSet = concepts)
}


buildConceptAttribute <- function(ids, attributeName, connection = NULL,
                                  vocabularyDatabaseSchema = NULL, fnName = attributeName) {

  if (methods::is(ids, "ConceptSet")) {
    rlang::abort(paste0(fnName, " requires a vector of concept ids, not a ConceptSet"))
  }
  checkmate::assertIntegerish(ids, min.len = 1, any.missing = FALSE, lower = 0)
  ids <- as.integer(ids)

  if (is.null(connection)) {
    # Concept ids alone are sufficient for SQL generation; names are display-only in Atlas.
    concepts <- purrr::map(ids, ~methods::new("Concept", concept_id = .x,
                                              concept_name = NA_character_))
  } else {
    detailedConceptSet <- getConceptSetDetails(cs(ids, name = attributeName),
                                               con = connection,
                                               vocabularyDatabaseSchema = vocabularyDatabaseSchema)
    concepts <- purrr::map(detailedConceptSet@Expression, ~.@Concept)
  }

  methods::new("conceptAttribute",
               name = attributeName,
               conceptSet = concepts)
}


#' Add a value as concept attribute
#' @param ids integer concept ids for the attribute
#' @param connection an optional connection to an OMOP CDM database, used only to look up
#'   concept names for display (e.g. in Atlas). When NULL (default) the attribute is built
#'   from the ids alone, which produces identical SQL.
#' @param vocabularyDatabaseSchema the schema of the OMOP vocabulary tables; only used
#'   when `connection` is supplied
#' @return
#' An attribute that can be used in a query function
#' @export
#'
valueAsConcept <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "ValueAsConcept",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "valueAsConcept")
}

#' ValueAsConcept attribute from a concept set (for round-trip without DB)
#'
#' Filter Measurement or Observation by value_as_concept_id using a concept set.
#' Use this when building from JSON (CodesetId reference); use \code{valueAsConcept(ids)} for ad-hoc concept ids.
#' @param conceptSet A ConceptSet object (e.g. from \code{cs()} or decompiled JSON).
#' @return An attribute for use in \code{\link{measurement}()} or \code{\link{observation}()}.
#' @export
valueAsConceptSet <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("valueAsConceptSet requires a ConceptSet object")
  }
  methods::new("conceptSetAttribute", name = "ValueAsConcept", conceptSet = conceptSet)
}

#' Add a drug type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{drugExposure()}.
#' @export
drugType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "DrugType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "drugType")
}

#' Add a condition type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return
#' An attribute that can be used in a query function
#' @export
#'
conditionType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "ConditionType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "conditionType")
}



#' Add a visit type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in any domain query that has a visit context filter.
#' @export
#'
visitType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "VisitType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "visitType")
}


#' Add a measurement type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{measurement()}.
#' @export
#'
measurementType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "MeasurementType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "measurementType")
}

#' Add an observation type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{observation()}.
#' @export
#'
observationType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "ObservationType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "observationType")
}


#' Add a procedure type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{procedure()}.
#' @export
#'
procedureType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "ProcedureType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "procedureType")
}

#' Add a death type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{death()}.
#' @export
#'
deathType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "DeathType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "deathType")
}

#' Add a device type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{deviceExposure()}.
#' @export
#'
deviceType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "DeviceType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "deviceType")
}

#' Add a specimen type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{specimen()}.
#' @export
#'
specimenType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "SpecimenType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "specimenType")
}

#' Add a condition status attribute
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{conditionOccurrence()}.
#' @export

conditionStatus <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "ConditionStatus",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "conditionStatus")
}

#' Route concept attribute for drug exposure
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{drugExposure()}.
#' @export
routeConcept <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "RouteConcept",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "routeConcept")
}

#' Dose unit attribute for drug exposure
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{drugExposure()}.
#' @export
doseUnit <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "DoseUnit",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "doseUnit")
}

#' Measurement operator attribute (e.g. equal to, less than)
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{measurement()}.
#' @export
measurementOperator <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "Operator",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "measurementOperator")
}

#' Observation qualifier attribute
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{observation()}.
#' @export
observationQualifier <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "Qualifier",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "observationQualifier")
}

#' Procedure modifier attribute
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{procedure()}.
#' @export
procedureModifier <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "Modifier",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "procedureModifier")
}

#' Place of service attribute for visit occurrence
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{visit()}.
#' @export
placeOfService <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "PlaceOfService",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "placeOfService")
}

#' Specimen anatomic site attribute
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{specimen()}.
#' @export
specimenAnatomicSite <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "AnatomicSite",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "specimenAnatomicSite")
}

#' Specimen disease status attribute
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{specimen()}.
#' @export
specimenDiseaseStatus <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  buildConceptAttribute(ids, attributeName = "DiseaseStatus",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "specimenDiseaseStatus")
}

#' Add a condition source concept attribute
#' @param conceptSet a ConceptSet object containing the source concepts
#' @return
#' An attribute that can be used in a query function
#' @export
#'
conditionSourceConcept <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("conditionSourceConcept requires a ConceptSet object")
  }

  res <- methods::new("conceptSetAttribute",
                      name = "ConditionSourceConcept",
                      conceptSet = conceptSet)
  return(res)
}

#' Add a drug source concept attribute
#' @param conceptSet a ConceptSet object containing the source concepts
#' @return
#' An attribute that can be used in a query function
#' @export
#'
drugSourceConcept <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("drugSourceConcept requires a ConceptSet object")
  }

  res <- methods::new("conceptSetAttribute",
                      name = "DrugSourceConcept",
                      conceptSet = conceptSet)
  return(res)
}


#' Add a procedure source concept attribute
#' @param conceptSet a ConceptSet object containing the source concepts
#' @return
#' An attribute that can be used in a query function
#' @export
#'
procedureSourceConcept <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("procedureSourceConcept requires a ConceptSet object")
  }

  res <- methods::new("conceptSetAttribute",
                      name = "ProcedureSourceConcept",
                      conceptSet = conceptSet)
  return(res)
}



#' Add a observation source concept attribute
#' @param conceptSet a ConceptSet object containing the source concepts
#' @return
#' An attribute that can be used in a query function
#' @export
#'
observationSourceConcept <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("observationSourceConcept requires a ConceptSet object")
  }

  res <- methods::new("conceptSetAttribute",
                      name = "ObservationSourceConcept",
                      conceptSet = conceptSet)
  return(res)
}

#' Add a measurement source concept attribute
#' @param conceptSet a ConceptSet object containing the source concepts
#' @return An attribute that can be used in a measurement query
#' @export
measurementSourceConcept <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("measurementSourceConcept requires a ConceptSet object")
  }
  res <- methods::new("conceptSetAttribute",
                      name = "MeasurementSourceConcept",
                      conceptSet = conceptSet)
  return(res)
}

#' Add a visit source concept attribute
#' @param conceptSet a ConceptSet object containing the source concepts
#' @return
#' An attribute that can be used in a query function
#' @export
#'
visitSourceConcept <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("visitSourceConcept requires a ConceptSet object")
  }

  res <- methods::new("conceptSetAttribute",
                      name = "VisitSourceConcept",
                      conceptSet = conceptSet)
  return(res)
}

#' Add a visit type attribute (filter by visit_concept_id) from a concept set
#'
#' Restricts criteria to events occurring in specified visit types (e.g. inpatient, ER).
#' Used when decompiling cohort JSON that has VisitType as concept list or CodesetId.
#' @param conceptSet a ConceptSet object containing visit type concepts
#' @return An attribute for use in conditionOccurrence(), drugExposure(), etc.
#' @export
visitTypeSet <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("visitTypeSet requires a ConceptSet object")
  }
  res <- methods::new("conceptSetAttribute",
                      name = "VisitType",
                      conceptSet = conceptSet)
  return(res)
}

#' Add a visit detail source concept attribute
#' @param conceptSet a ConceptSet object containing the source concepts
#' @return An attribute for use in visitDetail()
#' @export
visitDetailSourceConcept <- function(conceptSet) {
  if (!methods::is(conceptSet, "ConceptSet")) {
    rlang::abort("visitDetailSourceConcept requires a ConceptSet object")
  }
  res <- methods::new("conceptSetAttribute",
                      name = "VisitDetailSourceConcept",
                      conceptSet = conceptSet)
  return(res)
}

# PayerPlanPeriod integer concept reference attributes -----
# These fields take an integer CodesetId referencing a concept set, same as source concept fields.

#' Payer concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the payer concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
payerConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "PayerConcept", conceptSet = conceptSet)
}

#' Plan concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the plan concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
planConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "PlanConcept", conceptSet = conceptSet)
}

#' Sponsor concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the sponsor concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
sponsorConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "SponsorConcept", conceptSet = conceptSet)
}

#' Stop reason concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the stop reason concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
stopReasonConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "StopReasonConcept", conceptSet = conceptSet)
}

#' Payer source concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the payer source concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
payerSourceConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "PayerSourceConcept", conceptSet = conceptSet)
}

#' Plan source concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the plan source concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
planSourceConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "PlanSourceConcept", conceptSet = conceptSet)
}

#' Sponsor source concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the sponsor source concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
sponsorSourceConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "SponsorSourceConcept", conceptSet = conceptSet)
}

#' Stop reason source concept attribute for PayerPlanPeriod
#' @param conceptSet A ConceptSet for the stop reason source concept filter.
#' @return An attribute for use in \code{payerPlanPeriod()}.
#' @export
stopReasonSourceConcept <- function(conceptSet) {
  checkmate::assertClass(conceptSet, "ConceptSet")
  methods::new("conceptSetAttribute", name = "StopReasonSourceConcept", conceptSet = conceptSet)
}

#' Add a observation period type attribute to determine the provenance of the record
#' @inheritParams valueAsConcept
#' @return
#' An attribute that can be used in a query function
#' @export
#'

observationPeriodType <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  # Circe expects the key "PeriodType" on ObservationPeriod criteria
  buildConceptAttribute(ids, attributeName = "PeriodType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "observationPeriodType")
}

#' Unit attribute for a query
#'
#' Filters by unit concept id (serializes to Circe \code{Unit} field).
#' Applies to \code{measurement()}, \code{observation()}, \code{doseEra()}, and \code{specimen()}.
#'
#' Named \code{measurementUnit} (not \code{unit}) to avoid masking \code{dplyr::unit}.
#' @inheritParams valueAsConcept
#' @return An attribute for use in \code{measurement()}, \code{observation()}, \code{doseEra()}, or \code{specimen()}.
#' @export
#'
measurementUnit <- function(ids, connection = NULL, vocabularyDatabaseSchema = NULL) {
  if (missing(ids)) {
    rlang::abort("Unit must be specified")
  }
  buildConceptAttribute(ids, attributeName = "Unit",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                        fnName = "measurementUnit")
}

# Coercion ------------------

setMethod("as.list", "conceptSetAttribute", function(x) {
  nm <- x@name
  # Circe expects these as arrays of Concept objects, not a CodesetId (integer).
  if (identical(nm, "ValueAsConcept") || identical(nm, "VisitType")) {
    val <- if (length(x@conceptSet@Expression) > 0L) {
      purrr::map(x@conceptSet@Expression, function(e) as.list(e@Concept))
    } else {
      list()
    }
    return(tibble::lst(`:=`(!!nm, val)))
  }
  tibble::lst(`:=`(!!nm, x@conceptSet@id))
})

setMethod("as.list", "conceptAttribute", function(x) {

  concepts <- purrr::map(x@conceptSet, ~as.list(.x))
  nm <- x@name

  tibble::lst(`:=`(!!nm, concepts))
})

## valueAsStringAttribute (Observation value_as_string filter, e.g. LIKE '%Yes%') ----

#' Attribute for Observation value_as_string filter (round-trip from Atlas ValueAsString)
#'
#' Serializes to \code{ValueAsString: { Text, Op }}; Circe generates \code{WHERE value_as_string LIKE ...}.
#' @param text Character string to match (e.g. \code{"Yes"}).
#' @param op Circe op: \code{"contains"} (default, LIKE \code{\%text\%}), \code{"starts"}, \code{"ends"}, \code{"equals"}.
#' @return Attribute for use in \code{observation()}.
#' @export
valueAsString <- function(text, op = "contains") {
  op <- match.arg(op, c("contains", "starts", "ends", "equals"))
  methods::new("valueAsStringAttribute", name = "ValueAsString", text = as.character(text)[1L], op = op)
}

setClass("valueAsStringAttribute",
         slots = c(name = "character", text = "character", op = "character"),
         prototype = list(name = "ValueAsString", text = NA_character_, op = "contains"))

setMethod("as.list", "valueAsStringAttribute", function(x) {
  list(ValueAsString = list(Text = x@text, Op = x@op))
})

# Capr Call -----------------
