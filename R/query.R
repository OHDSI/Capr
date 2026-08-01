
# Class definition -----

#' An S4 class for a Circe Query
#'
#' A query is a medical concept that can be extracted from a database through a 'where' clause in a 'SQL' statement.
#' This includes concepts.
#'
#' @slot domain The domain to search (e.g. "Condition", "Drug", "Measurement", etc)
#' @slot conceptSet The Concept set describing the observation to serach for
#' @slot attributes a list of attributes that modify the query (e.g. `male()`, `female()`, `age(gte(65))`)
#' @include conceptSet.R
setClass("Query",
         slot = c(
           domain = "character",
           conceptSet = "ConceptSet",
           attributes = "list"
         ),
         prototype = list(
           domain = NA_character_,
           conceptSet = new("ConceptSet"),
           attributes = list()
         )
)

setValidity("Query", function(object) {
  validDomains <- c("ConditionEra",
                    "ConditionOccurrence",
                    "Death",
                    "ProcedureOccurrence",
                    "DeviceExposure",
                    "DoseEra",
                    "DrugExposure",
                    "DrugEra",
                    "Measurement",
                    "Observation",
                    "PayerPlanPeriod",
                    "Specimen",
                    "VisitOccurrence",
                    "VisitDetail",
                    "ObservationWindow", # check on this
                    "ObservationPeriod")
  stopifnot(object@domain %in% validDomains)

  domainsInConceptSet <- purrr::map_chr(object@conceptSet@Expression, ~.@Concept@domain_id)
  domainsInConceptSet <- domainsInConceptSet[!is.na(domainsInConceptSet)]
  domainsInConceptSet <- domainsInConceptSet[domainsInConceptSet != ""]

  domainMap <- c("ConditionEra" = "Condition",
                 "ConditionOccurrence" = "Condition",
                 "DeviceExposure" = "Device",
                 "DrugExposure" = "Drug",
                 "Measurement" = "Measurement",
                 "Specimen" = "Specimen",
                 "VisitOccurrence" = "Visit",
                 "VisitDetail" = "Visit")

  # Print a warning if the concept set does not include concepts with the expected domain_id (domain_id must be populated)
  if ((object@domain %in% names(domainMap)) &&
      (length(domainsInConceptSet) > 0) &&
      !(domainMap[object@domain] %in% domainsInConceptSet)) {
    rlang::warn(glue::glue("{object@domain} query does contain concepts in {domainMap[object@domain]} domain."))
  }

  # TODO validation of attributes. Allowed attributes are query specific.

  TRUE
})

# Printing ----

#' @aliases show,Query-method
setMethod("show", "Query", function(object) {
  cat("<Capr Query> ", fmt_query(object), "\n", sep = "")
})


# Constructors -----

query <- function(domain, conceptSet = NULL, ...) {

  # bundle attributes as a list
  atb <- list(...)
  checkCaprDots(atb,
                c("conceptSetAttribute", "conceptAttribute", "valueAsStringAttribute",
                  "opAttributeSuper", "logicAttribute", "keyValueAttribute",
                  "dateAdjustmentAttribute", "nestedAttribute"),
                "query", "Capr attribute objects", "conceptSet")

  if (is.null(conceptSet)) {
    query <- methods::new("Query",
                 domain = domain,
                 attributes = atb)
  } else {
    query <- methods::new("Query",
                 domain = domain,
                 conceptSet = conceptSet,
                 attributes = atb)
  }


  return(query)
}

#' Query the condition occurrence domain
#'
#' @param conceptSet A \code{ConceptSet} built with \code{\link{cs}()}, or \code{NULL} for any condition.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()}, \code{endDate()},
#'   \code{age()}, \code{male()}/\code{female()}, \code{conditionType()}, \code{conditionStatus()},
#'   \code{conditionSourceConcept()}, \code{nestedWithAll()}, \code{dateAdjustment()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{entry}}, \code{\link{atLeast}}, \code{\link{cs}}
#' @examples
#' t2dm <- cs(descendants(201826L), name = "T2DM")
#' conditionOccurrence(t2dm)
#' conditionOccurrence(t2dm, firstOccurrence(), age(gte(18L)))
#' @export
conditionOccurrence <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "ConditionOccurrence",
        conceptSet = conceptSet,
        ...)
}

#' Query the drug exposure domain
#'
#' @param conceptSet A \code{ConceptSet} built with \code{\link{cs}()}, or \code{NULL} for any drug.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()}, \code{endDate()},
#'   \code{age()}, \code{drugType()}, \code{drugRefills()}, \code{drugQuantity()},
#'   \code{daysOfSupply()}, \code{lotNumber()}, \code{stopReason()}, \code{routeConcept()},
#'   \code{drugSourceConcept()}, \code{nestedWithAll()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{drugEra}}, \code{\link{entry}}, \code{\link{cs}}
#' @examples
#' metformin <- cs(descendants(1503297L), name = "Metformin")
#' drugExposure(metformin)
#' drugExposure(metformin, firstOccurrence(), daysOfSupply(gte(90L)))
#' @export 
drugExposure <- function(conceptSet, ...) {
  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "DrugExposure",
        conceptSet = conceptSet,
        ...)
}


#' Query the device exposure domain
#'
#' @param conceptSet A \code{ConceptSet} built with \code{\link{cs}()}, or \code{NULL} for any device.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()}, \code{endDate()},
#'   \code{age()}, \code{deviceType()}, \code{uniqueDeviceId()}, \code{quantityValue()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{entry}}, \code{\link{cs}}
#' @export 
deviceExposure <- function(conceptSet, ...) {
  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "DeviceExposure",
        conceptSet = conceptSet,
        ...)
}

#' Query the measurement domain
#'
#' @param conceptSet A \code{ConceptSet} built with \code{\link{cs}()}, or \code{NULL} for any measurement.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()},
#'   \code{valueAsNumber()}, \code{valueAsConcept()}, \code{measurementUnit()},
#'   \code{measurementType()}, \code{rangeLow()}, \code{rangeHigh()},
#'   \code{measurementSourceConcept()}, \code{age()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{valueAsNumber}}, \code{\link{measurementUnit}}, \code{\link{entry}}
#' @examples
#' hba1c <- cs(descendants(4184637L), name = "HbA1c")
#' measurement(hba1c, valueAsNumber(gt(6.5)), measurementUnit(8554L))
#' @export
measurement <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "Measurement",
        conceptSet = conceptSet,
        ...)
}

#' Query the procedure occurrence domain
#'
#' @param conceptSet A \code{ConceptSet} built with \code{\link{cs}()}, or \code{NULL} for any procedure.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()}, \code{age()},
#'   \code{procedureType()}, \code{procedureModifier()}, \code{quantityValue()},
#'   \code{procedureSourceConcept()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{entry}}, \code{\link{cs}}
#' @export
procedure <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "ProcedureOccurrence",
        conceptSet = conceptSet,
        ...)
}

#' Query the drug era domain
#'
#' @param conceptSet A \code{ConceptSet} for the drug ingredient.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()}, \code{endDate()},
#'   \code{eraLength()}, \code{occurrenceCount()}, \code{ageAtStart()}, \code{ageAtEnd()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{drugExposure}}, \code{\link{drugExit}}, \code{\link{entry}}
#' @export
drugEra <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "DrugEra",
        conceptSet = conceptSet,
        ...)
}

#' Query the dose era domain
#'
#' @param conceptSet A drug ingredient concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
#' @export
doseEra <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "DoseEra",
        conceptSet = conceptSet,
        ...)
}

#' Query the condition era domain
#'
#' @param conceptSet A \code{ConceptSet} for the condition.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()}, \code{endDate()},
#'   \code{eraLength()}, \code{occurrenceCount()}, \code{ageAtStart()}, \code{ageAtEnd()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{conditionOccurrence}}, \code{\link{entry}}
#' @export
conditionEra <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "ConditionEra",
        conceptSet = conceptSet,
        ...)
}

#' Query the visit occurrence domain
#'
#' @param conceptSet A \code{ConceptSet} built with \code{\link{cs}()}, or \code{NULL} for any visit.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()}, \code{endDate()},
#'   \code{visitType()}, \code{visitLength()}, \code{visitSourceConcept()}, \code{placeOfService()},
#'   \code{age()}, \code{nestedWithAll()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{entry}}, \code{\link{cs}}
#' @examples
#' ip <- cs(descendants(9201L, 262L), name = "Inpatient")
#' visit(ip)
#' @export
visit <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "VisitOccurrence",
        conceptSet = conceptSet,
        ...)
}

#' Query the death domain
#'
#' @param conceptSet A \code{ConceptSet} for the death cause concept, or \code{NULL} (most common —
#'   death records are typically matched without a concept set).
#' @param ... Optional attributes: \code{startDate()}, \code{deathType()}, \code{age()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{censoringEvents}}, \code{\link{exit}}
#' @examples
#' # Any death (no concept set)
#' death(NULL)
#' @export
death <- function(conceptSet = NULL, ...) {

  query(domain = "Death",
        conceptSet = conceptSet,
        ...)
}

#' Query the observation domain
#'
#' @param conceptSet A \code{ConceptSet} built with \code{\link{cs}()}, or \code{NULL} for any observation.
#' @param ... Optional attributes: \code{firstOccurrence()}, \code{startDate()},
#'   \code{valueAsNumber()}, \code{valueAsString()}, \code{valueAsConcept()},
#'   \code{observationType()}, \code{observationQualifier()}, \code{observationSourceConcept()},
#'   \code{age()}, etc.
#' @return A \code{Query} object.
#' @seealso \code{\link{entry}}, \code{\link{cs}}
#' @export
observation <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "Observation",
        conceptSet = conceptSet,
        ...)
}

#' Query the specimen domain
#'
#' @param conceptSet A specimen concept set
#' @param ... optional attributes (e.g. CorrelatedCriteria)
#'
#' @return A Capr Query
#' @export
specimen <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "Specimen",
        conceptSet = conceptSet,
        ...)
}

#' Query the visit detail domain
#'
#' @param conceptSet A visit detail concept set
#' @param ... optional attributes (e.g. VisitDetailSourceConcept)
#'
#' @return A Capr Query
#' @export
visitDetail <- function(conceptSet, ...) {

  # Check if conceptSet argument is missing
  if (missing(conceptSet)) {
    stop("conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL")
  }

  query(domain = "VisitDetail",
        conceptSet = conceptSet,
        ...)
}

#' Query the observation period domain
#'
#' @param ... optional attributes
#'
#' @return A Capr Query of domain observation period
#' @export
observationPeriod <- function(...) {
  query(domain = "ObservationPeriod", conceptSet = NULL, ...)
}

#' Query the payer plan period domain (CDM ≥ 5.3)
#'
#' @param ... optional attributes (e.g. \code{firstOccurrence()}, \code{startDate()},
#'   \code{periodLength()}, \code{ageAtStart()}, \code{ageAtEnd()}, \code{genderCS()})
#' @return A Capr Query of domain PayerPlanPeriod
#' @export
payerPlanPeriod <- function(...) {
  query(domain = "PayerPlanPeriod", conceptSet = NULL, ...)
}

# Coercion -----
## Coerce Query ----
setMethod("as.list", "Query", function(x) {
  # Include CodesetId only when the query has a concept set (non-empty expression).
  # When conceptSet is empty/null (e.g. "any condition" with only ConditionSourceConcept), omit CodesetId.
  ll <- list()
  if (length(x@conceptSet@Expression) > 0L && length(x@conceptSet@id) >= 1L) {
    id <- x@conceptSet@id
    ll[["CodesetId"]] <- if (is.numeric(id) || is.integer(id)) as.integer(id)[1L] else id[1L]
  }
  # List out attributes. Put *TypeExclude keys first so serialized JSON key order
  # matches Atlas/CIRCE (e.g. conditionTypeExclude before other attributes).
  if (length(x@attributes) > 0) {
    atr <- purrr::map(x@attributes, ~as.list(.x)) |>
      purrr::reduce(append)
    typeExcludeKeys <- grep("TypeExclude$", names(atr), value = TRUE)
    otherKeys <- setdiff(names(atr), typeExcludeKeys)
    ll <- append(ll, atr[c(typeExcludeKeys, otherKeys)])
  }

  # ObservationPeriod: Circe expects UserDefinedPeriod { StartDate, EndDate };
  # we store as OccurrenceStartDate (op/Value/Extent). Convert on export.
  if (x@domain == "ObservationPeriod" && "OccurrenceStartDate" %in% names(ll)) {
    osd <- ll$OccurrenceStartDate
    startDate <- format(as.Date(osd$Value), "%Y-%m-%d")
    endDate <- if (identical(osd$Op, "bt") && !is.na(osd$Extent))
      format(as.Date(osd$Extent), "%Y-%m-%d") else startDate
    ll$OccurrenceStartDate <- NULL
    ll$OccurrenceEndDate <- NULL
    ll$UserDefinedPeriod <- list(StartDate = startDate, EndDate = endDate)
  }

  # Use empty named list when ll is empty so JSON serializes as {} not [] (CIRCE
  # expects domain value to be an object, e.g. ObservationPeriod: {}).
  if (length(ll) == 0L) {
    ll <- structure(list(), names = character(0))
  }

  tibble::lst(
    !!x@domain := ll
  )
})
# class(x@conceptSet@Expression[[1]])
# as.list(x@conceptSet@Expression[[1]])
# setMethod("as.list", "Query", function(x) {
#
#   x <- condition(cs(1:2))
#   # A query has exactly one concept set so id is always 0
#   conceptSetList <- list(id = 0,
#                          name = x@conceptSet@Name,
#                          items = lapply(x@conceptSet@Expression, as.list))
#
#   #TODO add attributes
#   atr <- list("Age" = list("Value" = 90, "Op" = "lt"),
#               "Age2" = list("Value" = 90, "Op" = "lt"))
#
#   domainList <- c(0, atr) |>  rlang::set_names(c("CodesetId", names(atr)))
#
#   atr_name <- "Age2"
#   atr_value <- list("Value" = 90, "Op" = "lt")
#
#   ll <- lst("ConceptSets" = conceptSetList,
#               !!x@domain := lst('CodesetId' = 0, !!atr_name := atr_value))
#
#   jsonlite::toJSON(ll, pretty = TRUE, auto_unbox = TRUE)
#   return(ll)
# })





