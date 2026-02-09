
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
                 "VisitOccurrence" = "Visit")

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
  cat(glue::glue("<Capr {object@domain} Query> {object@conceptSet@Name}"), "\n")

  # TODO make this a one line print method
  # cli::console_width() can give the available space in the console

  # cat("Attributes:", "\n")
  # if (length(object@Attributes) > 0) {
  #   for (i in seq_along(object@Attributes)) {
  #     cat("\t",paste0(i, ") "))
  #     show(object@Attributes[[i]])
  #     cat("\n")
  #   }
  # } else {
  #   cat("None", "\n")
  # }
})


# Constructors -----

query <- function(domain, conceptSet = NULL, ...) {

  # bundle attributes as a list
  atb <- list(...)

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

#' Query the condition domain
#'
#' @param conceptSet A condition concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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

#' Query the drug domain
#'
#' @param conceptSet A drug concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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


#' Query the drug domain
#'
#' @param conceptSet A drug concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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
#' @param conceptSet A measurement concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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

#' Query the procedure domain
#'
#' @param conceptSet A procedure concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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
#' @param conceptSet A drug ingredient concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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
#' @param conceptSet A condition concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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
#' @param conceptSet A condition concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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
#' @param conceptSet A death domain, with rare exceptions this is always NULL
#' @param ... optional attributes
#'
#' @return A Capr Query
#' @export
death <- function(conceptSet = NULL, ...) {

  query(domain = "Death",
        conceptSet = conceptSet,
        ...)
}

#' Query the observation domain
#'
#' @param conceptSet A condition concept set (optional)
#' @param ... optional attributes
#'
#' @return A Capr Query
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

#' Query the observation period domain
#'
#' @param ... optional attributes
#'
#' @return A Capr Query of domain observation period
#' @export
observationPeriod <- function(...) {
  query(domain = "ObservationPeriod", conceptSet = NULL, ...)
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





