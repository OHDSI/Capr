
#' An S4 class for a Circe Query
#'
#' A query is a medical concept that can be extracted from a database through a 'where' clause in a SQL statement.
#' This includes concepts.
#'
#' @slot domain The domain to search (e.g. "Condition", "Drug", "Measurement", etc)
#' @slot conceptSet The Concept set describing the observation to serach for
#' @slot attributes a list of attributes that modify the query (e.g. `male()`, `female()`, `age(gte(65))`)
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
  validDomains <- c("Condition", "conditionOccurrence", "Observation", "Drug", "Measurement") #...
  stopifnot(object@domain %in% validDomains)
  TRUE
})

#' @rdname show-method
#' @aliases show,Query-method
setMethod("show", "Query", function(object) {
  cat(glue::glue("<Capr {object@domain} Query> {object@conceptSet@Name}"), "\n")
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

query <- function(domain, conceptSet, ...) {

  # bundle attributes as a list
  atb <- list(...)

  query <- new("Query",
               domain = domain,
               conceptSet = conceptSet,
               attributes = atb)
  return(query)

}

#' Query the condition domain
#'
#' @param conceptSet A condition concept set
#' @param ... optional attributes
#'
#' @return A Capr Query
#' @export
conditionOccurrence <- function(conceptSet, ...) {

  query(domain = "conditionOccurrence",
        conceptSet = conceptSet,
        ...)

}

drugExposure <- function(conceptSet, ...) {

  query(domain = "drugExposure",
        conceptSet = conceptSet,
        ...)

}

measurement <- function(conceptSet, ...) {

  query(domain = "measurement",
        conceptSet = conceptSet,
        ...)

}

procedureOccurrence <- function(conceptSet, ...) {

  query(domain = "procedureOccurrence",
        conceptSet = conceptSet,
        ...)

}

