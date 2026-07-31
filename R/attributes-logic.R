# Classes ----------------------------

## Logic Attribute -----
#' An S4 class for a logical attribute
#' @description
#' with a logic attribute if it is specified than we assume it is true
#' @slot
#' name the name of the attribute
setClass("logicAttribute", slots = c(name = "character"), prototype = list(name = NA_character_))
# Constructors -----------
#' Add first occurrence attribute
#' @return
#' An attribute that can be used in a query function
#' @export
firstOccurrence <- function() {
  res <- methods::new("logicAttribute", name = "First")
  return(res)
}

# Coercion ------------------

setMethod("as.list", "logicAttribute", function(x) {

  tibble::lst(`:=`(!!x@name, TRUE))
})

## Key-value attribute (e.g. ConditionTypeExclude = FALSE) ----

#' Attribute that serializes to a single key-value pair in query JSON.
#' Used for boolean query options like ConditionTypeExclude.
#' Has a \code{name} slot (same as key) so compile/collectGuid work.
setClass("keyValueAttribute",
         slots = c(key = "character", value = "logical", name = "character"),
         prototype = list(key = NA_character_, value = NA, name = NA_character_))

setMethod("as.list", "keyValueAttribute", function(x) {
  stats::setNames(list(x@value), x@key)
})

#' ConditionTypeExclude query attribute
#'
#' When FALSE, condition type is not excluded (include all). When TRUE, excludes
#' specified condition types (Capr does not support type-concept lists without DB).
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude (unsupported in decompiler)
#' @return An attribute for use in conditionOccurrence()
#' @export
conditionTypeExclude <- function(exclude = FALSE) {
  k <- "ConditionTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' DeathTypeExclude query attribute
#'
#' When FALSE, death type is not excluded (include all). When TRUE, excludes
#' specified death types (Capr does not support type-concept lists without DB).
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude (unsupported in decompiler)
#' @return An attribute for use in death()
#' @export
deathTypeExclude <- function(exclude = FALSE) {
  k <- "DeathTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' MeasurementTypeExclude query attribute
#'
#' When FALSE, measurement type is not excluded (include all). When TRUE, excludes
#' specified measurement types (Capr does not support type-concept lists without DB).
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude (unsupported in decompiler)
#' @return An attribute for use in measurement()
#' @export
measurementTypeExclude <- function(exclude = FALSE) {
  k <- "MeasurementTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' SpecimenTypeExclude query attribute
#'
#' When FALSE, specimen type is not excluded (include all). When TRUE, excludes
#' specified specimen types (Capr does not support type-concept lists without DB).
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude (unsupported in decompiler)
#' @return An attribute for use in specimen()
#' @export
specimenTypeExclude <- function(exclude = FALSE) {
  k <- "SpecimenTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' DrugTypeExclude query attribute
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude
#' @return An attribute for use in \code{drugExposure()}
#' @export
drugTypeExclude <- function(exclude = FALSE) {
  k <- "DrugTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' DeviceTypeExclude query attribute
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude
#' @return An attribute for use in \code{deviceExposure()}
#' @export
deviceTypeExclude <- function(exclude = FALSE) {
  k <- "DeviceTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' ObservationTypeExclude query attribute
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude
#' @return An attribute for use in \code{observation()}
#' @export
observationTypeExclude <- function(exclude = FALSE) {
  k <- "ObservationTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' ProcedureTypeExclude query attribute
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude
#' @return An attribute for use in \code{procedure()}
#' @export
procedureTypeExclude <- function(exclude = FALSE) {
  k <- "ProcedureTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}

#' VisitTypeExclude query attribute
#' @param exclude logical; FALSE = include all types (default), TRUE = exclude
#' @return An attribute for use in \code{visit()}
#' @export
visitTypeExclude <- function(exclude = FALSE) {
  k <- "VisitTypeExclude"
  methods::new("keyValueAttribute", key = k, value = as.logical(exclude)[1L], name = k)
}
