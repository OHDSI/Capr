# Classes -----------------------

## Occurrence --------------

#' An S4 class for an occurrence.
#' @description This determines how many events need to occur to count the criteria
#' in the cohort definition (relative to the index event)
#' @slot type a character string determine the logic for counting occurrences. Can be
#' all, any, exactly, atLeast, or atMost
#' @slot count an integer specifying the number of occurrences for a criteria
#' @slot isDistinct optional logical; if TRUE, count distinct (e.g. by CountColumn)
#' @slot countColumn optional character; when counting, use this column for distinct (e.g. "DOMAIN_CONCEPT")
setClass("Occurrence",
         slots = c(
           type = "character",
           count = "integer",
           isDistinct = "logical",
           countColumn = "character"
         ),
         prototype = list(
           type = NA_character_,
           count = NA_integer_,
           isDistinct = as.logical(NA),
           countColumn = NA_character_
         )
)
## Criteria ----------------
#' An S4 for a criteria
#' @description a criteria is a temporal observation of a clinical event relative to the index event
#' @slot occurrence an occurrence object specifying how many events must occur
#' to consider the event as part of the cohort definition
#' @slot query a query object that provides context to the clinical event of interest
#' @slot aperture an eventAperture object that shows the temporal span where the event is to be observed
#' relative to the index event
#' @include window.R query.R
setClass("Criteria",
         slots = c(
           occurrence = 'Occurrence',
           query = 'Query',
           aperture = 'EventAperture'),
         prototype = list(
           occurrence = new("Occurrence"),
           query = new("Query"),
           aperture = new("EventAperture")
         )
)

## Gorup ----------------
#' An S4 class for a group
#' @description a group is the combination of multiple criteria or sub groups
#' @slot occurrence an occurrence object specifying how many events must occur
#' to consider the event as part of the cohort definition
#' @slot critera a list of criteria that are grouped together
#' @slot group a list of sub-groups to consider
setClass("Group",
         slots = c(
           occurrence = 'Occurrence',
           criteria = 'list',
           group = 'list'),
         prototype = list(
           occurrence = new("Occurrence"),
           criteria = list(),
           group = list()
         )
)

# Class Type ----
is.Criteria <- function(x) {
  any(methods::is(x) == "Criteria")
}

is.Group <- function(x) {
  any(methods::is(x) == "Group")
}

# Constructors -----------------------

## Criteria ----------------

#' Require exactly N occurrences of a query within a window
#'
#' @param x Integer count of required occurrences.
#' @param query A \code{Query} object (e.g. from \code{conditionOccurrence()}).
#' @param aperture An \code{EventAperture} built with \code{\link{duringInterval}}. Defaults to all time.
#' @param distinct If \code{TRUE}, count distinct values of \code{countColumn}.
#' @param countColumn Column to use for distinct counting (e.g. \code{"DOMAIN_CONCEPT"}).
#' @return A \code{Criteria} object for use in \code{\link{withAll}}, \code{\link{withAny}}, etc.
#' @seealso \code{\link{atLeast}}, \code{\link{atMost}}, \code{\link{duringInterval}}
#' @examples
#' t2dm <- cs(descendants(201826L), name = "T2DM")
#' # No prior T2DM diagnosis at any time before index
#' exactly(0, conditionOccurrence(t2dm), duringInterval(eventStarts(-Inf, 0)))
#' @export
exactly <- function(x,
                    query,
                    aperture = duringInterval(eventStarts(-Inf, Inf)),
                    distinct = NA,
                    countColumn = NA_character_) {

  if (methods::is(aperture, "EventWindow")) {
    cli::cli_abort(c(
      "{.arg aperture} must be an {.cls EventAperture}.",
      "i" = "Wrap your window with {.fn duringInterval}, e.g. {.code duringInterval(eventStarts(-365, 0))}."
    ))
  }

  occurrence <- methods::new("Occurrence",
      type = "exactly",
      count = as.integer(x),
      isDistinct = as.logical(distinct)[1L],
      countColumn = as.character(countColumn)[1L])

  res <- methods::new("Criteria",
      occurrence = occurrence,
      query = query,
      aperture = aperture)
  return(res)
}

#' Require at least N occurrences of a query within a window
#'
#' @param x Minimum integer count of required occurrences.
#' @param query A \code{Query} object (e.g. from \code{conditionOccurrence()}).
#' @param aperture An \code{EventAperture} built with \code{\link{duringInterval}}. Defaults to all time.
#' @param distinct If \code{TRUE}, count distinct values of \code{countColumn}.
#' @param countColumn Column to use for distinct counting (e.g. \code{"DOMAIN_CONCEPT"}).
#' @return A \code{Criteria} object for use in \code{\link{withAll}}, \code{\link{withAny}}, etc.
#' @seealso \code{\link{exactly}}, \code{\link{atMost}}, \code{\link{duringInterval}}
#' @examples
#' metformin <- cs(descendants(1503297L), name = "Metformin")
#' # At least 2 metformin fills in the 365 days before index
#' atLeast(2, drugExposure(metformin), duringInterval(eventStarts(-365, 0)))
#' @export
atLeast <- function(x,
                    query,
                    aperture = duringInterval(eventStarts(-Inf, Inf)),
                    distinct = NA,
                    countColumn = NA_character_) {

  if (methods::is(aperture, "EventWindow")) {
    cli::cli_abort(c(
      "{.arg aperture} must be an {.cls EventAperture}.",
      "i" = "Wrap your window with {.fn duringInterval}, e.g. {.code duringInterval(eventStarts(-365, 0))}."
    ))
  }

  occurrence <- methods::new("Occurrence",
      type = "atLeast",
      count = as.integer(x),
      isDistinct = as.logical(distinct)[1L],
      countColumn = as.character(countColumn)[1L])

  res <- methods::new("Criteria",
      occurrence = occurrence,
      query = query,
      aperture = aperture)
  return(res)
}

#' Require at most N occurrences of a query within a window
#'
#' @param x Maximum integer count of allowed occurrences.
#' @param query A \code{Query} object (e.g. from \code{conditionOccurrence()}).
#' @param aperture An \code{EventAperture} built with \code{\link{duringInterval}}. Defaults to all time.
#' @param distinct If \code{TRUE}, count distinct values of \code{countColumn}.
#' @param countColumn Column to use for distinct counting (e.g. \code{"DOMAIN_CONCEPT"}).
#' @return A \code{Criteria} object for use in \code{\link{withAll}}, \code{\link{withAny}}, etc.
#' @seealso \code{\link{exactly}}, \code{\link{atLeast}}, \code{\link{duringInterval}}
#' @examples
#' t2dm <- cs(descendants(201826L), name = "T2DM")
#' # At most 1 T2DM diagnosis before index
#' atMost(1, conditionOccurrence(t2dm), duringInterval(eventStarts(-365, 0)))
#' @export
atMost <- function(x,
                   query,
                   aperture = duringInterval(eventStarts(-Inf, Inf)),
                   distinct = NA,
                   countColumn = NA_character_) {

  if (methods::is(aperture, "EventWindow")) {
    cli::cli_abort(c(
      "{.arg aperture} must be an {.cls EventAperture}.",
      "i" = "Wrap your window with {.fn duringInterval}, e.g. {.code duringInterval(eventStarts(-365, 0))}."
    ))
  }

  occurrence <- methods::new("Occurrence",
      type = "atMost",
      count = as.integer(x),
      isDistinct = as.logical(distinct)[1L],
      countColumn = as.character(countColumn)[1L])

  res <- methods::new("Criteria",
      occurrence = occurrence,
      query = query,
      aperture = aperture)
  return(res)
}

## Group -------

#' Function to construct a group where all criteria and groups must be satisfied
#' @param ... a set of criteria or groups
#' @return an S4 group class specifying a bundle of criteria that all must be satisfied in context of the cohort definition
#' @export
withAll <- function(...){
  items <- list(...)
  res <- methods::new("Group",
                      occurrence = methods::new("Occurrence", type = "all"),
                      criteria = purrr::discard(items, is.Group),
                      group = purrr::keep(items, is.Group)
  )
  return(res)
}

#' Function to construct a group where any criteria and groups may be satisfied
#' @param ... a set of criteria or groups
#' @return an S4 group class specifying a bundle of criteria that any may be satisfied in context of the cohort definition
#' @export
withAny <- function(...){
  items <- list(...)
  res <- methods::new("Group",
                      occurrence = methods::new("Occurrence", type = "any"),
                      criteria = purrr::discard(items, is.Group),
                      group = purrr::keep(items, is.Group)
  )
  return(res)
}

#' Function to construct a group where at least some of the criteria or groups must be satisfied
#' @param x an integer specifying the number of criteria or groups that must be satisfied
#' @param ... a set of criteria or groups
#' @return an S4 group class specifying a bundle of criteria that at least x be satisfied in context of the cohort definition
#' @export
withAtLeast <- function(x, ...){
  items <- list(...)
  res <- methods::new("Group",
                     occurrence = methods::new("Occurrence", type = "atLeast", count = as.integer(x)),
                     criteria = purrr::discard(items, is.Group),
                     group = purrr::keep(items, is.Group)
  )
  return(res)
}
#' Function to construct a group where at most some of the criteria or groups must be satisfied
#' @param x an integer specifying the number of criteria or groups that must be satisfied
#' @param ... a set of criteria or groups
#' @return an S4 group class specifying a bundle of criteria that at most x be satisfied in context of the cohort definition
#' @export
withAtMost <- function(x, ...){
  items <- list(...)
  res <- methods::new("Group",
      occurrence = methods::new("Occurrence", type = "atMost", count = as.integer(x)),
      criteria = purrr::discard(items, is.Group),
      group = purrr::keep(items, is.Group)
  )
  return(res)
}

# Coercion ----------------



## Coerce Occurrence -----

#function to determine code occurrence type
codeOccurrenceType <- function(x) {
  dplyr::case_when(
    x == "exactly" ~ 0L,
    x == "atMost" ~ 1L,
    x == "atLeast" ~ 2L,
    TRUE ~ NA_integer_
  )
}

setMethod("as.list", "Occurrence", function(x) {
  ll <- list('Type' = codeOccurrenceType(x@type),
             'Count' = x@count)
  if (!is.na(x@isDistinct) && isTRUE(x@isDistinct)) {
    ll[["IsDistinct"]] <- TRUE
  }
  if (!is.na(x@countColumn) && nzchar(x@countColumn)) {
    ll[["CountColumn"]] <- x@countColumn
  }
  return(ll)
})

## Coerce Criteria (Count) -----

setMethod("as.list", "Criteria", function(x) {
  ll <- list('Criteria' = as.list(x@query),
             'Occurrence' = as.list(x@occurrence)) |>
    append(as.list(x@aperture), after = 1)
  return(ll)
})

## Coerce Group -----

setMethod("as.list", "Group", function(x) {

  criteriaList <- purrr::keep(x@criteria, is.Criteria) |>
    purrr::map(~as.list(.x))
  # Circe expects one demographic criterion per group when combining age + gender (single WHERE branch)
  demoAttrs <- purrr::discard(x@criteria, is.Criteria)
  demographicsList <- if (length(demoAttrs) == 0L) {
    list()
  } else {
    merged <- purrr::map(demoAttrs, ~as.list(.x)) |> purrr::reduce(append)
    list(merged)
  }

  if (length(x@group) == 0) {
    groupsList <- list()
  } else {
    groupsList <- purrr::map(x@group, ~as.list(.x))
  }

  typeStr <- switch(x@occurrence@type,
    all = "ALL", any = "ANY", atLeast = "AT_LEAST", atMost = "AT_MOST",
    toupper(x@occurrence@type))
  ll <- list('Type' = typeStr,
             'Count' = x@occurrence@count,
             'CriteriaList' = criteriaList,
             'DemographicCriteriaList' = demographicsList,
             'Groups' = groupsList)
  if (is.na(ll$Count)) {
    ll$Count <- NULL
  }

  return(ll)
})
