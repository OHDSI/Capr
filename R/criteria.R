# Classes -----------------------

## Occurrence --------------

#' An S4 class for an occurrence.
#' @description This determines how many events need to occur to count the criteria
#' in the cohort definition (relative to the index event)
#' @slot type a character string determine the logic for counting occurrences. Can be
#' all, any, exactly, atLeast, or atMost
#' @slot count an integer specifying the number of occurrences for a criteria
setClass("Occurrence",
         slots = c(
           type = "character",
           count = "integer"
         ),
         prototype = list(
           type = NA_character_,
           count = NA_integer_
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
  methods::is(x) == "Criteria"
}

is.Group <- function(x) {
  methods::is(x) == "Group"
}

# Constructors -----------------------

## Criteria ----------------

#' Function to enumerate an exact count of occurrences
#' @param x the integer counting the number of occurrences
#' @param query a query object that provides context to the clinical event of interest
#' @param aperture an eventAperture object that shows the temporal span where the event is to be observed
#' relative to the index event
#' @export
exactly <- function(x,
                    query,
                    aperture = duringInterval(eventStarts(-Inf, Inf))) {

  if (methods::is(aperture, "EventWindow")) {
    aperture <- duringInterval(aperture)
  }

  occurrence <- methods::new("Occurrence",
      type = "exactly",
      count = as.integer(x))

  res <- methods::new("Criteria",
      occurrence = occurrence,
      query = query,
      aperture = aperture)
  return(res)
}

#' Function to enumerate an minimal count of occurrences
#' @param x the integer counting the number of occurrences
#' @param query a query object that provides context to the clinical event of interest
#' @param aperture an eventAperture object that shows the temporal span where the event is to be observed
#' relative to the index event
#' @export
atLeast <- function(x,
                    query,
                    aperture = duringInterval(eventStarts(-Inf, Inf))) {

  if (methods::is(aperture, "EventWindow")) {
    aperture <- duringInterval(aperture)
  }

  occurrence <- methods::new("Occurrence",
      type = "atLeast",
      count = as.integer(x))

  res <- methods::new("Criteria",
      occurrence = occurrence,
      query = query,
      aperture = aperture)
  return(res)
}

#' Function to enumerate a maximum count of occurrences
#' @param x the integer counting the number of occurrences
#' @param query a query object that provides context to the clinical event of interest
#' @param aperture an eventAperture object that shows the temporal span where the event is to be observed
#' relative to the index event
#' @export
atMost <- function(x,
                   query,
                   aperture = duringInterval(eventStarts(-Inf, Inf))) {

  if (methods::is(aperture, "EventWindow")) {
    aperture <- duringInterval(aperture)
  }

  occurrence <- methods::new("Occurrence",
      type = "atMost",
      count = as.integer(x))

  res <- methods::new("Criteria",
      occurrence = occurrence,
      query = query,
      aperture = aperture)
  return(res)
}

## Group -------

#' Function to construct a group where all criteria and groups must be satisfied
#' @param ... a set of criteria or groups
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
  return(ll)

})

## Coerce Criteria (Count) -----

setMethod("as.list", "Criteria", function(x) {
  ll <- list('Criteria' = as.list(x@query),
             'Occurrence' = as.list(x@occurrence)) %>%
    append(as.list(x@aperture), after = 1)
  return(ll)
})

## Coerce Group -----

setMethod("as.list", "Group", function(x) {

  criteriaList <- purrr::keep(x@criteria, is.Criteria) %>%
    purrr::map(~as.list(.x))
  demographicsList <- purrr::discard(x@criteria, is.Criteria) %>%
    purrr::map(~as.list(.x))

  if (length(x@group) == 0) {
    groupsList <- list()
  } else {
    groupsList <- purrr::map(x@group, ~as.list(.x))
  }

  ll <- list('Type' = toupper(x@occurrence@type),
             'Count' = x@occurrence@count,
             'CriteriaList' = criteriaList,
             'DemographicCriteriaList' = demographicsList,
             'Groups' = groupsList)
  if (is.na(ll$Count)) {
    ll$Count <- NULL
  }

  return(ll)
})
