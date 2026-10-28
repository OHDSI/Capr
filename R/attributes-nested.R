# Nested Attribute class ----------------------------

#' An S4 class for a nested attribute
#' @slot
#' name the name of the attribute
#' @slot
#' conceptSet a list representing the concepts for the attribute
#'
#' @include criteria.R
setClass("nestedAttribute",
         slots = c(name = "character", group = "Group"),
         prototype = list(name = "CorrelatedCriteria",
  group = methods::new("Group")))


# Constructor -------------------

#' Construct a nested group where all criteria and groups must be satisfied
#' @param ...   a set of criteria or groups
#'
#' @export
nestedWithAll <- function(...) {
  items <- list(...)
  methods::new("nestedAttribute",
               group = methods::new("Group",
                                    occurrence = methods::new("Occurrence", type = "all"),
                                    criteria = purrr::discard(items, is.Group),
                                    group = purrr::keep(items, is.Group)))
}

#' Function to construct a nested group where any criteria and groups may be satisfied
#' @param ...   a set of criteria or groups
#' @export
nestedWithAny <- function(...) {
  items <- list(...)
  methods::new("nestedAttribute",
               group = methods::new("Group",
                                    occurrence = methods::new("Occurrence", type = "any"),
                                    criteria = purrr::discard(items, is.Group),
                                    group = purrr::keep(items, is.Group)))
}

#' Function to construct a nested group where at least some of the criteria or groups must be
#' satisfied
#' @param x     an integer specifying the number of criteria or groups that must be satisfied
#' @param ...   a set of criteria or groups
#' @export
nestedWithAtLeast <- function(x, ...) {
  items <- list(...)
  methods::new("nestedAttribute",
               group = methods::new("Group", occurrence = methods::new("Occurrence",
    type = "atLeast", count = as.integer(x)), criteria = purrr::discard(items, is.Group), group = purrr::keep(items,
    is.Group)))
}
#' Function to construct a nested group where at most some of the criteria or groups must be satisfied
#' @param x     an integer specifying the number of criteria or groups that must be satisfied
#' @param ...   a set of criteria or groups
#' @export
nestedWithAtMost <- function(x, ...) {
  items <- list(...)
  methods::new("nestedAttribute",
               group = methods::new("Group", occurrence = methods::new("Occurrence",
    type = "atMost", count = as.integer(x)), criteria = purrr::discard(items, is.Group), group = purrr::keep(items,
    is.Group)))
}

# Coercion -------------------

setMethod("as.list", "nestedAttribute", function(x) {
  nm <- x@name
  tibble::lst(`:=`(!!nm, as.list(x@group)))
})

# Custom Era attributes ----------------------------

#' An S4 class for a CustomEra criteria list
#' @slot criteria a list of Query objects
setClass("criteriaListAttribute",
         slots = c(name = "character", criteria = "list"),
         prototype = list(name = "CriteriaList", criteria = list()))

#' An S4 class for a CustomEra gap-days value
#' @slot value integer number of gap days
setClass("gapDaysAttribute",
         slots = c(name = "character", value = "integer"),
         prototype = list(name = "GapDays", value = 0L))

#' Construct the criteria list for a custom era
#' @param criteria A list of \code{Query} objects.
#' @return An attribute for use in \code{\link{customEra}()}.
#' @export
customEraCriteria <- function(criteria) {
  if (!is.list(criteria) || length(criteria) == 0) {
    stop("criteria must be a non-empty list of Query objects.")
  }
  isQuery <- vapply(criteria, function(x) methods::is(x, "Query"), logical(1))
  if (!all(isQuery)) {
    stop("criteria must contain only Query objects.")
  }
  methods::new("criteriaListAttribute", criteria = criteria)
}

#' Set the gap days for a custom era
#' @param x Integer number of days between adjacent events to merge.
#' @return An attribute for use in \code{\link{customEra}()}.
#' @export
customEraGapDays <- function(x) {
  x <- as.integer(x)
  if (length(x) != 1L || is.na(x) || x < 0L) {
    stop("x must be a single non-negative integer.")
  }
  methods::new("gapDaysAttribute", value = x)
}

setMethod("as.list", "criteriaListAttribute", function(x) {
  list(CriteriaList = lapply(x@criteria, as.list))
})

setMethod("as.list", "gapDaysAttribute", function(x) {
  list(GapDays = as.integer(x@value))
})
