# Nested Attribute class ----------------------------

#' An S4 class for a nested attribute
#' @slot
#' name the name of the attribute
#' @slot
#' conceptSet a list representing the concepts for the attribute
# @include criteria.R
setClass("nestedAttribute",
         slots = c(name = "character", group = "Group"),
         prototype = list(name = "CorrelatedCriteria",
  group = methods::new("Group")))


# Constructor -------------------

#' Function to construct a nested group where all criteria and groups must be satisfied
#' @param ...   a set of criteria or groups
#' @return an S4 group class specifying a bundle of criteria that all must be satisfied in context of the cohort definition
#' @export
nestedWithAll <- function(...) {
  items <- list(...)
  methods::new("nestedAttribute",
               group = methods::new("Group", occurrence = methods::new("Occurrence",
    type = "all"), criteria = purrr::discard(items, is.Group), group = purrr::keep(items, is.Group)))

}

#' Function to construct a nested group where any criteria and groups may be satisfied
#' @param ...   a set of criteria or groups
#' @return an S4 group class specifying a bundle of criteria that any may be satisfied in context of the cohort definition
#' @export
nestedWithAny <- function(...) {
  items <- list(...)
  methods::new("nestedAttribute",
               group = methods::new("Group", occurrence = methods::new("Occurrence",
    type = "any"), criteria = purrr::discard(items, is.Group), group = purrr::keep(items, is.Group)))
}

#' Function to construct a nested group where at least some of the criteria or groups must be
#' satisfied
#' @param x     an integer specifying the number of criteria or groups that must be satisfied
#' @param ...   a set of criteria or groups
#' @return an S4 group class specifying a bundle of criteria that at least x be satisfied in context of the cohort definition
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
#' @return an S4 group class specifying a bundle of criteria that at most x be satisfied in context of the cohort definition
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
