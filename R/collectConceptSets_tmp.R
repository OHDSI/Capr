setGeneric("collectConceptSets", function(x) standardGeneric("collectConceptSets"))

#' @include query.R
setMethod("collectConceptSets", "Query", function(x) {
  x@conceptSet
})

#' @include count.R
setMethod("collectConceptSets", "Criteria", function(x) {
  collectConceptSets(x@query)
})

#' @include count.R
setMethod("collectConceptSets", "Group", function(x) {
  purrr::map(x@criteria, ~collectConceptSets(.x))
})

