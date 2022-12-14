setMethod("collectConceptSets", "Query", function(x) {
  tmp <- x@conceptSet
  as.list(tmp)
})


setMethod("collectConceptSets", "Count", function(x) {
  collectConceptSets(x@query)
})

setMethod("collectConceptSets", "Group", function(x) {
  purrr::map(x@criteria, ~collectConceptSets(.x))
})

