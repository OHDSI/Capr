setGeneric("collectConceptSets", function(x) standardGeneric("collectConceptSets"))

#' @include query.R
setMethod("collectConceptSets", "Query", function(x) {
 as.list(x@conceptSet)
})

#' @include criteria.R
setMethod("collectConceptSets", "Criteria", function(x) {
  collectConceptSets(x@query)
})

#' @include criteria.R
setMethod("collectConceptSets", "Group", function(x) {
  purrr::map(x@criteria, ~collectConceptSets(.x)) %>%
    append(purrr::map(x@group, ~collectConceptSets(.x)))
})



setMethod("collectConceptSets", "CohortEntry", function(x) {
  purrr::map(x@entryEvents, ~collectConceptSets(.x)) %>%
    append(collectConceptSets(x@additionalCriteria))
  #TODO may need a flatten here with additional criteria
})

setMethod("collectConceptSets", "CohortAttrition", function(x) {
  purrr::map(unname(x@rules), ~collectConceptSets(.x)) %>%
    purrr::flatten()
})

setMethod("collectConceptSets", "CohortExit", function(x) {
  #check if endstrategy is drug exit
  es_nm_check <- "conceptSet" %in% methods::slotNames(methods::is(x@endStrategy))
  if (es_nm_check) {
    #get concept sets from drug exit
    ll <- list(as.list(x@endStrategy@conceptSet))
  } else {
    ll <- list()
  }
  append(ll, purrr::map(x@censoringCriteria@criteria, ~collectConceptSets(.x)))

})

setMethod("collectConceptSets", "Cohort", function(x) {
  ll <- collectConceptSets(x@entry) %>%
    append(collectConceptSets(x@attrition)) %>%
    append(collectConceptSets(x@exit))

  # get all guids
  tbl <- tibble::tibble(
    guid = purrr::map_chr(ll, ~.x$id),
    name = purrr::map_chr(ll, ~.x$name)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      circeId = seq_along(guid) - 1L,
      name = ifelse(name == "", paste0("conceptSet", circeId))
    ) %>%
    dplyr::relocate(circeId, name, guid)



  return(ll)

})


