# Utilities ---------------
getGuid <- function(x) {
  tibble::tibble(
    guid = x@conceptSet@id
  )
}

replaceGuid <- function(x, y) {
  x@conceptSet@id <- y
  return(x)
}


# Collect Guid --------------------------


setGeneric("collectGuid", function(x) standardGeneric("collectGuid"))

#' @include query.R
setMethod("collectGuid", "Query", function(x) {
  getGuid(x)
})

#' @include criteria.R
setMethod("collectGuid", "Criteria", function(x) {
  collectGuid(x@query)
})

#' @include criteria.R
setMethod("collectGuid", "Group", function(x) {
  purrr::map(x@criteria, ~collectGuid(.x)) %>%
    append(purrr::map(x@group, ~collectGuid(.x)))
})



setMethod("collectGuid", "CohortEntry", function(x) {
  purrr::map(x@entryEvents, ~collectGuid(.x)) %>%
    append(collectGuid(x@additionalCriteria))
  #TODO may need a flatten here with additional criteria
})

setMethod("collectGuid", "CohortAttrition", function(x) {
  purrr::map(unname(x@rules), ~collectGuid(.x)) %>%
    purrr::flatten()
})

setMethod("collectGuid", "CohortExit", function(x) {
  #check if endstrategy is drug exit
  es_nm_check <- "conceptSet" %in% methods::slotNames(methods::is(x@endStrategy))
  if (es_nm_check) {
    #get concept sets from drug exit
    ll <- list(getGuid(x@endStrategy))
  } else {
    ll <- list()
  }
  append(ll, purrr::map(x@censoringCriteria@criteria, ~collectGuid(.x)))

})

setMethod("collectGuid", "Cohort", function(x) {

  collectGuid(x@entry) %>%
    append(collectGuid(x@attrition)) %>%
    append(collectGuid(x@exit)) %>%
    purrr::map_dfr(~.x) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      codesetId = dplyr::row_number() - 1,
      codesetId = as.integer(codesetId)
    )

})

# Replace CodesetId -----------------------
## TODO HASH table implementation of find/replace
setGeneric("replaceCodesetId", function(x, guidTable) standardGeneric("replaceCodesetId"))

setMethod("replaceCodesetId", "Query", function(x, guidTable) {

  y <- getGuid(x) %>%
    dplyr::inner_join(guidTable, by = c("guid")) %>%
    pull(codesetId)

  x <- replaceGuid(x, y)

  return(x)
})

setMethod("replaceCodesetId", "DrugExposureExit", function(x, guidTable) {

  y <- getGuid(x) %>%
    dplyr::inner_join(guidTable, by = c("guid")) %>%
    pull(codesetId)

  x <- replaceGuid(x, y)

  return(x)
})


setMethod("replaceCodesetId", "Criteria", function(x, guidTable) {

  x@query <- replaceCodesetId(x@query, guidTable = guidTable)

  return(x)
})


setMethod("replaceCodesetId", "Group", function(x, guidTable) {
  crit <- purrr::map(x@criteria, ~replaceCodesetId(.x, guidTable))
  grp <- purrr::map(x@group, ~replaceCodesetId(.x, guidTable))

  x@criteria <- crit
  x@group <- grp

  return(x)
})


setMethod("replaceCodesetId", "CohortEntry", function(x, guidTable) {
  pc <- purrr::map(x@entryEvents, ~replaceCodesetId(.x, guidTable))
  ac <- replaceCodesetId(x@additionalCriteria, guidTable)

  x@entryEvents <- pc
  x@additionalCriteria <- ac
  return(x)

})

setMethod("replaceCodesetId", "CohortAttrition", function(x, guidTable = guidTable) {
  irs <- purrr::map(x@rules, ~replaceCodesetId(.x, guidTable))
  x@rules <- irs
  return(x)
})


setMethod("replaceCodesetId", "CohortExit", function(x, guidTable = guidTable) {
  #check if endstrategy is drug exit
  es_nm_check <- "conceptSet" %in% methods::slotNames(methods::is(x@endStrategy))
  if (es_nm_check) {
    #get concept sets from drug exit
    es <- replaceCodesetId(x@endStrategy, guidTable = guidTable)
    x@endStrategy <- es
  }
  # Censoring Events replace
  cen <- purrr::map(x@censoringCriteria@criteria, ~replaceCodesetId(.x, guidTable))
  x@censoringCriteria@criteria <- cen
  return(x)

})


setMethod("replaceCodesetId", "Cohort", function(x, guidTable = guidTable) {

  x@entry <- replaceCodesetId(x@entry, guidTable = guidTable)
  x@attrition <- replaceCodesetId(x@attrition, guidTable = guidTable)
  x@exit <- replaceCodesetId(x@exit, guidTable = guidTable)

  return(x)
})

# list Concept Set ------------------

setGeneric("listConceptSets", function(x) standardGeneric("listConceptSets"))

#' @include query.R
setMethod("listConceptSets", "Query", function(x) {
  as.list(x@conceptSet)
})

#' @include criteria.R
setMethod("listConceptSets", "Criteria", function(x) {
  listConceptSets(x@query)
})

#' @include criteria.R
setMethod("listConceptSets", "Group", function(x) {
  purrr::map(x@criteria, ~listConceptSets(.x)) %>%
    append(purrr::map(x@group, ~listConceptSets(.x)))
})



setMethod("listConceptSets", "CohortEntry", function(x) {
  purrr::map(x@entryEvents, ~listConceptSets(.x)) %>%
    append(listConceptSets(x@additionalCriteria))
  #TODO may need a flatten here with additional criteria
})

setMethod("listConceptSets", "CohortAttrition", function(x) {
  purrr::map(unname(x@rules), ~listConceptSets(.x)) %>%
    purrr::flatten()
})

setMethod("listConceptSets", "CohortExit", function(x) {
  #check if endstrategy is drug exit
  es_nm_check <- "conceptSet" %in% methods::slotNames(methods::is(x@endStrategy))
  if (es_nm_check) {
    #get concept sets from drug exit
    ll <- list(as.list(x@endStrategy@conceptSet))
  } else {
    ll <- list()
  }
  append(ll, purrr::map(x@censoringCriteria@criteria, ~listConceptSets(.x)))

})

setMethod("listConceptSets", "Cohort", function(x) {
  ll <- listConceptSets(x@entry) %>%
    append(listConceptSets(x@attrition)) %>%
    append(listConceptSets(x@exit))

  #TODO Remove duplicates

  return(ll)

})
