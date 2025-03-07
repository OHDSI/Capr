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


# setMethod("collectGuid", "Query", function(x) {
#   getGuid(x)
# })
#
# setMethod("collectGuid", "nestedAttribute", function(x) {
#   collectGuid(x@group)
# })

#' @include query.R
setMethod("collectGuid", "Query", function(x) {
  ids <- getGuid(x)

  #collect guids for nested attributes
  checkNest <- purrr::map_chr(x@attributes, ~as.character(.x@name))
  if (any(checkNest %in% c("CorrelatedCriteria"))) {
    ii <- which(checkNest == "CorrelatedCriteria")
    id2 <- collectGuid(x@attributes[[ii]]@group) |>
      purrr::flatten()

    ids <- dplyr::bind_rows(ids, id2)
  }
  return(ids)

})

#' @include criteria.R
setMethod("collectGuid", "Criteria", function(x) {
  collectGuid(x@query)
})

#' @include criteria.R
setMethod("collectGuid", "Group", function(x) {
  purrr::map(x@criteria, ~collectGuid(.x)) |>
    append(purrr::map(x@group, ~collectGuid(.x)))
})


setMethod("collectGuid", "CohortEntry", function(x) {
  purrr::map(x@entryEvents, ~collectGuid(.x)) |>
    append(collectGuid(x@additionalCriteria))
  #TODO may need a flatten here with additional criteria
})

setMethod("collectGuid", "CohortAttrition", function(x) {
  purrr::map(unname(x@rules), ~collectGuid(.x)) |>
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

  tt <- collectGuid(x@entry) |>
    append(collectGuid(x@attrition)) |>
    append(collectGuid(x@exit)) |>
    unlist() |>
    unname() |>
    unique()

  tb <- tibble::tibble(guid = tt) |>
    dplyr::mutate(
      codesetId = dplyr::row_number() - 1,
      codesetId = as.integer(.data$codesetId)
    )

  return(tb)
})

# Replace CodesetId -----------------------
## TODO HASH table implementation of find/replace
setGeneric("replaceCodesetId", function(x, guidTable) standardGeneric("replaceCodesetId"))


setMethod("replaceCodesetId", "Query", function(x, guidTable) {

  if (nrow(getGuid(x)) > 0) {
    y <- getGuid(x) |>
      dplyr::inner_join(guidTable, by = c("guid")) |>
      dplyr::pull(.data$codesetId)
  } else {
    y <- NULL
  }

  #first replace the query id
  x <- replaceGuid(x, y)

  #next check for any nested criteria and replace
  #replace guids for nested attributes
  checkNest <- purrr::map_chr(x@attributes, ~as.character(.x@name))
  if (any(checkNest %in% c("CorrelatedCriteria"))) {
    ii <- which(checkNest == "CorrelatedCriteria")
    nest <- replaceCodesetId(x@attributes[[ii]]@group, guidTable)

    x@attributes[[ii]]@group <- nest
  }

  return(x)
})

setMethod("replaceCodesetId", "DrugExposureExit", function(x, guidTable) {

  y <- getGuid(x) |>
    dplyr::inner_join(guidTable, by = c("guid")) |>
    dplyr::pull(.data$codesetId)

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
  qs <- as.list(x@conceptSet)

  # handle listing concepts if have nestedAttribute
  #next check for any nested criteria and replace
  #replace guids for nested attributes
  checkNest <- purrr::map_chr(x@attributes, ~as.character(.x@name))
  if (any(checkNest %in% c("CorrelatedCriteria"))) {
    ii <- which(checkNest == "CorrelatedCriteria")
    nest <- listConceptSets(x@attributes[[ii]]@group)
    out <- c(list(qs), nest)
  } else {
    out <- list(qs)
  }

  return(out)
})

#' @include criteria.R
setMethod("listConceptSets", "Criteria", function(x) {
 listConceptSets(x@query)
})

check_names <- function(x) {
  check <- names(x) %in% c("id", "name", "expression")
  if (length(check) == 0) {
    FALSE
  } else{
    all(check)
  }
}

#' @include criteria.R
setMethod("listConceptSets", "Group", function(x) {

  #Start with criteria
  a <- purrr::map(x@criteria, ~listConceptSets(.x))
  if (length(a) == 0) {
    ll1 <- list()
  } else {
    la1 <- purrr::keep(a, ~check_names(.x))
    la2 <- purrr::discard(a, ~check_names(.x))
    if (length(la2) > 0) {
      la2 <- purrr::list_flatten(la2)
    }

    ll1 <- c(la1, la2)
  }

  # Next Group
  b <- purrr::map(x@group, ~listConceptSets(.x))
  if (length(b) == 0) {
    ll2 <- list()
  } else {
    lb1 <- purrr::keep(b, ~check_names(.x))
    lb2 <- purrr::discard(b, ~check_names(.x))
    if (length(lb2) > 0) {
      lb2 <- purrr::list_flatten(lb2)
    }

    ll2 <- c(lb1, lb2)
  }

  c(ll1, ll2)
})

setMethod("listConceptSets", "CohortEntry", function(x) {

  ce <- purrr::map(x@entryEvents, ~listConceptSets(.x))
  check <- purrr::map_int(ce, ~length(.x))
  if (!all(check == 3)) {
    ce <- ce |>
      purrr::flatten()
  }

  ce |>
    append(listConceptSets(x@additionalCriteria))
  # TODO may need a flatten here with additional criteria
})

setMethod("listConceptSets", "CohortAttrition", function(x) {
  purrr::map(unname(x@rules), ~listConceptSets(.x)) |>
    purrr::list_flatten()
})

setMethod("listConceptSets", "CohortExit", function(x) {
  # check if endstrategy is drug exit
  es_nm_check <- "conceptSet" %in% methods::slotNames(methods::is(x@endStrategy))
  if (es_nm_check) {
    # get concept sets from drug exit
    ll <- list(as.list(x@endStrategy@conceptSet))
  } else {
    ll <- list()
  }
  # get concept sets from censoring criteria
  ll2 <- purrr::map(x@censoringCriteria@criteria,
                    ~listConceptSets(.x) |>
                      purrr::flatten())

  res <- append(ll, ll2)
  return(res)
})

setMethod("listConceptSets", "Cohort", function(x) {
  l1 <- listConceptSets(x@entry)
  l2 <- listConceptSets(x@attrition)
  l3 <- listConceptSets(x@exit)
  ll <- c(l1, l2, l3) |>
    .removeNullId()

  ids <- purrr::map_chr(ll, ~as.character(.x$id))

  rr <- ll[!duplicated(ids)]
  return(rr)
})


.removeNullId <- function(ll) {
  idToDrop <- purrr::map_lgl(ll, ~is.null(.x$id)) |>
    which()

  ll2 <- ll[-c(idToDrop)]
  return(ll2)
}
