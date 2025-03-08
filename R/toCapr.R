conceptSetToCapr <- function(cs) {
  #get concept ids from concept set expressions
  cid <- purrr::map(purrr::map(cs, function(x) x$expression$items),
                    function(y) purrr::map_int(y, function(z) z$concept$CONCEPT_ID))
  #get names from concept set expressions
  nm <- purrr::map(cs, function(x) x$name)
  # turn into Capr concept set
  caprCs <- purrr::map2(cid, nm, ~cs(.x, name = .y))

  #get mapping from concept set exrpessions (i.e includeDescendants, isExcluded)
  #if there is no mapping the function will return a NULL
  mapping <- purrr::map(purrr::map(cs, function(x) x$expression$items),
                        function(y) purrr::map(y, function(z) names(z)[names(z) %notin% "concept"]))


  getMappings <- function(x) {
    list('includeDescendants' = "includeDescendants" %in% x,
         'isExcluded' = "isExcluded" %in% x,
         'includeMapped' = "includeMapped" %in% x)
  }

  #run the get mappings function
  mapping <- purrr::map(mapping, function(x) purrr::map(x, getMappings))

  for (i in seq_along(caprCs)) {
    for (j in seq_along(caprCs[[i]]@Expression)) {
      caprCs[[i]]@Expression[[j]]@includeDescendants <- mapping[[i]][[j]]$includeDescendants
      caprCs[[i]]@Expression[[j]]@isExcluded <- mapping[[i]][[j]]$isExcluded
      caprCs[[i]]@Expression[[j]]@includeMapped <- mapping[[i]][[j]]$includeMapped
    }
  }

  return(caprCs)

}


getCsKey <- function(cs, caprCs) {
  oid <- purrr::map_int(cs, ~.x$id)
  caprHash <- purrr::map_chr(caprCs, ~.x@id)
  tb <- tibble::tibble(
    oid = oid,
    caprHash = caprHash
  ) |>
    dplyr::mutate(
      idx = dplyr::row_number(), .before = 1
    )
  return(tb)
}

# function to turn a circe query into capr
queryToCapr <- function(x, caprCs, csTb) {

  # get the capr domain call
  domain <- names(x) |> SqlRender::snakeCaseToCamelCase()

  # get the caprCs index of the concept set
  idx <- csTb |>
    dplyr::filter(
      oid == x[[1]]$CodesetId
    ) |>
    dplyr::pull(idx)

  # pull out the query cs
  queryCs <- caprCs[[idx]]

  # make a query call
  queryCall <- rlang::call2(
    domain,
    queryCs
    #TODO add attributes to query call
  )

  res <- eval(queryCall)

  return(res)

}

#function to turn a circe count into a capr criteria
criteriaToCapr <- function(x, caprCs, csTb) {
  # prep query
  criteriaQuery <- queryToCapr(x$Criteria, caprCs = caprCs, csTb = csTb)

  # prep aperture
  es <- eventStarts(
    a = x$StartWindow$Start$Days * x$StartWindow$Start$Coeff,
    b = x$StartWindow$End$Days * x$StartWindow$End$Coeff,
    index = ifelse(x$StartWindow$UseEventEnd, "endDate", "startDate")
  )

  if (!is.null(x$EndWindow)) {
    en <- eventStarts(
      a = x$EndWindow$Start$Days * x$EndWindow$Start$Coeff,
      b = x$EndWindow$End$Days * x$EndWindow$End$Coeff,
      index = ifelse(x$EndWindow$UseEventEnd, "endDate", "startDate")
    )
  } else {
    en <- NULL
  }

  # create duration interval
  di <- duringInterval(
    startWindow = es,
    endWindow = en,
    restrictVisit = ifelse(!is.null(x$RestrictVisit), TRUE, FALSE),
    ignoreObservationPeriod = ifelse(!is.null(x$IgnoreObservationPeriod), TRUE, FALSE)
  )

  # identify the occurrence type capr call
  tt <- as.character(x$Occurrence$Type)
  caprCall <- switch(
    tt,
    '0' = "exactly",
    '1' = "atMost",
    '2' = "atLeast"
  )

  criteriaCall <- rlang::call2(
    caprCall,
    x = x$Occurrence$Count,
    query = criteriaQuery,
    aperture = di
  )
  res <- eval(criteriaCall)

  return(res)
}

#function to turn circe group into capr group
groupToCapr <- function(x, caprCs, csTb) {

}

# function to turn a circe primary criteria into capr
primaryCriteriaToCapr <- function(cd) {

  # get concept sets for arrangement
  cs <- cd$ConceptSets
  caprCs <- conceptSetToCapr(cs)
  csTb <- getCsKey(cs, caprCs)

  pc <- cd$PrimaryCriteria

  #get the query Calls
  pcQueries <- purrr::map(pc$CriteriaList, ~queryToCapr(.x, caprCs = caprCs, csTb = csTb))

  ctsObs <- continuousObservation(
    priorDays = pc$ObservationWindow$PriorDays,
    postDays = pc$ObservationWindow$PostDays
  )
  pcL <- pc$PrimaryCriteriaLimit$Type


  pcCall <- rlang::call2(
    "entry",
    !!!pcQueries
  )
  res <- eval(pcCall)

  res@observationWindow <- ctsObs
  res@primaryCriteriaLimit <- pcL
  res@qualifiedLimit <- cd$QualifiedLimit$Type

  return(res)

}

# function to covert inclusion rules to Capr
inclusionRulesToCapr <- function(cd) {
  # get concept sets for arrangement
  cs <- cd$ConceptSets
  caprCs <- conceptSetToCapr(cs)
  csTb <- getCsKey(cs, caprCs)

  ir <- cd$InclusionRules
}



# function to covert end strategy and censor to Capr
exitToCapr <- function(cd) {
  # get concept sets for arrangement
  cs <- cd$ConceptSets
  caprCs <- conceptSetToCapr(cs)
  csTb <- getCsKey(cs, caprCs)

  censor <- cd$CensoringCriteria
}

# function to convert collapse to Capr
eraToCapr <- function(cd) {

  res <- era(
    eraDays = cd$CollapseSettings$EraPad,
    studyStartDate = cd$CensorWindow$StartDate,
    studyEndDate = cd$CensorWindow$EndDate
  )

  return(res)
}


#' Read a circe json definition and convert it to a Capr cohort definition
#' @export
#' @param path character string that must be a valid path to read json from
readInCirce <- function(path) {
  checkmate::assertFileExists(path)
  circeObj <- jsonlite::fromJSON(path)
  checkmate::assertList(circeObj)
  entry <- primaryCriteriaToCapr(circeObj)

  Capr::cohort(entry = entry)
  return(caprDef)
}

