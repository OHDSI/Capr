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
criteriaToCapr <- function() {}


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