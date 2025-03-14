# Concept Set -----------------
# function to coerce concept sets to capr cs
conceptSetToCapr <- function(ccs) {
  #get concept ids from concept set expressions
  cid <- purrr::map(purrr::map(ccs, function(x) x$expression$items),
                    function(y) purrr::map_int(y, function(z) z$concept$CONCEPT_ID))
  #get names from concept set expressions
  nm <- purrr::map(ccs, function(x) x$name)
  # turn into Capr concept set
  caprCs <- purrr::map2(cid, nm, ~cs(.x, name = .y))

  #get mapping from concept set exrpessions (i.e includeDescendants, isExcluded)
  #if there is no mapping the function will return a NULL
  `%notin%` <- Negate("%in%")
  mapping <- purrr::map(purrr::map(ccs, function(x) x$expression$items),
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

# function to create concept set merge key
getCsKey <- function(ccs, caprCs) {
  #get the old id
  oid <- purrr::map_int(ccs, ~.x$id)
  #find new capr hash
  caprHash <- purrr::map_chr(caprCs, ~.x@id)
  #make a merge key
  tb <- tibble::tibble(
    oid = oid,
    caprHash = caprHash
  ) |>
    dplyr::mutate(
      #order by apperance
      idx = dplyr::row_number(), .before = 1
    )
  return(tb)
}
 # Attributes -------------------------

conceptAttributeToCapr <- function(attribute, name, type = NULL) {

  cl <- purrr::map(
    attribute,
    ~newConcept(
      id = .x$CONCEPT_ID,
      conceptName = .x$CONCEPT_NAME,
      conceptCode = .x$CONCEPT_CODE,
      domainId = .x$DOMAIN_ID,
      invalidReasonCaption = .x$INVALID_REASON_CAPTION,
      standardConceptCaption = .x$STANDARD_CONCEPT_CAPTION,
      vocabularyId = .x$VOCABULARY_ID
    )@Concept
  )
  ca <- methods::new("conceptAttribute", name = name, conceptSet = cl)
  return(ca)
}

dateAdjustmentAttributeToCapr <- function(attribute, name, type = NULL) {
  daa <- methods::new("dateAdjustmentAttribute",
               startWith = attribute$StartWith,
               startOffset = attribute$StartOffset,
               endWith = attribute$EndWith,
               endOffset = attribute$EndOffset)
  return(daa)
}

logicAttributeToCapr <- function(attribute, name, type = NULL) {
  la <- methods::new("logicAttribute", name = name)
  return(la)
}


opAttributeToCapr <- function(attribute, name, type) {

  if (type == "numeric") {
    if (is.null(attribute$Extent)) {
      extent <- NA_real_
    }

    opa <- methods::new("opAttributeNumeric",
                        name = name,
                        op = attribute$Op,
                        value = attribute$Value,
                        extent = extent)
  }

  if (type == "integer") {
    if (is.null(attribute$Extent)) {
      extent <- NA_integer_
    } else{
      extent <- as.integer(attribute$Extent)
    }

    opa <- methods::new("opAttributeInteger",
                        name = name,
                        op = attribute$Op,
                        value = as.integer(attribute$Value),
                        extent = extent)
  }

  if (type == "date") {
    if (is.null(attribute$Extent)) {
      extent <- lubridate::NA_Date_
    } else {
      extent <- lubridate::as_date(attribute$Extent)
    }

    opa <- methods::new("opAttributeDate",
                        name = name,
                        op = attribute$Op,
                        value = lubridate::as_date(attribute$Value),
                        extent = extent)
  }

  return(opa)

}

attributesToCapr <- function(x) {

  #get the attribute names
  attributeNames <- names(x)[which(names(x) != "CodesetId")]
  # get the attribute list
  circeAttributeList <- x[which(names(x) != "CodesetId")]

  # pull the merge key
  attributeKey <- readr::read_csv(
    file = fs::path_package(package = "Capr", "csv/attributeKey.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::filter(
      attributeName %in% attributeNames
    )

  caprAttributeList <- vector('list', length = length(circeAttributeList))
  for (i in seq_along(caprAttributeList)) {
    caprAttrFn <- attributeKey$functionCall[i]
    name <- attributeKey$attributeName[i]
    if (!is.na(attributeKey$attributeType[i])) {
      type <- attributeKey$attributeType[i]
    } else {
      type <- NULL
    }
    caprAttrCall <- rlang::call2(
      caprAttrFn,
      attribute = circeAttributeList[[i]],
      name = name,
      type = type
    )
    caprAttributeList[[i]] <- eval(caprAttrCall)
  }

  return(caprAttributeList)
}

# Query --------------------

# function to turn a circe query into capr
queryToCapr <- function(x, caprCs, csTb) {

  # get the capr domain call
  domain <- names(x) |> snakecase::to_lower_camel_case()

  # get attributes if any
  attributeList <- attributesToCapr(x[[1]])

  # get the caprCs index of the concept set
  if (any(names(x[[1]]) == "CodesetId")) {
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
      conceptSet = queryCs,
      !!!attributeList
    )

  } else {
    # make a query call
    queryCall <- rlang::call2(
      domain,
      !!!attributeList
    )
  }


  res <- eval(queryCall)

  return(res)

}

# Criteria ----------------------------
#function to turn a circe count into a capr criteria
criteriaToCapr <- function(x, caprCs, csTb) {
  # prep query
  criteriaQuery <- queryToCapr(x$Criteria, caprCs = caprCs, csTb = csTb)

  dy <- ifelse(is.null(x$StartWindow$Start$Days), Inf, x$StartWindow$Start$Days)
  # prep aperture
  es <- eventStarts(
    a = ifelse(is.null(x$StartWindow$Start$Days), Inf, x$StartWindow$Start$Days) * x$StartWindow$Start$Coeff,
    b = ifelse(is.null(x$StartWindow$End$Days), Inf, x$StartWindow$End$Days) * x$StartWindow$End$Coeff,
    index = ifelse(x$StartWindow$UseEventEnd, "endDate", "startDate")
  )

  if (!is.null(x$EndWindow)) {
    en <- eventStarts(
      a = ifelse(is.null(x$EndWindow$Start$Days), Inf, x$EndWindow$Start$Days) * x$EndWindow$Start$Coeff,
      b = ifelse(is.null(x$EndWindow$End$Days), Inf, x$EndWindow$End$Days) * x$EndWindow$End$Coeff,
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

# Group -----------------------------
#function to turn circe group into capr group
# NOT DONE!!!!!
groupToCapr <- function(x, caprCs, csTb) {
  # get the fn call
  tt <- as.character(x$Type)
  caprCall <- switch(
    tt,
    'ALL' = "withAll",
    'ANY' = "withAny",
    'AT_LEAST' = "withAtLeast",
    'AT_MOST' = "withAtMost"
  )

  #get the criteriaList first
  criteriaList <- purrr::map(x$CriteriaList, ~criteriaToCapr(.x, caprCs = caprCs, csTb = csTb))
  # TODO demographic criteria list
  # demoCriteriaList <- purrr::map(x$DemographicCriteriaList, ~demoCriteriaToCapr(...))
  # TODO group list
  #groupList <- purrr::map(x$Groups, ~groupToCapr(.x, caprCs = caprCs, csTb = csTb))

  if (any(caprCall %in% c("withAtLeast", "withAtMost"))) {
    amt <- x$Count
    groupCall <- rlang::call2(
      caprCall,
      x = amt,
      !!!groupList
    )
  } else {
    groupCall <- rlang::call2(
      caprCall,
      !!!criteriaList
    )
  }

  res <- eval(groupCall)
  return(res)
}

# Primary Criteria -------------------
# function to turn a circe primary criteria into capr
primaryCriteriaToCapr <- function(cd) {

  # get concept sets for arrangement
  ccs <- cd$ConceptSets
  caprCs <- conceptSetToCapr(ccs)
  csTb <- getCsKey(ccs, caprCs)

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
  #TODO additional criteria with groups

  return(res)

}

# Inclusion Rules -------------------------------------
# function to covert inclusion rules to Capr
inclusionRulesToCapr <- function(cd) {
  # get concept sets for arrangement
  cs <- cd$ConceptSets
  caprCs <- conceptSetToCapr(cs)
  csTb <- getCsKey(cs, caprCs)

  # extract the rules
  ir <- cd$InclusionRules
  # turn each rule into a capr group
  rules <- purrr::map(ir, ~groupToCapr(.x$expression, caprCs = caprCs, csTb = csTb))

  # extract the rule names
  ruleNames <- purrr::map(ir, ~.x$name)

  #build the attrition call
  irCall <- rlang::call2(
    "attrition",
    !!!rules,
    expressionLimit = cd$ExpressionLimit$Type
  )
  #evaluate the attrition call
  res <- eval(irCall)
  names(res@rules) <- ruleNames

  return(res)
}


# Exit ---------------------------
# function to covert end strategy and censor to Capr
exitToCapr <- function(cd) {
  # get concept sets for arrangement
  cs <- cd$ConceptSets
  caprCs <- conceptSetToCapr(cs)
  csTb <- getCsKey(cs, caprCs)

  censor <- cd$CensoringCriteria
}

# Era --------------------
# function to convert collapse to Capr
eraToCapr <- function(cd) {

  res <- era(
    eraDays = cd$CollapseSettings$EraPad,
    studyStartDate = cd$CensorWindow$StartDate,
    studyEndDate = cd$CensorWindow$EndDate
  )

  return(res)
}

# UI -----------------------------
#' Read a circe json definition and convert it to a Capr cohort definition
#' @export
#' @param path character string that must be a valid path to read json from
readInCirce <- function(path) {
  checkmate::assertFileExists(path)

  # import json file into R
  circeObj <- jsonlite::fromJSON(path)
  checkmate::assertList(circeObj)

  # coerce circe primary criteria, qualified and additional criteria into Capr entry
  entry <- primaryCriteriaToCapr(circeObj)

  # coerce circe inclusion rules and expression limit into Capr attrition
  attrition <- attritionToCapr(circeObj)

  # coerce circe censoring criteria and end strategy into Capr exit
  exit <- exitToCapr(circeObj)

  # coerce circe collapse settings and censor window into Capr era
  era <- eraToCapr(circeObj)

  # combine sub components into a Capr cohort
  caprDef <- Capr::cohort(
    entry = entry,
    attrition = attrition,
    exit = exit,
    era = era
  )

  return(caprDef)
}

