# Classes-----------------------

## CohortEntry ----

#' @include window.R count.R query.R conceptSet.R
setClass("CohortEntry",
         slots = c(
           entryEvents = "list",
           observationWindow = "ObservationWindow",
           primaryCriteriaLimit = "character",
           additionalCriteria = "Group",
           qualifiedLimit = "character"
         ),
         prototype = list(
           entryEvents = list(),
           observationWindow = new("ObservationWindow"),
           primaryCriteriaLimit = "First",
           additionalCriteria = new("Group"),
           qualifiedLimit = "First"
         )
)

## CohortAttrition ----

setClass("CohortAttrition",
         slots = c(
           rules = "list",
           expressionLimit = "character"
         ),
         prototype = list(
           rules = list(),
           expressionLimit = "First"
         ))


## CohortExit ----
setClass("CohortExit",
         slots = c(
           endStrategy = "ANY",
           censoringCriteria = "CensoringCriteria"
         ),
         prototype = list(
           endStrategy = new("ObservationExit"),
           censoringCriteria = new("CensoringCriteria")
         )
)

## CohortEra----

setClass("CohortEra",
         slots = c(
           eraDays = "integer",
           studyStartDate = "Date",
           studyEndDate = "Date"
         ),
         prototype = list(
           eraDays = 0L,
           studyStartDate = lubridate::NA_Date_,
           studyEndDate = lubridate::NA_Date_
         ))


setClass("Cohort",
         slot = c(
           entry = "CohortEntry",
           attrition = "CohortAttrition",
           exit = "CohortExit",
           era = "CohortEra"
         ),
         prototype = list(
           entry = new("CohortEntry"),
           attrition = new("CohortAttrition"),
           exit = new("CohortExit"),
           era = new("CohortEra")
         )
)

# Constructors --------------------

#' Create a cohort entry criteria
#'
#' @param ... Capr Queries
#' @param observationWindow a time specifying the minimal time a person is observed
#' @param primaryCriteriaLimit Which primary criteria matches should be considered for inclusion? "First", "Last" or "All"
#' @param additionalCriteria a Capr group that adds restriction to the entry event
#' @param qualifiedLimit Which criteria matches should be considered for inclusion? "First", "Last" or "All"
#'
#' @return A cohort entry Capr object
#' @export
entry <- function(...,
                  observationWindow = continuousObservation(0L, 0L),
                  primaryCriteriaLimit = c("First", "All", "Last"),
                  additionalCriteria = NULL,
                  qualifiedLimit = c("First", "All", "Last")) {

  primaryCriteriaLimit <- checkmate::matchArg(primaryCriteriaLimit, c("First", "All", "Last"))
  qualifiedLimit <- checkmate::matchArg(qualifiedLimit, c("First", "All", "Last"))

  cohort_entry <- new("CohortEntry",
                      entryEvents = list(...),
                      observationWindow = observationWindow,
                      primaryCriteriaLimit = primaryCriteriaLimit,
                      qualifiedLimit = qualifiedLimit
  )

  if (!is.null(additionalCriteria)) {
    cohort_entry@additionalCriteria <- additionalCriteria
  }

  return(cohort_entry)
}

#' Create a cohort attrition object
#'
#' @param ... Capr groups
#' @param expressionLimit how to limit initial events per person either First, All, or Last
#' @export
attrition <- function(..., expressionLimit = c("First", "All", "Last")) {

  expressionLimit <- checkmate::matchArg(expressionLimit, c("First", "All", "Last"))

  new("CohortAttrition",
      rules = list(...),
      expressionLimit = expressionLimit)

}



#' Function that creates a cohort exit object
#' @param es the endStrategy object to specify for the exit
#' @param censor the censoring criteria to specify for the exit
#' @export
exit <- function(es = NULL, censor = NULL){
  if (is.null(es) & is.null(censor)) {
    ee <- new("CohortExit")
  } else if (is.null(censor)) {
    ee <- new("CohortExit",
              endStrategy = es)
  } else if (is.null(es)) {
    ee <- new("CohortExit",
              censoringCriteria = censor)
  } else {
    ee <- new("CohortExit",
              endStrategy = es,
              censoringCriteria = censor)
  }

  return(ee)


}
#' Create a Cohort Era class object
#'
#' The Cohort Era depicts the time span of the cohort. The Censor Window includes
#' the date window for which we register events. The Collapse Settings identify the era padding
#' between events before exiting a cohort.
#'
#' @param eraDays a numeric that specifies the number of days for the era padding
#' @param studyStartDate a date string that specifies the starting date of registration
#' @param studyEndDate a date string that specifies the end date of registration
#' @export
era <- function(eraDays = 0L,
                      studyStartDate = NULL,
                      studyEndDate = NULL) {
  if (is.null(studyStartDate)) {
    studyStartDate <- lubridate::NA_Date_
  }

  if (is.null(studyEndDate)) {
    studyEndDate <- lubridate::NA_Date_
  }
  new("CohortEra",
      eraDays = eraDays,
      studyStartDate = studyStartDate,
      studyEndDate = studyEndDate)
}

#' Function that creates a cohort object
#' @param entry the index event of the cohort
#' @param attrition rules that restrict the cohort further, developing attrition
#' @param exit the event where the person exits the cohort
#' @param era Cohort era (collapse) logic created with the `cohortEra` function
#' @export
cohort <- function(entry,
                   attrition = NULL,
                   exit = NULL,
                   era = NULL) {

  # Entry should be a list of queries or groups
  if (is(entry, "Query")){
    entry <- entry(entry)
  }

  cd <- new("Cohort", entry = entry)

  if (!is.null(attrition)) {
    cd@attrition <- attrition
  }

  if (!is.null(exit)) {
    cd@exit <- new("Exit")
  }

  if (!is.null(era)) {
    cd@era <- new("Era")
  }

  return(cd)
}

# Coercion --------------------

## Coerce Entry ----------
setMethod("as.list", "CohortEntry", function(x) {
  pc <- list(
    'CriteriaList' = purrr::map(x@entryEvents, ~as.list(.x)),
    'ObservationWindow' = as.list(x@observationWindow),
    'PrimaryCriteriaLimit' = list('Type' = x@primaryCriteriaLimit)
  )

  ac <- list(
    'AdditionalCriteria' = as.list(x@additionalCriteria),
    'QualifiedLimit' = list('Type' = x@qualifiedLimit)
  )

  ll <- list('PrimaryCriteria' = pc) %>%
    append(ac)

  if (is.na(ll$AdditionalCriteria$Type)) {
    ll$AdditionalCriteria <- NULL
  }

  return(ll)
})

## Coerce Attrition ----------
setMethod("as.list", "CohortAttrition", function(x) {
  ll <- list(
    'ExpressionLimit' = list('Type' = x@expressionLimit),
    'InclusionRules' = purrr::map(x@rules, ~as.list(.x))
  )
  return(ll)
})


## Coerce Exit ----------
setMethod("as.list", "CohortExit", function(x) {
  ll <- list(
    'EndStrategy' = list('Type' = x@expressionLimit),
    'CensoringCrieria' = purrr::map(x@rules, ~as.list(.x))
  )
  return(ll)
})

## Coerce Era ----------
setMethod("as.list", "CohortEra", function(x) {
  ll <- list(
    'CollapseSettings' = list(
      'CollapseType' = "ERA",
      'EraPad' = x@eraDays
    ),
    'CensorWindow' = list(
      'StartDate' = x@studyStartDate,
      'EndDate' = x@studyEndDate
    )
  )

  ll$CensorWindow <- purrr::discard(ll$CensorWindow, is.na)

  return(ll)
})

