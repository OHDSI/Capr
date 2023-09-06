# Classes-----------------------

## CohortEntry ----

#' @include window.R query.R conceptSet.R criteria.R
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
#' @include exit.R
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

## Cohort----
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

  cohort_entry <- methods::new("CohortEntry",
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
#' @return A cohort attrition object that can be used in a cohort definition
#' @export
attrition <- function(..., expressionLimit = c("First", "All", "Last")) {

  expressionLimit <- checkmate::matchArg(expressionLimit, c("First", "All", "Last"))

  methods::new("CohortAttrition",
      rules = list(...),
      expressionLimit = expressionLimit)

}

#' Function that creates a cohort exit object
#' @param endStrategy the endStrategy object to specify for the exit
#' @param censor the censoring criteria to specify for the exit
#' @return A cohort exit object that can be used in a cohort definition
#' @export
exit <- function(endStrategy, censor = NULL){
  if (is.null(censor)) {
    ee <- methods::new("CohortExit",
              endStrategy = endStrategy)
  } else {
    ee <- methods::new("CohortExit",
              endStrategy = endStrategy,
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
#' @return a S4 CohortEra class object defining the eras of the cohort definition
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
  methods::new("CohortEra",
      eraDays = eraDays,
      studyStartDate = studyStartDate,
      studyEndDate = studyEndDate)
}


#' Function that creates a cohort object
#' @param entry the index event of the cohort
#' @param attrition rules that restrict the cohort further, developing attrition
#' @param exit the event where the person exits the cohort
#' @param era Cohort era (collapse) logic created with the `cohortEra` function
#' @return an S4 Cohort class object describing the cohort definiton
#' @export
cohort <- function(entry,
                   attrition = NULL,
                   exit = NULL,
                   era = NULL) {

  # Entry should be a list of queries or groups
  if (methods::is(entry, "Query")){
    entry <- entry(entry)
  }

  cd <- methods::new("Cohort", entry = entry)

  if (!is.null(attrition)) {
    cd@attrition <- attrition
  }

  if (is.null(exit)) {
    cd@exit <- methods::new("CohortExit")
  } else{
    cd@exit <- exit
  }

  if (is.null(era)) {
    cd@era <- methods::new("CohortEra")
  } else{
    cd@era <- era
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

  nm <- names(x@rules)
  if (is.null(nm)) {
    nm <- paste0("rule", seq_along(x@rules))
  }

  irs <- purrr::map2(
    nm,
    unname(x@rules),
    ~list('name' = .x,
          'expression' = as.list(.y))
  )

  ll <- list(
    'ExpressionLimit' = list('Type' = x@expressionLimit),
    'InclusionRules' = irs
  )
  return(ll)
})


## Coerce Exit ----------
setMethod("as.list", "CohortExit", function(x) {
  ll <- list(
    'EndStrategy' = as.list(x@endStrategy),
    'CensoringCriteria' = as.list(x@censoringCriteria)
  )
  if (length(ll$EndStrategy) == 0) {
    ll$EndStrategy <- NULL
  }
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

## Coerce Cohort ----------
setMethod("as.list", "Cohort", function(x) {

  ll <- as.list(x@entry) %>%
    append(as.list(x@attrition)) %>%
    append(as.list(x@exit)) %>%
    append(as.list(x@era)) %>%
    append(list("cdmVersionRange" = ">=5.0.0"))

  return(ll)
})
#' Function to coerce cohort to circe
#' @param cd the Capr cohort class
#' @return an s3 list representing the circe object to coerce to json
#' @export
toCirce <- function(cd) {

  #get all guids from cohort definition and remove duplicates
  guidTable <- collectGuid(cd)

  #replace guids with codeset integer
  cd2 <- replaceCodesetId(cd, guidTable = guidTable)

  cdCirce <- list(
    #start with getting concept set structure
    'ConceptSets' = listConceptSets(cd2)
  ) %>%
    #append cohort structure
    append(as.list(cd2))

  return(cdCirce)
}

#' Compile a Capr object to json
#'
#' @param object A Capr object such as a cohort, list of cohorts, or concept set.
#' @param ... Arguments passed on to jsonlite::toJSON.
#' e.g. `pretty = TRUE` for nicely formatted json.
#'
#' @return The json representation of the Capr object
#' @export
setGeneric("compile", function(object, ...) { standardGeneric("compile") })

compile.Cohort <- function(object, ...) {
  as.character(jsonlite::toJSON(toCirce(object), auto_unbox = TRUE, ...))
}

#' Compile a Capr cohort to json
#'
#' @param object A Capr cohort or list of Capr cohorts
#' @param ... Arguments passed on to jsonlite::toJSON.
#' e.g. `pretty = TRUE` for nicely formatted json.
#'
#' @return The json representation of Capr cohorts
#' @importFrom generics compile
#' @exportS3Method compile Cohort
#' @export
#' @rdname compile-methods
#' @examples
#' ch <- cohort(condition(cs(1,2)))
#' compile(ch)
setMethod("compile", "Cohort", compile.Cohort)

compile.ConceptSet <- function(object, ...) {
  x <- list(items = lapply(object@Expression, as.list))
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, ...))
}

#' Compile a Capr Concept Set to json
#'
#' @export
#' @rdname compile-methods
#' @param object A Capr Concept Set created with `cs`
#' @param ... Arguments passed on to jsonlite::toJSON.
#' e.g. `pretty = TRUE` for nicely formatted json.
#'
#' @return The json representation of Capr cohorts
setMethod("compile", "ConceptSet", compile.ConceptSet)

setMethod("show", "Cohort", function(object) {
  # TODO make this pretty on the console
  utils::str(object, max.level = 2)
})

#' Write Cohort json file
#'
#' @param x A Capr cohort
#' @param path The name of the file to create
#' @export
#' @return Invisibly returns the path to the json file that was written
#' @examples
#' cs1 <- cs(descendants(exclude(436665),440383,442306,4175329))
#' cs1 <- getConceptSetDetails(cs1)
#' x <- cohort(condition(cs1))
#' writeCohort(x, "cohortDefinition.json")
writeCohort <- function(x, path) {

  checkmate::assertClass(x, "Cohort")
  checkmate::assertCharacter(path, len = 1, min.chars = 1, pattern = "\\.json$")

  toCirce(x) %>%
    jsonlite::write_json(
      path = path,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  invisible(path)
}

# Templates ------------
# Generate a Capr cohort using a template
# @param file the input file of a concept set
# @param .capr a function that creates a capr cohort
# @return A Capr cohort definition
# @export
# generateCaprTemplate <- function(file, .capr) {
#
#   # get file name
#   name <- tools::file_path_sans_ext(basename(file))
#   #retreive concept set
#   conceptSet <- Capr::readConceptSet(path = file, name = name)
#
#   #generate cohort from template
#   .capr(conceptSet)
# }

# writeCohort <- function(x, path, ...) {
#   checkmate::assertClass(x, "Cohort")
#   checkmate::assertCharacter(path, len = 1, min.chars = 1, pattern = "\\.json$")
#   # check that concept set details are filled in
#   check <- unlist(x$ConceptSets, recursive = TRUE)
#   if (any(check[grepl( "CONCEPT_NAME|STANDARD_CONCEPT", names(check))] == "")) {
#     rlang::abort("Concept set details are missing. Fill in concept set details using `getConceptSetDetails()`")
#   }
#   jsonlite::write_json(x = as.list(x), path = path, auto_unbox = TRUE, pretty = TRUE, ...)
# }


capr_to_circe <- function(cd) {

  circeJson <- toCirce(cd) %>%
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
    as.character()

  return(circeJson)

}

#' Make a cohort dataframe for cohort generator
#' @param ... multiple capr cohorts to bind into a dataframe
#' @return a tibble containing cohortId, name, sql and json to pipe into CohortGenerator.
#' @export
makeCohortSet <- function(...) {

  cohortList <- rlang::dots_list(..., .named = TRUE)

  check <- purrr::map_chr(cohortList, ~methods::is(.x))

  if(!all(check == "Cohort")) {
    stop("all cohorts need to be a Capr Cohort class")
  }

  # get cohort Id
  cohortId <- seq_along(cohortList)

  # get cohort names
  cohortName <- names(cohortList)

  # get cohort json
  cohortJson <- purrr::map_chr(cohortList, ~capr_to_circe(.x))

  # get ohdsi sql
  ohdsiSql <- purrr::map_chr(
    cohortJson,
    ~CirceR::buildCohortQuery(
      expression = .x,
      options = CirceR::createGenerateOptions(generateStats = TRUE)
    )
  )

  # make tibble for cohort generator
  df <- tibble::tibble(
    cohortId = cohortId,
    cohortName = cohortName,
    sql = ohdsiSql,
    json = cohortJson
  )
  return(df)
}

