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

# Guard for constructors that collect objects via `...`: without it, a misspelled
# named argument (e.g. `primaryCriterialimit = "All"`) is silently swallowed into
# the dots and the real parameter reverts to its default.
checkCaprDots <- function(dots, allowedClasses, fnName, expected, namedArgs) {
  attributeClasses <- c("logicAttribute", "opAttributeSuper", "conceptAttribute",
                        "nestedAttribute", "dateAdjustmentAttribute", "keyValueAttribute",
                        "conceptSetAttribute")
  for (i in seq_along(dots)) {
    ok <- any(vapply(allowedClasses, function(cl) methods::is(dots[[i]], cl), logical(1)))
    if (!ok) {
      nm <- names(dots)[i]
      isAttr <- any(vapply(attributeClasses, function(cl) methods::is(dots[[i]], cl), logical(1)))
      cls <- class(dots[[i]])[1]
      msg <- c(
        "Arguments passed to {.fn {fnName}} must be {expected}; argument {i} is {.cls {cls}}."
      )
      if (isAttr) {
        msg <- c(msg, "i" = "{.cls {cls}} is a query attribute \\u2014 pass it inside the query call, e.g. {.code conditionOccurrence(cs, {cls}(...))}.")
      } else if (!is.null(nm) && nzchar(nm) && !(nm %in% c("", NA))) {
        msg <- c(msg, "i" = "The argument is named {.val {nm}} \\u2014 did you misspell one of {.fn {fnName}}'s parameters ({.code {paste(namedArgs, collapse = ', ')}})?")
      }
      cli::cli_abort(msg)
    }
  }
  invisible(dots)
}

#' Create a cohort entry definition
#'
#' Defines the index event(s) and required observation window for a cohort. Each query passed
#' as \code{...} is a candidate index event; a person enters the cohort at the first (or all,
#' depending on \code{primaryCriteriaLimit}) qualifying event.
#'
#' @param ... One or more \code{Query} objects (e.g. \code{conditionOccurrence()}, \code{drugExposure()}).
#' @param observationWindow Minimum continuous observation before and after index.
#'   See \code{\link{continuousObservation}}. Default \code{continuousObservation(0L, 0L)}.
#' @param primaryCriteriaLimit Whether to index on \code{"First"}, \code{"Last"}, or \code{"All"}
#'   qualifying events per person.
#' @param additionalCriteria An optional \code{Group} (\code{withAll}/\code{withAny}) further
#'   restricting which events qualify as the index. See \code{\link{withAll}}.
#' @param qualifiedLimit Result limit after applying \code{additionalCriteria}. Required when
#'   \code{additionalCriteria} is non-\code{NULL}; defaults to \code{primaryCriteriaLimit}.
#' @return A \code{CohortEntry} object.
#' @seealso \code{\link{cohort}}, \code{\link{attrition}}, \code{\link{exit}},
#'   \code{\link{continuousObservation}}, \code{\link{withAll}}
#' @examples
#' t2dm <- cs(descendants(201826L), name = "T2DM")
#' entry(conditionOccurrence(t2dm), primaryCriteriaLimit = "First")
#' @export
entry <- function(...,
                  observationWindow = continuousObservation(0L, 0L),
                  primaryCriteriaLimit = c("First", "All", "Last"),
                  additionalCriteria = NULL,
                  qualifiedLimit = NULL) {

  checkCaprDots(list(...), "Query", "entry", "Capr Query objects",
                c("observationWindow", "primaryCriteriaLimit", "additionalCriteria", "qualifiedLimit"))
  primaryCriteriaLimit <- checkmate::matchArg(primaryCriteriaLimit, c("First", "All", "Last"))
  if (!is.null(additionalCriteria) && is.null(qualifiedLimit)) {
    stop("qualifiedLimit must be provided when additionalCriteria is used.", call. = FALSE)
  }
  if (is.null(qualifiedLimit)) {
    qualifiedLimit <- primaryCriteriaLimit
  } else {
    qualifiedLimit <- checkmate::matchArg(qualifiedLimit, c("First", "All", "Last"))
  }

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

#' Define cohort inclusion/exclusion rules (attrition)
#'
#' Each named argument is one attrition rule — a \code{Group} built with
#' \code{\link{withAll}}, \code{\link{withAny}}, \code{\link{withAtLeast}}, or
#' \code{\link{withAtMost}}. Rules are applied in order after the index event is selected.
#'
#' @param ... Named \code{Group} objects, each representing one inclusion or exclusion rule.
#' @param expressionLimit Which qualifying events per person survive attrition —
#'   \code{"First"} (default), \code{"All"}, or \code{"Last"}. Should align with
#'   \code{primaryCriteriaLimit} in \code{\link{entry}}.
#' @return A \code{CohortAttrition} object.
#' @seealso \code{\link{cohort}}, \code{\link{entry}}, \code{\link{withAll}}, \code{\link{withAny}}
#' @examples
#' t2dm <- cs(descendants(201826L), name = "T2DM")
#' attrition(
#'   noT2DM = withAll(exactly(0, conditionOccurrence(t2dm),
#'                            duringInterval(eventStarts(-Inf, 0)))),
#'   expressionLimit = "First"
#' )
#' @export
attrition <- function(..., expressionLimit = c("First", "All", "Last")) {

  checkCaprDots(list(...), c("Group", "Criteria"), "attrition",
                "Capr Group objects (from withAll/withAny/withAtLeast/withAtMost) or Criteria",
                "expressionLimit")
  expressionLimit <- checkmate::matchArg(expressionLimit, c("First", "All", "Last"))

  methods::new("CohortAttrition",
      rules = list(...),
      expressionLimit = expressionLimit)

}

#' Define cohort exit strategy
#'
#' @param endStrategy An end strategy object: \code{\link{observationExit}},
#'   \code{\link{fixedExit}}, or \code{\link{drugExit}}.
#' @param censor Optional \code{\link{censoringEvents}} object of queries that end the cohort
#'   early if they occur after the index date.
#' @return A \code{CohortExit} object.
#' @seealso \code{\link{observationExit}}, \code{\link{fixedExit}}, \code{\link{drugExit}},
#'   \code{\link{censoringEvents}}, \code{\link{cohort}}
#' @examples
#' exit(endStrategy = observationExit())
#' exit(endStrategy = fixedExit(offsetDays = 30L))
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
#' Define cohort era collapse settings
#'
#' Controls how adjacent cohort episodes are collapsed (merged) and optionally constrains
#' the study window. Episodes separated by fewer than \code{eraDays} days are merged.
#'
#' @param eraDays Gap in days between consecutive episodes below which they are collapsed
#'   into a single era. Default \code{0L} (no collapse).
#' @param studyStartDate Optional date to truncate cohort membership before this date.
#' @param studyEndDate Optional date to truncate cohort membership after this date.
#' @return A \code{CohortEra} object.
#' @seealso \code{\link{cohort}}
#' @examples
#' era(eraDays = 30L)
#' era(eraDays = 99999L)  # Occ cohort: collapse all episodes into one long era
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


#' Build a cohort definition
#'
#' Assembles the four components of an OHDSI cohort definition into a single
#' \code{Cohort} object that can be serialized to Circe-compatible JSON via
#' \code{\link{toCohortJson}} or written to disk with \code{\link{writeCohort}}.
#'
#' @param entry A \code{CohortEntry} from \code{\link{entry}()}.
#' @param attrition Optional \code{CohortAttrition} from \code{\link{attrition}()}.
#' @param exit Optional \code{CohortExit} from \code{\link{exit}()}. Defaults to observation exit.
#' @param era Optional \code{CohortEra} from \code{\link{era}()}. Defaults to no era collapse.
#' @return A \code{Cohort} S4 object.
#' @seealso \code{\link{entry}}, \code{\link{attrition}}, \code{\link{exit}}, \code{\link{era}},
#'   \code{\link{toCohortJson}}, \code{\link{writeCohort}}
#' @examples
#' giBleed <- cs(descendants(192671L), name = "GI Bleed")
#' ch <- cohort(
#'   entry = entry(conditionOccurrence(giBleed), primaryCriteriaLimit = "First"),
#'   exit  = exit(endStrategy = observationExit())
#' )
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
  # Emit CriteriaList as domain-keyed query only (matches Atlas/CIRCE format);
  # each item is { "Measurement": {...} } not { "Criteria": {...}, "Occurrence": ..., ... }
  criteriaList <- purrr::map(x@entryEvents, function(ev) {
    if (methods::is(ev, "Criteria")) {
      cli::cli_abort(c(
        "{.fn entry} only accepts {.cls Query} objects, not {.cls Criteria}.",
        "i" = "Pass the Query directly, e.g. {.code conditionOccurrence(...)} without wrapping it in {.fn atLeast}/{.fn atMost}/{.fn exactly}."
      ))
    }
    as.list(ev)
  })
  pc <- list(
    'CriteriaList' = criteriaList,
    'ObservationWindow' = as.list(x@observationWindow),
    'PrimaryCriteriaLimit' = list('Type' = x@primaryCriteriaLimit)
  )

  ac <- list(
    'AdditionalCriteria' = as.list(x@additionalCriteria),
    'QualifiedLimit' = list('Type' = x@qualifiedLimit)
  )

  ll <- list('PrimaryCriteria' = pc) |>
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

  ll <- as.list(x@entry) |>
    append(as.list(x@attrition)) |>
    append(as.list(x@exit)) |>
    append(as.list(x@era)) |>
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

  # ValueAsConcept and VisitType are serialized inline (array of concepts); do not list those
  # concept sets in ConceptSets or Circe would create an extra codeset and concept set count would differ.
  allSets <- listConceptSets(cd2)
  usage <- collectConceptSetAttributeUsage(cd2)
  idsByUsage <- split(
    vapply(usage, function(u) u$id, integer(1L)),
    vapply(usage, function(u) u$name, character(1L))
  )
  inlineOnlyNames <- c("ValueAsConcept", "VisitType")
  excludeIds <- integer(0L)
  for (nm in inlineOnlyNames) {
    ids <- unique(idsByUsage[[nm]] %||% integer(0L))
    otherIds <- unique(unlist(idsByUsage[names(idsByUsage) != nm], use.names = FALSE))
    excludeIds <- c(excludeIds, setdiff(ids, otherIds))
  }
  excludeIds <- unique(excludeIds)
  if (length(excludeIds) > 0L) {
    allSets <- purrr::keep(allSets, function(cs) !(cs$id %in% excludeIds))
  }

  cdCirce <- list(
    #start with getting concept set structure
    'ConceptSets' = allSets
  ) |>
    #append cohort structure
    append(as.list(cd2))

  return(cdCirce)
}

#' Compile a Capr object to json
#'
#' @param object A Capr object such as a cohort, list of cohorts, or concept set.
#' @param ... Arguments passed on to jsonlite::toJSON (e.g. \code{pretty = TRUE}).
#'   For the Cohort method, \code{includeConceptSets} is also allowed; see
#'   \code{\link{compile,Cohort-method}}.
#'
#' @return The json representation of the Capr object
#' @export
setGeneric("compile", function(object, ...) { standardGeneric("compile") })

# Remap cohort codeset ids (0,1,2,... from replaceCodesetId) back to original ids from includeConceptSets.
# Used when includeConceptSets is provided so round-trip JSON preserves original concept set numbering.
remapCirceCodesetIdsToOriginal <- function(circe, guidTable, codesetKeys = c(
  "CodesetId", "CodesetID", "DrugCodesetId",
  "ObservationSourceConcept", "VisitSourceConcept", "ConditionSourceConcept",
  "DrugSourceConcept", "ProcedureSourceConcept", "MeasurementSourceConcept", "VisitDetailSourceConcept"
)) {
  if (is.null(guidTable) || nrow(guidTable) == 0L) return(circe)
  origIds <- suppressWarnings(as.integer(guidTable$guid))
  map <- stats::setNames(origIds, as.character(guidTable$codesetId))
  recurse <- function(x) {
    if (is.null(x)) return(x)
    if (!is.list(x)) return(x)
    if (length(x) == 0L) return(x)
    if (!is.null(names(x))) {
      for (k in names(x)) {
        if (k %in% codesetKeys && length(x[[k]]) == 1L && is.numeric(x[[k]])) {
          m <- map[as.character(as.integer(x[[k]]))]
          if (length(m) > 0L && !is.na(m[1L])) x[[k]] <- as.integer(m[1L])
        } else if (k != "ConceptSets") {
          x[[k]] <- recurse(x[[k]])
        }
      }
    } else {
      x <- lapply(x, recurse)
    }
    x
  }
  for (nm in setdiff(names(circe), "ConceptSets")) circe[[nm]] <- recurse(circe[[nm]])
  circe
}

serializeCohortToJson <- function(object, ..., includeConceptSets = NULL) {
  guidTable <- if (length(includeConceptSets) > 0L) collectGuid(object) else NULL
  circe <- toCirce(object)
  if (length(includeConceptSets) > 0L) {
    # Use includeConceptSets as the full ConceptSets list (order and ids) so round-trip matches (e.g. pah_event_cohort with duplicate sets).
    validCs <- Filter(function(cs) methods::is(cs, "ConceptSet"), includeConceptSets)
    if (length(validCs) > 0L) {
      circe$ConceptSets <- unname(lapply(validCs, function(cs) {
        csList <- as.list(cs)
        if (is.null(csList$id) || !is.numeric(csList$id)) csList$id <- seq_along(validCs)[match(cs, validCs, 0L)] - 1L
        csList$id <- as.integer(csList$id)
        csList
      }))
    }
    # Restore original concept set ids in cohort structure (replaceCodesetId had assigned 0,1,2,...; map back to original ids).
    circe <- remapCirceCodesetIdsToOriginal(circe, guidTable)
  }
  as.character(jsonlite::toJSON(circe, auto_unbox = TRUE, ...))
}

compile.Cohort <- function(object, ..., includeConceptSets = NULL) {
  .Deprecated(new = "toCohortJson", package = "Capr", old = "compile")
  serializeCohortToJson(object, ..., includeConceptSets = includeConceptSets)
}


#' @rdname as.json
#' @aliases as.json,Cohort-method
setMethod("as.json", "Cohort", function(x, pretty = TRUE, ...) {
  toCohortJson(x, pretty = pretty, ...)
})

#' Convert a Capr cohort to JSON
#'
#' @param object A Capr cohort.
#' @param ... Arguments passed on to jsonlite::toJSON (e.g. \code{pretty = TRUE}).
#' @param includeConceptSets Optional list of \code{ConceptSet} objects to include
#'   in the JSON even if not referenced in the cohort (e.g. for round-trip equivalence).
#'
#' @return A JSON character string for the cohort.
#' @export
toCohortJson <- function(object, ..., includeConceptSets = NULL) {

  checkmate::assertClass(object, "Cohort")
  serializeCohortToJson(object, ..., includeConceptSets = includeConceptSets)
}

#' Compile a Capr cohort to json
#'
#' @param object A Capr cohort or list of Capr cohorts
#' @param ... Arguments passed on to jsonlite::toJSON (e.g. \code{pretty = TRUE}).
#' @param includeConceptSets Optional list of \code{ConceptSet} objects to include
#'   in the JSON even if not referenced in the cohort (e.g. for round-trip equivalence).
#'
#' @return The json representation of Capr cohorts
#' @export
#' @rdname compile-methods
#' @examples
#' ch <- cohort(conditionOccurrence(cs(1,2, name = "concepts")))
#' compile(ch)
setMethod("compile", "Cohort", compile.Cohort)

serializeConceptSetToJson <- function(object, ...) {
  x <- list(items = lapply(object@Expression, as.list))
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, ...))
}

compile.ConceptSet <- function(object, ...) {
  .Deprecated(new = "toConceptSetJson", package = "Capr", old = "compile")
  serializeConceptSetToJson(object, ...)
}

#' Convert a Capr concept set to JSON
#'
#' @param object A Capr concept set created with \code{cs()}.
#' @param ... Arguments passed on to jsonlite::toJSON.
#'
#' @return A JSON character string for the concept set expression.
#' @export
toConceptSetJson <- function(object, ...) {

  checkmate::assertClass(object, "ConceptSet")
  serializeConceptSetToJson(object, ...)
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
#' \dontrun{
#' cs1 <- cs(descendants(exclude(436665),440383,442306,4175329), name = "concepts")
#' # optional step to fill in concept set details. Requires database connection.
#' con <- {A CDM datbase connection}
#' cs1 <- getConceptSetDetails(cs1, con)
#'
#' x <- cohort(conditionOccurrence(cs1))
#' writeCohort(x, "cohortDefinition.json")
#' }
writeCohort <- function(x, path) {

  checkmate::assertClass(x, "Cohort")
  checkmate::assertCharacter(path, len = 1, min.chars = 1, pattern = "\\.json$")

  toCirce(x) |>
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


# capr_to_circe <- function(cd) {
#
#   circeJson <- toCirce(cd) |>
#     jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
#     as.character()
#
#   return(circeJson)
#
# }

#' Make a cohort dataframe for cohort generator
#' @param ... multiple capr cohorts to bind into a dataframe
#' @return a tibble containing cohortId, name, sql and json to pipe into CohortGenerator.
#' @export
makeCohortSet <- function(...) {
  if (!rlang::is_installed("CirceR")) {
    stop("CirceR is required but not installed. Install it with `devtools::install_github('OHDSI/CirceR')`")
  }

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
  cohortJson <- purrr::map_chr(cohortList, ~compile(.x))

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

