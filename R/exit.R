
# Exit Classes --------------
## ObservationExit --------------

#' An S4 class for a cohort exit based on end of continuous observation.
#' @description The event persists until the end of continuous observation of the persons
#' @slot index specification of event date to offset
#' @slot offsetDays an integer specifying the number of days to offset from the event date
setClass("ObservationExit",
         slots = c(
           exit = "character"
         ),
         prototype = list(
           exit = "End of Continuous Observation"
         )
)

## FixedDurationExit --------------

#' An S4 class for a cohort exit based on fixed duration persistence.
#' @description The event end date is derived from adding a number of days to
#' the event's start or end date. If an offset is added to the event's start date,
#' all cohort episodes will have the same fixed duration (subject to further censoring).
#' If an offset is added to the event's end date, persons in the cohort may have
#' varying cohort duration times due to the varying event durations
#' (such as eras of persistent drug exposure or visit length of stay).
#' This event persistence assures that the cohort end date will be no greater
#' than the selected index event date, plus the days offset.
#' @slot index specification of event date to offset
#' @slot offsetDays an integer specifying the number of days to offset from the event date
setClass("FixedDurationExit",
         slots = c(
           index = "character",
           offsetDays = "integer"
         ),
         prototype = list(
           index = NA_character_,
           offsetDays = NA_integer_
         )
)

## DrugExposureExit --------------

#' An S4 class for a cohort exit based on continuous exposure persistence.
#' @description Specify a concept set that contains one or more drugs.
#' A drug era will be derived from all drug exposure events for any of the drugs within the specified concept set,
#' using the specified persistence window as a maximum allowable gap in days between successive exposure
#' events and adding a specified surveillance window to the final exposure event.
#' If no exposure event end date is provided, then an exposure event end date is inferred to
#' be event start date + days supply in cases when days supply is available or
#' event start date + 1 day otherwise. This event persistence assures that the cohort end date
#' will be no greater than the drug era end date.
#' @slot conceptSet the concept set of the drug exposure used to identify the exit
#' @slot persistenceWindow allow for a maximum of days between exposure records when inferring the era
#' of persistence exposure
#' @slot surveillanceWindow add days to the end of the era of persistence exposure as an additional period of
#' surveillance prior to cohort exit
#' @slot daysSupplyOverride force drug exposure days supply to a set number of days
#' @slot count an integer specifying the number of occurrences for a criteria
#' @include conceptSet.R
setClass("DrugExposureExit",
         slots = c(
           conceptSet = "ConceptSet",
           persistenceWindow = "integer",
           surveillanceWindow = "integer",
           daysSupplyOverride = "integer"
         ),
         prototype = list(
           conceptSet = new("ConceptSet"),
           persistenceWindow = NA_integer_,
           surveillanceWindow = NA_integer_,
           daysSupplyOverride = NA_integer_
         )
)


## Censoring Event ----
#' An S4 class identifying a censoring criteria for the cohort
#' @description The censoring criteria specifies events where the person exits the cohort.
#' These events are based on a query class object and users can specify multiple queries
#' in the censoring criteria.
#' @slot criteria a list of Capr query class objects that specify the events that
#' would lead a person to exit the cohort.
setClass("CensoringCriteria",
         slots = c(
           criteria = "list"
         ),
         prototype = list(
           criteria = list()
         )
)

# Constructors -----

#' Function to create an exit based on continuous observation
#' @return an S4 ObservationExit class which defines the cohort exit as the end of continuous observation
#' @export
observationExit <- function() {
  methods::new("ObservationExit")
}

#' Function to create an exit based on exit based on the end of a continuous drug exposure
#' @param conceptSet the concept set of the drug exposure used to identify the exit
#' @param persistenceWindow allow for a maximum of days between exposure records when inferring the era
#' of persistence exposure
#' @param surveillanceWindow add days to the end of the era of persistence exposure as an additional period of
#' surveillance prior to cohort exit
#' @param daysSupplyOverride force drug exposure days supply to a set number of days
#' @return an S4 DrugExposueExit class which defines the cohort exit by end of drug exposure
#' @export
drugExit <- function(conceptSet,
                     persistenceWindow = 0L,
                     surveillanceWindow = 0L,
                     daysSupplyOverride = NULL) {

  checkmate::expect_integerish(persistenceWindow, len = 1)
  checkmate::expect_integerish(surveillanceWindow, len = 1)
  checkmate::expect_integerish(daysSupplyOverride, len = 1, null.ok = TRUE)

  persistenceWindow <- as.integer(persistenceWindow)
  surveillanceWindow <- as.integer(surveillanceWindow)

  if (is.null(daysSupplyOverride)) {
    daysSupplyOverride <- NA_integer_
  } else {
    daysSupplyOverride <- as.integer(daysSupplyOverride)
  }


  if(is.null(daysSupplyOverride)) {
    daysSupplyOverride <- NA_integer_
  }

  ee <- methods::new("DrugExposureExit",
      conceptSet = conceptSet,
      persistenceWindow = persistenceWindow,
      surveillanceWindow = surveillanceWindow,
      daysSupplyOverride = daysSupplyOverride)
  return(ee)
}

#' Function to create an exit based on exit based on the end of a continuous drug exposure
#' @param index specification of event date to offset. Can be either startDate or endDate
#' @param offsetDays an number specifying the days to offset from the event date. Will coerce to an integer
#' @return a fixed Duration exit S4 object used to define the cohort exit as the end of a specified time
#' @export
fixedExit <- function(index = c("startDate", "endDate"), offsetDays){

  index <- checkmate::matchArg(index, c("startDate", "endDate"))

  ee <- methods::new("FixedDurationExit",
      index = index,
      offsetDays = as.integer(offsetDays))
  return(ee)
}

# Deprecate endStrategy general function
# #' Constructor function for end strategies
# #' @description an end strategy defines how persons exit the cohort. There are
# #' two options: 1) exit based on a fixed duration relative to the initial event,
# #' and 2) exit based on the end of a continuous drug exposure
# #' @param type specify the type of end strategy
# #' @param ... dots collecting arguments for building the end strategy.
# #' If the type is DrugExposureExit the dots argument requires a minimum of a conceptSet and
# #' may also specify the surveillanceWindow, persistenceWindow and daysSupplyOverride.
# #' If the the type is FixedDurationExit the dots argument requires specification of
# #' the index and offsetDays.
# #' @export
# endStrategy <- function(type = c("DrugExposureExit",
#                                  "FixedDurationExit"),
#                         ...) {
#
#   args <- rlang::list2(...)
#   if (type == "DrugExposureExit") {
#     es <- rlang::inject(drugExit(!!!args))
#   } else{
#     es <- rlang::inject(fixedExit(!!!args))
#   }
#   return(es)
# }

#' Constructor for a set of censoring events
#' @param ... a list of Capr query objects that are used as censoring events
#' @return a censoring criteria S4 object used to define the censoring events of the cohort definition
#' @export
censoringEvents <- function(...) {
  dots <- list(...)
  ee <- methods::new("CensoringCriteria",
      criteria = dots)
  return(ee)
}


# Coercion -----------

toPascal <- function(x) {
  SqlRender::camelCaseToTitleCase(x) |>
    stringr::str_replace_all("\\s", "")
}

## Coerce Observation Exit -------
setMethod("as.list", "ObservationExit", function(x) {
  ll <- list()
  return(ll)
})

## Coerce Fixed Duration Exit -------
setMethod("as.list", "FixedDurationExit", function(x) {
  ll <- list(
    'DateOffset' = list(
      'DateField' = toPascal(x@index),
      'Offset' = x@offsetDays
    )
  )
  return(ll)
})


## Coerce Drug Exposure Exit -------
setMethod("as.list", "DrugExposureExit", function(x) {
  ll <- list(
    'DrugCodesetId' = x@conceptSet@id,
    'GapDays' = x@persistenceWindow,
    'Offset' = x@surveillanceWindow,
    'DaysSupplyOverride' = x@daysSupplyOverride
  ) |>
    purrr::discard(is.na)
  ll <- list('CustomEra' = ll)
  return(ll)
})

## Coerce Censoring Criteria -------
setMethod("as.list", "CensoringCriteria", function(x) {
  ll <-  purrr::map(x@criteria, ~as.list(.x))
  return(ll)
})
