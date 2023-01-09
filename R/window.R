# Classes --------------

## ObservationWindow -------------------
#' An S4 class for an ObservationWindow
#' @description this determines the minimal observation time before and after index for all persons
#' in the cohort
#' @slot priorDays minimum number of days prior to the cohort index
#' @slot postDays minimum number of days post cohort index
setClass("ObservationWindow",
         slots = c(priorDays = "integer",
                   postDays = "integer"),
         prototype = list(
           priorDays = 0L,
           postDays = 0L
         )
)

## Endpoint ----------------
#' An S4 class for an Endpoint
#' @description this determines the time in days relative to an index either before or after
#' @slot days either a character string all or an integer for the number of days
#' @slot coeff a character string either before or after
setClass("Endpoint",
         slots = c(
           days = "numeric",
           coeff = "numeric"
         ),
         prototype = list(
           days = NA_real_,
           coeff = NA_real_
         ))


## EventWindow -----------------------
#' An S4 class for a Window
#'
#' A window class provides details on the end points of the timeline
#'
#' @slot event a character string either start or end. Identifies the point of reference for the window
#' @slot start an endpoint object containing the days and coefficient for the start of the window
#' @slot end an endpoint object containing the days and coefficient for the end of the window
#' @slot index A character string either start or end. Identifies where the index is relative to the window
setClass("EventWindow",
         slots = c(event = "character",
                   start = "Endpoint",
                   end = "Endpoint",
                   index = "character"),
         prototype = list(
           event = NA_character_,
           start = new("Endpoint"),
           end = new("Endpoint"),
           index = NA_character_
         ))

## EventAperture -------------------

#' An S4 class for Aperture
#'
#'The aperture class provides context to when the criteria must be observed in a person timeline to pretain to the expression
#'
#' @slot startWindow a EventWindow class object identifying the start window
#' @slot endWindow a EventWindow class object ifentifying the end window (optional)
#' @slot restrictVisit a logic toggle where TRUE restricts to the same visit
#' @slot ignoreObservationPeriod a logic toggle where TRUE allows events outside the observation period
setClass("EventAperture",
         slots = c(startWindow = "EventWindow",
                   endWindow = "EventWindow",
                   restrictVisit = "logical",
                   ignoreObservationPeriod = "logical"),
         prototype = list(
           startWindow = new("EventWindow"),
           endWindow = new("EventWindow"),
           restrictVisit = FALSE,
           ignoreObservationPeriod = FALSE
         ))

# Constructors ----------------------

## ObservationWindow ---------------------
#' A function to construct the observationWindow
#' @param priorDays minimum number of observation days prior to the cohort index. Default 0 days
#' @param postDays minimum number of observation days post cohort index. Default 0 days
#' @export
continuousObservation <- function(priorDays = 0L,
                          postDays = 0L) {

  new("ObservationWindow",
      priorDays = as.integer(priorDays),
      postDays = as.integer(postDays))

}

#Depreciated Endpoint functions
#' #' A function to offset the number of days relative to index
#' #' @param days a number specifying the number of days to offset from index where
#' #' an event may be observed. In this function a negative number means days before index
#' #' and a postive number means days after index.
#' #' @export
#' offset <- function(days) {
#'   coeff <- dplyr::if_else(sign(days) == 1, "after", "before", "before")
#'   new("Endpoint",
#'       days = as.integer(abs(days)),
#'       coeff = coeff)
#' }
#'
#' #' Function looking at all time before an event
#' #' @export
#' allDaysBefore <- function() {
#'   new("Endpoint",
#'       days = "all",
#'       coeff = "before")
#' }
#'
#' #' Function looking at all time after an event
#' #' @export
#' allDaysAfter <- function() {
#'   new("Endpoint",
#'       days = "all",
#'       coeff = "after")
#' }


## EventWindow ---------------------

#' Function creates an event window where the event starts
#' @param a the left side of the event window
#' @param b the right side of the event window
#' @param index specifying what part of the index we start looking for events
#' either at the index start date or index enddate
#' @export
eventStarts <- function(a, b, index = c("startDate", "endDate")){

  index <- checkmate::matchArg(index, c("startDate", "endDate"))

  new("EventWindow",
      event = "start",
      start = new("Endpoint",
                  days = abs(a),
                  coeff = sign(b)),
      end = new("Endpoint",
                days = abs(b),
                coeff = sign(b)),
      index = index)
}

#' Function creates an event window where the event ends
#' @param a the left side of the event window
#' @param b the right side of the event window
#' @param index specifying what part of the index we start looking for events
#' either at the index start date or index enddate
#' @export
eventEnds <- function(a, b, index = c("startDate", "endDate")) {
  index <- checkmate::matchArg(index, c("startDate", "endDate"))

  new("EventWindow",
      event = "end",
      start = new("Endpoint",
                  days = abs(a),
                  coeff = sign(b)),
      end = new("Endpoint",
                days = abs(b),
                coeff = sign(b)),
      index = index)
}


## Event Aperture -------------------
#' Function that creates an eventAperture an opening where an event can occur
#' relative to the index event
#' @param startWindow the starting window where an event can occur
#' @param endWindow the end window of where an event can occur. This parameter is optional
#' @param restrictVisit a logical toggle specifying whether the event should occur on the same visit
#' @param ignoreObservationPeriod a logical toggle specifying whether we can consider events outside the
#' observation period
#' @export
duringInterval <- function(startWindow,
                           endWindow = NULL,
                           restrictVisit = FALSE,
                           ignoreObservationPeriod = FALSE) {

  if (is.null(endWindow)) {
    res <- new("EventAperture",
               startWindow = startWindow,
               restrictVisit = restrictVisit,
               ignoreObservationPeriod = ignoreObservationPeriod)
  } else {
    res <- new("EventAperture",
               startWindow = startWindow,
               endWindow = endWindow,
               restrictVisit = restrictVisit,
               ignoreObservationPeriod = ignoreObservationPeriod)
  }
  return(res)
}


# Coercion ---------------------

## Coerce ObservationWindow ----
setMethod("as.list", "ObservationWindow", function(x) {
  ll <- list('PriorDays' = x@priorDays,
             'PostDays' = x@postDays)
  return(ll)

})

## Coerce Endpoint ----
setMethod("as.list", "Endpoint", function(x) {
  ll <- list('Days' = x@days,
             'Coeff' = as.integer(x@coeff))

  if (is.infinite(ll$Days)) {
    ll$Days <- NULL
  } else {
    ll$Days <- as.integer(ll$Days)
  }

  return(ll)

})

## Coerce EventWindow ----
setMethod("as.list", "EventWindow", function(x) {

  index <- ifelse(x@index == "endDate", TRUE, FALSE)
  event <- ifelse(x@event == "end", TRUE, FALSE)

  ll <- list('Start' = as.list(x@start),
             'End' = as.list(x@end),
             'UseIndexEnd' = index,
             'UseEventEnd' = event)

  return(ll)

})

## Coerce EventAperture ----

setMethod("as.list", "EventAperture", function(x) {
  ll <- list(
    'StartWindow' = as.list(x@startWindow),
    'EndWindow' = as.list(x@endWindow),
    'RestrictVisit' = x@restrictVisit,
    'IgnoreObservationPeriod' = x@ignoreObservationPeriod
  ) %>%
    purrr::discard(isFALSE)

  if (is.na(ll$EndWindow$UseEventEnd)) {
    ll$EndWindow <- NULL
  }

  return(ll)
})
