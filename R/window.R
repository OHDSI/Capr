# Classes --------------

## ObservationWindow -------------------
setClass("ObservationWindow",
         slots = c(priorDays = "integer",
                   postDays = "integer"),
         prototype = list(
           priorDays = 0L,
           postDays = 0L
         )
)

## Endpoint ----------------
#' An S4 class for an Endpoing
#' @description this determines the time in days relative to an index either before or after
#' @slot days either a character string all or an integer for the number of days
#' @slot coeff a character string either before or after
setClass("Endpoint",
         slots = c(
           days = "ANY",
           coeff = "character"
         ),
         prototype = list(
           days = NA_character_,
           coeff = NA_character_
         ))


## EventWindow -----------------------
#' An S4 class for a Window
#'
#' A window class provides details on the end points of the timeline
#'
#' @slot Event a character string either start or end. Identifies the point of reference for the window
#' @slot Start an endpoint object containing the days and coefficient for the start of the window
#' @slot End an endpoint object containing the days and coefficient for the end of the window
#' @slot Index A character string either start or end. Identifies where the index is relative to the window
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

## Aperture -------------------

#' An S4 class for Aperture
#'
#'The aperture class provides context to when the criteria must be observed in a person timeline to pretain to the expression
#'
#' @slot StartWindow a EventWindow class object identifying the start window
#' @slot EndWindow a EventWindow class object ifentifying the end window (optional)
#' @slot RestrictVisit a logic toggle where TRUE restricts to the same visit
#' @slot IgnoreObservationPeriod a logic toggle where TRUE allows events outside the observation period
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

observationWindow <- function(priorDays = 0L,
                          postDays = 0L) {

  new("ObservationWindow",
      priorDays = as.integer(priorDays),
      postDays = as.integer(postDays))

}

## Endpoint ------

offset <- function(days) {
  coeff <- dplyr::if_else(sign(days) == 1, "after", "before", "before")
  new("Endpoint",
      days = as.integer(abs(days)),
      coeff = coeff)
}

allDaysBefore <- function() {
  new("Endpoint",
      days = "all",
      coeff = "before")
}

allDaysAfter <- function() {
  new("Endpoint",
      days = "all",
      coeff = "after")
}


## Timeline ---------------------

eventStarts <- function(a, b, index = c("startDate", "endDate")){

  index <- checkmate::matchArg(index, c("startDate", "endDate"))
  new("EventWindow",
      event = "start",
      start = a,
      end = b,
      index = index)
}

eventEnds <- function(a, b, index = c("startDate", "endDate")) {
  index <- checkmate::matchArg(index, c("startDate", "endDate"))
  new("EventWindow",
      event = "end",
      start = a,
      end = b,
      index = index)
}

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


# Coersion ---------------------
