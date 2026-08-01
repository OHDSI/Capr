# date Adjustment Attribute


# Class ----------------------------

#' An S4 class for a date adjustment attribute
#' @slot name the name of the attribute
#' @slot startWith character string either START_DATE or END_DATE
#' @slot startOffset an integer value, default 0
#' @slot endWith character string either START_DATE or END_DATE
#' @slot endOffset an integer value, default 0
setClass("dateAdjustmentAttribute",
         slots = c(name = "character",
                   startWith = "character",
                   startOffset = "integer",
                   endWith = "character",
                   endOffset = "integer"
         ),
         prototype = list(
           name = "DateAdjustment",
           startWith = "START_DATE",
           startOffset = 0L,
           endWith = "END_DATE",
           endOffset = 0L
         )
)

# Builder -----------------

#' Shift the start and/or end date of a query event
#'
#' Adds a \code{DateAdjustment} attribute to a domain query, shifting the event's effective
#' start and/or end date relative to a reference anchor (\code{START_DATE} or \code{END_DATE})
#' and an offset in days. Used in Circe wherever \code{DateAdjustment} is supported.
#'
#' For \code{observationPeriod()} and \code{payerPlanPeriod()}, \code{dateAdjustment()} also
#' fulfils the role of Circe's \code{UserDefinedPeriod}: when a \code{dateAdjustmentAttribute}
#' with \code{name = "DateAdjustment"} is present on an ObservationPeriod or PayerPlanPeriod
#' query, Capr serializes it as \code{UserDefinedPeriod \{ StartDate, EndDate \}} in the
#' Circe JSON, which constrains the period to a caller-specified date window.
#'
#' @param startWith character; anchor for the adjusted start date, either
#'   \code{"START_DATE"} (default) or \code{"END_DATE"}.
#' @param startOffset integer offset in days added to the \code{startWith} anchor. Default 0.
#' @param endWith character; anchor for the adjusted end date, either
#'   \code{"END_DATE"} (default) or \code{"START_DATE"}.
#' @param endOffset integer offset in days added to the \code{endWith} anchor. Default 0.
#' @return A \code{dateAdjustmentAttribute} for use in any domain query constructor.
#' @export
dateAdjustment <- function(startWith = "START_DATE",
                           startOffset = 0L,
                           endWith = "END_DATE",
                           endOffset = 0L) {


  methods::new("dateAdjustmentAttribute",
               startWith = startWith,
               startOffset = startOffset,
               endWith = endWith,
               endOffset = endOffset)

}

# Coercion --------------

setMethod("as.list", "dateAdjustmentAttribute", function(x) {

  atr <- list(
    StartWith = x@startWith,
    StartOffset = x@startOffset,
    EndWith = x@endWith,
    EndOffset = x@endOffset)

  tibble::lst(`:=`(!!x@name, atr))
})
