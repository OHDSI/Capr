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

#' Function to create age attribute
#' @param startWith character string either START_DATE or END_DATE
#' @param startOffset an integer value, default 0
#' @param endWith character string either START_DATE or END_DATE
#' @param endOffset an integer value, default 0
#' @return A dateAdjustment attribute class that can be used with a query
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
