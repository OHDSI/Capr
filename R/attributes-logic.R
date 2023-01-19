# Classes ----------------------------

## Logic Attribute -----
#' An S4 class for a logical attribute
#' @description with a logic attribute if it is specified than we assume it is true
#' @slot name the name of the attribute
setClass("logicAttribute",
         slots = c(
           name = "character"
         ),
         prototype = list(
           name = NA_character_
         ))
# Constructors -----------
firstOccurrence <- function() {
  new("logicAttribute",
      name = "First")
}