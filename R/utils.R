#' Print a line break
#'
#' @param t A number from 1 to 4 representing the type of line break
#'
#' @return Prints a line break. Does not return a value.
lineBreak <- function(t = c(1,2,3,4)) {
  if (t == 1) {
    cat("\n=====================================================================\n")
  }
  if (t == 2) {
    cat("\n______________________________________________\n\n")
  }
  if (t == 3) {
    cat("\n----------------------------\n")
  }
  if (t == 4) {
    cat("\n*****\n")
  }
}

