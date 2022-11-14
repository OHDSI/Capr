# operators --------------------


#' @export
lt <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "lt",
                     value = as.integer(value))

  return(opAttribute)
}

#' @export
gt <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "gt",
                     value = as.integer(value))

  return(opAttribute)
}

#' @export
lte <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "lte",
                     value = as.integer(value))

  return(opAttribute)
}

#' @export
gte <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "gte",
                     value = as.integer(value))

  return(opAttribute)
}

#' @export
eq <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "eq",
                     value = as.integer(value))

  return(opAttribute)
}

#' @export
bt <- function(value, extent) {
  opAttribute <- new("opAttribute",
                     op = "bt",
                     value = as.integer(value),
                     extent = as.integer(extent))

  return(opAttribute)
}

#' @export
nbt <- function(value, extent) {
  opAttribute <- new("opAttribute",
                     op = "!bt",
                     value = as.integer(value),
                     extent = as.integer(extent))

  return(opAttribute)
}

#op attribute names----------------------

#' @export
age <- function(opAttribute) {
  opAttribute@name <- "age"
  return(opAttribute)
}

#' @export
valueAsNumber <- function(opAttribute) {
  opAttribute@name <- "valueAsNumber"
  return(opAttribute)
}

#' @export
rangeHigh <- function(opAttribute) {
  opAttribute@name <- "rangeHigh"
  return(opAttribute)
}

#' @export
rangeLow <- function(opAttribute) {
  opAttribute@name <- "rangeLow"
  return(opAttribute)
}
