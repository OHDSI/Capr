# operators --------------------
lt <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "lt",
                     value = as.integer(value))

  return(opAttribute)
}

gt <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "gt",
                     value = as.integer(value))

  return(opAttribute)
}


lte <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "lte",
                     value = as.integer(value))

  return(opAttribute)
}

gte <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "gte",
                     value = as.integer(value))

  return(opAttribute)
}


eq <- function(value) {
  opAttribute <- new("opAttribute",
                     op = "eq",
                     value = as.integer(value))

  return(opAttribute)
}

bt <- function(value, extent) {
  opAttribute <- new("opAttribute",
                     op = "bt",
                     value = as.integer(value),
                     extent = as.integer(extent))

  return(opAttribute)
}

nbt <- function(value, extent) {
  opAttribute <- new("opAttribute",
                     op = "!bt",
                     value = as.integer(value),
                     extent = as.integer(extent))

  return(opAttribute)
}

#op attribute names----------------------

age <- function(opAttribute) {
  opAttribute@name <- "age"
  return(opAttribute)
}

valueAsNumber <- function(opAttribute) {
  opAttribute@name <- "valueAsNumber"
  return(opAttribute)
}

rangeHigh <- function(opAttribute) {
  opAttribute@name <- "rangeHigh"
  return(opAttribute)
}

rangeLow <- function(opAttribute) {
  opAttribute@name <- "rangeLow"
  return(opAttribute)
}
