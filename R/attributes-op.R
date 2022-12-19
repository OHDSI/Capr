# Classes ----------------------------

## opAttributeNumeric ----

#' An S4 class for a op attribute that is a numeric
#' @slot name the name of the attribute
#' @slot op the operator one of: gt,lt,gte,lte,eq,bt,!bt
#' @slot value a value serving as the single limit or lower limit in a bt
#' @slot extent a value serving as the upper limit in a bt, otherwise this is empty
setClass("opAttributeNumeric",
         slots = c(
           name = "character",
           op = "character",
           value = "numeric",
           extent = "numeric"
         ),
         prototype = list(
           name = NA_character_,
           op = NA_character_,
           value = NA_real_,
           extent = NA_real_
         ))

#' @aliases show,opAttributeNumeric-method
setMethod("show", "opAttributeNumeric", function(object) {
  symbol <- opToPrint(object@op)
  if (symbol == "-") {
    pp <- paste0("in {", object@value, symbol, object@extent, "}")
  } else if (symbol == "!-"){
    pp <- paste0("not in {", object@value, "-", object@extent, "}")
  } else {
    pp <- paste(symbol, object@value)
  }

  txt <- paste0("Capr Op Attribute Numeric: ", object@name, " ", pp)
  cli::cat_bullet(txt, bullet = "sup_plus")

})


## opAttributeInteger ----

#' An S4 class for a op attribute that is an integer
#' @slot name the name of the attribute
#' @slot op the operator one of: gt,lt,gte,lte,eq,bt,!bt
#' @slot value a value serving as the single limit or lower limit in a bt.
#' @slot extent a value serving as the upper limit in a bt, otherwise this is empty
setClass("opAttributeInteger",
         slots = c(
           name = "character",
           op = "character",
           value = "integer",
           extent = "integer"
         ),
         prototype = list(
           name = NA_character_,
           op = NA_character_,
           value = NA_integer_,
           extent = NA_integer_
         ))


#' @aliases show,opAttributeInteger-method
setMethod("show", "opAttributeInteger", function(object) {
  symbol <- opToPrint(object@op)
  if (symbol == "-") {
    pp <- paste0("in {", object@value, symbol, object@extent, "}")
  } else if (symbol == "!-"){
    pp <- paste0("not in {", object@value, "-", object@extent, "}")
  } else {
    pp <- paste(symbol, object@value)
  }
  txt <- paste0("Capr Op Attribute Integer: ", object@name, " ", pp)
  cli::cat_bullet(txt, bullet = "sup_plus")

})


## opAttributeDate ----

#' An S4 class for a op attribute that is a date
#' @slot name the name of the attribute
#' @slot op the operator one of: gt,lt,gte,lte,eq,bt,!bt
#' @slot value a value serving as the single limit or lower limit in a bt.
#' @slot extent a value serving as the upper limit in a bt, otherwise this is empty
setClass("opAttributeDate",
         slots = c(
           name = "character",
           op = "character",
           value = "Date",
           extent = "Date"
         ),
         prototype = list(
           name = NA_character_,
           op = NA_character_,
           value = lubridate::NA_Date_,
           extent = lubridate::NA_Date_
         ))


#' @aliases show,opAttributeDate-method
setMethod("show", "opAttributeDate", function(object) {
  symbol <- opToPrint(object@op)
  if (symbol == "-") {
    pp <- paste0("in {", object@value, symbol, object@extent, "}")
  } else if (symbol == "!-"){
    pp <- paste0("not in {", object@value, "-", object@extent, "}")
  } else {
    pp <- paste(symbol, object@value)
  }
  txt <- paste0("Capr Op Attribute Date: ", object@name, " ", pp)
  cli::cat_bullet(txt, bullet = "sup_plus")

})

# Helpers --------------------

opToPrint <- function(x) {
  tibble::tibble(
    symbol = c("<", "<=", ">", ">=", "==", "-", "!-"),
    op = c("lt", "lte", "gt", "gte", "eq", "bt", "!bt")
    ) %>%
    dplyr::filter(op == x) %>%
    dplyr::pull(symbol)
}

## lt --------
#' Less than operator
#' @description function that builds an opAttribute based on less than logic
#' @param x the value to used as a bound in the op logic. This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("lt", function(x) standardGeneric("lt"))

#' @rdname lt
#' @aliases lt,integer-method
setMethod('lt', "integer", function(x){
  new("opAttributeInteger",
      op = "lt",
      value = x)
})

#' @rdname lt
#' @aliases lt,numeric-method
setMethod('lt', "numeric", function(x){
  new("opAttributeNumeric",
      op = "lt",
      value = x)
})

#' @rdname lt
#' @aliases lt,Date-method
setMethod('lt', "Date", function(x){
  new("opAttributeDate",
      op = "lt",
      value = x)
})

## gt --------
#' Greater than operator
#' @description function that builds an opAttribute based on greater than logic
#' @param x the value to used as a bound in the op logic. This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("gt", function(x) standardGeneric("gt"))

#' @rdname gt
#' @aliases gt,integer-method
setMethod('gt', "integer", function(x){
  new("opAttributeInteger",
      op = "gt",
      value = x)
})
#' @rdname gt
#' @aliases gt,numeric-method
setMethod('gt', "numeric", function(x){
  new("opAttributeNumeric",
      op = "gt",
      value = x)
})
#' @rdname gt
#' @aliases gt,Date-method
setMethod('gt', "Date", function(x){
  new("opAttributeDate",
      op = "gt",
      value = x)
})

## lte --------
#' Less than or equal to operator
#' @description function that builds an opAttribute based on less than or equal to than logic
#' @param x the value to used as a bound in the op logic. This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("lte", function(x) standardGeneric("lte"))

#' @rdname lte
#' @aliases lte,integer-method
setMethod('lte', "integer", function(x){
  new("opAttributeInteger",
      op = "lte",
      value = x)
})
#' @rdname lte
#' @aliases lte,numeric-method
setMethod('lte', "numeric", function(x){
  new("opAttributeNumeric",
      op = "lte",
      value = x)
})
#' @rdname lte
#' @aliases lte,Date-method
setMethod('lte', "Date", function(x){
  new("opAttributeDate",
      op = "lte",
      value = x)
})

## gte --------
#' Greater than or equal to operator
#' @description function that builds an opAttribute based on greater than or equal to logic
#' @param x the value to used as a bound in the op logic. This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("gte", function(x) standardGeneric("gte"))

#' @rdname gte
#' @aliases gte,integer-method
setMethod('gte', "integer", function(x){
  new("opAttributeInteger",
      op = "gte",
      value = x)
})
#' @rdname gte
#' @aliases gte,numeric-method
setMethod('gte', "numeric", function(x){
  new("opAttributeNumeric",
      op = "gte",
      value = x)
})
#' @rdname gte
#' @aliases gte,Date-method
setMethod('gte', "Date", function(x){
  new("opAttributeDate",
      op = "gte",
      value = x)
})

## eq --------
#' Equal to operator
#' @description function that builds an opAttribute based on equal to logic
#' @param x the value to used as a bound in the op logic. This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("eq", function(x) standardGeneric("eq"))

#' @rdname eq
#' @aliases eq,integer-method
setMethod('eq', "integer", function(x){
  new("opAttributeInteger",
      op = "eq",
      value = x)
})
#' @rdname eq
#' @aliases eq,numeric-method
setMethod('eq', "numeric", function(x){
  new("opAttributeNumeric",
      op = "eq",
      value = x)
})
#' @rdname eq
#' @aliases eq,Date-method
setMethod('eq', "Date", function(x){
  new("opAttributeDate",
      op = "eq",
      value = x)
})


## bt --------
#' Between operator
#' @description function that builds an opAttribute based on  between logic
#' @param x the left side bound of the between logic This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @param y the right side bound of the between logic. This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("bt", function(x, y) standardGeneric("bt"))

#' @rdname bt
#' @aliases bt,integer-method
setMethod('bt', "integer", function(x, y){
  new("opAttributeInteger",
      op = "bt",
      value = x,
      extent = y)
})

#' @rdname bt
#' @aliases bt,numeric-method
setMethod('bt', "numeric", function(x, y){
  new("opAttributeNumeric",
      op = "bt",
      value = x,
      extent = y)
})

#' @rdname bt
#' @aliases bt,Date-method
setMethod('bt', "Date", function(x, y){
  new("opAttributeDate",
      op = "bt",
      value = x,
      extent = y)
})

## nbt --------
#' Not between operator
#' @description function that builds an opAttribute based on not between logic
#' @param x the left side bound of the between logic This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @param y the right side bound of the between logic. This can either be an integer,
#' numeric, or Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("nbt", function(x, y) standardGeneric("nbt"))

#' @rdname nbt
#' @aliases bt,integer-method
setMethod('nbt', "integer", function(x, y){
  new("opAttributeInteger",
      op = "!bt",
      value = x,
      extent = y)
})
#' @rdname nbt
#' @aliases bt,numeric-method
setMethod('nbt', "numeric", function(x, y){
  new("opAttributeNumeric",
      op = "!bt",
      value = x,
      extent = y)
})
#' @rdname nbt
#' @aliases nbt,Date-method
setMethod('nbt', "Date", function(x, y){
  new("opAttributeDate",
      op = "!bt",
      value = x,
      extent = y)
})

# Constructors -----------

## Integer Constructors -----

#' Function to create age attribute
#' @param op an opAttribute object that is either numeric or integer that defines
#' the logical operation used to determine eligible patient age
#' @export
age <- function(op) {

  check <- grepl("opAttribute", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  new("opAttributeInteger",
      name = "Age",
      op = op@op,
      value = as.integer(op@value),
      extent = as.integer(op@extent))
}

#' Function to create days supply attribute
#' @description This function is used only for a drug  query. days supply is a column
#' in the drug exposure table of the cdm. This attribute allows a subquery to find drugs
#' that satisfy certain values determined by the op logic.
#' @param op an opAttribute object that is either numeric or integer that defines
#' the logical operation used to determine eligible number of days of supply
#' @export
daysOfSupply <- function(op) {

  check <- grepl("opAttribute", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  new("opAttributeInteger",
      name = "DaysSupply",
      op = opAttribute@op,
      value = as.integer(opAttribute@value),
      extent = as.integer(opAttribute@extent))
}


#' Function to create refills attribute
#' @description This function is used only for a drug  query. refills is a column
#' in the drug exposure table of the cdm. This attribute allows a subquery to find drugs
#' that satisfy certain values determined by the op logic.
#' @param op an opAttribute object that is either numeric or integer that defines
#' the logical operation used to determine eligible number of refills
#' @export
drugRefills <- function(op) {

  check <- grepl("opAttribute", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  new("opAttributeInteger",
      name = "Refills",
      op = opAttribute@op,
      value = as.integer(opAttribute@value),
      extent = as.integer(opAttribute@extent))
}

## Numeric Constructors ----

#' Function to create valueAsNumber attribute
#' @description This function is used only for measurement query. valueAsNumber is a column
#' in the measurement table of the cdm. This attribute allows a subquery to find measurements
#' that satisfy certain values determined by the op logic.
#' @param op an opAttribute object that is either numeric or integer that defines
#' the logical operation used to determine eligible patient age
#' @export
valueAsNumber<- function(op) {

  check <- grepl("opAttribute", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  new("opAttributeNumeric",
      name = "ValueAsNumber",
      op = op@op,
      value = as.integer(op@value),
      extent = as.integer(op@extent))
}

#' Function to create rangeHigh attribute
#' @description This function is used only for measurement query. range_high is a column
#' in the measurement table of the cdm. This attribute allows a subquery to find measurements
#' that satisfy certain values determined by the op logic.
#' @param op an opAttribute object that is either numeric or integer that defines
#' the logical operation used to determine eligible range high
#' @export
rangeHigh <- function(op) {

  check <- grepl("opAttribute", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  new("opAttributeNumeric",
      name = "RangeHigh",
      op = op@op,
      value = as.integer(op@value),
      extent = as.integer(op@extent))
}

#' Function to create rangeLow attribute
#' @description This function is used only for measurement query. range_low is a column
#' in the measurement table of the cdm. This attribute allows a subquery to find measurements
#' that satisfy certain values determined by the op logic.
#' @param op an opAttribute object that is either numeric or integer that defines
#' the logical operation used to determine eligible range low
#' @export
rangeLow <- function(op) {

  check <- grepl("opAttribute", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  new("opAttributeNumeric",
      name = "RangeLow",
      op = opAttribute@op,
      value = as.integer(opAttribute@value),
      extent = as.integer(opAttribute@extent))
}



#' Function to create quantity attribute
#' @description This function is used only for a drug  query. quantity is a column
#' in the drug exposure table of the cdm. This attribute allows a subquery to find drugs
#' that satisfy certain values determined by the op logic.
#' @param op an opAttribute object that is either numeric or integer that defines
#' the logical operation used to determine eligible quantity
#' @export
drugQuantity <- function(op) {

  check <- grepl("opAttribute", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  new("opAttributeNumeric",
      name = "Quantity",
      op = opAttribute@op,
      value = as.integer(opAttribute@value),
      extent = as.integer(opAttribute@extent))
}




## Date Constructors ----

#' Function that creates a start date attribute
#' @param op an opAttribute object must be a date that defines
#' the logical operation used to determine eligible start dates
#' @export
startDate <- function(op) {

  check <- grepl("opAttributeDate", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeDate.")
  }

  new("opAttributeDate",
      name = "StartDate",
      op = opAttribute@op,
      value = as.integer(opAttribute@value),
      extent = as.integer(opAttribute@extent))
}

#' Function that creates a end date attribute
#' @param op an opAttribute object must be a date that defines
#' the logical operation used to determine eligible end dates
#' @export
endDate <- function(op) {

  check <- grepl("opAttributeDate", methods::is(op))
  if (!check) {
    stop("Input must be an opAttributeDate.")
  }

  new("opAttributeDate",
      name = "EndDate",
      op = opAttribute@op,
      value = as.integer(opAttribute@value),
      extent = as.integer(opAttribute@extent))
}
