# Classes ----------------------------

## opAttributeSuper ----

#' An S4 super class for other opAttribute objects to inherit.
setClass("opAttributeSuper",
         slots = c(name = "character"))

setMethod("show", "opAttributeSuper", function(object) {
  symbol <- opToPrint(object@op)
  if (symbol == "-") {
    pp <- paste0("in {", object@value, symbol, object@extent, "}")
  } else if (symbol == "!-") {
    pp <- paste0("not in {", object@value, "-", object@extent, "}")
  } else {
    pp <- paste(symbol, object@value)
  }

  txt <- paste0("Capr Op Attribute: ", object@name, " ", pp)
  cli::cat_bullet(txt, bullet = "sup_plus")
})

## opAttributeNumeric ----

#' An S4 class for a op attribute that is a numeric
#' @slot
#' name the name of the attribute
#' @slot
#' op the operator one of: gt,lt,gte,lte,eq,bt,!bt
#' @slot
#' value a value serving as the single limit or lower limit in a bt
#' @slot
#' extent a value serving as the upper limit in a bt, otherwise this is empty
setClass("opAttributeNumeric",
         contains = "opAttributeSuper",
         slots = c(name = "character", op = "character", value = "numeric", extent = "numeric"),

         prototype = list(name = NA_character_, op = NA_character_, value = NA_real_, extent = NA_real_))


## opAttributeInteger ----

#' An S4 class for a op attribute that is an integer
#' @slot
#' name the name of the attribute
#' @slot
#' op the operator one of: gt,lt,gte,lte,eq,bt,!bt
#' @slot
#' value a value serving as the single limit or lower limit in a bt.
#' @slot
#' extent a value serving as the upper limit in a bt, otherwise this is empty
setClass("opAttributeInteger",
         contains = "opAttributeSuper",
         slots = c(name = "character", op = "character", value = "integer", extent = "integer"),

         prototype = list(name = NA_character_,
                          op = NA_character_,
                          value = NA_integer_,
                          extent = NA_integer_))


## opAttributeDate ----

#' An S4 class for a op attribute that is a date
#' @slot
#' name the name of the attribute
#' @slot
#' op the operator one of: gt,lt,gte,lte,eq,bt,!bt
#' @slot
#' value a value serving as the single limit or lower limit in a bt.
#' @slot
#' extent a value serving as the upper limit in a bt, otherwise this is empty
setClass("opAttributeDate",
         contains = "opAttributeSuper",
         slots = c(name = "character", op = "character", value = "Date", extent = "Date"),

         prototype = list(name = NA_character_,
                          op = NA_character_,
                          value = lubridate::NA_Date_,
                          extent = lubridate::NA_Date_))

# Helpers --------------------

opToPrint <- function(x) {
  tibble::tibble(symbol = c("<", "<=", ">", ">=", "==", "-", "!-"), op = c("lt", "lte", "gt", "gte",
                                                                           "eq", "bt", "!bt")) %>%
    dplyr::filter(.data$op == x) %>%
    dplyr::pull(.data$symbol)
}

## lt --------
#' Less than operator
#' @description
#' function that builds an opAttribute based on less than logic
#' @param x   the value to used as a bound in the op logic. This can either be an integer, numeric, or
#'            Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("lt", function(x) standardGeneric("lt"))

#' @rdname
#' lt
#' @aliases
#' lt,integer-method
setMethod("lt", "integer", function(x) {
  methods::new("opAttributeInteger", op = "lt", value = x)
})

#' @rdname
#' lt
#' @aliases
#' lt,numeric-method
setMethod("lt", "numeric", function(x) {
  methods::new("opAttributeNumeric", op = "lt", value = x)
})

#' @rdname
#' lt
#' @aliases
#' lt,Date-method
setMethod("lt", "Date", function(x) {
  methods::new("opAttributeDate", op = "lt", value = x)
})

## gt --------
#' Greater than operator
#' @description
#' function that builds an opAttribute based on greater than logic
#' @param x   the value to used as a bound in the op logic. This can either be an integer, numeric, or
#'            Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("gt", function(x) standardGeneric("gt"))

#' @rdname
#' gt
#' @aliases
#' gt,integer-method
setMethod("gt", "integer", function(x) {
  methods::new("opAttributeInteger", op = "gt", value = x)
})
#' @rdname
#' gt
#' @aliases
#' gt,numeric-method
setMethod("gt", "numeric", function(x) {
  methods::new("opAttributeNumeric", op = "gt", value = x)
})
#' @rdname
#' gt
#' @aliases
#' gt,Date-method
setMethod("gt", "Date", function(x) {
  methods::new("opAttributeDate", op = "gt", value = x)
})

## lte --------
#' Less than or equal to operator
#' @description
#' function that builds an opAttribute based on less than or equal to than logic
#' @param x   the value to used as a bound in the op logic. This can either be an integer, numeric, or
#'            Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("lte", function(x) standardGeneric("lte"))

#' @rdname
#' lte
#' @aliases
#' lte,integer-method
setMethod("lte", "integer", function(x) {
  methods::new("opAttributeInteger", op = "lte", value = x)
})
#' @rdname
#' lte
#' @aliases
#' lte,numeric-method
setMethod("lte", "numeric", function(x) {
  methods::new("opAttributeNumeric", op = "lte", value = x)
})
#' @rdname
#' lte
#' @aliases
#' lte,Date-method
setMethod("lte", "Date", function(x) {
  methods::new("opAttributeDate", op = "lte", value = x)
})

## gte --------
#' Greater than or equal to operator
#' @description
#' function that builds an opAttribute based on greater than or equal to logic
#' @param x   the value to used as a bound in the op logic. This can either be an integer, numeric, or
#'            Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("gte", function(x) standardGeneric("gte"))

#' @rdname
#' gte
#' @aliases
#' gte,integer-method
setMethod("gte", "integer", function(x) {
  methods::new("opAttributeInteger", op = "gte", value = x)
})
#' @rdname
#' gte
#' @aliases
#' gte,numeric-method
setMethod("gte", "numeric", function(x) {
  methods::new("opAttributeNumeric", op = "gte", value = x)
})
#' @rdname
#' gte
#' @aliases
#' gte,Date-method
setMethod("gte", "Date", function(x) {
  methods::new("opAttributeDate", op = "gte", value = x)
})

## eq --------
#' Equal to operator
#' @description
#' function that builds an opAttribute based on equal to logic
#' @param x   the value to used as a bound in the op logic. This can either be an integer, numeric, or
#'            Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("eq", function(x) standardGeneric("eq"))

#' @rdname
#' eq
#' @aliases
#' eq,integer-method
setMethod("eq", "integer", function(x) {
  methods::new("opAttributeInteger", op = "eq", value = x)
})
#' @rdname
#' eq
#' @aliases
#' eq,numeric-method
setMethod("eq", "numeric", function(x) {
  methods::new("opAttributeNumeric", op = "eq", value = x)
})
#' @rdname
#' eq
#' @aliases
#' eq,Date-method
setMethod("eq", "Date", function(x) {
  methods::new("opAttributeDate", op = "eq", value = x)
})


## bt --------
#' Between operator
#' @description
#' function that builds an opAttribute based on between logic
#' @param x   the left side bound of the between logic This can either be an integer, numeric, or Date
#'            data type. Different data types will return the appropriate opAttribute type
#' @param y   the right side bound of the between logic. This can either be an integer, numeric, or
#'            Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("bt", function(x, y) standardGeneric("bt"))

#' @rdname
#' bt
#' @aliases
#' bt,integer-method
setMethod("bt", "integer", function(x, y) {
  methods::new("opAttributeInteger", op = "bt", value = x, extent = y)
})

#' @rdname
#' bt
#' @aliases
#' bt,numeric-method
setMethod("bt", "numeric", function(x, y) {
  methods::new("opAttributeNumeric", op = "bt", value = x, extent = y)
})

#' @rdname
#' bt
#' @aliases
#' bt,Date-method
setMethod("bt", "Date", function(x, y) {
  methods::new("opAttributeDate", op = "bt", value = x, extent = y)
})

## nbt --------
#' Not between operator
#' @description
#' function that builds an opAttribute based on not between logic
#' @param x   the left side bound of the between logic This can either be an integer, numeric, or Date
#'            data type. Different data types will return the appropriate opAttribute type
#' @param y   the right side bound of the between logic. This can either be an integer, numeric, or
#'            Date data type. Different data types will return the appropriate opAttribute type
#' @export
#' @docType methods
setGeneric("nbt", function(x, y) standardGeneric("nbt"))

#' @rdname
#' nbt
#' @aliases
#' nbt,integer-method
setMethod("nbt", "integer", function(x, y) {
  methods::new("opAttributeInteger", op = "!bt", value = x, extent = y)
})
#' @rdname
#' nbt
#' @aliases
#' nbt,numeric-method
setMethod("nbt", "numeric", function(x, y) {
  methods::new("opAttributeNumeric", op = "!bt", value = x, extent = y)
})
#' @rdname
#' nbt
#' @aliases
#' nbt,Date-method
setMethod("nbt", "Date", function(x, y) {
  methods::new("opAttributeDate", op = "!bt", value = x, extent = y)
})

# Constructors -----------

## Integer Constructors -----

#' Function to create age attribute
#' @param op   an opAttribute object that is either numeric or integer that defines the logical
#'             operation used to determine eligible patient age
#' @return An age attribute that can be used in a cohort definition
#' @export
age <- function(op) {

  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeInteger",
               name = "Age",
               op = op@op,
               value = as.integer(op@value),
               extent = as.integer(op@extent))
}

#' Function to create days supply attribute
#' @description
#' This function is used only for a drug query. days supply is a column in the drug exposure table of
#' the cdm. This attribute allows a subquery to find drugs that satisfy certain values determined by
#' the op logic.
#' @param op   an opAttribute object that is either numeric or integer that defines the logical
#'             operation used to determine eligible number of days of supply
#' @return An attribute that can be used in a cohort definition
#' @export
daysOfSupply <- function(op) {

  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeInteger", name = "DaysSupply", op = op@op, value = as.integer(op@value),
               extent = as.integer(op@extent))
}


#' Function to create refills attribute
#' @description
#' This function is used only for a drug query. refills is a column in the drug exposure table of the
#' cdm. This attribute allows a subquery to find drugs that satisfy certain values determined by the
#' op logic.
#' @param op   an opAttribute object that is either numeric or integer that defines the logical
#'             operation used to determine eligible number of refills
#' @return An attribute that can be used in a cohort definition
#' @export
drugRefills <- function(op) {

  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeInteger",
               name = "Refills",
               op = op@op,
               value = as.integer(op@value),
               extent = as.integer(op@extent))
}

## Numeric Constructors ----

#' Function to create valueAsNumber attribute
#' @description
#' This function is used only for measurement query. valueAsNumber is a column in the measurement
#' table of the cdm. This attribute allows a subquery to find measurements that satisfy certain values
#' determined by the op logic.
#' @param op   an opAttribute object that is either numeric or integer that defines the logical
#'             operation used to determine eligible patient age
#' @return An attribute that can be used in a cohort definition
#' @export
valueAsNumber <- function(op) {

  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeNumeric",
               name = "ValueAsNumber",
               op = op@op,
               value = op@value,
               extent = op@extent)
}

#' Function to create rangeHigh attribute
#' @description
#' This function is used only for measurement query. range_high is a column in the measurement table
#' of the cdm. This attribute allows a subquery to find measurements that satisfy certain values
#' determined by the op logic.
#' @param op   an opAttribute object that is either numeric or integer that defines the logical
#'             operation used to determine eligible range high
#' @return An attribute that can be used in a cohort definition
#' @export
rangeHigh <- function(op) {

  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeNumeric",
               name = "RangeHigh",
               op = op@op,
               value = op@value,
               extent = op@extent)
}

#' Function to create rangeLow attribute
#' @description
#' This function is used only for measurement query. range_low is a column in the measurement table of
#' the cdm. This attribute allows a subquery to find measurements that satisfy certain values
#' determined by the op logic.
#' @param op   an opAttribute object that is either numeric or integer that defines the logical
#'             operation used to determine eligible range low
#' @return An attribute that can be used in a cohort definition
#' @export
rangeLow <- function(op) {

  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeNumeric",
               name = "RangeLow",
               op = op@op,
               value = op@value,
               extent = op@extent)
}



#' Function to create quantity attribute
#' @description
#' This function is used only for a drug query. quantity is a column in the drug exposure table of the
#' cdm. This attribute allows a subquery to find drugs that satisfy certain values determined by the
#' op logic.
#' @param op   an opAttribute object that is either numeric or integer that defines the logical
#'             operation used to determine eligible quantity
#' @return An attribute that can be used in a cohort definition
#' @export
drugQuantity <- function(op) {

  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeNumeric",
               name = "Quantity",
               op = op@op,
               value = op@value,
               extent = op@extent)
}




## Date Constructors ----

#' Function that creates a start date attribute
#' @param op   an opAttribute object must be a date that defines the logical operation used to
#'             determine eligible start dates
#' @return An attribute that can be used in a cohort definition
#' @export
startDate <- function(op) {

  check <- all(grepl("opAttribute(Date|Super)", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeDate.")
  }

  methods::new("opAttributeDate",
               name = "OccurrenceStartDate",
               op = op@op,
               value = op@value,
               extent = op@extent)
}

#' Function that creates a end date attribute
#' @param op   an opAttribute object must be a date that defines the logical operation used to
#'             determine eligible end dates
#' @return An attribute that can be used in a cohort definition
#' @export
endDate <- function(op) {

  check <- all(grepl("opAttribute(Date|Super)", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeDate.")
  }

  methods::new("opAttributeDate",
               name = "OccurrenceEndDate",
               op = op@op,
               value = op@value,
               extent = op@extent)
}

# Coercion ------------
#' @importFrom rlang :=
listOpAttribute <- function(x) {
  atr <- list(Op = x@op, Value = x@value, Extent = x@extent) %>%
    purrr::discard(is.na)

  tibble::lst(`:=`(!!x@name, atr))
}

## Coerce Numeric ----
setMethod("as.list", "opAttributeSuper", listOpAttribute)
