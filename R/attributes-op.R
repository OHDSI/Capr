# Classes ----------------------------

## opAttributeSuper ----

#' An S4 super class for other opAttribute objects to inherit.
setClass("opAttributeSuper",
         slots = c(name = "character"))

setMethod("show", "opAttributeSuper", function(object) {
  symbol <- opToPrint(object@op)
  # For string operators that don't have a symbol mapping
  if (is.na(symbol) || length(symbol) == 0) {
    pp <- paste(object@op, object@value)
  } else if (symbol == "-") {
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


## opAttributeCharacter ----

#' An S4 class for a op attribute that is a character
#' @slot
#' name the name of the attribute
#' @slot
#' op the operator one of: startsWith, contains, endsWith, !startsWith, !contains, !endsWith
#' @slot
#' value a value or pattern for comparison
#' @slot
#' extent unused for character attributes
setClass("opAttributeCharacter",
         contains = "opAttributeSuper",
         slots = c(name = "character", op = "character", value = "character", extent = "character"),

         prototype = list(name = NA_character_,
                          op = NA_character_,
                          value = NA_character_,
                          extent = NA_character_))





# Helpers --------------------

opToPrint <- function(x) {
  mappings <- tibble::tibble(
    symbol = c("<", "<=", ">", ">=", "==", "-", "!-"),
    op = c("lt", "lte", "gt", "gte", "eq", "bt", "!bt")
  )
  result <- mappings |>
    dplyr::filter(.data$op == x) |>
    dplyr::pull(.data$symbol)
  # Return NA for operators not in the mapping (e.g., string operators)
  if (length(result) == 0) return(NA_character_) else return(result)
}

## lt --------
#' Less than operator
#'
#' Builds a comparison operator for use inside attribute functions such as \code{\link{age}},
#' \code{\link{valueAsNumber}}, \code{\link{startDate}}, etc.
#' Accepts integer, numeric, or Date inputs; returns the matching opAttribute type.
#'
#' @param x Comparison bound (integer, numeric, or Date).
#' @return An opAttribute for use inside attribute constructor functions.
#' @seealso \code{\link{gt}}, \code{\link{gte}}, \code{\link{lte}}, \code{\link{eq}},
#'   \code{\link{bt}}, \code{\link{age}}, \code{\link{valueAsNumber}}
#' @examples
#' # Patients under 18
#' conditionOccurrence(cs(1L, name = "test"), age(lt(18L)))
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
#'
#' Builds a comparison operator for use inside attribute functions such as \code{\link{age}},
#' \code{\link{valueAsNumber}}, \code{\link{startDate}}, etc.
#' Accepts integer, numeric, or Date inputs; returns the matching opAttribute type.
#'
#' @param x Comparison bound (integer, numeric, or Date).
#' @return An opAttribute for use inside attribute constructor functions.
#' @seealso \code{\link{lt}}, \code{\link{gte}}, \code{\link{lte}}, \code{\link{eq}}, \code{\link{bt}}
#' @examples
#' # HbA1c > 6.5
#' measurement(cs(1L, name = "HbA1c"), valueAsNumber(gt(6.5)))
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
#'
#' Builds a comparison operator for use inside attribute functions such as \code{\link{age}},
#' \code{\link{valueAsNumber}}, \code{\link{startDate}}, etc.
#' Accepts integer, numeric, or Date inputs; returns the matching opAttribute type.
#'
#' @param x Comparison bound (integer, numeric, or Date).
#' @return An opAttribute for use inside attribute constructor functions.
#' @seealso \code{\link{lt}}, \code{\link{gt}}, \code{\link{gte}}, \code{\link{eq}}, \code{\link{bt}}
#' @examples
#' # Age 65 or younger
#' conditionOccurrence(cs(1L, name = "test"), age(lte(65L)))
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
#'
#' Builds a comparison operator for use inside attribute functions such as \code{\link{age}},
#' \code{\link{valueAsNumber}}, \code{\link{startDate}}, etc.
#' Accepts integer, numeric, or Date inputs; returns the matching opAttribute type.
#'
#' @param x Comparison bound (integer, numeric, or Date).
#' @return An opAttribute for use inside attribute constructor functions.
#' @seealso \code{\link{lt}}, \code{\link{gt}}, \code{\link{lte}}, \code{\link{eq}}, \code{\link{bt}}
#' @examples
#' # Age 18 or older
#' conditionOccurrence(cs(1L, name = "test"), age(gte(18L)))
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
#'
#' Builds a comparison operator for use inside attribute functions such as \code{\link{age}},
#' \code{\link{valueAsNumber}}, \code{\link{startDate}}, etc.
#' Accepts integer, numeric, Date, or character inputs.
#'
#' @param x Comparison value (integer, numeric, Date, or character).
#' @return An opAttribute for use inside attribute constructor functions.
#' @seealso \code{\link{lt}}, \code{\link{gt}}, \code{\link{lte}}, \code{\link{gte}}, \code{\link{bt}}
#' @examples
#' # Exactly 2 refills
#' drugExposure(cs(1L, name = "drug"), drugRefills(eq(2L)))
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

#' @rdname
#' eq
#' @aliases
#' eq,character-method
setMethod("eq", "character", function(x) {
  methods::new("opAttributeCharacter", op = "eq", value = x)
})

## contains --------
#' String contains operator
#'
#' Builds a text-match operator for use with text filter attribute functions such as
#' \code{\link{lotNumber}}, \code{\link{stopReason}}, \code{\link{uniqueDeviceId}}.
#'
#' @param x Character substring to match.
#' @return An opAttributeCharacter for use inside text attribute functions.
#' @seealso \code{\link{stringStartsWith}}, \code{\link{stringEndsWith}},
#'   \code{\link{stringNotContains}}, \code{\link{lotNumber}}, \code{\link{stopReason}}
#' @examples
#' # Drug exposures where lot number contains "LOT"
#' drugExposure(cs(1L, name = "drug"), lotNumber(stringContains("LOT")))
#' @export
#' @docType methods
setGeneric("stringContains", function(x) standardGeneric("stringContains"))

#' @rdname
#' stringContains
#' @aliases
#' stringContains,character-method
setMethod("stringContains", "character", function(x) {
  methods::new("opAttributeCharacter", op = "contains", value = x)
})

## startsWith --------
#' String starts with operator
#'
#' @param x Character prefix to match.
#' @return An opAttributeCharacter for use inside text attribute functions.
#' @seealso \code{\link{stringContains}}, \code{\link{stringEndsWith}}, \code{\link{lotNumber}}
#' @export
#' @docType methods
setGeneric("stringStartsWith", function(x) standardGeneric("stringStartsWith"))

#' @rdname
#' stringStartsWith
#' @aliases
#' stringStartsWith,character-method
setMethod("stringStartsWith", "character", function(x) {
  methods::new("opAttributeCharacter", op = "startsWith", value = x)
})

## endsWith --------
#' String ends with operator
#'
#' @param x Character suffix to match.
#' @return An opAttributeCharacter for use inside text attribute functions.
#' @seealso \code{\link{stringContains}}, \code{\link{stringStartsWith}}, \code{\link{lotNumber}}
#' @export
#' @docType methods
setGeneric("stringEndsWith", function(x) standardGeneric("stringEndsWith"))

#' @rdname
#' stringEndsWith
#' @aliases
#' stringEndsWith,character-method
setMethod("stringEndsWith", "character", function(x) {
  methods::new("opAttributeCharacter", op = "endsWith", value = x)
})

## notStartsWith --------
#' String does not start with operator
#'
#' @param x Character prefix to exclude.
#' @return An opAttributeCharacter for use inside text attribute functions.
#' @seealso \code{\link{stringNotContains}}, \code{\link{stringNotEndsWith}}
#' @export
#' @docType methods
setGeneric("stringNotStartsWith", function(x) standardGeneric("stringNotStartsWith"))

#' @rdname
#' stringNotStartsWith
#' @aliases
#' stringNotStartsWith,character-method
setMethod("stringNotStartsWith", "character", function(x) {
  methods::new("opAttributeCharacter", op = "!startsWith", value = x)
})

## notContains --------
#' String does not contain operator
#'
#' @param x Character substring to exclude.
#' @return An opAttributeCharacter for use inside text attribute functions.
#' @seealso \code{\link{stringNotStartsWith}}, \code{\link{stringNotEndsWith}}
#' @export
#' @docType methods
setGeneric("stringNotContains", function(x) standardGeneric("stringNotContains"))

#' @rdname
#' stringNotContains
#' @aliases
#' stringNotContains,character-method
setMethod("stringNotContains", "character", function(x) {
  methods::new("opAttributeCharacter", op = "!contains", value = x)
})

## notEndsWith --------
#' String does not end with operator
#'
#' @param x Character suffix to exclude.
#' @return An opAttributeCharacter for use inside text attribute functions.
#' @seealso \code{\link{stringNotStartsWith}}, \code{\link{stringNotContains}}
#' @export
#' @docType methods
setGeneric("stringNotEndsWith", function(x) standardGeneric("stringNotEndsWith"))

#' @rdname
#' stringNotEndsWith
#' @aliases
#' stringNotEndsWith,character-method
setMethod("stringNotEndsWith", "character", function(x) {
  methods::new("opAttributeCharacter", op = "!endsWith", value = x)
})

## bt --------
#' Between operator
#'
#' Builds a range comparison for use inside attribute functions such as \code{\link{age}},
#' \code{\link{valueAsNumber}}, \code{\link{startDate}}, etc.
#' Accepts integer, numeric, or Date inputs.
#'
#' @param x Lower bound (integer, numeric, or Date).
#' @param y Upper bound; must match type of \code{x}.
#' @return An opAttribute for use inside attribute constructor functions.
#' @seealso \code{\link{nbt}}, \code{\link{lt}}, \code{\link{gt}}, \code{\link{age}}
#' @examples
#' # Age between 18 and 65
#' conditionOccurrence(cs(1L, name = "test"), age(bt(18L, 65L)))
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
#'
#' Builds an exclusion range comparison for use inside attribute functions.
#' Accepts integer, numeric, or Date inputs.
#'
#' @param x Lower bound (integer, numeric, or Date).
#' @param y Upper bound; must match type of \code{x}.
#' @return An opAttribute for use inside attribute constructor functions.
#' @seealso \code{\link{bt}}, \code{\link{lt}}, \code{\link{gt}}
#' @examples
#' # Age outside 18-65
#' conditionOccurrence(cs(1L, name = "test"), age(nbt(18L, 65L)))
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

#' Age attribute for a query
#'
#' Filters criteria by the person's age at the event date. Pass any comparison operator
#' (\code{\link{lt}}, \code{\link{gt}}, \code{\link{gte}}, \code{\link{lte}}, \code{\link{eq}},
#' \code{\link{bt}}, \code{\link{nbt}}).
#'
#' @param op An opAttribute built with \code{lt()}, \code{gt()}, \code{bt()}, etc.
#' @return An integer opAttribute with \code{name = "Age"} for use in any domain query.
#' @seealso \code{\link{ageAtStart}}, \code{\link{ageAtEnd}}, \code{\link{lt}}, \code{\link{gt}},
#'   \code{\link{gte}}, \code{\link{bt}}
#' @examples
#' # Age 18 or older at index
#' conditionOccurrence(cs(1L, name = "test"), age(gte(18L)))
#' # Age between 18 and 65
#' conditionOccurrence(cs(1L, name = "test"), age(bt(18L, 65L)))
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

#' Age at era start attribute (for era and period domains)
#' @param op an opAttribute object (numeric or integer) defining the age comparison
#' @return An attribute for use in \code{conditionEra()}, \code{drugEra()}, \code{doseEra()}, \code{observationPeriod()}.
#' @export
ageAtStart <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  methods::new("opAttributeInteger", name = "AgeAtStart", op = op@op,
               value = as.integer(op@value), extent = as.integer(op@extent))
}

#' Age at era end attribute (for era and period domains)
#' @param op an opAttribute object (numeric or integer) defining the age comparison
#' @return An attribute for use in \code{conditionEra()}, \code{drugEra()}, \code{doseEra()}, \code{observationPeriod()}.
#' @export
ageAtEnd <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  methods::new("opAttributeInteger", name = "AgeAtEnd", op = op@op,
               value = as.integer(op@value), extent = as.integer(op@extent))
}

#' Visit length attribute for VisitOccurrence
#' @param op an opAttribute object (numeric or integer) defining the length comparison in days
#' @return An attribute for use in \code{visit()}.
#' @export
visitLength <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  methods::new("opAttributeInteger", name = "VisitLength", op = op@op,
               value = as.integer(op@value), extent = as.integer(op@extent))
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

#' Quantity attribute — ProcedureOccurrence, DeviceExposure, Specimen
#' @param op an opAttribute object (numeric or integer) defining the quantity comparison.
#' @return An attribute for use in \code{procedure()}, \code{deviceExposure()}, or \code{specimen()}.
#' @export
quantityValue <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  methods::new("opAttributeNumeric", name = "Quantity", op = op@op, value = op@value, extent = op@extent)
}

#' Occurrence count attribute for ConditionEra
#'
#' Filter condition era criteria by the number of condition eras (e.g. occurrence count = 0
#' for no matching eras). Used only in a condition era query.
#' @param op   an opAttribute object (integer) that defines the logical operation and value
#'             (e.g. \code{eq(0L)} for "count equals 0")
#' @return An attribute for use in \code{\link{conditionEra}()}
#' @export
occurrenceCount <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeInteger",
               name = "OccurrenceCount",
               op = op@op,
               value = as.integer(op@value),
               extent = as.integer(op@extent))
}

#' Era length attribute for DrugEra
#'
#' Filter drug era criteria by era length in days (e.g. \code{gt(14L)} for era longer than 14 days).
#' Used only in a drug era query.
#' @param op   an opAttribute object (integer) that defines the logical operation and value
#' @return An attribute for use in \code{\link{drugEra}()}
#' @export
eraLength <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeInteger",
               name = "EraLength",
               op = op@op,
               value = as.integer(op@value),
               extent = as.integer(op@extent))
}

#' Period length attribute — ObservationPeriod, PayerPlanPeriod
#' @param op an opAttribute object (integer or numeric) defining the length comparison in days.
#' @return An attribute for use in \code{observationPeriod()} or \code{payerPlanPeriod()}.
#' @export
periodLength <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  methods::new("opAttributeInteger", name = "PeriodLength", op = op@op,
               value = as.integer(op@value), extent = as.integer(op@extent))
}

#' Dose value attribute for DoseEra
#'
#' Filter dose era criteria by dose value (e.g. \code{gt(92)} for dose > 92).
#' Used only in a dose era query.
#' @param op   an opAttribute object (numeric) that defines the logical operation and value
#' @return An attribute for use in \code{\link{doseEra}()}
#' @export
doseValue <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeNumeric",
               name = "DoseValue",
               op = op@op,
               value = op@value,
               extent = op@extent)
}

#' Range high ratio attribute for Measurement
#'
#' Filter measurement criteria by the ratio value_as_number / range_high (e.g. \code{gt(2)} for ratio > 2).
#' Used only in a measurement query.
#' @param op   an opAttribute object (numeric) that defines the logical operation and value
#' @return An attribute for use in \code{\link{measurement}()}
#' @export
rangeHighRatio <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeNumeric or opAttributeInteger.")
  }
  methods::new("opAttributeNumeric",
               name = "RangeHighRatio",
               op = op@op,
               value = as.numeric(op@value),
               extent = op@extent)
}

## Character Constructors ----

#' Lot number attribute for DrugExposure
#'
#' Filter drug exposure criteria by lot number (e.g. \code{contains("LOT123")} for lot numbers containing "LOT123").
#' Used only in a drug exposure query.
#' @param op   an opAttribute object (character) that defines the logical operation and value
#'             (e.g. \code{contains("LOT")} for lot numbers containing "LOT")
#' @return An attribute for use in \code{\link{drugExposure}()}
#' @export
lotNumber <- function(op) {
  check <- all(grepl("opAttribute", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeCharacter.")
  }
  if (!methods::is(op, "opAttributeCharacter")) {
    stop("Input must be an opAttributeCharacter.")
  }
  methods::new("opAttributeCharacter",
               name = "LotNumber",
               op = op@op,
               value = op@value,
               extent = op@extent)
}

#' Filter by stop reason text — ConditionOccurrence or DrugExposure
#' @param op an \code{opAttributeCharacter} built with e.g. \code{stringContains("reason")}.
#' @return An attribute for use in \code{conditionOccurrence()} or \code{drugExposure()}.
#' @export
stopReason <- function(op) {
  if (!methods::is(op, "opAttributeCharacter")) stop("Input must be an opAttributeCharacter.")
  methods::new("opAttributeCharacter", name = "StopReason", op = op@op, value = op@value, extent = op@extent)
}

#' Filter by unique device ID text — DeviceExposure
#' @param op an \code{opAttributeCharacter} built with e.g. \code{stringContains("DV")}.
#' @return An attribute for use in \code{deviceExposure()}.
#' @export
uniqueDeviceId <- function(op) {
  if (!methods::is(op, "opAttributeCharacter")) stop("Input must be an opAttributeCharacter.")
  methods::new("opAttributeCharacter", name = "UniqueDeviceId", op = op@op, value = op@value, extent = op@extent)
}

#' Filter by specimen source ID text — Specimen
#' @param op an \code{opAttributeCharacter} built with e.g. \code{stringContains("src")}.
#' @return An attribute for use in \code{specimen()}.
#' @export
specimenSourceId <- function(op) {
  if (!methods::is(op, "opAttributeCharacter")) stop("Input must be an opAttributeCharacter.")
  methods::new("opAttributeCharacter", name = "SourceId", op = op@op, value = op@value, extent = op@extent)
}

## Date Constructors ----

#' Function that creates a start date attribute
#' @param op   an opAttribute object must be a date that defines the logical operation used to
#'             determine eligible start dates
#' @param type specify the type of date to use either occurrence or era. default as occurrence
#' @return An attribute that can be used in a cohort definition
#' @export
startDate <- function(op, type = "occurrence") {

  type <- match.arg(type, choices = c("occurrence", "era"))

  check <- all(grepl("opAttribute(Date|Super)", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeDate.")
  }

  if (type == "occurrence") {
    sd <- methods::new("opAttributeDate",
                 name = "OccurrenceStartDate",
                 op = op@op,
                 value = op@value,
                 extent = op@extent)
  }

  if (type == "era") {
    sd <- methods::new("opAttributeDate",
                 name = "EraStartDate",
                 op = op@op,
                 value = op@value,
                 extent = op@extent)
  }
  return(sd)

}

#' Function that creates a end date attribute
#' @param op   an opAttribute object must be a date that defines the logical operation used to
#'             determine eligible end dates
#' @param type specify the type of date to use either occurrence or era. default as occurrence
#' @return An attribute that can be used in a cohort definition
#' @export
endDate <- function(op, type = "occurrence") {

  type <- match.arg(type, choices = c("occurrence", "era"))

  check <- all(grepl("opAttribute(Date|Super)", methods::is(op)))
  if (!check) {
    stop("Input must be an opAttributeDate.")
  }

  if (type == "occurrence") {
    ed <- methods::new("opAttributeDate",
                       name = "OccurrenceEndDate",
                       op = op@op,
                       value = op@value,
                       extent = op@extent)
  }

  if (type == "era") {
    ed <- methods::new("opAttributeDate",
                       name = "EraEndDate",
                       op = op@op,
                       value = op@value,
                       extent = op@extent)
  }
  return(ed)

}

# Coercion ------------
#' @importFrom rlang :=
listOpAttribute <- function(x) {
  atr <- list(Op = x@op, Value = x@value, Extent = x@extent) |>
    purrr::discard(is.na)

  tibble::lst(`:=`(!!x@name, atr))
}

## Coerce Numeric ----
setMethod("as.list", "opAttributeSuper", listOpAttribute)

# For character attributes (e.g., LotNumber), use "Text" instead of "Value" (Atlas convention)
listOpAttributeCharacter <- function(x) {
  atr <- list(Text = x@value, Op = x@op) |>
    purrr::discard(is.na)

  tibble::lst(`:=`(!!x@name, atr))
}

setMethod("as.list", "opAttributeCharacter", listOpAttributeCharacter)
