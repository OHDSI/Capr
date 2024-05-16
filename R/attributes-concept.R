# Concept Attribute class ----------------------------

#' An S4 class for a concept attribute
#' @slot
#' name the name of the attribute
#' @slot
#' conceptSet a list representing the concepts for the attribute
#' @include conceptSet.R
setClass("conceptAttribute",
         slots = c(name = "character",
                   conceptSet = "list"  # TODO why is this a list and not a concept set object?
),
  prototype = list(name = NA_character_, conceptSet = list()))

setValidity("conceptAttribute", function(object) {
  # TODO check that each object in conceptSet list is of Concept Class
  stopifnot(is.list(object@conceptSet), is.character(object@name), length(object@name) == 1)
  TRUE
})

# Console Print ---------------

setMethod("show", "conceptAttribute", function(object) {

  tbl <- tibble::tibble(concept_id = purrr::map_int(object@conceptSet, ~as.integer(.x@concept_id)),
    concept_name = purrr::map_chr(object@conceptSet,
                                  ~as.character(.x@concept_name)), concept_code = purrr::map_chr(object@conceptSet,
      ~as.character(.x@concept_code)), domain_id = purrr::map_chr(object@conceptSet,
                                                                  ~as.character(.x@domain_id)),
    vocabulary_id = purrr::map_chr(object@conceptSet,
                                   ~as.character(.x@vocabulary_id)), concept_class_id = purrr::map_chr(object@conceptSet,
      ~as.character(.x@concept_class_id)))
  cli::cat_bullet(paste("Capr Concept Attribute:", object@name), bullet = "sup_plus")
  print(tbl)
})

# Constructors -------------

#' Add male attribute to a query
#'
#' @return
#' An attribute that can be used in a query function
#' @export
#'
#' @describeIn
#' attributes male demographic attribute
#'
#' @examples
#' # Create a cohort of males with Type 1 diabetes
#' t1dm <- cs(descendants(201254, 435216, 40484648), name = "type 1 diabetes")
#' t1dm_males <- cohort(conditionOccurrence(t1dm, male()))
male <- function() {

  methods::new("conceptAttribute",
               name = "Gender",
               conceptSet = list(methods::new("Concept", concept_id = 8507L,
    concept_name = "MALE", concept_code = "M", domain_id = "Gender", vocabulary_id = "Gender", concept_class_id = "Gender")))
}

#' Add female attribute to a query
#'
#' @return
#' An attribute that can be used in a query function
#' @export
#'
#' @describeIn
#' attributes female demographic attribute
#' @examples
#' # Create a cohort of males with Type 1 diabetes
#' t1dm <- cs(descendants(201254, 435216, 40484648), name = "type 1 diabetes")
#' t1dm_females <- cohort(conditionOccurrence(t1dm, female()))
female <- function() {

  methods::new("conceptAttribute",
               name = "Gender",
               conceptSet = list(methods::new("Concept", concept_id = 8532L,
    concept_name = "FEMALE", concept_code = "F", domain_id = "Gender", vocabulary_id = "Gender",
    concept_class_id = "Gender")))
}


findConceptInVocabulary <- function(id, connection, vocabularyDatabaseSchema) {

  detailedConceptSet <- cs(id, name = glue::glue("{id}")) |>
    getConceptSetDetails(con = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  return(detailedConceptSet)
}

pullConceptClass <- function(detailedConceptSet) {
  conceptClass <- detailedConceptSet@Expression[[1]]@Concept
  return(conceptClass)
}

buildConceptAttribute <- function(ids, attributeName, connection, vocabularyDatabaseSchema) {

  # get concepts from vocabulary table
  conceptsForAttributes <- purrr::map(
    ids,
    ~findConceptInVocabulary(id = .x, connection = connection, vocabularyDatabaseSchema = vocabularyDatabaseSchema) |>
      pullConceptClass()
  )

  attr_concept <- methods::new("conceptAttribute",
                               name = attributeName,
                               conceptSet = conceptsForAttributes)
  return(attr_concept)
}

#' Add a drug type attribute to determine the provenance of the record
#' @param ids the concept ids for the attribute
#' @param connection a connection to an OMOP dbms to get vocab info about the concept
#' @param vocabularyDatabaseSchema the database schema for the vocabularies
#' @return
#' An attribute that can be used in a query function
#' @export
#'
drugType <- function(ids, connection, vocabularyDatabaseSchema) {
  res <- buildConceptAttribute(ids = ids, attributeName = "DrugType",
                        connection = connection,
                        vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  return(res)
}

#' Add a condition type attribute to determine the provenance of the record
#' @param ids the concept ids for the attribute
#' @param connection a connection to an OMOP dbms to get vocab info about the concept
#' @param vocabularyDatabaseSchema the database schema for the vocabularies
#' @return
#' An attribute that can be used in a query function
#' @export
#'
conditionType <- function(ids, connection, vocabularyDatabaseSchema) {
  res <- buildConceptAttribute(ids = ids, attributeName = "ConditionType",
                               connection = connection,
                               vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  return(res)
}



#' Add a visit type attribute to determine the provenance of the record
#' @param ids the concept ids for the attribute
#' @param connection a connection to an OMOP dbms to get vocab info about the concept
#' @param vocabularyDatabaseSchema the database schema for the vocabularies
#' @return
#' An attribute that can be used in a query function
#' @export
#'
visitType <- function(ids, connection, vocabularyDatabaseSchema) {
  res <- buildConceptAttribute(ids = ids, attributeName = "VisitType",
                               connection = connection,
                               vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  return(res)
}


#' Add a measurement type attribute to determine the provenance of the record
#' @param ids the concept ids for the attribute
#' @param connection a connection to an OMOP dbms to get vocab info about the concept
#' @param vocabularyDatabaseSchema the database schema for the vocabularies
#' @return
#' An attribute that can be used in a query function
#' @export
#'
measurementType <- function(ids, connection, vocabularyDatabaseSchema) {
  res <- buildConceptAttribute(ids = ids, attributeName = "measurementType",
                               connection = connection,
                               vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  return(res)
}

#' Add a observation type attribute to determine the provenance of the record
#' @param ids the concept ids for the attribute
#' @param connection a connection to an OMOP dbms to get vocab info about the concept
#' @param vocabularyDatabaseSchema the database schema for the vocabularies
#' @return
#' An attribute that can be used in a query function
#' @export
#'
observationType <- function(ids, connection, vocabularyDatabaseSchema) {
  res <- buildConceptAttribute(ids = ids, attributeName = "observationType",
                               connection = connection,
                               vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  return(res)
}


#' Add a procedure type attribute to determine the provenance of the record
#' @param ids the concept ids for the attribute
#' @param connection a connection to an OMOP dbms to get vocab info about the concept
#' @param vocabularyDatabaseSchema the database schema for the vocabularies
#' @return
#' An attribute that can be used in a query function
#' @export
#'
procedureType <- function(ids, connection, vocabularyDatabaseSchema) {
  res <- buildConceptAttribute(ids = ids, attributeName = "procedureType",
                               connection = connection,
                               vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  return(res)
}

#' Add unit attribute to a query
#' @param x   A single character idetifier for a unit or a concept set that identifies units
#' @return
#' An attribute that can be used in a query function
#' @export
#'
#'
#' @examples
#' # create a unit attribute
#' unit(8713L)
#' unit("%")
unit <- function(x) {
  if (missing(x)) {
    rlang::abort("Unit must be specified")
  }

  stopifnot(is.character(x) || is.numeric(x) || methods::is(x, "ConceptSet"))

  if (is.character(x)) {
    stopifnot(length(x) == 1)
    conceptId <- switch(x,
                        `%` = 8554,
                        percent = 8554,
                        `mmol/mol` = 9579,
                        `millimole per mole` = 9579,

      rlang::abort(paste(x, "is not a recogized unit identifier")))

    conceptSet <- list(methods::new("Concept",
                                    concept_id = as.integer(conceptId),
                                    concept_name = x))
  } else if (is.numeric(x)) {
    conceptSet <- purrr::map(x, ~methods::new("Concept", concept_id = as.integer(.x)))
  } else if (methods::is(x, "ConceptSet")) {
    x <- as.data.frame(cs(1:3))$conceptId
    conceptSet <- purrr::map(x, ~methods::new("Concept", concept_id = as.integer(.x)))
  } else {
    rlang::abort("unit only accepts concept sets, integers, or character unit ids")
  }

  # conceptSet <- as.list(as.data.frame(conceptSet)$conceptId) conceptSet <- as.list(conceptSet)

  res <- methods::new("conceptAttribute", name = "Unit", conceptSet = conceptSet)
  return(res)
}

# Coercion ------------------

setMethod("as.list", "conceptAttribute", function(x) {

  concepts <- purrr::map(x@conceptSet, ~as.list(.x))
  nm <- x@name

  tibble::lst(`:=`(!!nm, concepts))
})

# Capr Call -----------------
