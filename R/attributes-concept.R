# Concept Attribute class ----------------------------

#' An S4 class for a concept attribute
#' @slot name the name of the attribute
#' @slot conceptSet a list representing the concepts for the attribute
setClass("conceptAttribute",
         slots = c(
           name = "character",
           conceptSet = "list"
         ),
         prototype = list(
           name = NA_character_,
           conceptSet = list()
         )
)

setValidity("conceptAttribute", function(object){
  #TODO check that each object in conceptSet list is of Concept Class
  stopifnot(is.list(object@conceptSet),
            is.character(object@name),
            length(object@name) == 1)
  TRUE
})

# Console Print ---------------

#' @rdname show-method
#' @aliases show,conceptAttribute-method
setMethod("show", "conceptAttribute", function(object) {

  tbl <- tibble::tibble(
    concept_id = purrr::map_int(object@conceptSet, ~.x@concept_id),
    concept_name = purrr::map_chr(object@conceptSet, ~.x@concept_name),
    concept_code = purrr::map_chr(object@conceptSet, ~.x@concept_code),
    domain_id = purrr::map_chr(object@conceptSet, ~.x@domain_id),
    vocabulary_id = purrr::map_chr(object@conceptSet, ~.x@vocabulary_id),
    concept_class_id = purrr::map_chr(object@conceptSet, ~.x@concept_class_id)
  )
  cli::cat_rule(paste("<Capr Concept Attribute>", object@name))
  print(tbl)
})

# Constructors -------------

#' Add male attribute to a query
#'
#' @return An attribute that can be used in a query function
#' @export
#'
#' @describeIn attributes
#'
#' @examples
#' \dontrun{
#' # Create a cohort of males with Type 1 diabetes
#' t1dm <- cs(descendants(201254,435216,40484648))
#' t1dm_males <- cohort(condition(t1dm, male()))
#' }
male <- function() {

  new("conceptAttribute",
      name = "male",
      conceptSet = list(
        new("Concept",
            concept_id = 8507L,
            concept_name = "MALE",
            concept_code = "M",
            domain_id = "Gender",
            vocabulary_id = "Gender",
            concept_class_id = "Gender")
      )
  )
}

#' Add female attribute to a query
#'
#' @return An attribute that can be used in a query function
#' @export
#'
#' @describeIn attributes
#' @examples
#' \dontrun{
#' # Create a cohort of males with Type 1 diabetes
#' t1dm <- cs(descendants(201254,435216,40484648))
#' t1dm_females <- cohort(condition(t1dm, female()))
#' }
female <- function() {

  new("conceptAttribute",
      name = "female",
      conceptSet = list(
        new("Concept",
            concept_id = 8532L,
            concept_name = "FEMALE",
            concept_code = "F",
            domain_id = "Gender",
            vocabulary_id = "Gender",
            concept_class_id = "Gender")
      )
  )
}



unit <- function(...) {
  concept_set <- purrr::map(..., ~new("Concept", concept_id = as.integer(.x)))
  new("conceptAttribute",
      name = "unit",
      conceptSet = concept_set)
}

# Type Coercion ------------------

# Capr Call -----------------
