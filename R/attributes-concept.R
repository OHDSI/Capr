# attribute scripts

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
            conceptId = 8507L,
            conceptName = "MALE",
            conceptCode = "M",
            domainId = "Gender",
            vocabularyId = "Gender",
            conceptClassId = "Gender")
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
            conceptId = 8532L,
            conceptName = "FEMALE",
            conceptCode = "F",
            domainId = "Gender",
            vocabularyId = "Gender",
            conceptClassId = "Gender")
      )
  )
}



unit <- function(...) {
  concept_set <- purrr::map(..., ~new("Concept", conceptId = as.integer(.x)))
  new("conceptAttribute",
      name = "unit",
      conceptSet = concept_set)
}
