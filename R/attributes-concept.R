#attribute scripts

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
