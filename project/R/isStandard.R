isStandard <- function(conceptSets, df) {
  concepts <- c()
  conceptSet <- c()
  standardness <- c()
  og <- df$concept$concept_id
  for (cs in names(conceptSets)) {
    for (L in conceptSets[[cs]]@Expression) {
      conceptSet <- append(conceptSet, cs)
      concept <- L@Concept@concept_id
      concepts <- append(concepts, concept)
      standardness <- append(standardness, L@Concept@standard_concept %>%
        gsub(pattern = 'S', replacement = 'Standard') %>%
        gsub(pattern = 'C', replacement = 'Classification')
      )
      if (standardness[length(standardness)] == "") {
        if (concept %in% og) {
          standardness[length(standardness)] <- "Non-Standard"
        } else {
          standardness[length(standardness)] <- "not found in data"
        }
      }
    }
  }
  res <- tibble::tibble(
    conceptSet = conceptSet,
    concept = concepts,
    standard = standardness
  )
  
  return(res)
}