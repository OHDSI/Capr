setClass("Concept",
         slot = c(
           conceptId = "integer",
           conceptName = "character",
           standardConcept = "character",
           standardConceptCaption = "character",
           invalidReason = "character",
           invalidReasonCaption = "character",
           conceptCode = "character",
           domainId = "character",
           vocabularyId = "character",
           conceptClassId = "character",
           includeDescendants = "logical",
           includeMapped = "logical",
           isExcluded = "logical"
         ),
         prototype = list(
           conceptId = NA_integer_,
           conceptName = NA_character_,
           standardConcept = NA_character_,
           standardConceptCaption = NA_character_,
           invalidReason = NA_character_,
           invalidReasonCaption = NA_character_,
           conceptCode = NA_character_,
           domainId = NA_character_,
           vocabularyId = NA_character_,
           conceptClassId = NA_character_,
           includeDescendants = FALSE,
           includeMapped = FALSE,
           isExcluded = FALSE
         )
)



setClass("ConceptSet",
         slot = c(
           id = "character",
           name = "character",
           conceptSet = "list"
         ),
         prototype = list(
           id = NA_character_,
           name = NA_character_,
           conceptSet = list()
         )
)


setClass("Query",
         slot = c(
           domain = "character",
           conceptSet = "ConceptSet",
           attributes = "list"
         ),
         prototype = list(
           domain = NA_character_,
           conceptSet = new("ConceptSet"),
           attributes = list()
         )
)
