# Helpers ---------------------------
setClass("ObservationWindow",
         slots = c(priorDays = "integer",
                   postDays = "integer"),
         prototype = list(
           priorDays = 0L,
           postDays = 0L
         )
)

setClass("Occurrence",
         slots = c(
           type = "character",
           count = "integer"
         ),
         prototype = list(
           type = NA_character_,
           count = NA_integer_
         )
)

# Concept Classes ---------------------------------
#
# setClass("Concept",
#          slot = c(
#            conceptId = "integer",
#            conceptName = "character",
#            standardConcept = "character",
#            standardConceptCaption = "character",
#            invalidReason = "character",
#            invalidReasonCaption = "character",
#            conceptCode = "character",
#            domainId = "character",
#            vocabularyId = "character",
#            conceptClassId = "character",
#            includeDescendants = "logical",
#            includeMapped = "logical",
#            isExcluded = "logical"
#          ),
#          prototype = list(
#            conceptId = NA_integer_,
#            conceptName = NA_character_,
#            standardConcept = NA_character_,
#            standardConceptCaption = NA_character_,
#            invalidReason = NA_character_,
#            invalidReasonCaption = NA_character_,
#            conceptCode = NA_character_,
#            domainId = NA_character_,
#            vocabularyId = NA_character_,
#            conceptClassId = NA_character_,
#            includeDescendants = FALSE,
#            includeMapped = FALSE,
#            isExcluded = FALSE
#          )
# )



# setClass("ConceptSet",
#          slot = c(
#            id = "character",
#            name = "character",
#            conceptSet = "list"
#          ),
#          prototype = list(
#            id = NA_character_,
#            name = NA_character_,
#            conceptSet = list()
#          )
# )

# Circe Classes --------------------------------------


# setClass("Count",
#          slots = c(
#            query = 'Query',
#            window = 'Window',
#            occurrence = 'Occurrence'),
#          prototype = list(
#            query = new("Query"),
#            window = new("Window"),
#            occurrence = new("Occurrence")
#          )
# )


setClass("Group",
         slots = c(
           occurrence = 'Occurrence',
           criteria = 'list',
           group = 'list'),
         prototype = list(
           occurrence = new("Occurrence"),
           criteria = list(),
           group = list()
         )
)

# Cohort Classes -------------------------------

setClass("CohortEntry",
         slot = c(
           entryEvents = "list",
           observationWindow = "ObservationWindow",
           primaryCriteriaLimit = "character",
           additionalCriteria = "Group",
           qualifiedLimit = "character"
         ),
         prototype = list(
           entryEvents = list(),
           observationWindow = new("ObservationWindow"),
           primaryCriteriaLimit = "First",
           additionalCriteria = new("Group"),
           qualifiedLimit = "First"
         )
)

setClass("CohortExit",
         slot = c(
           endStrategy = "list",
           censor = "list"
         ),
         prototype = list(
           endStrategy = list('type' = "end of continuous observation"),
           censor = list()
         )
)


setClass("CohortEra",
         slots = c(
           era_pad = "integer",
           start_date = "Date",
           end_date = "Date"
         ),
         prototype = list(
           era_pad = 0L,
           start_date = lubridate::date("1970-01-01"),
           end_date = lubridate::date("2099-12-31")
         ))

setClass("Cohort",
         slot = c(
           entry = "CohortEntry",
           ir = "list",
           exit = "CohortExit",
           era = "CohortEra"
         ),
         prototype = list(
           entry = new("CohortEntry"),
           ir = list(),
           exit = new("CohortExit"),
           era = new("CohortEra")
         )
)


# Attribute Classes -----------------------------

setClass("conceptAttribute",
         slot = c(
           name = "character",
           conceptSet = "list"
         ),
         prototype = list(
           name = NA_character_,
           conceptSet = list()
         )
)

setClass("opAttribute",
         slot = c(
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
         )
)


