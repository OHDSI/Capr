## ----knitr, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# knitr::purl(
#   input = './R/cohortCapr_md.Rmd',
#   output = './R/cohortCapr.R'
# )


## ----Get project configurations-------------------------------------------------------------------------
connectionConfig <- config::get(config = 'config', file = './config/connection_config.yml')
isStandardConfig <- config::get(config = 'config', file = './config/is_standard_config.yml')


## ----connect to database, eval=TRUE, include=TRUE-------------------------------------------------------
#  Use connection details from configuration
connectionDetails <- createConnectionDetails(
  dbms = connectionConfig$dbms,
  user = connectionConfig$user,
  password = connectionConfig$password,
  server = connectionConfig$server,
  port = connectionConfig$port,
  oracleDriver = connectionConfig$oracleDriver,
  pathToDriver = connectionConfig$pathToDriver
)


## ----concept sets, echo=TRUE----------------------------------------------------------------------------
## Concept sets
source("./R/conceptSets.R")

# Establish connection
con <- connect(connectionDetails)

conceptSets$conceptSets <- conceptSets$conceptSets %>%
  # Add details for all concepts (excl. descendants)
  lapply(FUN = getConceptSetDetails,
         con = con,
         vocabularyDatabaseSchema = connectionConfig$vocabulary_schema)

# Disconnect
disconnect(con)


## ----count occurences, echo=TRUE------------------------------------------------------------------------
## Count occurrences of each concept in data

# Establish connection
con <- connect(connectionDetails)

# Get countOccurrences function
source("./R/countOccurrences.R")

# Get links between tables and fields as input
source("./R/table_linked_to_concept_field.R")

# count occurrences of each concept 'x' and print results; cardiac complications as example
# cardiacComplicationsCounts <- 
#   countOccurrences(
#     conceptSets$concepts$cardiacComplications, c("condition_occurrence", "procedure_occurrence"), links, con, connectionConfig$cdm_schema
#   ) %>% print()
# labTestsCounts <- 
#   countOccurrences(
#     conceptSets$concepts$labTests, c("measurement"), links, con, connectionConfig$cdm_schema
#   ) %>% print()

# Disconnect
disconnect(con)


## ----Standard non-standard check------------------------------------------------------------------------
source('./R/isStandard.R')

# Connect to DB
con <- connect(connectionDetails)

# Return table of non-standard concepts
nonStandard <- isStandard(
  db_connection = con,
  data_concepts_path = isStandardConfig$concepts_path,
  # (optional) Save the results (with standard and non-standard concepts)
  # save_path = isStandardConfig$save_path
  )

# Disconnect
disconnect(con)

# Print all non-standard concepts
nonStandard


## ----Cohort definition----------------------------------------------------------------------------------
## Cohort definition
# Create cohort definition
ch <- cohort(
  entry = entry(
    # enter patients < 80 years old who have had a lab test
    measurement(conceptSets$conceptSets$labTests, age(lt(80))),
    observationWindow = continuousObservation(0, 0),
    primaryCriteriaLimit = "All"
  ),
  attrition = attrition(
    withAny(
      # include all cardiac complications
      atLeast(
        x = 1,
        # include all cardiac complications represented by condition concepts
        query = conditionOccurrence(conceptSets$conceptSets$cardiacComplications)
      ),
      atLeast(
        x = 1,
        # include all cardiac complications represented by procedure concepts
        query = procedure(conceptSets$conceptSets$cardiacComplications)
      )
    ),
    withAny(
      atLeast(
        x = 1,
        # include all cardiac surgeries
        query = procedure(conceptSets$conceptSets$cardiacProcedures)
      )
    )
  ),
  exit = exit(
    endStrategy = observationExit(),
    censor = censoringEvents(
      # exit if intubation has taken place
      procedure(conceptSets$conceptSets$intubation)
    )
  )
)


## ----json and sql---------------------------------------------------------------------------------------
## Cohort json and sql
# Generate json for cohort
chJson <- ch %>%
  toCirce() %>%
  jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE) %>%
  as.character()

# Generate cohort sql query
sql <- CirceR::buildCohortQuery(
  expression = CirceR::cohortExpressionFromJson(chJson),
  options = CirceR::createGenerateOptions(generateStats = FALSE)
)


## ----Save cohort and concept set json-------------------------------------------------------------------
write(chJson, "./json/cohort.json")
for (cs in names(conceptSets$conceptSets)) {
  writeConceptSet(
    x = conceptSets$conceptSets[[cs]],
    path = paste("./json/", cs, "_cs.json", sep="")
  )
}


## ----Create and generate cohorts------------------------------------------------------------------------
# Establish connection
con <- connect(connectionDetails)

# Cohorts to create
cohortsToCreate <- tibble::tibble(
  cohortId = 9876,
  cohortName = "cohort",
  sql = sql
)

# Cohort tables
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "cohort")
CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "cohort",
  cohortTableNames = cohortTableNames,
)

# Generate the cohorts
cohortsGenerated <- CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "cdm",
  cohortDatabaseSchema = "cohort",
  cohortTableNames = cohortTableNames,
  cohortDefinitionSet = cohortsToCreate
)

# Get cohort counts
cohortCounts <- CohortGenerator::getCohortCounts(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = "cohort",
  cohortTable = cohortTableNames$cohortTable
)

# Disconnect
disconnect(con)


cohortCounts


## ----retrieve updated DB--------------------------------------------------------------------------------
# Establish connection
con <- connect(connectionDetails)

# Count unique person_id in the person table
query_person <- 
  paste0("SELECT COUNT(DISTINCT person_id) AS num_persons FROM ", connectionConfig$cdm_schema, ".person")
result_person <- dbGetQuery(con, query_person)$num_persons

# Count unique subject_id in the cardiac_arrest table
query_cohort <-   
  paste0("SELECT COUNT(DISTINCT subject_id) AS num_persons FROM ", connectionConfig$cohort_schema, ".cohort")

result_cohort <- dbGetQuery(con, query_cohort)$num_persons

# Print results
cat("Number of persons in dataset: ", result_person, "\n")
cat("Number of persons in cohort: ", result_cohort, "\n")


# Disconnect
disconnect(con)

