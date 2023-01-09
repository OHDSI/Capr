# test as.list and collectConcepts

#testing script
source("R/conceptSet.R")
source("R/query.R")
source("R/attributes-concept.R")
source("R/attributes-op.R")
source("R/window.R")
source("R/criteria.R")
source("R/exit.R")
source("R/cohort.R")
source("R/in_development/collectCodesetId.R")
library(tidyverse)

cd <- cohort(
  entry = entry(
    condition(cs(descendants(201826L)), male()),
    observationWindow = continuousObservation(365, 0)
  ),
  attrition = attrition(
    'no t1d' = withAll(
      criteria(
        exactly(0),
        condition(cs(descendants(201254L))),
        duringInterval(eventStarts(-Inf, -1))
      )
    ),
    'abnormal hba1c' = withAll(
      criteria(
        atLeast(1),
        measurement(
          cs(descendants(4184637L)),
          valueAsNumber(lt(13)),
          unit(8713L)),
        duringInterval(eventStarts(-Inf, -1))
      )
    )
  )
)

#need simple cohort for synpuf
cd <- cohort(
  entry = entry(
    condition(cs(descendants(201826L), name = "t2d"), male()),
    observationWindow = continuousObservation(365, 0)
  )
)

jj <- toCirce(cd)
jsonlite::toJSON(jj, pretty = TRUE, auto_unbox = TRUE)


json <- RJSONIO::toJSON(jj)
sql <- CirceR::cohortExpressionFromJson(json) %>%
  CirceR::buildCohortQuery(options = CirceR::createGenerateOptions(generateStats = TRUE))

cohortsToCreate <- tibble::tibble(
  cohortId = 999,
  cohortName = "CaprTest",
  sql = sql
)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = keyring::key_get("cdm_server"),
  user = keyring::key_get("cdm_user"),
  password = keyring::key_get("cdm_password"),
  port = "5441"
)
#set database schemas
vocabularyDatabaseSchema <- "cdm_531"
cdmDatabaseSchema <- "cdm_531"
resultsDatabaseSchema <- "martin_lavallee_results"

name <- "BOOEX"
cohortTableNames <- list(cohortTable = name,
                         cohortInclusionTable = paste0(name, "_inclusion"),
                         cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                         cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                         cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                         cohortCensorStatsTable = paste0(name, "_censor_stats"))

CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                  cdmDatabaseSchema = cdmDatabaseSchema ,
                                  cohortDatabaseSchema = resultsDatabaseSchema,
                                  cohortTableNames = cohortTableNames,
                                  cohortDefinitionSet = cohortsToCreate,
                                  incremental = FALSE)
CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                 cohortDatabaseSchema = resultsDatabaseSchema,
                                 cohortTable = "BOOEX",
                                 cohortIds = c(999),
                                 cohortDefinitionSet = cohortsToCreate)
