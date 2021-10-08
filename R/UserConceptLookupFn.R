# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of Capr
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#



#' get concept id details
#'
#' For one or more concept id, get concept id details by querying the
#' OMOP vocabulary in the database.
#'
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @template     OracleTempSchema
#' @param        conceptIds a vector of concept ids
#' @param        mapToStandard logic to map to standard OMOP concept
#' @return       a tibble data frame object with conceptId, conceptName, standardConcept,
#'               standardConceptCaption, invalidReason, invalidReasonCaption, conceptCode,
#'               domainId, vocabularyId, conceptClassId.
#'
#' @export

getConceptIdDetails <- function(conceptIds,
                                connectionDetails = NULL,
                                connection = NULL,
                                vocabularyDatabaseSchema = NULL,
                                oracleTempSchema = NULL,
                                mapToStandard = TRUE) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(conceptIds, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # Set up connection to server ----------------------------------------------------
  if (is.null(connection)) {
    if (!is.null(connectionDetails)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    } else {
      stop("No connection or connectionDetails provided.")
    }
  }

  conceptQuery <- "SELECT * FROM @vocabularyDatabaseSchema.concept WHERE concept_id IN (@conceptId);"
  conceptDetails <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                               sql = conceptQuery,
                                                               snakeCaseToCamelCase = TRUE,
                                                               vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                                               oracleTempSchema = oracleTempSchema,
                                                               conceptId = conceptIds) %>%
    dplyr::tibble()

  #if mapping to the standard concept join to concept_relationship----------------------------------
  if (mapToStandard){
    #conceptIdToMap  <- conceptDetails$.$conceptId bug fix from dbConnector
    conceptIdToMap  <- conceptDetails$conceptId
    mappingQuery <- "SELECT b.* FROM @vocabularyDatabaseSchema.concept_relationship a
    JOIN  @vocabularyDatabaseSchema.concept b on b.concept_id = a.concept_id_2 AND a.relationship_id = '@relationship'
    WHERE a.concept_id_1 in (@conceptId);"
    conceptDetails <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                                 sql = mappingQuery,
                                                                 snakeCaseToCamelCase = TRUE,
                                                                 vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                                                 oracleTempSchema = oracleTempSchema,
                                                                 conceptId = conceptIdToMap,
                                                                 relationship = "Maps to") %>%
      dplyr::tibble()
  }
  return(conceptDetails)
}


#' Lookup Concepts by OMOP Concept Code using Vocabulary
#'
#' This function looks up concepts using the OMOP concept code and vocabulary. Function requires a dbms connection to use
#'
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @template     OracleTempSchema
#' @param        conceptCode a character vector of concept codes
#' @param        vocabulary a single character string with the vocabulary of the codes
#' @param        mapToStandard logic to map to standard OMOP concept
#' @return       a tibble data frame object with conceptId, conceptName, standardConcept,
#'               standardConceptCaption, invalidReason, invalidReasonCaption, conceptCode,
#'               domainId, vocabularyId, conceptClassId.
#'
#' @export

getConceptCodeDetails <- function(conceptCode,
                                  vocabulary,
                                  connectionDetails = NULL,
                                  connection = NULL,
                                  vocabularyDatabaseSchema = NULL,
                                  oracleTempSchema = NULL,
                                  mapToStandard = TRUE) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(conceptCode, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # Set up connection to server ----------------------------------------------------
  if (is.null(connection)) {
    if (!is.null(connectionDetails)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    } else {
      stop("No connection or connectionDetails provided.")
    }
  }
  # Set up concept query -------------------------------------------------------
  conceptQuery <- "SELECT * FROM @vocabularyDatabaseSchema.concept
  WHERE concept_code in ('@conceptCode') AND vocabulary_id = '@vocabulary';"
  conceptDetails <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                               sql = conceptQuery,
                                                               snakeCaseToCamelCase = TRUE,
                                                               vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                                               oracleTempSchema = oracleTempSchema,
                                                               vocabulary = vocabulary,
                                                               conceptCode = paste(conceptCode, collapse = "','")) %>%
    dplyr::tibble()

  #if mapping to the standard concept join to concept_relationship----------------------------------
  if (mapToStandard){
    #conceptIdToMap  <- conceptDetails$.$conceptId
    conceptIdToMap  <- conceptDetails$conceptId
    mappingQuery <- "SELECT b.* FROM @vocabularyDatabaseSchema.concept_relationship a
    JOIN  @vocabularyDatabaseSchema.concept b on b.concept_id = a.concept_id_2 AND a.relationship_id = '@relationship'
    WHERE a.concept_id_1 in (@conceptId);"
    conceptDetails <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                                 sql = mappingQuery,
                                                                 snakeCaseToCamelCase = TRUE,
                                                                 vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                                                 oracleTempSchema = oracleTempSchema,
                                                                 conceptId = conceptIdToMap,
                                                                 relationship = "Maps to") %>%
      dplyr::tibble()
  }

  # if (simplifyToDataframe){ #simplify to dataframe use for conceptsetexpressions
  #   conceptDetails <- conceptDetails$.
  # }

  return(conceptDetails)
}

#' Lookup concept name as a general search
#'
#' This function looks up concepts based on the concept name. It can be modified to conduct
#' an exact name search or general search that contains the concept name in the concept.
#'
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @template     OracleTempSchema
#' @param        keyword a character string used to search OMOP concepts
#' @param        searchType options to aid search. Can use like match, exact match or any match
#' @return       a tibble data frame object with conceptId, conceptName, standardConcept,
#'               standardConceptCaption, invalidReason, invalidReasonCaption, conceptCode,
#'               domainId, vocabularyId, conceptClassId.
#'
#' @export

lookupKeyword<-function(keyword,
                        searchType = c("like", "exact" , "any"),
                        connectionDetails = NULL,
                        connection = NULL,
                        vocabularyDatabaseSchema = NULL,
                        oracleTempSchema = NULL) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(keyword, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # Set up connection to server ----------------------------------------------------
  if (is.null(connection)) {
    if (!is.null(connectionDetails)) {
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
    } else {
      stop("No connection or connectionDetails provided.")
    }
  }
  # Set up concept query -------------------------------------------------------


  conceptQuery <- "SELECT * FROM @vocabularyDatabaseSchema.concept WHERE"
  searchType <- match.arg(searchType)
  searchType <- match.arg(searchType)
  searchType <- switch(searchType,
                       like = " LOWER(concept_name) LIKE '@keyword'",
                       exact = " concept_name = '@keyword'",
                       any = " concept_name LIKE '%@keyword%'")
  conceptQuery <- paste0(conceptQuery, searchType)

  conceptDetails <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                               sql = conceptQuery,
                                                               snakeCaseToCamelCase = TRUE,
                                                               vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                                               oracleTempSchema = oracleTempSchema,
                                                               keyword = keyword) %>%
    dplyr::tibble()

  return(conceptDetails)
}
