# Copyright 2020 Observational Health Data Sciences and Informatics
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
#
##' Lookup Concepts by OMOP Concept Id
#'
#' This function looks up concepts using the OMOP concept id. Function requires a dbms connection to use
#'
#' @param conceptIds standard concept id
#' @param cdmDatabaseSchema designate cdm Database schema, if connected to cdm then leave NULL
#' @param mapToStandard logic toggle to map the concepts to standard OMOP concepts
#' @return a data frame is returned ordered:
#' concept_id, concept_name, standard_concept, standard_concept_caption
#' invalid_reason, invalid_reason_caption, concept_code, domain_id, vocabulary_id, concept_class_id.
#' @importFrom DatabaseConnector querySql disconnect
#' @importFrom SqlRender render translate
#' @export

lookupConceptIds <- function(conceptIds, cdmDatabaseSchema=NULL,mapToStandard =TRUE){

  if (is.null(connectionDetails$conn)) {
    conn <- conn
  } else {
    conn <- connectionDetails$conn
  }

  if(is.null(cdmDatabaseSchema)){
    cdmDatabaseSchema <- connectionDetails$schema
  }

  conceptQuery <- "SELECT * FROM @cdmDatabaseSchema.concept WHERE concept_id IN (@conceptId);"


  sql <- SqlRender::render(conceptQuery,cdmDatabaseSchema=cdmDatabaseSchema,
                           conceptId = conceptIds)

  sql <- SqlRender::translate(sql,targetDialect=connectionDetails$dbms)

  concepts_df<-DatabaseConnector::querySql(conn,sql)

  if(nrow(concepts_df)==0){
    stop("Query returned empty table concept Id not in cdm")
  }

  concepts_df <- formatConceptTable(concepts_df)
  if(mapToStandard){
    concepts_df <- mapConceptToStandard(concepts_df)
  }
  return(concepts_df)

  if (is.null(connectionDetails$conn)) {
    DatabaseConnector::disconnect(conn)

  }
}


#' Lookup Concepts by OMOP Concept Code using Vocabulary
#'
#' This function looks up concepts using the OMOP concept code and vocabulary. Function requires a dbms connection to use
#'
#' @param vocabulary source vocabulary to search (i.e. ICD10, SNOMED). Must be character string
#' @param conceptCode source code of vocabulary to serach (example: if ICD10 E11 is T2D). Must be a character string.
#' @param cdmDatabaseSchema designate cdm Database schema, if connected to cdm then leave NULL
#' @param mapToStandard logic toggle to map the concepts to standard OMOP concepts
#' @return a data frame is returned ordered:
#' concept_id, concept_name, standard_concept, standard_concept_caption
#' invalid_reason, invalid_reason_caption, concept_code, domain_id, vocabulary_id, concept_class_id.
#' @importFrom DatabaseConnector querySql disconnect
#' @importFrom SqlRender render translate
#' @export

lookupConceptCodes <- function(vocabulary, conceptCode, cdmDatabaseSchema=NULL, mapToStandard =TRUE){


  #replace this with cdm connection utils
  if (is.null(connectionDetails$conn)) {
    conn <- conn
  } else {
    conn <- connectionDetails$conn
  }

  if(is.null(cdmDatabaseSchema)){
    cdmDatabaseSchema <- connectionDetails$schema
  }

  conceptQuery <- "SELECT * FROM @cdmDatabaseSchema.concept
  WHERE concept_code in ('@conceptCode') AND vocabulary_id = '@vocabulary';"


  sql <- SqlRender::render(conceptQuery,cdmDatabaseSchema=cdmDatabaseSchema,
                           vocabulary = vocabulary,
                           conceptCode = conceptCode)
  if(length(conceptCode) > 1){

    options(useFancyQuotes = FALSE)
    sql<-sub(sQuote(paste0(conceptCode,collapse = ",")),paste0("'",conceptCode, "'",collapse = ",") ,sql)

  }
  sql <- SqlRender::translate(sql,targetDialect=connectionDetails$dbms)

  concepts_df<-DatabaseConnector::querySql(conn,sql)
  if(nrow(concepts_df)==0){
    stop("Query returned empty table concept Id not in cdm")
  }
  concepts_df <- formatConceptTable(concepts_df)
  if(mapToStandard){
    concepts_df <- mapConceptToStandard(concepts_df)
  }

  return(concepts_df)

  #replace this with cdm connection utils
  if (is.null(connectionDetails$conn)) {
    DatabaseConnector::disconnect(conn)

  }
}

#' Lookup Concepts by Vocabulary
#'
#' This function looks up concepts using the OMOP concept code and vocabulary. Function requires a dbms connection to use
#'
#' @param vocabulary source vocabulary to search (i.e. ICD10, SNOMED). Must be character string
#' @param cdmDatabaseSchema designate cdm Database schema, if connected to cdm then leave NULL
#' @param mapToStandard logic toggle to map the concepts to standard OMOP concepts
#' @return a data frame is returned ordered:
#' concept_id, concept_name, standard_concept, standard_concept_caption
#' invalid_reason, invalid_reason_caption, concept_code, domain_id, vocabulary_id, concept_class_id.
#' @importFrom DatabaseConnector querySql disconnect
#' @importFrom SqlRender render translate
#' @export

lookupVocabulary <- function(vocabulary, cdmDatabaseSchema=NULL, mapToStandard =TRUE){


  #replace this with cdm connection utils
  if (is.null(connectionDetails$conn)) {
    conn <- conn
  } else {
    conn <- connectionDetails$conn
  }

  if(is.null(cdmDatabaseSchema)){
    cdmDatabaseSchema <- connectionDetails$schema
  }

  conceptQuery <- "SELECT * FROM @cdmDatabaseSchema.concept
  WHERE vocabulary_id = '@vocabulary';"


  sql <- SqlRender::render(conceptQuery,cdmDatabaseSchema=cdmDatabaseSchema,
                           vocabulary = vocabulary)
  sql <- SqlRender::translate(sql,targetDialect=connectionDetails$dbms)

  concepts_df<-DatabaseConnector::querySql(conn,sql)
  if(nrow(concepts_df)==0){
    stop("Query returned empty table concept Id not in cdm")
  }

  if(mapToStandard){
    concepts_df <- suppressWarnings(formatConceptTable(concepts_df))
    concepts_df <- mapConceptToStandard(concepts_df)
  } else{
    concepts_df <- formatConceptTable(concepts_df)
  }

  return(concepts_df)

  #replace this with cdm connection utils
  if (is.null(connectionDetails$conn)) {
    DatabaseConnector::disconnect(conn)

  }
}


#' Lookup concept name as a general search
#'
#' This function looks up concepts based on the concept name. It can be modified to conduct
#' an exact name search or general search that contains the concept name in the concept.
#'
#' @param keyword a word a or phrase to search concepts
#' @param search_type how to use keyword: a) like the keyword, b)exact keyword , or c) any match of keyword
#' @param cdmDatabaseSchema designate cdm Database schema, if connected to cdm then leave NULL
#' @return a data.table with all concepts found from the search
#' @importFrom data.table data.table
#' @importFrom DatabaseConnector querySql disconnect
#' @importFrom SqlRender render translate
#' @export

lookupKeyword<-function(keyword,search_type = c("like", "exact" , "any"),
                        cdmDatabaseSchema=NULL){


  if (is.null(connectionDetails$conn)) {
    conn <- conn
  } else {
    conn <- connectionDetails$conn
  }

  if(is.null(cdmDatabaseSchema)){
    cdmDatabaseSchema <- connectionDetails$schema
  }


  search_type <- match.arg(search_type)

  nameSearchQuery <- "SELECT * FROM @cdmDatabaseSchema.concept WHERE"

  search_type <- match.arg(search_type)
  searchTypeAdd <- switch(search_type,
                          like = " LOWER(concept_name) LIKE '@keyword'",
                          exact = " concept_name = '@keyword'",
                          any = " concept_name LIKE '%@keyword%'")

  nameSearchQuery <- paste0(nameSearchQuery, searchTypeAdd)



  sql <- SqlRender::render(nameSearchQuery,cdmDatabaseSchema=cdmDatabaseSchema,
                           keyword = keyword)

  sql <- SqlRender::translate(sql,targetDialect=connectionDetails$dbms)

  concept_search<-DatabaseConnector::querySql(conn,sql)
  concept_search <- data.table::data.table(concept_search)
  concept_search <- formatConceptTable(concept_search)

  return(concept_search)


  if (is.null(connectionDetails$conn)) {
    DatabaseConnector::disconnect(conn)
  }
}


#' Map to a Standard Concept
#'
#' This function allows you to map a non-standard concept to a standard concept.
#' Necessary in order to run valid queries in the OMOP CDM
#' @param conceptsDf a dataframe containing a concept id that can be mapped to standard. Use this in a pipe
#' @param conceptId a non-standard concept Id used to map to a standard
#' @param cdmDatabaseSchema designate cdm Database schema, if connected to cdm then leave NULL
#' @return a dataframe containing that mapped standard concept id
#' @importFrom DatabaseConnector querySql disconnect
#' @importFrom SqlRender render translate
#' @export

mapConceptToStandard <- function(conceptsDf =NULL, conceptId = NULL, cdmDatabaseSchema = NULL){

  if (is.null(connectionDetails$conn)) {
    conn <- conn
  } else {
    conn <- connectionDetails$conn
  }

  if(is.null(cdmDatabaseSchema)){
    cdmDatabaseSchema <- connectionDetails$schema
  }

  if(missing(conceptId)){
    conceptId <- conceptsDf$CONCEPT_ID
  }


  relationship <- "Maps to"
  mappingQuery <- "SELECT * FROM @cdmDatabaseSchema.concept_relationship
  JOIN  @cdmDatabaseSchema.concept on concept_id = concept_id_2 AND relationship_id = '@relationship'
  WHERE concept_id_1 in (@conceptId);"


  sql <- SqlRender::render(mappingQuery,cdmDatabaseSchema=cdmDatabaseSchema,
                           conceptId = conceptId,
                           relationship = relationship)

  sql <- SqlRender::translate(sql,targetDialect=connectionDetails$dbms)

  map <- DatabaseConnector::querySql(conn,sql)
  map <- map[ ,c(7:16)]
  map <- formatConceptTable(map)
  return(map)

  if (is.null(connectionDetails$conn)) {
    DatabaseConnector::disconnect(conn)
  }

}

