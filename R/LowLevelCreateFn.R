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
#
##low level create functions

#' createComponent
#' @param Name a name
#' @param Description a description default null
#' @param ComponentType match an arg from vector
#' @param CriteriaExpression include anything for the criteria can be null
#' @param Limit determine limit
#' @param ConceptSetExpression add anny concept set expressions
#' @include LowLevelClasses.R
#' @importFrom methods new
createComponent <- function(Name,
                            Description = NULL,
                            ComponentType = c("ConceptSetExpression",
                                               "Group",
                                               "Query",
                                               "Count",
                                               "Attribute",
                                               "PrimaryCriteria",
                                               "AdditionalCriteria",
                                               "InclusionRules",
                                               "EndStrategy",
                                               "CensoringCriteria",
                                               "CohortEra",
                                               "Empty"),
                            CriteriaExpression = NULL,
                            Limit = NULL,
                            ConceptSetExpression = NULL){
  #if description is null than make string NA
  if(is.null(Description)) {
    Description <- NA_character_
  }
  #create the meta data
  md <- new("MetaData",
            ComponentType = ComponentType,
            Name = Name,
            Description = Description)
  if(ComponentType == "PrimaryCriteria") {
    Limit <- list('PrimaryCriteriaLimit' = new('Limit', Type = Limit))
  } else if(ComponentType == "AdditionalCriteria") {
    Limit <- list('QualifiedLimit' = new('Limit', Type = Limit))
  } else if(ComponentType == "InclusionRules") {
    Limit <- list('ExpressionLimit' = new('Limit', Type = Limit))
  } else{
    Limit <- list()
  }
  if(is.null(CriteriaExpression)) {
    CriteriaExpression <- list()
  }
  if(is.null(Limit)){
    Limit <- list()
  }
  if(is.null(ConceptSetExpression)) {
    ConceptSetExpression <- list()
  }
  comp <- new("Component",
              MetaData = md,
              CriteriaExpression = CriteriaExpression,
              Limit = Limit,
              ConceptSetExpression = ConceptSetExpression)
  return(comp)
}

#######################
#Create Attributes
#########################

#'  createOpAttribute
#' @param Name a name
#' @param Op a type of operator
#' @param Value a value either integer or character for dates
#' @param Extent only if Op is bt or !bt, otherwise NULL. Value is either integer or character for dates
#' @include LowLevelClasses.R
#' @importFrom methods new
#' @export
createOpAttribute <- function(Name, Op, Value, Extent = NULL){
  Op <- mapOperator(Op)
  if(Op == "bt" | Op == "!bt"){
    if(is.null(Extent)){
      stop("need a value and an extent for between and not between")
    }
  }

  if(is.null(Extent)){
    Extent <- NA_integer_
  }
  Contents <- list(Value = Value, Extent = Extent)

  if (grepl("Date", Name)) {
    Contents <- lapply(Contents, as.character)
  } else{
    Contents <- lapply(Contents, as.integer)
  }

  att <- new("OpAttribute",
             Name = Name,
             Op = Op,
             Contents = Contents)
  comp <- createComponent(Name = "OpAttribute",
                          ComponentType = "Attribute",
                          CriteriaExpression = list(att))
  return(comp)

}



#'  createSourceConceptAttribute
#' @param Domain the type of domain for the source concept
#' @param ConceptSetExpression the concept set expression component to add
#' @include LowLevelClasses.R
#' @importFrom methods new
#' @export
createSourceConceptAttribute <- function(Domain,ConceptSetExpression){
  att <- new("SourceConceptAttribute",
             Name = paste0(Domain, "SourceConcept"),
             SourceCodesetId = ConceptSetExpression@ConceptSetExpression[[1]]@id)
  comp <- createComponent(Name = "SourceConceptAttribute",
                          ComponentType = "Attribute",
                          CriteriaExpression = list(att),
                          ConceptSetExpression = ConceptSetExpression@ConceptSetExpression)
  return(comp)
}

#Create Concept Attribute
#'  createConceptAttribue
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @template     OracleTempSchema
#' @param        conceptIds the list of ids to lookup, need OMOP vocabulary connection
#' @param        mapToStandard whether to map concept ids to standard or leave as is default is TRUE
#' @param        name name of the ttribute name
#' @include LowLevelClasses.R
#' @importFrom methods new
#' @export
createConceptAttribute <- function(conceptIds,
                                    connectionDetails = NULL,
                                    connection = NULL,
                                    vocabularyDatabaseSchema = NULL,
                                    oracleTempSchema = NULL,
                                    mapToStandard = TRUE,
                                    name){
  #get concepts for attribute
  concepts <- getConceptIdDetails(conceptIds = conceptIds,
                                  connectionDetails = connectionDetails,
                                  connection = connection,
                                  vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                  oracleTempSchema = oracleTempSchema,
                                  mapToStandard = mapToStandard)
  concepts$INVALID_REASON_CAPTION <- "Unknown"
  concepts$STANDARD_CONCEPT_CAPTION <- "Unknown"
  concepts <- concepts[,c(7,1,2,3,11,12,4)]
  names(concepts)[c(1:4,7)] <- c("CONCEPT_CODE","CONCEPT_ID",
                                 "CONCEPT_NAME", "DOMAIN_ID",
                                 "VOCABULARY_ID")
  concepts <- unname(apply(concepts,1,as.list))
  concepts <- lapply(concepts, function(x) {
    x$CONCEPT_ID <- as.integer(x$CONCEPT_ID)
    return(x)})
  att <- new("ConceptAttribute",
             Name = name,
             Concepts = concepts)
  comp <- createComponent(Name = "ConceptAttribute",
                          ComponentType = "Attribute",
                          CriteriaExpression = list(att))
  return(comp)
}
#old function without concept connection
# createConceptAttribute <- function(conceptIds, mapToStandard = TRUE, name){
#   concepts <- lookupConceptIds(conceptIds = conceptIds, mapToStandard = mapToStandard)
#   concepts <- concepts[,c("CONCEPT_CODE", "CONCEPT_ID", "CONCEPT_NAME", "DOMAIN_ID", "VOCABULARY_ID")]
#   concepts <- unname(apply(concepts,1,as.list))
#   concepts <- lapply(concepts, function(x) {
#     x$CONCEPT_ID <- as.integer(x$CONCEPT_ID)
#     return(x)})
#   att <- new("ConceptAttribute",
#              Name = name,
#              Concepts = concepts)
#   comp <- createComponent(Name = "ConceptAttribute",
#                           ComponentType = "Attribute",
#                           ComponentClass = "Attribute",
#                           CriteriaExpression = list(att))
#   return(comp)
# }


#'  createLogicalAttribue
#' @param name is the name of the attribute
#' @param logic whether the logic is true or false, default is true
#' @include LowLevelClasses.R
#' @importFrom methods new
#' @export
createLogicalAttribute <- function(name, logic = TRUE){
  att <- new("LogicAttribute", Name = name, Logic = logic)
  comp <- createComponent(Name = "LogicAttribute",
                          ComponentType = "Attribute",
                          CriteriaExpression = list(att))
  return(comp)
}


#create query function
#'  createQuery
#' @param Domain list the domain from the table we are searching in the query
#' @param Component add the concept set expression we want to query
#' @param attributeList a list of attribute class components to add, if not attributes keep null
#' @param Name is the name of query, optional
#' @param Description an optional description of the query
#' @include LowLevelClasses.R
#' @importFrom methods new
createQuery <- function(Domain,
                        Component = NULL,
                        attributeList = NULL,
                        Name = NULL,
                        Description = NULL){
  #if name null use a generic name
  if(is.null(Name)){
    Name <- paste(Domain, "Query")
  }
  #create query Class
  if(is.null(Component)){ #if null only use domain
    query <- new("Query",
                 Domain = Domain)
    cse <- list()
  } else{ #else link the codeset Id
    query <- new("Query",
                 Domain = Domain,
                 CodesetId = Component@ConceptSetExpression[[1]]@id)
    cse <- Component@ConceptSetExpression
  }

  #check attributes are all attributes
  check <- lapply(attributeList,componentType)
  if(!all(grepl("Attribute",check))){
    stop("not all additional parameters are attributes")
  }

  if(!is.null(attributeList)){
    query@Attributes <- lapply(attributeList, function(x) x@CriteriaExpression[[1]])
    nm <- sapply(attributeList, function(x) x@MetaData@Name)
    idx <- c(grep("SourceConceptAttribute", nm), grep("CorrelatedCriteria", nm))
    if(length(idx) >0){
      for(i in idx){
        cse <- append(cse, attributeList[[i]]@ConceptSetExpression)
      }
    }
  }

  comp <- createComponent(Name = Name,
                          Description = Description,
                          ComponentType = "Query",
                          CriteriaExpression = list(query),
                          ConceptSetExpression = cse)
  return(comp)
}

