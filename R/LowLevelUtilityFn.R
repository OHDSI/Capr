# Copyright 2022 Observational Health Data Sciences and Informatics
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
################
#Checkers
#################
#function to check the component class
#' @export
setGeneric("componentType", function(x){standardGeneric("componentType")})

#' Function to find the Component Class
#'
#' @param x the component to check
#' @return a character string with the component class
#' @include LowLevelClasses.R
#' @rdname componentType-method
#' @aliases componentType
setMethod("componentType", "Component",
          function(x){
            x@MetaData@ComponentType
          })

#' @export
setGeneric("getConceptSetExpression", function(x){standardGeneric("getConceptSetExpression")})

#' Function to get Concept Set Expressions
#'
#' @param x the component to check
#' @return a list of concept set expressions used in the object
#' @rdname getConceptSetExpression-method
#' @aliases getConceptSetExpression
setMethod("getConceptSetExpression", "Component",
          function(x){
            x@ConceptSetExpression
          })

#' @export
setGeneric("getConceptSetId", function(x){standardGeneric("getConceptSetId")})

#' Function to find the ConceptSetId
#'
#' @param x the component to check
#' @return the id from the conceptset expression
#' @rdname getConceptSetId-method
#' @aliases getConceptSetId
setMethod("getConceptSetId", "ConceptSetExpression",
          function(x){
            x@id
          })

#' @rdname getConceptSetId-method
#' @aliases getConceptSetId
setMethod("getConceptSetId", "Query",
          function(x){
            x@CodesetId
          })

##################
#Utilities
#################

#' Function to get concept ids from concept set expression in object
#'
#' @param x the object to check
#' @return a list or vector of concept id integers
#' @include LowLevelClasses.R
#' @export
checkConceptIds <- function(x) {

  checkConceptField(x, field = "CONCEPT_ID")
  # #if object is a concept set expression
  # if (is(x) == "ConceptSetExpression") {
  #   #extract expression
  #   dd <- x@Expression
  #   #extract concept id  from concept set item
  #   rr <- purrr::map_int(dd, ~slot(slot(.x, name = "Concept"), name = "CONCEPT_ID"))
  # }
  #
  # #if the oject is a component
  # if (is(x) == "Component") {
  #   #recurisvely run the funtion
  #   rr <- purrr::map(x@ConceptSetExpression, ~checkConceptIds(.x))
  # }
  # return(rr)
}


#' Function to get concept fields from concept set expression in object
#'
#' @param x the object to check
#' @param field the concept field to check
#' @return a list or vector of concept fields
#' @include LowLevelClasses.R
#' @export
checkConceptField <- function(x, field) {

  #check field input
  fieldOps <- toupper(c("concept_id", "concept_name",
                "standard_concept", "standard_concept_caption",
                "invalid_reason", "invalid_reason_caption",
                "concept_code", "domain_id", "vocabulary_id",
                "concept_class_id"))
  checkFieldOps <- field %in% fieldOps

  if (!checkFieldOps) {
    msg <- paste("Incorrect field input. Valid field options include: \n",
                 paste(fieldOps, collapse = ", "))
    stop(msg)
  }


  #if object is a concept set expression
  if (methods::is(x) == "ConceptSetExpression") {
    #extract expression
    dd <- x@Expression
    #extract concept id  from concept set item
    if (field == "CONCEPT_ID"){
      rr <- purrr::map_int(dd, ~slot(slot(.x, name = "Concept"), name = field))
    } else{
      rr <- purrr::map_chr(dd, ~slot(slot(.x, name = "Concept"), name = field))
    }

  }

  #if the oject is a component
  if (methods::is(x) == "Component") {
    #recurisvely run the funtion
    rr <- purrr::map(x@ConceptSetExpression, ~checkConceptField(.x, field = field))
  }
  return(rr)
}

#function to remove duplicate concept set expressions
#' Function that removes duplicate concept set expressions
#'
#' @param cse the list of concept set expressions used in the object
#' @importFrom purrr discard
#' @import methods
#' @return a list of concept set expressions free of duplicates
removeDupCSE <- function(cse){
  # cse <- list()
  # for(i in seq_along(tt)){
  #   cse <- append(cse, tt[[i]]@ConceptSetExpression)
  # }
  #extract the id from each concept set expression
  idx <- sapply(cse, function(x) x@id)
  #count any duplicates
  dups <- duplicated(idx)
  for(i in seq_along(dups)){#start for loop for i slots in cse list
    if(dups[i]){#check if cseNames are duplicated
      cse[[i]] <- NA_character_ #if cseNames has a duplicate NULL out cse from list
    }#end duplication check
  }#end for loop
  #discard any concept set expressions not needed
  cse <- purrr::discard(cse,function(x) is(x)[1] != "ConceptSetExpression")
  return(cse)
}

#' map the operator among options
#'
#' @param op the operator input we want to map
#' @return the circe op
mapOperator <- function(op){
  opdf <- data.frame(symb = c("<", "<=", ">", ">=", "==", "--", "!-"),
                     text = c("less than", "less than or equal to", "greater than",
                              "greater than or equal to", "equal to", "between", "not between"),
                     short = c("lt", "lte", "gt", "gte", "eq", "bt", "!bt"),
                     idx = 1:7,
                     stringsAsFactors = FALSE)
  jval <- c("lt", "lte", "gt", "gte", "eq", "bt", "!bt")

  jval[which(apply(opdf, 2, "%in%", table= op), arr.ind = T)[1]]
}

#' Create an Empty Component
#'
#'@return an empty component
createEmptyComponent <- function(){
  createComponent(Name = NA_character_, ComponentType = "Empty")
}

###Atribute Options list
#check and finish list
# AttributeOptions <- list('Op' =c("Age", "OccurrenceStartDate", "OccurrenceEnd", "AgeAtEnd",
#                                "AgeAtStart", "PeriodLength", "ValueAsNumber", "RangeLow",
#                                "RangeHigh", "RangeLowRatio", "RangeHighRatio",
#                                "EraStartDate", "EraEndDate", "OccurrenceCount",
#                                "EraLength", "Refills", "Quantity", "DaysSupply",
#                                "EffectiveDrugDose", "VisitLength"),
#                        'Concept' = c("ConditionType", "Gender", "VisitType",
#                                      "DrugType", "RouteConcepts", "DoseUnit",
#                                      "ProviderSepcialty", "PlaceOfService",
#                                      "ProcedureType", "Modifier", "ObservationType",
#                                      "ValueAsConcept", "Qualifier", "Unit",
#                                      "MeasurementType", "Operator", "DeathType",
#                                      "DeviceType"),
#                        'Logical' =c("First", "DrugTypeExclude", "ConditionTypeExclude",
#                                     "VisitTypeExclude", "ProcedureTypeExclude",
#                                     "ObservationTypeExclude", "MeasurementTypeExclude",
#                                     "Abnormal", "DeathTypeExclude", "DeviceTypeExclude"),
#                        'SourceConcept' = c("VisitSourceConcept","DrugSourceConcept",
#                                            "ConditionSourceConcept", "ProcedureSourceConcept",
#                                            "ObservationSourceConcept", "MeasurementSourceConcept",
#                                            "DeathSourceConcept", "DeviceSourceConcept"),
#                        'TextFilter' =c("ValueAsString", "StopReason", "UniqueDeviceId"))





