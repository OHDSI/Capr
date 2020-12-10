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
##Visit Occurrence wrapper for create Query
#' create VisitOccurrence for create Query
#'
#' This function creates a query based on visitOccurrence. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createVisitOccurrence <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "VisitOccurrence", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#' create ObservationPeriod for create Query
#'
#' This function creates a query based on ObservationPeriod. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createObservationPeriod <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "ObservationPeriod", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#ConditionOccurrence wrapper for create Query
#' create ConditionOccurrence for create Query
#'
#' This function creates a query based on ConditionOccurrence. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createConditionOccurrence <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "ConditionOccurrence", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}


#Observation wrapper for create Query
#' create Observation for create Query
#'
#' This function creates a query based on Observation. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createObservation <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "Observation", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#ConditionEra wrapper for create Query
#' create ConditionEra for create Query
#'
#' This function creates a query based on ConditionEra. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createConditionEra <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "ConditionEra", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}
#DrugEra wrapper for create Query
#' create DrugEra for create Query
#'
#' This function creates a query based on DrugEra. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createDrugEra <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "DrugEra", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#DrugExposure wrapper for create Query
#' create DrugExposure for create Query
#'
#' This function creates a query based on DrugExposure. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createDrugExposure <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "DrugExposure", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#Measurement wrapper for create Query
#' create Measurement for create Query
#'
#' This function creates a query based on Measurement. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createMeasurement <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "Measurement", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#ProcedureOccurrence wrapper for create Query
#' create ProcedureOccurrence for create Query
#'
#' This function creates a query based on ProcedureOccurrence. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createProcedureOccurrence <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "ProcedureOccurrence", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#Death wrapper for create Query
#' create Death for create Query
#'
#' This function creates a query based on Death. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createDeath <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "Death", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#DoseEra wrapper for create Query
#' create DoseEra for create Query
#'
#' This function creates a query based on DoseEra. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createDoseEra <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "DoseEra", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}

#DeviceExposure wrapper for create Query
#' create DeviceExposure for create Query
#'
#' This function creates a query based on DeviceExposure. Input pertinent conceptSetExpression and attirbuteList
#' @param conceptSetExpression place a component class concept set expression for domain. The concept set expressions
#' must be adhere to the domain of the query
#' @param attributeList a list of attributes to add to the query, if no attributes used then leave null
#' @include lowLevelCreateFn.R
#' @return a componet of query class
#' @export
createDeviceExposure <- function(conceptSetExpression=NULL, attributeList =NULL){
  createQuery(Component = conceptSetExpression, #create Query with conceptSet Expression
              Domain = "DeviceExposure", #preassign domain from function wrapper
              attributeList = attributeList) #add attributes to query if there are any must be list
}
