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
##################
#save component function
#########################

setGeneric("saveState", function(x){standardGeneric("saveState")})

#' Save State for components
#'
#' These function coerce s4 CAPR objects to s3 so that they are in a json save state
#'
#' @param x a criteria class object in s4
#' @return the object converted to s3 to be saved as a json object
#' @rdname saveState-method
#' @aliases saveState
#' @aliases saveState,Concept-method
setMethod("saveState", "Concept",
          function(x){
            nm <- methods::slotNames(methods::is(x))
            concept <- lapply(nm, slot, object = x)
            names(concept) <- nm
            return(concept)
          })

#' @rdname saveState-method
#' @aliases saveState,ConceptSetItem-method
setMethod("saveState", "ConceptSetItem",
          function(x){
            list('Concept' = saveState(x@Concept),
                 'isExcluded' = x@isExcluded,
                 'includeDescendants' = x@includeDescendants,
                 'includeMapped' = x@includeMapped)
          })

#' @rdname saveState-method
#' @aliases saveState,ConceptSetExpression-method
setMethod("saveState", "ConceptSetExpression",
          function(x){
            list('id' = x@id,
                 'Name' = x@Name,
                 'Expression' = lapply(x@Expression, saveState))
          })

#' @rdname saveState-method
#' @aliases saveState,OpAttribute-method
setMethod("saveState", "OpAttribute",
          function(x){
            list('Name' = x@Name,
                 'Op' = x@Op,
                 'Contents' = x@Contents)
          })

#' @rdname saveState-method
#' @aliases saveState,SourceConceptAttribute-method
setMethod("saveState", "SourceConceptAttribute",
          function(x){
            list('Name' = x@Name,
                 'SourceCodesetId' = x@SourceCodesetId)
          })

#' @rdname saveState-method
#' @aliases saveState,ConceptAttribute-method
setMethod("saveState", "ConceptAttribute",
          function(x){
            list('Name' = x@Name,
                 'Concepts' = x@Concepts)
          })

#' @rdname saveState-method
#' @aliases saveState,CorrelatedCriteriaAttribute-method
setMethod("saveState", "CorrelatedCriteriaAttribute",
          function(x){
            list('Name' = x@Name,
                 'Group' = saveState(x@Group))
          })

#' @rdname saveState-method
#' @aliases saveState,LogicAttribute-method
setMethod("saveState", "LogicAttribute",
          function(x){
            list('Name' = x@Name,
                 'Logic' = x@Logic)
          })

#' @rdname saveState-method
#' @aliases saveState,Window-method
setMethod("saveState", "Window",
          function(x){
            list('Event' = x@Event,
                 'Start' = x@Start,
                 'End' = x@End,
                 'Index' = x@Index)
          })

#' @rdname saveState-method
#' @aliases saveState,Timeline-method
setMethod("saveState", "Timeline",
          function(x){
            list('StartWindow' = saveState(x@StartWindow),
                 'EndWindow' = saveState(x@EndWindow),
                 'RestrictVisit' = x@RestrictVisit,
                 'IgnoreObservationPeriod' = x@IgnoreObservationPeriod)
          })

#' @rdname saveState-method
#' @aliases saveState,Occurrence-method
setMethod("saveState", "Occurrence",
          function(x){
            list('Type' = x@Type,
                 'Count' = x@Count,
                 'isDistinct' = x@isDistinct)
          })

#' @rdname saveState-method
#' @aliases saveState,ExpressionType-method
setMethod("saveState", "ExpressionType",
          function(x){
            list('Type' = x@Type,
                 'Count' = x@Count)
          })

#' @rdname saveState-method
#' @aliases saveState,ObservationWindow-method
setMethod("saveState", "ObservationWindow",
          function(x){
            list('PriorDays' = x@PriorDays,
                 'PostDays' = x@PostDays)
          })

#' @rdname saveState-method
#' @aliases saveState,Limit-method
setMethod("saveState", "Limit",
          function(x){
            list('Type' = x@Type)
          })

#' @rdname saveState-method
#' @aliases saveState,Query-method
setMethod("saveState", "Query", function(x){
  ll <- list('Domain' = x@Domain,
             'CodesetId' = x@CodesetId,
             'Attributes' = lapply(x@Attributes, saveState))
  if(length(ll$CodesetId) == 0){
    ll$CodesetId <- NA_character_
  }
  return(ll)
})

#' @rdname saveState-method
#' @aliases saveState,Count-method
setMethod("saveState", "Count",
          function(x){
            list('Criteria' = saveState(x@Criteria),
                 'Timeline' = saveState(x@Timeline),
                 'Occurrence' =saveState(x@Occurrence))
          })

#' @rdname saveState-method
#' @aliases saveState,Group-method
setMethod("saveState", "Group",
          function(x){
            list('Type' = saveState(x@Type),
                 'CriteriaList' = lapply(x@CriteriaList, saveState),
                 'DemographicCriteriaList' = lapply(x@DemographicCriteriaList, saveState),
                 'Groups' = lapply(x@Groups, saveState))
          })

#' @rdname saveState-method
#' @aliases saveState,MetaData-method
setMethod("saveState", "MetaData",
          function(x){
            list('ComponentClass' = x@ComponentClass,
                 'Name' = x@Name,
                 'Description' = x@Description)
          })

#' @rdname saveState-method
#' @aliases saveState,DateOffsetEndStrategy-method
setMethod("saveState", "DateOffsetEndStrategy",
          function(x){
            list('DateField' = x@DateField,
                 'Offset' = x@Offset)
          })

#' @rdname saveState-method
#' @aliases saveState,CustomEraEndStrategy-method
setMethod("saveState", "CustomEraEndStrategy",
          function(x){
            list('DrugCodesetId' = x@DrugCodesetId,
                 'GapDays' = x@GapDays,
                 'Offset' = x@Offset)
          })

#' @rdname saveState-method
#' @aliases saveState,EndOfCtsObsEndStrategy-method
setMethod("saveState", "EndOfCtsObsEndStrategy",
          function(x){
            list('EndOfContinuousObservation' = x@EndOfContinuousObservation)
          })

#' @rdname saveState-method
#' @aliases saveState,CollapseSettings-method
setMethod("saveState", "CollapseSettings",
                    function(x){
                      list('CollapseType' = x@CollapseType,
                           'EraPad' = x@EraPad)
                    })

#' @rdname saveState-method
#' @aliases saveState,CensorWindow-method
setMethod("saveState", "CensorWindow",
          function(x){
            list('StartDate' = x@StartDate,
                 'EndDate' = x@EndDate)
          })

#' @rdname saveState-method
#' @aliases saveState,Component-method
setMethod("saveState", "Component",
          function(x){
            if(x@MetaData@ComponentClass == "PrimaryCriteria"){
              cl <- lapply(x@CriteriaExpression$CriteriaList, saveState)
              ow <- saveState(x@CriteriaExpression$ObservationWindow)
              ll <-  list('MetaData' = saveState(x@MetaData),
                          'CriteriaExpression' = list('CriteriaList' = cl,
                                                      'ObservationWindow' = ow),
                          'Limit' = lapply(x@Limit, saveState),
                          'ConceptSetExpression' = lapply(x@ConceptSetExpression, saveState))
            } else if(x@MetaData@ComponentClass == "CohortEra"){
              ll <-  list('MetaData' = saveState(x@MetaData),
                          'CriteriaExpression' = list('CollapseSettings' = saveState(x@CriteriaExpression$CollapseSettings),
                                                      'CensorWindow' = saveState(x@CriteriaExpression$CensorWindow)),
                          'Limit' = list(),
                          'ConceptSetExpression' = list())
            } else{
              ll <-  list('MetaData' = saveState(x@MetaData),
                     'CriteriaExpression' = lapply(x@CriteriaExpression, saveState),
                     'Limit' = lapply(x@Limit, saveState),
                     'ConceptSetExpression' = lapply(x@ConceptSetExpression, saveState))
            }
            return(ll)
          })


