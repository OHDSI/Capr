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

#Concept
#' A coercion function to convert to a CAPR concept
#'
#' This function takes a data frame containing information about a concept and converts it into the Concept class
#'
#' @param x the object to coerce
#' @return a concept class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.Concept <- function(x){

  x$INVALID_REASON_CAPTION <- ifelse(is.na(x$invalidReason),"Valid", "Invalid")
  x$INVALID_REASON <- ifelse(is.na(x$invalidReason),"V", x$invalidReason)
  x$STANDARD_CONCEPT_CAPTION <- ifelse(is.na(x$standardConcept),"Non-Standard",
                                       ifelse(x$standardConcept =="C", "Classification", "Standard"))
  x$STANDARD_CONCEPT <- ifelse(is.na(x$standardConcept), "N", x$standardConcept)

  new("Concept",
      CONCEPT_ID = as.integer(x$conceptId),
      CONCEPT_NAME = as.character(x$conceptName),
      STANDARD_CONCEPT = as.character(x$STANDARD_CONCEPT),
      STANDARD_CONCEPT_CAPTION = as.character(x$STANDARD_CONCEPT_CAPTION),
      INVALID_REASON = as.character(x$INVALID_REASON),
      INVALID_REASON_CAPTION = as.character(x$INVALID_REASON_CAPTION),
      CONCEPT_CODE = as.character(x$conceptCode),
      DOMAIN_ID =as.character(x$domainId),
      VOCABULARY_ID = as.character(x$vocabularyId),
      CONCEPT_CLASS_ID = as.character(x$conceptClassId))
}

#' A coercion function to load to a CAPR concept
#'
#' This function takes a data frame containing information about a concept and converts it into the Concept class
#'
#' @param x the object to coerce
#' @return a concept class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.ConceptLoad <- function(x){
  new("Concept",
      CONCEPT_ID = as.integer(x$CONCEPT_ID),
      CONCEPT_NAME = as.character(x$CONCEPT_NAME),
      STANDARD_CONCEPT = as.character(x$STANDARD_CONCEPT),
      STANDARD_CONCEPT_CAPTION = as.character(x$STANDARD_CONCEPT_CAPTION),
      INVALID_REASON = as.character(x$INVALID_REASON),
      INVALID_REASON_CAPTION = as.character(x$INVALID_REASON_CAPTION),
      CONCEPT_CODE = as.character(x$CONCEPT_CODE),
      DOMAIN_ID =as.character(x$DOMAIN_ID),
      VOCABULARY_ID = as.character(x$VOCABULARY_ID),
      CONCEPT_CLASS_ID = as.character(x$CONCEPT_CLASS_ID))
}

#' A coercion function to convert to a CAPR conceptSetItem
#'
#' This function takes a list and converts it into the Concept set Item class
#'
#' @param x the object to coerce
#' @return a conceptSetItem class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.ConceptSetItem <- function(x){
  new("ConceptSetItem",
      Concept = as.ConceptLoad(x$Concept),
      isExcluded = x$isExcluded,
      includeDescendants = x$includeDescendants,
      includeMapped = x$includeMapped)
}

#' A coercion function to convert to a CAPR conceptSetExpression
#'
#'
#' @param x the object to coerce
#' @return a concept set expression class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.ConceptSetExpression <- function(x){
  new("ConceptSetExpression",
      id = x$id,
      Name = x$Name,
      Expression = lapply(x$Expression, as.ConceptSetItem))
}

#Meta Data
#' A coercion function to convert to a CAPR metaData
#'
#' @param x the object to coerce
#' @return a meta data class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.MetaData <- function(x){
  md <- new("MetaData",
            ComponentType = x$ComponentType,
            Name = x$Name)
  if(is.null(x$Description)){
    md@Description <- NA_character_
  }else{
    md@Description <- x$Description
  }
  return(md)
}

#Sub components
#' A coercion function to convert to a CAPR window
#'
#' @param x the object to coerce
#' @return a window class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.Window <- function(x){
  checkNames <- all(names(x) %in% c("Event", "Start", "End", "Index"))
  if(checkNames){
    newWin <- new("Window",
                  Event = x$Event,
                  Start = x$Start,
                  End = x$End,
                  Index = x$Index)
  }else{
    newWin <- new("Window")
    win <- unlist(x)
    #update coefficieint Before -1 nad After 1
    coeff <- as.integer(unname(win[grep("Coeff", names(win))]))
    coeff <- ifelse(coeff == 1, "After", "Before")
    newWin@Start$Coeff <- coeff[1]
    newWin@End$Coeff <- coeff[2]
    #update Event
    idx <- grep("Event", names(win))
    if(length(idx) !=0 ){
      event <- as.logical(win[idx])
      newWin@Event <- ifelse(event,"EventEnds", "EventStarts")
    } else{
      newWin@Event <- "EventStarts"
    }
    #update Index
    idx <- grep("Index", names(win))
    if(length(idx) !=0){
      index <- as.logical(win[idx])
      newWin@Index <- ifelse(index, "IndexEndDate", "IndexStartDate")
    } else{
      newWin@Index <- "IndexStartDate"
    }
    #set up days
    days <- win[grep("Days", names(win))]
    if(length(days) ==2){
      newWin@Start$Days <- as.integer(unname(days[grep("Start", names(days))]))
      newWin@End$Days <- as.integer(unname(days[grep("End", names(days))]))
    } else if (length(days) ==1){ #this is where errors can occur, correctly place all
      if(length(grep("Start", names(win))) ==1){ #if the length of start list is 1 Start days is all
        newWin@Start$Days <- "All"
        newWin@End$Days <- as.integer(unname(days[1]))
      } else if (length(grep("End", names(win)))==1){ #if the length of end list is 1 end days is all
        newWin@End$Days <- "All"
        newWin@Start$Days <- as.integer(unname(days[1]))
      }else{ #if neither satify default to putting end days all and start days as number
        newWin@End$Days <- "All"
        newWin@Start$Days <- as.integer(unname(days[1]))
      }
    } else{
      newWin@Start$Days <- "All"
      newWin@End$Days <- "All"
    }
  }
  return(newWin)
}

#' A coercion function to convert to a CAPR Occurrence
#'
#' @param x the object to coerce
#' @return a occurrence class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.Occurrence <- function(x){
  if(is.integer(x$Type)){
    x$Type  <- ifelse(x$Type ==2, "at_least",
                      ifelse(x$Type ==1, "at_most", "exactly"))
  }
  occ <- new("Occurrence",
             Type = x$Type,
             Count = x$Count)
  if("isDistinct" %in% names(x)){
    occ@isDistinct <- x$isDistinct
  }else{
    occ@isDistinct <- FALSE
  }

  return(occ)
}

#' A coercion function to convert to a CAPR timeline
#'
#' @param x the object to coerce
#' @return a timeline class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.Timeline <- function(x){
  #create new timeline object
  tw <- new("Timeline",
            StartWindow = as.Window(x$StartWindow)) #Coerce Start Window
  if("EndWindow" %in% names(x)){ #check if end window is in names
    check <- sum(sapply(x$EndWindow, length)) #check if end window is empty
    if(check == 0){
      tw@EndWindow <- new("Window") #if empty create new empty window
    } else{
      tw@EndWindow <- as.Window(x$EndWindow) #if not empty coerce
    }
  } else{
    tw@EndWindow <- new("Window") #if end window not in names create empty window
  }

  if("RestrictVisit" %in% names(x)){ #check if restrict visit is in names
    tw@RestrictVisit <- x$RestrictVisit #if is than add to obj
  } else{
    tw@RestrictVisit <- FALSE #if name not in obj than make false
  }

  if("IgnoreObservationPeriod" %in% names(x)){ #check if ignore observation period is in names
    tw@IgnoreObservationPeriod <- x$IgnoreObservationPeriod#if is than add to obj
  } else{
    tw@IgnoreObservationPeriod <- FALSE#if name not in obj than make false
  }
  return(tw)
}

#' A coercion function to convert to a CAPR expression type
#'
#' @param x the object to coerce
#' @return an expressionType class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.ExpressionType <- function(x){
  et <- new("ExpressionType",
            Type = x$Type)
  if("Count" %in% names(x)){
    if(is.null(x$Count) | is.character(x$Count)){
      et@Count <- NA_integer_
    } else{
      et@Count <- x$Count
    }
  }
  return(et)
}

#' A coercion function to convert to a CAPR ObservationWindow
#'
#' @param x the object to coerce
#' @return an observation window class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.ObservationWindow <- function(x){
  new("ObservationWindow",
      PriorDays = x$PriorDays,
      PostDays = x$PostDays)
}

#' A coercion function to convert to a CAPR limit
#'
#' @param x the object to coerce
#' @return a limit class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.Limit <- function(x){
  new("Limit",
      Type = x$Type)
}

#' A coercion function to convert to a CAPR CohortEra
#'
#' @param x the object to coerce
#' @return a cohortEra class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.CohortEra <- function(x){
  cs <- x$CollapseSettings
  cw <- x$CensorWindow
  cs <- new("CollapseSettings",
            CollapseType = cs$CollapseType,
            EraPad = as.integer(cs$EraPad))

  if(length(cw) == 0){
    cw <- new("CensorWindow")
  } else if(length(cw) ==1){
    if(names(cw) == "StartDate"){
      cw <- new("CensorWindow",
                StartDate = cw$StartDate)
    } else{
      cw <- new("CensorWindow",
                EndDate = cw$EndDate)
    }
  } else{
    cw <- new("CensorWindow",
              StartDate = cw$StartDate,
              EndDate = cw$EndDate)
  }
  comp <- createComponent(Name = "Cohort Era Details",
                          Description = NULL,
                          ComponentType = "CohortEra",
                          CriteriaExpression = list('CollapseSettings' = cs,
                                                    'CensorWindow' = cw))
  return(comp)
}


#########---Load Only ---------######
#Attribute
#' A coercion function to convert to a CAPR attribute
#'
#' This function takes a saved CAPR attribute json and returns an attribute CAPR R object
#'
#' @param x the object to coerce
#' @return a attribute class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.AttributeLoad <- function(x){

  if(all(names(x) %in% c("Name", "Op", "Contents"))){
    att <- new("OpAttribute",
               Name = x$Name,
               Op = x$Op,
               Contents = x$Contents)
    if(is.null(att@Contents$Extent)){
      if(grepl("Date",att@Name)){
        att@Contents$Extent <- NA_character_
      }else{
        att@Contents$Extent <- NA_integer_
      }

    }
  }
  if(all(names(x) %in% c("Name", "SourceCodesetId"))){
    att <- new("SourceConceptAttribute",
               Name = x$Name,
               SourceCodesetId = x$SourceCodesetId)
  }
  if(all(names(x) %in% c("Name", "Concepts"))){
    att <- new("ConceptAttribute",
               Name = x$Name,
               Concepts = x$Concepts)
  }
  if(all(names(x) %in% c("Name", "Group"))){
    att <- new("CorrelatedCriteriaAttribute",
               Name = x$Name,
               Group = as.GroupLoad(x$Group))
  }
  if(all(names(x) %in% c("Name", "Logic"))){
    att <- new("LogicAttribute",
               Name = x$Name,
               Logic = x$Logic)
  }
  return(att)
}

#' A coercion function to convert to a CAPR query
#'
#' This function takes a saved CAPR query json and returns query CAPR R object
#'
#' @param x the object to coerce
#' @return a query class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.QueryLoad <- function(x){
  qq <- new("Query",
            Domain = x$Domain,
            Attributes = lapply(x$Attributes, as.AttributeLoad))
  if(is.null(x$CodesetId)){
    qq@CodesetId <- NA_character_
  } else{
    qq@CodesetId <- x$CodesetId
  }
  return(qq)
}

#' A coercion function to convert to a CAPR count
#'
#' This function takes a saved CAPR count json and returns count CAPR R object
#'
#' @param x the object to coerce
#' @return a count class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.CountLoad <- function(x){
  new("Count",
      Criteria = as.QueryLoad(x$Criteria),
      Timeline = as.Timeline(x$Timeline),
      Occurrence = as.Occurrence(x$Occurrence))
}

#' A coercion function to convert to a CAPR group
#'
#' This function takes a saved CAPR group json and returns group CAPR R object
#'
#' @param x the object to coerce
#' @return a group class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.GroupLoad <- function(x){
  new("Group",
      Type = as.ExpressionType(x$Type),
      CriteriaList = lapply(x$CriteriaList, as.CountLoad),
      DemographicCriteriaList = lapply(x$DemographicCriteriaList, as.AttributeLoad),
      Groups = lapply(x$Groups, as.GroupLoad))
}

#' A coercion function to convert to a CAPR EndStrategy
#'
#' This function takes a saved CAPR EndStrategy json and returns EndStrategy CAPR R object
#'
#' @param x the object to coerce
#' @return a EndStrategy class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.EndStrategyLoad <- function(x){
  if("DateField" %in% names(x)){
    es <- new("DateOffsetEndStrategy",
              DateField = x$DateField,
              Offset = as.integer(x$Offset))
  } else if("DrugCodesetId" %in% names(x)){
    es <- new("CustomEraEndStrategy",
              DrugCodeSetId = x$DrugCodesetId,
              GapDays = as.integer(x$GapDays),
              Offset = as.integer(x$Offset))
  } else{
    es <- new("EndOfCtsObsEndStrategy")
  }
  return(es)
}

#' A coercion function to convert to a CAPR component
#'
#' This function takes a saved CAPR component json and returns component CAPR R object
#'
#' @param x the object to coerce
#' @return a component class object
#' @importFrom methods new
#' @include LowLevelClasses.R
as.ComponentLoad <- function(x){
  if(x$MetaData$ComponentType == "CohortEra"){
    comp <- as.CohortEra(x$CriteriaExpression)
  } else{
    comp <- new("Component",
                MetaData = as.MetaData(x$MetaData),
                Limit = lapply(x$Limit, as.Limit),
                ConceptSetExpression = lapply(x$ConceptSetExpression, as.ConceptSetExpression))
    compClass <- componentType(comp)
    comp@CriteriaExpression <- switch(compClass,
                                      PrimaryCriteria = list('CriteriaList' = lapply(x$CriteriaExpression$CriteriaList, as.QueryLoad),
                                                             'ObservationWindow' = as.ObservationWindow(x$CriteriaExpression$ObservationWindow)),
                                      Query = lapply(x$CriteriaExpression, as.QueryLoad),
                                      AdditionalCriteria = lapply(x$CriteriaExpression, as.GroupLoad),
                                      Group = lapply(x$CriteriaExpression, as.GroupLoad),
                                      Count = lapply(x$CriteriaExpression,as.CountLoad),
                                      Attribute = lapply(x$CriteriaExpression, as.AttributeLoad),
                                      InclusionRules = lapply(x$CriteriaExpression, as.ComponentLoad),
                                      EndStrategy = lapply(x$CriteriaExpression, as.EndStrategyLoad),
                                      ConceptSetExpression = list(),
                                      CensoringCriteria = lapply(x$CriteriaExpression, as.QueryLoad))
  }

  return(comp)
}


