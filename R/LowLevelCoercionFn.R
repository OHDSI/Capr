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
###############
#Coersion Functions
################

#as.Circe ( CAPR --> CIRCE)
setGeneric('as.Circe',function(x){standardGeneric("as.Circe")})
######----Base Structs-----##############

#' Coersive function from S4 to S3
#'
#' To serialize between json and R, an S3 list object is required. CAPR creates an organized s4 object that maintains
#' components of the cohort definition. CIRCE needs to be in an S3 structure in R before serializing to json. These
#' functions maintain consistency between the s3 and s4 data structures
#'
#' @param x a component class object in s4
#' @return the object converted back to s3 that can be used for json seralization
#' @rdname as.Circe-method
#' @aliases as.Circe
#' @aliases as.Circe,Window-method
setMethod('as.Circe',
          'Window',
          function(x){
            win <- list('Start' = x@Start,
                        'End' = x@End,
                        'UseEventEnd' = x@Event,
                        'UseIndexEnd' = x@Index)
            if(win$Start$Days == "All") {
              win$Start$Days <- NULL
            }

            if(win$End$Days == "All") {
              win$End$Days <- NULL
            }

            if(win$Start$Coeff == "Before") {
              win$Start$Coeff <- -1L
            }
            if(win$End$Coeff == "Before") {
              win$End$Coeff <- -1L
            }
            if(win$Start$Coeff == "After") {
              win$Start$Coeff <- 1L
            }
            if(win$End$Coeff == "After") {
              win$End$Coeff <- 1L
            }
            win$UseEventEnd <- ifelse(win$UseEventEnd == "EventStarts", FALSE, TRUE)
            win$UseIndexEnd <- ifelse(win$UseIndexEnd == "IndexStartDate", FALSE, TRUE)

            return(win)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,Timeline-method
setMethod('as.Circe', 'Timeline',
          function(x) {

            if(length(x@EndWindow@Event)==0) {
              ew <- NA_character_
            }else{
              ew <- as.Circe(x@EndWindow)
            }

            if(length(x@StartWindow@Event)==0) {
              sw <- NA_character_
            }else{
              sw <- as.Circe(x@StartWindow)
            }
            tl <- list('StartWindow' = sw,
                       'EndWindow' = ew,
                       'RestrictVisit' = x@RestrictVisit,
                       'IgnoreObservationPeriod' = x@IgnoreObservationPeriod)

            if(length(tl$StartWindow) == 1) {
              tl$StartWindow <-NULL
            }
            if(length(tl$EndWindow) == 1) {
              tl$EndWindow <- NULL
            }
            if(!tl$RestrictVisit) {
              tl$RestrictVisit <- NULL #null out RestrictVisit if FALSE
            }
            if(!tl$IgnoreObservationPeriod) {
              tl$IgnoreObservationPeriod <- NULL #null out if IgnoreObservationPeriod is FALSE
            }
            return(tl)
          })
#' @rdname as.Circe-method
#' @aliases as.Circe,Occurrence-method
setMethod("as.Circe", "Occurrence",
          function(x) {
            occ <- list('Type' = x@Type,
                        'Count' = x@Count,
                        'IsDistinct' = x@isDistinct) #create occurrence
            if(!occ$IsDistinct) {
              occ$IsDistinct <- NULL #null out isDistinct if FALSE
            }

            occ$Type <- ifelse(occ$Type == "at_least", 2L,
                               ifelse(occ$Type == "at_most",1L,0L))
            return(occ)
          })
#' @rdname as.Circe-method
#' @aliases as.Circe,ObservationWindow-method
setMethod('as.Circe', 'ObservationWindow',
          function(x) {
            ow <- list(PriorDays = x@PriorDays,
                       PostDays = x@PostDays)
            return(ow)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,Limit-method
setMethod('as.Circe', 'Limit',
          function(x) {
             ll <- list('Type' = x@Type)
            return(ll)
          })
#' @rdname as.Circe-method
#' @aliases as.Circe,ExpressionType-method
setMethod('as.Circe', 'ExpressionType',
          function(x) {
            ll <- list('Type' = x@Type,
                       'Count' = x@Count)
            if(is.na(ll$Count)) {
              ll$Count <- NULL
            }
            return(ll)
          })
######----Concepts-----##############
#' @rdname as.Circe-method
#' @aliases as.Circe,Concept-method
setMethod("as.Circe", "Concept",
          function(x) {
            con <- list('CONCEPT_ID' = x@CONCEPT_ID,
                        'CONCEPT_NAME' = x@CONCEPT_NAME,
                        'STANDARD_CONCEPT'= x@STANDARD_CONCEPT,
                        'STANDARD_CONCEPT_CAPTION' = x@STANDARD_CONCEPT_CAPTION,
                        'INVALID_REASON' = x@INVALID_REASON,
                        'INVALID_REASON_CAPTION' = x@INVALID_REASON_CAPTION,
                        'CONCEPT_CODE' = x@CONCEPT_CODE,
                        'DOMAIN_ID' = x@DOMAIN_ID,
                        'VOCABULARY_ID' = x@VOCABULARY_ID,
                        'CONCEPT_CLASS_ID' = x@CONCEPT_CLASS_ID)
            return(con)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,ConceptSetItem-method
setMethod("as.Circe", "ConceptSetItem",
          function(x) {
            cr <- list('concept' = as.Circe(x@Concept),
                       'isExcluded' = x@isExcluded,
                       'includeDescendants' = x@includeDescendants,
                       'includeMapped' = x@includeMapped)
            if(!cr$isExcluded) {
              cr$isExcluded <- NULL
            }
            if(!cr$includeDescendants) {
              cr$includeDescendants <- NULL
            }
            if(!cr$includeMapped) {
              cr$includeMapped <- NULL
            }
            return(cr)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,ConceptSetExpression-method
setMethod("as.Circe", "ConceptSetExpression",
          function(x) {
            cse <- list('id'= x@id,
                        'name' = x@Name,
                        'expression'= list('items' = lapply(x@Expression, as.Circe)))

            return(cse)

          })

######----Attributes-----##############
#' @rdname as.Circe-method
#' @aliases as.Circe,OpAttribute-method
setMethod('as.Circe', 'OpAttribute',
          function(x) {
            if (x@Op != "bt" | x@Op != "!bt") {
              att <- list(list('Value' = x@Contents$Value,
                               'Op' = x@Op))
            } else{
              att <- list(list('Value' = x@Contents$Value,
                               'Extent' = x@Contents$Extent,
                               'Op' = x@Op))
            }
            names(att) <- x@Name
            return(att)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,SourceConceptAttribute-method
setMethod('as.Circe', "SourceConceptAttribute",
          function(x) {
            att <- x@SourceCodesetId
            att <- ifelse(nchar(att) == 36, as.character(att), as.integer(att))
            names(att) <- x@Name
            return(att)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,ConceptAttribute-method
setMethod('as.Circe', "ConceptAttribute",
          function(x) {
            att <- list(x@Concepts)
            names(att) <- x@Name
            return(att)
          })
#' @rdname as.Circe-method
#' @aliases as.Circe,LogicAttribute-method
setMethod('as.Circe', "LogicAttribute",
          function(x) {
            att <- x@Logic
            names(att) <- x@Name
            return(att)
          })


#' @rdname as.Circe-method
#' @aliases as.Circe,CorrelatedCriteriaAttribute-method
setMethod('as.Circe', "CorrelatedCriteriaAttribute",
          function(x){
            grp <- as.Circe(x@Group)
            att <- list(grp)
            names(att) <- x@Name
            return(att)
          })

######----Circe Structs-----##############
#' @rdname as.Circe-method
#' @aliases as.Circe,QueryAttribute-method
setMethod('as.Circe', 'Query',
          function(x){
            cid <- x@CodesetId
            cid <- ifelse(nchar(cid) == 36, as.character(cid), as.integer(cid))
            qq <- list('CodesetId' = cid)
            if(length(x@Attributes) > 0) {
              for(i in 1:length(x@Attributes)) {
                qq <- append(qq, as.Circe(x@Attributes[[i]]))
              }
            }
            if(is.na(x@CodesetId)) { #if codeset id has 0 length null out
              qq$CodesetId <-NULL
            }
            query <- list(qq)

            names(query) <- x@Domain
            return(query)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,CountAttribute-method
setMethod('as.Circe',"Count",
          function(x){
            crit <- list('Criteria'=as.Circe(x@Criteria))
            cc<- append(crit,as.Circe(x@Timeline))
            cc <- append(cc,list('Occurrence' =as.Circe(x@Occurrence)))
            return(cc)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,GroupAttribute-method
setMethod('as.Circe', "Group",
          function(x){
            cl <- lapply(x@CriteriaList,as.Circe)
            cl <- unname(cl)
            if(length(x@DemographicCriteriaList) > 0 ){
              dcl <- lapply(x@DemographicCriteriaList, as.Circe)
            } else{
              dcl <- list()
            }
            if(length(x@Groups) >0 ){
              gg <- lapply(x@Groups, as.Circe)
            }else{
              gg<-list()
            }
            tt <- as.Circe(x@Type)

            grp <- list('CriteriaList' = cl,
                        'DemographicCriteriaList' = dcl,
                        'Groups' = gg)
            grp <- append(tt, grp)
            # if(length(x@Type@Count)==0){
            #   grp$Count <- NULL
            # }
            return(grp)
          })


######----Additional Cohort Pieces-----##############
#' @rdname as.Circe-method
#' @aliases as.Circe,DateOffsetEndStrategy-method
setMethod("as.Circe", "DateOffsetEndStrategy",
          function(x){
            es <- list('DateOffset' = list('DateField' = x@DateField,
                                                   'Offset' = x@Offset))
            return(es)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,CustomEraEndStrategy-method
setMethod("as.Circe", "CustomEraEndStrategy",
          function(x){
            cid <- x@DrugCodesetId
            cid <-ifelse(nchar(cid)==36, as.character(cid), as.integer(cid))
            es <- list('CustomEra' = list('DrugCodesetId' = cid,
                                          'GapDays' = x@GapDays,
                                           'Offset' = x@Offset))
            return(es)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,CollapseSettings-method
setMethod("as.Circe", "CollapseSettings",
          function(x){
            col <- list('CollapseType' = x@CollapseType,
                        'EraPad' = x@EraPad)
            return(col)
          })

#' @rdname as.Circe-method
#' @aliases as.Circe,CensorWindow-method
setMethod("as.Circe", "CensorWindow",
          function(x){
            cw <- list('StartDate' = x@StartDate,
                       'EndDate' = x@EndDate)
            if(is.na(x@StartDate)){
              cw$StartDate <- NULL
            }
            if(is.na(x@EndDate)){
              cw$EndDate <- NULL
            }
            return(cw)
          })

######----Component-----##############

#' Convert Primary Criteria Component to CIRCE
#'
#' @param x the component to convert
#' @include LowLevelUtilityFn.R
#' @return a circe converted component
convertPrimaryCriteriaToCIRCE <- function(x){
  check <- componentType(x)
  if(check != "PrimaryCriteria"){
    stop("The Component is not a Primary Criteria. Cannot coerce")
  }
  pc <- list('CriteriaList' = lapply(x@CriteriaExpression$CriteriaList, as.Circe),
             'ObservationWindow' = as.Circe(x@CriteriaExpression$ObservationWindow),
             'PrimaryCriteriaLimit' = as.Circe(x@Limit$PrimaryCriteriaLimit))
  return(pc)
}

#' Convert Additional Criteria Component to CIRCE
#'
#' @param x the component to convert
#' @include LowLevelUtilityFn.R
#' @return a circe converted component
convertAdditionalCriteriaToCIRCE <- function(x){
  check <- componentType(x)
  if(check != "AdditionalCriteria"){
    stop("The Component is not a Additonal Criteria. Cannot coerce")
  }
  if(length(x@CriteriaExpression)>0){
    ac <- list('AdditionalCriteria' =as.Circe(x@CriteriaExpression[[1]]))
  } else{
    ac <- list('AdditionalCriteria' = list())
  }
  lim <- list('QualifiedLimit' =as.Circe(x@Limit$QualifiedLimit))
  ac <- append(ac, lim)
  return(ac)
}

#' Convert single rule (group) Component to CIRCE
#'
#' @param x the component to convert
#' @include LowLevelUtilityFn.R
#' @return a circe converted component
convertRuleToCIRCE <- function(x){
  check <- componentType(x)
  if(check != "Group"){
    stop("The Component is not a Group. Cannot coerce")
  }
  rule <- list('name' = x@MetaData@Name,
               'description' = x@MetaData@Description,
               'expression' = as.Circe(x@CriteriaExpression[[1]]))
  if(is.na(rule$description)){
    rule$description <- NULL
  }
  return(rule)
}

#' Convert Inclusion Rules Component to CIRCE
#'
#' @param x the component to convert
#' @include LowLevelUtilityFn.R
#' @return a circe converted component
convertInclusionRulesToCIRCE <- function(x){
  check <- componentType(x)
  if(check != "InclusionRules"){
    stop("The Component is not a InclusionRules. Cannot coerce")
  }
  lim <- list('ExpressionLimit' =as.Circe(x@Limit$ExpressionLimit))
  if(length(x@CriteriaExpression)>0){
    irs <- list('InclusionRules' =lapply(x@CriteriaExpression,convertRuleToCIRCE))
  } else{
    irs <- list('InclusionRules' = list())
  }
  irs <- append(lim, irs)
  return(irs)
}

#' Convert End Strategy Component to CIRCE
#'
#' @param x the component to convert
#' @include LowLevelUtilityFn.R
#' @return a circe converted component
convertEndStrategyToCIRCE <- function(x){
  check <- componentType(x)
  if(check != "EndStrategy"){
    stop("The Component is not a EndStrategy. Cannot coerce")
  }
  esType <- is(x@CriteriaExpression[[1]])
  if(esType == "EndOfCtsObsEndStrategy"){
    es <- list()
  }else{
    es <- as.Circe(x@CriteriaExpression[[1]])
  }
  return(es)
}

#' Convert Censoring Criteria Component to CIRCE
#'
#' @param x the component to convert
#' @include LowLevelUtilityFn.R
#' @return a circe converted component
convertCensoringCriteriaToCIRCE <- function(x){
  check <- componentType(x)
  if(check != "CensoringCriteria"){
    stop("The Component is not a Censoring Criteria. Cannot coerce")
  }
  cen <- lapply(x@CriteriaExpression,as.Circe)
  return(cen)
}

#' Convert CohortEra Component to CIRCE
#'
#' @param x the component to convert
#' @include LowLevelUtilityFn.R
#' @return a circe converted component
convertCohortEraToCIRCE <- function(x){
  check <- componentType(x)
  if(check != "CohortEra"){
    stop("The Component is not a CohortEra. Cannot coerce")
  }
  cs <- list('CollapseSettings' =as.Circe(x@CriteriaExpression$CollapseSettings))
  cw <- list('CensorWindow' =as.Circe(x@CriteriaExpression$CensorWindow))
  ce <- c(cs, cw)
  return(ce)
}

#' @rdname as.Circe-method
#' @aliases as.Circe,Component-method
setMethod("as.Circe", "Component",
          function(x){
            class <- componentType(x)
            comp <- switch(class,
                           Empty = list(),
                           PrimaryCriteria = convertPrimaryCriteriaToCIRCE(x),
                           AdditionalCriteria = convertAdditionalCriteriaToCIRCE(x),
                           InclusionRules = convertInclusionRulesToCIRCE(x),
                           EndStrategy = convertEndStrategyToCIRCE(x),
                           CensoringCriteria = convertCensoringCriteriaToCIRCE(x),
                           CohortEra = convertCohortEraToCIRCE(x))
            return(comp)
          })

##############------------Change CodesetId using merge table--------------#####

setGeneric("UpdateCirceCodesetId",function(x, conceptTable){standardGeneric("UpdateCirceCodesetId")})

#' Change CodesetId to Integer
#'
#' When creating the circe json object, an internal reference system needs to be established for the concept set expressions.
#' This function will update the concept ids from its guid to the ordering of the ids in a merge table. The codeset Ids
#' will be integers starting from 0 in the circe instance.
#'
#' @param x a component class object in s4
#' @param conceptTable a merge table to match guid to codeset id integer
#' @return an object with updated codeset id
#' @rdname UpdateCirceCodesetId-method
#' @aliases UpdateCirceCodesetId
#' @aliases UpdateCirceCodesetId,SourceConceptAttribute-method
setMethod("UpdateCirceCodesetId", "SourceConceptAttribute",
          function(x,conceptTable){
            x@SourceCodesetId <- as.character(merge(x@SourceCodesetId,conceptTable,by.x="x", by.y="id")$index)
            return(x)
          })

#' @rdname UpdateCirceCodesetId-method
#' @aliases UpdateCirceCodesetId,Query-method
setMethod("UpdateCirceCodesetId", "Query",
          function(x,conceptTable){
            x@CodesetId <- as.character(merge(x@CodesetId,conceptTable,by.x="x", by.y="id")$index)
            if(length(x@CodesetId) ==0){
              x@CodesetId <- NA_character_
            }
            if(length(x@Attributes)>0){
              for(j in seq_along(x@Attributes)){
                if(is(x@Attributes[[j]]) == "SourceConceptAttribute"){
                  x@Attributes[[j]] <- UpdateCirceCodesetId(x@Attributes[[j]], conceptTable = conceptTable)
                }
                if(is(x@Attributes[[j]]) == "CorrelatedCriteriaAttribute"){
                  x@Attributes[[j]]@Group <- UpdateCirceCodesetId(x@Attributes[[j]]@Group, conceptTable = conceptTable)
                }
              }
            }
            return(x)
          })

#' @rdname UpdateCirceCodesetId-method
#' @aliases UpdateCirceCodesetId,Count-method
setMethod("UpdateCirceCodesetId", "Count",
          function(x,conceptTable){
            x@Criteria <- UpdateCirceCodesetId(x@Criteria, conceptTable)
            return(x)
          })
#' @rdname UpdateCirceCodesetId-method
#' @aliases UpdateCirceCodesetId,Group-method
setMethod("UpdateCirceCodesetId", "Group",
          function(x,conceptTable){
            x@CriteriaList <- lapply(x@CriteriaList, UpdateCirceCodesetId, conceptTable = conceptTable)
            x@Groups <- lapply(x@Groups, UpdateCirceCodesetId, conceptTable = conceptTable)
            return(x)
          })

#' @rdname UpdateCirceCodesetId-method
#' @aliases UpdateCirceCodesetId,CustomEraEndStrategy-method
setMethod("UpdateCirceCodesetId", "CustomEraEndStrategy",
          function(x,conceptTable){
            x@DrugCodesetId <- as.character(merge(x@DrugCodesetId,conceptTable,by.x="x", by.y="id")$index)
            return(x)
          })

#' Update codeset id for inclusion rule
#'
#' @param x the group that need to update codeset Ids
#' @param conceptTable a merge table to match guid to codeset id integer
#' @return an object with updated codeset id
UpdateCodesetIdRule <- function(x,conceptTable){
  check <- componentType(x)
  if(check != "Group"){
    stop("The Component is not a Group. Cannot coerce")
  }
  x@CriteriaExpression[[1]] <- UpdateCirceCodesetId(x@CriteriaExpression[[1]], conceptTable = conceptTable)
  return(x)
}
######----Update adn convert function-----##############

#' A function to update codeset Ids and convert to circe
#'
#' @param x the object to update and convert
#' @param conceptTable a merge table to match guid to codeset id integer
#' @return an object with updated codeset id
UpdateAndConvert <- function(x, conceptTable){
  check <- componentType(x)
  if(check == "PrimaryCriteria"){
    x@CriteriaExpression$CriteriaList <- lapply(x@CriteriaExpression$CriteriaList,
                                                UpdateCirceCodesetId, conceptTable = conceptTable)
    comp <- as.Circe(x)
  }
  if(check == "AdditionalCriteria"){
    if(length(x@CriteriaExpression) > 0){
      x@CriteriaExpression[[1]] <- UpdateCirceCodesetId(x@CriteriaExpression[[1]], conceptTable = conceptTable)
    }
    comp <- as.Circe(x)
  }
  if(check == "InclusionRules"){
    if(length(x@CriteriaExpression) >0){
      x@CriteriaExpression<- lapply(x@CriteriaExpression,
                                    UpdateCodesetIdRule, conceptTable = conceptTable)
    }
    comp <- as.Circe(x)
  }
  if(check == "EndStrategy"){
    esType <- is(x@CriteriaExpression[[1]])
    if(esType == "CustomEraEndStrategy"){
      x@CriteriaExpression[[1]] <- UpdateCirceCodesetId(x@CriteriaExpression[[1]], conceptTable = conceptTable)
    }
    comp <- as.Circe(x)
  }
  if(check == "CensoringCriteria"){
    x@CriteriaExpression<- lapply(x@CriteriaExpression,
                                  UpdateCirceCodesetId, conceptTable = conceptTable)
    comp <- as.Circe(x)
  }
  if(check == "CohortEra"){
    comp <- as.Circe(x)
  }

  if(check == "Empty"){
    comp <- as.Circe(x)
  }
  return(comp)
}


######----Convert Cohort Definition to CIRCE-----##############
#' Function to update cohort definition to CIRCE
#'
#' @param x the cohort definition to convert to circe
#' @return a circe object in R
convertCohortDefinitionToCIRCE <- function(x){
  if(is(x) != "CohortDefinition"){
    stop("Must be of Cohort Definition Class")
  }

  #Extract concept set expressions from all possible locations
  cse <- list()
  for(i in c("PrimaryCriteria", "AdditionalCriteria","InclusionRules", "EndStrategy", "CensoringCriteria")){
    y <- slot(x, i)
    if(i == "InclusionRules"){
      for(j in seq_along(y@CriteriaExpression)){
        cse <- append(cse, getConceptSetExpression(y@CriteriaExpression[[j]]))
      }
    }else{
      cse <- append(cse, getConceptSetExpression(y))
    }
  }
  #remove duplicate concept set expressions
  cse <- removeDupCSE(cse)
  cse <- lapply(cse, as.Circe)
  conceptTable <- data.frame('id' = sapply(cse,"[[", "id"),
                             'index' = as.integer(seq_along(sapply(cse,"[[", "id")) -1),
                             stringsAsFactors = FALSE)
  for (i in seq_along(cse)){
    cse[[i]]$id <- merge(cse[[i]]$id, conceptTable, by.x = "x", by.y = "id")$index
  }
  #Update AC and IRS do first so can manage empty lists
  ac1 <- UpdateAndConvert(x@AdditionalCriteria, conceptTable = conceptTable) #update and convert ac
  if (length(ac1) == 0){#if the previous function reutrns a list length zero
    qualLimit <- list('Type' = "First") #default qualified limit to First
    ac <- list()
  } else{ #ow
    ac <- ac1$AdditionalCriteria #update ac to be additional criteria part of update
    qualLimit <- ac1$QualifiedLimit #get qualified limit
  }
  irs1 <- UpdateAndConvert(x@InclusionRules, conceptTable = conceptTable) #update and convert irs
  if(length(irs1) == 0){#if previous function returns a list length zero
    expLimit <- list('Type' = "First") #default expression limit to first
    irs <- list()
  } else{
    irs <- irs1$InclusionRules#extract inclusion rules
    expLimit <- irs1$ExpressionLimit #extract expression limit
  }
  #place created objects or update and convert
  cd <- list('ConceptSets' = cse,
             'PrimaryCriteria' = UpdateAndConvert(x@PrimaryCriteria, conceptTable = conceptTable),
             'AdditionalCriteria'= ac,
             'QualifiedLimit' = qualLimit,
             'ExpressionLimit' =expLimit,
             'InclusionRules' = irs,
             'EndStrategy' = UpdateAndConvert(x@EndStrategy, conceptTable = conceptTable),
             'CensoringCriteria' = UpdateAndConvert(x@CensoringCriteria, conceptTable = conceptTable),
             'CollapseSettings' = UpdateAndConvert(x@CohortEra, conceptTable = conceptTable)$CollapseSettings,
             'CensorWindow' = UpdateAndConvert(x@CohortEra, conceptTable = conceptTable)$CensorWindow,
             'cdmVersionRange' = x@CohortDetails@cdmVersionRange)
  if(length(cd$AdditionalCriteria) == 0){#if the additional criteria has an empty list
    cd$AdditionalCriteria <- NULL #remove from list
  }
  if(length(cd$EndStrategy) == 0){#if the additional criteria has an empty list
    cd$EndStrategy <- NULL #remove from list
  }
  return(cd)
}

