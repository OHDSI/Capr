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
#' Get concept sets from cohort expression and prepare R language
#'
#' This function takes the concept sets from the circe cohort definition
#' and generates R functions to create them in the R environment. The data
#' saved is R language to generate the objects. They are evaluated separately
#'
#' @param x the circe cohort definition
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 expr sym !!
#' @importFrom magrittr %>%
#' @return r language to generate the concept set expressions of the cohort
getConceptSetCall <- function(x){
  cs <- x$ConceptSets
  `%notin%` <- Negate("%in%") #negate the %in% function
  #get concept ids from concept set expressions
  cid <- purrr::map(purrr::map(cs, function(x) x$expression$items),
                    function(y) purrr::map_int(y, function(z) z$concept$CONCEPT_ID))
  #get names from concept set expressions
  nm <- purrr::map(cs, function(x) x$name)
  #get mapping from concept set exrpessions (i.e includeDescendants, isExcluded)
  #if there is no mapping the function will return a NULL
  mapping <- purrr::map(purrr::map(cs, function(x) x$expression$items),
                        function(y) purrr::map(y, function(z) names(z)[names(z) %notin% "concept"]))
  #create a function to check if each type of mapping is available in CAPR.
  #if it is check the list for this input as true
  getMappings <- function(x){
    list('includeDescendants' = "includeDescendants" %in% x,
         'isExcluded' = "isExcluded" %in% x,
         'includeMapped' = "includeMapped" %in% x)
  }
  #run the get mappings function
  mapping <- purrr::map(mapping, function(x) purrr::map(x,getMappings))
  #create the R language saves of the assignment functions that were inputed
  #convert each concept set id vector into an assignment function
  cidLang <- purrr::map2(seq_along(cid),cid, ~rlang::call2("<-",rlang::sym(paste0("cid",.x-1)),.y))
  #convert each name into an assignment function
  nmLang <- purrr::map2(seq_along(nm),nm, ~rlang::call2("<-",rlang::sym(paste0("nm",.x-1)),.y))
  #convert each mapping into an assignment funciton
  mappingLang <- purrr::map2(seq_along(mapping),mapping, ~rlang::call2("<-",rlang::sym(paste0("conceptMapping",.x-1)),.y))

  #create an empty vector to store the concept set langs
  conceptSetsLang <- vector('list', length = length(cs))
  for (i in seq_along(conceptSetsLang)) { #for each item in the list do
    #create the object call for each of the assignment functions previously created
    cid <- rlang::sym(paste0("cid",i - 1)) #set the cid object
    nm <- rlang::sym(paste0("nm",i - 1)) #set the naming objects
    mapping <- rlang::sym(paste0("conceptMapping",i - 1)) #set the mapping objects
    #create a temporary expression for the function. bang bang (!!) the object calls into the fexpression
    tmp <- rlang::expr(lookupConceptIds(conceptIds = !!cid, mapToStandard = FALSE) %>%
                  createConceptSetExpressionCustom(Name = !!nm,conceptMapping = !!mapping))
    #create the assignments for each of the conecept Sets
    conceptSetsLang[[i]] <- rlang::call2("<-", rlang::sym(paste0("conceptSet",i -1)), tmp)
  }

  conceptSetsLang <- list(cidLang, nmLang, mappingLang, conceptSetsLang)
  return(conceptSetsLang)
}


#' Get attributes from cohort expression and prepare R language
#'
#' This function creates attributes within the queries and turns them into
#' R language which will then create them as a CAPR object
#'
#' @param x the circe cohort definition
#' @param objNm the naming convention to assign the object
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 expr sym !!
#' @include LowLevelUtilityFn.R
#' @return r language to generate the concept set expressions of the cohort
createAttributeCall <- function(x,objNm){
  AttributeOptions <- list('Op' = c("Age", "OccurrenceStartDate", "OccurrenceEndDate", "AgeAtEnd",
                                   "AgeAtStart", "PeriodLength", "ValueAsNumber", "RangeLow",
                                   "RangeHigh", "RangeLowRatio", "RangeHighRatio",
                                   "EraStartDate", "EraEndDate", "OccurrenceCount",
                                   "EraLength", "Refills", "Quantity", "DaysSupply",
                                   "EffectiveDrugDose", "VisitLength"),
                           'Concept' = c("ConditionType", "Gender", "VisitType",
                                         "DrugType", "RouteConcepts", "DoseUnit",
                                         "ProviderSepcialty", "PlaceOfService",
                                         "ProcedureType", "Modifier", "ObservationType",
                                         "ValueAsConcept", "Qualifier", "Unit",
                                         "MeasurementType", "Operator", "DeathType",
                                         "DeviceType"),
                           'Logical' = c("First", "DrugTypeExclude", "ConditionTypeExclude",
                                        "VisitTypeExclude", "ProcedureTypeExclude",
                                        "ObservationTypeExclude", "MeasurementTypeExclude",
                                        "Abnormal", "DeathTypeExclude", "DeviceTypeExclude"),
                           'SourceConcept' = c("VisitSourceConcept","DrugSourceConcept",
                                               "ConditionSourceConcept", "ProcedureSourceConcept",
                                               "ObservationSourceConcept", "MeasurementSourceConcept",
                                               "DeathSourceConcept", "DeviceSourceConcept"),
                           'TextFilter' = c("ValueAsString", "StopReason", "UniqueDeviceId"),
                           'CorrelatedCriteria' = c("CorrelatedCriteria"))
  nm <- paste0("create",names(x),"Attribute") #paste all the Attribute Options with create to call the wrappers
  jj <- vector('list',length(nm)) #initialize a list of attributes
  for (i in seq_along(jj)) { #start for loop
    if (any(AttributeOptions$Op %in% names(x)[i])){ #if the ith name is in the Op options do
      jj[[i]] <- rlang::call2(nm[i], Op=x[[i]]$Op, Value = x[[i]]$Value, Extent = x[[i]]$Extent)
      jj[[i]] <- rlang::call2("<-", rlang::sym(paste0("att",objNm,"_",i)), jj[[i]])
      #create the Op attribute call wrapper
      next
    }
    if (any(AttributeOptions$Logical %in% names(x)[i])){ #if the ith name is in the Logical options do
      jj[[i]] <- rlang::call2(nm[i], logic=x[[i]]) #create the logical attribute call wapper
      jj[[i]] <- rlang::call2("<-", rlang::sym(paste0("att",objNm,"_",i)), jj[[i]])
      next
    }
    if (any(AttributeOptions$SourceConcept %in% names(x)[i])){ #if the ith name is in the SourceConcept options do
      jj[[i]] <- rlang::call2(nm[i], ConceptSetExpression=rlang::sym(paste0("conceptSet",x[[i]]))) #create the logical attribute call wapper
      jj[[i]] <- rlang::call2("<-", rlang::sym(paste0("att",objNm,"_",i)), jj[[i]])
      next
    }
    if (any(AttributeOptions$Concept %in% names(x)[i])){ #if the ith name is in the Concept options do
      conceptIds <- sapply(x[[i]], function(x) getElement(x, "CONCEPT_ID")) #get all concept Ids
      jj[[i]] <- rlang::call2(nm[i], conceptIds=conceptIds, mapToStandard = rlang::expr(FALSE)) #create the concept attribute call wapper
      jj[[i]] <- rlang::call2("<-", rlang::sym(paste0("att",objNm,"_",i)), jj[[i]])
      next
    }
    if (any(AttributeOptions$CorrelatedCriteria %in% names(x)[i])){ #if the ith name is in the Concept options do
      grpLang <- createGroupCall(x[[i]], "CorrelatedCriteria")
      ccCall <- rlang::call2(nm[i], Group = rlang::sym("CorrelatedCriteria"))
      ccAssign <- rlang::call2("<-", rlang::sym(paste0("att",objNm,"_",i)), ccCall)
      jj[[i]] <- list('GroupLang' = grpLang, 'CorrelatedCriteriaLang' = ccAssign)
      next
    }
  }
  return(jj)
}

#' Get queries from cohort expression and prepare R language
#'
#' This function creates queries and turns them into
#' R language which will then create them as a CAPR object
#'
#' @param x the circe cohort definition
#' @param nm the naming convention to assign the object
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 sym !!! syms
#' @include LowLevelUtilityFn.R
#' @return r language to generate the concept set expressions of the cohort
createQueryCall <- function(x,nm){
  domainFunction <- paste0("create", names(x)) #get domain name turn to domainFunction wrapper for createQuery
  check <- any("CodesetId" == names(x[[1]])) #check if codesetId is in names
  if (check){ #if TRUE
    conceptSetObj <- rlang::sym(paste0("conceptSet", x[[1]]$CodesetId))# make object name conceptSet number from id
  } else{
    conceptSetObj <- NULL #otherwise make the object null, no concept set id. There is an attribute in the query
  }
  attributeList <- x[[1]] #get remaining slots which would be attributes
  attributeList$CodesetId <- NULL #null out the codeset id if its there
  if (length(attributeList) ==0){
    attributeList <- NULL
    attLang <- NULL
    queryCall <- rlang::call2(domainFunction,#create the query call input create wrapped
                       conceptSetExpression = conceptSetObj, #input conceptSetObj
                       attributeList = attLang)  #include the attribute list
    queryCall <- rlang::call2("<-", rlang::sym(paste0("query", nm)), queryCall)
    rr <-list('Query' = queryCall)
  } else{
    attributeList <- createAttributeCall(attributeList,nm) #run createAttributeCall for the list of attributes
    attLang <- rlang::syms(paste0("att", nm, "_",seq_along(attributeList))) #create list of objects assigned in call
    #create an expression listing the listed elements, append this to the attribute List
    attributeList <- append(attributeList, rlang::call2("<-", rlang::sym(paste0("attrList", nm)), rlang::expr(list(!!!attLang))))
    #attLang <- list(rlang::sym(paste0("att",nm,"_",seq_along(attributeList))))
    queryCall <- rlang::call2(domainFunction,#create the query call input create wrapped
                       conceptSetExpression = conceptSetObj, #input conceptSetObj
                       attributeList = rlang::sym(paste0("attrList", nm)))  #include the attribute list
    queryCall <- rlang::call2("<-", rlang::sym(paste0("query", nm)), queryCall) #create the query Call
    rr <-list('AttributeList' =attributeList,
              'Query' = queryCall) #create a list of objects to save
  }
  return(rr)#return the query call
}

#' Function to create a window object call
#'
#' @param x the circe cohort definition
#' @importFrom rlang expr !!
#' @return r language to generate the windows of the cohort
createWindowCall <- function(x){
  StartDays <- ifelse(is.null(x$Start$Days), "All", as.integer(x$Start$Days)) #grab start days if null make All else make int
  StartCoeff <- ifelse(x$Start$Coeff == 1, "After", "Before") #grab start coeff if 1 make After else before
  EndDays <- ifelse(is.null(x$End$Days), "All", as.integer(x$End$Days)) #grab end days if null make All else make int
  EndCoeff <- ifelse(x$End$Coeff == 1, "After", "Before") #grab end coeff if 1 make After else before
  EventStarts <- !ifelse(is.null(x$UseEventEnd), FALSE, x$UseEventEnd) #grab use event end if null make false else use toggle, then reverse
  IndexStart <- !ifelse(is.null(x$UseIndexEnd), FALSE, x$UseIndexEnd) #grab use index end if null make false else use toggle, then reverse

  #create the expression for createWindow
  winLang <- rlang::expr(createWindow(StartDays = !!StartDays, StartCoeff = !!StartCoeff, #input start params using bang bang(!!)
                                  EndDays = !!EndDays, EndCoeff = !!EndCoeff, #input end params
                                  EventStarts = !!EventStarts, IndexStart = !!IndexStart)) #input timeline reference toggle
  return(winLang)
}

#' Function to create a timeline call
#'
#' @param x the circe cohort definition
#' @param objectName the naming convention to assign the object
#' @importFrom rlang call2 sym
#' @return r language to generate the timelines of the cohort
createTimelineCall <- function(x,objectName){
  StartWindow <- createWindowCall(x$StartWindow) #Create the start window call
  if (!is.null(x$EndWindow)){#if the end window is in list
    EndWindow <- createWindowCall(x$EndWindow) #create the end window call
  } else{ #ow make null
    EndWindow <- NULL
  }
  if (!is.null(x$IgnoreObservationPeriod)){# if ignore observation period is in list
    IgnoreObservationPeriod <- x$IgnoreObservationPeriod #place it in variable
  } else{ #else make new var with default false
    IgnoreObservationPeriod <- FALSE
  }
  if (!is.null(x$RestrictVisit)){ #if restrict visit is in list
    RestrictVisit <- x$RestrictVisit #place it in variable
  } else{#else make new var with default false
    RestrictVisit <- FALSE
  }
  #create the timeline calle
  tlCall <- rlang::call2("createTimeline", StartWindow = StartWindow, #add inputs
                         EndWindow = EndWindow, RestrictVisit = RestrictVisit,
                         IgnoreObservationPeriod = IgnoreObservationPeriod)
  rlang::call2("<-", rlang::sym(paste0("timeline",objectName)), tlCall) #return call assignment with objectName
}

#' Get counts from cohort expression and prepare R language
#'
#' @param x the circe cohort definition
#' @param nm the naming convention to assign the object
#' @importFrom rlang call2 sym
#' @return r language to generate the counts of the cohort
createCountCall <- function(x, nm){
  queryLang <-createQueryCall(x$Criteria,nm) #get query language from Criteria for assignment
  tlLang <- createTimelineCall(x,nm) #get timeline language from object for assignment
  #get logiv if 2 then at_least if 1 then at_most ow exactly
  Logic <- ifelse(x$Occurrence$Type ==2, "at_least", ifelse(x$Occurrence$Type ==1, "at_most", "exactly"))
  #get count and convert to integer
  Count <- as.integer(x$Occurrence$Count)
  if (!is.null(x$IsDistinct)){##if isDistinct is not null
    IsDistinct <- x$IsDistinct #then assign it
  } else{#else create default of false
    IsDistinct <- FALSE
  }
  #create the createCount function with inputs
  countLang <- rlang::call2("createCount", Query = rlang::sym(paste0("query", nm)),#use the assignment name for query
                            Logic = Logic, #add the logic
                            Count = Count, #add the count input
                            isDistinct = IsDistinct, #add the isDistinct input
                            Timeline = rlang::sym(paste0("timeline", nm))) #use the assingment for timeline

  countLang <- rlang::call2("<-", rlang::sym(paste0("count", nm)), countLang) #create the assignment
  #return list of objects
  rr <- list('QueryLanguage' = queryLang,#add query language to list
             'TimelineLanguage' = tlLang, #add timeline language to list
             'CountLanguage' = countLang) # add count language to list
  return(rr) #return a list of all objects used to construct
}

#' Get groups from cohort expression and prepare R language
#'
#' This function creates groups from cohort and turns them into
#' R language which will then create them as a CAPR objects
#'
#' @param x the circe cohort definition
#' @param nm the naming convention for sub-objects
#' @param assignName the naming convention to assign the object
#' @importFrom purrr map2
#' @importFrom rlang call2 expr sym !!! syms
#' @return r language to generate the groups of the cohort
createGroupCall <- function(x,nm,assignName =NULL){
  if (is.null(assignName)){
    assignName <- nm
  }
  type <- x$Type # get type as input
  if (!is.null(x$Count)){ #if count item is not null then make as integer
    count <- as.integer(x$Count)
  }else{ #else keep null
    count <- NULL
  }
  #extract information and create Count calls in the criteria list (purrr::map2 used for 2 lists)
  if (length(x$CriteriaList) >0){
    criteriaListLang <- purrr::map2(x$CriteriaList, #extract pieces from criteria list
                                    seq_along(x$CriteriaList), #make indicies for objs created
                                    ~createCountCall(x =.x, nm= paste0(assignName,"_",.y))) #make the count call for all in list
    critNm <-rlang::syms(paste0("count", assignName, "_",seq_along(criteriaListLang)))#list objects created by count call functions
    critNm <- rlang::expr(list(!!!critNm)) #create an expression listing the list of oject names
  } else{
    criteriaListLang <- NULL
    critNm <- NULL
  }

  #extract information and create Attributes for demographic criterias
  if (length(x$DemographicCriteriaList) > 0){
    dclLang <- purrr::map2(x$DemographicCriteriaList, #extract pieces from demographic criteria list
                           seq_along(x$DemographicCriteriaList), #make indicies for objs created
                           ~createAttributeCall(x =.x, objNm= paste0("DemCrit_",assignName,"_",.y))) #make the attribute call for all in list
    dclNm <- rlang::syms(paste0("attDemCrit_", assignName, "_",seq_along(dclLang),"_1"))#list objects created by attribute call functions
    dclNm <- rlang::expr(list(!!!dclNm)) #create an expression listing the list of oject names
  } else{
    dclLang <- NULL
    dclNm <- NULL
  }
  #extract information and create Groups (recursively) for groups
  if (length(x$Groups) > 0){
    grpLang <- purrr::map2(x$Groups, #extract pieces from groups list
                           seq_along(x$Groups), #make indicies for objs created
                           ~createGroupCall(x =.x, nm= paste0("Grp_",assignName,"_",.y))) #make the group call for all in list
    grpNm <- rlang::syms(paste0("Grp_", assignName, "_",seq_along(grpLang)))#list objects created by group call functions
    grpNm <- rlang::expr(list(!!!grpNm))#create an expression listing the list of oject names
  } else{
    grpLang <- NULL
    grpNm <- NULL
  }
  #make the createGroup function with inputs
  GroupLang <- rlang::call2("createGroup", #identify the create group function call
                            Name = nm,#give the group a name
                            type = type, #add the type
                            count = count, #add the count input even if NULL
                            criteriaList = critNm, #input crit obj names made from assignment
                            demographicCriteriaList = dclNm, #input dcl obj names made from assignment
                            Groups = grpNm) #input group obj names made from assignment

  GroupLang <- rlang::call2("<-", rlang::sym(assignName), GroupLang) #create the assignment
  #return list of objects
  rr <- list('CriteriaListLanguage' = criteriaListLang,#add crit language to list
             'DemographicCriteriaListLanguage' = dclLang, #add dcl language to list
             'SubGroupsLanguage' = grpLang, #add sub group language to list
             'GroupLanguage' = GroupLang) # add group language to list
  rr <- Filter(Negate(is.null), rr)
  return(rr) #return a list of all objects used to construct
}



#' Get primary criteria from cohort expression and prepare R language
#'
#' @param x the circe cohort definition
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 expr sym !!! syms
#' @return r language to generate the primary criteria of the cohort
getPCCall <- function(x){
  pc <- x$PrimaryCriteria
  queryLang <- purrr::map2(pc$CriteriaList, #use the criteriaList to get queries
                           seq_along(pc$CriteriaList), #make and index for naming
                           ~createQueryCall(x = .x,nm = paste0("PC",.y))) #create query calls
  qName <-rlang::syms(paste0("queryPC",seq_along(queryLang)))#list objects created by query call functions
  qName <- rlang::expr(list(!!!qName)) #create an expression listing the list of oject names
  PriorDays <- pc$ObservationWindow$PriorDays #extract prior days from list
  PostDays <- pc$ObservationWindow$PostDays #extract post days from list
  owLang <- rlang::call2("createObservationWindow", #create function call for createObservation window
                         PriorDays = PriorDays, #use prior days input
                         PostDays = PostDays) #use post days input
  NameforComponent <- "cohortPrimaryCriteria"
  pcLang <- rlang::call2("createPrimaryCriteria", #make calle for primary criteria function
                  Name = NameforComponent, #add name for component
                  ComponentList =qName, #add list of query object calls
                  ObservationWindow = owLang, #add the call for createObservation window
                  Limit = pc$PrimaryCriteriaLimit$Type) #add symbol for Limit type
  pcLang <- rlang::call2("<-", rlang::sym("PrimaryCriteria"), pcLang) #convert this into an assignment call
  rr <- list('PCQueryLanguage'=queryLang, 'PCLanguage' = pcLang)#return list of queries needed to build PC and the PC
  return(rr)
}

#' Get additional criteria from cohort expression and prepare R language
#'
#' @param x the circe cohort definition
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 expr sym !!!
#' @return r language to generate the additional criteria of the cohort
getACCall <- function(x){
  if (!is.null(x$AdditionalCriteria)){#if the additional criteria is not null
    grpLang <- createGroupCall(x$AdditionalCriteria, nm = "AC") #make the group call
    acNm <- rlang::sym("AC") #make a symbol of the assigned object
  } else{
    grpLang <- NULL #if no Additional Criteria in list than make both null
    acNm <- NULL
  }
  qualLimit <- x$QualifiedLimit$Type #extract that qualified limit
  NameforComponent <-"cohortAdditionalCriteria" #make a name for the component
  acLang <- rlang::call2("createAdditionalCriteria", #make the createAdditionalCriteriaCall
                  Name =NameforComponent, #place the name
                  Contents = acNm, #symbol for the  assigned object
                  Limit = qualLimit) #add the qualified limit

  acLang <- rlang::call2("<-", rlang::sym("AdditionalCriteria"), acLang) #convert the create into an assignment
  rr <- list('ACGroupLanguage' = grpLang, 'ACLanguage' = acLang) #create a list of all things needed to build objects
  return(rr)
}


#' Get inclusion rules from cohort expression and prepare R language
#'
#' @param x the circe cohort definition
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 expr sym !!! syms
#' @return r language to generate the inclusion rules of the cohort
getIRSCall <- function(x){
  if (length(x$InclusionRules) > 0){ #if inclusion rules list is greater than 0
    grpLang <- purrr::map2(x$InclusionRules,seq_along(x$InclusionRules), #create the group call for each inclusion rule
                           ~createGroupCall(x=.x$expression, nm=.x$name, assignName = paste0("InclusionRule",.y)))
    irsNm <- rlang::syms(paste0("InclusionRule", seq_along(x$InclusionRules))) #make the assingment names
    irsNm <- rlang::expr(list(!!!irsNm)) #create an expression listing the list of oject names
  } else{
    grpLang <- NULL #if no inclusion rules exist then make null
    irsNm <- NULL
  }
  expLimit <- x$ExpressionLimit$Type #extract the expression limit
  NameforComponent <-"cohortInclusionRules" #make a name for the inclusion rules
  irsLang <- rlang::call2("createInclusionRules", # make the create Inclusion Rules call
                  Name =NameforComponent, #add the name for the component
                  Contents = irsNm, #add the list of inclusion rules used to build the object
                  Limit = expLimit) #include the expression limit

  irsLang <- rlang::call2("<-", rlang::sym("InclusionRules"), irsLang) #convert create to assignment
  rr <- list('IRGroupLanguage' = grpLang, 'IRLanguage' = irsLang) #list all things used to create all objects
  return(rr)
}

#' Get end strategy from cohort expression and prepare R language
#'
#' @param x the circe cohort definition
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 expr sym !!!
#' @return r language to generate the end strategy of the cohort
getESCall <- function(x){
  if (!is.null(x$EndStrategy)){ #if end strategy is present in cohort definition list
    es <- x$EndStrategy #extract the end strategy
    esFnName <- paste0("create",names(es), "EndStrategy") #create the function call using the name of the end strategy
    if (names(es) == "DateOffset"){ #if the end strategy is a date offset make this particular create funciton
      esLang <- rlang::call2(esFnName, #use the function call name
                             offset = es$DateOffset$Offset, #add the offset input
                             eventDateOffset = es$DateOffset$DateField) #add the date field input
    }
    if (names(es) == "CustomEra"){#if the end strategy is a custom era make this particular function call
      esLang <- rlang::call2(esFnName, #input the function call name, place the concept set expression associated with drug cid
                             ConceptSetExpression =rlang::sym(paste0("conceptSet", es$CustomEra$DrugCodesetId)),
                             gapDays = es$CustomEra$GapDays,#place gap days
                             offset = es$CustomEra$Offset) #place offset
    }
    esLang <- rlang::call2("<-", rlang::sym("EndStrategy"), esLang) #take function call and construct assignment
  } else{
    esLang <-NULL #if endstrategy does not exist then make this a null (will use end of continuous observation)
  }

  rr <- list('ESLanguage' = esLang)
  return(rr) #return call language for creating end strategy
}

#' Get censoring criteria from cohort expression and prepare R language
#'
#' @param x the circe cohort definition
#' @importFrom purrr map map_int map2
#' @importFrom rlang call2 expr sym !!! syms
#' @return r language to generate the censoring criteria of the cohort
getCenCall <- function(x){
  cen <- x$CensoringCriteria
  if (length(cen) >0){
    queryLang <- purrr::map2(cen, #use the list of censoring criterias to get queries
                             seq_along(cen), #make and index for naming
                             ~createQueryCall(x = .x,nm = paste0("Cen",.y))) #create query calls
    qName <-rlang::syms(paste0("queryPC",seq_along(queryLang)))#list objects created by query call functions
    qName <- rlang::expr(list(!!!qName)) #create an expression listing the list of oject names
    NameforComponent <- "cohortCensoringCriteria"
    cenLang <- rlang::call2("createCensoringCriteria", #make calle for censoring criteria function
                    Name = NameforComponent, #add name for component
                    ComponentList =qName) #add list of query object calls
    cenLang <- rlang::call2("<-", rlang::sym("CensoringCriteria"), cenLang) #convert this into an assignment call
    rr <- list('CenQueryLanguage'=queryLang, 'CenLanguage' = cenLang)#return list of queries needed to build PC and the PC
  } else{
    rr <- NULL
  }
  return(rr)
}

#' Get cohort era from cohort expression and prepare R language
#'
#' @param x the circe cohort definition
#' @importFrom rlang call2 sym
#' @return r language to generate the cohort era of the cohort
getCohortEraCall <- function(x){
  EraPadDays <- as.integer(x$CollapseSettings$EraPad)
  LeftCensorDate <- x$CensorWindow$StartDate
  RightCensorDate <- x$CensorWindow$EndDate

  cohortEraLang <- rlang::call2("createCohortEra",
                                EraPadDays = EraPadDays,
                                LeftCensorDate = LeftCensorDate,
                                RightCensorDate = RightCensorDate)
  cohortEraLang <- rlang::call2("<-", rlang::sym("CohortEra"), cohortEraLang)
  rr <- list('CohortEraLanguage' = cohortEraLang)
  return(rr)
}

#' Get call to build cohort definition
#'
#' This function generates the cohort definition call and the
#' R language calls needed to build the lower level objects for the cohort definition
#'
#' @param x the circe cohort definition
#' @param nm the naming convention to assign the object
#' @importFrom rlang call2 sym
#' @include LowLevelUtilityFn.R
#' @return r language to generate the cohort
getCohortDefinitionCall <- function(x, nm =NULL){
  if (is.null(nm)){
    nm <- "CohortDefinition"
  }
  cdLang <- list('ConceptSets' = unlist(getConceptSetCall(x), use.names = FALSE),
             'PrimaryCriteria' = unlist(getPCCall(x), use.names = FALSE),
             'AdditionalCriteria' = unlist(getACCall(x), use.names = FALSE),
             'InclusionRules' = unlist(getIRSCall(x), use.names = FALSE),
             'EndStrategy' = unlist(getESCall(x), use.names = FALSE),
             'CensoringCriteria' = unlist(getCenCall(x), use.names = FALSE),
             'CohortEra' = unlist(getCohortEraCall(x), use.names = FALSE))
  cdNm <- vector('list', length(cdLang))
  names(cdNm) <- names(cdLang)
  for(i in seq_along(cdLang)){
    if (is.null(cdLang[[i]])){
      next
    } else{
      cdNm[[i]] <- rlang::sym(names(cdLang)[i])
    }
  }
  cohortCall <- rlang::call2("createCohortDefinition",
                             Name = nm,
                             cdmVersionRange = x$cdmVersionRange,
                             PrimaryCriteria = cdNm$PrimaryCriteria,
                             AdditionalCriteria = cdNm$AdditionalCriteria,
                             InclusionRules = cdNm$InclusionRules,
                             EndStrategy = cdNm$EndStrategy,
                             CensoringCriteria = cdNm$CensoringCriteria,
                             CohortEra = cdNm$CohortEra)
  cohortCall <- rlang::call2("<-", rlang::sym(nm), cohortCall)
  rr <- list('createCDCall' = cdLang, 'CohortCall' = cohortCall)
  return(rr)
}



# cs <- getConceptSetCall(cohort1$ConceptSets)
# pc <- createPCCall(cohort1)
# ac <- createACCall(cohort1)
# ir <- createIRSCall(cohort1)
# cd <-unlist(list(cs,pc,ac,ir), use.names = FALSE)
# sink("~/Documents/testCAPRCallOutput.txt")
# for(i in seq_along(cd)){
#   print(cd[[i]])
# }
# sink()
# #
# pcLang <- call2("createPrimaryCriteria2",ComponentList = queryLang,
#                 call2("createObservationWindow", PriorDays=0L, PostDays =0L),
#                 Limit = sym("All"))
#
# pcLang <- expr(createPrimaryCriteria2(Name = "PrimaryCriteria",
#                                       ComponentList = list(query1),
#                                       ObservationWindow = createObservationWindow(PriorDays = 0L, PostDays = 0L),
#                                       Limit = "All"))
# str(eval(pcLang))
