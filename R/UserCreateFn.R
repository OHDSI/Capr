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
############################################
########Create Functions v2
###########################################

#' Function creates an Observation Window
#'
#' This function creates an observation window used in a primary criteria. The observation window provides the amount of
#' time before and after the initial event of continuous observation necessary for a person to be eligible to enter the cohort.
#' The minimal observation days would be 0 days of prior observation and 0 days of post observations. This is the default for
#' this function.
#'
#' @param PriorDays number of days prior to the initial event of continuous observation
#' @param PostDays number of days of continous observation after index date
#' @return This function returns a observation window class object providing prior and post days of observation
#' @include LowLevelClasses.R
#' @importFrom methods new
#' @export
createObservationWindow <- function(PriorDays =0L,PostDays =0L){
  new("ObservationWindow",
      PriorDays =as.integer(PriorDays), #convert prior days to integer
      PostDays = as.integer(PostDays)) #conver post days to integer
}


#' Function to initialize a new window object
#'
#' A window depicts the timeline from which events are counted. The window has four components: Start, End, EventStart,
#' and Index Start. First, we determine whether observations are viewed from the start of the event or at the end.
#' By default EventStart is TRUE. Next the start of recording is identied using days and coefficient. The coefficient
#' distinguishes how the days are counted relative to the index date. The end recording is the same as the start,
#' now identifying the end of observation. Finally it is identified whether the index date is relative the start or
#' end of occurrence. A timeline has a start and end window. Usually the end window is not defined. An End Window
#' adds a constraint to the Start Window of a timeline
#'
#' @param StartDays number of days at start of window
#' @param StartCoeff where to begin counting relative to index date: before or after
#' @param EndDays number of days to end window
#' @param EndCoeff where to end counting relative to index date: before or after
#' @param EventStarts if TRUE then this counts from the start of an event otherwise from the end of an event
#' @param IndexStart if TRUE then the index date is the start of event otherwise the end of an event
#' @include LowLevelClasses.R
#' @importFrom methods new
#' @return a new window class object
#' @export
createWindow <- function(StartDays, StartCoeff =c("Before", "After"),
                         EndDays, EndCoeff = c("Before", "After"), EventStarts=TRUE, IndexStart=TRUE){
  StartCoeff <- match.arg(StartCoeff) #match argument options before or after
  EndCoeff <- match.arg(EndCoeff)
  if(StartDays != "All"){
    StartDays <-as.integer(StartDays) #if start days is not all make days integer
  }
  if(EndDays != "All"){
    EndDays <- as.integer(EndDays) #if end days is not all make days integer
  }
  #convert start and end into lists
  start <- list('Days' = StartDays, 'Coeff' =StartCoeff )
  end <- list('Days' =EndDays, 'Coeff' = EndCoeff)
  #create new window object
  new("Window",
      Event = ifelse(EventStarts, "EventStarts", "EventEnds"),
      Start = start,
      End = end,
      Index = ifelse(IndexStart, "IndexStartDate", "IndexEndDate"))
}

#' Set the Timeline in the criteria
#'
#' When a criteria object is initialized a default timeline object is also initialized. To change the timeline object
#' we set it to a new information. Inputs include StartWindow, EndWindow, RestrictVisit, and IgnoreObservationPeriod.
#' The StartWindow and EndWindow inputs require a window class object. A new window can be initialized using the
#' createWindow function.
#'
#' @param StartWindow a window class object that modifies when to begin monitoring for an observation
#' @param EndWindow a window class object that ends the time observing events. This window is not always created so the
#' default is NULL, initializing an empty window
#' @param RestrictVisit a logic toggle where TRUE restricts to the same visit
#' @param IgnoreObservationPeriod a logic toggle where TRUE allows events outside the observation period
#' @include LowLevelClasses.R
#' @importFrom methods new is
#' @return a new Timeline class object
#' @export
createTimeline <- function(StartWindow, EndWindow =NULL, RestrictVisit = FALSE, IgnoreObservationPeriod = FALSE){
  if(is(StartWindow) != "Window"){
    stop("The start window must be of window class") #check if start window is of window class
  }
  #create new timeline object
  tim <- new("Timeline",
             StartWindow = StartWindow,
             RestrictVisit = RestrictVisit,
             IgnoreObservationPeriod = IgnoreObservationPeriod)
  if(!is.null(EndWindow)){
    if(is(EndWindow) != "Window"){ #if the end window is not null it must be of window class
      stop("The end window must be of window class")
    }
    tim@EndWindow <- EndWindow #add end window to class if not null
  }
  return(tim)
}

#' Create Concept Set list
#'
#' This function takes a data frame of OMOP concepts, establishes the mapping logic and bundles them together as
#' a concept set item.
#' With this function, toggling the mapping options sets the logic for all concepts in the concept set expression. If the
#' user wants to set a custom mapping for each concept in the expression the user should use createConceptSetExpressionCustom.
#' This is an evolving function.
#'
#' @param conceptSet a dataframe containing the concepts one would like to add to the concept set. The data frame
#' of concepts can be queried using the lookup concept functions (requires a connection to an OMOP CDM).
#' @param isExcluded logic toggle when true excludes the defined concept when attached to a concept set expression
#' @param includeDescendants logic toggle where default true includes descendant concepts to the defined concept
#' @param includeMapped logic toggle when true includes mapped concepts to the defined concept
#' @return This function returns a concept set item object
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @include LowLevelLoadFn.R
#' @importFrom methods new is
#' @export
createConceptSet <- function(conceptSet, includeDescendants =TRUE,
                                 isExcluded =FALSE, includeMapped = FALSE){
  concepts <- apply(conceptSet, 1, function(x) as.Concept(as.list(x))) #convert data frame into list of concept class obj
  concepts <- unname(concepts) #remove names
  concepts <- lapply(concepts, function(x) { #for each concept in the list set the concept set item
    new("ConceptSetItem", Concept=x,
        isExcluded = isExcluded,
        includeDescendants = includeDescendants,
        includeMapped = includeMapped)
  })
  return(concepts)
}

#' Create Concept Set Expression
#'
#' This function takes a data frame of OMOP concepts, establishes the mapping logic and bundles them together as
#' a concept set expression. A new concept expression created in R sets a guid for the concept id. This unique identifier
#' is used to link the concept set expressions to its implementation within the cohort definition (typically as a query).
#' With this function, toggling the mapping options sets the logic for all concepts in the concept set expression. If the
#' user wants to set a custom mapping for each concept in the expression the user should use createConceptSetExpressionCustom.
#' This is an evolving function.
#'
#' @param conceptSet a dataframe containing the concepts one would like to add to the concept set. The data frame
#' of concepts can be queried using the lookup concept functions (requires a connection to an OMOP CDM).
#' @param Name a name for the concept set expression.
#' @param isExcluded logic toggle when true excludes the defined concept when attached to a concept set expression
#' @param includeDescendants logic toggle where default true includes descendant concepts to the defined concept
#' @param includeMapped logic toggle when true includes mapped concepts to the defined concept
#' @return This function returns a component class object which contains the concept set expression
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @include LowLevelLoadFn.R
#' @importFrom methods new is
#' @export
createConceptSetExpression <- function(conceptSet, Name, includeDescendants =TRUE,
                                        isExcluded =FALSE, includeMapped = FALSE){
  # concepts <- apply(conceptSet,1, function(x) as.Concept(as.list(x))) #convert data frame into list of concept class obj
  # concepts <- unname(concepts) #remove names
  # concepts <- lapply(concepts, function(x) { #for each concept in the list set the concept set item
  #   new("ConceptSetItem", Concept=x,
  #       isExcluded = isExcluded,
  #       includeDescendants = includeDescendants,
  #       includeMapped = includeMapped)
  # })
  concepts <- createConceptSet(conceptSet = conceptSet,
                                   includeDescendants = includeDescendants,
                                   isExcluded = isExcluded,
                                   includeMapped = includeMapped)
  #create the a new concept set expression from the concept set, this also sets the guid concept id
  cse <- new("ConceptSetExpression", Name = Name, Expression = concepts)

  #attach the concept set expressions to a component class object
  comp <- createComponent(Name = Name,
                          ComponentType = "ConceptSetExpression",
                          ConceptSetExpression = list(cse))

  return(comp)
}

#' Function to help user develop the concept mapping
#'
#' This function creates a concept mapping list that is used to establish the concept set item for each
#' member of the concept set expression. This function is evolving.
#'
#' @param n the length of the concept set expression
#' @param includeDescendants a logic vector of length n that contains the toggle for whether the concept should
#' include descendants. If the parameter is left null then will return all FALSE
#' @param isExcluded a logic vector of length n that contains the toggle for whether the concept should
#' be excluded. If the parameter is left null then will return all FALSE
#' @param includeMapped a logic vector of length n that contains the toggle for whether the concept should
#' include mapped concepts. If the parameter is left null then will return all FALSE
#' @return This function returns a list for concept mapping for the concept set expression
#' @export
createConceptMapping <- function(n,
                                 includeDescendants=NULL,
                                 isExcluded=NULL,
                                 includeMapped=NULL){
  cm <- vector('list', length=n) #initialize list
  cm <- lapply(cm, function(x){
    x <- list('includeDescendants' = FALSE,
              'isExcluded' = FALSE,
              'includeMapped' = FALSE)
    return(x)
  }) #make list of objects all false

  if(is.null(includeDescendants)){
    includeDescendants <-rep(FALSE,n) #if null make all false
  }else{
    if(length(includeDescendants) !=n){ #error if not length n
      stop("the includeDescendants vector must match the number of concepts")
    }
  }
  if(is.null(isExcluded)){ #if null make all false
    isExcluded <-rep(FALSE,n)
  }else{
    if(length(isExcluded) !=n){ #error if not length n
      stop("the isExcluded vector must match the number of concepts")
    }
  }
  if(is.null(includeMapped)){ #if null make all false
    includeMapped <-rep(FALSE,n)
  }else{
    if(length(includeMapped) !=n){#error if not length n
      stop("the includeMapped vector must match the number of concepts")
    }
  }
  for(i in seq_along(cm)){ #for each item in list replace item with desired mapping
    cm[[i]]$includeDescendants <- includeDescendants[i]
    cm[[i]]$isExcluded <- isExcluded[i]
    cm[[i]]$includeMapped <- includeMapped[i]
  }
  return(cm)
}

#' Toggle the concept mapping for select positions
#'
#' This functions changes the logical object (TRUE or FALSE) to its other state. This helps toggle the concept
#' mapping for a select set in a large list
#'
#' @param conceptMapping the conceptMapping object
#' @param pos the positions to toggle
#' @param mapping select the mapping type to toggle at each position
#' @return This function returns a list for concept mapping for the concept set expression
#' @export
toggleConceptMapping <- function(conceptMapping,
                                 pos,
                                 mapping = c("includeDescendants",
                                             "isExcluded",
                                             "includeMapped")){
  mapping <- match.arg(mapping)
  for(i in pos){
    conceptMapping[[i]][[mapping]] <- !conceptMapping[[i]][[mapping]]
  }
  return(conceptMapping)
}

#' Create a Custom Concept Set Expression
#'
#' This function takes a data frame of OMOP concepts, establishes the mapping logic and bundles them together as
#' a concept set expression. A new concept expression created in R sets a guid for the concept id. This unique identifier
#' is used to link the concept set expressions to its implementation within the cohort definition (typically as a query).
#' With this function, the user can pre-define a full list of mapping for each concept set item in the concept set expression.
#' This is an evolving function
#'
#' @param conceptSet a dataframe containing the concepts one would like to add to the concept set. The data frame
#' of concepts can be queried using the lookup concept functions (requires a connection to an OMOP CDM).
#' @param Name a name for the concept set expression.
#' @param conceptMapping a list of mapping for each concept set item. The list will contain whether the concept should
#' includeDescendants, isExcluded or includeMapped. If the concept Mapping is left null then by default only the
#' includeDescendants mapping will be true for all. others will remain false.
#' @return This function returns a component class object which contains the concept set expression
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @include LowLevelLoadFn.R
#' @importFrom methods new
#' @export
createConceptSetExpressionCustom <- function(conceptSet, Name, conceptMapping = NULL){

  concepts <- apply(conceptSet,1, function(x) as.Concept(as.list(x)))#convert data frame into list of concept class obj
  concepts <- unname(concepts) #unname object
  if (is.null(conceptMapping)){ #if the conceptMapping is null
    concepts <- lapply(concepts, function(x) {
      new("ConceptSetItem", Concept = x) #for each concept convert into a default ConceptSetItem class
    })#default is includeDescendants =TRUE, isExcluded =FALSE, includeMapped =FALSE
  } else{
    for (i in seq_along(concepts)){#if the conceptMapping is not null
      #set the custome conceptset item for each concept via mapping list
      concepts[[i]] <- new("ConceptSetItem", Concept = concepts[[i]],
                           includeDescendants = conceptMapping[[i]]$includeDescendants,
                           isExcluded = conceptMapping[[i]]$isExcluded,
                           includeMapped = conceptMapping[[i]]$includeMapped)
    }
  }
  #create the a new concept set expression from the concept set, this also sets the guid concept id
  cse <- new("ConceptSetExpression", Name = Name, Expression = concepts)

  #attach the concept set expressions to a component class object
  comp <- createComponent(Name = Name,
                          ComponentType = "ConceptSetExpression",
                          ConceptSetExpression = list(cse))

  return(comp)
}

################
#skip createQuery --> moved to low level create
#Create Query wrappers found in userCreateDomainFn.R
#############################

#' Function creates a count object
#'
#' This function creates a count object of the cohort definition. The count object is used to express a query over
#' a number of occurrences within a timeline relative to the initial event. A count comes from the number of times
#' the applied query must be counted in the candidate patient timeline for them to be a suitable occurrence of a clinical
#' construct.
#' @param Query a component that is of query class
#' @param Logic how to express the count i.e. exactly, at_least, at_most
#' @param Count how many times the query occurs to be eligible
#' @param isDistinct a logic toggle where if TRUE only counts distinct occurrences
#' @param Timeline a timeline class object orienting the time points of recording in reference to the initial event
#' @param Name a character string naming the count object, this is optional so default is null
#' @param Description a character string describing the count object, this is optional so default is null
#' @return This function returns a component class object which contains the count object and attached concept
#' set expressions
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @include LowLevelUtilityFn.R
#' @importFrom methods new
#' @export
createCount <- function(Query, Logic = c("at_least", "at_most", "exactly"), Count,
                        isDistinct=FALSE, Timeline,
                        Name =NULL, Description=NULL){
  if(componentType(Query) != "Query"){
    stop("Query Input must be a Query Class") # if query input is not query class return error
  }
  if(is.null(Name)){ #create a name for the component part when null
  Name <- paste(Query@CriteriaExpression[[1]]@Domain, "Count")
  }
  query <- Query@CriteriaExpression[[1]] #set the query
  cse <- Query@ConceptSetExpression #get the concept set expression for the query
  Logic <-match.arg(Logic) #set logic match to arg
  ct <- new("Count", #create the count class object
            Criteria = query,
            Timeline = Timeline,
            Occurrence = new("Occurrence",
                             Type = Logic,
                             Count = as.integer(Count),
                             isDistinct = isDistinct))

  comp <- createComponent(Name = Name, #place count class object in component container
                          Description = Description,
                          ComponentType = "Count",
                          CriteriaExpression = list(ct),
                          ConceptSetExpression = cse) #attach concept set expressions
  return(comp)
}

#' Function creates a group object
#'
#' This function creates a group object of the cohort definition. The group object binds multiple queries, counts,
#' attributes and other groups to create one component. For entry into the cohort the patient must have a valid instance
#' of all aspects of the group. Groups are used in additional criteria, inclusion rules and correlated criteria. One can
#' attach a list of counts as a criteria list, a list of demographic criteria (select attributes) or a list of sub groups.
#' @param Name a character string naming the group object, this is required for the object. One should make the name
#' descriptive of what the group is trying to identify.
#' @param type a character string expressing the combination of qualifiying criterias for restriction. Valid options are
#' ALL meaning all aspects of the group must be true to enter cohort, ANY meaning at least 1 aspect of the group must
#' be true, AT_LEAST meaning at least a certain count of the group must be true of AT_MOST meaning at most a certain count
#' must be true of the group. The type entry must be in all capital letters
#' @param count the count of criterias needed for restriction. The count only applies if the type if AT_LEAST or AT_MOST.
#' Otherwise this parameter remains NULL
#' @param criteriaList a list of component class count objects to be added. May be left empty, but at least one of
#' criteriaList, demographicCriteriaList and Groups must be filled. The input must be a list of components
#' @param demographicCriteriaList a list of select component class attributes to be added. May be left empty,
#'  but at least one of criteriaList, demographicCriteriaList and Groups must be filled. The input must be a list of components
#' @param Groups a list of component class groups to be added. May be left empty,
#'  but at least one of criteriaList, demographicCriteriaList and Groups must be filled. The input must be a list of components
#' @param Description a character string describing the count object, this is optional so default is null
#' @return This function returns a component class object which contains the group object and attached concept
#' set expressions
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @include LowLevelUtilityFn.R
#' @importFrom methods new
#' @export
createGroup <- function(Name,
                        type = c("ALL", "ANY", "AT_LEAST", "AT_MOST"),
                        count = NULL,
                        criteriaList = NULL,
                        demographicCriteriaList  = NULL,
                        Groups = NULL,
                        Description = NULL){
  et <- new("ExpressionType", Type = type) #create and expression type object
  if(!is.null(count)){
    et@Count <- as.integer(count) #if the count is not null coerce to integer
  }
  #check that at least one slot is full. If all null return error
  if(is.null(criteriaList) & is.null(demographicCriteriaList) & is.null(Groups)){
    stop("At least one slot must be filled")
  }
  #make null list if criteria list is null
  if(is.null(criteriaList)){
    cl <- list()
  } else{ #otherwise extract the counts for each component in the list
    cl <- lapply(criteriaList, function(x) x@CriteriaExpression[[1]])
  }
  if(is.null(demographicCriteriaList)){
    dcl <- list()  #make null list if demographic criteria list is null
  } else{#otherwise extract the attributes for each component in the list
    dcl <- lapply(demographicCriteriaList, function(x) x@CriteriaExpression[[1]])
  }
  if(is.null(Groups)){
    Grps <- list() #make null list if group list is null
  } else{#otherwise extract the groups for each component in the list
    Grps <- lapply(Groups, function(x) x@CriteriaExpression[[1]])
  }
  #create a new group object
  grp <- new("Group", Type = et,
             CriteriaList = cl,
             DemographicCriteriaList = dcl,
             Groups = Grps)

  #get concept set expressions
  cse1 <- append(criteriaList,Groups)
  if(length(cse1) > 0){
    cse <- list()
    for(i in seq_along(cse1)){
      cse <- append(cse, cse1[[i]]@ConceptSetExpression)
    }
    cse <- removeDupCSE(cse)#remove duplicate concept set expressions
  } else{
    cse <- list()
  }
  #create the component containing the group
  comp <- createComponent(Name = Name,
                          Description = Description,
                          ComponentType = "Group",
                          CriteriaExpression = list(grp),
                          ConceptSetExpression = cse)
  return(comp)

}


#' Function creates a date offset end strategy
#'
#'This function creates a date offset end strategy. From the ATLAS page: the event end date is derived from adding
#'a number of days to the event's start or end date. If an offset is added to the event's start date,
#'all cohort episodes will have the same fixed duration (subject to further censoring). If an offset
#'is added to the event's end date, persons in the cohort may have varying cohort duration times due to the
#'varying event durations (such as eras of persistent drug exposure or visit length of stay).
#'This event persistence assures that the cohort end date will be no greater than the selected index event date,
#'plus the days offset.
#' @param offset an integer value specifying padding to the cohort exit.
#' @param eventDateOffset an input only for DateOffset specifying whether to add an offset to the start or end of an event
#' (i.e. StartDate, EndDate)
#' @return This function returns a component class object which contains the end strategy object
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new
#' @export
createDateOffsetEndStrategy <- function(offset, eventDateOffset = c("StartDate", "EndDate")){
  eventDateOffset <- match.arg(eventDateOffset)
  es <- new("DateOffsetEndStrategy",
            DateField = eventDateOffset,
            Offset = as.integer(offset))
  createComponent(Name = "End Strategy Date Offset",
                          ComponentType = "EndStrategy",
                          CriteriaExpression = list(es))
}

#' Function creates an end strategy from a custom era
#'
#'This function creates a custom era end strategy. From the ATLAS page: Specify a concept set
#'that contains one or more drugs. A drug era will be derived from all drug exposure events for
#' any of the drugs within the concept set, using the specified persistence window as a maximum allowable
#' gap in days between successive exposure events and adding a specified surveillance window to the final exposure event.
#'  If no exposure event end date is provided, then an exposure event end date is inferred to be event start
#'  date + days supply in cases when days supply is available or event start date + 1 day otherwise.
#'This event persistence assures that the cohort end date will be no greater than the drug era end date.
#' @param ConceptSetExpression a component of concept set expression class that contains information
#' on the drug concets to use to define the end strategy
#' @param offset an integer value specifying padding to the cohort exit.
#' @param gapDays the maximum allowable days between successive exposures.
#' @return This function returns a component class object which contains the end strategy object
#' @include LowLevelUtilityFn.R
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new
#' @export
createCustomEraEndStrategy <- function(ConceptSetExpression, gapDays,offset){
  check <- componentType(ConceptSetExpression)
  if(check != "ConceptSetExpression"){
    stop("Component Class is not ConceptSetExpression")
  }
  es <- new("CustomEraEndStrategy",
            DrugCodesetId = ConceptSetExpression@ConceptSetExpression[[1]]@id,
            GapDays = as.integer(gapDays),
            Offset = as.integer(offset))
  createComponent(Name = "End Strategy Custom Drug Era",
                          ComponentType = "EndStrategy",
                          CriteriaExpression = list(es),
                          ConceptSetExpression = list(ConceptSetExpression@ConceptSetExpression[[1]]))
}

#######################
#Cohort Definition Components
#######################

#' Function creates a Primary Criteria
#'
#' Function creates a primary criteria from multiple queries. User adds a list of component class queries, identifies the
#' observation window and the criteria limit.
#'
#' @param Name a character string naming the group object, this is required for the object. One should make the name
#' descriptive of what the group is trying to identify.
#' @param ComponentList a list of query components to add to the primary criteria. These components include
#' the queries and concept set expression used in the cohort.
#' @param ObservationWindow an observationWindow class object that set the prior and post days of continuous
#' observation for the initial event
#' @param Limit how to limit initial events per person
#' @param Description a character string describing the count object, this is optional so default is null
#' @return new primary criteria component.
#' @include LowLevelUtilityFn.R
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new is
#' @export
createPrimaryCriteria <- function(Name,
                                   ComponentList,
                                   ObservationWindow = NULL,
                                   Limit,
                                   Description = NULL){
  if(is.null(ObservationWindow)){
    ObservationWindow <- createObservationWindow(PriorDays = 0, PostDays = 0)
  }
  check <- lapply(ComponentList, is)
  if(!all(grepl("Component",check))){
    stop("All inputs in a Component list must be of Component class")
  }
  cl <- lapply(ComponentList, function(x) x@CriteriaExpression[[1]])
  cse <- lapply(ComponentList, function(x) x@ConceptSetExpression[[1]])
  pc <- createComponent(Name = Name,
                                Description = Description,
                                ComponentType = "PrimaryCriteria",
                                CriteriaExpression = list('CriteriaList' = cl,
                                                  'ObservationWindow' = ObservationWindow),
                                Limit = Limit,
                                ConceptSetExpression = cse)
  return(pc)
}

#' Function creates an Additional Criteria
#'
#' Function creates an Additional Criteria from a component class group
#'
#' @param Name a character string naming the group object, this is required for the object. One should make the name
#' descriptive of what the group is trying to identify.
#' @param Contents a single component of group class that describes the additional criteria. If the Contents
#' are empty then the additional criteria is only decribed by the qualified limit
#' @param Limit how to limit initial events per person
#' @param Description a character string describing the count object, this is optional so default is null
#' @return new additional criteria component.
#' @include LowLevelUtilityFn.R
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new is
#' @export
createAdditionalCriteria <- function(Name,
                                      Contents = NULL,
                                      Limit,
                                      Description = NULL){
  if(!is.null(Contents)){
    if(is(Contents) != "Component"){
      stop("All inputs in a Component list must be of Component class")
    }
    ac <- createComponent(Name = Name,
                          Description = Description,
                          ComponentType = "AdditionalCriteria",
                          CriteriaExpression = Contents@CriteriaExpression,
                          Limit = Limit,
                          ConceptSetExpression = Contents@ConceptSetExpression)
  } else {
    ac <- createComponent(Name = Name,
                          Description = Description,
                          ComponentType = "AdditionalCriteria",
                          CriteriaExpression = NULL,
                          Limit = Limit,
                          ConceptSetExpression = NULL)
  }
  return(ac)
}

#' Function creates an Inclusion Rule
#'
#' Function creates a Inclusion Rule from a list of groups, each specifying a unique rule
#' @param Name a character string naming the inclusion rules, this is required for the object. One should make the name
#' descriptive of what the group is trying to identify.
#' @param Contents a list of component class groups to be inserted into the inclusion rules. Each group
#' in the list is a separate rule.
#' @param Limit how to limit initial events per person
#' @param Description a character string describing the count object, this is optional so default is null
#' @return new inclusion rules component.
#' @include LowLevelUtilityFn.R
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new is
#' @export
createInclusionRules <- function(Name, Contents, Limit, Description = NULL){

  check <- lapply(Contents, is)
  if(!all(grepl("Component",check))){
    stop("All inputs in a Component list must be of Component class")
  }

  irs <- createComponent(Name = Name,
                        Description = Description,
                        ComponentType = "InclusionRules",
                        CriteriaExpression = Contents,
                        Limit = Limit)
  return(irs)
}

#' Function creates a Censoring Criteria
#'
#' Function creates a Censoring Criteria from a list of queries
#' @param Name a character string naming the inclusion rules, this is required for the object. One should make the name
#' descriptive of what the group is trying to identify.
#' @param ComponentList a list of component class queries to be inserted into the censoring criteria.
#' @param Description a character string describing the count object, this is optional so default is null
#' @return new censoring criteria component.
#' @include LowLevelUtilityFn.R
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new is
#' @export
createCensoringCriteria <- function(Name, ComponentList, Description = NULL){
  check <- lapply(ComponentList, is)
  if(!all(grepl("Component",check))){
    stop("All inputs in a Component list must be of Component class")
  }
  cl <- lapply(ComponentList, function(x) x@CriteriaExpression[[1]])
  cse <- lapply(ComponentList, function(x) x@ConceptSetExpression[[1]])
  cen <- createComponent(Name = Name,
                         Description = Description,
                         ComponentType = "CensoringCriteria",
                         CriteriaExpression = cl,
                         ConceptSetExpression =  cse)
  return(cen)
}

#' Create a Cohort Era class object
#'
#' The Cohort Era depicts the time span of the cohort. The Censor Window includes
#' the date window for which we register events. The Collapse Settings identify the era padding
#' between events before exiting a cohort.
#'
#' @param EraPadDays a numeric that specifies the number of days for the era padding
#' @param LeftCensorDate a date string that specifies the starting date of registration
#' @param RightCensorDate a date string that specifies the end date of registration
#' @return a cohort era component
#' @include LowLevelUtilityFn.R
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new is
#' @export
createCohortEra <- function(EraPadDays = 0L,
                             LeftCensorDate = NULL,
                             RightCensorDate = NULL){

  CollapseSettings <- new("CollapseSettings", EraPad = EraPadDays)
  CensorWindow <- new("CensorWindow", StartDate = NA_character_, EndDate = NA_character_)


  #2) if left not null and right is null make one entry named.list
  if(!is.null(LeftCensorDate) & is.null(RightCensorDate)){
    CensorWindow@StartDate <- LeftCensorDate
  }

  #3) if left null and right is not null make one entry named.list
  if(is.null(LeftCensorDate) & !is.null(RightCensorDate)){
    CensorWindow@EndDate <- RightCensorDate
  }


  #4) if left not null or right is not null make two entry named.list
  if(!is.null(LeftCensorDate) & !is.null(RightCensorDate)){
    CensorWindow@StartDate <- LeftCensorDate
    CensorWindow@EndDate <- RightCensorDate
  }

  comp <-createComponent(Name = "Cohort Era Details",
                         Description = NULL,
                         ComponentType = "CohortEra",
                         CriteriaExpression = list('CollapseSettings' = CollapseSettings,
                                                   'CensorWindow' = CensorWindow))
  return(comp)

}


#' Create Cohort Definition class object
#'
#' This function creates a Cohort Definition class object from multiple component parts. A cohort definition
#' contains at a minimum a primary criteria class. The cohort definition can further contain a inclusion rules,
#' additional criteria, censoring criteria and end strategy classes to provide more details on cohort restriction
#' and cohort exit. Other components may also be manipulated but since they do not rely on a concept set expressions,
#' they can be manipulated in separate methods. The cohort definition class differs from the circe expression in that
#' it does not have a separate space for concept set expressions, which are bundled within the component.
#'
#' @param Name make a name for the cohort to add to the cohort details
#' @param PrimaryCriteria add primary criteria object
#' @param AdditionalCriteria add additional criteria object. if null then will create an additional criteria with qualified limit
#' @param InclusionRules add inclusion rules object. if null will create empty inclusion rules with expression limit
#' @param EndStrategy add end strategy object. if null will add end of continuous era strategy
#' @param CensoringCriteria add censoring criteria object. if null will add empty censoring criteria
#' @param CohortEra add cohort era object. if null will add collapse settings with 0 day pad and no censor window
#' @param Description add a description detail to cohort details, optional
#' @param Author add an author name to cohort details, optional
#' @param cdmVersionRange add a cdm version range typically >= 5.0.0, please specify if not v5
#' @return cohort definition class object with defined inputs. This can now be compiled into ohdisql and converted to json
#' @include LowLevelUtilityFn.R
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new is
#' @export
#' @export
createCohortDefinition <- function(Name,
                                    Description = NA_character_,
                                    Author = NA_character_,
                                    cdmVersionRange = ">=5.0.0",
                                    PrimaryCriteria,
                                    AdditionalCriteria =NULL,
                                    InclusionRules = NULL,
                                    EndStrategy = NULL,
                                    CensoringCriteria = NULL,
                                    CohortEra = NULL){
  #handle cohort definition meta data
  md <- new("CohortDetails",
            Name = Name,
            Description = Description,
            Author = Author,
            cdmVersionRange = cdmVersionRange)

  cd <- new("CohortDefinition", CohortDetails = md)

  #Handle primaryCriteria
  if(componentType(PrimaryCriteria) != "PrimaryCriteria"){
    stop("The Primary Criteria Component is not a PrimaryCriteria Component class")
  }

  cd@PrimaryCriteria <- PrimaryCriteria

  #Handle Additional Criteria
  if(is.null(AdditionalCriteria)){
    cd@AdditionalCriteria <- createEmptyComponent()
  } else{
    if(componentType(AdditionalCriteria) != "AdditionalCriteria"){
      stop("The Additional Criteria Component is not a AdditionalCriteria Component class")
    }
    cd@AdditionalCriteria <- AdditionalCriteria
  }


  if(is.null(InclusionRules)){
    cd@InclusionRules <- createEmptyComponent()
  } else{
    if(componentType(InclusionRules) != "InclusionRules"){
      stop("The Inclusion Rules Component is not a InclusionRules Component class")
    }
    cd@InclusionRules <- InclusionRules
  }


  if(is.null(EndStrategy)){
    cd@EndStrategy <- createComponent(Name = "End Strategy End of Continuous Observation",
                                      ComponentType = "EndStrategy",
                                      CriteriaExpression = list(new("EndOfCtsObsEndStrategy")))
  } else{
    if(componentType(EndStrategy) != "EndStrategy"){
      stop("The End Strategy Component is not a EndStrategy Component class")
    }
    cd@EndStrategy <- EndStrategy
  }


  if(is.null(CensoringCriteria)){
    cd@CensoringCriteria <- createEmptyComponent()
  } else{
    if(componentType(CensoringCriteria) != "CensoringCriteria"){
      stop("The Censoring Criteria Component is not a CensoringCriteria Component class")
    }
    cd@CensoringCriteria <- CensoringCriteria
  }


  if(is.null(CohortEra)){
    cd@CohortEra <- createCohortEra()
  } else{
    if(componentType(CohortEra) != "CohortEra"){
      stop("The A Cohort Era Component is not a CohortEraComponent class")
    }
    cd@CohortEra  <- CohortEra
  }

  return(cd)
}
