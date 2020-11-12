##############
#New classes for CAPR

#' An S4 class for Meta Data
#'
#' A class for meta data, info about component structure
#'
#' @slot ComponentClass name of component class (this is formally defined)
#' @slot Name name for component customized by user
#' @slot Description description of the component
#' @slot Index A character string either IndexStartDate or IndexEndDate Identifies where the index is relative to the window
setClass('MetaData',
         slots = c(ComponentClass = 'character',
                   Name = 'character',
                   Description = 'character'))


#' An S4 class for a Window
#'
#'A window class provides details on the end points of the timeline
#'
#' @slot Event a character string either EventStarts or EventEnds. Identifies the point of reference for the window
#' @slot Start a list containing the days and coefficient for the start of the window
#' @slot End A list containing the days and coefficient for the end of the window
#' @slot Index A character string either IndexStartDate or IndexEndDate Identifies where the index is relative to the window
setClass("Window",
         slots=c(Event = 'character',
                 Start = 'list',
                 End = 'list',
                 Index = 'character'))

#' An S4 class for Timeline
#'
#'The timeline class provides context to when the criteria must be observed in a person timeline to pretain to the expression
#'
#' @slot StartWindow a window class object identifying the start window
#' @slot EndWindow a window class object ifentifying the end window (optional)
#' @slot RestrictVisit a logic toggle where TRUE restricts to the same visit
#' @slot IgnoreObservationPeriod a logic toggle where TRUE allows events outside the observation period
setClass("Timeline",
         slots=c(StartWindow = "Window",
                 EndWindow = "Window",
                 RestrictVisit = 'logical',
                 IgnoreObservationPeriod = 'logical'))

#' An S4 class for Occurrence
#'
#' The Occurrence class provides logic on the number of criterias that most be true in a person for them to be contained in
#' the expression
#'
#' @slot Type a character string of either at most, at least, or exactly providing context to the number of occurrences
#' @slot Count an integer value that provides the number of occurrences
#' @slot isDistinct a logic toggle where if TRUE only counts distinct occurrences
setClass("Occurrence", #a class counting the number of occurrences of the event
         slots=c(Type = 'character',
                 Count = 'integer',
                 isDistinct = 'logical'))

#' An S4 class for Expression type
#'
#' An expression type quantifies the number of criteria's needed to set as restriction. Types include:
#' All, Any, at least and at most. If the expression type is at least or at most a count is required
#' to express the type
#'
#' @slot Type boolean operator for the number of items in group to include. all, any, at most and at least
#' @slot Count the number of criteria's needed for restriction. If Type is ALL or ANY this value is NA
setClass("ExpressionType",
         slots = c(Type = "character",
                   Count = "integer"))

#' Initialization function for s4 classes
#'
#' A basic structure to initialize objects
#'
#' @param .Object an object to initialize
#' @param Type default ALL expressions
#' @param Count NA_integer
#' @importFrom methods callNextMethod
#' @return an initial expressionType object
setMethod("initialize", "ExpressionType",
          function(.Object,
                   Type = "ALL",
                   Count = NA_integer_){
            .Object <- callNextMethod()
            .Object@Type<- Type
            .Object@Count <- Count
            .Object
          })

#' An S4 class for ObservationWindow
#'
#' A class designating an amount of time necessary for an initial event to be recorded
#'
#' @slot PriorDays minimal amount of time before event for it to be recorded
#' @slot PostDays minimal amount of time after an event for it to be recorded
setClass("ObservationWindow",#
         slots =c(PriorDays = "integer", #
                  PostDays = "integer")) #

#' An S4 class for Limit
#'
#' A class designating a limit of events per person Types include: all first last
#'
#' @slot Type how to limit events per person: all, first, or last
setClass('Limit',
         slots = c(Type = "character"))

#' An S4 class for a Concepet
#'
#' A concept class contains all the information about the concept from the OMOP voabulary
#'
#' @slot CONCEPT_ID the id of the concept
#' @slot CONCEPT_NAME the name of the concept
#' @slot STANDARD_CONCEPT whether the cncept is standard, single letter
#' @slot STANDARD_CONCEPT_CAPTION whether the concept is standard full phrase
#' @slot INVALID_REASON Whether the concept is invalid single letter
#' @slot INVALID_REASON_CAPTION whether the concept is invalid standard phrase
#' @slot CONCEPT_CODE the original code of the concept from its vocabulary
#' @slot DOMAIN_ID the domain of the concept
#' @slot VOCABULARY_ID the name of the vocabulary
#' @slot CONCEPT_CLASS_ID type of concept class
setClass("Concept",#a class that designates a concept
         slots = c(CONCEPT_ID = "integer",
                   CONCEPT_NAME = "character",
                   STANDARD_CONCEPT = "character",
                   STANDARD_CONCEPT_CAPTION = "character",
                   INVALID_REASON = "character",
                   INVALID_REASON_CAPTION = "character",
                   CONCEPT_CODE = "character",
                   DOMAIN_ID ="character",
                   VOCABULARY_ID = "character",
                   CONCEPT_CLASS_ID = "character"))

#' An S4 class for ConceptSetItem
#'
#' a class that provides information on the mapping of the concept
#'
#' @slot Concept a concept class object
#' @slot isExcluded toggle if want to exclude the concept
#' @slot includeDescendants toggle if want to include descendants
#' @slot includeMapped toggle if want to include map
setClass("ConceptSetItem",
         slots=c(Concept = "Concept",
                 isExcluded = "logical",
                 includeDescendants ="logical",
                 includeMapped = "logical"))


#' Initialization function for s4 conceptSetItem
#'
#' A basic structure to initialize conceptSetItem
#'
#' @param .Object an object to initialize
#' @param Concept new concept class
#' @param isExcluded default FALSE
#' @param includeDescendants default TRUE
#' @param includeMapped default FALS
#' @importFrom methods callNextMethod
#' @return an initial ConceptSetItem object
setMethod("initialize", "ConceptSetItem",
          function(.Object,
                   Concept = new("Concept"),
                   isExcluded =FALSE,
                   includeDescendants = TRUE,
                   includeMapped = FALSE){
            .Object <- callNextMethod()
            .Object@Concept<- Concept
            .Object@isExcluded<- isExcluded
            .Object@includeDescendants <- includeDescendants
            .Object@includeMapped <- includeMapped
            .Object
          })

#' An S4 class for ConceptSetExpresion
#'
#' A class for the concept set expressions bundles multiple concepts with mapping
#'
#' @slot id an id for the concept set expression to identify within a component
#' @slot Name the name of the concept set expression
#' @slot Expression a list containing expressions. expressions include multiple conceptSetItem
setClass("ConceptSetExpression",
         slots= c(id = "character",
                  Name = "character",
                  Expression = "list"))


#' Initialization function for s4 conceptSetExpression
#'
#' A basic structure to initialize conceptSetExpression. start an id with a guid, no name and an empty list
#'
#' @param .Object an object to initialize
#' @param id generate a guid for the new concept set expression instance
#' @param Name empty character name
#' @param Expression empty list
#' @importFrom methods callNextMethod
#' @importFrom uuid UUIDgenerate
#' @return an initial ConceptSetExpression object with a guid
setMethod("initialize", "ConceptSetExpression",
          function(.Object,
                   id = uuid::UUIDgenerate(),
                   Name = NA_character_,
                   Expression = list()){
            .Object <- callNextMethod()
            .Object@id<- id
            .Object@Name<- Name
            .Object@Expression <- Expression
            .Object
          })

#CIRCE strucs

#' An S4 class for Group
#'
#' A group that bundles criterias together identifying an event
#'
#' @slot Type a expression type class boolean for the number of items to make the group count
#' @slot CriteriaList a list of items (counts and queries) that would identify a medical event
#' @slot DemographicCriteriaList a list of demographic attributes that could identify a population
#' @slot Groups a list of other groups that are contained within a group
setClass("Group", #
         slots = c(Type = 'ExpressionType',
                   CriteriaList = 'list',
                   DemographicCriteriaList = 'list',
                   Groups = 'list'))

#' An S4 class for a Query
#'
#' A query is a medical concept that can be extracted from a database through a where statement.
#'  This would include concepts
#'
#' @slot Domain the domain where the concepts can be found
#' @slot CodesetId the id that matches the concept set expression
#' @slot Attributes a list of attributes that modify the query with more information
setClass("Query", #
         slots = c(Domain = "character",
                   CodesetId = "character",#
                   Attributes = "list"))#


#' Initialization function for s4 Query
#'
#' A basic structure to initialize a query with anull domain, codesetid and empty list
#'
#' @param .Object an object to initialize
#' @param Domain NA character string
#' @param CodesetId NA character string
#' @param Attributes null list
#' @importFrom methods callNextMethod
#' @return an initial ConceptSetItem object
setMethod("initialize", "Query",
          function(.Object,
                   Domain = NA_character_,
                   CodesetId = NA_character_,
                   Attributes = list()){
            .Object <- callNextMethod()
            .Object@Domain <- Domain
            .Object@CodesetId <- CodesetId
            .Object@Attributes <- Attributes
            .Object
          })

#' An S4 class for a Count
#'
#' A count class provides a number of occurrences of the query and the timeline that it happens
#'
#' @slot Criteria a query class object
#' @slot Timeline a timeline class object
#' @slot Occurrence an occurrence class object
setClass("Count",
         slots =c(Criteria = "Query",
                  Timeline = "Timeline",
                  Occurrence = "Occurrence"))


#Attributes

#' An S4 class for an Op Attribute
#'
#' An operator attribute meaning it has some value with a boolean operator
#'
#' @slot Name the name of the attribute
#' @slot Op the operator gt,lt,gte,lte,eq,neq,bt,!bt
#' @slot Contents the contents of the attribute as a list. includes the value and the extent
setClass("OpAttribute",
         slots =c(Name = "character",
                  Op = "character",
                  Contents= "list"))

#' An S4 class for SourceConceptAttribute
#'
#' An attribute that looks at utilizing the source concepts instead of standard concepts
#'
#' @slot Name name of the attribute
#' @slot SourceCodesetId a source concept id, conection to concept set expression
setClass("SourceConceptAttribute",
         slots=c(Name = "character",#
                 SourceCodesetId = "character"))#

#' An S4 class for Concept Attribute
#'
#' A concept attribute, using concepts to identify the attribute like a gender or race etc
#'
#' @slot Name the name of the attribute
#' @slot Concepts a list containing the concepts used to identify the attribute
setClass("ConceptAttribute",
         slots = c(Name = "character",
                   Concepts = "list"))


#' An S4 class for CorrelatedCriteriaAttribue
#'
#' A group attribute that is nested within a query.
#'
#' @slot Name name of the attribute
#' @slot Group a group class object for the attribute
setClass("CorrelatedCriteriaAttribute",#
         slots = c(Name = "character",#
                   Group = "Group"))#

#' An S4 class for Logic Attribute
#'
#' This class creates a logic attribute which says either true or false if the name of the attribute
#' is maintained
#'
#' @slot Name a name of the attribute
#' @slot Logic TRUE or FALSE for this attribute
setClass("LogicAttribute",
         slots = c(Name = "character",
                   Logic = "logical"))#




#Container for save state. has info about the object, whether it contains information for a criteria, concepts, and a limit
#' An S4 class for Component
#'
#' This class is an flexible container to store information about the cohort definition, allowing us to maintain information
#' in smaller parts that remain relevant in isolation. The structure of circe cohort definition relies on a concept set
#' table that stores information for queries. In each cohort component an internal reference id is used to maintain
#' consistency between the expression of the cohort criteria and the actionable concepts. The component container
#' bundles the concept set expression and the criteria expression into one object that is saveable and inheritable.
#' Smaller classes are stored within the container and when they are converted into a superior class the component container
#' is modified but the previous information is kept in tact. A component consists of 4 parts: meta data which stores
#' the name, description and the componentClass. The componentClass identifies what kind of component one is using. Next
#' the criteriaExpression stores any information about the deployment of the medical concept. This includes queries, counts,
#' groups, attributes and other structures that detail the information of the specific component class. The limit
#' is a section that specifies the limit of entry for person events. Is it the first event, all events or last event for
#' the criteriaExpression we are interested in observing. Finally the concept set expression holds the concepts relevant
#' to the criteria expression. The component can be saved as a json file or loaded back into its s4 class.
#'
#' @slot MetaData meta information about the object
#' @slot CriteriaExpression a list of criteria that is in the object
#' @slot Limit a list containing any limits
#' @slot ConceptSetExpression a list containing any concept sets
setClass('Component',#a component class objec is a container of information
         slots =c(MetaData = 'MetaData',#
                  CriteriaExpression = "list",#
                  Limit = "list",#
                  ConceptSetExpression = "list"))#


#Additional Cohort Pieces
#End Strategy

#' An S4 class for DateOffsetEndStrategy
#'
#' An end strategy class specifying a number of days from the start or end of the initial event until
#' cohort exit
#'
#' @slot DateField a character string specifying either the StartDate or EndDate of the initial event to begin counting
#' days until cohort exit
#' @slot Offset an integer value specifying padding to the cohort exit.
setClass("DateOffsetEndStrategy",#an end strategy class
         slots=c(DateField = "character",#
                 Offset = "integer"))

#' An S4 class for CustomEraEndStrategy
#'
#' An end strategy class specifying the time until the end of drug use for cohort exit
#'
#' @slot DrugCodesetId the guid of the drug concept set expression to activate in the end strategy
#' @slot GapDays an integer showing the maximum allowable days between successive exposures.
#' @slot Offset an integer value specifying padding to the cohort exit.
setClass("CustomEraEndStrategy",
         slot = c(DrugCodesetId = "character",
                  GapDays = "integer",
                  Offset = "integer"))

#' An S4 class for EndOfCtsObsEndStrategy
#'
#' When the end strategy is not defined the cohort exit is done based on the end of continuous observation.
#' This class is an end strategy type.
#'
#' @slot EndOfContinuousObservation set as true for end strategy option
setClass("EndOfCtsObsEndStrategy",
         slot=c(EndOfContinuousObservation = "logical"))


#' Initialization function for s4 "EndOfCtsObsEndStrategy
#'
#' @param .Object an object to initialize
#' @param EndOfContinuousObservation set TRUE
#' @importFrom methods callNextMethod
#' @return an initial end strategy object
setMethod("initialize", "EndOfCtsObsEndStrategy",
          function(.Object,
                   EndOfContinuousObservation = TRUE){
            .Object <- callNextMethod()
            .Object@EndOfContinuousObservation<- EndOfContinuousObservation
            .Object
          })

#' An S4 class for Collapse Settings
#'
#' A class providing information that identifies the padding for cohort eras
#'
#' @slot Type boolean operator for the number of items in group to include. all, any, at most and at least
#' @slot Count the number of criteria's needed for restriction. If Type is ALL or ANY this value is NA
setClass("CollapseSettings",
         slots = c(CollapseType = "character",
                   EraPad = "integer"))

#' Initialization function for s4 CollapseSettings
#'
#'
#' @param .Object an object to initialize
#' @param CollapseType default character string ERA
#' @param EraPad default integer 0
#' @importFrom methods callNextMethod
#' @return an initial CollapseSettings object
setMethod("initialize", "CollapseSettings",
          function(.Object,
                   CollapseType = "ERA",
                   EraPad = 0L){
            .Object <- callNextMethod()
            .Object@CollapseType<- CollapseType
            .Object@EraPad <- EraPad
            .Object
          })

#' An S4 class for CensorWindow
#'
#' A class showing dates that indicate the range of entries the are captured in the cohort
#'
#' @slot StartDate the left side of truncation for the study observation
#' @slot EndDate the right side of truncation for the study observation
setClass("CensorWindow",
         slots = c(StartDate = "character",
                   EndDate = "character"))

#' Initialization function for s4 cCensorWindow
#'
#'
#' @param .Object an object to initialize
#' @param StartDate NA character
#' @param EndDate NA character
#' @importFrom methods callNextMethod
#' @return an initial CensorWindow object
setMethod("initialize", "CensorWindow",
          function(.Object,
                   StartDate = NA_character_,
                   EndDate = NA_character_){
            .Object <- callNextMethod()
            .Object@StartDate<- StartDate
            .Object@EndDate <- EndDate
            .Object
          })

#' An S4 class providing details for the Cohort
#'
#' @slot Name a name for the cohort
#' @slot Description a text field providing an information on the cohort and what it is intended
#' @slot Author who created the cohort
#' @slot cdmVersionRange the range of cdm versions
setClass("CohortDetails",
         slots=c(Name = "character",
                 Description = "character",
                 Author = "character",
                 cdmVersionRange = "character"))

#Cohort Definition


#' An S4 class for Cohort Definition
#'
#' A cohort definition contains information about how to quantify a clinical concept.
#'
#' @slot CohortDetails a cohortDetails object providing meta information about the cohort
#' @slot PrimaryCriteria a component class containing the primary criteria
#' @slot AdditionalCriteria a component class containing the additional criteria
#' @slot InclusionRules a component class containing the Inclusion Rules
#' @slot EndStrategy a component class containing the End Strategy
#' @slot CensoringCriteria a component class containing the censoring criteria
#' @slot CohortEra a component class containing the cohort era
setClass("CohortDefinition",
         slots=c(CohortDetails = "CohortDetails",
                 PrimaryCriteria = "Component",
                 AdditionalCriteria = "Component",
                 InclusionRules = "Component",
                 EndStrategy = "Component",
                 CensoringCriteria ="Component",
                 CohortEra = "Component")
)



