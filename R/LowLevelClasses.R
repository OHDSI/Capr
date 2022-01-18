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

# Note: This file is broken into sections. Read the sections in reverse order from bottom to top of this file.


# TODO Where do these classes fit in the big picture?
# Other classes -------------------------------------------------------------------

#' An S4 class for a Window
#'
#' A window class provides details on the end points of the timeline
#'
#' @slot Event a character string either EventStarts or EventEnds. Identifies the point of reference for the window
#' @slot Start a list containing the days and coefficient for the start of the window
#' @slot End A list containing the days and coefficient for the end of the window
#' @slot Index A character string either IndexStartDate or IndexEndDate Identifies where the index is relative to the window
setClass("Window",
         slots = c(Event = "character",
                  Start = "list",
                  End = "list",
                  Index = "character"))

setValidity("Window", function(object) {
  # TODO Create Window validation rules
  TRUE
})

#' Show statements of capr objects
#'
#' These functions print the capr object to console in a readable format
#'
#' @param object the object to show
#' @return a console print of the object
#' @rdname show-method
#' @aliases show
#' @aliases show,Window-method
setMethod("show", "Window", function(object) {
  cat(object@Event, object@Start$Days, "Days", object@Start$Coeff, "and",
      object@End$Days, "Days", object@End$Coeff, object@Index)
})

#' An S4 class for Timeline
#'
#'The timeline class provides context to when the criteria must be observed in a person timeline to pretain to the expression
#'
#' @slot StartWindow a window class object identifying the start window
#' @slot EndWindow a window class object ifentifying the end window (optional)
#' @slot RestrictVisit a logic toggle where TRUE restricts to the same visit
#' @slot IgnoreObservationPeriod a logic toggle where TRUE allows events outside the observation period
setClass("Timeline",
         slots = c(StartWindow = "Window",
                 EndWindow = "Window",
                 RestrictVisit = "logical",
                 IgnoreObservationPeriod = "logical"))

setValidity("Timeline", function(object) {
  # TODO create Timeline validation rules
  TRUE
})

#' @rdname show-method
#' @aliases show,Timeline-method
setMethod("show", "Timeline", function(object) {
  cat("Timeline", "\n")
  show(object@StartWindow)
  cat("\n")
  if (length(object@EndWindow@Start) == 2){
    show(object@EndWindow)
    cat("\n")
  }
  if(object@RestrictVisit) {
    cat("at the same visit as cohort entry")
    cat("\n")
  }
  if (object@IgnoreObservationPeriod) {
    cat("allow events outside observation period")
    cat("\n")
  }
})

#' An S4 class for Occurrence
#'
#' The Occurrence class provides logic on the number of criterias that most be true in a person for them to be contained in
#' the expression
#'
#' @slot Type a character string of either at most, at least, or exactly providing context to the number of occurrences
#' @slot Count an integer value that provides the number of occurrences
#' @slot isDistinct a logic toggle where if TRUE only counts distinct occurrences
setClass("Occurrence", #a class counting the number of occurrences of the event
         slots = c(Type = "character",
                 Count = "integer",
                 isDistinct = "logical")
         # TODO add prototype
         )

setValidity("Occurrence", function(object) {
  # TODO Occurrence validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,Occurrence-method
setMethod("show", "Occurrence", function(object) {
  cat(utils::str(object))
})

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
                   Count = "integer"),
         prototype = list(Type = "ALL",
                          Count = NA_integer_)
         )

setValidity("ExpressionType", function(object) {
  # TODO ExpressionType validity checks
  TRUE
})

#' An S4 class for ObservationWindow
#'
#' A class designating an amount of time necessary for an initial event to be recorded
#'
#' @slot PriorDays minimal amount of time before event for it to be recorded
#' @slot PostDays minimal amount of time after an event for it to be recorded
setClass("ObservationWindow",
         slots = c(PriorDays = "integer",
                   PostDays = "integer"))

setValidity("ObservationWindow", function(object) {
  # TODO create ObservationWindow validation rules
  TRUE
})

#' @rdname show-method
#' @aliases show,ObservationWindow-method
setMethod("show", "ObservationWindow", function(object) {
  cat("Observation Window:", "\n")
  cat("Continous observation of at least", object@PriorDays, "days prior and",
      object@PostDays, "days after event index date")
})

# TODO where do these Circe structure classes fit into the big picture (semantic model)
# Circe structures --------------------------------------------------------

#' An S4 class for Group
#'
#' TODO clarify the description of a group.
#' A group that bundles criteria together identifying an event
#'
#' @slot Type a expression type class Boolean for the number of items to make the group count
#' @slot CriteriaList a list of items (counts and queries) that would identify a medical event
#' @slot DemographicCriteriaList a list of demographic attributes that could identify a population
#' @slot Groups a list of other groups that are contained within a group
setClass("Group",
         slots = c(Type = 'ExpressionType',
                   CriteriaList = 'list',
                   DemographicCriteriaList = 'list',
                   Groups = 'list'))

setValidity("Group", function(object) {
  # TODO Group validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,Group-method
setMethod("show", "Group", function(object) {
  ty <- object@Type@Type
  if (ty == "AT_LEAST" | ty == "AT_MOST") {
    cat("Having",tolower(ty), object@Type@Count, "of the following criteria")
  } else{
    cat("Having",tolower(ty), "of the following criteria")
  }
  lineBreak(1)
  if (length(object@CriteriaList) > 0) {
    cat("Criteria List")
    lineBreak(3)
    for (i in seq_along(object@CriteriaList)) {
      show(object@CriteriaList[[i]])
    }
  }
  if (length(object@DemographicCriteriaList) > 0) {
    cat("Demographic Criteria List")
    lineBreak(3)
    for (i in seq_along(object@DemographicCriteriaList)) {
      show(object@DemographicCriteriaList[[i]])
    }
  }
  if (length(object@Groups) > 0) {
    cat("Groups List")
    lineBreak(3)
    for (i in seq_along(object@Groups)) {
      show(object@Groups[[i]])
    }
  }
})

#' An S4 class for a Query
#'
#' TODO clarify description of a Query
#' A query is a medical concept that can be extracted from a database through a 'where' clause in a SQL statement.
#' This includes concepts. (?)
#'
#' @slot Domain the domain where the concepts can be found
#' @slot CodesetId the id that matches the concept set expression
#' @slot Attributes a list of attributes that modify the query with more information
setClass("Query",
         slots = c(Domain = "character",
                   CodesetId = "character",
                   Attributes = "list"),
         prototype = list(Domain = NA_character_,
                          CodesetId = NA_character_,
                          Attributes = list())
)

setValidity("Query", function(object) {
  # TODO Query validation rules
  TRUE
})

#' @rdname show-method
#' @aliases show,Query-method
setMethod("show", "Query", function(object) {
  cat("Query", "\n")
  cat("Domain:", object@Domain, "\n")
  cat("CodesetId:", object@CodesetId, "\n")
  cat("Attributes:", "\n")
  if (length(object@Attributes) > 0) {
    for (i in seq_along(object@Attributes)) {
      cat("\t",paste0(i, ") "))
      show(object@Attributes[[i]])
      cat("\n")
    }
  } else {
    cat("None", "\n")
  }
})

#' An S4 class for a Count
#'
#' A count class provides a number of occurrences of the query and the timeline that it happens
#'
#' @slot Criteria a query class object
#' @slot Timeline a timeline class object
#' @slot Occurrence an occurrence class object
setClass(
  "Count",
         slots = c(Criteria = "Query",
                  Timeline = "Timeline",
                  Occurrence = "Occurrence"))

setValidity("Count", function(object) {
  # TODO Count Validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,Count-method
setMethod("show", "Count", function(object) {
  cat("Count", object@Occurrence@Type, object@Occurrence@Count)
  if (object@Occurrence@isDistinct) {
    cat(" distinct occurrence(s) of")
  } else {
    cat(" occurrence(s) of")
  }
  lineBreak(4)
  show(object@Criteria)
  lineBreak(4)
  show(object@Timeline)
})

# Attributes --------------------------------------------------------------

#' An S4 class for an Op Attribute
#'
#' An operator attribute meaning it has some value with a boolean operator
#'
#' @slot Name the name of the attribute
#' @slot Op the operator gt,lt,gte,lte,eq,neq,bt,!bt
#' @slot Contents the contents of the attribute as a list. includes the value and the extent
setClass("OpAttribute",
         slots = c(Name = "character",
                   Op = "character",
                   Contents = "list"))

setValidity("OpAttribute", function(object) {
  # TODO Create OpAttribute validity checks
  TRUE
})

#' @rdname show-method
#' @aliases show,OpAttribute-method
setMethod("show", "OpAttribute", function(object) {
  cat(paste0(methods::is(object), ":"),object@Name, "==>")
  op <- object@Op
  if (op == "bt" | op == "!bt") {
    cat("", op, object@Contents$Value, "and", object@Contents$Extent)
  } else{
    cat("", op, object@Contents$Value)
  }
})

#' An S4 class for SourceConceptAttribute
#'
#' An attribute that looks at utilizing the source concepts instead of standard concepts
#'
#' @slot Name name of the attribute
#' @slot SourceCodesetId a source concept id, connection to concept set expression
setClass("SourceConceptAttribute",
         slots = c(Name = "character",
                   SourceCodesetId = "character"))

# TODO add validity check and show method

#' An S4 class for Concept Attribute
#'
#' A concept attribute, using concepts to identify the attribute like a gender or race etc
#'
#' @slot Name the name of the attribute
#' @slot Concepts a list containing the concepts used to identify the attribute
setClass("ConceptAttribute",
         slots = c(Name = "character",
                   Concepts = "list"))

# TODO add validity check and show method

#' An S4 class for CorrelatedCriteriaAttribute
#'
#' A group attribute that is nested within a query.
#'
#' @slot Name name of the attribute
#' @slot Group a group class object for the attribute
setClass("CorrelatedCriteriaAttribute",#
         slots = c(Name = "character",#
                   Group = "Group"))#

# TODO add validity check and show method

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

# TODO add validity check and show method

# TODO how do end strategy classes fit into the big picture (circe semantic model)?
# EndStrategy -------------------------------------------------------------

#' An S4 class for DateOffsetEndStrategy
#'
#' An end strategy class specifying a number of days from the start or end of the initial event until
#' cohort exit
#'
#' @slot DateField a character string specifying either the StartDate or EndDate of the initial event to begin counting
#' days until cohort exit
#' @slot Offset an integer value specifying padding to the cohort exit.
setClass("DateOffsetEndStrategy",
         slots=c(DateField = "character",
                 Offset = "integer"))

# TODO add validity check and show method

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

# TODO add validity check and show method

#' An S4 class for EndOfCtsObsEndStrategy
#'
#' When the end strategy is not defined the cohort exit is done based on the end of continuous observation.
#' This class is an end strategy type.
#'
#' @slot EndOfContinuousObservation set as true for end strategy option
setClass("EndOfCtsObsEndStrategy",
         slot = c(EndOfContinuousObservation = "logical"),
         prototype = list(EndOfContinuousObservation = TRUE))

# TODO add validity check and show method

#' An S4 class for Collapse Settings
#'
#' A class providing information that identifies the padding for cohort eras
#'
#' @slot Type boolean operator for the number of items in group to include. all, any, at most and at least
#' @slot Count the number of criteria's needed for restriction. If Type is ALL or ANY this value is NA
setClass("CollapseSettings",
         slots = c(CollapseType = "character",
                   EraPad = "integer"),
         prototype = list(CollapseType = "ERA",
                          EraPad = 0L))

# TODO add validity check and show method

#' An S4 class for CensorWindow
#'
#' A class showing dates that indicate the range of entries the are captured in the cohort
#'
#' @slot StartDate the left side of truncation for the study observation
#' @slot EndDate the right side of truncation for the study observation
setClass("CensorWindow",
         slots = c(StartDate = "character",
                   EndDate = "character"),
         prototype = list(StartDate = NA_character_,
                          EndDate = NA_character_))

# TODO add validity check and show method

# Component slot ConceptSetExpression ------------------------------------------

#' An S4 class for a ConceptSet
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
setClass("Concept",
         slots = c(CONCEPT_ID = "integer",
                   CONCEPT_NAME = "character",
                   STANDARD_CONCEPT = "character",
                   STANDARD_CONCEPT_CAPTION = "character",
                   INVALID_REASON = "character",
                   INVALID_REASON_CAPTION = "character",
                   CONCEPT_CODE = "character",
                   DOMAIN_ID = "character",
                   VOCABULARY_ID = "character",
                   CONCEPT_CLASS_ID = "character")
         # TODO write prototype
         )

setValidity("Concept", function(object) {
  # TODO
})

#' @rdname show-method
#' @aliases show,Concept-method
setMethod("show", "Concept", function(object) {
  nm <- methods::slotNames(methods::is(object))
  concept <- unname(sapply(nm, slot, object = object))
  cid <- paste("conceptId:", concept[1])
  cname <- paste("conceptName:", concept[2])
  cstd <- paste("standardConcept:", concept[3])
  cdom <- paste("domainId:", concept[8])
  cat("",cid, "\n", cname, "\n", cstd,"\n", cdom,"\n")
})

#' An S4 class for ConceptSetItem
#'
#' A class that provides information on the mapping of the concept
#'
#' @slot Concept a concept class object
#' @slot isExcluded toggle if want to exclude the concept
#' @slot includeDescendants toggle if want to include descendants
#' @slot includeMapped toggle if want to include map
setClass("ConceptSetItem",
         slots = c(Concept = "Concept",
                   isExcluded = "logical",
                   includeDescendants = "logical",
                   includeMapped = "logical"),
         prototype = list(Concept = new("Concept"),
                          isExcluded = FALSE,
                          includeDescendants = TRUE,
                          includeMapped = FALSE)
         )

setValidity("ConceptSetItem", function(object) {
  # TODO fill in validation logic
  TRUE
})


# print a checkmark
# stringi::stri_unescape_unicode("\\u2714")
#' @rdname show-method
#' @aliases show,ConceptSetItem-method
#' @export
setMethod("show", "ConceptSetItem", function(object) {
  domains <- paste(unique(vapply(object, function(x) x@Concept@DOMAIN_ID, character(1))), collapse = ", ")
  cli::cat_line(paste(domains, "Concept Set"))
  purrr::walk(object, function(x) {
    x <- x@Concept
    smry <- glue::glue("{x@CONCEPT_ID}: {x@CONCEPT_NAME} ({x@STANDARD_CONCEPT})")
    ie <- ifelse(x@isExcluded, "Excluded", "")
    id <- ifelse(x@includeDescendants, "+Descendants", "")
    im <- ifelse(x@includeMapped, "Mapped", "")
    cli::cat_line(paste(smry, paste(ie,id,im, collapse = ", ")))
  })
})

# This is the method definition from UserPrintFn
# TODO combine two show methods for ConceptSetItem into a single method
# setMethod("show", "ConceptSetItem", function(object){
#   printCapr(object@Concept)
#   ie <- paste("isExcluded:", object@isExcluded)
#   id <- paste("includeDescendants:", object@includeDescendants)
#   im <- paste("includeMapped:", object@includeMapped)
#   cat(paste(" Mapping ==>",ie,id,im), "\n")
# })

#' An S4 class for ConceptSetExpresion
#'
#' A class for the concept set expressions bundles multiple concepts with mapping
#'
#' @slot id an id for the concept set expression to identify within a component
#' @slot Name the name of the concept set expression
#' @slot Expression a list containing expressions. expressions include multiple conceptSetItem objects
setClass("ConceptSetExpression",
         slots = c(id = "character",
                   Name = "character",
                   Expression = "list"),
         prototype = list(id = uuid::UUIDgenerate(),
                          Name = NA_character_,
                          Expression = list()))

setValidity("ConceptSetExpression", function(object) {
  # TODO create validation rules for ConceptSetExpression
  TRUE
})

#' @rdname show-method
#' @aliases show,ConceptSetExpression-method
setMethod("show", "ConceptSetExpression", function(object) {
  cat(object@Name, "\n")
  cat("CodesetId:", object@id,"\n")
  cat("Expression:")
  for (i in seq_along(object@Expression)){
    lineBreak(3)
    cat(paste0("ConceptItem",i), "\n")
    show(object@Expression[[i]])
  }
})

# Component slot Limit ---------------------------------------------------------

#' An S4 class for Limit
#'
#' A class designating a limit of events per person Types include: all first last
#'
#' @slot Type how to limit events per person: all, first, or last
setClass("Limit",
         slots = c(Type = "character"),
         prototype = list(Type = "first"))

setValidity("Limit", function(object) {
  if(length(object@Type) == 1 && object@Type %in% c("all", "first", "last")) {
    "Limit Type must be one of 'all', 'fist', last'"
  } else {
    TRUE
  }
})

#' @rdname show-method
#' @aliases show,Limit-method
setMethod("show", "Limit", function(object) {
  event <- ifelse(object@Type == "all", "events", "event")
  cat(paste("Limit to", object@Type, event, "per person."))
})

# Component slot CriteriaExpression --------------------------------------------

# TODO How is CriteriaExpression represented using Capr low level classes?

# Component slot MetaData ------------------------------------------------------

#' An S4 class for Component MetaData
#' TODO confirm possible values for ComponentType. Should Index be included as a slot?
#' @slot ComponentType name of component class (this is formally defined) Possible values are...
#' @slot Name name for component customized by user
#' @slot Description description of the component
#' @slot Index A character string either IndexStartDate or IndexEndDate Identifies where the index is relative to the window
setClass("MetaData",
         slots = c(ComponentType = "character",
                   Name = "character",
                   Description = "character")
         # TODO add prototype
)

setValidity("MetaData", function(object) {
  validTypes <- c("PrimaryCriteria", "AdditionalCriteria", "InclusionRules", "EndStrategy", "CensoringCriteria", "CohortEra",
                  "Query", "Count", "Group", "ConceptSetExpression", "Attribute", "Empty")
  if (!(object@ComponentType %in% validTypes)) {
    paste0("ComponentType must be one of '", paste(validTypes, collapse = ", "), "'", ", not '", object@ComponentType, "'")
    # TODO add additional checks. Should we check length of name and Description. Should we use the argument checking package?
  } else {
    TRUE
  }
})

#' @rdname show-method
#' @aliases show,MetaData-method
setMethod("show", "MetaData", function(object) {
  cat(utils::str(object))
})

# Capr Component ----------------------------------------------------------

#' An S4 class for a cohort definition component
#'
#' This class is an flexible container used to store the component parts of cohort definition allowing us to maintain information
#' in smaller parts that remain relevant in isolation. The structure of a Circe cohort definition relies on a concept set
#' table that stores information for queries. In each cohort component an internal reference id is used to maintain
#' consistency between the expression of the cohort criteria and the actionable concepts. The component container
#' bundles the concept set expression and the criteria expression into one object that is saveable and inheritable.
#' Smaller classes are stored within the container and when they are converted into a superior class the component container
#' is modified but the previous information is kept in tact. A component consists of 4 parts: MetaData stores
#' the name, description and the ComponentType. The ComponentType identifies what kind of component one is using. Next
#' the criteriaExpression stores any information about the deployment of the medical concept. This includes queries, counts,
#' groups, attributes and other structures that detail the information of the specific component class. The limit
#' is a slot that specifies the limit of entry for person events, e.g. the first event, all events, or last event for
#' the criteriaExpression. Finally the ConceptSetExpression slot holds the concepts relevant
#' to the criteria expression and their unique identifies. A Component object can be saved as a json file or loaded back into its s4 class.
#' In some cases components can be nested inside other components
#' TODO Explain the possible nesting structures that can exist. Question: why does metaData get its own class but other slots do not?
#'
#' @slot MetaData meta information about the object
#' @slot CriteriaExpression a list of criteria that is in the object
#' @slot Limit a list containing any limits
#' @slot ConceptSetExpression a list containing any concept sets
setClass("Component",
         slots = c(MetaData = "MetaData",
                   CriteriaExpression = "list",
                   Limit = "list",
                   ConceptSetExpression = "list"),
         # TODO improve prototype
         prototype = list(MetaData = new("MetaData"),
                          CriteriaExpression = list(),
                          Limit = list(),
                          ConceptSetExpression = list())
)

setValidity("Component", function(object) {
  # TODO fill in component validity logic
  TRUE
})

#' @rdname show-method
#' @aliases show,Component-method
setMethod("show", "Component", function(object) {

  name <- ifelse(nchar(object@MetaData@Name) > 0, paste(":", object@MetaData@Name), "")

  cli::cat_line(cli::rule(paste0(object@MetaData@ComponentType, name)))
  cli::cat_line("Critera Expression")
  cli::cat_line(utils::str(object@CriteriaExpression, max.level = 2))

  print(object@Limit)
  cli::cat_line("Concept Set Expression")
  cli::cat_line(utils::str(object@ConceptSetExpression, max.level = 2))
})


# TODO combine print methods for component into a single method
#' Show Contents of a Component
#'
#' This function prints the contents of a component. Note 1/27/21 attributes and some other s4 classes
#' need to be implemented
#'
#' param showFullConceptSetExpressions T/F options to include full details of concept expressions
# printComponent <- function(x, showFullConceptSetExpressions = FALSE){
#   lineBreak(1)
#   if(methods::is(x) != "Component"){
#     stop("The object is not a component")
#   }
#   cat("Component Type:", x@MetaData@ComponentType, "\n")
#   cat("Name:", x@MetaData@Name, "\n")
#   cat("Description:", x@MetaData@Description)
#   lineBreak(2)
#   if (length(x@CriteriaExpression) > 0) {
#     if (componentType(x) == "PrimaryCriteria"){
#       cat("Criteria List")
#       for (i in seq_along(x@CriteriaExpression$CriteriaList)) {
#         lineBreak(3)
#         cat(paste0(i, ") "))
#         printCapr(x@CriteriaExpression$CriteriaList[[i]])
#       }
#       lineBreak(2)
#       printCapr(x@CriteriaExpression$ObservationWindow)
#     } else if (componentType(x) == "InclusionRules") {
#       for (i in seq_along(x@CriteriaExpression)) {
#         cat(paste0(i, ") "))
#         printComponent(x@CriteriaExpression[[i]], showFullConceptSetExpressions = showFullConceptSetExpressions)
#       }
#     } else {
#       for (i in seq_along(x@CriteriaExpression)) {
#         cat(paste0("Criteria ", i, ")"), "\n")
#         printCapr(x@CriteriaExpression[[i]])
#       }
#     }
#   }
#   lineBreak(2)
#   if (length(x@Limit) > 0) {
#     tt <- paste0(names(x@Limit),":")
#     lim <- x@Limit[[1]]@Type
#     cat(tt, lim)
#     lineBreak(2)
#   }
#   cat("Concept Set Expressions")
#   if (length(x@ConceptSetExpression) > 0) {
#     for (i in seq_along(x@ConceptSetExpression)) {
#       lineBreak(3)
#       cat(paste0(i, ") "))
#       if (showFullConceptSetExpressions) {
#         printCapr(x@ConceptSetExpression[[i]])
#       } else {
#         cat(x@ConceptSetExpression[[i]]@Name, "\n")
#         cat("CodesetId:", x@ConceptSetExpression[[i]]@id)
#       }
#     }
#   } else {
#     cat("None")
#   }
#   lineBreak(1)
# }

# TODO add unit tests for validity and print methods



# Cohort Definition -------------------------------------------------------

#' An S4 class providing metadata for a CohortDefinition
#'
#' The cohort details do not affect the cohort definition and are for improving
#' human readability only.
#'
#' @slot Name a name for the cohort
#' @slot Description a text field providing an information on the cohort and what it is intended
#' @slot Author who created the cohort
#' @slot cdmVersionRange the range of cdm versions
setClass("CohortDetails",
         slots = c(Name = "character",
                   Description = "character",
                   Author = "character",
                   cdmVersionRange = "character"),
         prototype = list(Name = "New Cohort Definition",
                          Description = "",
                          Author = "",
                          cdmVersionRange = ""))


setValidity("CohortDetails", function(object) {
  if (length(object@Name) != 1) {
    paste("Name must be a character vector of length 1, not", length(object@Name))
  } else if (length(object@Description) != 1) {
    paste("Description must be a character vector of length 1, not", length(object@Description))
  } else if (length(object@Author) != 1) {
    paste("Description must be a character vector of length 1, not", length(object@Author))
  } else if (length(object@cdmVersionRange) != 1) {
    paste("Description must be a character vector of length 1, not", length(object@cdmVersionRange))
  } else if (nchar(object@Name) == 0) {
    "Name cannot be an empty string"
  } else {
    TRUE
  }
})

#' @rdname show-method
#' @aliases show,CohortDetails-method
setMethod("show", "CohortDetails", function(object) {
  nm <- stringr::str_trunc(object@Name, 40)
  aut <- ifelse(nchar(object@Author) > 0, paste0("[", object@Author, "]"), "")
  ver <- ifelse(nchar(object@cdmVersionRange) > 0, paste0("(", object@cdmVersionRange, ")"), "")
  cat(nm, aut, ver)
})

# TODO add unit tests for validity checks
# (object <- new("CohortDetails", Name = letters[1:3]))
# (object <- new("CohortDetails"))
# (object <- new("CohortDetails", Name = "blah"))

#' An S4 class for a Circe Cohort Definition
#'
#' A cohort definition contains information about how to quantify a clinical phenotype.
#' The ultimate purpose of Capr is to allow the creation and manipulation of Circe cohort
#' definitions in R making CohortDefinition its most important class.
#'
#' @slot CohortDetails a cohortDetails object providing meta information about the cohort
#' @slot PrimaryCriteria a component class containing the primary criteria
#' @slot AdditionalCriteria a component class containing the additional criteria
#' @slot InclusionRules a component class containing the Inclusion Rules
#' @slot EndStrategy a component class containing the End Strategy
#' @slot CensoringCriteria a component class containing the censoring criteria
#' @slot CohortEra a component class containing the cohort era
setClass("CohortDefinition",
         slots = c(CohortDetails = "CohortDetails",
                   PrimaryCriteria = "Component",
                   AdditionalCriteria = "Component",
                   InclusionRules = "Component",
                   EndStrategy = "Component",
                   CensoringCriteria ="Component",
                   CohortEra = "Component")
         # TODO add prototype
)

setValidity("CohortDefinition", function(object) {
  # Type checking is automatically enforced
  # TODO implement addtitional validity checks
})

#' @rdname show-method
#' @aliases show,CohortDefinition-method
setMethod("show", "CohortDefinition", function(object) {
  cat(utils::str(object, maxlevel = 3))
  # TODO write a better print method for CohortDefinition
})
