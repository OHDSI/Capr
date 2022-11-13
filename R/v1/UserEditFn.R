#' Function to edit Meta data
#'
#' This function edits a meta data class
#' @param obj the component you wish to edit
#' @param slotNm the slot to edit
#' @param edit the edit to make
#' @return the edit s4 class object
#' @include LowLevelUtilityFn.R
#' @importFrom methods slot
#' @export
editMetaData <- function(obj, slotNm, edit) {
  check0 <- class(obj)
  if (check0 != "MetaData") {
    stop("The object must be a metaData to edit using this function")
  }
  methods::slot(obj, slotNm) <- edit
  return(obj)
}

#' Function to edit Limit
#'
#' This function edits a limit class
#' @param obj the component you wish to edit
#' @param edit the edit to make either all first or last
#' @return the edited s4 class object
#' @importFrom methods slot
#' @export
editLimit <- function(obj, edit = c("All", "First", "Last")){
  check0 <- class(obj)
  if (check0 != "Limit") {
    stop("The object must be a limit to edit using this function")
  }
  methods::slot(obj, "Type") <- edit
  return(obj)
}
#' Function to edit a concept set item
#'
#' This function edits a concept set item class
#' @param obj the component you wish to edit
#' @param edit the edit to make
#' @param index an index to specify a postion in a list
#' @param mapping a character of includeDescendants, isExcluded or includeMapped to toggle logic
#' @param newName a character string updating the name of the concept set expression
#' @return the edit s4 class object
#' @importFrom methods slot
#' @importFrom uuid UUIDgenerate
#' @export
editConceptSetItem <- function(obj,
                                     edit,
                                     index = NULL,
                                     mapping = NULL,
                                     newName = NULL) {
  #error handler for cse object
  check0 <- class(obj)
  if (check0 != "ConceptSetExpression") {
    stop("The object must be a concept set expression to edit using this function")
  }
  if (is.null(index)) {
    index <- 1 #if no index then make 1
  }
  if (is.null(mapping)) {
    #error handling for replacing concept set item
    check1 <- class(edit)
    if (check1 != "ConceptSetItem") {
      stop("If changing the full concept set item in concept set expression it must be of ConceptSetItem class")
    }
    #change concept set item
    methods::slot(obj, "Expression")[[index]] <- edit #if no mapping edit the full concept set item
  } else {
    #error handling for toggling mapping
    # check2 <- typeof(edit)
    # if (check2 != "logical") {
    #   stop("If changing the concept set item mapping the edit must be of type logical")
    # }
    #change concept set item mapping
    methods::slot(methods::slot(obj, "Expression")[[index]], mapping) <- as.logical(edit)
  }
  #since the concept set expression has changed. Update the id
  methods::slot(obj, "id") <- uuid::UUIDgenerate()

  if (is.null(newName)) {
    #if no new name specified then label the update date and time
    newName <- paste(obj@Name, "Updated:", as.character(Sys.time()))
  }
  check3 <- typeof(newName)
  if (typeof(check3) != "character") {
    stop("The new name must be a character string")
  }
  methods::slot(obj, "Name") <- newName #if new name specified replace with name
  #return updated object
  return(obj)
}

#' Function to edit an Occurrence
#'
#' This function edits an occurrence class
#' @param obj the component you wish to edit
#' @param slotNm the slot to edit
#' @param edit the edit to make
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editOccurrence <- function(obj, slotNm, edit){
  #error handler for cse object
  check0 <- class(obj)
  if (check0 != "Occurrence") {
    stop("The object must be a occurrence to edit using this function")
  }

  #error handler if slot is type
  if (slotNm == "Type") {
    check1 <- edit %in% c("at_least", "exactly", "at_most")
    if (!check1) {
      stop("The type must be either (as spelled) at_least, exactly, at_most")
    }
    methods::slot(obj, slotNm) <- edit
  }

  #error handler if slot is count
  if (slotNm == "Count") {
    methods::slot(obj, slotNm) <- as.integer(edit)
  }

  #error handler if slot is Distinct
  if (slotNm == "isDistinct") {
    methods::slot(obj, slotNm) <- as.logical(edit)
  }

  return(obj)
}

#' Function to edit a window
#'
#' This function edits a window class
#' @param obj the component you wish to edit
#' @param slotNm the slot to edit
#' @param edit the edit to make
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editWindow <- function(obj, slotNm, edit) {
  #error handler for window object
  check0 <- class(obj)
  if (check0 != "Window") {
    stop("The object must be a window to edit using this function")
  }

  if (slotNm == "Event") {
    check1 <- edit %in% c("EventStarts", "EventEnds")
    if (!check1) {
      stop("The event must be either as spelled EventStarts or EventEnds")
    }
  }
  if (slotNm == "Index") {
    check2 <- edit %in% c("IndexStartDate", "IndexEndDate")
    if (!check2) {
      stop("The event must be either as spelled IndexStartDate or IndexEndDate")
    }
  }
  if (slotNm == "Start" | slotNm == "End") {
    check3 <- typeof(edit)
    if (check3 != "list"){
      stop("The edit must be a list of length 2 where the first element is either All or an integer for days and the second element is either as spelled Before or After")
    }
    edit <- list('Days' = edit[[1]], 'Coeff' = edit[[2]])
  }
  methods::slot(obj, slotNm) <- edit
  return(obj)
}

#' Function to edit Timeline
#'
#' This function edits a timeline class
#' @param obj the component you wish to edit
#' @param slotNm the slot to edit
#' @param edit the edit to make
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editTimeline <- function(obj, slotNm, edit) {
  #error handler for timeline object
  check0 <- class(obj)
  if (check0 != "Timeline") {
    stop("The object must be a timeline to edit using this function")
  }

  if (slotNm == "RestrictVisit" | slotNm == "IgnoreObservationPeriod") {
    methods::slot(obj, slotNm) <- as.logical(edit)
  }

  if (slotNm == "StartWindow" | slotNm == "EndWindow") {
    check1 <- class(edit)
    if (check1 != "Window") {
      stop("The object to replace must be of window class")
    }
    methods::slot(obj, slotNm) <- edit
  }
  return(obj)
}

#' Function to edit Observation Window
#'
#' This function edits a observation window class
#' @param obj the component you wish to edit
#' @param slotNm the slot to edit
#' @param edit the edit to make
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editObservationWindow <- function(obj, slotNm, edit){
  #error handler for timeline object
  check0 <- class(obj)
  if (check0 != "ObservationWindow") {
    stop("The object must be a observation window to edit using this function")
  }
  methods::slot(obj, slotNm) <- as.integer(edit)
  return(obj)
}

#' Function to edit Query
#'
#' This function edits a query class
#'
#' @param obj the component you wish to edit
#' @param slotNm the slot to edit
#' @param edit the edit to make
#' @param index an integer index specifying the location within a list, if not needed leave null
#' @return the edit s4 class object
#' @importFrom methods slot
editQuery <- function(obj, edit, slotNm, index = NULL) {
  #error handler for query object
  check0 <- class(obj)
  if (check0 != "Query") {
    stop("The object must be a query to edit using this function")
  }

  if (slotNm == "Attributes") {
    check1 <- componentType(edit)
    if (check1 != "Attribute") {
      stop("The object to replace must be a component of type attribute")
    }
    if (is.null(index)) {
      index <- 1
    }
    methods::slot(obj, slotNm)[[index]] <- edit@CriteriaExpression[[1]]
  } else {
    methods::slot(obj, slotNm) <- edit
  }
  return(obj)
}


#' Function to edit Meta data
#'
#' This function edits a meta data class
#' @param obj the component you wish to edit
#' @param edit the edit to make
#' @param slotNms a list object where each entry is a slot across multiple objects. The list must
#' be constructed in order and can be done by following the object structure. For example to edit
#' a window in the timeline one must construct a list('Timeline', 'StartWindow','Start'). If one
#' wants to edit the count in an occurrence the list is: list('Occurrence','Count').
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editCount <- function(obj, edit, slotNms) {
  #error handler for count object
  check0 <- class(obj)
  if (check0 != "Count") {
    stop("The object must be a count to edit using this function")
  }

  check1 <- typeof(slotNms)
  if (check1 != "list") {
    stop("The slotNms must be a list object")
  }

  if (slotNms[[1]] == "Criteria") {
    if (length(slotNms) == 2){
      obj@Criteria <- editQuery(obj@Criteria, edit, slotNm = slotNms[[2]])
    }
    if (length(slotNms) ==1){
      check1 <- class(edit)
      if (check1 != "Query") {
        stop("The object to replace must be of query class")
      }
      methods::slot(obj, slotNms[[1]]) <- edit
    }
  }

  if (slotNms[[1]] == "Timeline") {
    if (length(slotNms) == 2){
      obj@Timeline <- editTimeline(obj@Timeline, edit, slotNm = slotNms[[2]])
    }
    if (length(slotNms) == 3){
      methods::slot(obj@Timeline,slotNms[[2]]) <- editWindow(methods::slot(obj@Timeline,slotNms[[2]]),
                                                                        edit, slotNm = slotNms[[3]])
    }
    if (length(slotNms) == 1){
      check2 <- class(edit)
      if (check2 != "Timeline") {
        stop("The object to replace must be of timeline class")
      }
      methods::slot(obj, slotNms[[1]]) <- edit
    }
  }

  if (slotNms[[1]] == "Occurrence") {
    if (length(slotNms) == 2){
      obj@Occurrence <- editOccurrence(obj@Occurrence, edit, slotNm = slotNms[[2]])
    }
    if (length(slotNms) ==1){
      check3 <- class(edit)
      if (check3 != "Occurrence") {
        stop("The object to replace must be of occurrence class")
      }
      methods::slot(obj, slotNms[[1]]) <- edit
    }

  }
  return(obj)
}

#' Function to edit Expression type
#'
#' This function edits a expression type class
#' @param obj the component you wish to edit
#' @param slotNm the slot to edit
#' @param edit the edit to make
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editExpressionType <- function(obj, edit, slotNm) {
  #error handler for query object
  check0 <- class(obj)
  if (check0 != "ExpressionType") {
    stop("The object must be a ExpressionType to edit using this function")
  }
  if (slotNm == "Type"){
    methods::slot(obj,slotNm) <- toupper(edit)
  }

  if(slotNm == "Count") {
    methods::slot(obj,slotNm) <- as.integer(edit)
  }
  return(obj)
}


#' Function to edit Primary Criteria
#'
#' This function edits a meta data class
#' @param primaryCriteria the primary criteria component you wish to edit
#' @param detail the slot to edit. The options include: Name, Description, CriteriaList,
#' Attribute, PriorDays, PostDays, ObservationWindow, PrimaryCriteriaLimit, ConceptSetItem,
#' ConceptSetMapping. Each slot has a particular edit type.
#' @param edit the edit to make. If the detail is Name or Description the edit must be a character string.
#' If the edit is PriorDays, PostDays or ObservationWindow the edit must be an integer, where the ObservationWindow
#' is an edit of two integers to modify both the prior and post days. If the edit is to the PrimaryCriteriaLimit
#' the edit must be a character string of All, First or Last. If the edit is to the conceptSetItem the
#' edit must be a ConceptSetItem class. And if the edit is to the concept set mapping the edit must be a logical (T/F).
#' If the edit is to the CriteriaList it must be a query type component and if it is to the attribute it must be an attribute type component
#' @param add a loggic toggle to say if you are adding a piece to the pc component
#' @param index an index to specify the position in a vector. This is needed for CriteriaList, Attribute, and
#' edits to the concept sets. The CriteriaList only needs a single index. The others need one index for
#' the position in the list and a second for the position inside the substructure.
#' @param mapping an character string specifying the mapping to change. Options are
#' includeDescendants, isExcluded, and includeMapped. This is only required if the detail is ConceptSetMapping
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editPrimaryCriteria <- function(primaryCriteria, detail = c("Name", "Description",
                                                            "CriteriaList",
                                                            "Attribute",
                                                            "PriorDays",
                                                            "PostDays",
                                                            "ObservationWindow",
                                                            "PrimaryCriteriaLimit",
                                                            "ConceptSetItem",
                                                            "ConceptMapping"),
                                edit,
                                add = FALSE,
                                index = NULL,
                                mapping = NULL) {
  check <- componentType(primaryCriteria)
  if (check != "PrimaryCriteria") {
    stop("The Component is not a Primary Criteria class.")
  }

  if (detail == "Name") {
    primaryCriteria@MetaData <- editMetaData(primaryCriteria@MetaData, "Name", edit)
  }

  if (detail == "Description") {
    primaryCriteria@MetaData <- editMetaData(primaryCriteria@MetaData, "Description", edit)
  }

  if (detail == "PriorDays") {
    primaryCriteria@CriteriaExpression$ObservationWindow <- editObservationWindow(primaryCriteria@CriteriaExpression$ObservationWindow,
                                                                                  slotNm = "PriorDays",
                                                                                  edit = edit)
  }

  if (detail == "PostDays") {
    primaryCriteria@CriteriaExpression$ObservationWindow <- editObservationWindow(primaryCriteria@CriteriaExpression$ObservationWindow,
                                                                                  slotNm = "PostDays",
                                                                                  edit = edit)
  }

  if (detail == "ObservationWindow") {
    if (length(edit) == 2) {
      stop("Editing the Observation Window requires a change to Post and Prior Days. Use a vector with two integer values")
    }
    primaryCriteria@CriteriaExpression$ObservationWindow <- editObservationWindow(primaryCriteria@CriteriaExpression$ObservationWindow,
                                                                                  slotNm = "PriorDays",
                                                                                  edit = edit[1])
    primaryCriteria@CriteriaExpression$ObservationWindow <- editObservationWindow(primaryCriteria@CriteriaExpression$ObservationWindow,
                                                                                  slotNm = "PostDays",
                                                                                  edit = edit[2])
  }

  if (detail == "PrimaryCriteriaLimit") {

    primaryCriteria@Limit <- editLimit(primaryCriteria@Limit, edit)
  }

  if (detail == "CriteriaList") {
    if (add) {
      #add a query to pc
      #error handler for not component class to add
      checkQuery1 <- methods::is(edit)
      if (checkQuery1 != "Component") {
        stop("To add to the Primary Criteria the edit must be a component class of query type")
      }
      #error handle of not query type
      checkQuery2 <- componentType(edit)
      if (checkQuery2 != "Query") {
        stop("Component Class must be of query type")
      }
      nHave <- length(primaryCriteria@CriteriaExpression$CriteriaList)
      #update query
      primaryCriteria@CriteriaExpression$CriteriaList[[nHave + 1]] <- edit@CriteriaExpression[[1]]
      #update cse
      primaryCriteria@ConceptSetExpression[[nHave + 1]] <- edit@ConceptSetExpression[[1]]
    } else {
      #otherwise edit in place of specified index
      if (is.null(index)) {
        index <- 1
      }
      primaryCriteria@CriteriaExpression$CriteriaList[[index]] <- edit@CriteriaExpression[[1]]
      #update cse
      primaryCriteria@ConceptSetExpression[[index]] <- edit@ConceptSetExpression[[1]]
    }
  }

  if (detail == "ConceptSetItem") {
    if (is.null(index)){
      index <- c(1,1)
    } else {
      if (length(index) != 2){
        stop("the index must be a vector of two integers. The first is the location in the list of concept set expressions and the second the location of the item to edit")
      }
    }
    oldId <- getConceptSetId( primaryCriteria@ConceptSetExpression[[index[1]]])
    primaryCriteria@ConceptSetExpression[[index[1]]] <- editConceptSetItem(primaryCriteria@ConceptSetExpression[[index[1]]],
                                                                        edit, index = index[2])
    newId <- getConceptSetId( primaryCriteria@ConceptSetExpression[[index[1]]])
    for (i in seq_along(primaryCriteria@CriteriaExpression$CriteriaList)) {
      if (oldId == getConceptSetId(primaryCriteria@CriteriaExpression$CriteriaList[[i]])) {
        primaryCriteria@CriteriaExpression$CriteriaList[[i]]<- editQuery(primaryCriteria@CriteriaExpression$CriteriaList[[i]],
                                                                         newId, slotNm = "CodesetId")
      }
    }
  }
  if (detail == "ConceptMapping") {
    if(is.null(mapping)) {
      stop("must include the mapping to change if edit the concept mapping")
    }
    if (is.null(index)){
      index <- c(1,1)
    } else {
      if (length(index) != 2){
        stop("the index must be a vector of two integers. The first is the location in the list of concept set expressions and the second the location of the item to edit")
      }
    }
    oldId <- getConceptSetId( primaryCriteria@ConceptSetExpression[[index[1]]])
    primaryCriteria@ConceptSetExpression[[index[1]]] <- editConceptSetItem(primaryCriteria@ConceptSetExpression[[index[1]]],
                                                                           edit = edit,
                                                                           index = index[2],
                                                                           mapping = mapping)
    newId <- primaryCriteria@ConceptSetExpression[[index[1]]]@id
    for (i in seq_along(primaryCriteria@CriteriaExpression$CriteriaList)) {
      if (oldId == getConceptSetId(primaryCriteria@CriteriaExpression$CriteriaList[[i]])) {
        primaryCriteria@CriteriaExpression$CriteriaList[[i]]<- editQuery(primaryCriteria@CriteriaExpression$CriteriaList[[i]],
                                                                         newId, slotNm = "CodesetId")
      }
    }

  }


  # if (detail == "Attribute") {
  #   #error handler for not component class to add
  #   checkQuery1 <- methods::is(edit)
  #   if (checkQuery1 != "Component") {
  #     stop("To add to the Primary Criteria the edit must be a component class of query type")
  #   }
  #   #error handle of not query type
  #   checkQuery2 <- componentType(edit)
  #   if (checkQuery2 != "Attribute") {
  #     stop("Component Class must be of Attribute type")
  #   }
  #
  #   if (add) {
  #     if (is.null(indicies)){
  #       idx <- 1
  #       idy <- length(primaryCriteria@CriteriaExpression$CriteriaList[[idx]]@Attributes) +1
  #     } else {
  #
  #     }
  #     n <- length(primaryCriteria@CriteriaExpression$CriteriaList[[indicies]]@Attributes) +1
  #
  #   } else {
  #     #edit a attribute
  #   }
  #   primaryCriteria@CriteriaExpression$CriteriaList[[idx]]@Attributes[[idy]] <- edit@CriteriaExpression[[1]]
  # }
  #
  #
  #

  return(primaryCriteria)
}

#' Function to edit Inclusion Rules
#'
#' This function edits a meta data class
#' @param inclusionRules the inclusion rules component you wish to edit
#' @param edit the edit to make. The edit must conform to the structure of the location detail
#' where the edit is made. See detail for more information
#' @param detail the slot to edit in the inclusion rules. Options are: Name, Description, Rule and Limit.
#' If editing the name or description the edit must be a character string. If editing the limit
#' the edit must be a character string of either All, First or Last. If the detail is a rule,
#' the edit must be a Group type component class. One can use the function componentType to
#' check the type for a component class object.
#' @param add a loggic toggle to say if you are adding a piece to the pc component
#' @param index an index to specify the position in a list that is to be modified. If null defaults to 1
#' @return the edit s4 class object
#' @importFrom methods slot
#' @export
editInclusionRules <- function(inclusionRules,
                               edit,
                               detail = c("Name","Description","Rule","Limit"),
                               add = FALSE,
                               index = NULL) {
  check <- componentType(inclusionRules)
  if (check != "InclusionRules") {
    stop("The Component is not a Primary Criteria class.")
  }

  if (detail == "Name") {
    inclusionRules@MetaData <- editMetaData(inclusionRules@MetaData, "Name", edit)
  }

  if (detail == "Description") {
    inclusionRules@MetaData <- editMetaData(inclusionRules@MetaData, "Description", edit)
  }

  if (detail == "PrimaryCriteriaLimit") {

    inclusionRules@Limit <- editLimit(inclusionRules@Limit, edit)
  }

  if (detail == "Rule") {
    check1 <- componentType(edit)
    if (check1 != "Group") {
      stop("The component type must be a group to edit or add")
    }
    if (add) {
      if (!is.null(index)){
        warning("An index was specified but the addition was made to length + 1 of the list (a new slot)")
      }
      index <- length(inclusionRules@CriteriaExpression) +1
    } else {
      if (is.null(index)) {
        index <- 1
      }
    }
    inclusionRules@CriteriaExpression[[index]] <- edit
  }

  return(inclusionRules)
}
#
# #' Function to edit a group
# #'
# #' This function edits a Group class
# #' @param obj the component you wish to edit
# #' @param edit the edit to make. The edits for this function must follow the structures located
# #' in the contents one wishes to edit. For example a Criteria list needs to be updated with
# #' a count component type. A demographic criteria list must be updated with an attribute type and
# #' a group must be updated with a group. Since group objects can become very large list, it
# #' is recommended that the user edits smaller pieces at the lower levels of a group object
# #' @param slotNm the slot to edit
# #' @param index the index position of the object to modify
# #' @return the edit s4 class object
# #' @importFrom methods slot
# #' @export
# editGroup <- function(obj, edit, slotNm, index) {
#   #error handler for query object
#   check0 <- class(obj)
#   if (check0 != "Group") {
#     stop("The object must be a group to edit using this function")
#  }
#
#   if (slotNm == "ExpressionType") {
#    if (index > 2 | index < 1) {
#       stop("The index for a type must be 1 or 2")
#     }
#     if (index == 2) {
#       methods::slot(obj, slotNm) <- editExpressionType(obj@ExpressionType, edit, slotNm = "Type")
#     }
#     if (index == 1) {
#      methods::slot(obj, slotNm) <- editExpressionType(obj@ExpressionType, edit, slotNm = "Count")
#     }
#   }
#
#  if (slotNm == "CriteriaList") {
#     check1 <- componentType(edit)
#     if (check1 != "Count") {
#       stop("The editing object must be a count")
#     }
#     methods::slot(obj, slotNm)[[index]] <- edit
#   }
#
#   if (slotNm == "DemographicCriteriaList") {
#     check2 <- componentType(edit)
#     if (check2 != "Attribute") {
#       stop("The editing object must be a attribute type")
#     }
#     methods::slot(obj, slotNm)[[index]] <- edit
#   }
#
#   if (slotNm == "Group") {
#     check3 <- componentType(edit)
#     if (check3 != "Group") {
#       stop("The editing object must be a group type")
#     }
#     methods::slot(obj, slotNm)[[index]] <- edit
#   }
#   return(obj)
# }

# editCount <- function(obj, edit, slotNm1, slotNm2 = NULL) {
#   #error handler for count object
#   check0 <- class(obj)
#   if (check0 != "Count") {
#     stop("The object must be a count to edit using this function")
#   }
#
#   if (slotNm1 == "Criteria") {
#     if (!is.null(slotNm2)){
#       obj@Criteria <- editQuery(obj@Criteria, edit, slotNm = slotNm2)
#     } else {
#       check1 <- class(edit)
#       if (check1 != "Query") {
#         stop("The object to replace must be of query class")
#       }
#       methods::slot(obj, slotNm1) <- edit
#     }
#   }
#
#   if (slotNm1 == "Timeline") {
#     if (!is.null(slotNm2)){
#       obj@Timeline <- editTimeline(obj@Timeline, edit, slotNm = slotNm2)
#     } else {
#       check2 <- class(edit)
#       if (check2 != "Timeline") {
#         stop("The object to replace must be of timeline class")
#       }
#       methods::slot(obj, slotNm1) <- edit
#     }
#   }
#
#   if (slotNm1 == "Occurrence") {
#     if (!is.null(slotNm2)){
#       obj@Occurrence <- editOccurrence(obj@Occurrence, edit, slotNm = slotNm2)
#     } else {
#       check3 <- class(edit)
#       if (check3 != "Occurrence") {
#         stop("The object to replace must be of occurrence class")
#       }
#       methods::slot(obj, slotNm1) <- edit
#     }
#
#   }
#   return(obj)
# }
