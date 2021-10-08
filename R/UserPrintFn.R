lineBreak <- function(t = c(1,2,3,4)) {
  if (t == 1) {
    cat("\n=====================================================================\n")
  }
  if (t == 2) {
    cat("\n______________________________________________\n\n")
  }
  if (t == 3) {
    cat("\n----------------------------\n")
  }
  if (t == 4) {
    cat("\n*****\n")
  }
}

###########################
#Concept Summary
##############################

setGeneric("printCapr", function(x){standardGeneric("printCapr")})

#' Print concept summary
#'
#' These function print a summary of the CAPR s4 objects
#'
#' @param x a criteria class object in s4
#' @return a rpinted summary of the component info
#' @rdname printCapr-method
#' @aliases printCapr
#' @aliases printCapr,Concept-method
#' @export
setMethod("printCapr", "Concept",
          function(x){
            nm <- methods::slotNames(methods::is(x))
            concept <- unname(sapply(nm, slot, object = x))
            cid <- paste("conceptId:", concept[1])
            cname <- paste("conceptName:", concept[2])
            cstd <- paste("standardConcept:", concept[3])
            cdom <- paste("domainId:", concept[8])
            cat("",cid, "\n", cname, "\n", cstd,"\n", cdom,"\n")
          })

#' @rdname printCapr-method
#' @aliases printCapr,ConceptSetItem-method
#' @export
setMethod("printCapr", "ConceptSetItem",
          function(x){
            printCapr(x@Concept)
            ie <- paste("isExcluded:", x@isExcluded)
            id <- paste("includeDescendants:", x@includeDescendants)
            im <- paste("includeMapped:", x@includeMapped)
            cat(paste(" Mapping ==>",ie,id,im), "\n")
          })

#' @rdname printCapr-method
#' @aliases printCapr,ConceptSetExpression-method
#' @export
setMethod("printCapr", "ConceptSetExpression",
          function(x){
            cat(x@Name, "\n")
            cat("CodesetId:", x@id,"\n")
            cat("Expression:")
            for (i in seq_along(x@Expression)){
              lineBreak(3)
              cat(paste0("ConceptItem",i), "\n")
              printCapr(x@Expression[[i]])
            }
          })

###############################
#Time Summary
#############################

#' @rdname printCapr-method
#' @aliases printCapr,ObservationWindow-method
#' @export
setMethod("printCapr", "ObservationWindow",
          function(x){
            cat("Observation Window:", "\n")
            cat("Continous observation of at least", x@PriorDays, "days prior and",
                x@PostDays,"days after event index date")
          })


#' @rdname printCapr-method
#' @aliases printCapr,Window-method
#' @export
setMethod("printCapr", "Window",
          function(x){
            cat(x@Event, x@Start$Days, "Days", x@Start$Coeff, "and",
                x@End$Days, "Days", x@End$Coeff, x@Index)
          })

#' @rdname printCapr-method
#' @aliases printCapr,Window-method
#' @export
setMethod("printCapr", "Timeline",
          function(x){
            cat("Timeline", "\n")
            printCapr(x@StartWindow)
            cat("\n")
            if (length(x@EndWindow@Start) == 2){
              printCapr(x@EndWindow)
              cat("\n")
            }
            if(x@RestrictVisit) {
              cat("at the same visit as cohort entry")
              cat("\n")
            }
            if (x@IgnoreObservationPeriod) {
              cat("allow events outside observation period")
              cat("\n")
            }
          })


#######################
#Attribute Summary
############################


#' @rdname printCapr-method
#' @aliases printCapr,OpAttribute-method
#' @export
setMethod("printCapr", "OpAttribute",
          function(x){
            cat(paste0(methods::is(x), ":"),x@Name, "==>")
            op <- x@Op
            if (op == "bt" | op == "!bt") {
              cat("",op, x@Contents$Value, "and", x@Contents$Extent)
            } else{
              cat("",op, x@Contents$Value)
            }
          })

#####################################
#Query Summary
####################################

#' @rdname printCapr-method
#' @aliases printCapr,Query-method
#' @export
setMethod("printCapr", "Query",
          function(x){
            cat("Query", "\n")
            cat("Domain:",x@Domain, "\n")
            cat("CodesetId:", x@CodesetId, "\n")
            cat("Attributes:", "\n")
            if (length(x@Attributes) > 0) {
              for (i in seq_along(x@Attributes)) {
                cat("\t",paste0(i, ") "))
                printCapr(x@Attributes[[i]])
                cat("\n")
              }
            } else {
              cat("None", "\n")
            }
          })


#####################################
#Count Summary
####################################

#' @rdname printCapr-method
#' @aliases printCapr,Count-method
#' @export
setMethod("printCapr", "Count",
          function(x){
            cat("Count",x@Occurrence@Type, x@Occurrence@Count)
            if (x@Occurrence@isDistinct) {
              cat(" distinct occurrence(s) of")
            } else{
              cat(" occurrence(s) of")
            }
            lineBreak(4)
            printCapr(x@Criteria)
            lineBreak(4)
            printCapr(x@Timeline)
          })

#####################################
#Group Summary
####################################
#' @rdname printCapr-method
#' @aliases printCapr,Group-method
#' @export
setMethod("printCapr", "Group",
          function(x){
            ty <- x@Type@Type
            if (ty == "AT_LEAST" | ty == "AT_MOST") {
              cat("Having",tolower(ty), x@Type@Count, "of the following criteria")
            } else{
              cat("Having",tolower(ty), "of the following criteria")
            }
            lineBreak(1)
            if (length(x@CriteriaList) > 0) {
              cat("Criteria List")
              lineBreak(3)
              for (i in seq_along(x@CriteriaList)) {
                printCapr(x@CriteriaList[[i]])
              }
            }
            if (length(x@DemographicCriteriaList) > 0) {
              cat("Demographic Criteria List")
              lineBreak(3)
              for (i in seq_along(x@DemographicCriteriaList)) {
                printCapr(x@DemographicCriteriaList[[i]])
              }
            }
            if (length(x@Groups) > 0) {
              cat("Groups List")
              lineBreak(3)
              for (i in seq_along(x@Groups)) {
                printCapr(x@Groups[[i]])
              }
            }
          })

#' Show Contents of a Component
#'
#' This function prints the contents of a component. Note 1/27/21 attributes and some other s4 classes
#' need to be implemented
#'
#' @param x the component s4 class you want to preview
#' @param showFullConceptSetExpressions T/F options to include full details of concept expressions
#' @importFrom methods is
#' @return A console print summarizing the s4 component part
#' @export
printComponent <- function(x, showFullConceptSetExpressions = FALSE){
  lineBreak(1)
  if(methods::is(x) != "Component"){
    stop("The object is not a component")
  }
  cat("Component Type:", x@MetaData@ComponentType, "\n")
  cat("Name:", x@MetaData@Name, "\n")
  cat("Description:", x@MetaData@Description)
  lineBreak(2)
  if (length(x@CriteriaExpression) > 0) {
    if (componentType(x) == "PrimaryCriteria"){
      cat("Criteria List")
      for (i in seq_along(x@CriteriaExpression$CriteriaList)) {
        lineBreak(3)
        cat(paste0(i, ") "))
        printCapr(x@CriteriaExpression$CriteriaList[[i]])
      }
      lineBreak(2)
      printCapr(x@CriteriaExpression$ObservationWindow)
    } else if (componentType(x) == "InclusionRules") {
      for (i in seq_along(x@CriteriaExpression)) {
        cat(paste0(i, ") "))
        printComponent(x@CriteriaExpression[[i]], showFullConceptSetExpressions = showFullConceptSetExpressions)
      }
    } else {
      for (i in seq_along(x@CriteriaExpression)) {
        cat(paste0("Criteria ", i, ")"), "\n")
        printCapr(x@CriteriaExpression[[i]])
      }
    }
  }
  lineBreak(2)
  if (length(x@Limit) > 0) {
    tt <- paste0(names(x@Limit),":")
    lim <- x@Limit[[1]]@Type
    cat(tt, lim)
    lineBreak(2)
  }
  cat("Concept Set Expressions")
  if (length(x@ConceptSetExpression) > 0) {
    for (i in seq_along(x@ConceptSetExpression)) {
      lineBreak(3)
      cat(paste0(i, ") "))
      if (showFullConceptSetExpressions) {
        printCapr(x@ConceptSetExpression[[i]])
      } else {
        cat(x@ConceptSetExpression[[i]]@Name, "\n")
        cat("CodesetId:", x@ConceptSetExpression[[i]]@id)
      }
    }
  } else {
      cat("None")
  }
  lineBreak(1)
}
