#low level create functions

#' createComponent
#' @param Name a name
#' @param Description a description default null
#' @param ComponentClass match an arg from vector
#' @param CriteriaExpression include anything for the criteria can be null
#' @param Limit determine limit
#' @param ConceptSetExpression add anny concept set expressions
#' @include lowLevelClasses.R
#' @importFrom methods new
createComponent <- function(Name,
                            Description = NULL,
                            ComponentClass = c("ConceptSetExpression",
                                               "Group",
                                               "Query",
                                               "Count",
                                               "Attribute",
                                               "PrimaryCriteria",
                                               "AdditionalCriteria",
                                               "InclusionRules",
                                               "EndStrategy",
                                               "CensoringCriteria",
                                               "CohortEra",
                                               "Empty"),
                            CriteriaExpression = NULL,
                            Limit = NULL,
                            ConceptSetExpression = NULL){
  #if description is null than make string NA
  if(is.null(Description)){
    Description <- NA_character_
  }
  #create the meta data
  md <- new("MetaData",
            ComponentClass = ComponentClass,
            Name = Name,
            Description = Description)
  if(ComponentClass == "PrimaryCriteria"){
    Limit <- list('PrimaryCriteriaLimit' = new('Limit', Type = Limit))
  } else if(ComponentClass == "AdditionalCriteria"){
    Limit <- list('QualifiedLimit' = new('Limit', Type = Limit))
  } else if(ComponentClass == "InclusionRules"){
    Limit <- list('ExpressionLimit' = new('Limit', Type = Limit))
  } else{
    Limit <- list()
  }
  if(is.null(CriteriaExpression)){
    CriteriaExpression <- list()
  }
  if(is.null(Limit)){
    Limit<- list()
  }
  if(is.null(ConceptSetExpression)){
    ConceptSetExpression <- list()
  }
  comp <- new("Component",
              MetaData = md,
              CriteriaExpression = CriteriaExpression,
              Limit = Limit,
              ConceptSetExpression = ConceptSetExpression)
  return(comp)
}

#######################
#Create Attributes
#########################

#'  createOpAttribute
#' @param Name a name
#' @param Op a type of operator
#' @param Value a value either integer or character for dates
#' @param Extent only if Op is bt or !bt, otherwise NULL. Value is either integer or character for dates
#' @include lowLevelClasses.R
#' @importFrom methods new
#' @export
createOpAttribute <- function(Name, Op, Value, Extent = NULL){
  Op <- mapOperator(Op)
  if(Op == "bt" | Op == "!bt"){
    if(is.null(Extent)){
      stop("need a value and an extent for between and not between")
    }
  }

  if(is.null(Extent)){
    Extent <- NA_integer_
  }
  Contents <- list(Value=Value,Extent=Extent)

  if(grepl("Date", Name)){
    Contents <- lapply(Contents,as.character)
  } else{
    Contents <- lapply(Contents,as.integer)
  }

  att <- new("OpAttribute",
             Name =Name,
             Op = Op,
             Contents =Contents)
  comp <- createComponent(Name = "OpAttribute",
                          ComponentClass = "Attribute",
                          CriteriaExpression = list(att))
  return(comp)

}



#'  createSourceConceptAttribute
#' @param Domain the type of domain for the source concept
#' @param ConceptSetExpression the concept set expression component to add
#' @include lowLevelClasses.R
#' @importFrom methods new
#' @export
createSourceConceptAttribute <- function(Domain,ConceptSetExpression){
  att <- new("SourceConceptAttribute",
             Name = paste0(Domain, "SourceConcept"),
             SourceCodesetId = ConceptSetExpression@ConceptSetExpression[[1]]@id)
  comp <- createComponent(Name = "SourceConceptAttribute",
                          ComponentClass = "Attribute",
                          CriteriaExpression = list(att),
                          ConceptSetExpression = ConceptSetExpression@ConceptSetExpression)
  return(comp)
}

#Create Concept Attribute
#'  createConceptAttribue
#' @param conceptIds the list of ids to lookup, need OMOP vocabulary connection
#' @param mapToStandard whether to map concept ids to standard or leave as is default is TRUE
#' @param name is the name of the attribute
#' @include lowLevelClasses.R
#' @importFrom methods new
#' @export
createConceptAttribute <- function(conceptIds, mapToStandard = TRUE, name){
  concepts <- lookupConceptIds(conceptIds = conceptIds, mapToStandard = mapToStandard)
  concepts <- concepts[,c("CONCEPT_CODE", "CONCEPT_ID", "CONCEPT_NAME", "DOMAIN_ID", "VOCABULARY_ID")]
  concepts <- unname(apply(concepts,1,as.list))
  concepts <- lapply(concepts, function(x) {
    x$CONCEPT_ID <- as.integer(x$CONCEPT_ID)
    return(x)})
  att <- new("ConceptAttribute",
             Name = name,
             Concepts = concepts)
  comp <- createComponent(Name = "ConceptAttribute",
                          ComponentClass = "Attribute",
                          CriteriaExpression = list(att))
  return(comp)
}
#'  createLogicalAttribue
#' @param name is the name of the attribute
#' @param logic whether the logic is true or false, default is true
#' @include lowLevelClasses.R
#' @importFrom methods new
#' @export
createLogicalAttribute <- function(name, logic = TRUE){
  att <- new("LogicAttribute", Name = name, Logic = logic)
  comp <- createComponent(Name = "LogicAttribute",
                          ComponentClass = "Attribute",
                          CriteriaExpression = list(att))
  return(comp)
}


#create query function
#'  createQuery
#' @param Domain list the domain from the table we are searching in the query
#' @param Component add the concept set expression we want to query
#' @param attributeList a list of attribute class components to add, if not attributes keep null
#' @param Name is the name of query, optional
#' @param Description an optional description of the query
#' @include lowLevelClasses.R
#' @importFrom methods new
createQuery <- function(Domain,
                        Component = NULL,
                        attributeList = NULL,
                        Name = NULL,
                        Description = NULL){
  #if name null use a generic name
  if(is.null(Name)){
    Name <- paste(Domain, "Query")
  }
  #create query Class
  if(is.null(Component)){ #if null only use domain
    query <- new("Query",
                 Domain = Domain)
    cse <- list()
  } else{ #else link the codeset Id
    query <- new("Query",
                 Domain = Domain,
                 CodesetId = Component@ConceptSetExpression[[1]]@id)
    cse <- Component@ConceptSetExpression
  }

  #check attributes are all attributes
  check <- lapply(attributeList,componentClass)
  if(!all(grepl("Attribute",check))){
    stop("not all additional parameters are attributes")
  }

  if(!is.null(attributeList)){
    query@Attributes <- lapply(attributeList, function(x) x@CriteriaExpression[[1]])
    nm <- sapply(attributeList, function(x) x@MetaData@Name)
    idx <- c(grep("SourceConceptAttribute", nm), grep("CorrelatedCriteria", nm))
    if(length(idx) >0){
      for(i in idx){
        cse <- append(cse, attributeList[[i]]@ConceptSetExpression)
      }
    }
  }

  comp <- createComponent(Name = Name,
                          Description = Description,
                          ComponentClass = "Query",
                          CriteriaExpression = list(query),
                          ConceptSetExpression = cse)
  return(comp)
}

