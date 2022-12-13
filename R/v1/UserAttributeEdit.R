#' Function to add Attribute to Query
#'
#' This function edits a expression type class
#' @param query identify the query object to edit
#' @param attribute the attribute to add to the query
#' @return the edited query component
#' @export
#add an attribute to a query
addAttributeToQuery <- function(query, attribute) {
  attrib <- attribute@CriteriaExpression[[1]]
  query@CriteriaExpression[[1]]@Attributes <- append(query@CriteriaExpression[[1]]@Attributes, attrib)
  return(query)
}

