% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UserCreateAttributeFn.R
\name{createQuantityAttribute}
\alias{createQuantityAttribute}
\title{create Quantity Attribute}
\usage{
createQuantityAttribute(Op, Value, Extent = NULL)
}
\arguments{
\item{Op}{defines logic for interpreting the numeric or date value.}

\item{Value}{an integer for the Quantity}

\item{Extent}{an integer for the Quantity only used if the op is bt or !bt}
}
\value{
a component of attribute class
}
\description{
This function creates an Operator attribute for person Quantity. The user selects the type of
operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
only used if the op is bt or !bt.
}
