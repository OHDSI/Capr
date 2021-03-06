% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LowLevelClasses.R
\docType{class}
\name{Query-class}
\alias{Query-class}
\title{An S4 class for a Query}
\description{
A query is a medical concept that can be extracted from a database through a where statement.
 This would include concepts
}
\section{Slots}{

\describe{
\item{\code{Domain}}{the domain where the concepts can be found}

\item{\code{CodesetId}}{the id that matches the concept set expression}

\item{\code{Attributes}}{a list of attributes that modify the query with more information}
}}

