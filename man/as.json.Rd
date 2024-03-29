% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conceptSet.R
\docType{methods}
\name{as.json}
\alias{as.json}
\alias{as.json,ConceptSet-method}
\title{Coerce Capr object to json}
\usage{
as.json(x, pretty = TRUE, ...)

\S4method{as.json}{ConceptSet}(x, pretty = TRUE, ...)
}
\arguments{
\item{x}{the capr object}

\item{pretty}{a toggle to make the json look nice, part of jsonlite}

\item{...}{additional arguments passes to jsonlite::toJSON}
}
\description{
Coerce Capr object to json
}
