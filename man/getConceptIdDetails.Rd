% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UserConceptLookupFn.R
\name{getConceptIdDetails}
\alias{getConceptIdDetails}
\title{get concept id details}
\usage{
getConceptIdDetails(
  conceptIds,
  connectionDetails = NULL,
  connection = NULL,
  vocabularyDatabaseSchema = NULL,
  oracleTempSchema = NULL,
  mapToStandard = TRUE
)
}
\arguments{
\item{conceptIds}{a vector of concept ids}

\item{connectionDetails}{An object of type \code{connectionDetails} as created using the
\code{\link[DatabaseConnector]{createConnectionDetails}} function in the
DatabaseConnector package. Can be left NULL if \code{connection} is
provided.}

\item{connection}{An object of type \code{connection} as created using the
\code{\link[DatabaseConnector]{connect}} function in the
DatabaseConnector package. Can be left NULL if \code{connectionDetails}
is provided, in which case a new connection will be opened at the start
of the function, and closed when the function finishes.}

\item{vocabularyDatabaseSchema}{Schema name where your OMOP vocabulary format resides.
Note that for SQL Server, this should include both the database and
schema name, for example 'vocabulary.dbo'.}

\item{oracleTempSchema}{Should be used in Oracle to specify a schema where the user has write
privileges for storing temporary tables.}

\item{mapToStandard}{logic to map to standard OMOP concept}
}
\value{
a tibble data frame object with conceptId, conceptName, standardConcept,
              standardConceptCaption, invalidReason, invalidReasonCaption, conceptCode,
              domainId, vocabularyId, conceptClassId.
}
\description{
For one or more concept id, get concept id details by querying the
OMOP vocabulary in the database.
}
