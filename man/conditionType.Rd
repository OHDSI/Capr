% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attributes-concept.R
\name{conditionType}
\alias{conditionType}
\title{Add a condition type attribute to determine the provenance of the record}
\usage{
conditionType(ids, connection, vocabularyDatabaseSchema)
}
\arguments{
\item{ids}{the concept ids for the attribute}

\item{connection}{a connection to an OMOP dbms to get vocab info about the concept}

\item{vocabularyDatabaseSchema}{the database schema for the vocabularies}
}
\value{
An attribute that can be used in a query function
}
\description{
Add a condition type attribute to determine the provenance of the record
}
