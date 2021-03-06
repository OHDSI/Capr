% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LowLevelBuildLangFn.R
\name{createAttributeCall}
\alias{createAttributeCall}
\title{Get attributes from cohort expression and prepare R language}
\usage{
createAttributeCall(x, objNm)
}
\arguments{
\item{x}{the circe cohort definition}

\item{objNm}{the naming convention to assign the object}
}
\value{
r language to generate the concept set expressions of the cohort
}
\description{
This function creates attributes within the queries and turns them into
R language which will then create them as a CAPR object
}
