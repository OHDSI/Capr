% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UserCreateFn.R
\name{createInclusionRules}
\alias{createInclusionRules}
\title{Function creates an Inclusion Rule}
\usage{
createInclusionRules(Name, Contents, Limit, Description = NULL)
}
\arguments{
\item{Name}{a character string naming the inclusion rules, this is required for the object. One should make the name
descriptive of what the group is trying to identify.}

\item{Contents}{a list of component class groups to be inserted into the inclusion rules. Each group
in the list is a separate rule.}

\item{Limit}{how to limit initial events per person}

\item{Description}{a character string describing the count object, this is optional so default is null}
}
\value{
new inclusion rules component.
}
\description{
Function creates a Inclusion Rule from a list of groups, each specifying a unique rule
}
