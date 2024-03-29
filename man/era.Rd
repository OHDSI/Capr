% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohort.R
\name{era}
\alias{era}
\title{Create a Cohort Era class object}
\usage{
era(eraDays = 0L, studyStartDate = NULL, studyEndDate = NULL)
}
\arguments{
\item{eraDays}{a numeric that specifies the number of days for the era padding}

\item{studyStartDate}{a date string that specifies the starting date of registration}

\item{studyEndDate}{a date string that specifies the end date of registration}
}
\value{
a S4 CohortEra class object defining the eras of the cohort definition
}
\description{
The Cohort Era depicts the time span of the cohort. The Censor Window includes
the date window for which we register events. The Collapse Settings identify the era padding
between events before exiting a cohort.
}
