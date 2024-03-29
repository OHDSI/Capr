% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/window.R
\name{duringInterval}
\alias{duringInterval}
\title{Function that creates an eventAperture an opening where an event can occur
relative to the index event}
\usage{
duringInterval(
  startWindow,
  endWindow = NULL,
  restrictVisit = FALSE,
  ignoreObservationPeriod = FALSE
)
}
\arguments{
\item{startWindow}{the starting window where an event can occur}

\item{endWindow}{the end window of where an event can occur. This parameter is optional}

\item{restrictVisit}{a logical toggle specifying whether the event should occur on the same visit}

\item{ignoreObservationPeriod}{a logical toggle specifying whether we can consider events outside the
observation period}
}
\value{
An event aperture that can be used in a Capr cohort definition
}
\description{
Function that creates an eventAperture an opening where an event can occur
relative to the index event
}
