% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/window.R
\docType{class}
\name{EventWindow-class}
\alias{EventWindow-class}
\title{An S4 class for a Window}
\description{
A window class provides details on the end points of the timeline
}
\section{Slots}{

\describe{
\item{\code{event}}{a character string either start or end. Identifies the point of reference for the window}

\item{\code{start}}{an endpoint object containing the days and coefficient for the start of the window}

\item{\code{end}}{an endpoint object containing the days and coefficient for the end of the window}

\item{\code{index}}{A character string either start or end. Identifies where the index is relative to the window}
}}

