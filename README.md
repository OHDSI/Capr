# Capr

Cohort definition Application Programming in R

# Introduction

Capr is an R package to develop and manipulate OHDSI cohort definitions. This package assists in creating a cohort definition that can be compiled by circe-be using CirceR. Cohorts developed in Capr are analogous to ATLAS cohorts. Additionally the package allows for development of cohort components, sub-items of the component that are meant to be reusable and mutable to assist creating cohorts in study development. Note this package is an early release and will be made fully available after presentation on the OHDSI community call. 


# System Requirements

Requires R (version 3.5.0 or higher). This package requires a connection to an OMOP vocabulary to query concepts. You will
also require installation of CirceR and DatabaseConnector. For installation details of those packages see: https://github.com/OHDSI/CirceR and https://github.com/OHDSI/DatabaseConnector. 

#Tutorial

A R package vignette is included with this package which can be found through upon install:

```r
vignette("CAPR_tutorial", package = "Capr")
```
