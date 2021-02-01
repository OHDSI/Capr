# Capr

Cohort definition Application Programming in R

# Introduction

Capr is an R package to develop and manipulate OHDSI cohort definitions. This package assists in creating a cohort definition that can be compiled by circe-be using CirceR. Cohort definitions developed in Capr are compatible with OHDSI ATLAS. Additionally the package allows for development of cohort design components, sub-items of a cohort design that are meant to be reusable and mutable to assist creating cohorts in study development. 

Note this package is an early release and is still under development. Capr was developed by LTS Computing LLC and has been released as open source for the OHDSI community.


# System Requirements

Installation
Requires R (version 3.5.0 or higher). 
You will also require installation of OHDSI CirceR and OHDSI DatabaseConnector. For installation details of those packages see: https://github.com/OHDSI/CirceR and https://github.com/OHDSI/DatabaseConnector. 

# Installation Instructions

```r
install.packages("remotes")
remotes::install_github("ohdsi/Capr")
```

# Operational
This package requires a connection to an OMOP vocabulary database to query concepts. The database connection is made using OHDSI DatabaseConnector.

# Contributors
Martin Lavallee, Lee Evans 

# Tutorial

An R package vignette is included with this package which can be found upon install:

```r
vignette("CAPR_tutorial", package = "Capr")
```
