Capr
====

[![Build Status](https://github.com/OHDSI/Capr/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/<reponame>/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/Capr/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/Capr?branch=main)

Capr is part of [HADES](https://ohdsi.github.io/Hades).


Cohort definition Application Programming in R

Introduction
============

Capr is an R package to develop and manipulate OHDSI cohort definitions. This package assists in creating a cohort definition that can be compiled by circe-be using CirceR. Cohort definitions developed in Capr are compatible with OHDSI ATLAS. Additionally the package allows for development of cohort design components, sub-items of a cohort design that are meant to be reusable and mutable to assist creating cohorts in study development. 

Note this package is an early release and is still under development. Capr was developed by LTS Computing LLC and has been released as open source for the OHDSI community.


System Requirements
============
Requires R (version 3.6.0 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in Capr require Java. Capr requires a connection to an OMOP vocabulary database to query concepts. 

Installation
=============
1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install Capr:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/Capr")
  ```
  
3. Optionally, run this to check if Capr was correctly installed:

  ```r
  connectionDetails <- createConnectionDetails(dbms="postgresql",
                                               server="my_server.org",
                                               user = "joe",
                                               password = "super_secret")

  checkCmInstallation(connectionDetails)
  ```
  
  Where dbms, server, user, and password need to be changed to the settings for your database environment. Type
  
  ```r
  ?createConnectionDetails
  ``` 
  
  for more details on how to configure your database connection.


User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/Capr).

PDF versions of the documentation are also available:
* Vignette: [Running multiple analyses at once using the CohortMethod package](https://raw.githubusercontent.com/OHDSI/CohortMethod/master/inst/doc/MultipleAnalyses.pdf)
* Package manual: [Capr.pdf](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/Capr.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/Capr/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
Capr is licensed under Apache License 2.0

Development
===========
Capr is being developed in R Studio.
