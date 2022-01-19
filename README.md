Capr
====

[![Build Status](https://github.com/OHDSI/Capr/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/Capr/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/Capr/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/Capr?branch=main)

Capr is part of [HADES](https://ohdsi.github.io/Hades).


Cohort definition Application Programming in R

Introduction
============

Capr is an R package to develop and manipulate OHDSI cohort definitions. This package assists in creating a cohort definition that can be compiled by circe-be using CirceR. Cohort definitions developed in Capr are compatible with OHDSI ATLAS. Additionally the package allows for development of cohort design components, sub-items of a cohort design that are meant to be reusable and mutable to assist creating cohorts in study development. 


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


User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/Capr).

PDF versions of the documentation are also available:
* Vignette: [Capr Tutorial](https://raw.githubusercontent.com/OHDSI/Capr/main/inst/doc/Capr_Tutorial.pdf)
* Vignette: [Capr Complex Cohort Example](https://raw.githubusercontent.com/OHDSI/Capr/main/inst/doc/complex-cohort-example.pdf)
* Vignette [Using extended cohort attributes in Capr](https://raw.githubusercontent.com/OHDSI/Capr/main/inst/doc/Capr_Attributes_Extended.pdf)
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
