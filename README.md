# Capr <a href="https://ohdsi.github.io/Capr/"><img src="man/figures/logo.png" align="right" height="90"/></a>

<!-- badges: start -->

[![codecov.io](https://codecov.io/github/OHDSI/Capr/coverage.svg?branch=main)](https://app.codecov.io/gh/OHDSI/Capr?branch=main) [![Build Status](https://github.com/OHDSI/Capr/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/Capr/actions?query=workflow%3AR-CMD-check)

<!-- badges: end -->

Capr is part of [HADES](https://ohdsi.github.io/Hades/)

# Introduction

The goal of Capr, pronounced 'kay-pr' like the edible flower, is to provide a language for expressing OHDSI Cohort definitions in R code. OHDSI defines a cohort as "a set of persons who satisfy one or more inclusion criteria for a duration of time" and provides a standardized approach for defining them (Circe-be). Capr exposes the standardized approach to cohort building through a programmatic interface in R which is particularly helpful when creating a large number of similar cohorts. Capr version 2 introduces a new user interface designed for readability with the goal that Capr code being a human readable description of a cohort while also being executable on an OMOP Common Data Model.

This package has been extended under the name Capr-PHEMS for the PHEMS consortium to facilitate the cohort building process via a template script and specific configurations, as well as providing extra functionality to enable querying the frequency of concept sets on a person and record level and to enable checking the standardness of concepts in a set or list of concepts, thereby making identification of non-standard concepts much more straightforward.

Learn more about the OHDSI approach to cohort building in the [cohorts chapter of the Book of OHDSI.](https://ohdsi.github.io/TheBookOfOhdsi/Cohorts.html)

# Installation


Users can install the current development version of Capr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/Capr")
```

For Capr-PHEMS, it is recommended to clone the repo and work from the template script cohortCapr.R, whilst using the Capr-PHEMS.Rproj as the project.

# Getting started with Capr-PHEMS
**Prerequisites**

Please make sure you have the following:
-   R version >= 4.4.0
-   RStudio >= 2024.04.2
-   (CRAN library) renv >= 1.0.7


1.  Clone the repo:
```
git clone https://github.com/thehyve/Capr-PHEMS.git
```

2.  Navigate to ./inst/config/. Here you will find two sample configuration files; config-sample.yml and connection_config-sample.yml

3.  Copy these files using: 
```
cp config-sample.yml config.yml
``` 
and 
```
cp connection_config-sample.yml connection_config.yml
``` 
and fill in your configuration details.

4a.  Open RStudio and verify renv >= 1.0.7 is installed by calling packageVersion("renv"). If not installed, call install.packages("renv)

4b.  In the top-right "Project" dropdown menu, select "Open Project" and open ./Capr-PHEMS.Rproj

4c.  .Rprofile will be sourced when opening the project; this will trigger renv::init() to initialize the environment. When prompted whether to use a DESCRIPTION file for dependency discovery, select option 1 to use the DESCRIPTION file. Next, you may be prompted the project already has a lockfile with another set of options. Here also select option 1 to restore the project from the lockfile. This will install all required packages to the environment. Your renv is now activated.

5.  A template script is provided under ./inst/templates/ as both an R script and .Rmd file. The code between these files is identical. It is recommended to use the .Rmd file, so chunks of code can easily be run at will. These chunks of code are self-explanatory.

# User Documentation

Documentation can be found on the [package website](https://ohdsi.github.io/Capr/).


PDF versions of the documentation are also available:

-   Vignette: [Using Capr](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/Using-Capr.pdf)
-   Vignette: [Capr Examples](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/Examples.pdf)
-   Vignette: [Working with Concept Sets in Capr](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/Capr-conceptSets.pdf)
-   Vignette: [Capr for Templating Cohort Definitions](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/capr_templates.pdf)
-   Vignette: [Capr components](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/capr_objects.pdf)
-   [Design Document](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/capr_design.pdf)
-   [Package manual](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/Capr.pdf)

# Support

-   Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
-   We use the <a href="https://github.com/OHDSI/Capr/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

# Contributing

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

# License

Capr is licensed under Apache License 2.0

# Development

Capr is being developed in R Studio.
Capr-PHEMS is beind developed in R Studio.

# Acknowledgements

-   The original Capr package is maintained by Martin Lavallee and Adam Black
-   Guidance and support for the original development of Capr came from Lee Evans and LTS Computing LLC
-   Additional scripts and templates specific to Capr-PHEMS are maintained by Guus Wilmink
