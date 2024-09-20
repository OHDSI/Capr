# Capr <a href="https://ohdsi.github.io/Capr/"><img src="man/figures/logo.png" align="right" height="90"/></a>

<!-- badges: start -->

[![codecov.io](https://codecov.io/github/OHDSI/Capr/coverage.svg?branch=main)](https://app.codecov.io/gh/OHDSI/Capr?branch=main) [![Build Status](https://github.com/OHDSI/Capr/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/Capr/actions?query=workflow%3AR-CMD-check)

<!-- badges: end -->

Capr is part of [HADES](https://ohdsi.github.io/Hades/)

# Introduction

The goal of Capr, pronounced 'kay-pr' like the edible flower, is to provide a language for expressing OHDSI Cohort definitions in R code. OHDSI defines a cohort as "a set of persons who satisfy one or more inclusion criteria for a duration of time" and provides a standardized approach for defining them (Circe-be). Capr exposes the standardized approach to cohort building through a programmatic interface in R which is particularly helpful when creating a large number of similar cohorts. Capr version 2 introduces a new user interface designed for readability with the goal that Capr code being a human readable description of a cohort while also being executable on an OMOP Common Data Model.

Learn more about the OHDSI approach to cohort building in the [cohorts chapter of the Book of OHDSI.](https://ohdsi.github.io/TheBookOfOhdsi/Cohorts.html)

# Installation


Users can install the current development version of Capr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/Capr")
```


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

# Acknowledgements

-   This package is maintained by Martin Lavallee and Adam Black
-   Guidance and support for the original development of Capr came from Lee Evans and LTS Computing LLC