# Capr <a href="https://ohdsi.github.io/Capr"><img src="man/figures/logo.png" align="right" height="110"/></a>



<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/Capr)](https://CRAN.R-project.org/package=Capr) [![codecov.io](https://codecov.io/github/OHDSI/Capr/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/Capr?branch=main) [![Build Status](https://github.com/OHDSI/Capr/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/Capr/actions?query=workflow%3AR-CMD-check)

<!-- badges: end -->

Capr is part of [HADES](https://ohdsi.github.io/Hades/)

# Introduction

The goal of Capr, pronounced 'kay-pr' like the edible flower, is to provide a language for expressing OHDSI Cohort definitions in R code. OHDSI defines a cohort as "a set of persons who satisfy one or more inclusion criteria for a duration of time" and provides a standardized approach for defining them (Circe-be). Capr exposes the standardized approach to cohort building through a programmatic interface in R which is particularly helpful when creating a large number of similar cohorts. Capr version 2 introduces a new user interface designed for readability with the goal that Capr code being a human readable description of a cohort while also being executable on an OMOP Common Data Model.

Learn more about the OHDSI approach to cohort building in the [cohorts chapter of the Book of OHDSI.](https://ohdsi.github.io/TheBookOfOhdsi/Cohorts.html)

# Installation

In the future, Capr will be avaiable on CRAN. For now you can install the current development version of Capr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/Capr")
```

# How to Use

## Examples

Capr uses many defaults that match the defaults in Atlas. Creating a simple cohort is a single line of code. As an example we will define a cohort of new users of diclofenac described in the [Book of OHDSI.](https://ohdsi.github.io/TheBookOfOhdsi/SuggestedAnswers.html#Cohortsanswers)

### Simple diclofenac cohort

``` r
library(Capr)

# Define concepts sets with cs()
diclofenac <- cs(descendants(1124300))

ch <- cohort(
  entry = drugEra(diclofenac)
)

ch
#> Formal class 'Cohort' [package "Capr"] with 4 slots
#>   ..@ entry    :Formal class 'CohortEntry' [package "Capr"] with 5 slots
#>   ..@ attrition:Formal class 'CohortAttrition' [package "Capr"] with 2 slots
#>   ..@ exit     :Formal class 'CohortExit' [package "Capr"] with 2 slots
#>   ..@ era      :Formal class 'CohortEra' [package "Capr"] with 3 slots
```

### Adding more complexity

We can make more complex cohorts by adding a window of continuous observation and a custom cohort exit. The following information was added to the diclofenac cohort:

-   Ages 16 or older
-   With at least 365 days of continuous observation prior to exposure
-   With cohort exit defined as discontinuation of exposure (allowing for a 30-day gap)

``` r
diclofenac <- cs(descendants(1124300))

ch <- cohort(
  entry = entry(drugEra(diclofenac, age(gte(16))),
                observationWindow = continuousObservation(-365L, 0L)),
  exit = drugExit(diclofenac)
)

ch
#> Formal class 'Cohort' [package "Capr"] with 4 slots
#>   ..@ entry    :Formal class 'CohortEntry' [package "Capr"] with 5 slots
#>   ..@ attrition:Formal class 'CohortAttrition' [package "Capr"] with 2 slots
#>   ..@ exit     :Formal class 'CohortExit' [package "Capr"] with 2 slots
#>   ..@ era      :Formal class 'CohortEra' [package "Capr"] with 3 slots
```

### Adding cohort attrition

Users can also add attrition to the cohort by specifying inclusion and exclusion criteria to modify the cohort entry. The following exclusion criteria were added to the diclofenac cohort:

-   Without prior exposure to any NSAID (Non-Steroidal Anti-Inflammatory Drug)
-   Without prior diagnosis of cancer

``` r
diclofenac <- cs(descendants(1124300), name = "diclofenac")
nsaid <- cs(descendants(21603933), name = "nsaid")
cancer <- cs(descendants(443392), name = "cancer")

ch <- cohort(
  entry = entry(drugEra(diclofenac, age(gte(16))),
                observationWindow = continuousObservation(-365L, 0L)),
  attrition = attrition(withAll(
    exactly(0, drug(nsaid), eventStarts(-Inf, 0, index = "startDate")),
    exactly(0, condition(cancer), eventStarts(-Inf, 0, index = "startDate"))
  )),
  exit = exit(
    endStrategy = drugExit(diclofenac, persistenceWindow = 30)
)

ch
#> Formal class 'Cohort' [package "Capr"] with 4 slots
#>   ..@ entry    :Formal class 'CohortEntry' [package "Capr"] with 5 slots
#>   ..@ attrition:Formal class 'CohortAttrition' [package "Capr"] with 2 slots
#>   ..@ exit     :Formal class 'CohortExit' [package "Capr"] with 2 slots
#>   ..@ era      :Formal class 'CohortEra' [package "Capr"] with 3 slots
```

## Save cohort as JSON

OHDSI standard cohorts are represented as json files and can be copy and pasted into Atlas.

``` r

path <- file.path(tempdir(), "diclofenacCohort.json")

writeCohort(ch, path)

cat(substr(readr::read_file(path), 1, 100))
#> {
#>   "ConceptSets": [
#>     {
#>       "id": 0,
#>       "name": "diclofenac",
#>       "expression": {
#> 
```

### Fill in missing concept set details

Users can build valid cohorts with minimal concept information, only supplying a concept id and name. The example below shows the minimal concept set input for Capr.

``` r

diclofenac <- cs(descendants(1124300), name = "diclofenac")

cat(as.json(diclofenac))
#> {
#>   "id": "11d012608fce118593830a3039042e56",
#>   "name": "diclofenac",
#>   "expression": {
#>     "items": [
#>       {
#>         "concept": {
#>           "CONCEPT_ID": 1124300,
#>           "CONCEPT_NAME": "",
#>           "STANDARD_CONCEPT": "",
#>           "STANDARD_CONCEPT_CAPTION": "",
#>           "INVALID_REASON": "",
#>           "INVALID_REASON_CAPTION": "",
#>           "CONCEPT_CODE": "",
#>           "DOMAIN_ID": "",
#>           "VOCABULARY_ID": "",
#>           "CONCEPT_CLASS_ID": ""
#>         },
#>         "isExcluded": false,
#>         "includeDescendants": true,
#>         "includeMapped": false
#>       }
#>     ]
#>   }
#> }
```

However, when saving cohorts it is helpful to fill in the concept details. This requires a live connection to an OMOP CDM database that includes the vocabularies used in the cohort definition.

``` r
con <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
diclofenac <- getConceptSetDetails(diclofenac, con, vocabularyDatabaseSchema = "main")
cat(as.json(diclofenac))
#> {
#>   "id": "11d012608fce118593830a3039042e56",
#>   "name": "diclofenac",
#>   "expression": {
#>     "items": [
#>       {
#>         "concept": {
#>           "CONCEPT_ID": 1124300,
#>           "CONCEPT_NAME": "Diclofenac",
#>           "STANDARD_CONCEPT": "S",
#>           "STANDARD_CONCEPT_CAPTION": "Standard",
#>           "INVALID_REASON": "V",
#>           "INVALID_REASON_CAPTION": "Valid",
#>           "CONCEPT_CODE": "3355",
#>           "DOMAIN_ID": "Drug",
#>           "VOCABULARY_ID": "RxNorm",
#>           "CONCEPT_CLASS_ID": "Ingredient"
#>         },
#>         "isExcluded": false,
#>         "includeDescendants": true,
#>         "includeMapped": false
#>       }
#>     ]
#>   }
#> }
```

### Generating Capr Cohorts

Once a Capr cohort has been constructed, the user can generate this cohort definition on an OMOP CDM connection. It is suggested to use [CohortGenerator](https://github.com/OHDSI/CohortGenerator) and [CirceR](https://github.com/OHDSI/CirceR) to generate Capr cohorts on a database.

## Building Capr Templates

A Capr cohort template is a function that always returns a Capr cohort. It can accept arguments that can be used to parameterize any part of a cohort definition. Capr cohort templates are the recommended approach for building large numbers of similar cohorts in R.

``` r

# A Capr cohort template is a function that returns a cohort
drugEraTemplate <- function(ingredientConceptId) {
  
  drugConceptSet <- cs(descendants(ingredientConceptId))
  
  cohort(
    entry = entry(drugEra(drugConceptSet, age(gte(16))),
                  observationWindow = continuousObservation(-365L, 0L)),
    exit = drugExit(drugConceptSet, persistenceWindow = 30)
  )
}


library(dplyr, warn.conflicts = FALSE)

# create a cohort for every single ingredient
df <- DBI::dbGetQuery(con, 
                      "Select * from concept where concept_class_id = 'Ingredient'") %>% 
  tibble() %>% 
  select(concept_id, concept_name) %>% 
  mutate(capr_cohort = purrr::map(concept_id, drugEraTemplate)) %>% 
  mutate(cohort_json = purrr::map_chr(capr_cohort, as.json))

df
#> # A tibble: 91 × 4
#>    concept_id concept_name    capr_cohort cohort_json                           
#>         <dbl> <chr>           <list>      <chr>                                 
#>  1    1557272 Alendronate     <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  2     708298 Midazolam       <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  3     701322 Memantine       <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  4     723013 Diazepam        <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  5    1129625 Diphenhydramine <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  6    1149196 Cetirizine      <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  7    1149380 fluticasone     <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  8    1150770 Astemizole      <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#>  9    1150836 Terfenadine     <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#> 10    1124300 Diclofenac      <Cohort>    "{\n  \"ConceptSets\": [\n    {\n    …
#> # … with 81 more rows
```

The capr_cohort column of the dataframe is a list of Capr cohort object. The cohort_json column contains the json specifications for each cohort.

``` r
DatabaseConnector::disconnect(con)
```

# User Documentation

Documentation can be found on the [package website](https://ohdsi.github.io/Capr).

PDF versions of the documentation are also available: 

- Vignette: [Using Capr](https://raw.githubusercontent.com/OHDSI/Capr/main/inst/doc/Using-Capr.pdf) 
- Vignette: [Examples](https://raw.githubusercontent.com/OHDSI/Capr/main/inst/doc/Examples.pdf) 
- [Package manual](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/Capr.pdf)

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
