
Capr 2.1.0
==========
- First CRAN release

Capr 2.0.2
==========
- fix tests for CRAN

Capr 2.0.5
==========
- change query functions to match known syntax (i.e. drug => drugExposure, condition => conditionOccurrence)
- require a name for `cs()`
- improve documentation (add vignette for query, count and group)


Capr 2.0.4
==========
- hot fix add procedure occurrence into query


Capr 2.0.3
==========
- add opAttribute super class
- add version dependency for purrr v1.0.1 for list_flatten

Capr 2.0.2
==========
- fix issue with first exposure

Capr 2.0.1
==========
- additions and corrections to vignettes and documentation
- compile cohort function added
- fix concept set print 
- prep for CRAN submission

Capr 2.0.0
==========
New user interface for cohort construction
    - `cs()` function to build and combine concept ids into sets
    - improved readability code to build cohort definition
    - coercion of Capr object to json to save as file for study
    - maintained support for Capr templates

Capr 1.0.4
==========
1. Bug Fixes
  - allow for numeric attributes stop automatic integer coersion
  - allow for death query without CSE in Censoring and Primary Criteria

Capr 1.0.3
==========

1. Fix bug in guid generation...did not create unique guid each time
2. Add function to create cohort data frame for CohortGenerator and CohortDiagnostics
3. Fix pdf documentation in readme

Capr 1.0.2
==========

1. Fix Hades and github logo on package down site   
2. Fix links to access vignettes from README
3. Other minor changes to prep for PhenotypePheburary


Capr 1.0.1
==========

1. Minor edits to documentation   
2. Removed oracleTempSchema from functions that access vocabulary schema of OMOP database


Capr 1.0.0
==========

Initial version of Capr package to functionalize the creation of cohort
definitions of OMOP CDM mapped data. Features include:     

1. Lookup functions to browse and identify concepts from OMOP Vocabularies

2. Creation functions of cohort definitions sub-components (concept set expressions,
queries, counts, groups) and main-components (primary criteria, additional criteria,
inclusion rules, censoring criteria and cohort eras).

3. Save and load component parts of cohort definition

4. Import circe json and convert into Capr objects

5. Print Capr R script that produces equivalent cohort definitions in Capr   

6. Show functions for Capr objects

