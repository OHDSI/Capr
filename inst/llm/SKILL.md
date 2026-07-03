---
name: capr-cohort-generation
description: >
  Generate OHDSI cohort definitions as Capr R code from natural-language descriptions.
  Use when the user asks to build, translate, or modify a cohort definition, phenotype,
  or study population using Capr, R, or OHDSI/Atlas cohort JSON.
---

# Capr Cohort Generation

Translate a natural-language cohort description into a validated Capr R script that compiles to
OHDSI (Circe/Atlas-compatible) cohort JSON.

## Requirements

- R with the **Capr** package installed (CirceR comes with it). No database connection is needed
  at any point — Capr builds and serializes cohort definitions entirely in memory.
- **`CAPR_REFERENCE.md`** (same directory as this file). Read it before writing any code; it is
  the only source of truth for the Capr API.

## Non-Negotiable Rules

1. **Use only functions and arguments documented in `CAPR_REFERENCE.md`.** If something seems
   missing, say so — do not improvise API.
2. **Never write a concept ID from memory.** This includes clinical concepts and type / unit /
   status / provider-specialty IDs. Concept IDs come from the user or are placeholders. If you
   know a likely candidate ID, put it in a comment marked `verify in ATHENA` — never in the code.
3. **Always produce the function-form output** described below, even for a one-off cohort.
4. **Always execute the generated file before delivering it.** Code that has not run is not done.

## Workflow

### Step 1 — Clarify before coding

Read the request and identify: the entry event, inclusion/exclusion criteria, exit strategy, era
logic, and the list of clinical concepts involved (each becomes a concept-set parameter).

Then apply the two gates in `CAPR_REFERENCE.md` § *Agent Workflow*:

- **The clarification checklist.** If any required design decision is unspecified and not clearly
  implied, **stop — do not write code yet**. Send a single message listing each open question
  with a proposed default the user can accept in one word. Wait for the answer. Never mix the
  questions and the generated code in one response.
- **The wrong-tool signals.** If the request needs cohort set operations, cross-event arithmetic,
  or ordinal logic beyond first occurrence, propose the decomposition pattern from the reference
  before generating anything.

Micro-decisions below the question threshold (e.g. exact window endpoints implied by convention)
are not worth a question: decide conventionally and record each one as an `# ASSUMPTION:` comment
on the relevant line.

### Step 2 — Generate the cohort function

Every deliverable is one R file with this structure:

```r
library(Capr)

#' Build the <phenotype> cohort definition
#'
#' <one-paragraph restatement of the cohort logic in plain English>
#'
#' @param t2dmCs  ConceptSet for type 2 diabetes (entry event)
#' @param insulinCs ConceptSet for insulin exposures (exclusion)
#' @return A Capr Cohort object; serialize with writeCohort() or compile()
createT2dmCohort <- function(t2dmCs, insulinCs) {
  cohort(
    entry = entry(
      conditionOccurrence(t2dmCs),
      observationWindow = continuousObservation(priorDays = 365L),  # ASSUMPTION: 365d washout
      primaryCriteriaLimit = "First"                                # ASSUMPTION: first occurrence
    ),
    attrition = attrition(
      "no prior insulin" = withAll(
        exactly(0, drugExposure(insulinCs),
                duringInterval(eventStarts(-Inf, -1)))              # ASSUMPTION: excludes index day
      ),
      expressionLimit = "First"
    ),
    exit = exit(endStrategy = observationExit()),
    era = era(eraDays = 0L)
  )
}

# ---- Example usage ----------------------------------------------------------
# Replace the placeholder concept sets with real ones before generating the
# cohort: build with cs(<concept ids>, name = ...), import an Atlas export with
# readConceptSet(<path>), or use ConceptSet objects already in your session.
t2dmCs    <- cs(0L, name = "Type 2 diabetes mellitus [PLACEHOLDER]")  # TODO: real concept set
insulinCs <- cs(0L, name = "Insulin [PLACEHOLDER]")                   # TODO: real concept set

cohortDef <- createT2dmCohort(t2dmCs, insulinCs)
writeCohort(cohortDef, "t2dm_cohort.json")
```

Contract:

- **One parameter per concept set**, typed `ConceptSet`, named `<concept>Cs`, documented with its
  role in the cohort. Type / unit / status filters take integer id vectors — pass those as
  parameters too when the user hasn't supplied the ids.
- **Parameterize concept sets always; everything else only on request.** Washout days, windows,
  persistence gaps etc. are hardcoded in the body with `# ASSUMPTION:` comments — do not add
  knob parameters unless the user asks for variants over that knob.
- **Return the `Cohort` object.** Serialization happens in the example block, not in the function.
- **Placeholders are `cs(0L, name = "<name> [PLACEHOLDER]")`** — executable (so validation works)
  but impossible to mistake for a real definition. Skip placeholders only when the user has told
  you the real variable names, file paths, or concept ids to use.
- If the user wants **many structurally identical cohorts**, show the batch pattern after the
  single example:

  ```r
  conceptSetList <- list(t2dm = t2dmCs, t1dm = t1dmCs)
  cohorts <- lapply(conceptSetList, createT2dmCohort, insulinCs = insulinCs)
  for (nm in names(cohorts)) writeCohort(cohorts[[nm]], paste0(nm, "_cohort.json"))
  ```

### Step 3 — Validate by executing

1. Run the file: `Rscript <file>.R`. It must run end-to-end — including the placeholder example
   block — and write the JSON file. No database is required.
2. If it errors: fix the code using `CAPR_REFERENCE.md` (the error usually means an argument or
   function outside the documented API) and re-run. If the same error survives three fix
   attempts, stop and show the user the error instead of thrashing.
3. Confirm Circe accepts the output — this catches structural problems R execution cannot:

   ```r
   Rscript -e 'json <- paste(readLines("t2dm_cohort.json"), collapse = "\n");
     invisible(CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(json),
       CirceR::createGenerateOptions(generateStats = FALSE)))'
   ```

   Success = SQL generates without error. Do not deliver a cohort that fails this check.

### Step 4 — Deliver

Report to the user, in this order:

1. **What was built** — one-sentence restatement of the cohort logic.
2. **Assumptions** — every `# ASSUMPTION:` from the code, as a list they can veto.
3. **Placeholders** — which concept sets are placeholders and how to swap in real ones
   (`cs()`, `readConceptSet()`, or existing objects).
4. **Optional hydration** — if they have an OMOP CDM connection, `getConceptSetDetails()` (or the
   `connection` argument on ids-based attributes) fills in concept names so Atlas displays them;
   purely cosmetic, never required.
5. The JSON file can be imported directly into Atlas.
