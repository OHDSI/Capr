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

1. **Every request starts with the Step 1 clarification message — even when the request looks
   fully specified.** Send the questions, wait for the user's reply, and only then write code.
   Never mix the questions and generated code in one response.
2. **Every generated file opens with a `Scope check` comment block** (template in Step 2)
   recording every design decision from the Step 1 checklist: the index event, the OMOP domain
   of every criterion, and one line per applicable design choice (entry limit, washout,
   index-day boundary, windows, sequencing, exit strategy). A line is tagged
   `(confirmed by user -- "<their answer>")` only when the user answered it in reply to *this*
   request's Step 1 message; everything else is `(ASSUMED -- <why>)`. A script missing this
   block is an incomplete deliverable, exactly like one missing `cohort()`.
3. **Use only functions and arguments documented in `CAPR_REFERENCE.md`.** If something seems
   missing, say so — do not improvise API.
4. **Never write a concept ID from memory.** This includes clinical concepts and type / unit /
   status / provider-specialty IDs. Concept IDs come from the user or are placeholders. If you
   know a likely candidate ID, put it in a comment marked `verify in ATHENA` — never in the code.
   **Every placeholder concept set needs its own distinct placeholder ID** — see the placeholder
   pattern in Step 2. Reusing one id across concept sets makes Capr silently merge them.
5. **Always produce the function-form output** described below, even for a one-off cohort.
6. **Always execute the generated code before delivering it.** Code that has not run is not done.
7. **Say so when the cohort is not expressible in Capr/Circe.** Check every request against the
   wrong-tool signals in `CAPR_REFERENCE.md` before writing code. A definition that compiles but
   means something different from what the user asked for is worse than no code — never deliver
   a silent approximation; state the mismatch and propose the decomposition pattern instead.

## Delivery Integration (host frameworks)

The generation contract is invariant: the Step 1 clarification message, the function-form
output with its `Scope check` block, validation by execution, and every Non-Negotiable Rule
above apply in every context. Where the deliverable *lives* and how it is *serialized* are
not: when the project you are working in supplies its own instructions for integrating Capr
definitions — another skill, an `AGENTS.md`/`CLAUDE.md`, or framework documentation (e.g. a
Picard/Ulysses study repository) — follow those instructions for the delivery step instead of
the default one-file-per-cohort with a `writeCohort()` example block. Typical overrides:

- **Target**: appending the cohort function and its invocation to an existing project script
  rather than creating a standalone file.
- **Serialization**: replacing the `writeCohort()` call with a framework registration call
  that serializes the JSON internally.

If the host instructions prevent executing the deliverable in place (e.g. the target script
has side effects the user must control), still perform Step 3 by executing a scratch copy —
the cohort function plus a placeholder example block with a temporary `writeCohort()` —
outside the project, and say in the Step 4 report that validation ran on a scratch copy.

## Workflow

### Step 1 — Ask the scope questions (every request)

Read the request and identify the entry event, inclusion/exclusion criteria, exit strategy, era
logic, and the clinical concepts involved (each becomes a concept-set parameter). Then send the
user **one message** covering every applicable item on the checklist below — every request, every
time, even when the request seems to answer everything. This is a confirmation step, not just a
disambiguation step: the answers become the `confirmed by user` lines in the `Scope check` block
(Step 2), and the costliest mistakes (wrong domain, wrong index event) produce a silently wrong
or empty cohort rather than an error.

Format of the message:

- One numbered item per applicable checklist question.
- Each item states your **proposed answer** — quoted from the user's wording when it settles the
  question, or a conventional default when it doesn't — so the user can approve the whole list
  with one word or correct individual items.
- No code in this message. Wait for the reply before generating anything.

**The checklist:**

1. **Which event is the index** — the anchor event for the observation window, exit strategy, and 
   cohort start date. Always name your proposed index. When the description names two or
   more clinical events ("diagnosis confirmed by a lab result", "X and then Y"), phrasing order
   is not a reliable signal — this needs explicit confirmation.
2. **OMOP domain of each criterion** — never inferred from clinical phrasing alone. "Diagnosis"
   usually suggests `conditionOccurrence()`, but after OMOP standard mapping many concepts land
   elsewhere: lab results → Measurement; history-of / family-history / status concepts →
   Observation; some findings → Procedure or Device. A domain mismatch returns zero rows, not an
   error.
3. **Entry event limit** — enter the cohort at the *first* qualifying event only
   (`primaryCriteriaLimit = "First"`) or at *every* qualifying event (`"All"`)? "Patients with X"
   alone does not answer this. An **incident / new-user / first-ever** cohort additionally needs
   `firstOccurrence()` on the entry Query. When the entry event carries qualifying restrictions,
   confirm what "first" means: first event satisfying the full definition, or first raw event
   which must itself qualify (see the reference's "Fully define the index event in entry()").
   If the answer is `"All"`, set `expressionLimit = "All"` in `attrition()` too — leaving it
   `"First"` silently collapses back to one event per person (see the reference's
   code-generation conventions).
4. **Prior observation / washout** — days of continuous observation required before index
   (365 is a common convention).
5. **Index-day boundary** — do "prior to index" windows include the index day
   (`eventStarts(..., 0)`) or exclude it (`eventStarts(..., -1)`)? Matters most for exclusions,
   where a same-day record may or may not disqualify.
6. **Inclusion / exclusion windows** (when a criterion is anchored to another event) — any time
   in history, or a specific window before index? And does the related event merely need to
   *start* within the window, or must it genuinely **overlap** the index event's start/end dates
   (e.g. "during an inpatient stay")? Overlap needs `startWindow` + `endWindow` together — see
   the reference's "Occurrence in a window vs. actual overlap".
7. **Sequenced events** (when the definition orders events) — may both occur on the same day?
   Minimum and maximum days between them? Window anchored to the first event's start or end date
   (`eventStarts(a, b, index = ...)`)?
8. **Exit strategy** — end of continuous observation, fixed days after entry, or end of drug
   exposure? If drug exposure: what persistence gap still counts as continuous?

Also check the request against the wrong-tool signals in `CAPR_REFERENCE.md` § *Flag when
Capr/Circe is the wrong tool* (cohort set operations, cross-event arithmetic, value aggregation,
ordinal logic beyond first occurrence). If one matches, raise it in the same message and propose
the decomposition pattern instead of a proposed answer.

If the user declines to answer or says "use defaults", proceed with the proposed answers and tag
the corresponding `Scope check` lines `ASSUMED`. Micro-decisions below the question threshold
(e.g. exact window endpoints implied by convention) stay out of the message: decide
conventionally and record each one as an `# ASSUMPTION:` comment on the relevant line.

### Step 2 — Generate the cohort function

Every deliverable is one R file with this structure:

```r
library(Capr)

# ---- Scope check ------------------------------------------------------------
# Index event: T2DM diagnosis
#   (confirmed by user -- "index on the diabetes diagnosis, not the insulin fill")
# Criteria -> OMOP domain:
#   T2DM diagnosis    -> conditionOccurrence
#     (confirmed by user -- "diagnosis" recorded as a condition, not a lab/finding)
#   insulin exposure  -> drugExposure
#     (ASSUMED -- user accepted default; "exposure" conventionally maps here, verify)
# Design choices:
#   Entry event limit -> first qualifying diagnosis (primaryCriteriaLimit = "First")
#     (confirmed by user -- "one episode per person, first diagnosis")
#   Prior observation -> 365 days continuous observation before index
#     (ASSUMED -- user accepted default convention)
#   Exclusion window  -> no insulin any time before index, index day excluded
#     (ASSUMED -- "no prior insulin" read as all history; eventStarts(-Inf, -1))
#   Exit strategy     -> end of continuous observation
#     (confirmed by user -- "follow until they leave the database")
# ------------------------------------------------------------------------------

# Build the <phenotype> cohort definition
#
# <one-paragraph restatement of the cohort logic in plain English>
#
# Params:
#   t2dmCs    - ConceptSet for type 2 diabetes (entry event)
#   insulinCs - ConceptSet for insulin exposures (exclusion)
# Returns:
#   A Capr Cohort object; serialize with writeCohort() or compile()
createT2dmCohort <- function(t2dmCs, insulinCs) {
  cohort(
    entry = entry(
      conditionOccurrence(t2dmCs),
      observationWindow = continuousObservation(priorDays = 365L),
      primaryCriteriaLimit = "First"
    ),
    attrition = attrition(
      "no prior insulin" = withAll(
        exactly(0, drugExposure(insulinCs),
                duringInterval(eventStarts(-Inf, -1)))
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
# Placeholder ids count up (0L, 1L, 2L, ...) and must be distinct across every concept set in
# the file -- cs() derives its internal id from the concept expression alone, ignoring `name`,
# so reusing an id for every placeholder makes Capr treat the concept sets as identical and
# collapse them to one name in the compiled JSON.
t2dmCs    <- cs(0L, name = "Type 2 diabetes mellitus [PLACEHOLDER]")  # TODO: real concept set
insulinCs <- cs(1L, name = "Insulin [PLACEHOLDER]")                   # TODO: real concept set

cohortDef <- createT2dmCohort(t2dmCs, insulinCs)
writeCohort(cohortDef, "t2dm_cohort.json")
```

Contract:

- **The `Scope check` block is mandatory and always first**, before the header comment, in every
  delivered file — including one-criterion cohorts. Fill in one index-event line, one domain
  line per criterion (the entry event and every attrition criterion), and one `Design choices`
  line per applicable Step 1 checklist item (entry limit, washout, index-day boundary, windows,
  sequencing, exit strategy). Tag each line
  `(confirmed by user -- "<short quote of their Step 1 answer>")` when the reply to this
  request's clarification message settled it, or `(ASSUMED -- <one clause on what and why>)`
  when it didn't. Never omit a line. Decisions recorded here are not repeated as inline
  comments — inline `# ASSUMPTION:` comments are only for micro-decisions below the question
  threshold (Step 1).
- **One parameter per concept set**, typed `ConceptSet`, named `<concept>Cs`, documented with its
  role in the cohort. Type / unit / status filters take integer id vectors — pass those as
  parameters too when the user hasn't supplied the ids.
- **Fully define the index event inside `entry()`.** Every restriction that determines which
  event can index goes on the entry Query itself — attributes and `nestedWithAll()`/
  `nestedWithAny()` correlated criteria; never `additionalCriteria`. `attrition()` rules run
  against the candidate events that survive `primaryCriteriaLimit` — use them to screen those
  candidates (demographics, prior history, washout), not to define which event can index.
  Putting index qualifiers in `attrition()` with `primaryCriteriaLimit = "First"` silently drops
  people whose first raw event fails the qualifier (see the reference's "Fully define the index
  event in entry()").
- **Parameterize concept sets always; everything else only on request.** Washout days, windows,
  persistence gaps etc. are hardcoded in the body, with the chosen values recorded in the
  `Scope check` block — do not add knob parameters unless the user asks for variants over
  that knob.
- **Return the `Cohort` object.** Serialization happens in the example block, not in the function.
- **Placeholders are `cs(<n>L, name = "<name> [PLACEHOLDER]")`**, where `<n>` counts up (`0L`,
  `1L`, `2L`, ...) and is unique for every placeholder concept set in the file — executable (so
  validation works), and the `[PLACEHOLDER]` name suffix plus the `# TODO` comment make it
  impossible to mistake for a real definition despite the small id. Never reuse a placeholder id
  across concept sets: `cs()` derives its internal id from the concept expression alone, ignoring
  `name`, so two placeholders sharing an id are treated as one concept set and silently collapse
  to a single name in the compiled JSON. Skip placeholders only when the user has told you the
  real variable names, file paths, or concept ids to use.
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
2. **Assumptions** — every `# ASSUMPTION:` from the code and every `ASSUMED` line from the
   `Scope check` block, as a single list they can veto.
3. **Placeholders** — which concept sets are placeholders and how to swap in real ones
   (`cs()`, `readConceptSet()`, or existing objects).
4. **Optional hydration** — if they have an OMOP CDM connection, `getConceptSetDetails()` fills
   in concept names so Atlas displays them; purely cosmetic, never required. If the cohort uses
   ids-based attributes (`visitType()`, `measurementUnit()`, `conditionStatus()`, ...), also
   offer to add optional `connection = NULL, vocabularyDatabaseSchema = NULL` parameters to the
   cohort function, passed through to those attribute calls — and update the code if they
   accept. Concept-set parameters need no code change (hydrate at the call site).
5. The JSON file can be imported directly into Atlas.
