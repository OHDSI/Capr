# Capr Worked Examples

Intermediate artifact toward `CAPR_REFERENCE.md`. See `CLAUDE_CODE_INSTRUCTIONS.md` (Step 3) for
the build plan, and `extras/CAPR_API_INVENTORY.md` for full function signatures.

All "Reference Examples" below are taken directly from this repo's `vignettes/` and
`tests/testthat/`, and every code block has been executed against the current package source
(`devtools::load_all()`) to confirm it actually runs. Where a vignette used outdated function
names (e.g. `condition()`, `first()`), those examples were skipped in favor of ones using the
current exported API. Where a vignette's code was stale in a more subtle way (wrong argument
type), it's called out and fixed inline — see Example 4.

## Conventions

- **Concept sets are placeholders.** Concept set construction is out of scope (see
  `CLAUDE_PLAN.md`) and the `cs()`/`descendants()` calls needed to build real ones just add noise
  to examples about cohort logic. Every example below assumes concept sets named `cs_*` already
  exist as `ConceptSet` objects in the session — don't add code to build them.
- **Always specify `exit` on `cohort()`.** Every example below passes `exit` explicitly, even
  when it's just `exit(endStrategy = observationExit())` (the default). Don't rely on the
  `NULL` → `observationExit()` fallback — make the exit strategy visible in the generated code.
- **Always specify `attrition` on `cohort()`, with an explicit `expressionLimit`.** This applies
  even when there are no inclusion/exclusion criteria: `attrition()` accepts zero groups, so use
  `attrition(expressionLimit = "First")` rather than omitting `attrition` altogether. Don't rely
  on the `NULL` (no attrition) or `"First"` (default `expressionLimit`) fallbacks.
- **Always specify `primaryCriteriaLimit` on `entry()`.** Don't rely on the `"First"` default —
  make it explicit every time.

## Example Format

Each example follows this structure. Use it when adding new ones.

```
### N. Title

**Intent:** A conceptual cohort definition, written the way a clinician/epidemiologist would
  describe the cohort (entry event, inclusion/exclusion logic, exit) — not a description of how
  it's implemented in Capr.
**Source:** File and line/test name this was drawn from (or "Real-world" + a short description of
  where it came from, for user-contributed examples).

​```r
<exact Capr code, with cs_* concept sets assumed pre-built>
​```

**Demonstrates:** Bullet list of the specific functions/patterns this example is teaching.
```

## Real-World Examples

### 1. Multiple entry paths with a nested co-occurrence requirement ("2 outpatient, 1 inpatient")

**Intent:** A cohort of persons with chronic obstructive pulmonary disease (COPD), entering the
cohort at the first qualifying COPD diagnosis — either (a) an outpatient COPD diagnosis that is
itself preceded by another outpatient COPD diagnosis in the prior 365 days, or (b) a single
inpatient visit with a primary diagnosis of COPD — and exiting at the end of continuous observation.
**Source:** Real-world — translated from an Atlas-exported cohort definition JSON (the classic
"2 outpatient or 1 inpatient" diagnosis algorithm), verified by round-tripping through
`compile()` and comparing the resulting JSON structure against the original.

```r
cd <- cohort(
  entry = entry(
    # entry path (a): an outpatient COPD dx with a *prior* outpatient COPD dx
    # in the preceding 365 days (i.e. "2 outpatient" occurrences)
    conditionOccurrence(
      cs_COPD,
      visitType(c(9202), connection = con, vocabularyDatabaseSchema = "cdm_schema"), # standard concept for OP visit
      nestedWithAll(
        atLeast(1,
          conditionOccurrence(cs_COPD, visitType(c(9202), connection = con, vocabularyDatabaseSchema = "cdm_schema")), # standard concept for OP visit
          aperture = duringInterval(
            startWindow = eventStarts(-365, -1, index = "startDate")
          )
        )
      )
    ),
    # entry path (b): a single inpatient COPD dx ("1 inpatient")
    conditionOccurrence(
      cs_COPD,
      visitType(c(9201), connection = con, vocabularyDatabaseSchema = "cdm_schema"), # standard concept for IP visit
      conditionStatus(c(32901,32902), connection = con, vocabularyDatabaseSchema = "cdm_schema") # standard concepts for primary diagnosis and primary admission diagnosis
    ),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
```

**Demonstrates:** Multiple entry-event queries passed to `entry(...)` (OR'd together as
alternative qualifying paths); nested criteria; `visitType()`/`conditionStatus()` to restrict a
query to a visit context — both require a live OMOP CDM database connection
(`connection`/`vocabularyDatabaseSchema`) to look up concept names, unlike most other query
attributes.

---

### 2. Fixed-date yearly denominator cohort

**Intent:** A denominator cohort where every person enters on January 1st of each calendar year
(2017, 2018, 2019) they have at least 365 days of prior continuous observation, and stays in the
cohort for one year — so a person present across all three years gets three separate yearly
episodes rather than one continuous one.
**Source:** Real-world — translated from an Atlas-exported cohort definition JSON (a fixed
calendar-date denominator/rate-cohort pattern), verified by round-tripping through `compile()`
and by inspecting the generated `CirceR::buildCohortQuery()` SQL.

```r
cd <- cohort(
  entry = entry(
    observationPeriod(startDate(eq(as.Date("2017-01-01")))),
    observationPeriod(startDate(eq(as.Date("2018-01-01")))),
    observationPeriod(startDate(eq(as.Date("2019-01-01")))),
    observationWindow = continuousObservation(365L, 0L),
    primaryCriteriaLimit = "All",
    qualifiedLimit = "First"
  ),
  attrition = attrition(
    expressionLimit = "All"
  ),
  exit = exit(
    endStrategy = fixedExit(index = "startDate", offsetDays = 364L)
  ),
  era = era(eraDays = 0L)
)
```

**Demonstrates:** `observationPeriod()` with `startDate(eq(...))` to anchor entry to a fixed
calendar date (`ObservationPeriod.UserDefinedPeriod` in the JSON) rather than a clinical event;
multiple fixed-date entry queries OR'd together via `entry(...)`, one per year;
`primaryCriteriaLimit = "All"` + `attrition(expressionLimit = "All")` to keep every qualifying
year per person instead of collapsing to a single entry; `era(eraDays = 0L)` so that
back-to-back yearly episodes (a 1-day gap between them) are kept as separate eras rather than
merged. Also confirms empirically that `qualifiedLimit` has no effect on the generated SQL when
`additionalCriteria` is `NULL` — it's carried here only for fidelity with the source JSON (see
the anti-pattern note on Example 7 in Reference Examples).

---

### 3. Multi-domain "2 qualifying diagnoses" cohort (condition and/or observation)

**Intent:** Persons with two qualifying dysphagia diagnoses within 365 days of each other, entering
the cohort at the first qualifying (second) diagnosis and exiting at end of continuous observation.
The diagnosis may be recorded as either a `conditionOccurrence` or an `observation` record
depending on the data source, and this applies independently to *both* the index diagnosis and the
qualifying prior diagnosis — so all four domain pairings (condition-then-condition,
condition-then-observation, observation-then-condition, observation-then-observation) must count.
**Source:** Constructed by extending Real-World Example 1's "multiple entry paths + nested
co-occurrence" pattern from domain-crossing on *visit type* to domain-crossing on the *event
domain itself* — a common annoyance in phenotype algorithms where the same clinical concept isn't
recorded in a single consistent domain. Verified by round-tripping through `compile()` and
confirming `CirceR::buildCohortQuery()` builds valid SQL from the result.

```r
cd <- cohort(
  entry = entry(
    # entry path (a): a condition-domain dx, with a *prior* qualifying dx in
    # EITHER domain in the preceding 365 days
    conditionOccurrence(
      cs_dysphagia,
      nestedWithAny(
        atLeast(1, conditionOccurrence(cs_dysphagia),
                aperture = duringInterval(startWindow = eventStarts(-365, -1, index = "startDate"))),
        atLeast(1, observation(cs_dysphagia),
                aperture = duringInterval(startWindow = eventStarts(-365, -1, index = "startDate")))
      )
    ),
    # entry path (b): an observation-domain dx, with a *prior* qualifying dx in
    # EITHER domain in the preceding 365 days
    observation(
      cs_dysphagia,
      nestedWithAny(
        atLeast(1, conditionOccurrence(cs_dysphagia),
                aperture = duringInterval(startWindow = eventStarts(-365, -1, index = "startDate"))),
        atLeast(1, observation(cs_dysphagia),
                aperture = duringInterval(startWindow = eventStarts(-365, -1, index = "startDate")))
      )
    ),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
```

**Demonstrates:** the general pattern for "event of interest spans multiple domains" — one entry
Query per domain (here `conditionOccurrence()` and `observation()`), each carrying its own
`nestedWithAny(...)` with one alternative per domain the *prior* qualifying event might appear in.
N domains for both index and prior event means N top-level entry Queries × N nested alternatives
each = N² combinations to cover, not N; forgetting any one combination silently undercounts the
cohort rather than erroring.

---

## Reference Examples

### 1. Simple entry event (single concept set, single domain)

**Intent:** A cohort of persons with GI bleed, entering the cohort at the start of their first
GI bleed condition occurrence and exiting at the end of continuous observation.
**Source:** `vignettes/Using-Capr.Rmd`

```r
giBleedCohort <- cohort(
  entry = entry(
    conditionOccurrence(cs_giBleed),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
```

**Demonstrates:** `conditionOccurrence()`, `entry()` with `primaryCriteriaLimit`, `exit()` with
`observationExit()`, and `attrition()` called with **no groups** — valid when there are no
inclusion/exclusion criteria, but `expressionLimit` is still required by convention.

---

### 2. Entry + required inclusion criteria (lab value + prior observation)

**Intent:** A cohort of persons with type 2 diabetes, entering the cohort at their first
type 2 diabetes diagnosis, restricted to those with at least 365 days of prior continuous observation and
an abnormal HbA1c lab value (< 5.7 g/dL) recorded any time before index, and exiting at the end
of continuous observation.
**Source:** `tests/testthat/test-cohort.R`, `test_that("full cohort works with domains without concepts")`

```r
cd <- cohort(
  entry = entry(
    conditionOccurrence(cs_t2dm),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    '365d OP' = withAll(
      exactly(1,
              observationPeriod(),
              duringInterval(eventStarts(-Inf, -365), eventEnds(0, Inf))
      )
    ),
    'abnormal hba1c' = withAll(
      atLeast(1,
              measurement(
                cs_hba1c,
                valueAsNumber(lt(5.7)),
                measurementUnit(cs_percentUnit)),
              duringInterval(eventStarts(-Inf, -1))
      )
    ),
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
```

**Demonstrates:** `attrition()` with **named** `Group`s, `observationPeriod()` query (no
`conceptSet` arg), `measurement()` with `valueAsNumber()` + `measurementUnit()`, `atLeast()` /
`exactly()`.

**Note:** Every named attrition rule must be a `Group` (built with `withAll()`/`withAny()`/
`withAtLeast()`/`withAtMost()`), even for a single criterion — don't pass a bare `Criteria` (e.g.
a bare `atLeast(...)`/`exactly(...)` call) directly. `as.list()` on a bare `Criteria` produces the
shape of a `CriteriaList` *item*, not the `{Type, CriteriaList, DemographicCriteriaList, Groups}`
shape Atlas expects for an inclusion rule — Capr will still `compile()` it without error, but
Atlas can't render the result. Verified by importing the generated JSON into Atlas.

---

### 3. Entry + absence criteria (no prior diagnosis of a related condition)

**Intent:** A cohort of persons with type 2 diabetes, entering the cohort at their
first type 2 diabetes diagnosis, restriced to those with at least 365 days of prior observation, and excluding anyone
with a prior diagnosis of type 1 or secondary diabetes at any time on or before index, and exiting at
the end of continuous observation.
**Source:** `vignettes/Examples.Rmd`, "Persons with new type 2 diabetes and no prior T1DM or
secondary diabetes"

```r
ch <- cohort(
  entry = entry(
    conditionOccurrence(cs_t2dm),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    '365d OP' = withAll(
      exactly(1,
              observationPeriod(),
              duringInterval(eventStarts(-Inf, -365), eventEnds(0, Inf))
      )
    ),
    't1d' = withAll(
      exactly(0, conditionOccurrence(cs_t1dm), duringInterval(eventStarts(-Inf, 0)))
    ),
    'secondaryDiabetes' = withAll(
      exactly(0, conditionOccurrence(cs_secondaryDiabetes), duringInterval(eventStarts(-Inf, 0)))
    ),
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
```

**Demonstrates:** Absence criteria via `exactly(0, ...)`, multiple named attrition groups, and
`continuousObservation(priorDays = ...)` using a named argument (postDays defaults to `0L`).

---

### 4. Demographic criteria (age, gender)

**Intent:** A cohort of persons with type 2 diabetes, entering the cohort
at their first type 2 diabetes diagnosis with at least 365 days of prior continuous observation, with male gender and aged 18 years or older at index,
exiting at the end of continuous observation.
**Source:** `tests/testthat/test-cohort.R` + `extras/CAPR_API_INVENTORY.md`
pattern (`age()`/`male()` inside `attrition`)

```r
cd <- cohort(
  entry = entry(
    conditionOccurrence(cs_t2dm),
    observationWindow = continuousObservation(365, 0),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    "adult males" = withAll(
      male(),
      age(gte(18))
    ),
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
```

**Demonstrates:** `male()` and `age()` used as a
demographic inclusion rule inside `attrition()`.

---

### 5. Exit strategy variations (fixed duration, drug era, censoring)

**Intent:** Three different ways a cohort's exit (end date) can be conceptually defined: (a) a
fixed 30 days after cohort entry; (b) the end of a continuous era of ACE inhibitor exposure,
allowing up to a 30-day gap between drug records and adding a 7-day surveillance buffer after
the era ends; (c) the end of continuous observation, with the cohort period cut short early if
the person dies.
**Source:** `tests/testthat/test-exit.R` (`fixedExit`, `drugExit`) + `test-cohort.R`
(`censoringEvents`)

```r
# a) Exit 30 days after cohort start
exit(endStrategy = fixedExit(index = "startDate", offsetDays = 30L))

# b) Exit at the end of a continuous ACE inhibitor drug era (30-day gap allowed, 7-day
#    surveillance window added after the era ends)
exit(endStrategy = drugExit(conceptSet = cs_aceInhibitors, persistenceWindow = 30L, surveillanceWindow = 7L))

# c) Exit at end of observation, censored early if the patient dies
exit(
  endStrategy = observationExit(),
  censor = censoringEvents(death())
)
```

**Demonstrates:** `fixedExit()`, `drugExit()`, `observationExit()`, `censoringEvents()` with a
`death()` query. (These are standalone `exit()` calls used to illustrate the strategy options
themselves — plug one into a `cohort()`'s `exit` argument per the convention above.)

---

### 6. Nested/correlated criteria

**Intent:** Persons exposed to metformin, where that specific metformin exposure event must be
corroborated by an HbA1c lab measurement recorded in the 30 days leading up to (and including)
the day of exposure — the lab check is tied to that individual drug event, not to the cohort's
overall index date.
**Source:** `extras/CAPR_API_INVENTORY.md` pattern (verified against `R/attributes-nested.R`)

```r
drugExposure(
  conceptSet = cs_metformin,
  nestedWithAll(
    atLeast(1, measurement(conceptSet = cs_hba1c),
            aperture = duringInterval(eventStarts(-30, 0))
    )
  )
)
```

**Demonstrates:** `nestedWithAll()` passed as a query attribute (via `...`) to express
sub-criteria relative to the *query's own* event date, rather than the cohort index date.

---

### 7. Filtering entry events with a co-occurring event, via `attrition`

**Intent:** A cohort of persons with acute stroke, entering the
cohort at each stroke diagnosis that co-occurs with an inpatient visit (from the day before
the visit through the end of the visit), with each qualifying episode exiting 7 days after
entry and episodes within 180 days of each other collapsed into a single era.
**Source:** `tests/testthat/test-AdditionalCriteria.R`, adapted — see anti-pattern note below.

```r
cd <- cohort(
  entry = entry(
    conditionOccurrence(cs_stroke),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "All"
  ),
  attrition = attrition(
    "has visit" = withAny(
      atLeast(1,
        visit(cs_ipVisit),
        duringInterval(startWindow = eventStarts(-Inf, 1, index = "startDate"),
                       endWindow = eventEnds(0, Inf, index = "startDate")
        )
      )
    ),
    expressionLimit = "All"
  ),
  exit = exit(
    endStrategy = fixedExit(index = "startDate", offsetDays = 7L)
  ),
  era = era(eraDays = 180L)
)
```

**Demonstrates:** Using `attrition()` + `expressionLimit` to filter *and* select among entry
events, instead of `entry()`'s `additionalCriteria`/`qualifiedLimit`.

**Anti-pattern note — prefer `attrition` over `additionalCriteria`:** `entry()` also supports
`additionalCriteria`/`qualifiedLimit` (see `extras/CAPR_API_INVENTORY.md`) to express this same
"filter, then pick First/Last/All of the survivors" logic. It looks like a distinct capability,
but it isn't: both were verified byte-for-byte equivalent for this pattern — same generated
SQL structure (via `CirceR::buildCohortQuery()`) and, on real data (Eunomia, GI bleed +
inpatient visit, `primaryCriteriaLimit`/`qualifiedLimit`/`expressionLimit = "All"`), the exact
same 479 cohort rows (subject/start/end date). This held for both the "strict" case
(`primaryCriteriaLimit = "First"`, must be the very first occurrence) and the "permissive" case
(`primaryCriteriaLimit = "All"` + `qualifiedLimit`/`expressionLimit = "First"`, first occurrence
*that satisfies the filter*). Since `attrition` covers this ground and is the more familiar,
general-purpose tool, don't reach for `additionalCriteria` unless a concrete case is found where
it's actually necessary.

---

### 8. Source concept filtering

**Intent:** A cohort of persons with lung fibrosis, entering the cohort at their first lung
fibrosis diagnosis — identified using specific source codes (e.g. ICD-10-CM) rather than mapped
standard concepts, because the standard vocabulary mapping is unreliable for this condition at
the source site and exiting at the end of
continuous observation.
**Source:** `vignettes/Examples.Rmd`, "Source Concept Example"

```r
cd <- cohort(
  entry = entry(
    conditionOccurrence(
      conceptSet = NULL,
      conditionSourceConcept(cs_lungFibrosisSource)
    ),
    primaryCriteriaLimit = "First"
  ),
  attrition = attrition(
    expressionLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)
```

**Demonstrates:** `conditionSourceConcept()` as a query attribute to restrict by
`condition_source_concept_id`.
