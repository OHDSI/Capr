# Capr R Package — LLM Reference

Reference for generating OHDSI cohort definitions with the **Capr** R package.
Every signature, enum value, and example in this document was checked against 
the package source; do not use functions or arguments not listed here.

## Overview

Capr (Cohort definition Application Programming in R) builds OHDSI cohort definitions as R
objects and serializes them to Atlas-compatible JSON. A cohort definition is assembled from four
components:

```
Cohort                  <- cohort()
├── CohortEntry         <- entry()       index event(s) + required observation window
├── CohortAttrition     <- attrition()   named inclusion/exclusion rule groups
├── CohortExit          <- exit()        end strategy + optional censoring events
└── CohortEra           <- era()         era-collapse gap + optional study date window
```

The building blocks inside those components:

- **Query** — an event drawn from one OMOP CDM domain table, filtered by a concept set and
  optional attributes (e.g. `conditionOccurrence(cs_t2dm)`). Queries are used as entry events,
  inside criteria, and as censoring events.
- **Criteria** — a Query wrapped with counting logic and a time window
  (`atLeast(1, query, aperture)` / `atMost(...)` / `exactly(...)`).
- **Group** — one or more Criteria/Groups combined with a logical operator
  (`withAll()` / `withAny()` / `withAtLeast()` / `withAtMost()`). Attrition rules must be Groups.
- **EventAperture** — the assessment window for a Criteria, built with
  `duringInterval(eventStarts(a, b))`.

## Prerequisites

```r
library(Capr)
```

**Concept sets are pre-built.** Assume every concept set the cohort needs already exists in the
session as a `ConceptSet` S4 object (named `cs_*` in this document). Do not generate code that
constructs concept sets. Concept sets are referenced by passing the object itself — as the first
argument (`conceptSet`) of a domain query constructor, as `conceptSet` in `drugExit()`, or as the
argument of a source-concept / value-as-concept-set attribute.

No database connection is needed to build and serialize a cohort definition. A connection is
optional everywhere it appears, used only to fill in concept names for Atlas display (see
"Recommend hydrating concept sets" below).

**Code-generation conventions.** Always make defaults explicit rather than relying on fallbacks:

- Always pass `primaryCriteriaLimit` to `entry()` (don't rely on the `"First"` default).
- Always pass `attrition` to `cohort()`, with an explicit `expressionLimit` — even when there are
  no inclusion rules, use `attrition(expressionLimit = "First")`.
- **`expressionLimit` follows `primaryCriteriaLimit`.** When `primaryCriteriaLimit = "All"`
  (multiple episodes per person), set `expressionLimit = "All"` as well — leaving it `"First"`
  silently collapses the cohort back to one event per person after the inclusion rules run,
  defeating the point of `"All"` entry. Diverge only when the user explicitly asks for it (e.g.
  "of all qualifying events, keep each person's first that passes the inclusion rules").
- Always pass `exit` to `cohort()`, even when it's just `exit(endStrategy = observationExit())`.

## Agent Workflow

The scope-question checklist (index event, domains, entry limit, washout, windows, exit) lives in
`SKILL.md` Step 1, which requires sending it to the user before writing any code. The subsections
below cover the design decisions that follow from those answers.

When the definition involves more than one clinical event: events that must co-occur with,
precede, or follow the index, and that would disqualify a candidate if absent, belong as nested
criteria (`nestedWithAll()`/`nestedWithAny()`) on the entry Query (see "Fully define the index
event in entry()" below); events only evaluated after entry is already fixed belong in
`attrition()`.

### Fully define the index event in entry()

Every restriction that determines *which event can serve as the index* belongs inside `entry()`,
on the entry Query itself — as query attributes (`visitType()`, `valueAsNumber()`, `age()`, ...)
or as correlated events via nested criteria (`nestedWithAll()` / `nestedWithAny()`). Attrition
rules are not an alternative place to define the index event: each rule is evaluated against the
candidate events that *survive* `primaryCriteriaLimit`, relative to each candidate's date — so
what a rule means depends on which events are still in play when it runs. Use `attrition()` to
screen those candidates (demographics, prior-history exclusions, washout conditions); use the
entry Query to define what qualifies as an index event in the first place.

Why this matters: Circe applies limits in sequence — `primaryCriteriaLimit` selects among the
raw entry events first, and only then are the attrition rules applied (with `expressionLimit`
selecting among the survivors). So with `primaryCriteriaLimit = "First"` and a qualifying
restriction in `attrition()`, a person whose *first* raw event fails the restriction is dropped
from the cohort entirely — even if a later event would have qualified. With the restriction on
the entry Query, that person correctly enters at their first *qualifying* event. Both
constructions compile, run, and look similar, but they define different cohorts.

```r
# Intent: index on the first stroke diagnosis that occurs during an inpatient visit

# DO NOT — "First" picks the person's first raw dx before the rule runs; persons whose
# first dx was outpatient are dropped entirely instead of indexing on a later inpatient dx
entry(conditionOccurrence(cs_stroke), primaryCriteriaLimit = "First")
# ... with an "inpatient visit at index" rule in attrition()

# DO — the index event itself is "stroke dx during an inpatient visit";
# "First" now selects the first event satisfying the full definition
entry(
  conditionOccurrence(
    cs_stroke,
    nestedWithAll(
      atLeast(1, visit(cs_ipVisit),
              duringInterval(eventStarts(-Inf, 0), endWindow = eventEnds(0, Inf)))
    )
  ),
  primaryCriteriaLimit = "First"
)
```

The deliberate exception is screening a fixed index event: "first-ever diagnosis, and exclude
the person if that first diagnosis wasn't inpatient" — there the index is the first-ever event
(`firstOccurrence()` on the entry Query), it is each person's only candidate, and the inpatient
requirement is correctly an attrition rule that accepts or rejects it. If the user's wording is
ambiguous between "first qualifying event" and "first event, which must qualify", **ask** — it
changes cohort membership.

`entry()`'s `additionalCriteria`/`qualifiedLimit` can express event-level filtering too, but it
is never needed: nested criteria on the entry Query cover the same logic with clearer semantics
(see Anti-Patterns #2).

### Flag when Capr/Circe is the wrong tool

Capr compiles to Circe, which defines a single self-contained cohort. Some requests cannot — or
should not — be forced into one Circe definition. **This is a hard rule, not a tip: check every
request against the signals below before writing any code, and when one matches, stop and tell
the user which part of their definition is not expressible — never silently deliver an
approximation.** A cohort that compiles and runs but means something different from what the
user asked for is the worst possible outcome, because nothing will ever error. Signals:

- **Set operations between cohorts** — "in cohort A but never in cohort B", overlap or union of
  two populations. A Circe cohort cannot reference another cohort.
- **Cross-event calculations** — change from baseline, cumulative dose, dose tapering, or any
  comparison between values of two different events. Attribute filters apply to one event at a
  time.
- **Aggregate arithmetic across events** — sums, averages, min/max, or rates over a person's 
  events ("mean HbA1c above 8", "total days supply over 90 in the year"). Criteria can *count* events 
  (`atLeast`/`exactly`/`atMost`), but cannot aggregate their values.
- **Ordinal/sequential event logic** beyond first occurrence — "the second treatment era",
  "the third hospitalization within a year". Nested criteria can sometimes approximate these;
  verify the logic carefully and say so if the translation is approximate.
- **Deep nesting** — if a faithful translation needs more than about two levels of nested
  groups, or many interacting inclusion rules, the definition may compile but be unreviewable
  and hard to validate.

**Recommended decomposition pattern:** split the logic into two or more simple Circe cohorts —
each one cleanly expressible in Capr — and combine the generated cohort tables downstream with
SQL (set operations, sequencing, and cross-event arithmetic are all straightforward there).
Tell the user which parts fit Capr and which need SQL, propose the split, and generate the Capr
portions once they agree.

### Recommend hydrating concept sets when the user has a database connection

A `ConceptSet` built directly with `cs()` only has `concept_id` populated — `concept_name`,
`domain_id`, `vocabulary_id`, etc. are left blank. The same applies to ids-based attributes like
`measurementUnit()`, `visitType()`, and the other type/provenance filters when called without a
connection. The generated cohort JSON is still fully valid and produces correct SQL, but Atlas's
UI has nothing to display for those blank fields, which makes the cohort harder for a human to
read/review there. This comes up most for small inline concept sets and attribute ids — larger,
pre-built concept sets (see Prerequisites) are usually already sourced from Atlas or ATHENA and
already carry full concept details.

If the user has (or can get) a live OMOP CDM database connection, tell them they can "hydrate" any
`ConceptSet` with real vocabulary details before compiling:

```r
conceptSet <- getConceptSetDetails(conceptSet, con, vocabularyDatabaseSchema = "cdm_schema")
```

For the ids-based attributes (`measurementUnit()`, `visitType()`, etc.), pass
`connection`/`vocabularyDatabaseSchema` directly to the attribute function instead. When the
generated cohort uses any ids-based attribute and the user wants hydration, don't just mention
it — update the code: add optional `connection = NULL, vocabularyDatabaseSchema = NULL`
parameters to the cohort function and pass them through to those attribute calls (concept-set
parameters need no code change; the user hydrates them at the call site).

This fills in the blank fields from the `concept` table so Atlas shows real names. It's optional —
don't insist on it or block on generating code without it — but mention it whenever you build a
`cs()` concept set that would otherwise compile with blank concept metadata.

## API Reference

**Type notation used below:**
`ConceptSet` — pre-built S4 concept set object. `Query` — from a domain query constructor.
`Criteria` — from `exactly()`/`atLeast()`/`atMost()`. `Group` — from `withAll()`/`withAny()`/
`withAtLeast()`/`withAtMost()`. `EventWindow` — from `eventStarts()`/`eventEnds()`.
`EventAperture` — from `duringInterval()`. `opAttribute` — from a comparison operator
(`lt`/`lte`/`gt`/`gte`/`eq`/`bt`/`nbt`); comes in `opAttributeInteger`, `opAttributeNumeric`, and
`opAttributeDate` variants.

### Top-Level Assembly

| Function | Description |
|---|---|
| `cohort(entry, attrition, exit, era)` | Assemble a complete `Cohort` object from its four components |
| `entry(..., observationWindow, primaryCriteriaLimit, additionalCriteria, qualifiedLimit)` | Define the index event(s) and qualifying conditions |
| `attrition(..., expressionLimit)` | Define named inclusion/exclusion rule groups |
| `exit(endStrategy, censor)` | Wrap an end strategy and optional censoring criteria |
| `era(eraDays, studyStartDate, studyEndDate)` | Set era-collapse padding and optional study date window |

#### `cohort(entry, attrition = NULL, exit = NULL, era = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `entry` | `CohortEntry` | — | Required |
| `attrition` | `CohortAttrition` or `NULL` | `NULL` | Pass explicitly per conventions above |
| `exit` | `CohortExit` or `NULL` | `NULL` | If `NULL`, defaults to `observationExit()`; pass explicitly |
| `era` | `CohortEra` or `NULL` | `NULL` | If `NULL`, defaults to `era(eraDays = 0L)` |

**Returns:** `Cohort` S4 object.

### Cohort Entry

#### `entry(..., observationWindow = continuousObservation(0L, 0L), primaryCriteriaLimit = c("First", "All", "Last"), additionalCriteria = NULL, qualifiedLimit = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `...` | `Query` | — | One or more index event Queries. Multiple Queries are OR'd — each is an alternative qualifying entry path |
| `observationWindow` | `ObservationWindow` | `continuousObservation(0L, 0L)` | Required continuous observation before/after index |
| `primaryCriteriaLimit` | `character` | `"First"` | Which qualifying events enter the cohort. One of `"First"`, `"All"`, `"Last"` |
| `additionalCriteria` | `Group` or `NULL` | `NULL` | Avoid — restrict the entry Query itself with attributes/nested criteria instead (see Anti-Patterns #2) |
| `qualifiedLimit` | `character` or `NULL` | `NULL` | One of `"First"`, `"All"`, `"Last"`. Required when `additionalCriteria` is non-`NULL`; a no-op otherwise (see Anti-Patterns #3) |

**Returns:** `CohortEntry`.

#### `continuousObservation(priorDays = 0L, postDays = 0L)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `priorDays` | `integer` | `0L` | Min observation days before cohort index |
| `postDays` | `integer` | `0L` | Min observation days after cohort index |

**Returns:** `ObservationWindow`.

### Inclusion Criteria (Attrition)

#### `attrition(..., expressionLimit = c("First", "All", "Last"))`

| Param | Type | Default | Notes |
|---|---|---|---|
| `...` | **Named** `Group` objects | — | Each named argument is one inclusion rule; the name is the rule's label. Zero groups is valid. Every rule must be a `Group`, never a bare `Criteria` (see Anti-Patterns #1) |
| `expressionLimit` | `character` | `"First"` | Which events satisfying all inclusion rules enter the cohort. One of `"First"`, `"All"`, `"Last"` |

**Returns:** `CohortAttrition`.
**Usage:** `attrition("Prior T2DM" = withAll(...), "No prior insulin" = withAll(...), expressionLimit = "First")`

### Cohort Exit

#### `exit(endStrategy, censor = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `endStrategy` | `ObservationExit`, `DrugExposureExit`, or `FixedDurationExit` | `observationExit()` | How the cohort end date is computed |
| `censor` | `CensoringCriteria` or `NULL` | `NULL` | From `censoringEvents()`; events that cut the cohort episode short |

**Returns:** `CohortExit`.

#### End strategies

Passed as `endStrategy` to `exit()`.

| Function | Description |
|---|---|
| `observationExit()` | Exit at end of continuous observation (default) |
| `fixedExit(index, offsetDays)` | Exit `offsetDays` days after event start or end |
| `drugExit(conceptSet, persistenceWindow, surveillanceWindow, daysSupplyOverride)` | Exit at end of continuous drug era |

##### `observationExit()`

No parameters. **Returns:** `ObservationExit` (serializes as an empty `EndStrategy` in JSON).

##### `fixedExit(index = c("startDate", "endDate"), offsetDays)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `index` | `character` | `"startDate"` | `"startDate"` or `"endDate"` |
| `offsetDays` | `numeric` | **Required — no default** | Coerced to `integer` |

**Returns:** `FixedDurationExit`.

##### `drugExit(conceptSet, persistenceWindow = 0L, surveillanceWindow = 0L, daysSupplyOverride = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `conceptSet` | `ConceptSet` | — | Drug ingredient concept set |
| `persistenceWindow` | `integer` | `0L` | Max gap in days between drug records when building the era |
| `surveillanceWindow` | `integer` | `0L` | Days added to end of era before exit |
| `daysSupplyOverride` | `integer` or `NULL` | `NULL` | Force a fixed days supply; `NULL` = use actual |

**Returns:** `DrugExposureExit`.

#### `censoringEvents(...)`

`...`: one or more `Query` objects. Any occurrence of these events ends the cohort episode early.
**Returns:** `CensoringCriteria`. Pass as `censor` in `exit()`.

### Era Logic

#### `era(eraDays = 0L, studyStartDate = NULL, studyEndDate = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `eraDays` | `integer` | `0L` | Max gap in days between neighboring cohort episodes when collapsing them into eras |
| `studyStartDate` | `Date` or `NULL` | `NULL` | Left-censor cohort era start dates to this date |
| `studyEndDate` | `Date` or `NULL` | `NULL` | Right-censor cohort era end dates to this date |

**Returns:** `CohortEra`.

### Query Constructors

One constructor per OMOP CDM domain table. All take `conceptSet` as the first argument plus zero
or more attribute objects via `...`. `conceptSet` is **required with no default** — pass
`conceptSet = NULL` explicitly if no concept filter is needed. Exceptions: `death()` defaults to
`NULL`; `observationPeriod()` has no `conceptSet` argument at all.

| Function | OMOP table | `conceptSet` | Notes |
|---|---|---|---|
| `conditionOccurrence(conceptSet, ...)` | CONDITION_OCCURRENCE | Required | |
| `conditionEra(conceptSet, ...)` | CONDITION_ERA | Required | Prefer `conditionOccurrence` unless the user asks for eras |
| `drugExposure(conceptSet, ...)` | DRUG_EXPOSURE | Required | |
| `drugEra(conceptSet, ...)` | DRUG_ERA | Required | Prefer `drugExposure` unless the user asks for eras |
| `doseEra(conceptSet, ...)` | DOSE_ERA | Required | |
| `measurement(conceptSet, ...)` | MEASUREMENT | Required | |
| `observation(conceptSet, ...)` | OBSERVATION | Required | |
| `procedure(conceptSet, ...)` | PROCEDURE | Required | |
| `visit(conceptSet, ...)` | VISIT_OCCURRENCE | Required | The *kind* of visit (inpatient, outpatient, ER) is the ConceptSet (`visit_concept_id`) — never `visitType()`, which is provenance here (see "Query Attributes — Type / Status") |
| `visitDetail(conceptSet, ...)` | VISIT_DETAIL | Required | Prefer `visit` unless the user asks for visit detail |
| `deviceExposure(conceptSet, ...)` | DEVICE_EXPOSURE | Required | |
| `specimen(conceptSet, ...)` | SPECIMEN | Required | |
| `death(conceptSet = NULL, ...)` | DEATH | Optional | |
| `observationPeriod(...)` | OBSERVATION_PERIOD | None | |

**Returns:** `Query` (all constructors).
**Domain mismatch:** if the concept set contains concepts whose `domain_id` doesn't match the
query domain (e.g. Drug concepts inside `conditionOccurrence()`), Capr issues a warning — not an
error. Match the constructor to the concept set's domain.

### Criteria Constructors

Wrap a Query with an occurrence count and an assessment window.

| Function | Description |
|---|---|
| `exactly(x, query, aperture, distinct, countColumn)` | Require exactly `x` occurrences of `query` within `aperture` |
| `atLeast(x, query, aperture, distinct, countColumn)` | Require at least `x` occurrences |
| `atMost(x, query, aperture, distinct, countColumn)` | Require at most `x` occurrences |

All three share one signature:

#### `exactly(x, query, aperture = duringInterval(eventStarts(-Inf, Inf)), distinct = NA, countColumn = NA_character_)`
#### `atLeast(x, query, aperture = ..., distinct = NA, countColumn = NA_character_)`
#### `atMost(x, query, aperture = ..., distinct = NA, countColumn = NA_character_)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `x` | `integer` | — | Number of occurrences required (`exactly(0, ...)` expresses absence) |
| `query` | `Query` | — | |
| `aperture` | `EventAperture` | `duringInterval(eventStarts(-Inf, Inf))` | Time window in which to look for the event |
| `distinct` | `logical` | `NA` | `NA`/`FALSE` = count all occurrences; `TRUE` = count distinct by `countColumn` |
| `countColumn` | `character` | `NA_character_` | Required if `distinct = TRUE`. One of `"DOMAIN_CONCEPT"`, `"START_DATE"`, `"VISIT_ID"` |

**Returns:** `Criteria`.

### Assessment Windows (EventAperture)

#### `duringInterval(startWindow, endWindow = NULL, restrictVisit = FALSE, ignoreObservationPeriod = FALSE)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `startWindow` | `EventWindow` | — | Window in which the event must **start**; build with `eventStarts()` |
| `endWindow` | `EventWindow` or `NULL` | `NULL` | Window in which the event must **end**; build with `eventEnds()` |
| `restrictVisit` | `logical` | `FALSE` | `TRUE` = event must occur during the same visit as the index event |
| `ignoreObservationPeriod` | `logical` | `FALSE` | `TRUE` = allow events outside the index event's observation period |

**Returns:** `EventAperture`.

#### `eventStarts(a, b, index = c("startDate", "endDate"))` / `eventEnds(a, b, index = ...)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `a` | `numeric` | — | Left bound, signed days relative to index (negative = before index). `-Inf` = all time before |
| `b` | `numeric` | — | Right bound, signed days relative to index (positive = after). `Inf` = all time after |
| `index` | `character` | `"startDate"` | Anchor the window on the index event's `"startDate"` or `"endDate"` |

**Returns:** `EventWindow`.

**Sign convention:** `0` means the index date itself. `eventStarts(-365, 0)` = "from 365 days
before index up to and including the index date"; use `b = -1` to exclude the index date
(strictly before). Common windows:

```r
duringInterval(eventStarts(-Inf, 0))       # any time on or before index
duringInterval(eventStarts(-Inf, -1))      # any time strictly before index
duringInterval(eventStarts(-365, -1))      # within 365 days before index, excluding index day
duringInterval(eventStarts(0, Inf))        # on index or any time after
duringInterval(eventStarts(-Inf, Inf))     # all time (the default)
```

**Occurrence in a window vs. actual overlap.** A `startWindow` alone only constrains where the
related event *starts* — it says nothing about the event's end date. To require the related
event to genuinely overlap the index event (its interval covers the index date, e.g. "during an
inpatient stay"), combine both windows:

```r
# related event starts on/before index AND ends on/after index = overlaps the index date
duringInterval(startWindow = eventStarts(-Inf, 0), endWindow = eventEnds(0, Inf))
```

These are different cohorts: "visit starting in the 30 days before index" admits a visit that
ended before index; "visit overlapping index" does not. `SKILL.md`'s checklist requires asking
the user which one they mean whenever a criterion is anchored to another event.

### Group Constructors

Combine `Criteria` and/or `Group` objects (they nest) with a logical operator. Demographic
attributes (`age()`, `male()`, `female()`, `genderConcepts()`) may also be passed directly into a
Group — this is how demographic inclusion rules are expressed.

| Function | Meaning |
|---|---|
| `withAll(...)` | All items must be satisfied (AND) |
| `withAny(...)` | At least one item must be satisfied (OR) |
| `withAtLeast(x, ...)` | At least `x` of the items must be satisfied |
| `withAtMost(x, ...)` | At most `x` of the items must be satisfied |

`x`: `integer`. **Returns:** `Group` (all four).

### Nested (Correlated) Criteria

Express sub-criteria evaluated relative to the **query's own event date** (not the cohort index
date). Pass the result as an attribute via `...` in a Query constructor.

| Function | Meaning |
|---|---|
| `nestedWithAll(...)` | All sub-criteria must hold |
| `nestedWithAny(...)` | Any sub-criterion must hold |
| `nestedWithAtLeast(x, ...)` | At least `x` must hold |
| `nestedWithAtMost(x, ...)` | At most `x` must hold |

`...`: `Criteria` and/or `Group`; `x`: `integer`. **Returns:** `nestedAttribute` (all four).

```r
# Drug exposure event that itself has an HbA1c lab in the preceding 30 days
drugExposure(
  conceptSet = cs_metformin,
  nestedWithAll(
    atLeast(1, measurement(conceptSet = cs_hba1c),
            aperture = duringInterval(eventStarts(-30, 0)))
  )
)
```

### Comparison Operators

Used as the `op` argument of numeric and date attribute functions (e.g. `age(gte(18L))`). The
returned type follows the type of the value passed in.

| Function | Meaning |
|---|---|
| `lt(x)` | `< x` |
| `lte(x)` | `<= x` |
| `gt(x)` | `> x` |
| `gte(x)` | `>= x` |
| `eq(x)` | `== x` |
| `bt(x, y)` | between, inclusive: `x <= value <= y` |
| `nbt(x, y)` | not between: `value < x` or `value > y` |

**Type rules:** integer literal (`18L`) → `opAttributeInteger`; double (`18`, `18.0`) →
`opAttributeNumeric`; `as.Date("2020-01-01")` → `opAttributeDate`. For `bt`/`nbt`, `x` and `y`
must be the same type.

### Query Attributes — Numeric

Pattern: `f(op)` where `op` is an `opAttribute` from a comparison operator. Pass via `...` in a
domain query constructor. Each errors immediately if `op` is not an `opAttribute`.

| Function | Returns `name =` | Recommended domain | CDM column |
|---|---|---|---|
| `age(op)` | `"Age"` | Any (also usable as a demographic in Groups) | age at event date |
| `daysOfSupply(op)` | `"DaysSupply"` | `drugExposure` | `days_supply` |
| `drugRefills(op)` | `"Refills"` | `drugExposure` | `refills` |
| `drugQuantity(op)` | `"Quantity"` | `drugExposure` | `quantity` |
| `valueAsNumber(op)` | `"ValueAsNumber"` | `measurement`, `observation` | `value_as_number` |
| `rangeHigh(op)` | `"RangeHigh"` | `measurement` | `range_high` |
| `rangeLow(op)` | `"RangeLow"` | `measurement` | `range_low` |
| `rangeHighRatio(op)` | `"RangeHighRatio"` | `measurement` | `value_as_number / range_high` |
| `occurrenceCount(op)` | `"OccurrenceCount"` | `conditionEra`, `drugEra` | rows rolled up into the era |
| `eraLength(op)` | `"EraLength"` | `drugEra` | era length in days |
| `doseValue(op)` | `"DoseValue"` | `doseEra` | dose value |

Each returns an `opAttributeInteger` or `opAttributeNumeric` matching the type of `op`.

### Query Attributes — Date

| Function | Description |
|---|---|
| `startDate(op, type)` | Filter by event start date; `type = "occurrence"` (default) or `"era"` |
| `endDate(op, type)` | Filter by event end date; same `type` options |
| `dateAdjustment(startWith, startOffset, endWith, endOffset)` | Shift the event's effective dates before criteria matching |
| `firstOccurrence()` | Restrict to the first occurrence of the event in the patient's history — the correct tool for incident / new-user / first-ever entry events |

#### `startDate(op, type = "occurrence")` / `endDate(op, type = "occurrence")`

| Param | Type | Default | Notes |
|---|---|---|---|
| `op` | `opAttributeDate` | — | Must be a **date** `opAttribute`; use `lt/lte/gt/gte/eq/bt/nbt` on an `as.Date()` value |
| `type` | `character` | `"occurrence"` | `"occurrence"` or `"era"` |

`startDate`: `"occurrence"` → `name = "OccurrenceStartDate"`; `"era"` → `name = "EraStartDate"`.
`endDate`: `"occurrence"` → `name = "OccurrenceEndDate"`; `"era"` → `name = "EraEndDate"`.
**Usage:** `startDate(gt(as.Date("2010-01-01")))`.

#### `dateAdjustment(startWith = "START_DATE", startOffset = 0L, endWith = "END_DATE", endOffset = 0L)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `startWith` | `character` | `"START_DATE"` | `"START_DATE"` or `"END_DATE"` |
| `startOffset` | `integer` | `0L` | Days to add to `startWith` |
| `endWith` | `character` | `"END_DATE"` | `"START_DATE"` or `"END_DATE"` |
| `endOffset` | `integer` | `0L` | Days to add to `endWith` |

#### `firstOccurrence()`

No parameters. **Returns:** `logicAttribute` (`name = "First"`).
For **incident / new-user cohorts** ("first ever diagnosis of X", "new users of Y"), put
`firstOccurrence()` on the entry Query — `primaryCriteriaLimit = "First"` alone is not enough,
because it takes the first event *matching the criteria* rather than the first in the patient's
history.

**Fixed calendar-date entry:** attaching `startDate()` to `observationPeriod()` produces what
Atlas calls a *user-defined period* — entry at a fixed calendar date instead of a clinical event.
There is no separate constructor; use this combination:

```r
observationPeriod(startDate(eq(as.Date("2017-01-01"))))                        # single fixed date
observationPeriod(startDate(bt(as.Date("2017-01-01"), as.Date("2017-06-30")))) # fixed date range
```

### Query Attributes — Type / Status

Restrict events by their `*_type_concept_id` or status concept (record provenance, e.g. EHR vs.
claims). All share one signature and return a `conceptAttribute`. Only use type filters when the
user explicitly asks for provenance restriction.

**Concept IDs are inputs, not knowledge.** The ids passed to these functions (and to
`measurementUnit()`, `valueAsConcept()`, `providerSpecialtyConcepts()`) must come from the user.
Do not supply concept ids from memory — vocabularies change and a plausible-but-wrong id
generates a silently wrong cohort. If the user asks for e.g. "inpatient visits only" without
giving an id, ask for it or use a clearly marked placeholder with a comment telling the user to
look up the current id in ATHENA. (Ids in this document's examples were verified at the time of
writing.)

| Param | Type | Default | Notes |
|---|---|---|---|
| `ids` | `integer` vector | — | Type/status concept IDs |
| `connection` | DatabaseConnector connection or `NULL` | `NULL` | Optional — used only to look up concept names for Atlas display. Without it the attribute is built from the ids alone, which produces identical SQL |
| `vocabularyDatabaseSchema` | `character` or `NULL` | `NULL` | Schema containing the `concept` table; only used with `connection` |

| Function | Returns `conceptAttribute` with `name =` | CDM column filtered |
|---|---|---|
| `conditionType(ids, ...)` | `"ConditionType"` | `condition_type_concept_id` |
| `conditionStatus(ids, ...)` | `"ConditionStatus"` | `condition_status_concept_id` |
| `drugType(ids, ...)` | `"DrugType"` | `drug_type_concept_id` |
| `visitType(ids, ...)` | `"VisitType"` | `visit_type_concept_id` on `visit()` queries; the linked visit's `visit_concept_id` on other domains — see note below |
| `measurementType(ids, ...)` | `"MeasurementType"` | `measurement_type_concept_id` |
| `observationType(ids, ...)` | `"ObservationType"` | `observation_type_concept_id` |
| `procedureType(ids, ...)` | `"ProcedureType"` | `procedure_type_concept_id` |
| `deathType(ids, ...)` | `"DeathType"` | `death_type_concept_id` |
| `deviceType(ids, ...)` | `"DeviceType"` | `device_type_concept_id` |
| `specimenType(ids, ...)` | `"SpecimenType"` | `specimen_type_concept_id` |
| `observationPeriodType(ids, ...)` | `"PeriodType"` | `period_type_concept_id` |

**`visitType()` means different things on different queries — never use it for care setting on
a `visit()` query.** Verified against generated Circe SQL:

- On a **`visit()` query**, the ConceptSet filters `visit_concept_id` — the kind of visit
  (inpatient, outpatient, ER, ...). `visitType()` filters `visit_type_concept_id`, which is
  *provenance* (how the record was sourced, e.g. "Visit derived from EHR"), not care setting.
  So "inpatient visit" is `visit(cs_ipVisit)` with the inpatient concept in the ConceptSet —
  putting it in `visitType()` instead filters the wrong column and returns zero rows.
- On a **non-visit domain query** (`conditionOccurrence()`, `drugExposure()`, ...),
  `visitType()` joins to the event's linked visit and filters that visit's `visit_concept_id` —
  there it *is* the care-setting filter ("diagnosis recorded during an outpatient visit"; see
  Example 9). The alternative nested-criteria form (`nestedWithAll(atLeast(1, visit(cs), ...))`)
  expresses temporal overlap with a visit rather than the record's own visit link.

**Exclude flags** — pass alongside the matching type attribute to invert it into an exclusion:

#### `conditionTypeExclude(exclude = FALSE)` / `measurementTypeExclude(exclude = FALSE)` / `deathTypeExclude(exclude = FALSE)` / `specimenTypeExclude(exclude = FALSE)`

`exclude`: `logical`. `FALSE` = include the listed types (default); `TRUE` = exclude them.
**Returns:** `keyValueAttribute`.

### Query Attributes — Source Concepts (no database needed)

Restrict an event by `*_source_concept_id` — used when standard-concept mapping is unreliable and
the cohort must match source codes (e.g. ICD-10-CM) directly. Each takes a single `ConceptSet`
and errors if given anything else.

| Function | Returns `conceptSetAttribute` with `name =` | CDM source column |
|---|---|---|
| `conditionSourceConcept(conceptSet)` | `"ConditionSourceConcept"` | `condition_source_concept_id` |
| `drugSourceConcept(conceptSet)` | `"DrugSourceConcept"` | `drug_source_concept_id` |
| `procedureSourceConcept(conceptSet)` | `"ProcedureSourceConcept"` | `procedure_source_concept_id` |
| `observationSourceConcept(conceptSet)` | `"ObservationSourceConcept"` | `observation_source_concept_id` |
| `measurementSourceConcept(conceptSet)` | `"MeasurementSourceConcept"` | `measurement_source_concept_id` |
| `visitSourceConcept(conceptSet)` | `"VisitSourceConcept"` | `visit_source_concept_id` |
| `visitDetailSourceConcept(conceptSet)` | `"VisitDetailSourceConcept"` | `visit_detail_source_concept_id` |

### Query Attributes — Measurement / Observation Values

Measurement and observation values may present in the database as numeric values
(`value_as_number`), strings (`value_as_string`), or concept IDs (`value_as_concept_id`).
Measurements may also be filtered by unit of measure.

| Function | Description |
|---|---|
| `valueAsNumber(op)` | Filter by `value_as_number` (see numeric table above) |
| `valueAsConcept(ids, ...)` | Filter by `value_as_concept_id` using raw concept IDs |
| `valueAsConceptSet(conceptSet)` | Filter by `value_as_concept_id` using a `ConceptSet` |
| `valueAsString(text, op)` | Filter `observation.value_as_string` |
| `measurementUnit(ids, ...)` | Filter by unit concept |

#### `valueAsConcept(ids, connection = NULL, vocabularyDatabaseSchema = NULL)`

Same optional-connection signature as the Type / Status attributes above.
**Returns:** `conceptAttribute` with `name = "ValueAsConcept"`.

#### `valueAsConceptSet(conceptSet)`

`conceptSet`: `ConceptSet` object.
**Returns:** `conceptSetAttribute` with `name = "ValueAsConcept"`. Serializes as an array of
Concept objects in JSON (not a `CodesetId`).

#### `valueAsString(text, op = "contains")`

| Param | Type | Default | Notes |
|---|---|---|---|
| `text` | `character` | — | String to match against `value_as_string` |
| `op` | `character` | `"contains"` | One of `"contains"` (LIKE `%text%`), `"starts"`, `"ends"`, `"equals"` |

**Returns:** `valueAsStringAttribute`. Use with `observation()`.

#### `measurementUnit(ids, connection = NULL, vocabularyDatabaseSchema = NULL)`

Same optional-connection signature as the Type / Status attributes above. `ids` is an integer
vector of unit concept IDs (e.g. `measurementUnit(8554L)` for percent) and is the only supported
input — no `ConceptSet`, no unit strings (e.g. `measurementUnit("%")`), even though older Capr
versions accepted those; errors on anything else.
**Returns:** `conceptAttribute` with `name = "Unit"`. Use with `measurement()`.

### Query Attributes — Visit

#### `providerSpecialtyConcepts(...)`

Restrict a `visit()` by the specialty of the provider conducting the visit.
`...`: integer concept IDs for the desired provider specialties.
**Returns:** `conceptAttribute` with `name = "ProviderSpecialty"`. Use inside `visit()`.

### Demographics

Used **directly inside a Group** (typically in `attrition()`) to express demographic inclusion
rules, or via `...` in a query constructor to filter a specific event by patient age/gender.

| Function | Description |
|---|---|
| `age(op)` | Patient age; `op` from a comparison operator, e.g. `age(gte(18L))` |
| `male()` | Male patients (gender concept 8507). No parameters |
| `female()` | Female patients (gender concept 8532). No parameters |
| `genderConcepts(...)` | One or more gender concept IDs (integers, coerced with `as.integer()`) |

`male()`, `female()`, and `genderConcepts()` each return a `conceptAttribute` with
`name = "Gender"`.

```r
attrition(
  "adult males" = withAll(male(), age(gte(18L))),
  expressionLimit = "First"
)
```

## Worked Examples

All examples assume concept sets named `cs_*` already exist in the session as `ConceptSet`
objects.

### 1. Simple entry event (single concept set, single domain)

**Intent:** Persons with GI bleed, entering the cohort at the start of their first GI bleed
condition occurrence and exiting at the end of continuous observation.

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

**Demonstrates:** the minimal cohort skeleton; `attrition()` with zero groups (valid when there
are no inclusion rules, `expressionLimit` still explicit).

### 2. Entry + required inclusion criteria (prior observation + lab value)

**Intent:** Persons with type 2 diabetes, entering at their first T2DM diagnosis, restricted to
those with at least 365 days of prior continuous observation and an HbA1c value below 5.7%
recorded any time before index; exit at end of continuous observation.

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
                measurementUnit(8554L)),  # percent
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

**Demonstrates:** named attrition Groups; `observationPeriod()` (no `conceptSet` argument) to
require prior observation; `measurement()` with `valueAsNumber()` + `measurementUnit()`.

### 3. Entry + absence criteria (no prior related diagnosis)

**Intent:** Persons with type 2 diabetes, entering at their first T2DM diagnosis, require ≥365 days of
prior observation, exclude anyone with a type 1 or secondary diabetes diagnosis at any time on or
before index; exit at end of continuous observation.

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

**Demonstrates:** absence/exclusion via `exactly(0, ...)`; multiple named attrition rules.

### 4. Demographic criteria (age, gender)

**Intent:** Persons with type 2 diabetes, entering at their first T2DM diagnosis with ≥365 days
of prior continuous observation, restricted to males aged 18 or older at index; exit at end of
continuous observation.

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

**Demonstrates:** `observationWindow = continuousObservation(...)` as the simpler way to require
prior observation; `male()` and `age()` passed directly into a Group as a demographic rule.

### 5. Exit strategy variations (fixed duration, drug era, censoring)

**Intent:** Three ways to define a cohort's end date: (a) a fixed 30 days after entry; (b) the
end of a continuous ACE-inhibitor exposure era allowing 30-day gaps, plus a 7-day surveillance
buffer; (c) end of observation, censored early if the person dies.

```r
# a) Exit 30 days after cohort start
exit(endStrategy = fixedExit(index = "startDate", offsetDays = 30L))

# b) Exit at the end of a continuous ACE inhibitor drug era
exit(endStrategy = drugExit(conceptSet = cs_aceInhibitors,
                            persistenceWindow = 30L,
                            surveillanceWindow = 7L))

# c) Exit at end of observation, censored early at death
exit(
  endStrategy = observationExit(),
  censor = censoringEvents(death())
)
```

**Demonstrates:** `fixedExit()`, `drugExit()`, `censoringEvents()` with a `death()` query. Plug
one of these into a `cohort()`'s `exit` argument.

### 6. Nested/correlated criteria

**Intent:** Persons exposed to metformin, where the metformin exposure event itself must have an
HbA1c measurement in the 30 days up to and including the exposure date — the lab check is tied to
that individual drug event, not the cohort index date.

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

**Demonstrates:** `nestedWithAll()` as a query attribute — sub-criteria anchored to the query's
own event date. Use nesting when the temporal requirement is relative to the event; use
`attrition()` when it's relative to the cohort index.

### 7. Filtering entry events with a co-occurring event, via `attrition`

**Intent:** Persons with acute stroke, entering the cohort at each stroke diagnosis that
co-occurs with an inpatient visit (from the day before the visit starts through the visit end);
each qualifying episode exits 7 days after entry, and episodes within 180 days of each other are
collapsed into one era.

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

**Demonstrates:** `primaryCriteriaLimit = "All"` + attrition with `expressionLimit` to filter
among entry events — safe *here* because both limits are `"All"`, so no selection happens before
the rule runs; when the intent is "first qualifying event", define the qualifier on the entry
Query itself instead (see "Fully define the index event in entry()" and Anti-Patterns #2);
`startWindow` + `endWindow` together to express "event overlaps index"; `era(eraDays = 180L)` to
merge nearby episodes.

### 8. Source concept filtering

**Intent:** Persons with lung fibrosis, entering at their first diagnosis identified by source
codes (e.g. ICD-10-CM) rather than mapped standard concepts, because the standard mapping is
unreliable for this condition; exit at end of continuous observation.

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

**Demonstrates:** `conceptSet = NULL` passed explicitly, with `conditionSourceConcept()` doing
the actual concept filtering against `condition_source_concept_id`.

### 9. Multiple entry paths with a nested co-occurrence requirement ("2 outpatient, 1 inpatient")

**Intent:** Persons with COPD, entering at the first qualifying diagnosis — either (a) an
outpatient COPD diagnosis that is itself preceded by another outpatient COPD diagnosis in the
prior 365 days, or (b) a single inpatient visit with COPD as the primary diagnosis; exit at end
of continuous observation. (Translated from a real Atlas-exported definition; the classic
"2 outpatient or 1 inpatient" algorithm.)

```r
cd <- cohort(
  entry = entry(
    # entry path (a): an outpatient COPD dx with a *prior* outpatient COPD dx
    # in the preceding 365 days (i.e. "2 outpatient" occurrences)
    conditionOccurrence(
      cs_COPD,
      visitType(9202L), # standard concept for OP visit
      nestedWithAll(
        atLeast(1,
          conditionOccurrence(cs_COPD, visitType(9202L)),
          aperture = duringInterval(
            startWindow = eventStarts(-365, -1, index = "startDate")
          )
        )
      )
    ),
    # entry path (b): a single inpatient COPD dx ("1 inpatient")
    conditionOccurrence(
      cs_COPD,
      visitType(9201L), # standard concept for IP visit
      conditionStatus(c(32901L, 32902L)) # primary diagnosis / primary admission diagnosis
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

**Demonstrates:** multiple entry Queries OR'd as alternative qualifying paths; nested criteria
inside an entry Query; `visitType()` / `conditionStatus()` to restrict a *condition* query to a
care setting — this works only on non-visit domains, where `visitType()` filters the linked
visit's `visit_concept_id`; on a `visit()` query the care setting goes in the ConceptSet instead
(see "Query Attributes — Type / Status" above).
No database connection is needed — pass `connection`/`vocabularyDatabaseSchema` only if the user
wants concept names displayed in Atlas (see "Query Attributes — Type / Status" above).

### 10. Fixed-date yearly denominator cohort

**Intent:** A denominator cohort where every person enters on January 1st of each calendar year
(2017–2019) in which they have ≥365 days of prior continuous observation, and stays in the cohort
for exactly one year — a person present across all three years gets three separate yearly
episodes. (Translated from a real Atlas-exported rate-denominator definition.)

```r
cd <- cohort(
  entry = entry(
    observationPeriod(startDate(eq(as.Date("2017-01-01")))),
    observationPeriod(startDate(eq(as.Date("2018-01-01")))),
    observationPeriod(startDate(eq(as.Date("2019-01-01")))),
    observationWindow = continuousObservation(365L, 0L),
    primaryCriteriaLimit = "All"
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

**Demonstrates:** `observationPeriod()` + `startDate(eq(...))` to anchor entry to fixed calendar
dates rather than a clinical event; one entry Query per year; `primaryCriteriaLimit = "All"` +
`expressionLimit = "All"` to keep every qualifying year per person; `era(eraDays = 0L)` so
back-to-back yearly episodes stay separate rather than merging.

### 11. Multi-domain "2 qualifying diagnoses" cohort (condition and/or observation)

**Intent:** Persons with two qualifying dysphagia diagnoses within 365 days of each other,
entering at the first qualifying (second) diagnosis and exiting at end of continuous observation.
The diagnosis may be recorded as either a `conditionOccurrence` or an `observation` record
depending on the data source, and this applies independently to *both* the index diagnosis and the
qualifying prior diagnosis — so all four domain pairings (condition-then-condition,
condition-then-observation, observation-then-condition, observation-then-observation) must count.
This is a common annoyance in phenotype algorithms: the same clinical concept isn't always
recorded in one consistent domain, so "look for X, then look for X again" silently undercounts the
cohort unless every domain combination for *both* occurrences is covered.

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
N domains for both the index and prior event means N top-level entry Queries × N nested
alternatives each = N² combinations to cover, not N. Ask the user which domains the event of
interest can appear in before generating code like this — don't assume a single domain just
because one was mentioned first.

## Anti-Patterns & Common Mistakes

Most hallucinated function names or wrong argument types throw an immediate R error and are
caught by executing the code. The items below are different: **`compile()` accepts them without
error**, but the result is silently wrong or silently redundant.

### 1. Passing a bare `Criteria` as a named `attrition()` rule

```r
# DO NOT
attrition(
  'abnormal hba1c' = atLeast(1, measurement(cs_hba1c, valueAsNumber(lt(13))))
)

# DO
attrition(
  'abnormal hba1c' = withAll(
    atLeast(1, measurement(cs_hba1c, valueAsNumber(lt(13))))
  )
)
```

Every named attrition rule must be a `Group` (`withAll()` / `withAny()` / `withAtLeast()` /
`withAtMost()`), even for a single criterion. A bare `Criteria` serializes as the wrong JSON
shape for an inclusion rule — `compile()` will not error, the JSON is well-formed, but Atlas
cannot render the rule (it silently disappears in the UI).

### 2. Qualifying the index event outside the entry Query

```r
# DO NOT — index-event restriction as an attrition rule: with primaryCriteriaLimit = "First",
# the person's first raw dx is chosen BEFORE the rule runs; persons whose first dx fails it
# are dropped entirely instead of entering at their first qualifying event
entry(conditionOccurrence(cs_stroke), primaryCriteriaLimit = "First")
# ... plus an "inpatient at index" rule in attrition()

# DO — make the restriction part of the index event definition (attributes / nested criteria)
entry(
  conditionOccurrence(
    cs_stroke,
    nestedWithAll(
      atLeast(1, visit(cs_ipVisit),
              duringInterval(eventStarts(-Inf, 0), endWindow = eventEnds(0, Inf)))
    )
  ),
  primaryCriteriaLimit = "First"
)
```

Both versions compile and run; they define **different cohorts** (see "Fully define the index
event in entry()" in the Agent Workflow section for the order-of-operations explanation). The
forms only coincide when the limits are matched to the same semantics (e.g.
`primaryCriteriaLimit = "All"` with `expressionLimit` doing the selection — see Worked
Example 7); do not rely on that. The same goes for `entry()`'s
`additionalCriteria`/`qualifiedLimit`: it can express event-level filtering, but nested criteria
on the entry Query cover the same logic with clearer semantics — avoid it.

### 3. Expecting `qualifiedLimit` to do something on its own

`qualifiedLimit` only affects the generated SQL when `additionalCriteria` is also set. Without
`additionalCriteria`, setting `qualifiedLimit` is a no-op — leave it unset.

## Output

Once the `Cohort` object is built with `cohort()`, finish with one of:

```r
json <- compile(cohortObject)          # returns the OHDSI cohort definition as a JSON string
writeCohort(cohortObject, "cohort.json")  # writes the JSON to a file (path must end in .json)
```

## Quick Reference Card

```r
library(Capr)

cohort(
  entry = entry(
    <Query>, [<Query>, ...],                    # multiple Queries are OR'd entry paths
    observationWindow = continuousObservation(priorDays, postDays),
    primaryCriteriaLimit = "First" | "All" | "Last"
  ),
  attrition = attrition(
    "<rule label>" = withAll(<Criteria/Group/demographic>, ...),   # or withAny / withAtLeast(x,) / withAtMost(x,)
    ...,
    expressionLimit = "First" | "All" | "Last"
  ),
  exit = exit(
    endStrategy = observationExit()
                | fixedExit(index = "startDate" | "endDate", offsetDays = <int>)
                | drugExit(<ConceptSet>, persistenceWindow, surveillanceWindow),
    censor = censoringEvents(<Query>, ...)      # optional
  ),
  era = era(eraDays = <int>, studyStartDate = <Date>, studyEndDate = <Date>)   # optional
)

# Queries (first arg = ConceptSet; attributes via ...):
conditionOccurrence(cs, ...)  drugExposure(cs, ...)  measurement(cs, ...)  observation(cs, ...)
procedure(cs, ...)  visit(cs, ...)  deviceExposure(cs, ...)  death(cs = NULL, ...)
observationPeriod(...)  conditionEra(cs, ...)  drugEra(cs, ...)  doseEra(cs, ...)  specimen(cs, ...)

# Criteria (counting logic around a Query):
exactly(x, query, aperture)   atLeast(x, query, aperture)   atMost(x, query, aperture)
# absence:  exactly(0, query, aperture)

# Assessment window:
duringInterval(eventStarts(a, b, index = "startDate" | "endDate"),
               endWindow = eventEnds(a, b, ...),          # optional
               restrictVisit = FALSE, ignoreObservationPeriod = FALSE)
# days relative to index: negative = before, 0 = index date, positive = after; -Inf/Inf = unbounded
# e.g. all time before index: eventStarts(-Inf, -1); prior 365d incl. index: eventStarts(-365, 0)

# Comparison ops (integer 18L / numeric 18 / date as.Date("...")):
lt(x) lte(x) gt(x) gte(x) eq(x) bt(x, y) nbt(x, y)

# Common attributes (pass via ... in a Query):
age(op)  valueAsNumber(op)  measurementUnit(ids)  valueAsConceptSet(cs)  startDate(op)  endDate(op)
firstOccurrence()  conditionSourceConcept(cs)  drugSourceConcept(cs)  nestedWithAll(<Criteria>, ...)
visitType(ids)  conditionType(ids)  conditionStatus(ids)   # provenance/type filters, ids = integer concept ids

# Demographics (inside a Group, typically in attrition):
male()  female()  genderConcepts(<ids>)  age(op)

# Serialize:
compile(cd)                 # -> JSON string
writeCohort(cd, "cd.json")  # -> file
```
