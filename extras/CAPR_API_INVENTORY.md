# Capr Public API Reference (Inventory)

Verified against source in `R/`. Built as an intermediate artifact toward `CAPR_REFERENCE.md`.
See `CLAUDE_CODE_INSTRUCTIONS.md` for the full build plan.

Package version: **2.1.1**

**Type notation:**
- `ConceptSet` — S4 object (assumed pre-built; concept set construction is out of scope)
- `Query` — S4 object from a domain query constructor
- `Criteria` — S4 object from `exactly()` / `atLeast()` / `atMost()`
- `Group` — S4 object from `withAll()` / `withAny()` / `withAtLeast()` / `withAtMost()`
- `EventWindow` — S4 object from `eventStarts()` / `eventEnds()`
- `EventAperture` — S4 object from `duringInterval()`
- `opAttribute` — any of `opAttributeInteger`, `opAttributeNumeric`, `opAttributeDate`

---

## Top-Level Cohort Assembly

| Function | Description |
|---|---|
| `cohort(entry, attrition, exit, era)` | Assemble a complete `Cohort` object from its four components |
| `entry(..., observationWindow, primaryCriteriaLimit, additionalCriteria, qualifiedLimit)` | Define the index event(s) and qualifying conditions |
| `attrition(..., expressionLimit)` | Define named inclusion/exclusion rule groups |
| `exit(endStrategy, censor)` | Wrap an end strategy and optional censoring criteria |
| `era(eraDays, studyStartDate, studyEndDate)` | Set era-collapse padding and optional study date window |

---

### `cohort(entry, attrition = NULL, exit = NULL, era = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `entry` | `CohortEntry` | — | |
| `attrition` | `CohortAttrition` or `NULL` | `NULL` | |
| `exit` | `CohortExit` or `NULL` | `NULL` | If `NULL`, defaults to `observationExit()` |
| `era` | `CohortEra` or `NULL` | `NULL` | If `NULL`, defaults to `era(eraDays = 0L)` |

**Returns:** `Cohort` S4 object.

---

## Cohort Entry

### `entry(..., observationWindow = continuousObservation(0L, 0L), primaryCriteriaLimit = c("First", "All", "Last"), additionalCriteria = NULL, qualifiedLimit = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `...` | `Query` | — | One or more index event Query objects |
| `observationWindow` | `ObservationWindow` | `continuousObservation(0L, 0L)` | The number of days of required observation time before and after index. From `continuousObservation()` |
| `primaryCriteriaLimit` | `character` | `"First"` | Whether the first, last, or all events qualifying for index event Query should be included in the cohort. One of `"First"`, `"All"`, `"Last"` |
| `additionalCriteria` | `Group` or `NULL` | `NULL` | Restricts qualifying events with a Group of Criteria before applying `qualifiedLimit` |
| `qualifiedLimit` | `character` or `NULL` | `NULL` | Whether the first, last, or all events qualifying for index event Query **and additionalCriteria** should be included in the cohort. One of `"First"`, `"All"`, `"Last"`. **Required when `additionalCriteria` is non-`NULL`.** |

**Returns:** `CohortEntry` S4 object.
**Validation:** `primaryCriteriaLimit` matched via `checkmate::matchArg()`. `qualifiedLimit`, if provided, also matched via `checkmate::matchArg()`.

### `continuousObservation(priorDays = 0L, postDays = 0L)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `priorDays` | `integer` | `0L` | Min observation days before cohort index |
| `postDays` | `integer` | `0L` | Min observation days after cohort index |

**Returns:** `ObservationWindow` S4 object.

---

## Inclusion Criteria (Attrition)

### `attrition(..., expressionLimit = c("First", "All", "Last"))`

| Param | Type | Default | Notes |
|---|---|---|---|
| `...` | **Named** `Group` objects | — | Restricts cohort inclusion via one or more Groups of Criteria |
| `expressionLimit` | `character` | `"First"` | Whether the first, last, or all events qualifying for the inclusion criteria should be included in the cohort. One of `"First"`, `"All"`, `"Last"` |

**Returns:** `CohortAttrition` S4 object.
**Usage:** `attrition("Prior T2DM" = withAll(...), "No prior insulin" = withAll(...))`.

---

## Cohort Exit

### `exit(endStrategy, censor = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| endStrategy | `ObservationExit`, `DrugExposureExit`, or `FixedDurationExit` | `observationExit()` | An endStrategy object which defines how to compute the default end date for each cohort event |
| censor | `CensoringCriteria` | NULL | A `censoringEvents` call containing one or more Query objects defining events upon which a cohort event will be censored |

**Returns:** `CohortExit` S4 object.
**Usage:** `exit(endStrategy = observationExit(), censor = NULL)`.

### endStrategy Options

Passed as `endStrategy` to `exit()`.

| Function | Description |
|---|---|
| `observationExit()` | Exit at end of continuous observation (default) |
| `fixedExit(index, offsetDays)` | Exit `offsetDays` days after event start or end |
| `drugExit(conceptSet, persistenceWindow, surveillanceWindow, daysSupplyOverride)` | Exit at end of continuous drug era |

---

#### `observationExit()`
No parameters.
**Returns:** `ObservationExit` S4 object (serializes as empty `EndStrategy` in JSON).

#### `fixedExit(index = c("startDate", "endDate"), offsetDays)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `index` | `character` | `"startDate"` | `"startDate"` or `"endDate"` |
| `offsetDays` | `numeric` | **Required — no default** | Coerced to `integer` |

**Returns:** `FixedDurationExit` S4 object.
**Validation:** `checkmate::matchArg(index, c("startDate", "endDate"))`.

#### `drugExit(conceptSet, persistenceWindow = 0L, surveillanceWindow = 0L, daysSupplyOverride = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `conceptSet` | `ConceptSet` | — | Drug ingredient concept set |
| `persistenceWindow` | `integer` | `0L` | Max gap in days between drug records when building continuous era |
| `surveillanceWindow` | `integer` | `0L` | Days added to end of era before cohort exit |
| `daysSupplyOverride` | `integer` or `NULL` | `NULL` | Force a fixed days supply; `NULL` = use actual |

**Returns:** `DrugExposureExit` S4 object.
**Validation:** `checkmate::expect_integerish()` on all numeric args.

### censoringEvents

#### `censoringEvents(...)`

Define query-based events that trigger cohort exit; passed as `censor` in `exit()`
`...`: `Query` objects.
**Returns:** `CensoringCriteria` S4 object.

---

## Era Logic

### `era(eraDays = 0L, studyStartDate = NULL, studyEndDate = NULL)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `eraDays` | `integer` | `0L` | Days of gap allowed between neighboring cohort episodes when collapsing episodes into eras |
| `studyStartDate` | `Date` or `NULL` | `NULL` | Left-censors cohort era start dates to the specified date; `NULL` = no restriction |
| `studyEndDate` | `Date` or `NULL` | `NULL` | Right-censors cohort era end dates to the specified date; `NULL` = no restriction |

**Returns:** `CohortEra` S4 object.

---

## Queries, Criteria, and Groups

A Query is the building block of all cohort entry, attrition, and censoring logic applied in a cohort definition.  Query objects can be wrapped as Criteria objects, allowing application of counting logic.  One or more Criteria can be included in a Group.

### Query Constructors

Query objects are created using one of the following domain-specific constructor functions.  The Query generated by a given constructor will only function against its corresponding OMOP CDM table. 

All constructors take `conceptSet` as their first argument plus zero or more attribute objects via `...`. The `conceptSet` argument is required and has no default — pass `conceptSet = NULL` explicitly if no concept filter is needed. `death()` and `observationPeriod()` are exceptions (see below).

**Domain mismatch warning:** If the `ConceptSet` contains concepts whose `domain_id` doesn't match the query domain (e.g. Drug concepts in a `conditionOccurrence()` call), `rlang::warn()` is issued — not an error.

| Function | OMOP table | `conceptSet` | Notes |
|---|---|---|---|
| `conditionOccurrence(conceptSet, ...)` | CONDITION_OCCURRENCE | Required | |
| `conditionEra(conceptSet, ...)` | CONDITION_ERA | Required | Prefer conditionOccurrence unless specifically asked for Era by user |
| `drugExposure(conceptSet, ...)` | DRUG_EXPOSURE | Required | |
| `drugEra(conceptSet, ...)` | DRUG_ERA | Required | Prefer drugExposure unless specifically asked for Era by user |
| `doseEra(conceptSet, ...)` | DOSE_ERA | Required | |
| `measurement(conceptSet, ...)` | MEASUREMENT | Required | |
| `observation(conceptSet, ...)` | OBSERVATION | Required | |
| `procedure(conceptSet, ...)` | PROCEDURE | Required | |
| `visit(conceptSet, ...)` | VISIT_OCCURRENCE | Required | |
| `visitDetail(conceptSet, ...)` | VISIT_DETAIL | Required | Prefer visit unless specifically asked for Visit Detail by user |
| `deviceExposure(conceptSet, ...)` | DEVICE_EXPOSURE | Required | |
| `specimen(conceptSet, ...)` | SPECIMEN | Required | |
| `death(conceptSet = NULL, ...)` | DEATH | Optional; defaults to `NULL` | |
| `observationPeriod(...)` | OBSERVATION_PERIOD | No `conceptSet` argument at all | |

**Returns:** `Query` S4 object (all constructors).

### Criteria Constructors

Criteria objects wrap a Query with the desired occurrence count and temporal window.  They are created using one of the following constructor functions.

| Function | Description |
|---|---|
| `exactly(x, query, aperture, distinct, countColumn)` | Require exactly `x` occurrences of `query` within `aperture` |
| `atLeast(x, query, aperture, distinct, countColumn)` | Require at least `x` occurrences |
| `atMost(x, query, aperture, distinct, countColumn)` | Require at most `x` occurrences |

All three share the same signature:

#### `exactly(x, query, aperture = duringInterval(eventStarts(-Inf, Inf)), distinct = NA, countColumn = NA_character_)`
#### `atLeast(x, query, aperture = duringInterval(eventStarts(-Inf, Inf)), distinct = NA, countColumn = NA_character_)`
#### `atMost(x, query, aperture = duringInterval(eventStarts(-Inf, Inf)), distinct = NA, countColumn = NA_character_)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `x` | `integer` | — | Number of occurrences to require |
| `query` | `Query` | — | From a domain query constructor |
| `aperture` | `EventAperture` | `duringInterval(eventStarts(-Inf, Inf))` | The time window within which to look for the event. See below for detail on the `EventAperture` object |
| `distinct` | `logical` | `NA` | If `NA` or `FALSE`, count all event occurrences. If `TRUE`, count distinct event occurrences by `countColumn` |
| `countColumn` | `character` | `NA_character_` | If `distinct` = `TRUE`, the type of column on which to count distinct occurrences. One of `"DOMAIN_CONCEPT"`, `"START_DATE"`, `"VISIT_ID"`. |

**Returns:** `Criteria` S4 object.

#### EventAperture

The assessment window for a Criteria object is defined using `EventAperture`.  EventAperture objects are constructed using the `duringInterval` function.

##### `duringInterval(startWindow, endWindow = NULL, restrictVisit = FALSE, ignoreObservationPeriod = FALSE)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `startWindow` | `EventWindow` | — | The time window within which the event must start, constructed using `eventStarts()` |
| `endWindow` | `EventWindow` or `NULL` | `NULL` | The time window within which the event must end, constructed using `eventEnds()` |
| `restrictVisit` | `logical` | `FALSE` | If `TRUE`, the event must occur in the same visit as the index event |
| `ignoreObservationPeriod` | `logical` | `FALSE` | If `TRUE`, allow events outside the observation period containing the index event |

**Returns:** `EventAperture` S4 object.

##### `eventStarts(a, b, index = c("startDate", "endDate"))`
##### `eventEnds(a, b, index = c("startDate", "endDate"))`

| Param | Type | Default | Notes |
|---|---|---|---|
| `a` | `numeric` | — | Left bound in signed days relative to index (negative = before index, positive = after). `-Inf` = all time before |
| `b` | `numeric` | — | Right bound in signed days relative to index. `Inf` = all time after |
| `index` | `character` | `"startDate"` | Specify `"startDate"` to set window relative to index event start date, or `"endDate"` to set window relative to index event end date |

**Returns:** `EventWindow` S4 object. `eventStarts` sets `event = "start"`; `eventEnds` sets `event = "end"`.

**Sign convention for zero:** `0` is treated as event occurring on the index date. So `eventStarts(-365, 0)` means "from 365 days before up to and including the index date". Use `b = -1` to exclude the index date itself.

**Validation:** `index` matched via `checkmate::matchArg()`.

### Group Constructors

Group objects combine Criteria or Group objects with a logical operator.

**Exception: Demographics attributes can be passed directly to a Group to apply demographics requirements as inclusion criteria (see below).**

| Function | Description |
|---|---|
| `withAll(...)` | All criteria/groups must be satisfied |
| `withAny(...)` | Any one criteria/group must be satisfied |
| `withAtLeast(x, ...)` | At least `x` criteria/groups must be satisfied |
| `withAtMost(x, ...)` | At most `x` criteria/groups must be satisfied |

#### `withAll(...)` / `withAny(...)`
`...`: any mix of `Criteria` and `Group` objects.
**Returns:** `Group` S4 object.

#### `withAtLeast(x, ...)` / `withAtMost(x, ...)`

| Param | Type | Notes |
|---|---|---|
| `x` | `integer` | Min/max number of the supplied items that must be satisfied |
| `...` | `Criteria` and/or `Group` | |

**Returns:** `Group` S4 object.

### Nested (Correlated) Criteria Constructors

Used as a query **attribute** — passed via `...` in a Query constructor call — to express sub-criteria that must hold for the same patient at a specified time relative to the event.

| Function | Description |
|---|---|
| `nestedWithAll(...)` | All sub-criteria must hold |
| `nestedWithAny(...)` | Any sub-criterion must hold |
| `nestedWithAtLeast(x, ...)` | At least `x` sub-criteria must hold |
| `nestedWithAtMost(x, ...)` | At most `x` sub-criteria must hold |

#### `nestedWithAll(...)` / `nestedWithAny(...)`
`...`: `Criteria` and/or `Group` objects.
**Returns:** `nestedAttribute` S4 object.

#### `nestedWithAtLeast(x, ...)` / `nestedWithAtMost(x, ...)`

| Param | Type |
|---|---|
| `x` | `integer` |
| `...` | `Criteria` and/or `Group` |

**Returns:** `nestedAttribute` S4 object.

```r
# Drug exposure that also has a prior lab within 30 days
drugExposure(
  conceptSet = cs_metformin,
  nestedWithAll(
    atLeast(1, measurement(conceptSet = cs_hba1c),
                           aperture = duringInterval(eventStarts(-30, 0))
    )
  )
)
```

### Query Attributes

#### Numeric

All follow pattern `f(op)` where `op` is any `opAttribute`. All stop with an error if `op` is not an `opAttribute` subclass. Pass via `...` in domain query constructors.

| Function | Returns `name =` | Recommended domain | CDM column | Notes |
|---|---|---|---|---|
| `age(op)` | `"Age"` | Any | `year_of_birth` | Patient age at event date |
| `daysOfSupply(op)` | `"DaysSupply"` | `drugExposure` | `days_supply` | |
| `drugRefills(op)` | `"Refills"` | `drugExposure` | `refills` | |
| `drugQuantity(op)` | `"Quantity"` | `drugExposure` | `quantity` | |
| `valueAsNumber(op)` | `"ValueAsNumber"` | `measurement`, `observation` | `value_as_number` | |
| `rangeHigh(op)` | `"RangeHigh"` | `measurement` | `range_high` | |
| `rangeLow(op)` | `"RangeLow"` | `measurement` | `range_low` | |
| `rangeHighRatio(op)` | `"RangeHighRatio"` | `measurement` | `value_as_number / range_high` | |
| `occurrenceCount(op)` | `"OccurrenceCount"` | `conditionEra`, `drugEra` | | Counts number of condition_occurrence or drug_exposure rows rolled up into the era |
| `eraLength(op)` | `"EraLength"` | `drugEra` | | Era length in days |
| `doseValue(op)` | `"DoseValue"` | `doseEra` | | Drug dose value |

All return an `opAttributeInteger` or `opAttributeNumeric` depending on the function.

#### Date

| Function | Description |
|---|---|
| `startDate(op, type)` | Filter by event start date; `type = "occurrence"` (default) or `"era"` |
| `endDate(op, type)` | Filter by event end date; same `type` options |
| `dateAdjustment(startWith, startOffset, endWith, endOffset)` | Shift event's effective dates before criteria matching |
| `firstOccurrence()` | Restrict to the first occurrence of the event in the patient's history |

---

##### `startDate(op, type = "occurrence")`
##### `endDate(op, type = "occurrence")`

| Param | Type | Default | Notes |
|---|---|---|---|
| `op` | `opAttributeDate` | — | Must be a date `opAttribute`; use `lt/lte/gt/gte/eq/bt/nbt` on a `Date` value |
| `type` | `character` | `"occurrence"` | `"occurrence"` or `"era"` |

`startDate`: `"occurrence"` → `name = "OccurrenceStartDate"`; `"era"` → `name = "EraStartDate"`.
`endDate`: `"occurrence"` → `name = "OccurrenceEndDate"`; `"era"` → `name = "EraEndDate"`.

**Returns:** `opAttributeDate`.
**Validation:** `stop()` if `op` is not `opAttributeDate`.
**Usage:** `startDate(gt(as.Date("2010-01-01")))`.

**Special case — `observationPeriod()` + fixed calendar dates:** When `startDate()` is attached
to `observationPeriod()` (rather than a clinical event query), Capr serializes it as
`UserDefinedPeriod` instead of `OccurrenceStartDate`/`OccurrenceEndDate` — this is what Atlas
calls a fixed-date entry event (e.g. "enter the cohort on 2017-01-01" regardless of any clinical
event). There is no separate `userDefinedPeriod()` constructor; it's produced automatically by
this combination:
- `startDate(eq(as.Date("2017-01-01")))` → `UserDefinedPeriod` with `StartDate` == `EndDate`
  (a single fixed date)
- `startDate(bt(as.Date("2017-01-01"), as.Date("2017-06-30")))` → `UserDefinedPeriod` with
  distinct `StartDate`/`EndDate` (a fixed date range)

Verified against `R/query.R:387-397` (`as.list,Query-method`), which special-cases
`x@domain == "ObservationPeriod"` to convert the `OccurrenceStartDate` op into `UserDefinedPeriod`
at serialization time, and against generated JSON via `compile()`.

---

##### `dateAdjustment(startWith = "START_DATE", startOffset = 0L, endWith = "END_DATE", endOffset = 0L)`

| Param | Type | Default | Notes |
|---|---|---|---|
| `startWith` | `character` | `"START_DATE"` | `"START_DATE"` or `"END_DATE"` |
| `startOffset` | `integer` | `0L` | Days to add to `startWith` |
| `endWith` | `character` | `"END_DATE"` | `"START_DATE"` or `"END_DATE"` |
| `endOffset` | `integer` | `0L` | Days to add to `endWith` |

**Returns:** `dateAdjustmentAttribute` S4 object.

---

##### `firstOccurrence()`
No parameters. **Returns:** `logicAttribute` S4 object (`name = "First"`). Restricts to the first recorded occurrence of the event.

---

#### Comparison Operators

Used as the `op` argument in numeric and date attribute functions (e.g. `age(gte(18L))`). S4 generics that dispatch on the type of `x`, returning the matching `opAttribute` subclass.

| Function | Meaning | Returns |
|---|---|---|
| `lt(x)` | `< x` | `opAttributeInteger` / `opAttributeNumeric` / `opAttributeDate` |
| `lte(x)` | `<= x` | same |
| `gt(x)` | `> x` | same |
| `gte(x)` | `>= x` | same |
| `eq(x)` | `== x` | same |
| `bt(x, y)` | `x <= value <= y` (between, inclusive) | same |
| `nbt(x, y)` | `value < x` or `value > y` (not between) | same |

**Type rules:**
- Pass `18L` (integer literal) for integer operators → returns `opAttributeInteger`
- Pass `18` or `18.0` → returns `opAttributeNumeric`
- Pass `as.Date("2020-01-01")` → returns `opAttributeDate`
- For `bt(x, y)` and `nbt(x, y)`: both `x` and `y` must be the same type

#### Type / Status

These attributes restrict events to specified type concepts and/or status concepts.

All follow signature `f(ids, connection, vocabularyDatabaseSchema)`, look up concept metadata from a connected OMOP CDM vocabulary, and return a `conceptAttribute`.

| Param | Type | Notes |
|---|---|---|
| `ids` | `integer` vector | Concept IDs to look up |
| `connection` | DBI connection | Active connection to an OMOP CDM |
| `vocabularyDatabaseSchema` | `character` | Schema containing the `concept` table |

| Function | Returns `conceptAttribute` with `name =` | CDM column filtered |
|---|---|---|
| `conditionType(ids, ...)` | `"ConditionType"` | `condition_type_concept_id` |
| `conditionStatus(ids, ...)` | `"ConditionStatus"` | `condition_status_concept_id` |
| `drugType(ids, ...)` | `"DrugType"` | `drug_type_concept_id` |
| `visitType(ids, ...)` | `"VisitType"` | `visit_type_concept_id` |
| `measurementType(ids, ...)` | `"measurementType"` | `measurement_type_concept_id` |
| `observationType(ids, ...)` | `"observationType"` | `observation_type_concept_id` |
| `procedureType(ids, ...)` | `"procedureType"` | `procedure_type_concept_id` |
| `observationPeriodType(ids, ...)` | `"observationPeriodType"` | `period_type_concept_id` |

**Exclude flags** — include along with the respective `xType` attribute to *exclude* events with the listed type concept IDs:

##### `conditionTypeExclude(exclude = FALSE)` / `measurementTypeExclude(exclude = FALSE)` / `deathTypeExclude(exclude = FALSE)` / `specimenTypeExclude(exclude = FALSE)`
`exclude`: `logical`. `FALSE` = include all types (default). `TRUE` = exclude.
**Returns:** `keyValueAttribute`.

#### Source Concepts

Restrict an event by concept IDs present in the `source_concept_id` field.  Used when OMOP standard concepts cannot be resolved to an appropriate concept set for the event of interest.

All accept a single `ConceptSet` argument. All call `rlang::abort()` if argument is not a `ConceptSet`. No DB required.

| Function | Returns `conceptSetAttribute` with `name =` | CDM source column |
|---|---|---|
| `conditionSourceConcept(conceptSet)` | `"ConditionSourceConcept"` | `condition_source_concept_id` |
| `drugSourceConcept(conceptSet)` | `"DrugSourceConcept"` | `drug_source_concept_id` |
| `procedureSourceConcept(conceptSet)` | `"ProcedureSourceConcept"` | `procedure_source_concept_id` |
| `observationSourceConcept(conceptSet)` | `"ObservationSourceConcept"` | `observation_source_concept_id` |
| `measurementSourceConcept(conceptSet)` | `"MeasurementSourceConcept"` | `measurement_source_concept_id` |
| `visitSourceConcept(conceptSet)` | `"VisitSourceConcept"` | `visit_source_concept_id` |
| `visitDetailSourceConcept(conceptSet)` | `"VisitDetailSourceConcept"` | `visit_detail_source_concept_id` |

#### Domain-specific Attributes

##### Visit

###### `providerSpecialtyConcepts(...)`

Restrict visit by the specialty of the provider conducting the visit.

`...`: integer concept IDs for the desired provider specialties
**Returns:** `conceptAttribute` with `name = "ProviderSpecialty"`. Use inside `visit()`.

##### Measurement & Observation

The following attributes apply to measurement & observation values, which may present in the database as numeric values (`value_as_number`), strings (`value_as_string`) or concept IDs (`value_as_concept_id`).  Measurements may also be filtered by the unit of measure.

| Function | Description |
|---|---|
| `valueAsNumber(op)` | Filter by `value_as_number` |
| `valueAsConcept(ids, connection, vocabularyDatabaseSchema)` | Filter by `value_as_concept_id` |
| `valueAsConceptSet(conceptSet)` | Filter by `value_as_concept_id` using a `ConceptSet` |
| `valueAsString(text, op)` | Filter `observation.value_as_string` |
| `measurementUnit(x)` | Filter by unit concept |

---

###### `valueAsNumber(op)`
`op`: numeric or integer `opAttribute`.
**Returns:** `opAttributeNumeric` with `name = "ValueAsNumber"`.

###### `valueAsConcept(ids, connection, vocabularyDatabaseSchema)`
Same DB-required signature as Type / Status attributes above.
**Returns:** `conceptAttribute` with `name = "ValueAsConcept"`.

###### `valueAsConceptSet(conceptSet)`
`conceptSet`: `ConceptSet` object.
**Returns:** `conceptSetAttribute` with `name = "ValueAsConcept"`. Serializes as an array of Concept objects in JSON (not a `CodesetId`). No DB required.

###### `valueAsString(text, op = "contains")`

| Param | Type | Default | Notes |
|---|---|---|---|
| `text` | `character` | — | String to match against `value_as_string` |
| `op` | `character` | `"contains"` | One of `"contains"` (LIKE `%text%`), `"starts"`, `"ends"`, `"equals"` |

**Returns:** `valueAsStringAttribute` S4 object. Use with `observation()`.
**Validation:** `match.arg(op, c("contains", "starts", "ends", "equals"))`.

###### `measurementUnit(x)`

| `x` type | Behavior |
|---|---|
| `ConceptSet` | The only supported input. Extracts concepts from the concept set as-is — only `concept_id` is populated unless the `ConceptSet` was already enriched (e.g. via `getConceptSetDetails()`); name/domain/vocabulary are blank otherwise. No DB required. |
| anything else (raw concept id, unit string, etc.) | Errors. `x` must be a `ConceptSet` — no other input type is supported, deliberately (fewer ways to do the same thing). |

**Returns:** `conceptAttribute` with `name = "Unit"`. Use with `measurement()`.

### Special Case: Demographics

To apply demographic criteria (age or gender) as a requirement for cohort inclusion in `attrition`, place an `age()` call (see above) and/or one of the following gender function calls within a Group:

| Function | Description |
|---|---|
| `male()` | Restrict to male patients (concept_id 8507) |
| `female()` | Restrict to female patients (concept_id 8532) |
| `genderConcepts(...)` | Restrict by one or more gender concept IDs |

#### `male()` / `female()`
No parameters. **Returns:** `conceptAttribute` with `name = "Gender"`.

#### `genderConcepts(...)`
`...`: integer concept IDs (coerced with `as.integer()`).
**Returns:** `conceptAttribute` with `name = "Gender"`.

```r
# include male patients >=18 years old at index
attrition(
    withAll(male(),
            age(gte(18))
    ),
    ...
)
```

*Total: 88 exported symbols across 16 functional groups.*