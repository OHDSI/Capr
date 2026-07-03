# Capr Anti-Patterns & Common Mistakes

Intermediate artifact toward `CAPR_REFERENCE.md`. See `CLAUDE_CODE_INSTRUCTIONS.md` (Step 4) for
the build plan.

This file is deliberately short. Most plausible LLM mistakes — hallucinated/deprecated function
names, missing required arguments, wrong argument types — throw an immediate, clear R error, and
the generate → execute → feed error back → fix validation loop (see `CLAUDE_PLAN.md`) resolves
those in one iteration on its own, especially with `CAPR_API_INVENTORY.md` in context to point at
the correct form. This file only covers the cases that loop *can't* catch: patterns that
`compile()` accepts without error but that are either silently wrong or silently redundant, so
nothing short of writing them down here would ever surface them. Every item was verified against
`R/` source and, where noted, against actual `compile()` output / an Atlas import.

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
`withAtMost()`), even for a single criterion. `as.list()` on a bare `Criteria` produces the shape
of a `CriteriaList` *item* (`{Criteria, StartWindow, Occurrence}`), not the
`{Type, CriteriaList, DemographicCriteriaList, Groups}` shape Atlas expects for
`InclusionRules[].expression`. **Capr's `compile()` will not error** — the JSON is well-formed,
just structurally wrong — so this bug is invisible until you actually import into Atlas.
Verified: `R/criteria.R` (`as.list,Criteria` vs. `as.list,Group`), confirmed by Atlas import
(the malformed rule simply didn't appear in the UI).

### 2. `entry()`'s `additionalCriteria`/`qualifiedLimit` instead of `attrition()`

```r
# DO NOT (works, but redundant and less familiar)
entry(
  conditionOccurrence(cs_stroke),
  primaryCriteriaLimit = "All",
  additionalCriteria = withAny(atLeast(1, visit(cs_ipVisit), ...)),
  qualifiedLimit = "All"
)

# DO
entry(conditionOccurrence(cs_stroke), primaryCriteriaLimit = "All")
# ...then filter/select in attrition() with expressionLimit, see CAPR_EXAMPLES.md Example 7
```
Verified byte-for-byte equivalent to the `attrition()`/`expressionLimit` approach — same
generated SQL (`CirceR::buildCohortQuery()`) and identical cohort membership on real Eunomia
data (479/479 matching rows), for both "strict" (`primaryCriteriaLimit = "First"`) and
"permissive" (`primaryCriteriaLimit = "All"` + `qualifiedLimit = "First"`) configurations. Don't
reach for `additionalCriteria` unless a concrete case is found where `attrition` can't do it —
none has been found yet. Full writeup: `CAPR_EXAMPLES.md`, Example 7.

### 3. Expecting `qualifiedLimit` to do something on its own

`qualifiedLimit` only has an effect on the generated SQL when `additionalCriteria` is also set —
without it, the "qualified events" ordinal column is computed but never filtered on (verified by
inspecting `CirceR::buildCohortQuery()` output). If you're not using `additionalCriteria`, setting
`qualifiedLimit` to anything other than `primaryCriteriaLimit`'s value is a no-op; prefer leaving
it unset (it defaults to `primaryCriteriaLimit`) unless you're matching an existing Atlas JSON
byte-for-byte (see `CAPR_EXAMPLES.md`, Real-World Example 2, "yearly denominator").

### 4. Passing a raw concept ID or unit string to `measurementUnit()`

```r
# DO NOT
measurementUnit(8713L)
measurementUnit("%")

# DO
measurementUnit(cs(8713L, name = "gram per deciliter"))
```
`measurementUnit(x)` only accepts a `ConceptSet` (built with `cs()`) — deliberately, so there's a
single way to pass a unit rather than several (a raw integer concept ID, a hardcoded unit string
shorthand, and a `ConceptSet` all doing roughly the same thing). It errors on anything else
(`R/attributes-concept.R`). This is worth flagging even though the generate → execute → fix loop
catches it immediately: older Capr versions accepted a bare integer concept ID or a hardcoded unit
string like `"%"`/`"mmol/mol"` directly (see `git log -p R/attributes-concept.R`), and that older
signature is exactly the kind of thing an LLM could confidently reproduce from training data,
wasting a fix iteration. Always build the unit as a `ConceptSet` via `cs()` first, same as every
other concept-set-typed attribute (`conditionSourceConcept()`, `valueAsConceptSet()`, etc.) — note
this only populates `concept_id`, leaving `concept_name`/`domain_id`/`vocabulary_id`/etc. blank in
the JSON (Atlas will show the unit with no display name, though the SQL is still correct); use
`getConceptSetDetails()` beforehand (see "Recommend hydrating concept sets" in
`CAPR_REFERENCE.md`) if the user has a database connection and wants Atlas to display the real
concept name.
