# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Package Does

**Capr** (Cohort definition Application Programming in R) is an OHDSI HADES R package for programmatically building OHDSI cohort definitions. It provides a DSL that lets users construct cohort definitions as executable R code rather than using the Atlas web UI. The output is serialized to OHDSI-compatible JSON via CirceR.

## Commands

```r
# Install dependencies (in R)
devtools::install_deps()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-cohort.R")

# Build package documentation
devtools::document()

# Run R CMD check (equivalent to CI)
devtools::check()

# Check code coverage
covr::package_coverage()
```

CI runs `R CMD check --no-manual --as-cran` on Windows, macOS, and Ubuntu via `.github/workflows/R_CMD_check_Hades.yaml`. Warnings are treated as errors.

## Architecture

Capr uses the **S4 object system** with a functional builder API. The general pattern is: construct S4 objects via constructor functions → compose them into a `Cohort` → serialize to JSON.

### Object Hierarchy

```
Cohort
├── CohortEntry       (entry event: which event + observation window)
├── CohortAttrition   (inclusion/exclusion groups)
│   └── Group[]       (withAll / withAny / withAtLeast / withAtMost)
│       └── Criteria[]
│           ├── Query (domain + ConceptSet + attribute filters)
│           └── Occurrence (exactly / atLeast / atMost)
├── CohortExit        (exit strategy: fixed, event-based, drug era, etc.)
└── CohortEra         (era collapse gap + censor window)
```

### Key Source Files

- `R/conceptSet.R` — `Concept`, `ConceptSetItem`, `ConceptSet` classes; `cs()` constructor
- `R/query.R` — `Query` S4 class; domain-specific constructors (`conditionOccurrence()`, `drugExposure()`, `measurement()`, `visit()`, `procedure()`, `observation()`, `death()`, etc.)
- `R/window.R` — temporal windows (`ObservationWindow`, `EventAperture`); `duringInterval()`, `before()`, `after()`
- `R/criteria.R` — `Criteria` S4 class combining a `Query` with occurrence rules and temporal windows
- `R/entry.R` — `CohortEntry`; `entry()` constructor
- `R/attrition.R` — `CohortAttrition`; grouping functions `withAll()`, `withAny()`, `withAtLeast()`, `withAtMost()`
- `R/exit.R` — `CohortExit`; exit strategy constructors
- `R/era.R` — `CohortEra`; `era()` constructor
- `R/cohort.R` — top-level `Cohort` class; `cohort()`, `toCohortJson()`, `toCirce()`, `as.json()`, `writeCohort()`
- `R/attributes-op.R` — value/comparison attribute functions (`valueAsNumber()`, `valueAsConcept()`, `age()`, etc.)
- `R/attributes-logic.R` — logical attribute modifiers
- `R/attributes-nested.R` — nested group operations
- `R/collectCodesetId.R` — maps UUIDs to integer codeset IDs before JSON serialization

### Serialization Flow

`cohort()` → `toCirce()` → `as.list()` (CirceR format) → `jsonlite::toJSON()` → OHDSI JSON

The `collectCodesetId()` step replaces GUID references with sequential integers, which is required before the CirceR conversion.

## Ongoing Work: LLM-Assisted Cohort Generation

There is an active project to build a self-contained LLM skill for generating Capr code from natural language. The shippable bundle lives in `inst/llm/` (`SKILL.md`, `CAPR_REFERENCE.md`, `validate.R`, `README.md`). See:

- `CLAUDE_CODE_INSTRUCTIONS.md` — step-by-step instructions for building `CAPR_REFERENCE.md` (a compact API reference for LLM in-context use, ~10–15K tokens)
- `CLAUDE_PLAN.md` — overall strategy: reference doc → skill prompt → validation loop → portability packaging

The validation approach is: generate Capr R code → execute in R without a DB connection (Capr builds JSON in-memory) → feed errors back to LLM → fix. The key design constraint is **no MCP servers or network infra** — everything must work locally with just R and the LLM agent.
