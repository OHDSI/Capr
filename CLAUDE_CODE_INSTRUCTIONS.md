# Instructions: Build a Capr API Reference Document for LLM-Assisted Cohort Definition

## Goal

Create a concise, precise reference document (`CAPR_REFERENCE.md`) that an LLM can use as in-context documentation to generate correct OHDSI cohort definitions using the Capr R package (i.e., the package in this repo). This reference will be loaded into an LLM's context window as a "skill" or system prompt, so **accuracy and compactness matter more than completeness**.

## Why This Matters

LLMs hallucinate function names, argument structures, and API patterns. The reference document is the primary defense against this. Every function signature, every valid enum value, every example must be verified against the actual source code.

## Context

- **Capr** is an R package for programmatically building OHDSI cohort definitions (which are typically created in Atlas).
- Concept sets are **out of scope** — assume they are pre-built and available as R objects. The focus is on cohort logic: entry events, inclusion criteria, exit strategies, censoring, temporal relationships, etc.
- The reference document should be **~10–15K tokens** (roughly 7,000–10,000 words). Larger is OK if needed for accuracy, but trim anything that doesn't help an LLM write correct code.

## Step-by-Step Process

Work through these steps **in order**. Complete each step before moving to the next.

### Step 1: Inventory the Public API

Read the `NAMESPACE` file to get the full list of exported functions and classes. Organize them into functional groups (e.g., cohort construction, criteria, temporal logic, demographic filters, exit strategies, etc.). Output a categorized list with brief one-line descriptions.

### Step 2: Document Function Signatures

For each exported function, read the R source files in `R/` and extract:
- Function name
- All parameters with their types and default values
- What the function returns
- Any important constraints or validation rules (check the function body for `stop()`, `assert`, `match.arg()`, etc.)

Pay special attention to:
- **Enum-like arguments** — parameters that only accept specific string values (via `match.arg` or similar). List every valid value explicitly.
- **S4/R5 classes** — if Capr uses formal classes, document their constructors and key slots/fields.
- **Pipe-friendly patterns** — if functions are designed to be chained, note that.

### Step 3: Extract Worked Examples

Read the `vignettes/` directory for tutorials and worked examples. Also check `tests/` for additional usage patterns. For each example:
- State the plain-English intent (e.g., "First exposure to drug X with 365 days of prior observation")
- Show the exact Capr R code
- Annotate which functions/patterns are being demonstrated

Aim for **5–10 diverse examples** covering:
1. Simple entry event (single concept set, single domain)
2. Entry + required inclusion criteria (e.g., prior diagnosis)
3. Entry + absence criteria (e.g., no prior exposure to drug Y)
4. Temporal relationships between events (before, after, during, within N days)
5. Demographic criteria (age, gender)
6. Exit strategy / end-of-observation logic
7. Nested/complex criteria (combining multiple conditions)
8. Any other patterns that appear frequently in vignettes or tests

### Step 4: Identify Anti-Patterns

Based on what you've learned about the API, list **common mistakes an LLM is likely to make**. Think about:
- Functions or arguments from other OHDSI packages (e.g., CohortGenerator, CirceR) that DON'T exist in Capr
- Argument names that are close to but different from the real ones
- Patterns that look plausible but produce incorrect cohort logic
- Common confusions between similar functions

Format these as explicit "DO NOT / DO" pairs.

### Step 5: Assemble the Reference Document

Combine everything into a single `CAPR_REFERENCE.md` file with this structure:

```
# Capr R Package — LLM Reference

## Overview
Brief description of what Capr does and how cohort definitions are structured.

## Prerequisites
What must be loaded/available (library calls, pre-built concept sets, etc.)

## API Reference
Grouped by category. Each function gets: signature, parameter table, return type, and a minimal usage snippet.
Scope this to functions used to *build* a cohort definition (entry, attrition, exit, era, queries,
criteria, groups, attributes). Do not include concept-set construction or JSON serialization functions
here — those get one line each in "Output," below.

## Worked Examples
The 5–10 examples from Step 3, each with intent + code.

## Anti-Patterns & Common Mistakes
The "DO NOT / DO" list from Step 4.

## Output
Once the `Cohort` object is fully built (via `cohort()`), tell the LLM exactly what to do with it —
this is the last step of every generation, so keep it to a few lines, not full API docs:
- `toCohortJson(cohortObject)` → JSON string
- `writeCohort(cohortObject, path)` → writes JSON straight to a `.json` file
Don't document `toCirce()` or `as.json()` here — they're redundant with `toCohortJson()`/`writeCohort()` for this workflow.

## Quick Reference Card
A compact cheat-sheet of the most-used functions and patterns.
```

### Step 6: Validate

After writing the reference document, **spot-check at least 5 function signatures** by re-reading the source code and confirming the reference is accurate. Fix any discrepancies.

## Important Guidelines

- **Only document what's in the source code.** Do not infer, guess, or fill in gaps from general OHDSI knowledge. If something is unclear in the source, note the uncertainty.
- **Prefer concrete code over prose.** Show the exact R code; don't just describe what a function does in words.
- **Be explicit about types.** If a parameter expects a `ConceptSet` object vs. a character string vs. an integer, say so.
- **Include the version/commit** of the Capr repo you're working from (check the DESCRIPTION file).
- **Concept sets are out of scope** — but DO document how concept sets are *referenced* (i.e., how they're passed to functions, what type they need to be).

## Deliverable

A single file: `CAPR_REFERENCE.md` — ready to be used as a skill/system-prompt for any LLM coding agent.

---

# Notes: After Testing — Building the Skill Package (Phases 4–5)

*Status as of 2026-07-03: Steps 1–6 above are complete. The reference lives at
`inst/llm/CAPR_REFERENCE.md` (ships with the package; `system.file("llm", "CAPR_REFERENCE.md",
package = "Capr")` resolves it). The intermediate artifacts (`extras/CAPR_API_INVENTORY.md`,
`CAPR_EXAMPLES.md`, `CAPR_ANTIPATTERNS.md`) were consolidated into the reference and deleted —
the reference is now the single source of truth and is edited directly (see Maintenance policy
below; their non-re-derivable verification evidence is preserved there too). The README has a
"Using Capr with LLM Coding Agents" section with the `system.file()` one-liner and a
CLAUDE.md/AGENTS.md pointer snippet. The reference currently includes an "Agent Workflow"
section (clarifying questions + Capr/Circe fit flags) as a temporary home for Layer-1 behavioral
guidance.*

## During Phase 4 testing (10–15 real-world cohort definitions)

Iterate on `inst/llm/CAPR_REFERENCE.md` directly:

- **New failure modes → the right section of the reference.** Hallucinated names / wrong
  arguments that errored and self-corrected via the validation loop need no documentation. Only
  add to the "Anti-Patterns & Common Mistakes" section what `toCohortJson()` accepts silently — that
  section's admission rule. Recurring *correct* patterns the model struggled to find go in
  "Worked Examples" as new entries (validate each by executing it against the current package
  source before adding; keep the Intent / code / Demonstrates structure).
- **Tune the Agent Workflow section** in the reference against observed behavior: cases where
  the agent guessed when it should have asked (add to the clarifying-questions checklist), or
  forced a fit when it should have flagged (add to the fit-flag signals). The "more than about
  two levels of nesting" threshold is a placeholder judgment — replace it with whatever the
  testing shows.
- **Verify the Circe-limitation claims.** Unlike the API sections, the fit-flag bullets (no
  cohort-to-cohort references, no cross-event arithmetic, ordinal logic limits) were written
  from general OHDSI knowledge, not verified against source. Confirm or correct them during
  testing.

## Phase 2/5: the skill package

Once testing stabilizes the content, package the three-layer skill:

1. **Create `inst/skills/capr-cohorts/SKILL.md`** (Agent Skills format: YAML frontmatter with
   `name` and a `description` that triggers on cohort-definition tasks). SKILL.md is Layer 1 and
   should hold: role framing, the code-generation conventions, the **Agent Workflow section
   lifted out of `CAPR_REFERENCE.md`** (move it — the reference then reverts to pure API
   content), the validation-loop instruction (execute generated code in R with Capr loaded, no
   DB needed; fix errors and re-run), and the Output instructions (`toCohortJson()`/`writeCohort()`).
   It references `CAPR_REFERENCE.md` as a supporting file to read before writing code.
2. **Layer 2 (concept-set inventory):** superseded — the function-form output (concept sets as
   `ConceptSet` parameters) makes a session inventory optional rather than required; see
   `CLAUDE_PLAN.md`'s Phase 2 status notes.
3. **Exported helper:** add something like `installAgentSkill(path = ".")` to Capr that copies
   the skill directory from `system.file("skills", ...)` into the project's `.claude/skills/`
   and prints the suggested AGENTS.md/CLAUDE.md pointer snippet. Keep the README's
   `system.file()` pointer approach documented as the agent-agnostic fallback.
4. **Check with HADES maintainers** whether the skill packaging belongs in the package `inst/`
   or a companion repo — the reference itself should stay in `inst/llm/` regardless, because
   installing with the package is what pins it to the installed API version.

## Maintenance policy (applies from now on)

- `inst/llm/CAPR_REFERENCE.md` is the **single source of truth** — edit it directly. (The
  `extras/CAPR_*.md` intermediates it was assembled from were folded in and deleted on
  2026-07-03.) Every signature/enum/example claim must stay verified against `R/` source;
  execute new or changed examples before committing. Keep the version/commit stamp in the
  reference header current.
- When the Capr API changes, re-verify the affected signatures in the reference against `R/`
  and update them in place.

## Verification evidence (preserved from the deleted intermediates)

The reference asserts several claims that are **not re-derivable from source** — they were
established empirically. Record of how, so they aren't re-litigated or accidentally weakened:

- **Bare `Criteria` as an attrition rule (Anti-Pattern #1):** `as.list()` on a bare `Criteria`
  produces the shape of a `CriteriaList` *item* (`{Criteria, StartWindow, Occurrence}`), not the
  `{Type, CriteriaList, DemographicCriteriaList, Groups}` shape Atlas expects for
  `InclusionRules[].expression`. Confirmed by importing the compiled JSON into Atlas: the
  malformed rule silently didn't render in the UI.
- **`additionalCriteria` ≡ `attrition` (Anti-Pattern #2):** verified byte-for-byte equivalent —
  same SQL from `CirceR::buildCohortQuery()` and the exact same 479 cohort rows
  (subject/start/end date) on Eunomia (GI bleed + inpatient visit), in both the "strict"
  (`primaryCriteriaLimit = "First"`) and "permissive" (`"All"` + limit `"First"`) configurations.
- **`qualifiedLimit` no-op without `additionalCriteria` (Anti-Pattern #3):** verified by
  inspecting `CirceR::buildCohortQuery()` output — the "qualified events" ordinal column is
  computed but never filtered on. (If omitted, `entry()` internally defaults it to
  `primaryCriteriaLimit`; see `R/cohort.R`.)
- **`observationPeriod()` + `startDate()` → `UserDefinedPeriod`:** the `as.list,Query` method in
  `R/query.R` special-cases `x@domain == "ObservationPeriod"`, converting the
  `OccurrenceStartDate` op to `UserDefinedPeriod` at serialization; confirmed in `toCohortJson()`
  output.
- **`measurementUnit()` legacy signatures:** older Capr versions accepted a raw integer concept
  ID or a unit string (`"%"`, `"mmol/mol"`) — see `git log -p R/attributes-concept.R` — which is
  why the reference explicitly warns against those forms (an LLM could reproduce them from
  training data).