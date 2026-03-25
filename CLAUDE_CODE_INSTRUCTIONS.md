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

## Worked Examples
The 5–10 examples from Step 3, each with intent + code.

## Anti-Patterns & Common Mistakes
The "DO NOT / DO" list from Step 4.

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