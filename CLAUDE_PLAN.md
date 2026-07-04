# Plan: LLM-Powered OHDSI Cohort Definition Generation with Capr

## Problem

Translating plain-English cohort definitions into correct OHDSI cohort definitions is tedious and error-prone. We want to let users describe a cohort in natural language and have an LLM coding agent generate the corresponding R code using the Capr package (i.e., the package in this repo).

## Requirements

- Anyone with access to an LLM coding agent (Claude Code, Cursor, Copilot, etc.) can use it
- No MCP servers or infrastructure that might be blocked by firewalls — only the LLM agent itself, Capr, and open-source R packages
- Concept set construction is out of scope — assume concept sets are pre-built and available as R objects
- Reducing/eliminating hallucinations is a top priority

## Core Idea: A Custom Skill (Prompt + Reference Documentation)

The most portable, firewall-friendly approach is to package everything the LLM needs as a **self-contained prompt/skill** — a markdown reference document that gets loaded into the LLM's context window before it generates code. No servers, no APIs beyond the LLM itself, no network calls at runtime.

## Components

### 1. Capr API Reference Document (`CAPR_REFERENCE.md`)

This is the single most important piece for reducing hallucinations. The LLM needs a precise, compact reference for the Capr API — not the full package documentation, but a distilled version optimized for code generation.

**What to include:**

- **Function signatures with all parameters, types, and defaults** — every public function the LLM might need (only include functions from `NAMESPACE` used in constructing cohort definitions)
- **Enum-like constrained values** — e.g., valid options for `index`, `primaryCriteriaLimit`, `offsetDays` operators, etc. These are prime hallucination targets.
- **Canonical patterns** — 5–10 worked examples covering the most common cohort shapes (simple entry with drug exposure, entry + inclusion criteria, entry + censoring events, nested temporal logic, correlated criteria). Each example should show the plain-English intent alongside the exact Capr code.
- **Explicit anti-patterns** — common mistakes the LLM might make, stated as "DO NOT do X, DO Y instead."

**How to build it:**

- Start from the Capr package vignettes and function documentation on the GitHub repo
- Supplement with the Capr source code (especially exported function signatures from `NAMESPACE` and the roxygen docs)
- Trim aggressively — the goal is to fit within ~10–15K tokens so it leaves room for conversation context. Every line should earn its place.

**Build instructions are in `CLAUDE_CODE_INSTRUCTIONS.md`.**

### 2. Prompt Structure (the "Skill")

The skill has three layers that get assembled at generation time:

**Layer 1 — System/Reference (static):** The Capr API reference document. Loaded once, always present.

**Layer 2 — Concept Set Inventory (per-session):** The user provides (or the agent reads from a file) the available concept sets and their variable names. Something like:

```
Available concept sets:
- cs_metformin: Metformin and descendants (drug)
- cs_t2dm: Type 2 Diabetes Mellitus (condition)
- cs_hba1c: HbA1c lab measurements (measurement)
```

This grounds the LLM so it references real objects rather than inventing them.

**Layer 3 — User Request (per-query):** The plain-English cohort definition. E.g., *"First exposure to metformin, requiring a prior T2DM diagnosis within 365 days before, with a continuous observation period of at least 180 days prior."*

### 3. Validation Loop

Even with great reference docs, LLMs will occasionally produce invalid code. Build in a **generate → validate → fix** loop:

- **Step A:** LLM generates Capr R code.
- **Step B:** The coding agent executes the code in R (with Capr loaded but *without* a database connection — Capr can build cohort definition JSON objects in-memory). Check for: syntax errors, missing functions, argument mismatches.
- **Step C:** If errors occur, feed the error message back to the LLM with the original reference doc still in context and ask it to fix the code.
- **Step D:** Once the code runs without error, optionally serialize the cohort definition to JSON (`toCirce()` or similar) so the user can inspect or import it into Atlas.

This loop is fully local — just R execution on the agent's machine.

### 4. Packaging for Portability

**Option A — A single markdown skill file.** Works in Claude Code, Cursor, Windsurf, Copilot Workspace, or any agent that supports custom instructions/skills. The user drops the file into their agent's skill/instruction directory, loads their concept sets, and starts prompting.

**Option B — An R project template.** A small repo containing:

- `SKILL.md` — the prompt/reference doc
- `validate.R` — a helper script that sources the generated code, runs it, and reports errors
- `README.md` — instructions for use with various LLM agents

Option B is more structured and easier to share via GitHub.

## Roadmap / Build Order

| Phase | Deliverable | Effort |
|-------|------------|--------|
| **Phase 1** | Capr API reference document — distill function signatures, valid values, and 5+ worked examples | Medium (biggest effort, highest impact) |
| **Phase 2** | Skill prompt template with the 3-layer structure | Small |
| **Phase 3** | Validation script (`validate.R`) that catches runtime errors | Small |
| **Phase 4** | Test on 10–15 real-world cohort definitions of varying complexity, iterate on the reference doc based on failure modes | Medium |
| **Phase 5** | Package as a GitHub repo with README and usage instructions | Small |

## Status & Design Updates (2026-07-03)

**Phase 1 — done.** `inst/llm/CAPR_REFERENCE.md`: table-formatted API core (per-function parameter
tables merged from the retired `extras/CAPR_API_INVENTORY.md`), agent workflow guidance
(clarification checklist, wrong-tool signals, hydration), and 10 worked examples, all verified by
execution.

**Package change that unlocked the no-DB constraint.** The type/status/unit/value-as-concept
attributes (`conditionType()`, `measurementUnit()`, `valueAsConcept()`, etc.) now take integer id
vectors with an **optional** `connection` (names are display-only in Atlas; SQL is identical
without them). This made the "generated code always executes without a database" contract hold
across the entire API. Along the way: fixed silent Circe key-casing bugs (`MeasurementType`,
`ObservationType`, `ProcedureType`, `PeriodType`) where the filter vanished from generated SQL,
added `deathType()`/`deviceType()`/`specimenType()`, and taught `jsonToCapr()` to decompile type
attribute lists.

**Phase 2 — done, with design refinements over the original 3-layer plan.**
`inst/llm/SKILL.md` (process only; defers API facts to the reference):

- **Output contract is a cohort-template function**, always: concept sets are `ConceptSet`
  parameters (type/unit concept ids are integer-vector parameters), design decisions are
  hardcoded in the body and recorded in the `Scope check` header block (inline `# ASSUMPTION:`
  comments only for micro-decisions), and an example-usage block invokes the
  function with executable placeholders (`cs(0L, name = "... [PLACEHOLDER]")`) and
  `writeCohort()`. This **supersedes Layer 2** (the concept-set inventory): the function
  signature is the grounding mechanism, so a session inventory is optional rather than required.
  It also directly serves the batch use case (many identically structured cohorts via `lapply`).
- **Clarification gate**: the checklist lives in `SKILL.md` Step 1 (moved out of the reference so
  the always-ask behavior is self-contained), and asking is unconditional — every request gets
  one batched message with a proposed answer per question, hard stop before code, even when the
  prompt seems fully specified. Confirmation is defined mechanically: `confirmed by user` means
  answered in the reply to this request's Step 1 message.
- **Anti-hallucination**: hard rule that concept ids never come from the model's memory
  (including type/unit/status ids); no curated id tables anywhere in the docs (vocabularies
  drift); candidate ids allowed only as `verify in ATHENA` comments.

**Phase 3 — done.** Validation is two-stage: `Rscript` the generated file end-to-end (placeholders
keep it executable, no DB), then confirm CirceR can generate SQL from the emitted JSON.
`inst/llm/validate.R` wraps both stages for agents and CI.

**Package hardening for the validation loop.** R silently swallows misspelled *named* arguments
into `...` (e.g. `entry(q, primaryCriterialimit = "All")` built a cohort with the default
`"First"` — semantic drift with no error). `entry()`, `attrition()`, `censoringEvents()`, and
`query()` now validate everything passed via `...` and error with a
did-you-misspell-a-parameter hint, so hallucinated/typo'd argument names fail loudly at
execution time instead of producing wrong cohorts.

**Phase 5 — done (as Option B, in-package).** The bundle ships in `inst/llm/`: `SKILL.md`,
`CAPR_REFERENCE.md`, `validate.R`, `README.md` (per-agent setup). Installed with the package, so
it's also reachable via `system.file("llm", package = "Capr")`. (A `concept_sets.R` user template
was tried and removed — concept-set tooling is being revisited separately.)

**Phase 4 — next.** Testing against real-world cohort descriptions in GH Copilot / Claude Code /
Cursor; iterate on the reference doc and skill based on failure modes.

## Key Design Decisions & Rationale

**Why a reference doc instead of RAG or fine-tuning?** Portability. RAG needs a vector store and retrieval infra. Fine-tuning locks you to one model. A reference doc works with any LLM that supports long context, which is all of them now. It also makes hallucination debugging transparent — you can see exactly what the LLM had available.

**Why exclude concept set building?** Right call for phase 1. Concept set construction requires vocabulary search against a database, which introduces network/infra dependencies. By treating concept sets as pre-built inputs, the entire workflow stays local.

**Why the validation loop matters so much:** Capr has a relatively small API surface, but the combinatorial space of valid cohort definitions is large. The LLM will occasionally invent plausible-but-wrong argument names or nesting structures. Catching this mechanically (via R execution) is far more reliable than trying to prevent it purely through prompting.