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

- **Function signatures with all parameters, types, and defaults** — every public function the LLM might need (`cohort()`, `entry()`, `exit()`, `attrition()`, `withAll()`, `withAny()`, `withAbsence()`, the temporal operators like `duringInterval()`, `before()`, `after()`, `between()`, era functions, demographic criteria, etc.)
- **Enum-like constrained values** — e.g., valid options for `occurrenceType`, `domain`, `ageComparison` operators, etc. These are prime hallucination targets.
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
- `concept_sets.R` — a template file where users define their concept sets
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

## Key Design Decisions & Rationale

**Why a reference doc instead of RAG or fine-tuning?** Portability. RAG needs a vector store and retrieval infra. Fine-tuning locks you to one model. A reference doc works with any LLM that supports long context, which is all of them now. It also makes hallucination debugging transparent — you can see exactly what the LLM had available.

**Why exclude concept set building?** Right call for phase 1. Concept set construction requires vocabulary search against a database, which introduces network/infra dependencies. By treating concept sets as pre-built inputs, the entire workflow stays local.

**Why the validation loop matters so much:** Capr has a relatively small API surface, but the combinatorial space of valid cohort definitions is large. The LLM will occasionally invent plausible-but-wrong argument names or nesting structures. Catching this mechanically (via R execution) is far more reliable than trying to prevent it purely through prompting.