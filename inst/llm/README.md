# Capr LLM Skill Bundle

Generate OHDSI cohort definitions from natural-language descriptions using any LLM coding agent
(Claude Code, GitHub Copilot, Cursor, ...) — no MCP servers, no network infrastructure, no
database connection. Just R, the Capr package, and the two markdown files in this directory
loaded into your agent's context.

## Files

| File | What it is |
|---|---|
| `SKILL.md` | The agent workflow: clarify → generate a cohort-template function → execute to validate → deliver. Load this into your agent. |
| `CAPR_REFERENCE.md` | The Capr API reference the skill depends on — every signature, valid enum value, and worked example. Must be readable by the agent (the skill refers to it). |
| `validate.R` | Standalone checker: executes a generated script and confirms CirceR can compile its JSON to SQL. Useful for agents and CI. |

## Prerequisites

- R with the Capr package installed (`remotes::install_github("OHDSI/Capr")`); CirceR comes with
  it.
- The agent must be able to run `Rscript` (or you run the validation steps yourself).
- **A reasoning-capable, frontier-tier model.** In tools with automatic model selection (e.g.,
  GitHub Copilot's "Auto"), explicitly pick a frontier model — Claude Sonnet/Opus-class,
  GPT-5-class, Gemini Pro-class or better — before invoking the skill. This is a setup choice
  you make before the agent starts, not something the agent can fix for itself: model selection
  happens above the agent loop, so text inside `SKILL.md` can't influence it. Lightweight
  "mini" / "flash" / non-reasoning tiers may produce cohort definitions that pass `validate.R` but are clinically wrong — validation checks API usage, not clinical intent.

If Capr is installed, this bundle is on disk at `system.file("llm", package = "Capr")`. 
You have 2 options for accessing it with your agent:

- Point the agent at it directly with `Rscript -e 'cat(system.file("llm", package = "Capr"))'`
  - This will only work if your agent can read files outside the repository it's working in
- Copy it into your project manually (`file.copy(system.file("llm", package = "Capr"), "docs/", recursive = TRUE)`)

## Setup per agent (manual file copy)

The pattern is always the same: make `SKILL.md` part of the agent's instructions, and keep
`CAPR_REFERENCE.md` somewhere the agent can read it (same directory is simplest).

- **Claude Code** — copy both markdown files into a skill directory:
  `.claude/skills/capr-cohort-generation/SKILL.md` (plus `CAPR_REFERENCE.md` alongside it).
  The skill activates automatically when you ask for a cohort definition.
- **GitHub Copilot** — put both files in your repo (e.g. `docs/llm/`) and add a line to
  `.github/copilot-instructions.md`: *"When asked to build an OHDSI cohort definition, follow
  `docs/llm/SKILL.md` and use only the API documented in `docs/llm/CAPR_REFERENCE.md`."*
  Alternatively, attach both files to the chat context manually.
- **Cursor / Windsurf / others** — add the same pointer to the agent's rules mechanism
  (`.cursor/rules/`, etc.), or paste `SKILL.md` followed by `CAPR_REFERENCE.md` directly into the
  conversation.

## Usage

1. Describe the cohort in plain English. The agent always responds first with one batched
   message confirming the full design checklist (index event, domains, entry limit, washout,
   windows, exit strategy) with proposed answers you can approve in one word — even when your
   description seems complete. That's by design; the costliest mistakes (wrong domain, wrong
   index event) produce silently wrong cohorts, not errors.
2. The agent delivers one R file: a `Scope check` header recording every design decision as
   confirmed-by-you or assumed, a cohort-template function (concept sets as parameters), plus
   an example invocation that writes the cohort JSON.
3. Check it yourself any time:

   ```sh
   Rscript validate.R my_cohort_script.R
   ```

   Exit 0 = the script runs without a database and its JSON compiles to SQL via CirceR. The JSON
   imports directly into Atlas.

## Design notes (why it works this way)

- **Concept ids are inputs, not model knowledge.** Agents are forbidden from writing concept ids
  from memory; unknown concept sets become executable placeholders with distinct, incrementing
  ids (`cs(0L, name = "... [PLACEHOLDER]")`, `cs(1L, ...)`, ...) that you replace with relevant
  Capr concept set expressions. Ids must differ across concept sets — Capr collapses concept sets 
  with identical expressions into one, regardless of name, so a repeated placeholder id silently 
  drops all but one name from the compiled JSON.
- **The function form is the contract.** Cohort logic is separated from concept-set acquisition,
  which also makes batch generation trivial: `lapply(conceptSetList, createMyCohort)`.
- **Everything runs without a database.** Capr builds and serializes cohort JSON in memory; a
  CDM connection is only ever an optional nicety to fill in display names for Atlas.
- **Phenotype archetypes reduce boilerplate.** The package exports `chronicCohort()`,
  `incidentCohort()`, `acuteCohort()`, `newUserCohort()`, `allDrugCohort()`,
  `measurementCohort()`, `procedureCohort()`, and `observationCohort()` — functions that encode
  common clinical patterns (prevalent conditions, first-ever diagnoses, drug new-user, lab
  thresholds, etc.). When a description matches an archetype, the deliverable is a one-line
  wrapper function. See `CAPR_REFERENCE.md` § *Phenotype Archetypes* for the full catalog and
  sensitivity-tuning guidance.
- **The delivery step is overridable by host frameworks.** The generation contract (clarify,
  function form, Scope check, validate) always applies, but a host project's own instructions
  may redirect where the code lands and how it is serialized — e.g. the
  [picard](https://github.com/OHDSI/picard) framework appends definitions to its builder
  scripts and registers them via its manifest API instead of `writeCohort()`. See *Delivery
  Integration* in `SKILL.md`.
