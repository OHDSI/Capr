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

If Capr is installed, this bundle is on disk at `system.file("llm", package = "Capr")`. Copy it
into your project (`file.copy(system.file("llm", package = "Capr"), "docs/", recursive = TRUE)`)
— most agents, GitHub Copilot in particular, can only read files inside the workspace. Agents
that can run commands and read outside the workspace (e.g. Claude Code) can alternatively be
pointed at the installed copy directly; see the package README. Re-copy when you upgrade Capr.

## Setup per agent

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
  ids (`cs(0L, name = "... [PLACEHOLDER]")`, `cs(1L, ...)`, ...) that you replace. Ids must differ
  across concept sets — Capr collapses concept sets with identical expressions into one,
  regardless of name, so a repeated placeholder id silently drops all but one name from the
  compiled JSON. Look ids up in [ATHENA](https://athena.ohdsi.org).
- **The function form is the contract.** Cohort logic is separated from concept-set acquisition,
  which also makes batch generation trivial: `lapply(conceptSetList, createMyCohort)`.
- **Everything runs without a database.** Capr builds and serializes cohort JSON in memory; a
  CDM connection is only ever an optional nicety to fill in display names for Atlas.
