# Capr <a href="https://ohdsi.github.io/Capr/"><img src="man/figures/logo.png" align="right" height="90"/></a>

<!-- badges: start -->

[![codecov.io](https://codecov.io/github/OHDSI/Capr/coverage.svg?branch=main)](https://app.codecov.io/gh/OHDSI/Capr?branch=main) [![Build Status](https://github.com/OHDSI/Capr/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/Capr/actions?query=workflow%3AR-CMD-check)

<!-- badges: end -->

Capr is part of [HADES](https://ohdsi.github.io/Hades/)

# Introduction

The goal of Capr, pronounced 'kay-pr' like the edible flower, is to provide a language for expressing OHDSI cohort definitions in R code. OHDSI defines a cohort as "a set of persons who satisfy one or more inclusion criteria for a duration of time" and provides a standardized approach for defining them (Circe). Capr exposes that standardized approach through a programmatic interface in R, making it easier to build, version, and reuse cohort definitions — particularly when creating a large number of similar cohorts. Capr code is designed to be human-readable while remaining directly executable against an OMOP Common Data Model.

Learn more about the OHDSI approach to cohort building in the [cohorts chapter of the Book of OHDSI.](https://ohdsi.github.io/TheBookOfOhdsi/Cohorts.html)

# Installation


Users can install the current development version of Capr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/Capr")
```


# User Documentation

Documentation can be found on the [package website](https://ohdsi.github.io/Capr/).

**Start here** based on your goal:

| Goal | Vignette |
|---|---|
| Build your first cohort | [Using Capr](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/Using-Capr.pdf) |
| Understand queries, criteria, and groups | [Capr components](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/capr_objects.pdf) |
| Work with concept sets | [Working with Concept Sets](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/Capr-conceptSets.pdf) |
| Convert Atlas JSON to Capr code | [Decompiling Atlas JSON](https://ohdsi.github.io/Capr/articles/jsonToCapr.html) |
| Build many cohorts from a template | [Capr for Templating](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/capr_templates.pdf) |
| See clinical cohort examples | [Cohort Definition Examples](https://raw.githubusercontent.com/OHDSI/Capr/main/extras/pdf_vignette/Examples.pdf) |

PDF versions of the documentation are also available on the [package website](https://ohdsi.github.io/Capr/).

# Using Capr with LLM Coding Agents

Capr ships with a self-contained skill bundle for LLM coding agents (Claude Code, GitHub
Copilot, Cursor, etc.) that generates validated cohort definitions from natural-language
descriptions — no MCP servers, no network infrastructure, no database connection. The bundle
is installed with the package, so it always matches the Capr version you have installed:

- `SKILL.md` — the agent workflow: confirm the design questions, generate a cohort-template
  function, execute to validate, deliver.
- `CAPR_REFERENCE.md` — the compact API reference the skill depends on (signatures, valid
  values, worked examples).
- `validate.R` — standalone checker that executes a generated script and confirms CirceR can
  compile its JSON to SQL.
- `README.md` — per-agent setup instructions and design notes.

**Requires a reasoning-capable, frontier-tier model.** In tools with automatic model selection
(e.g., GitHub Copilot's "Auto"), explicitly pick a frontier model — Claude Sonnet/Opus-class,
GPT-5-class, Gemini Pro-class or better — before invoking the skill. Lightweight "mini" /
"flash" / non-reasoning tiers may produce cohort definitions that pass validation but are
clinically wrong, and this is not fixable by refining the skill instructions.

There are two ways to wire it up:

**Copy the bundle into your project** (recommended — works with every agent, including ones
that can only read files inside the workspace):

``` r
file.copy(system.file("llm", package = "Capr"), "docs/", recursive = TRUE)
```

Then follow the per-agent setup instructions in the copied `docs/llm/README.md`. Re-run the
copy when you upgrade Capr — `CAPR_REFERENCE.md` states the Capr version it was verified
against, so a stale copy is detectable.

**Or point the agent at the installed copy** (for agents that can run commands and read files
outside the workspace) — add to your project's agent instruction file
(`CLAUDE.md`, `AGENTS.md`, or equivalent):

``` markdown
## Capr cohort definitions
When asked to build an OHDSI cohort definition, follow the workflow in SKILL.md and use only
the API documented in CAPR_REFERENCE.md, both found under:
`Rscript -e 'cat(system.file("llm", package = "Capr"))'`
```

# Support

-   Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
-   We use the <a href="https://github.com/OHDSI/Capr/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

# Contributing

Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

# License

Capr is licensed under Apache License 2.0

# Development

Capr is developed and maintained as part of the [HADES](https://ohdsi.github.io/Hades/) ecosystem.

# Acknowledgements

-   This package is maintained by Martin Lavallee and Adam Black
-   Guidance and support for the original development of Capr came from Lee Evans and LTS Computing LLC
