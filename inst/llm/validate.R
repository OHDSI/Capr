# validate.R — validate a generated Capr cohort script without a database.
#
# Usage:
#   Rscript validate.R <generated_script.R> [cohort1.json cohort2.json ...]
#
# Stage 1: executes the script in a clean environment. It must run end-to-end
#          (including any placeholder example block) with no database connection.
# Stage 2: for each cohort JSON — the paths given on the command line, or, if none
#          are given, every .json file the script wrote next to itself — checks
#          that CirceR parses it and generates SQL from it.
#
# Exit status 0 = all checks passed; 1 = failure (error printed for the agent to fix).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  message("Usage: Rscript validate.R <generated_script.R> [cohort1.json ...]")
  quit(status = 1)
}
scriptPath <- args[1]
jsonPaths <- args[-1]

if (!file.exists(scriptPath)) {
  message("FAIL: script not found: ", scriptPath)
  quit(status = 1)
}

suppressPackageStartupMessages(library(Capr))

# --- Stage 1: execute the script -------------------------------------------
started <- Sys.time()
env <- new.env(parent = globalenv())
stage1 <- tryCatch(
  {
    # chdir so relative writeCohort() paths land next to the script
    source(scriptPath, local = env, chdir = TRUE)
    TRUE
  },
  error = function(e) {
    message("FAIL (execution): ", conditionMessage(e))
    FALSE
  }
)
if (!stage1) quit(status = 1)
message("OK  (execution): ", scriptPath, " ran without error")

# --- Stage 1b: catch accidental placeholder id collisions -------------------
# cs() derives its auto id from the concept expression alone, ignoring `name`, so two concept
# sets meant to be different (e.g. two placeholders) that end up with the same id collapse into
# a single named entry in the compiled JSON. Scan every ConceptSet object left in the script's
# environment and fail if any two share an id but have different names.
conceptSets <- Filter(function(x) methods::is(x, "ConceptSet"), as.list(env))
if (length(conceptSets) > 1) {
  ids   <- vapply(conceptSets, function(x) as.character(x@id), character(1))
  nms   <- vapply(conceptSets, function(x) x@Name, character(1))
  collided <- FALSE
  for (d in unique(ids[duplicated(ids)])) {
    nmsForId <- unique(nms[ids == d])
    if (length(nmsForId) > 1) {
      collided <- TRUE
      message("FAIL (concept set collision): ", paste(nmsForId, collapse = " / "),
              " share the same underlying concept set id and will collapse into a single ",
              "named entry in the compiled JSON. Give each concept set a distinct placeholder id.")
    }
  }
  if (collided) quit(status = 1)
}
message("OK  (concept sets): no placeholder id collisions among ", length(conceptSets),
        " concept set(s)")

# --- Stage 2: Circe accepts the emitted JSON --------------------------------
if (length(jsonPaths) == 0) {
  scriptDir <- dirname(normalizePath(scriptPath))
  candidates <- list.files(scriptDir, pattern = "\\.json$", full.names = TRUE)
  jsonPaths <- candidates[file.mtime(candidates) >= started]
  if (length(jsonPaths) == 0) {
    message("WARN: no cohort JSON written by the script and none given on the command line; ",
            "skipping the Circe check")
    quit(status = 0)
  }
}

failed <- FALSE
for (jsonPath in jsonPaths) {
  if (!file.exists(jsonPath)) {
    message("FAIL (circe): file not found: ", jsonPath)
    failed <- TRUE
    next
  }
  res <- tryCatch(
    {
      json <- paste(readLines(jsonPath, warn = FALSE), collapse = "\n")
      expr <- CirceR::cohortExpressionFromJson(json)
      sql <- CirceR::buildCohortQuery(expr, CirceR::createGenerateOptions(generateStats = FALSE))
      stopifnot(is.character(sql), nchar(sql) > 0)
      TRUE
    },
    error = function(e) {
      message("FAIL (circe): ", jsonPath, ": ", conditionMessage(e))
      FALSE
    }
  )
  if (res) message("OK  (circe): ", jsonPath, " compiles to SQL") else failed <- TRUE
}

quit(status = if (failed) 1 else 0)
