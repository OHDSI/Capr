#!/usr/bin/env Rscript
# Analyze SQL equivalence failures: run round-trip on cohorts, find first failure,
# diff normalized SQL and optionally compare JSON keys.
# Usage: Rscript tools/analyzeSqlFailures.R [n_sample]

ca <- commandArgs(trailingOnly = FALSE)
fa <- ca[grepl("^--file=", ca)]
scriptDir <- if (length(fa) > 0) dirname(sub("^--file=", "", fa[1])) else "."
source(file.path(scriptDir, "roundtrip_utils.R"))

devtools::load_all()
if (!("package:Capr" %in% search())) library(Capr, character.only = TRUE)
library(CirceR)

jsonFolder <- system.file("cohorts", package = "PhenotypeLibrary", mustWork = FALSE)
jsonFiles <- list.files(jsonFolder, pattern = "\\.json$", full.names = TRUE)

outRPath <- tempfile("rt")
dir.create(outRPath, showWarnings = FALSE)
on.exit(unlink(outRPath, recursive = TRUE), add = TRUE)

runOne <- function(jsonPath) {
  name <- sub("\\.json$", "", basename(jsonPath))
  rPath <- file.path(outRPath, paste0(name, ".R"))
  tryCatch(jsonToCaprFile(jsonPath, rPath, mode = "skip"), error = function(e) return(NULL))
  if (!file.exists(rPath)) return(NULL)
  orig <- readChar(jsonPath, file.info(jsonPath)$size)
  env <- new.env(parent = .GlobalEnv)
  err <- tryCatch(sys.source(rPath, envir = env), error = function(e) e)
  if (inherits(err, "error")) return(NULL)
  if (is.null(env$cohortDef)) return(NULL)
  rt <- tryCatch(compile(env$cohortDef), error = function(e) NULL)
  if (is.null(rt)) return(NULL)
  opts <- CirceR::createGenerateOptions(generateStats = FALSE)
  sqlO <- tryCatch(
    CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(orig), options = opts),
    error = function(e) NULL
  )
  sqlR <- tryCatch(
    CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(rt), options = opts),
    error = function(e) NULL
  )
  if (is.null(sqlO) || is.null(sqlR)) return(list(name = name, sqlEq = FALSE, err = TRUE))
  sqlEq <- identical(normalizeCirceSql(sqlO), normalizeCirceSql(sqlR))
  list(name = name, orig = orig, rt = rt, sqlO = sqlO, sqlR = sqlR, sqlEq = sqlEq)
}

nSample <- as.integer(commandArgs(TRUE)[1])
if (is.na(nSample) || nSample <= 0) nSample <- 100L
set.seed(42)
idx <- sample(seq_along(jsonFiles), min(nSample, length(jsonFiles)))
message("Running round-trip on ", length(idx), " cohorts...")
results <- lapply(jsonFiles[idx], runOne)
results <- results[!vapply(results, is.null, logical(1))]
failed <- results[!vapply(results, function(r) isTRUE(r$sqlEq), logical(1))]
failed <- failed[!vapply(failed, function(r) isTRUE(r$err), logical(1))]
passed <- results[vapply(results, function(r) isTRUE(r$sqlEq), logical(1))]

message("Sampled: ", length(results), " round-trips, ", length(passed), " SQL pass, ", length(failed), " SQL fail")

if (length(failed) == 0) {
  message("No SQL failures in sample.")
  quit(save = "no", status = 0)
}

# Analyze first 5 failures: SQL diff position + JSON field comparison
for (fi in seq_len(min(5L, length(failed)))) {
  r <- failed[[fi]]
  message("")
  message("=== Failure ", fi, ": ", r$name, " ===")
  no <- normalizeCirceSql(r$sqlO)
  nr <- normalizeCirceSql(r$sqlR)
  diffInfo <- firstSqlDiff(no, nr, contextChars = 35L)
  if (!is.na(diffInfo$position)) {
    message("First SQL diff at position ", diffInfo$position, ":")
    message("  Orig: ", diffInfo$orig)
    message("  Rt  : ", diffInfo$rt)
  }
  # Compare key JSON fields
  o <- jsonlite::fromJSON(r$orig, simplifyVector = FALSE)
  rt <- jsonlite::fromJSON(r$rt, simplifyVector = FALSE)
  for (key in c("QualifiedLimit", "ExpressionLimit", "PrimaryCriteria")) {
    vo <- o[[key]]
    vr <- rt[[key]]
    if (is.list(vo)) vo <- vo$Type %||% names(vo)[1] %||% paste(capture.output(str(vo)), collapse = " ")
    if (is.list(vr)) vr <- vr$Type %||% names(vr)[1] %||% paste(capture.output(str(vr)), collapse = " ")
    if (!identical(vo, vr)) message("  JSON diff ", key, ": orig=", vo, " rt=", vr)
  }
  if (!identical(length(o$ConceptSets), length(rt$ConceptSets))) {
    message("  JSON diff ConceptSets count: orig=", length(o$ConceptSets), " rt=", length(rt$ConceptSets))
  }
}

message("")
message("Done.")
