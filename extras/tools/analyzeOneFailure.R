#!/usr/bin/env Rscript
# Analyze one cohort that fails SQL equivalence: compare JSON and SQL.
# Usage: Rscript tools/analyzeOneFailure.R <cohort_id>
# Example: Rscript tools/analyzeOneFailure.R 1024

args <- commandArgs(TRUE)
if (length(args) < 1) stop("Usage: Rscript tools/analyzeOneFailure.R <cohort_id>")
name <- args[1]

ca <- commandArgs(trailingOnly = FALSE)
fa <- ca[grepl("^--file=", ca)]
scriptDir <- if (length(fa) > 0) dirname(sub("^--file=", "", fa[1])) else "."
source(file.path(scriptDir, "roundtrip_utils.R"))

devtools::load_all()
if (!("package:Capr" %in% search())) library(Capr, character.only = TRUE)
library(CirceR)
library(jsonlite)

jsonFolder <- system.file("cohorts", package = "PhenotypeLibrary", mustWork = FALSE)
jsonPath <- file.path(jsonFolder, paste0(name, ".json"))
if (!file.exists(jsonPath)) stop("Cohort file not found: ", jsonPath)

outR <- tempfile(fileext = ".R")
jsonToCaprFile(jsonPath, outR, mode = "skip")
if (!file.exists(outR)) stop("Decompile did not produce R file")
env <- new.env(parent = .GlobalEnv)
err <- tryCatch(sys.source(outR, envir = env), error = identity)
if (inherits(err, "error")) stop("Source failed: ", conditionMessage(err))
if (is.null(env$cohortDef)) stop("Generated R did not create cohortDef")
allCs <- Filter(function(x) methods::is(x, "ConceptSet"), mget(ls(env), envir = env, ifnotfound = list(NULL)))
rtRaw <- compile(env$cohortDef, includeConceptSets = allCs)

origRaw <- readChar(jsonPath, file.info(jsonPath)$size)
orig <- fromJSON(origRaw, simplifyVector = FALSE)

# Use reordered round-trip JSON for SQL comparison (same as test's roundtripCirceSqlEquivalent).
rtForSql <- reorderRoundtripConceptSetsToMatchOriginal(origRaw, rtRaw)
rt <- fromJSON(rtForSql, simplifyVector = FALSE)
opts <- CirceR::createGenerateOptions(generateStats = FALSE)
sqlO <- CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(origRaw), options = opts)
sqlR <- CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(rtForSql), options = opts)
sqlEq <- identical(normalizeCirceSql(sqlO), normalizeCirceSql(sqlR))
message("Cohort ", name, " SQL equivalent: ", sqlEq)
if (sqlEq) quit(save = "no", status = 0)

# Find first SQL diff
no <- normalizeCirceSql(sqlO)
nr <- normalizeCirceSql(sqlR)
diffInfo <- firstSqlDiff(no, nr, contextChars = 45L)
if (!is.na(diffInfo$position)) {
  message("First SQL diff at position ", diffInfo$position)
  message("  Orig: ", diffInfo$orig)
  message("  Rt:   ", diffInfo$rt)
}

# Compare key JSON
message("")
message("=== ConceptSets count ===")
message("Orig: ", length(orig$ConceptSets), "  Rt: ", length(rt$ConceptSets))

message("")
message("=== Concept set items (id, isExcluded, includeDescendants) ===")
getItemInfo <- function(cs) {
  it <- cs$expression$items
  if (is.null(it)) return(list())
  lapply(it, function(x) {
    c <- x$concept %||% x
    list(id = as.integer(c$CONCEPT_ID %||% c$concept_id), isExcluded = isTRUE(x$isExcluded), includeDescendants = isTRUE(x$includeDescendants))
  })
}
for (k in seq_len(max(length(orig$ConceptSets), length(rt$ConceptSets)))) {
  if (k <= length(orig$ConceptSets)) {
    oi <- getItemInfo(orig$ConceptSets[[k]])
    message("Orig set ", k, " (id ", orig$ConceptSets[[k]]$id, "): ", length(oi), " items")
    for (j in seq_along(oi)) message("    ", oi[[j]]$id, " ex=", oi[[j]]$isExcluded, " desc=", oi[[j]]$includeDescendants)
  }
  if (k <= length(rt$ConceptSets)) {
    ri <- getItemInfo(rt$ConceptSets[[k]])
    message("Rt   set ", k, " (id ", rt$ConceptSets[[k]]$id, "): ", length(ri), " items")
    for (j in seq_along(ri)) message("    ", ri[[j]]$id, " ex=", ri[[j]]$isExcluded, " desc=", ri[[j]]$includeDescendants)
  }
}

message("")
message("=== QualifiedLimit / ExpressionLimit ===")
message("Orig: QualifiedLimit=", orig$QualifiedLimit$Type, " ExpressionLimit=", orig$ExpressionLimit$Type)
message("Rt:   QualifiedLimit=", rt$QualifiedLimit$Type, " ExpressionLimit=", rt$ExpressionLimit$Type)

message("")
message("=== PrimaryCriteria first criterion keys ===")
if (length(orig$PrimaryCriteria$CriteriaList) > 0 && length(rt$PrimaryCriteria$CriteriaList) > 0) {
  domO <- names(orig$PrimaryCriteria$CriteriaList[[1]])[1]
  domR <- names(rt$PrimaryCriteria$CriteriaList[[1]])[1]
  message("Orig domain: ", domO, " keys: ", paste(names(orig$PrimaryCriteria$CriteriaList[[1]][[domO]]), collapse = ", "))
  message("Rt   domain: ", domR, " keys: ", paste(names(rt$PrimaryCriteria$CriteriaList[[1]][[domR]]), collapse = ", "))
}

message("")
message("=== EndStrategy ===")
message("Orig: ", paste(names(orig$EndStrategy), collapse = ", "))
message("Rt:   ", paste(names(rt$EndStrategy), collapse = ", "))
if (length(orig$EndStrategy) > 0 && length(rt$EndStrategy) > 0) {
  message("Orig EndStrategy first: ", paste(names(orig$EndStrategy[[1]]), orig$EndStrategy[[1]], collapse = " "))
  message("Rt   EndStrategy first: ", paste(names(rt$EndStrategy[[1]]), rt$EndStrategy[[1]], collapse = " "))
}
