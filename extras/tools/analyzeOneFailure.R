#!/usr/bin/env Rscript
# Analyze one cohort that fails SQL equivalence: compare JSON and SQL.
# Usage: Rscript tools/analyzeOneFailure.R <cohort_id>
# Example: Rscript tools/analyzeOneFailure.R 1024

args <- commandArgs(TRUE)
if (length(args) < 1) stop("Usage: Rscript tools/analyzeOneFailure.R <cohort_id>")
name <- args[1]

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

# Normalize SQL (from test file)
sort_concept_id_in_lists <- function(s) {
  pattern <- "concept_id in \\(([0-9,]+)\\)"
  m <- gregexpr(pattern, s)[[1]]
  if (m[1] < 0) return(s)
  starts <- as.integer(m); lens <- attr(m, "match.length")
  matches <- substring(s, starts, starts + lens - 1)
  inners <- sub(pattern, "\\1", matches)
  replacements <- vapply(inners, function(inner) {
    nums <- sort(as.integer(strsplit(inner, ",", fixed = TRUE)[[1]]))
    paste0("concept_id in (", paste(nums, collapse = ","), ")")
  }, character(1))
  for (i in rev(seq_along(starts))) {
    s <- paste0(substr(s, 1, starts[i] - 1), replacements[i], substr(s, starts[i] + lens[i], nchar(s)))
  }
  s
}
normalize_circe_sql <- function(s) {
  s <- trimws(gsub("[ \t\r\n]+", " ", s))
  s <- gsub("@codeset_[0-9]+", "@codeset_X", s)
  s <- gsub("[0-9]+ as codeset_id", "X as codeset_id", s)
  s <- gsub("codeset_id = [0-9]+", "codeset_id = X", s)
  s <- sort_concept_id_in_lists(s)
  s
}

# Reorder round-trip ConceptSets to match original so codeset indices align for SQL comparison.
concept_set_fingerprint <- function(cs) {
  items <- cs$expression$items
  if (is.null(items)) items <- list()
  fp <- lapply(items, function(it) {
    c <- it$concept
    if (is.null(c)) c <- it
    list(
      id = as.integer(c$CONCEPT_ID %||% c$concept_id),
      isExcluded = isTRUE(it$isExcluded),
      includeDescendants = isTRUE(it$includeDescendants),
      includeMapped = isTRUE(it$includeMapped)
    )
  })
  fp <- fp[order(vapply(fp, function(x) x$id, integer(1)))]
  list(fp)
}
`%||%` <- function(x, y) if (is.null(x)) y else x

reorder_roundtrip_concept_sets_to_match_original <- function(originalJsonStr, roundTripJsonStr) {
  orig <- fromJSON(originalJsonStr, simplifyVector = FALSE)
  rt <- fromJSON(roundTripJsonStr, simplifyVector = FALSE)
  origSets <- orig$ConceptSets %||% list()
  rtSets <- rt$ConceptSets %||% list()
  if (length(origSets) == 0 || length(rtSets) == 0) return(roundTripJsonStr)
  origFp <- lapply(origSets, concept_set_fingerprint)
  rtFp <- lapply(rtSets, concept_set_fingerprint)
  rtUsed <- logical(length(rtSets))
  newOrder <- integer(0)
  for (i in seq_along(origFp)) {
    for (j in which(!rtUsed)) {
      if (identical(origFp[[i]], rtFp[[j]])) {
        newOrder <- c(newOrder, j)
        rtUsed[j] <- TRUE
        break
      }
    }
  }
  if (length(newOrder) != length(rtSets)) return(roundTripJsonStr)
  reordered <- rtSets[newOrder]
  oldIds <- vapply(seq_along(reordered), function(k) reordered[[k]]$id, integer(1))
  for (k in seq_along(reordered)) reordered[[k]]$id <- k - 1L
  idMap <- setNames(seq_along(reordered) - 1L, as.character(oldIds))
  rt$ConceptSets <- reordered
  replace_codeset_ids <- function(x, map) {
    if (is.null(x)) return(x)
    if (!is.list(x)) return(x)
    for (key in c("CodesetId", "CodesetID", "DrugCodesetId")) {
      if (!is.null(x[[key]])) {
        m <- map[as.character(x[[key]])]
        if (length(m) > 0L && !is.na(m[1L])) x[[key]] <- m[1L]
      }
    }
    x[] <- lapply(x, replace_codeset_ids, map = map)
    x
  }
  rt <- replace_codeset_ids(rt, idMap)
  as.character(toJSON(rt, auto_unbox = TRUE))
}

# Use reordered round-trip JSON for SQL comparison (same as test's roundtrip_circe_sql_equivalent).
rtForSql <- reorder_roundtrip_concept_sets_to_match_original(origRaw, rtRaw)
rt <- fromJSON(rtForSql, simplifyVector = FALSE)
opts <- CirceR::createGenerateOptions(generateStats = FALSE)
sqlO <- CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(origRaw), options = opts)
sqlR <- CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(rtForSql), options = opts)
sqlEq <- identical(normalize_circe_sql(sqlO), normalize_circe_sql(sqlR))
message("Cohort ", name, " SQL equivalent: ", sqlEq)
if (sqlEq) quit(save = "no", status = 0)

# Find first SQL diff
no <- normalize_circe_sql(sqlO)
nr <- normalize_circe_sql(sqlR)
n <- min(nchar(no), nchar(nr))
for (i in 1:n) {
  if (substr(no, i, i) != substr(nr, i, i)) {
    message("First SQL diff at position ", i)
    message("  Orig: ", substr(no, max(1, i - 45), i + 55))
    message("  Rt:   ", substr(nr, max(1, i - 45), i + 55))
    break
  }
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
