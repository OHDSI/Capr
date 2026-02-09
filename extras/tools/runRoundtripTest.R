#!/usr/bin/env Rscript
# Run round-trip test on all PhenotypeLibrary cohorts: JSON -> jsonToCapr -> R -> source -> compile.
# Reports JSON (semantic) equivalence and SQL equivalence (original vs reordered round-trip JSON).
# Usage: Rscript extras/runRoundtripTest.R
# Requires: Capr (load_all from repo or install), PhenotypeLibrary, CirceR (for SQL comparison).

ca <- commandArgs(trailingOnly = FALSE)
fa <- ca[grepl("^--file=", ca)]
scriptDir <- if (length(fa) > 0) dirname(sub("^--file=", "", fa[1])) else "."
source(file.path(scriptDir, "roundtrip_utils.R"))

devtools::load_all()
if (!("package:Capr" %in% search())) library(Capr, character.only = TRUE)

if (!requireNamespace("PhenotypeLibrary", quietly = TRUE)) {
  message("PhenotypeLibrary is not installed. Install it first.")
  quit(save = "no", status = 1)
}
jsonFolder <- system.file("cohorts", package = "PhenotypeLibrary", mustWork = FALSE)
if (!nzchar(jsonFolder) || !dir.exists(jsonFolder)) {
  message("PhenotypeLibrary cohorts folder not found.")
  quit(save = "no", status = 1)
}
jsonFiles <- list.files(jsonFolder, pattern = "\\.json$", full.names = TRUE)
message("Found ", length(jsonFiles), " cohort JSON files in PhenotypeLibrary")
if (length(jsonFiles) == 0L) quit(save = "no", status = 1)

# Normalize Circe JSON list (concept set order and IDs) for comparison.
normalizeCirceList <- function(x, idMap = NULL) {
  if (is.null(idMap) && is.list(x) && !is.null(x$ConceptSets) && length(x$ConceptSets) > 0L) {
    ids <- vapply(x$ConceptSets, function(cs) as.integer(if (!is.null(cs$id)) cs$id else 0L), integer(1L))
    o <- order(ids)
    x$ConceptSets <- x$ConceptSets[o]
    idMap <- stats::setNames(seq_along(ids), as.character(ids[o]))
    for (k in seq_along(x$ConceptSets)) x$ConceptSets[[k]]$id <- idMap[[k]]
  }
  if (!is.list(x)) return(x)
  if (!is.null(idMap)) {
    for (key in c("CodesetId", "CodesetID")) {
      if (!is.null(x[[key]])) {
        mapped <- idMap[as.character(x[[key]])]
        if (length(mapped) > 0L && !is.na(mapped[1L])) x[[key]] <- mapped[1L]
      }
    }
  }
  x[] <- lapply(x, function(v) {
    if (is.list(v) && length(v) > 0L && is.null(names(v))) {
      lapply(v, function(w) normalizeCirceList(w, idMap))
    } else {
      normalizeCirceList(v, idMap)
    }
  })
  x
}

roundtripCirceSqlEquivalent <- function(originalJsonStr, roundTripJsonStr) {
  if (!requireNamespace("CirceR", quietly = TRUE)) {
    return(list(ok = NA, msg = "CirceR not installed"))
  }
  # Reorder round-trip ConceptSets to match original so codeset indices align
  rtForSql <- reorderRoundtripConceptSetsToMatchOriginal(originalJsonStr, roundTripJsonStr)
  opts <- CirceR::createGenerateOptions(generateStats = FALSE)
  sqlOrig <- tryCatch(
    CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(originalJsonStr), options = opts),
    error = function(e) list(ok = FALSE, msg = paste0("Original SQL: ", conditionMessage(e)))
  )
  if (is.list(sqlOrig) && !is.null(sqlOrig$ok)) return(sqlOrig)
  sqlRt <- tryCatch(
    CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(rtForSql), options = opts),
    error = function(e) list(ok = FALSE, msg = paste0("Round-trip SQL: ", conditionMessage(e)))
  )
  if (is.list(sqlRt) && !is.null(sqlRt$ok)) return(sqlRt)
  if (identical(normalizeCirceSql(sqlOrig), normalizeCirceSql(sqlRt))) {
    return(list(ok = TRUE, msg = "OK"))
  }
  list(ok = FALSE, msg = "Generated Circe SQL differs")
}

# JSON (semantic) equivalence: same concept set count + concept IDs per set, same primary criteria domains, same end strategy.
# Compare original vs reordered round-trip so concept set order aligns.
roundtripJsonSemanticallyEquivalent <- function(originalJsonStr, roundTripJsonStr) {
  rtForSql <- reorderRoundtripConceptSetsToMatchOriginal(originalJsonStr, roundTripJsonStr)
  orig <- jsonlite::fromJSON(originalJsonStr, simplifyVector = FALSE)
  rt <- jsonlite::fromJSON(rtForSql, simplifyVector = FALSE)
  if (length(orig$ConceptSets %||% list()) != length(rt$ConceptSets %||% list())) {
    return(list(ok = FALSE, msg = "Concept set count differs"))
  }
  getConceptIds <- function(cs) {
    items <- cs$expression$items %||% list()
    sort(as.integer(vapply(items, function(it) {
      c <- it$concept %||% it
      id <- c$CONCEPT_ID %||% c$concept_id
      if (is.null(id)) NA_integer_ else as.numeric(id)
    }, numeric(1))))
  }
  origIds <- lapply(orig$ConceptSets %||% list(), getConceptIds)
  rtIds <- lapply(rt$ConceptSets %||% list(), getConceptIds)
  if (!identical(origIds, rtIds)) {
    return(list(ok = FALSE, msg = "Concept IDs in sets differ"))
  }
  pclOrig <- orig$PrimaryCriteria$CriteriaList %||% list()
  pclRt <- rt$PrimaryCriteria$CriteriaList %||% list()
  origDomains <- vapply(pclOrig, function(cl) names(cl)[1L], character(1L))
  rtDomains <- vapply(pclRt, function(cl) names(cl)[1L], character(1L))
  if (!identical(sort(origDomains), sort(rtDomains))) {
    return(list(ok = FALSE, msg = "Primary criteria domains differ"))
  }
  origEnd <- names(orig$EndStrategy %||% list())[1L]
  rtEnd <- names(rt$EndStrategy %||% list())[1L]
  if (!identical(origEnd, rtEnd)) {
    return(list(ok = FALSE, msg = paste0("End strategy differs: ", origEnd, " vs ", rtEnd)))
  }
  list(ok = TRUE, msg = "OK")
}

runOneRoundtrip <- function(jsonPath, outRPath) {
  name <- sub("\\.json$", "", basename(jsonPath))
  rPath <- file.path(outRPath, paste0(name, ".R"))
  tryCatch(
    jsonToCaprFile(jsonPath, rPath, mode = "skip"),
    error = function(e) return(list(ok = FALSE, name = name, stage = "jsonToCapr", msg = conditionMessage(e)))
  )
  if (!file.exists(rPath)) {
    return(list(ok = FALSE, name = name, stage = "write", msg = "R file was not created"))
  }
  originalRaw <- readChar(jsonPath, file.info(jsonPath)$size)
  # Use .GlobalEnv as parent so sourced code sees package:Capr
  env <- new.env(parent = .GlobalEnv)
  err <- tryCatch(
    sys.source(rPath, envir = env),
    error = function(e) return(list(ok = FALSE, name = name, stage = "source", msg = conditionMessage(e)))
  )
  if (inherits(err, "list")) return(err)
  cohortDef <- env$cohortDef
  if (is.null(cohortDef)) {
    return(list(ok = FALSE, name = name, stage = "source", msg = "Generated R did not create cohortDef"))
  }
  # Include all ConceptSet objects from env so unused concept sets are preserved (same count as original JSON)
  allCs <- tryCatch({
    objs <- mget(ls(env), envir = env, ifnotfound = list(NULL))
    Filter(function(x) methods::is(x, "ConceptSet"), objs)
  }, error = function(e) list())
  roundTripJsonStr <- tryCatch(
    if (length(allCs) > 0L) compile(cohortDef, includeConceptSets = allCs) else compile(cohortDef),
    error = function(e) return(list(ok = FALSE, name = name, stage = "compile", msg = conditionMessage(e)))
  )
  if (inherits(roundTripJsonStr, "list")) return(roundTripJsonStr)
  list(ok = TRUE, name = name, originalRaw = originalRaw, roundTripJsonStr = roundTripJsonStr)
}

outRPath <- tempfile("capr_roundtrip")
dir.create(outRPath, showWarnings = FALSE, recursive = TRUE)
results <- lapply(jsonFiles, runOneRoundtrip, outRPath = outRPath)
unlink(outRPath, recursive = TRUE)

roundtripOk <- vapply(results, function(r) identical(r$ok, TRUE), logical(1L))
nTotal <- length(jsonFiles)
nNoRoundtrip <- sum(!roundtripOk)

# JSON (semantic) and SQL equivalence for cohorts that round-tripped
jsonResults <- list()
sqlResults <- list()
for (i in which(roundtripOk)) {
  r <- results[[i]]
  jsonResults[[r$name]] <- roundtripJsonSemanticallyEquivalent(r$originalRaw, r$roundTripJsonStr)
  sqlResults[[r$name]] <- roundtripCirceSqlEquivalent(r$originalRaw, r$roundTripJsonStr)
}

nRoundtrip <- length(sqlResults)
jsonPass <- sum(vapply(jsonResults, function(x) identical(x$ok, TRUE), logical(1L)))
jsonFail <- sum(vapply(jsonResults, function(x) identical(x$ok, FALSE), logical(1L)))
sqlPass <- sum(vapply(sqlResults, function(x) identical(x$ok, TRUE), logical(1L)))
sqlFail <- sum(vapply(sqlResults, function(x) identical(x$ok, FALSE), logical(1L)))
sqlNa <- sum(vapply(sqlResults, function(x) identical(x$ok, NA), logical(1L)))

message("")
message("Round-trip (PhenotypeLibrary cohorts):")
message("  Round-trip succeeded: ", nRoundtrip, " (decompile -> source -> compile)")
message("  Round-trip failed:   ", nNoRoundtrip, " (write/source/compile)")
message("  Total:               ", nTotal)
message("")
message("JSON equivalence (semantic: concept sets, primary criteria domains, end strategy; reordered round-trip):")
message("  Pass: ", jsonPass)
message("  Fail: ", jsonFail)
message("  (among ", nRoundtrip, " cohorts that round-tripped)")
message("")
message("SQL equivalence (same Circe SQL from original vs reordered round-trip JSON):")
message("  Pass: ", sqlPass)
message("  Fail: ", sqlFail)
if (sqlNa > 0L) message("  N/A (CirceR not installed): ", sqlNa)
message("  (among ", nRoundtrip, " cohorts that round-tripped)")
message("")

if (nNoRoundtrip > 0L) {
  failedRt <- results[!roundtripOk]
  stages <- table(vapply(failedRt, function(r) r$stage, character(1)))
  message("Round-trip failures by stage:")
  for (s in names(stages)) message("  ", s, ": ", stages[s])
  message("")
  # Write all failures to file for analysis (relative to repo root if in extras/)
  failFile <- "tools/roundtrip_failures.txt"
  if (file.exists("extras/runRoundtripTest.R")) failFile <- file.path("tools", "roundtrip_failures.txt")
  tryCatch({
    conn <- file(failFile, open = "wt")
    on.exit(close(conn))
    writeLines(paste0(vapply(failedRt, function(r) paste0(r$name, "\t", r$stage, "\t", gsub("[\t\n\r]+", " ", r$msg)), character(1))), conn)
  }, error = function(e) NULL)
  message("All ", length(failedRt), " round-trip failures (name, stage, msg):")
  for (r in failedRt) {
    message("  ", r$name, " [", r$stage, "] ", substr(r$msg, 1L, 80))
  }
  message("")
}

if (jsonFail > 0L) {
  jsonFailNames <- names(jsonResults)[vapply(jsonResults, function(x) identical(x$ok, FALSE), logical(1L))]
  message("JSON-equivalence failures (", length(jsonFailNames), "): ", paste(head(jsonFailNames, 20), collapse = ", "))
  if (length(jsonFailNames) > 20L) message(" ... and ", length(jsonFailNames) - 20L, " more")
  message("")
}

if (sqlFail > 0L) {
  sqlFailNames <- names(sqlResults)[vapply(sqlResults, function(x) identical(x$ok, FALSE), logical(1L))]
  message("SQL-equivalence failures (", length(sqlFailNames), "): ", paste(sort(sqlFailNames), collapse = ", "))
}
