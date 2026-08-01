# Run end-to-end tests on a vector of cohort JSON files.
# For each file: decompile (jsonToCapr -> source -> compile), then compare
# decompiled JSON and SQL to circeR. Print nothing when both match; print
# file name and message when JSON or SQL is not equivalent.
#
# Usage:
#   source("extras/tools/runE2ETest.R")
#   runE2ETest(c("path/to/cohort1.json", "path/to/cohort2.json"))
#
# Requires: Capr (devtools::load_all() or install), CirceR (for SQL comparison).
# roundtrip_utils.R must be in the same directory as this file.

ca <- commandArgs(trailingOnly = FALSE)
fa <- ca[grepl("^--file=", ca)]
scriptDir <- if (length(fa) > 0) dirname(sub("^--file=", "", fa[1])) else "extras/tools"
source(file.path(scriptDir, "roundtrip_utils.R"))

roundtripCirceSqlEquivalent <- function(originalJsonStr, roundTripJsonStr) {
  if (!requireNamespace("CirceR", quietly = TRUE)) {
    return(list(ok = NA, msg = "CirceR not installed"))
  }
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
  # Compare sets of concept sets regardless of order (sort by canonical key so order doesn't cause failure)
  canonical <- function(ids) paste(sort(ids), collapse = ",")
  origSorted <- origIds[order(vapply(origIds, canonical, character(1)))]
  rtSorted <- rtIds[order(vapply(rtIds, canonical, character(1)))]
  if (!identical(origSorted, rtSorted)) {
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
    Capr::jsonToCaprFile(jsonPath, rPath, mode = "skip"),
    error = function(e) return(list(ok = FALSE, name = name, stage = "jsonToCapr", msg = conditionMessage(e)))
  )
  if (!file.exists(rPath)) {
    return(list(ok = FALSE, name = name, stage = "write", msg = "R file was not created"))
  }
  originalRaw <- readChar(jsonPath, file.info(jsonPath)$size)
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
  allCs <- tryCatch({
    objs <- mget(ls(env), envir = env, ifnotfound = list(NULL))
    Filter(function(x) methods::is(x, "ConceptSet"), objs)
  }, error = function(e) list())
  roundTripJsonStr <- tryCatch(
    if (length(allCs) > 0L) Capr::toCohortJson(cohortDef, includeConceptSets = allCs) else Capr::toCohortJson(cohortDef),
    error = function(e) return(list(ok = FALSE, name = name, stage = "compile", msg = conditionMessage(e)))
  )
  if (inherits(roundTripJsonStr, "list")) return(roundTripJsonStr)
  list(ok = TRUE, name = name, originalRaw = originalRaw, roundTripJsonStr = roundTripJsonStr)
}

#' Run end-to-end tests on cohort JSON files
#'
#' For each JSON file: decompile (jsonToCapr -> source -> toCohortJson), then
#' compare the decompiled round-trip JSON and Circe SQL to the original.
#' Prints nothing when both JSON and SQL are equivalent; prints the file name
#' and which check failed when not.
#'
#' @param jsonFiles Character vector of paths to cohort JSON files.
#' @param outRPath Optional directory for temporary R files (default: temp dir).
#' @return Invisibly, a list with elements \code{roundtrip_ok}, \code{json_ok},
#'   \code{sql_ok} (each logical same length as \code{jsonFiles}), and
#'   \code{details} (list of per-file result lists).
#' @export
runE2ETest <- function(jsonFiles, outRPath = tempfile("capr_e2e")) {
  stopifnot(is.character(jsonFiles))
  jsonFiles <- jsonFiles[nzchar(jsonFiles)]
  if (length(jsonFiles) == 0L) {
    return(invisible(list(roundtrip_ok = logical(), json_ok = logical(), sql_ok = logical(), details = list())))
  }
  if (!requireNamespace("Capr", quietly = TRUE)) {
    stop("Capr is required. Install the package or run devtools::load_all() from the Capr repo.")
  }
  if (!("package:Capr" %in% search())) {
    library(Capr, character.only = TRUE)
  }
  dir.create(outRPath, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outRPath, recursive = TRUE), add = TRUE)

  n <- length(jsonFiles)
  roundtrip_ok <- logical(n)
  json_ok <- rep(NA, n)
  sql_ok <- rep(NA, n)
  details <- vector("list", n)

  for (i in seq_along(jsonFiles)) {
    path <- jsonFiles[i]
    if (!file.exists(path)) {
      message(basename(path), ": file not found")
      next
    }
    r <- runOneRoundtrip(path, outRPath)
    details[[i]] <- r
    roundtrip_ok[i] <- identical(r$ok, TRUE)
    if (!roundtrip_ok[i]) {
      message(basename(path), ": round-trip failed (", r$stage, ")")
      next
    }
    jsonRes <- roundtripJsonSemanticallyEquivalent(r$originalRaw, r$roundTripJsonStr)
    sqlRes <- roundtripCirceSqlEquivalent(r$originalRaw, r$roundTripJsonStr)
    json_ok[i] <- identical(jsonRes$ok, TRUE)
    sql_ok[i] <- identical(sqlRes$ok, TRUE)
    if (!json_ok[i]) {
      message(basename(path), ": json not equivalent")
    }
    if (!sql_ok[i] && !identical(sqlRes$ok, NA)) {
      message(basename(path), ": sql not equivalent")
    }
  }

  invisible(list(
    roundtrip_ok = roundtrip_ok,
    json_ok = json_ok,
    sql_ok = sql_ok,
    details = details
  ))
}

#' Generate SQL from original and round-trip JSON and write diff to file
#'
#' Takes one cohort JSON file, generates Circe SQL from the original JSON and
#' from the round-trip (decompile to R -> source -> compile back to JSON),
#' then writes the unified diff of the two SQL strings to \code{diff.sql}.
#'
#' @param jsonPath Character. Path to one cohort JSON file.
#' @param outFile Character. Path of the output file for the SQL diff (default: \code{diff.sql} in current directory).
#' @return Invisibly, a list with \code{roundtrip_ok}, \code{sql_orig}, \code{sql_roundtrip},
#'   and \code{diff_file} (path written). On round-trip or Circe failure, \code{outFile}
#'   is still written with an error message.
#' @export
diffCirceSql <- function(jsonPath, outFile = "diff.sql") {
  stopifnot(is.character(jsonPath), length(jsonPath) == 1L, nzchar(jsonPath))
  if (!file.exists(jsonPath)) {
    stop("JSON file not found: ", jsonPath)
  }
  if (!requireNamespace("Capr", quietly = TRUE)) {
    stop("Capr is required. Install the package or run devtools::load_all() from the Capr repo.")
  }
  if (!requireNamespace("CirceR", quietly = TRUE)) {
    stop("CirceR is required to generate SQL.")
  }
  if (!("package:Capr" %in% search())) {
    library(Capr, character.only = TRUE)
  }

  outRPath <- tempfile("capr_diff")
  dir.create(outRPath, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outRPath, recursive = TRUE), add = TRUE)

  r <- runOneRoundtrip(jsonPath, outRPath)
  if (!identical(r$ok, TRUE)) {
    writeLines(
      paste0("Round-trip failed (", r$stage, "): ", r$msg),
      outFile
    )
    return(invisible(list(roundtrip_ok = FALSE, sql_orig = NULL, sql_roundtrip = NULL, diff_file = outFile)))
  }

  originalRaw <- r$originalRaw
  rtForSql <- reorderRoundtripConceptSetsToMatchOriginal(originalRaw, r$roundTripJsonStr)
  opts <- CirceR::createGenerateOptions(generateStats = FALSE)
  sqlOrig <- tryCatch(
    CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(originalRaw), options = opts),
    error = function(e) paste0("# Error generating original SQL: ", conditionMessage(e))
  )
  sqlRt <- tryCatch(
    CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(rtForSql), options = opts),
    error = function(e) paste0("# Error generating round-trip SQL: ", conditionMessage(e))
  )
  if (is.list(sqlOrig) && !is.null(sqlOrig$ok)) {
    sqlOrig <- paste0("# Error: ", sqlOrig$msg)
  }
  if (is.list(sqlRt) && !is.null(sqlRt$ok)) {
    sqlRt <- paste0("# Error: ", sqlRt$msg)
  }

  normOrig <- normalizeCirceSql(sqlOrig)
  normRt <- normalizeCirceSql(sqlRt)
  if (identical(normOrig, normRt)) {
    writeLines("# No differences (normalized SQL is identical).", outFile)
    return(invisible(list(
      roundtrip_ok = TRUE,
      sql_orig = sqlOrig,
      sql_roundtrip = sqlRt,
      diff_file = outFile
    )))
  }

  f1 <- tempfile("sql_orig", fileext = ".sql")
  f2 <- tempfile("sql_rt", fileext = ".sql")
  on.exit(unlink(c(f1, f2)), add = TRUE)
  writeLines(sqlOrig, f1)
  writeLines(sqlRt, f2)
  diffOut <- system2("diff", c("-u", f1, f2), stdout = TRUE, stderr = TRUE)
  status <- attr(diffOut, "status")
  hasDiff <- length(diffOut) > 0L && (is.null(status) || status %in% c(0L, 1L))
  if (hasDiff) {
    writeLines(
      c(
        "# Unified diff: original (Circe) vs round-trip (Circe).",
        paste0("# JSON: ", normalizePath(jsonPath, winslash = "/")),
        "",
        diffOut
      ),
      outFile
    )
  } else {
    writeLines(
      c(
        "# Original (Circe) SQL:",
        "---",
        sqlOrig,
        "",
        "# Round-trip (Circe) SQL:",
        "---",
        sqlRt
      ),
      outFile
    )
  }
  invisible(list(
    roundtrip_ok = TRUE,
    sql_orig = sqlOrig,
    sql_roundtrip = sqlRt,
    diff_file = outFile
  ))
}

# Example usage (run manually, not on source):
#   devtools::load_all(".")
#   source("extras/tools/roundtrip_utils.R"); source("extras/tools/runE2ETest.R")
#   paths <- list.files("~/Desktop/AtlasCohortGenerator/inst/cohorts/", pattern = "json", full.names = TRUE)
#   runE2ETest(paths[100:300])
#   diffCirceSql("path/to/cohort.json", outFile = "diff.sql")



