#!/usr/bin/env Rscript
# Run round-trip test on all PhenotypeLibrary cohorts: JSON -> jsonToCapr -> R -> source -> compile.
# Reports JSON (semantic) equivalence and SQL equivalence (original vs reordered round-trip JSON).
# Usage: Rscript extras/runRoundtripTest.R
# Requires: Capr (load_all from repo or install), PhenotypeLibrary, CirceR (for SQL comparison).

devtools::load_all()
if (!("package:Capr" %in% search())) library(Capr, character.only = TRUE)
`%||%` <- function(x, y) if (is.null(x)) y else x

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

# Sort concept_id in (id1,id2,...) so order differences don't affect equivalence
sort_concept_id_in_lists <- function(s) {
  pattern <- "concept_id in \\(([0-9,]+)\\)"
  m <- gregexpr(pattern, s)[[1]]
  if (m[1] < 0) return(s)
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  matches <- substring(s, starts, starts + lens - 1)
  inners <- sub(pattern, "\\1", matches)
  replacements <- vapply(inners, function(inner) {
    nums <- sort(as.integer(strsplit(inner, ",", fixed = TRUE)[[1]]))
    paste0("concept_id in (", paste(nums, collapse = ","), ")")
  }, character(1))
  for (i in rev(seq_along(starts))) {
    s <- paste0(
      substr(s, 1, starts[i] - 1),
      replacements[i],
      substr(s, starts[i] + lens[i], nchar(s))
    )
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

# Fingerprint a concept set for matching (ignore id, name, concept metadata)
concept_set_fingerprint <- function(cs) {
  items <- cs$expression$items %||% list()
  fp <- lapply(items, function(it) {
    c <- it$concept %||% it
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

# Reorder round-trip ConceptSets to match original order; remap CodesetIds so SQL comparison is valid.
reorder_roundtrip_concept_sets_to_match_original <- function(originalJsonStr, roundTripJsonStr) {
  orig <- jsonlite::fromJSON(originalJsonStr, simplifyVector = FALSE)
  rt <- jsonlite::fromJSON(roundTripJsonStr, simplifyVector = FALSE)
  origSets <- orig$ConceptSets %||% list()
  rtSets <- rt$ConceptSets %||% list()
  if (length(origSets) == 0 || length(rtSets) == 0) return(roundTripJsonStr)
  origFp <- lapply(origSets, concept_set_fingerprint)
  rtFp <- lapply(rtSets, concept_set_fingerprint)
  # For each orig set in order, find matching rt set (by fingerprint)
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
  idMap <- stats::setNames(seq_along(reordered) - 1L, as.character(oldIds))
  rt$ConceptSets <- reordered
  replace_codeset_ids <- function(x, map) {
    if (is.null(x)) return(x)
    if (is.list(x)) {
      for (key in c("CodesetId", "CodesetID", "DrugCodesetId")) {
        if (!is.null(x[[key]])) {
          m <- map[as.character(x[[key]])]
          if (length(m) > 0L && !is.na(m[1L])) x[[key]] <- m[1L]
        }
      }
      return(lapply(x, replace_codeset_ids, map = map))
    }
    x
  }
  rt <- replace_codeset_ids(rt, idMap)
  as.character(jsonlite::toJSON(rt, auto_unbox = TRUE))
}

roundtrip_circe_sql_equivalent <- function(originalJsonStr, roundTripJsonStr) {
  if (!requireNamespace("CirceR", quietly = TRUE)) {
    return(list(ok = NA, msg = "CirceR not installed"))
  }
  # Reorder round-trip ConceptSets to match original so codeset indices align
  rtForSql <- reorder_roundtrip_concept_sets_to_match_original(originalJsonStr, roundTripJsonStr)
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
  if (identical(normalize_circe_sql(sqlOrig), normalize_circe_sql(sqlRt))) {
    return(list(ok = TRUE, msg = "OK"))
  }
  list(ok = FALSE, msg = "Generated Circe SQL differs")
}

# JSON (semantic) equivalence: same concept set count + concept IDs per set, same primary criteria domains, same end strategy.
# Compare original vs reordered round-trip so concept set order aligns.
roundtrip_json_semantically_equivalent <- function(originalJsonStr, roundTripJsonStr) {
  rtForSql <- reorder_roundtrip_concept_sets_to_match_original(originalJsonStr, roundTripJsonStr)
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

run_one_roundtrip <- function(jsonPath, outRPath) {
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
results <- lapply(jsonFiles, run_one_roundtrip, outRPath = outRPath)
unlink(outRPath, recursive = TRUE)

roundtrip_ok <- vapply(results, function(r) identical(r$ok, TRUE), logical(1L))
n_total <- length(jsonFiles)
n_no_roundtrip <- sum(!roundtrip_ok)

# JSON (semantic) and SQL equivalence for cohorts that round-tripped
json_results <- list()
sql_results <- list()
for (i in which(roundtrip_ok)) {
  r <- results[[i]]
  json_results[[r$name]] <- roundtrip_json_semantically_equivalent(r$originalRaw, r$roundTripJsonStr)
  sql_results[[r$name]] <- roundtrip_circe_sql_equivalent(r$originalRaw, r$roundTripJsonStr)
}

n_roundtrip <- length(sql_results)
json_pass <- sum(vapply(json_results, function(x) identical(x$ok, TRUE), logical(1L)))
json_fail <- sum(vapply(json_results, function(x) identical(x$ok, FALSE), logical(1L)))
sql_pass <- sum(vapply(sql_results, function(x) identical(x$ok, TRUE), logical(1L)))
sql_fail <- sum(vapply(sql_results, function(x) identical(x$ok, FALSE), logical(1L)))
sql_na <- sum(vapply(sql_results, function(x) identical(x$ok, NA), logical(1L)))

message("")
message("Round-trip (PhenotypeLibrary cohorts):")
message("  Round-trip succeeded: ", n_roundtrip, " (decompile -> source -> compile)")
message("  Round-trip failed:   ", n_no_roundtrip, " (write/source/compile)")
message("  Total:               ", n_total)
message("")
message("JSON equivalence (semantic: concept sets, primary criteria domains, end strategy; reordered round-trip):")
message("  Pass: ", json_pass)
message("  Fail: ", json_fail)
message("  (among ", n_roundtrip, " cohorts that round-tripped)")
message("")
message("SQL equivalence (same Circe SQL from original vs reordered round-trip JSON):")
message("  Pass: ", sql_pass)
message("  Fail: ", sql_fail)
if (sql_na > 0L) message("  N/A (CirceR not installed): ", sql_na)
message("  (among ", n_roundtrip, " cohorts that round-tripped)")
message("")

if (n_no_roundtrip > 0L) {
  failed_rt <- results[!roundtrip_ok]
  stages <- table(vapply(failed_rt, function(r) r$stage, character(1)))
  message("Round-trip failures by stage:")
  for (s in names(stages)) message("  ", s, ": ", stages[s])
  message("")
  # Write all failures to file for analysis (relative to repo root if in extras/)
  failFile <- "tools/roundtrip_failures.txt"
  if (file.exists("extras/runRoundtripTest.R")) failFile <- file.path("tools", "roundtrip_failures.txt")
  tryCatch({
    conn <- file(failFile, open = "wt")
    on.exit(close(conn))
    writeLines(paste0(vapply(failed_rt, function(r) paste0(r$name, "\t", r$stage, "\t", gsub("[\t\n\r]+", " ", r$msg)), character(1))), conn)
  }, error = function(e) NULL)
  message("All ", length(failed_rt), " round-trip failures (name, stage, msg):")
  for (r in failed_rt) {
    message("  ", r$name, " [", r$stage, "] ", substr(r$msg, 1L, 80))
  }
  message("")
}

if (json_fail > 0L) {
  json_fail_names <- names(json_results)[vapply(json_results, function(x) identical(x$ok, FALSE), logical(1L))]
  message("JSON-equivalence failures (", length(json_fail_names), "): ", paste(head(json_fail_names, 20), collapse = ", "))
  if (length(json_fail_names) > 20L) message(" ... and ", length(json_fail_names) - 20L, " more")
  message("")
}

if (sql_fail > 0L) {
  sql_fail_names <- names(sql_results)[vapply(sql_results, function(x) identical(x$ok, FALSE), logical(1L))]
  message("SQL-equivalence failures (", length(sql_fail_names), "): ", paste(sort(sql_fail_names), collapse = ", "))
}
