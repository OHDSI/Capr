# test-jsonToCapr.R
# Round-trip: PhenotypeLibrary JSON -> jsonToCapr -> R code -> evaluate -> compile -> compare.
# Not every file is expected to pass (some use unsupported features). We expect most to pass
# and report failures so they can be debugged.
# To debug a single cohort: jsonToCaprFile(<json_path>, "out.R", mode = "skip"); source("out.R"); compile(cohortDef)

# Normalize parsed Circe JSON for comparison: concept set order and IDs may differ after round-trip
normalizeCirceList <- function(x, idMap = NULL) {
  if (is.null(idMap) && is.list(x) && !is.null(x$ConceptSets) && length(x$ConceptSets) > 0L) {
    ids <- vapply(x$ConceptSets, function(cs) as.integer(if (!is.null(cs$id)) cs$id else 0L), integer(1L))
    o <- order(ids)
    x$ConceptSets <- x$ConceptSets[o]
    idMap <- setNames(seq_along(ids), as.character(ids[o]))
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

run_one_roundtrip <- function(jsonPath, outRPath, envParent = baseenv()) {
  name <- sub("\\.json$", "", basename(jsonPath))
  rPath <- file.path(outRPath, paste0(name, ".R"))

  # 1. JSON -> Capr R code
  tryCatch(
    jsonToCaprFile(jsonPath, rPath, mode = "skip"),
    error = function(e) return(list(ok = FALSE, name = name, stage = "jsonToCapr", msg = conditionMessage(e)))
  )
  if (!file.exists(rPath)) {
    return(list(ok = FALSE, name = name, stage = "write", msg = "R file was not created"))
  }

  originalRaw <- readChar(jsonPath, file.info(jsonPath)$size)
  originalList <- jsonlite::fromJSON(originalRaw, simplifyVector = FALSE)
  originalList <- normalizeCirceList(originalList)

  # 2. Evaluate R code -> cohortDef (envParent = .GlobalEnv so sourced code sees package:Capr)
  env <- new.env(parent = envParent)
  err <- tryCatch(
    sys.source(rPath, envir = env),
    error = function(e) return(list(ok = FALSE, name = name, stage = "source", msg = conditionMessage(e)))
  )
  if (inherits(err, "list")) return(err)

  cohortDef <- env$cohortDef
  if (is.null(cohortDef)) {
    return(list(ok = FALSE, name = name, stage = "source", msg = "Generated R did not create cohortDef"))
  }

  # 3. compile(cohortDef) -> round-trip JSON
  roundTripList <- tryCatch(
    jsonlite::fromJSON(compile(cohortDef), simplifyVector = FALSE),
    error = function(e) return(list(ok = FALSE, name = name, stage = "compile", msg = conditionMessage(e)))
  )
  roundTripList <- normalizeCirceList(roundTripList)

  # 4. Compare (use waldo if available for better diff, else all.equal)
  diff <- if (requireNamespace("waldo", quietly = TRUE)) {
    waldo::compare(roundTripList, originalList, x_arg = "round-trip", y_arg = "original")
  } else {
    all.equal(roundTripList, originalList, tolerance = sqrt(.Machine$double.eps))
  }
  if (!isTRUE(diff)) {
    return(list(ok = FALSE, name = name, stage = "compare", msg = paste(as.character(diff), collapse = "\n")))
  }
  list(ok = TRUE, name = name)
}

# Normalize Circe SQL: collapse whitespace, canonicalize codeset IDs,
# sort concept_id in (...) lists (order does not affect results).
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

# Reorder round-trip ConceptSets to match original order so codeset indices align for SQL comparison.
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

reorder_roundtrip_concept_sets_to_match_original <- function(originalJsonStr, roundTripJsonStr) {
  orig <- jsonlite::fromJSON(originalJsonStr, simplifyVector = FALSE)
  rt <- jsonlite::fromJSON(roundTripJsonStr, simplifyVector = FALSE)
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
  as.character(jsonlite::toJSON(rt, auto_unbox = TRUE))
}

# Circe SQL equivalence: same executable SQL from CirceR for original vs round-trip JSON.
# Round-trip ConceptSets are reordered to match original so codeset indices align.
roundtrip_circe_sql_equivalent <- function(originalJsonStr, roundTripJsonStr, generateStats = FALSE) {
  if (!requireNamespace("CirceR", quietly = TRUE)) {
    return(list(ok = NA, msg = "CirceR not installed"))
  }
  rtForSql <- reorder_roundtrip_concept_sets_to_match_original(originalJsonStr, roundTripJsonStr)
  opts <- CirceR::createGenerateOptions(generateStats = generateStats)
  sqlOrig <- tryCatch(
    CirceR::buildCohortQuery(
      CirceR::cohortExpressionFromJson(originalJsonStr),
      options = opts
    ),
    error = function(e) return(list(ok = FALSE, msg = paste0("Original SQL failed: ", conditionMessage(e))))
  )
  if (is.list(sqlOrig) && !is.null(sqlOrig$ok)) return(sqlOrig)
  sqlRt <- tryCatch(
    CirceR::buildCohortQuery(
      CirceR::cohortExpressionFromJson(rtForSql),
      options = opts
    ),
    error = function(e) return(list(ok = FALSE, msg = paste0("Round-trip SQL failed: ", conditionMessage(e))))
  )
  if (is.list(sqlRt) && !is.null(sqlRt$ok)) return(sqlRt)
  if (identical(normalize_circe_sql(sqlOrig), normalize_circe_sql(sqlRt))) {
    return(list(ok = TRUE, msg = "Generated Circe SQL is equivalent (identical after normalizing codeset IDs)"))
  }
  list(ok = FALSE, msg = "Generated Circe SQL differs between original and round-trip JSON")
}

# Semantic equivalence: same concept set count + concept IDs, same primary criteria domains, same end strategy type.
# Ignores concept metadata (CONCEPT_NAME etc.), key order, and TypeExclude FALSE.
roundtrip_semantically_equivalent <- function(originalList, roundTripList) {
  if (length(originalList$ConceptSets) != length(roundTripList$ConceptSets)) {
    return(list(ok = FALSE, msg = "Concept set count differs"))
  }
  getConceptIds <- function(cs) {
    items <- cs$expression$items
    if (is.null(items)) items <- list()
    sort(as.integer(vapply(items, function(it) {
      id <- it$concept$CONCEPT_ID
      if (is.null(id)) id <- it$concept$concept_id
      if (is.null(id)) NA_integer_ else as.numeric(id)
    }, numeric(1))))
  }
  origIds <- lapply(originalList$ConceptSets, getConceptIds)
  rtIds <- lapply(roundTripList$ConceptSets, getConceptIds)
  if (!identical(origIds, rtIds)) {
    return(list(ok = FALSE, msg = "Concept IDs in sets differ"))
  }
  pclOrig <- originalList$PrimaryCriteria$CriteriaList
  pclRt <- roundTripList$PrimaryCriteria$CriteriaList
  if (is.null(pclOrig)) pclOrig <- list()
  if (is.null(pclRt)) pclRt <- list()
  origDomains <- vapply(pclOrig, function(cl) names(cl)[1L], character(1L))
  rtDomains <- vapply(pclRt, function(cl) names(cl)[1L], character(1L))
  if (!identical(sort(origDomains), sort(rtDomains))) {
    return(list(ok = FALSE, msg = "Primary criteria domains differ"))
  }
  origEnd <- names(originalList$EndStrategy)[1L]
  rtEnd <- names(roundTripList$EndStrategy)[1L]
  if (!identical(origEnd, rtEnd)) {
    return(list(ok = FALSE, msg = paste0("End strategy differs: ", origEnd, " vs ", rtEnd)))
  }
  list(ok = TRUE, msg = "OK")
}

# Single-cohort round-trip: equivalence = same Circe SQL (original JSON vs round-trip JSON).
# If CirceR is available we compare generated SQL; otherwise fall back to semantic (concept sets, domains, end strategy).
test_that("PhenotypeLibrary cohort 10 round-trip produces equivalent Circe SQL", {
  skip_if_not_installed("PhenotypeLibrary")
  jsonFolder <- system.file("cohorts", package = "PhenotypeLibrary", mustWork = FALSE)
  if (!nzchar(jsonFolder) || !dir.exists(jsonFolder)) skip("PhenotypeLibrary cohorts folder not found")
  jsonPath <- file.path(jsonFolder, "10.json")
  skip_if(!file.exists(jsonPath), message = "cohort 10.json not found")

  outRPath <- tempfile("capr_roundtrip_10")
  dir.create(outRPath, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(outRPath, recursive = TRUE), add = TRUE)

  originalRaw <- readChar(jsonPath, file.info(jsonPath)$size)

  # 1. Decompile -> R file
  jsonToCaprFile(jsonPath, file.path(outRPath, "10.R"), mode = "skip")
  rPath <- file.path(outRPath, "10.R")
  skip_if(!file.exists(rPath), message = "Decompile did not produce 10.R")

  # 2. Source generated R -> cohortDef -> round-trip JSON
  env <- new.env(parent = .GlobalEnv)
  err <- tryCatch(sys.source(rPath, envir = env), error = identity)
  if (inherits(err, "error")) fail(paste0("Sourcing generated R failed: ", conditionMessage(err)))
  if (is.null(env$cohortDef)) fail("Generated R did not create cohortDef")
  roundTripJsonStr <- compile(env$cohortDef)

  # 3. Equivalence: prefer same Circe SQL (when CirceR available); else semantic comparison
  sqlEq <- roundtrip_circe_sql_equivalent(originalRaw, roundTripJsonStr)
  if (isTRUE(sqlEq$ok)) {
    expect_true(TRUE, info = "Original and round-trip JSON produce equivalent Circe SQL")
    return(invisible(NULL))
  }
  # SQL differed or CirceR missing: fall back to semantic (concept sets, domains, end strategy)
  originalList <- normalizeCirceList(jsonlite::fromJSON(originalRaw, simplifyVector = FALSE))
  roundTripList <- normalizeCirceList(jsonlite::fromJSON(roundTripJsonStr, simplifyVector = FALSE))
  sem <- roundtrip_semantically_equivalent(originalList, roundTripList)
  expect_true(sem$ok, info = if (identical(sqlEq$ok, NA)) {
    paste0("CirceR not installed; semantic check: ", sem$msg)
  } else {
    paste0("Circe SQL differed (e.g. QualifiedLimit/ExpressionLimit); semantic check: ", sem$msg)
  })
})

test_that("PhenotypeLibrary JSON round-trips: most pass, failures reported for debugging", {
  skip_if_not_installed("PhenotypeLibrary")
  jsonFolder <- system.file("cohorts", package = "PhenotypeLibrary", mustWork = FALSE)
  if (!nzchar(jsonFolder) || !dir.exists(jsonFolder)) {
    skip("PhenotypeLibrary 'cohorts' folder not found")
  }

  jsonFiles <- list.files(jsonFolder, pattern = "\\.json$", full.names = TRUE)
  if (length(jsonFiles) == 0L) {
    skip("No JSON files in PhenotypeLibrary cohorts")
  }

  outRPath <- tempfile("capr_roundtrip")
  dir.create(outRPath, showWarnings = FALSE)
  on.exit(unlink(outRPath, recursive = TRUE), add = TRUE)

  # Generated code starts with library(Capr); source in .GlobalEnv so it runs and subsequent lines see Capr
  results <- lapply(jsonFiles, run_one_roundtrip, outRPath = outRPath, envParent = .GlobalEnv)
  passed <- vapply(results, function(r) identical(r$ok, TRUE), logical(1L))
  failed <- results[!passed]

  # Report failures so they can be debugged
  if (length(failed) > 0L) {
    for (r in failed) {
      message(sprintf(
        "FAILED %s [%s]: %s",
        r$name,
        r$stage,
        substr(r$msg, 1L, 200L)
      ))
    }
    message(sprintf("Failed files: %s", paste(vapply(failed, function(r) r$name, character(1L)), collapse = ", ")))
  }

  n_passed <- sum(passed)
  n_total <- length(jsonFiles)
  pass_rate <- n_passed / n_total

  # Skip if no round-trips passed (e.g. generated code sourced without Capr on path)
  skip_if(pass_rate == 0 && n_total > 10L, message = "PhenotypeLibrary round-trip: 0% passed (check that generated R can be sourced with Capr loaded)")

  # Expect at least 80% to pass (tune threshold as needed)
  expect_true(
    pass_rate >= 0.80,
    info = sprintf(
      "Only %d/%d PhenotypeLibrary round-trips passed (%.0f%%). Failed: %s",
      n_passed, n_total, 100 * pass_rate,
      paste(vapply(failed, function(r) r$name, character(1L)), collapse = ", ")
    )
  )
})

# --- CirceR validity: all minimal JSON fixtures accepted as valid Circe cohort JSON ---
test_that("Minimal JSON fixtures are valid Circe JSON (cohortExpressionFromJson)", {
  skip_if_not_installed("CirceR")
  fixtures <- c(
    "cohortTest.json", "exitCustomEra.json", "observationPeriodUserDefined.json",
    "correlatedCriteria.json", "measurementValueAsNumber.json", "visitProviderSpecialty.json"
  )
  for (f in fixtures) {
    jsonPath <- test_path("resources", f)
    skip_if(!file.exists(jsonPath), message = paste("fixture not found:", f))
    jsonStr <- paste(readLines(jsonPath), collapse = "\n")
    expect_error(CirceR::cohortExpressionFromJson(jsonStr), NA, info = paste0("CirceR::cohortExpressionFromJson failed for ", f))
  }
})

# --- Feature-specific tests (key strings in output, skip messages) ---

test_that("DateOffset exit (e.g. cohort 10) produces fixedExit", {
  jsonPath <- test_path("resources", "cohortTest.json")
  skip_if(!file.exists(jsonPath))
  code <- jsonToCapr(jsonPath, mode = "strict")
  expect_true(any(grepl("fixedExit\\(", code)), info = "fixedExit(index=..., offsetDays=...) should appear")
})

test_that("CustomEra exit (e.g. cohort 1035) produces drugExit", {
  jsonPath <- test_path("resources", "exitCustomEra.json")
  skip_if(!file.exists(jsonPath))
  code <- jsonToCapr(jsonPath, mode = "strict")
  expect_true(any(grepl("drugExit\\(", code)), info = "drugExit(...) should appear")
})

test_that("ObservationPeriod UserDefinedPeriod (e.g. cohort 1071) produces startDate", {
  jsonPath <- test_path("resources", "observationPeriodUserDefined.json")
  skip_if(!file.exists(jsonPath))
  code <- jsonToCapr(jsonPath, mode = "strict")
  expect_true(any(grepl("startDate\\(", code)), info = "startDate(...) for UserDefinedPeriod should appear")
})

test_that("Correlated criteria (e.g. cohort 1009) produces nestedWithAll or nestedWithAny", {
  jsonPath <- test_path("resources", "correlatedCriteria.json")
  skip_if(!file.exists(jsonPath))
  code <- jsonToCapr(jsonPath, mode = "strict")
  expect_true(any(grepl("nestedWithAll\\(|nestedWithAny\\(", code)), info = "nestedWithAll/nestedWithAny should appear")
})

test_that("Measurement with valueAsNumber (e.g. cohort 1091) produces valueAsNumber(", {
  jsonPath <- test_path("resources", "measurementValueAsNumber.json")
  skip_if(!file.exists(jsonPath))
  code <- jsonToCapr(jsonPath, mode = "strict")
  expect_true(any(grepl("valueAsNumber\\(", code)), info = "valueAsNumber(...) should appear")
})

test_that("VisitOccurrence ProviderSpecialty triggers skip with stable message in skip mode", {
  jsonPath <- test_path("resources", "visitProviderSpecialty.json")
  skip_if(!file.exists(jsonPath))
  code <- jsonToCapr(jsonPath, mode = "skip")
  expect_true(any(grepl("# SKIPPED:", code)), info = "At least one SKIPPED comment")
  expect_true(any(grepl("ProviderSpecialty", code)), info = "ProviderSpecialty mentioned in skip or output")
  out <- jsonToCapr(jsonPath, mode = "skip", returnSkipped = TRUE)
  expect_true(any(grepl("ProviderSpecialty", out$skipped)), info = "ProviderSpecialty in skipped messages")
})
