#!/usr/bin/env Rscript
# Analyze SQL equivalence failures: run round-trip on cohorts, find first failure,
# diff normalized SQL and optionally compare JSON keys.
# Usage: Rscript tools/analyzeSqlFailures.R [n_sample]

devtools::load_all()
if (!("package:Capr" %in% search())) library(Capr, character.only = TRUE)
library(CirceR)

jsonFolder <- system.file("cohorts", package = "PhenotypeLibrary", mustWork = FALSE)
jsonFiles <- list.files(jsonFolder, pattern = "\\.json$", full.names = TRUE)

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

outRPath <- tempfile("rt")
dir.create(outRPath, showWarnings = FALSE)
on.exit(unlink(outRPath, recursive = TRUE), add = TRUE)

run_one <- function(jsonPath) {
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
  sqlEq <- identical(normalize_circe_sql(sqlO), normalize_circe_sql(sqlR))
  list(name = name, orig = orig, rt = rt, sqlO = sqlO, sqlR = sqlR, sqlEq = sqlEq)
}

nSample <- as.integer(commandArgs(TRUE)[1])
if (is.na(nSample) || nSample <= 0) nSample <- 100L
set.seed(42)
idx <- sample(seq_along(jsonFiles), min(nSample, length(jsonFiles)))
message("Running round-trip on ", length(idx), " cohorts...")
results <- lapply(jsonFiles[idx], run_one)
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
  no <- normalize_circe_sql(r$sqlO)
  nr <- normalize_circe_sql(r$sqlR)
  n <- min(nchar(no), nchar(nr))
  charsO <- strsplit(no, "")[[1]]
  charsR <- strsplit(nr, "")[[1]]
  diffs <- which(charsO[1:n] != charsR[1:n])
  if (length(diffs) > 0) {
    i <- diffs[1]
    message("First SQL diff at position ", i, ":")
    message("  Orig: ", substr(no, max(1, i - 35), i + 55))
    message("  Rt  : ", substr(nr, max(1, i - 35), i + 55))
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
