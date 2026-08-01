#!/usr/bin/env Rscript
# Run decompile regression: finds inst/cohorts/*.json (or *.json recursively) in the given path,
# runs jsonToCapr(..., mode = "skip", returnSkipped = TRUE), writes CSV summary and top-10 skip report.
# Usage: Rscript tools/runDecompileRegression.R [path]
#   path = PhenotypeLibrary package root or any dir with cohort JSON; default = system.file(package="PhenotypeLibrary")
# From R with Capr loaded: source("tools/runDecompileRegression.R"); runDecompileRegression("/path/to/PhenotypeLibrary")

#' Run decompile regression over a set of cohort JSON files
#'
#' Finds all \code{inst/cohorts/*.json} in a provided PhenotypeLibrary folder (or all
#' \code{*.json} under a given directory), runs \code{jsonToCapr(..., mode = "skip", returnSkipped = TRUE)},
#' and captures success/failure, skipped messages, and empty-group warnings.
#' Writes a CSV summary and prints a top-10 skip reason report.
#'
#' @param phenotypeLibraryPath Character. Path to the PhenotypeLibrary package root
#'   (containing \code{inst/cohorts/}) or any directory tree containing \code{*.json} cohort files.
#' @param outCsv Character. Path to the output CSV summary (default: \code{decompile_regression_summary.csv} in the current directory).
#' @param outDir Character. Directory to write decompiled R files (default: temporary directory).
#' @return Invisibly, a data frame with columns: \code{cohort}, \code{success}, \code{errorMsg},
#'   \code{nSkipped}, \code{nEmptyGroupWarnings}, \code{skippedSample}, \code{emptyGroupSample}.
#'   Also writes \code{outCsv} and prints top-10 skip reasons.
runDecompileRegression <- function(phenotypeLibraryPath,
                                   outCsv = "decompile_regression_summary.csv",
                                   outDir = tempfile("capr_decompile_regression")) {
  cohortDir <- file.path(phenotypeLibraryPath, "inst", "cohorts")
  if (!dir.exists(cohortDir)) {
    jsonFiles <- list.files(phenotypeLibraryPath, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  } else {
    jsonFiles <- list.files(cohortDir, pattern = "\\.json$", full.names = TRUE)
  }

  if (length(jsonFiles) == 0L) {
    message("No JSON files found under ", phenotypeLibraryPath)
    return(invisible(data.frame()))
  }

  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

  allSkipped <- character()

  results <- lapply(jsonFiles, function(jsonPath) {
    cohortName <- sub("\\.json$", "", basename(jsonPath))
    outRPath <- file.path(outDir, paste0(cohortName, ".R"))

    out <- tryCatch(
      Capr::jsonToCapr(jsonPath, mode = "skip", returnSkipped = TRUE),
      error = function(e) {
        list(
          lines = character(),
          skipped = character(),
          emptyGroupWarnings = character(),
          error = conditionMessage(e)
        )
      }
    )

    if (!is.null(out$error)) {
      return(list(
        cohort = cohortName,
        success = FALSE,
        errorMsg = out$error,
        nSkipped = 0L,
        nEmptyGroupWarnings = 0L,
        skippedSample = "",
        emptyGroupSample = "",
        skippedVec = character()
      ))
    }

    writeLines(out$lines, outRPath)
    skipped <- out$skipped
    emptyWarns <- out$emptyGroupWarnings
    allSkipped <<- c(allSkipped, skipped)

    list(
      cohort = cohortName,
      success = TRUE,
      errorMsg = NA_character_,
      nSkipped = length(skipped),
      nEmptyGroupWarnings = length(emptyWarns),
      skippedSample = paste(head(skipped, 3L), collapse = " | "),
      emptyGroupSample = paste(head(emptyWarns, 2L), collapse = " | "),
      skippedVec = skipped
    )
  })

  df <- do.call(rbind, lapply(results, function(r) {
    data.frame(
      cohort = r$cohort,
      success = r$success,
      errorMsg = r$errorMsg,
      nSkipped = r$nSkipped,
      nEmptyGroupWarnings = r$nEmptyGroupWarnings,
      skippedSample = r$skippedSample,
      emptyGroupSample = r$emptyGroupSample,
      stringsAsFactors = FALSE
    )
  }))

  # Top-10 skip reasons from collected allSkipped

  skipTable <- if (length(allSkipped) > 0L) {
    sort(table(allSkipped), decreasing = TRUE)
  } else {
    structure(integer(0), .Dim = 0L, .Dimnames = list(character(0)), class = "table")
  }

  write.csv(df, outCsv, row.names = FALSE)
  message("Summary written to ", outCsv, " (", nrow(df), " cohorts)")

  message("\n--- Top 10 skip reasons (count) ---")
  top10 <- head(skipTable, 10L)
  if (length(top10) > 0L) {
    for (i in seq_along(top10)) {
      message(sprintf("  %d. [%s] %s", i, top10[i], names(top10)[i]))
    }
  } else {
    message("  (none)")
  }

  invisible(df)
}

# When run via Rscript: load Capr and run with first argument as path
if (identical(Sys.getenv("R_TESTS"), "") && length(commandArgs(trailingOnly = TRUE)) > 0L) {
  if (!requireNamespace("Capr", quietly = TRUE)) {
    if (file.exists("DESCRIPTION")) devtools::load_all()
    else stop("Load Capr first (install or devtools::load_all from Capr root)")
  }
  path <- commandArgs(trailingOnly = TRUE)[1L]
  if (!dir.exists(path)) stop("Directory not found: ", path)
  runDecompileRegression(path)
}
