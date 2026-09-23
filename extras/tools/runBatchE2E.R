# Run E2E on a batch of cohort JSONs, write SQL diffs for failures.
# Run from Capr repo root. Requires Capr (load_all), CirceR.
#
# Usage:
#   Rscript extras/tools/runBatchE2E.R [batch_size] [start_index]
# Example: run 100 files starting at 0 (first 100):
#   Rscript extras/tools/runBatchE2E.R 100 0
# Example: next 100 (files 101-200):
#   Rscript extras/tools/runBatchE2E.R 100 100
#
# SQL diffs for failing cohorts are written to extras/tools/diffs/<basename>_diff.sql

args <- commandArgs(trailingOnly = TRUE)
batchSize <- if (length(args) >= 1L) as.integer(args[1]) else 100L
startIdx  <- if (length(args) >= 2L) as.integer(args[2]) else 0L

cohortDir <- path.expand("~/Desktop/AtlasCohortGenerator/inst/cohorts")
if (!dir.exists(cohortDir)) {
  stop("Cohort directory not found: ", cohortDir)
}

# Load Capr and helpers (assume run from repo root)
if (file.exists("DESCRIPTION") && read.dcf("DESCRIPTION", "Package")[1] == "Capr") {
  devtools::load_all(".")
} else if (!("package:Capr" %in% search())) {
  library(Capr, character.only = TRUE)
}
scriptDir <- "extras/tools"
if (!file.exists(file.path(scriptDir, "roundtrip_utils.R"))) {
  stop("Run from Capr repo root so extras/tools/roundtrip_utils.R is found")
}
source(file.path(scriptDir, "roundtrip_utils.R"))
source(file.path(scriptDir, "runE2ETest.R"))

paths <- list.files(cohortDir, pattern = "[.]json$", full.names = TRUE)
paths <- paths[order(basename(paths))]
total <- length(paths)
from <- min(startIdx + 1L, total)
to   <- min(startIdx + batchSize, total)
batch <- paths[seq(from, to)]
if (length(batch) == 0L) {
  message("No files in range ", from, "..", to)
  quit(save = "no", status = 0)
}

message("Batch: files ", from, "..", to, " of ", total, " (", length(batch), " files)")
res <- runE2ETest(batch)
message("Roundtrip ok: ", sum(res$roundtrip_ok), "/", length(batch))
message("JSON ok: ", sum(res$json_ok, na.rm = TRUE), "/", length(batch))
message("SQL ok: ", sum(res$sql_ok, na.rm = TRUE), "/", length(batch))

sqlFailIdx <- which(!res$sql_ok & !is.na(res$sql_ok))
if (length(sqlFailIdx) == 0L) {
  message("No SQL failures in this batch.")
  quit(save = "no", status = 0)
}

message("SQL failures: ", length(sqlFailIdx))
outDir <- file.path(scriptDir, "diffs")
dir.create(outDir, showWarnings = FALSE)
for (i in sqlFailIdx) {
  p <- batch[i]
  name <- sub("[.]json$", "", basename(p))
  outFile <- file.path(outDir, paste0(name, "_diff.sql"))
  diffCirceSql(p, outFile = outFile)
  message("  ", basename(p), " -> ", outFile)
}
message("Diffs written to: ", outDir)
