# Validate all Atlas cohort JSON files from PhenotypeLibrary against the
# Atlas cohort JSON schema (inst/atlas-cohort-schema.json).

test_that("All PhenotypeLibrary cohort JSON files conform to Atlas cohort schema", {
  skip_if_not_installed("PhenotypeLibrary")
  skip_if_not_installed("jsonvalidate")

  jsonFolder <- system.file("cohorts", package = "PhenotypeLibrary", mustWork = FALSE)
  if (!nzchar(jsonFolder) || !dir.exists(jsonFolder)) {
    skip("PhenotypeLibrary 'cohorts' folder not found")
  }

  schemaPath <- system.file("atlas-cohort-schema.json", package = "Capr", mustWork = TRUE)
  expect_true(nzchar(schemaPath), info = "Atlas cohort schema file not found in inst/")

  jsonFiles <- list.files(jsonFolder, pattern = "[.]json$", full.names = TRUE)
  if (length(jsonFiles) == 0L) {
    skip("No JSON files in PhenotypeLibrary cohorts")
  }

  validator <- jsonvalidate::json_validator(schemaPath, engine = "ajv")
  failures <- character(0)

  for (path in jsonFiles) {
    json <- readLines(path, warn = FALSE)
    jsonStr <- paste(json, collapse = "\n")
    ok <- validator(jsonStr)
    if (!isTRUE(ok)) {
      err <- attr(ok, "errors")
      msg <- if (is.data.frame(err) && nrow(err) > 0) {
        paste(utils::capture.output(print(err)), collapse = "; ")
      } else {
        paste(ok, collapse = " ")
      }
      failures <- c(failures, paste0(basename(path), ": ", msg))
    }
  }

  expect(
    length(failures) == 0L,
    paste0(
      length(failures), " of ", length(jsonFiles),
      " PhenotypeLibrary cohort JSON files failed schema validation:\n",
      paste(head(failures, 20), collapse = "\n"),
      if (length(failures) > 20) paste0("\n... and ", length(failures) - 20, " more")
    )
  )
})
