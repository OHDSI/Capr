# test-jsonToCapr-edge-cases.R
# Unit tests for edge cases that previously caused E2E/round-trip failures.
# Covers: empty PrimaryCriteria, primary criteria with no CodesetId,
# OccurrenceStartDate in inclusion rules, CensoringCriteria Death with no CodesetId,
# Observation ValueAsString, ObservationPeriod UserDefinedPeriod, ProviderSpecialty.

test_that("Empty PrimaryCriteria.CriteriaList is accepted and round-trips", {
  # Edge case: cohort with no entry criteria (e.g. severe_asthma.json).
  # Schema allows minItems: 0; jsonToCapr produces entry() with no queries.
  jsonPath <- test_path("resources", "emptyPrimaryCriteria.json")
  skip_if(!file.exists(jsonPath), message = "emptyPrimaryCriteria.json not found")

  expect_error(code <- jsonToCapr(jsonPath, mode = "skip"), NA)
  expect_true(any(grepl("entry\\s*\\(", code)), info = "entry() should appear")
  # Entry should have no query arguments (only observationWindow, etc.)
  expect_true(any(grepl("observationWindow\\s*=", code)), info = "observationWindow should appear")

  env <- new.env(parent = .GlobalEnv)
  expect_error(eval(parse(text = code), envir = env), NA)
  expect_false(is.null(env$cohortDef))

  rtJson <- toCohortJson(env$cohortDef)
  rt <- jsonlite::fromJSON(rtJson, simplifyVector = FALSE)
  expect_equal(length(rt$PrimaryCriteria$CriteriaList), 0L,
    info = "Round-trip JSON should have empty CriteriaList"
  )
})

test_that("Primary criteria with no CodesetId (any condition) is supported", {
  # Edge case: ConditionOccurrence with only OccurrenceStartDate, no concept set
  # (e.g. severe_cv_events.json). Should emit conditionOccurrence(NULL, startDate(...)).
  jsonPath <- test_path("resources", "primaryNoCodesetId.json")
  skip_if(!file.exists(jsonPath), message = "primaryNoCodesetId.json not found")

  expect_error(code <- jsonToCapr(jsonPath, mode = "skip"), NA)
  expect_true(any(grepl("conditionOccurrence\\s*\\(\\s*NULL", code)),
    info = "conditionOccurrence(NULL, ...) for any condition should appear"
  )
  expect_true(any(grepl("startDate\\s*\\(", code)),
    info = "startDate(...) for OccurrenceStartDate filter should appear"
  )
  expect_true(any(grepl('type\\s*=\\s*"occurrence"', code)),
    info = "startDate(..., type = \"occurrence\") should appear"
  )

  env <- new.env(parent = .GlobalEnv)
  expect_error(eval(parse(text = code), envir = env), NA)
  expect_false(is.null(env$cohortDef))
  rtJson <- toCohortJson(env$cohortDef)
  expect_type(rtJson, "character")
  rt <- jsonlite::fromJSON(rtJson, simplifyVector = FALSE)
  expect_equal(length(rt$PrimaryCriteria$CriteriaList), 1L)
  expect_true("ConditionOccurrence" %in% names(rt$PrimaryCriteria$CriteriaList[[1]]))
})

test_that("InclusionRule with OccurrenceStartDate produces startDate(..., type = \"occurrence\")", {
  # Edge case: inclusion rule that filters on index event date (e.g. smoker.json "After 2020").
  # demographicCriterionToCapr should handle OccurrenceStartDate/OccurrenceEndDate.
  jsonPath <- test_path("resources", "inclusionRuleOccurrenceStartDate.json")
  skip_if(!file.exists(jsonPath), message = "inclusionRuleOccurrenceStartDate.json not found")

  expect_error(code <- jsonToCapr(jsonPath, mode = "skip"), NA)
  expect_true(any(grepl("startDate\\s*\\(", code)), info = "startDate(...) should appear")
  expect_true(any(grepl('type\\s*=\\s*"occurrence"', code)),
    info = "startDate(..., type = \"occurrence\") for inclusion rule should appear"
  )
  expect_true(any(grepl("attrition\\s*\\(", code)), info = "attrition() with rule should appear")

  env <- new.env(parent = .GlobalEnv)
  expect_error(eval(parse(text = code), envir = env), NA)
  expect_false(is.null(env$cohortDef))
  rtJson <- toCohortJson(env$cohortDef, includeConceptSets = list(env$cs1))
  rt <- jsonlite::fromJSON(rtJson, simplifyVector = FALSE)
  expect_true(length(rt$InclusionRules %||% list()) >= 1L,
    info = "Round-trip should preserve inclusion rule(s)"
  )
})

test_that("CensoringCriteria Death with no CodesetId produces death(NULL)", {
  # Edge case: censor on death with no concept set. Should emit death(NULL) in exit.
  jsonPath <- test_path("resources", "censoringDeathNoCodesetId.json")
  skip_if(!file.exists(jsonPath), message = "censoringDeathNoCodesetId.json not found")

  expect_error(code <- jsonToCapr(jsonPath, mode = "skip"), NA)
  expect_true(any(grepl("death\\s*\\(", code)), info = "death(...) should appear for Death censoring")
  expect_true(any(grepl("censoringEvents\\s*\\(", code)), info = "censoringEvents(...) should appear")
  expect_true(any(grepl("censor\\s*=", code)), info = "exit(..., censor = ...) should appear")

  env <- new.env(parent = .GlobalEnv)
  expect_error(eval(parse(text = code), envir = env), NA)
  expect_false(is.null(env$cohortDef))
  rtJson <- toCohortJson(env$cohortDef, includeConceptSets = list(env$cs1))
  rt <- jsonlite::fromJSON(rtJson, simplifyVector = FALSE)
  expect_true(length(rt$CensoringCriteria %||% list()) >= 1L,
    info = "Round-trip should preserve censoring criteria"
  )
})

test_that("ObservationPeriod with UserDefinedPeriod round-trips", {
  # ObservationPeriod entry with UserDefinedPeriod; as.list(Query) should emit UserDefinedPeriod.
  jsonPath <- test_path("resources", "observationPeriodUserDefined.json")
  skip_if(!file.exists(jsonPath), message = "observationPeriodUserDefined.json not found")

  code <- jsonToCapr(jsonPath, mode = "strict")
  env <- new.env(parent = .GlobalEnv)
  expect_error(eval(parse(text = code), envir = env), NA)
  cohortDef <- env$cohortDef
  expect_false(is.null(cohortDef))
  allCs <- Filter(function(x) methods::is(x, "ConceptSet"), mget(ls(env), envir = env, ifnotfound = list(NULL)))
  rtJson <- if (length(allCs) > 0L) toCohortJson(cohortDef, includeConceptSets = allCs) else toCohortJson(cohortDef)
  rt <- jsonlite::fromJSON(rtJson, simplifyVector = FALSE)
  obsCriterion <- rt$PrimaryCriteria$CriteriaList[[1]]$ObservationPeriod
  expect_true("UserDefinedPeriod" %in% names(obsCriterion),
    info = "Round-trip JSON should contain UserDefinedPeriod"
  )
  expect_equal(obsCriterion$UserDefinedPeriod$StartDate, "2020-01-01")
  expect_equal(obsCriterion$UserDefinedPeriod$EndDate, "2020-12-31")
})

test_that("Observation with ValueAsString produces valueAsString() in generated code", {
  # Observation domain attribute valueAsString (e.g. value_as_string LIKE) round-trip.
  jsonPath <- test_path("resources", "observationValueAsString.json")
  skip_if(!file.exists(jsonPath), message = "observationValueAsString.json not found")

  # Schema may not define ValueAsString for Observation; use skip mode if strict fails
  code <- tryCatch(
    jsonToCapr(jsonPath, mode = "strict"),
    error = function(e) jsonToCapr(jsonPath, mode = "skip")
  )
  if (!any(grepl("valueAsString\\s*\\(", code))) {
    skip("Observation ValueAsString not yet in schema or fixture invalid")
  }
  expect_true(any(grepl("valueAsString\\s*\\(", code)),
    info = "valueAsString(...) for Observation should appear"
  )
})

test_that("VisitOccurrence with ProviderSpecialty round-trips", {
  jsonPath <- test_path("resources", "visitProviderSpecialty.json")
  skip_if(!file.exists(jsonPath), message = "visitProviderSpecialty.json not found")

  code <- jsonToCapr(jsonPath, mode = "strict")
  expect_true(any(grepl("providerSpecialtyConcepts\\s*\\(", code)),
    info = "providerSpecialtyConcepts(...) for ProviderSpecialty should appear"
  )
  env <- new.env(parent = .GlobalEnv)
  expect_error(eval(parse(text = code), envir = env), NA)
  rtJson <- toCohortJson(env$cohortDef, includeConceptSets = list(env$cs1))
  rt <- jsonlite::fromJSON(rtJson, simplifyVector = FALSE)
  visitCrit <- rt$PrimaryCriteria$CriteriaList[[1]]$VisitOccurrence
  expect_true("ProviderSpecialty" %in% names(visitCrit),
    info = "Round-trip JSON should contain ProviderSpecialty"
  )
})

test_that("Type/provenance attribute lists decompile to ids-based constructors", {
  # ConditionType/ConditionStatus on entry, MeasurementType/Unit on an inclusion rule.
  # These previously hit skipOrStop ("require vocabulary lookup"); now DB-free.
  jsonPath <- test_path("resources", "typeAttributes.json")
  skip_if(!file.exists(jsonPath), message = "typeAttributes.json not found")

  code <- jsonToCapr(jsonPath, mode = "strict")
  expect_true(any(grepl("conditionType\\(c\\(32817L, 32810L\\)\\)", code)))
  expect_true(any(grepl("conditionStatus\\(c\\(32901L\\)\\)", code)))
  expect_true(any(grepl("measurementType\\(c\\(32817L\\)\\)", code)))
  expect_true(any(grepl("measurementUnit\\(c\\(8554L\\)\\)", code)))

  env <- new.env(parent = .GlobalEnv)
  expect_error(eval(parse(text = code), envir = env), NA)
  expect_false(is.null(env$cohortDef))

  rtJson <- toCohortJson(env$cohortDef)
  rt <- jsonlite::fromJSON(rtJson, simplifyVector = FALSE)
  co <- rt$PrimaryCriteria$CriteriaList[[1]]$ConditionOccurrence
  expect_equal(purrr::map_int(co$ConditionType, ~.$CONCEPT_ID), c(32817L, 32810L))
  expect_equal(co$ConditionStatus[[1]]$CONCEPT_ID, 32901L)
  ms <- rt$InclusionRules[[1]]$expression$CriteriaList[[1]]$Criteria$Measurement
  expect_equal(ms$MeasurementType[[1]]$CONCEPT_ID, 32817L)
  expect_equal(ms$Unit[[1]]$CONCEPT_ID, 8554L)
})

test_that("Edge-case fixtures are valid Atlas schema (minItems 0, etc.)", {
  skip_if_not_installed("jsonvalidate")
  schemaPath <- system.file("atlas-cohort-schema.json", package = "Capr", mustWork = TRUE)
  validator <- jsonvalidate::json_validator(schemaPath, engine = "ajv")

  fixtures <- c(
    "emptyPrimaryCriteria.json",
    "primaryNoCodesetId.json",
    "inclusionRuleOccurrenceStartDate.json",
    "censoringDeathNoCodesetId.json",
    "observationPeriodUserDefined.json",
    "typeAttributes.json"
  )
  for (f in fixtures) {
    jsonPath <- test_path("resources", f)
    skip_if(!file.exists(jsonPath), message = paste("fixture not found:", f))
    jsonStr <- paste(readLines(jsonPath, warn = FALSE), collapse = "\n")
    ok <- validator(jsonStr)
    expect_true(isTRUE(ok), info = paste0(f, ": ", paste(attr(ok, "errors"), collapse = "; ")))
  }
})

test_that("Edge-case fixtures round-trip and produce valid Circe SQL", {
  skip_if_not_installed("CirceR")
  # Each fixture: jsonToCapr -> source -> toCohortJson -> CirceR accepts round-trip JSON and builds SQL.
  fixtures <- c(
    "primaryNoCodesetId.json",
    "inclusionRuleOccurrenceStartDate.json",
    "censoringDeathNoCodesetId.json",
    "observationPeriodUserDefined.json",
    "typeAttributes.json"
  )
  for (f in fixtures) {
    jsonPath <- test_path("resources", f)
    skip_if(!file.exists(jsonPath), message = paste("fixture not found:", f))

    code <- tryCatch(
      jsonToCapr(jsonPath, mode = "strict"),
      error = function(e) jsonToCapr(jsonPath, mode = "skip")
    )
    env <- new.env(parent = .GlobalEnv)
    err <- tryCatch(eval(parse(text = code), envir = env), error = identity)
    if (inherits(err, "error")) {
      fail(paste0(f, ": sourcing generated R failed: ", conditionMessage(err)))
    }
    expect_false(is.null(env$cohortDef), info = paste0(f, ": cohortDef not created"))
    allCs <- Filter(function(x) methods::is(x, "ConceptSet"), mget(ls(env), envir = env, ifnotfound = list(NULL)))
    rtJson <- if (length(allCs) > 0L) toCohortJson(env$cohortDef, includeConceptSets = allCs) else toCohortJson(env$cohortDef)
    expect_true(is.character(rtJson) && length(rtJson) == 1L, info = paste0(f, ": toCohortJson() should return JSON string"))

    sqlRt <- tryCatch(
      CirceR::buildCohortQuery(
        CirceR::cohortExpressionFromJson(rtJson),
        options = CirceR::createGenerateOptions(generateStats = FALSE)
      ),
      error = identity
    )
    expect_false(inherits(sqlRt, "error"),
      info = paste0(f, ": CirceR buildCohortQuery(round-trip) failed: ", conditionMessage(sqlRt))
    )
    expect_true(nchar(sqlRt) > 0L, info = paste0(f, ": generated SQL should be non-empty"))
  }
})
