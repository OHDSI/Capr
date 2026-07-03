#' Decompile Atlas/Circe cohort JSON to Capr R code
#'
#' These functions convert a cohort definition exported from Atlas (Circe JSON)
#' into equivalent Capr R code (concept sets and \code{cohort()} calls).
#'
#' @section Supported features:
#' Concept sets (\code{cs()}), primary criteria and entry, additional criteria
#' (\code{withAll}/\code{withAny}), inclusion rules (\code{attrition()}), correlated
#' criteria (\code{nestedWithAll}/\code{nestedWithAny}), start/end windows
#' (\code{duringInterval}), occurrence (\code{exactly}/\code{atLeast}/\code{atMost}),
#' common attributes (age, dates, firstOccurrence, dateAdjustment, source concepts),
#' Measurement/DrugExposure attributes, exit strategies (\code{fixedExit},
#' \code{drugExit}, \code{observationExit}), censoring criteria, and era collapse.
#'
#' @section Unsupported (fail in strict mode or skip with \code{mode = "skip"}):
#' Domains: Specimen, VisitDetail; VisitOccurrence.ProviderSpecialty;
#' any \code{*TypeExclude == TRUE} or \code{*Type} lists (Type lists require vocabulary lookup in Capr);
#'
#' Unknown domain keys are reported via \code{detectUnsupportedKeys()} to avoid silent drift.
#'
#' @param jsonPath Character. Path to the cohort JSON file.
#' @param mode \code{"strict"} (default): stop on unsupported elements.
#'   \code{"skip"}: omit unsupported parts and emit \code{# SKIPPED:} comments (unsafe).
#' @param returnSkipped If \code{TRUE}, return a list with \code{lines}, \code{skipped}, and \code{emptyGroupWarnings} instead of just the character string (for regression/reporting).
#' @return A single character string of R code with newlines between lines (so \code{cat(jsonToCapr(...))} prints nicely), or if \code{returnSkipped = TRUE}, a list with \code{lines}, \code{skipped}, \code{emptyGroupWarnings}. The generated code starts with \code{library(Capr)} so the file is self-contained and can be sourced directly.
#' @seealso \code{\link{cohort}}, \code{\link{cs}}
#' @importFrom rlang %||%
#' @importFrom stats setNames
#' @export
jsonToCapr <- function(jsonPath, mode = c("strict", "skip"), returnSkipped = FALSE) {
  emitter <- makeEmitter(mode)
  stopifnot(file.exists(jsonPath))

  jsonStr <- paste(readLines(jsonPath, warn = FALSE), collapse = "\n")
  validateAtlasCohortJson(jsonStr, jsonPath = jsonPath)
  cohortJson <- jsonlite::fromJSON(jsonStr, simplifyVector = FALSE)

  # -----------------------------
  # ConceptSets
  # -----------------------------
  conceptSetDefs <- lapply(cohortJson$ConceptSets %||% list(), function(conceptSet) {
    conceptSetToCode(conceptSet)
  })

  conceptSetById <- stats::setNames(
    lapply(conceptSetDefs, \(x) x$varName),
    vapply(cohortJson$ConceptSets %||% list(), \(cs) as.character(cs$id), character(1))
  )

  # -----------------------------
  # InclusionRules -> attrition()
  # -----------------------------
  attritionLines <- inclusionRulesToAttritionLines(
    inclusionRules = cohortJson$InclusionRules %||% list(),
    expressionLimit = cohortJson$ExpressionLimit %||% "First",
    conceptSetById = conceptSetById,
    emitter = emitter
  )

  # -----------------------------
  # Entry (PrimaryCriteria)
  # -----------------------------
  primaryCriteria <- cohortJson$PrimaryCriteria
  if (is.null(primaryCriteria)) stop("JSON missing PrimaryCriteria", call. = FALSE)

  primaryCriteriaList <- primaryCriteria$CriteriaList %||% list()
  # Empty CriteriaList is allowed (e.g. placeholder cohort); entry will have no queries.

  # SourceConcept keys that can be a single CodesetId (integer) meaning "any concept" + filter by that concept set
  sourceConceptKeys <- c("ConditionSourceConcept", "DrugSourceConcept", "ProcedureSourceConcept", "ObservationSourceConcept", "VisitSourceConcept", "MeasurementSourceConcept", "VisitDetailSourceConcept")
  primaryQueryCalls <- Filter(
    Negate(is.null),
    lapply(primaryCriteriaList, function(primaryNode) {
      if (length(primaryNode) == 0L || length(names(primaryNode)) == 0L) return(NULL)
      domainKey <- names(primaryNode)[[1]]
      domainVal <- primaryNode[[1]]

      queryFun <- domainKeyToQueryFun(domainKey, emitter)
      if (is.null(queryFun)) return(NULL)

      codesetId <- domainVal$CodesetId %||% domainVal$CodesetID %||% NULL
      # No CodesetId is allowed for Death, ObservationPeriod (any period), or when a SourceConcept attribute references a concept set
      allowNoCodeset <- domainKey %in% c("Death", "ObservationPeriod")
      noMainConceptSet <- is.null(codesetId)
      if (noMainConceptSet) {
        if (allowNoCodeset) {
          # ObservationPeriod() takes no concept set arg; Death() takes conceptSet = NULL
          conceptSetVar <- if (identical(domainKey, "ObservationPeriod")) character(0) else "NULL"
        } else {
          srcId <- NULL
          for (k in sourceConceptKeys) {
            v <- domainVal[[k]]
            if (length(v) == 1L && is.numeric(v) && !is.null(conceptSetById[[as.character(v)]])) {
              srcId <- as.character(v)
              break
            }
          }
          if (!is.null(srcId)) {
            conceptSetVar <- "conceptSet = NULL"
          } else {
            # No CodesetId and no SourceConcept: "any" event (e.g. any condition with date filter)
            conceptSetVar <- "NULL"
          }
        }
      } else {
        conceptSetVar <- conceptSetById[[as.character(codesetId)]]
        if (is.null(conceptSetVar)) return(emitter$skipOrStop(paste0("PrimaryCriteria CodesetId not found in ConceptSets: ", codesetId)))
      }
      # ObservationPeriod() takes no concept set; Atlas JSON may still include CodesetId for the empty set
      if (identical(domainKey, "ObservationPeriod")) conceptSetVar <- character(0)

      attributeCalls <- domainAttributesToCapr(domainKey, domainVal, emitter, jsonContextPath = "PrimaryCriteria", conceptSetById = conceptSetById)

      # CorrelatedCriteria attached to query
      if (!is.null(domainVal$CorrelatedCriteria) && length(domainVal$CorrelatedCriteria) > 0) {
        corr <- correlatedCriteriaToCapr(domainVal$CorrelatedCriteria, conceptSetById, emitter)
        if (!is.null(corr)) attributeCalls <- c(attributeCalls, corr)
      }

      queryArgs <- c(conceptSetVar, attributeCalls)
      sprintf("%s(%s)", queryFun, paste(queryArgs, collapse = ", "))
    })
  )

  if (length(primaryQueryCalls) == 0 && length(primaryCriteriaList) > 0) {
    stop("PrimaryCriteria produced no supported entry queries (strict) or all were skipped (skip).", call. = FALSE)
  }

  observationWindowCode <- observationWindowToCode(primaryCriteria$ObservationWindow %||% list(PriorDays = 0L, PostDays = 0L))
  primaryLimitCode <- limitToCode(primaryCriteria$PrimaryCriteriaLimit %||% "First")
  qualifiedLimitCode <- limitToCode(cohortJson$QualifiedLimit %||% primaryCriteria$PrimaryCriteriaLimit %||% "First")

  additionalCriteriaCall <- additionalCriteriaToCode(
    additionalCriteria = cohortJson$AdditionalCriteria %||% NULL,
    conceptSetById = conceptSetById,
    emitter = emitter
  )

  # -----------------------------
  # Exit
  # -----------------------------
  endStrategyCall <- endStrategyToCapr(cohortJson$EndStrategy %||% NULL, conceptSetById, emitter)
  censoringCall <- censoringCriteriaToCapr(cohortJson$CensoringCriteria %||% list(), conceptSetById, emitter)

  # -----------------------------
  # Era collapse
  # -----------------------------
  eraCall <- "era()"
  if (!is.null(cohortJson$CollapseSettings)) {
    eraCall <- collapseSettingsToEraCall(
      collapseSettings = cohortJson$CollapseSettings,
      censorWindow = cohortJson$CensorWindow %||% NULL,
      emitter = emitter
    )
  }

  # -----------------------------
  # Assemble output
  # -----------------------------
  conceptLines <- c(
    skipHeaderLines(emitter),
    "# --- concept sets ---",
    unlist(lapply(conceptSetDefs, \(x) x$lines)),
    ""
  )

  entryQueryLines <- if (length(primaryQueryCalls) > 0) {
    paste0("    ", paste0(primaryQueryCalls, collapse = ",\n    "), ",")
  } else {
    character(0)
  }
  entryLines <- c(
    "# --- cohort ---",
    "cohortDef <- cohort(",
    "  entry = entry(",
    entryQueryLines,
    sprintf("    observationWindow = %s,", observationWindowCode),
    sprintf("    primaryCriteriaLimit = %s,", primaryLimitCode),
    if (!is.null(additionalCriteriaCall)) sprintf("    additionalCriteria = %s,", additionalCriteriaCall) else NULL,
    sprintf("    qualifiedLimit = %s", qualifiedLimitCode),
    "  ),",
    if (!is.null(attritionLines) && length(attritionLines) > 0) "  attrition = attritionObj," else NULL,
    "  exit = exit(",
    sprintf("    endStrategy = %s%s", endStrategyCall, if (!is.null(censoringCall)) "," else ""),
    if (!is.null(censoringCall)) sprintf("    censor = %s", censoringCall) else NULL,
    "  ),",
    sprintf("  era = %s", eraCall),
    ")",
    ""
  )

  lines <- c(
    "library(Capr)",
    "",
    "# Generated by jsonToCapr()",
    sprintf("# Source JSON: %s", normalizePath(jsonPath, winslash = "/")),
    "",
    conceptLines,
    attritionLines %||% character(),
    entryLines
  )

  if (returnSkipped) {
    return(list(
      lines = lines,
      skipped = emitter$getSkipped(),
      emptyGroupWarnings = emitter$getEmptyGroupWarnings()
    ))
  }
  paste(lines, collapse = "\n")
}

#' Write decompiled Capr R code to a file
#'
#' @param jsonPath Character. Path to the cohort JSON file.
#' @param outRPath Character. Path to the output R file (directory is created if needed).
#' @param mode \code{"strict"} or \code{"skip"} (see \code{\link{jsonToCapr}}).
#' @return Invisibly, \code{outRPath}.
#' @export
jsonToCaprFile <- function(jsonPath, outRPath, mode = c("strict", "skip")) {
  lines <- jsonToCapr(jsonPath, mode = mode)
  outDir <- dirname(outRPath)
  if (nzchar(outDir)) {
    dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  }
  writeLines(lines, outRPath)
  invisible(outRPath)
}

#' Insert decompiled Capr code into the current document (RStudio only)
#'
#' Converts the given cohort JSON to Capr R code and inserts it into the
#' active RStudio editor. The function looks upward from the current cursor
#' position for the previous call to \code{insertCaprCode()} in the file and
#' inserts the generated code on the line directly below that call.
#' For interactive use in RStudio only; requires the \pkg{rstudioapi} package.
#'
#' @param jsonPath Character. Path to the cohort JSON file.
#' @param mode \code{"strict"} or \code{"skip"} (see \code{\link{jsonToCapr}}).
#' @return Invisibly, the inserted Capr code (character string).
#' @seealso \code{\link{jsonToCapr}}, \code{\link{jsonToCaprFile}}
#' @export
insertCaprCode <- function(jsonPath, mode = c("strict", "skip")) {
  rlang::check_installed("rstudioapi", reason = "to insert Capr code into the editor in RStudio")
  if (!interactive()) {
    stop("insertCaprCode() is for interactive use only.", call. = FALSE)
  }
  if (!rstudioapi::isAvailable()) {
    stop("RStudio API is not available. Use insertCaprCode() from within RStudio.", call. = FALSE)
  }
  code <- jsonToCapr(jsonPath, mode = mode)
  context <- rstudioapi::getActiveDocumentContext()
  if (is.null(context) || length(context$contents) == 0L) {
    stop("No active document in RStudio.", call. = FALSE)
  }
  cursorRow <- context$selection[[1L]]$range$end[["row"]]
  contents <- context$contents
  callRows <- which(grepl("insertCaprCode\\s*\\(", contents[seq_len(cursorRow)]))
  if (length(callRows) == 0L) {
    stop("No call to insertCaprCode() found above the cursor in the current document.", call. = FALSE)
  }
  callRow <- callRows[[length(callRows)]]
  line <- contents[[callRow]]
  insertPos <- rstudioapi::document_position(row = callRow, column = nchar(line) + 1L)
  rstudioapi::insertText(insertPos, paste0("\n", code), id = context$id)
  invisible(code)
}

# =============================================================================
# Emitter / Mode
# =============================================================================

makeEmitter <- function(mode = c("strict", "skip")) {
  mode <- match.arg(mode)
  skipped <- character()
  emptyGroupWarnings <- character()

  skipOrStop <- function(msg) {
    if (mode == "strict") stop(msg, call. = FALSE)
    skipped <<- c(skipped, msg)
    return(NULL)
  }

  warnEmptyGroup <- function(context) {
    if (mode == "skip") {
      emptyGroupWarnings <<- c(emptyGroupWarnings, context)
    }
  }

  getSkipped <- function() skipped
  getEmptyGroupWarnings <- function() emptyGroupWarnings
  list(
    mode = mode,
    skipOrStop = skipOrStop,
    warnEmptyGroup = warnEmptyGroup,
    getSkipped = getSkipped,
    getEmptyGroupWarnings = getEmptyGroupWarnings
  )
}

skipHeaderLines <- function(emitter) {
  skipped <- emitter$getSkipped()
  emptyWarns <- emitter$getEmptyGroupWarnings()

  lines <- character()

  if (length(skipped) > 0 || length(emptyWarns) > 0) {
    lines <- c(lines, "# --- SKIPPED / WARNINGS (mode=\"skip\" is UNSAFE) ---")
    if (length(skipped) > 0) lines <- c(lines, paste0("# SKIPPED: ", skipped))
    if (length(emptyWarns) > 0) lines <- c(lines, paste0("# WARNING: group became empty after skips: ", emptyWarns))
    lines <- c(lines, "")
  }

  lines
}

# =============================================================================
# Utilities 
# =============================================================================

# Validate JSON string against Atlas cohort schema; stop with informative error if invalid.
validateAtlasCohortJson <- function(jsonStr, jsonPath = NULL) {
  schemaPath <- system.file("atlas-cohort-schema.json", package = "Capr", mustWork = TRUE)
  result <- jsonvalidate::json_validate(jsonStr, schemaPath, engine = "ajv")
  if (isTRUE(result)) return(invisible(NULL))
  err <- attr(result, "errors")
  msg <- if (is.data.frame(err) && nrow(err) > 0) {
    paste0(
      "JSON does not conform to Atlas cohort schema.",
      if (length(jsonPath)) paste0(" File: ", jsonPath),
      "\nSchema validation errors:\n",
      paste(utils::capture.output(print(err)), collapse = "\n")
    )
  } else {
    paste0(
      "JSON does not conform to Atlas cohort schema.",
      if (length(jsonPath)) paste0(" File: ", jsonPath)
    )
  }
  stop(msg, call. = FALSE)
}

toCamelCase <- function(x) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) return("x")
  x <- x[1]
  x <- gsub("[^A-Za-z0-9]+", " ", x)
  parts <- strsplit(trimws(x), "\\s+")[[1]]
  if (length(parts) == 0 || (length(parts) == 1 && parts == "")) return("x")
  parts <- c(
    tolower(parts[1]),
    paste0(toupper(substring(parts[-1], 1, 1)), tolower(substring(parts[-1], 2)))
  )
  paste0(parts, collapse = "")
}

makeConceptSetVarName <- function(conceptSetName, conceptSetId) {
  base <- toCamelCase(conceptSetName %||% "conceptSet")
  paste0("cs", toupper(substring(base, 1, 1)), substring(base, 2), conceptSetId)
}

parseCirceDate <- function(x) {
  if (is.character(x) && grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return(as.Date(x))
  x
}

formatScalar <- function(x, integersAsNumeric = FALSE) {
  if (inherits(x, "Date")) return(sprintf('as.Date(%s)', deparse(as.character(x))))
  if (is.numeric(x) && is.finite(x)) {
    if (!integersAsNumeric && abs(x - round(x)) < .Machine$double.eps^0.5) {
      return(sprintf("%sL", as.integer(round(x))))
    }
    return(as.character(x))
  }
  if (is.character(x)) return(deparse(x))
  stop("Unsupported scalar type")
}

getConceptId <- function(item) {
  item$CONCEPT_ID %||% item$concept_id %||%
    (if (!is.null(item$concept)) item$concept$CONCEPT_ID %||% item$concept$concept_id else NULL)
}

conceptListToIds <- function(conceptList) {
  if (is.null(conceptList) || length(conceptList) == 0) return(integer())
  if (is.numeric(conceptList)) return(as.integer(conceptList))
  as.integer(vapply(conceptList, function(u) getConceptId(u), numeric(1)))
}

# =============================================================================
# ConceptSets
# =============================================================================

# Build one cs() argument part from a run of items with same (isExcluded, includeDescendants, includeMapped).
# Order of items is preserved by processing items in sequence and grouping consecutive same-signature items.
conceptSetItemsToParts <- function(items) {
  if (is.null(items) || length(items) == 0) return(character(0))
  conceptIds <- vapply(items, function(it) {
    cid <- it$concept$CONCEPT_ID %||% it$concept$concept_id
    if (is.null(cid)) stop("Concept set item missing concept ID", call. = FALSE)
    as.integer(cid)
  }, integer(1))
  isExcluded <- vapply(items, function(it) isTRUE(it$isExcluded), logical(1))
  includeDesc <- vapply(items, function(it) isTRUE(it$includeDescendants), logical(1))
  includeMapped <- vapply(items, function(it) isTRUE(it$includeMapped), logical(1))

  # Group consecutive items with same signature (order-preserving)
  sig <- paste(isExcluded, includeDesc, includeMapped, sep = ".")
  runs <- list()
  i <- 1
  while (i <= length(items)) {
    s <- sig[i]
    j <- i
    while (j < length(items) && sig[j + 1] == s) j <- j + 1
    runs <- c(runs, list(list(ids = conceptIds[i:j], ex = isExcluded[i], desc = includeDesc[i], mapped = includeMapped[i])))
    i <- j + 1
  }

  parts <- vapply(runs, function(r) {
    idsStr <- paste0(r$ids, collapse = ", ")
    if (r$ex) {
      if (r$desc && r$mapped) sprintf("exclude(descendants(mapped(%s)))", idsStr)
      else if (r$desc) sprintf("exclude(descendants(%s))", idsStr)
      else if (r$mapped) sprintf("exclude(mapped(%s))", idsStr)
      else sprintf("exclude(%s)", idsStr)
    } else {
      if (r$desc && r$mapped) sprintf("descendants(mapped(%s))", idsStr)
      else if (r$desc) sprintf("descendants(%s)", idsStr)
      else if (r$mapped) sprintf("mapped(%s)", idsStr)
      else idsStr
    }
  }, character(1))
  parts
}

conceptSetToCode <- function(conceptSet) {
  items <- conceptSet$expression$items
  varName <- makeConceptSetVarName(conceptSet$name, conceptSet$id)

  if (is.null(items) || length(items) == 0) {
    line <- sprintf("%s <- cs(name = %s, id = %s)", varName, deparse(conceptSet$name), conceptSet$id)
    return(list(varName = varName, lines = line))
  }

  parts <- conceptSetItemsToParts(items)
  line <- sprintf(
    "%s <- cs(%s, name = %s, id = %s)",
    varName,
    paste(parts, collapse = ", "),
    deparse(conceptSet$name),
    conceptSet$id
  )

  list(varName = varName, lines = line)
}

# =============================================================================
# Domain mapping
# =============================================================================

domainKeyToQueryFun <- function(domainKey, emitter) {
  switch(
    domainKey,
    ConditionOccurrence = "conditionOccurrence",
    ConditionEra        = "conditionEra",
    DrugExposure        = "drugExposure",
    DrugEra             = "drugEra",
    ProcedureOccurrence = "procedure",
    Measurement         = "measurement",
    VisitOccurrence     = "visit",
    Observation         = "observation",
    Death               = "death",
    DeviceExposure      = "deviceExposure",
    ObservationPeriod   = "observationPeriod",
    DoseEra             = "doseEra",

    Specimen             = "specimen",
    VisitDetail           = "visitDetail",

    { emitter$skipOrStop(paste0("Unsupported domain key: ", domainKey)); NULL }
  )
}

# =============================================================================
# Windows / interval
# =============================================================================

eventWindowToCode <- function(windowJson) {
  sideToOffset <- function(side) {
    coeff <- side$Coeff %||% 1
    days  <- side$Days
    if (is.null(days)) return(if (coeff < 0) "-Inf" else "Inf")
    offset <- as.integer(days) * as.integer(coeff)
    sprintf("%sL", offset)
  }

  startOffset <- sideToOffset(windowJson$Start %||% list(Coeff = -1))
  endOffset   <- sideToOffset(windowJson$End   %||% list(Coeff =  1, Days = 0))

  index <- if (isTRUE(windowJson$UseIndexEnd)) "endDate" else "startDate"
  windowFun <- if (isTRUE(windowJson$UseEventEnd)) "eventEnds" else "eventStarts"

  sprintf("%s(%s, %s, index = %s)", windowFun, startOffset, endOffset, deparse(index))
}

apertureToCode <- function(startWindow, endWindow = NULL, restrictVisit = FALSE, ignoreObservationPeriod = FALSE) {
  startCode <- eventWindowToCode(startWindow %||% list())
  if (is.null(endWindow)) {
    return(sprintf(
      "duringInterval(startWindow = %s, restrictVisit = %s, ignoreObservationPeriod = %s)",
      startCode,
      if (isTRUE(restrictVisit)) "TRUE" else "FALSE",
      if (isTRUE(ignoreObservationPeriod)) "TRUE" else "FALSE"
    ))
  }

  endCode <- eventWindowToCode(endWindow)
  sprintf(
    "duringInterval(startWindow = %s, endWindow = %s, restrictVisit = %s, ignoreObservationPeriod = %s)",
    startCode,
    endCode,
    if (isTRUE(restrictVisit)) "TRUE" else "FALSE",
    if (isTRUE(ignoreObservationPeriod)) "TRUE" else "FALSE"
  )
}

# =============================================================================
# Occurrence
# =============================================================================

occurrenceWrap <- function(occurrence, innerCall) {
  type <- occurrence$Type %||% 2
  count <- as.integer(occurrence$Count %||% 1)
  isDistinct <- occurrence$IsDistinct %||% FALSE
  countColumn <- occurrence$CountColumn %||% ""

  wrapper <- switch(
    as.character(type),
    "0" = "exactly",
    "1" = "atMost",
    "2" = "atLeast",
    stop("Unsupported Occurrence.Type: ", type)
  )

  args <- c(sprintf("%sL", count), innerCall)
  if (isTRUE(isDistinct) || (is.character(countColumn) && nzchar(countColumn))) {
    if (isTRUE(isDistinct)) args <- c(args, "distinct = TRUE")
    if (is.character(countColumn) && nzchar(countColumn)) {
      args <- c(args, sprintf('countColumn = "%s"', countColumn))
    }
  }
  sprintf("%s(%s)", wrapper, paste(args, collapse = ", "))
}

# =============================================================================
# Limits / ObservationWindow
# =============================================================================

observationWindowToCode <- function(observationWindow) {
  priorDays <- observationWindow$PriorDays %||% 0L
  postDays  <- observationWindow$PostDays  %||% 0L
  sprintf("continuousObservation(priorDays = %sL, postDays = %sL)", as.integer(priorDays), as.integer(postDays))
}

limitToCode <- function(limit) {
  if (is.null(limit)) return('"All"')
  if (is.character(limit) && length(limit) == 1) return(deparse(limit))
  if (is.list(limit) && !is.null(limit$Type)) return(deparse(limit$Type))
  stop("Unsupported limit structure.", call. = FALSE)
}

# =============================================================================
# opAttribute + attribute constructors
# =============================================================================

opAttributeToCode <- function(opObj, integersAsNumeric = FALSE) {
  if (is.null(opObj) || is.null(opObj$Op)) return(NULL)

  op <- opObj$Op
  value <- parseCirceDate(opObj$Value)
  extent <- if (!is.null(opObj$Extent)) parseCirceDate(opObj$Extent) else NULL

  if (is.null(value)) stop("Op attribute missing Value", call. = FALSE)

  fmt <- function(z) formatScalar(z, integersAsNumeric = integersAsNumeric)

  if (op %in% c("bt", "BT")) {
    if (is.null(extent)) stop("bt op missing Extent", call. = FALSE)
    return(sprintf("bt(%s, %s)", fmt(value), fmt(extent)))
  }

  if (op %in% c("!bt", "nbt", "NBT")) {
    if (is.null(extent)) stop("nbt (!bt) op missing Extent", call. = FALSE)
    return(sprintf("nbt(%s, %s)", fmt(value), fmt(extent)))
  }

  if (op %in% c("gt", "gte", "lt", "lte", "eq")) {
    return(sprintf("%s(%s)", op, fmt(value)))
  }

  stop("Unsupported op: ", op, call. = FALSE)
}

dateAdjustmentToCapr <- function(dateAdjustment) {
  startWith <- dateAdjustment$StartWith %||% "START_DATE"
  startOffset <- as.integer(dateAdjustment$StartOffset %||% 0L)
  endWith <- dateAdjustment$EndWith %||% "END_DATE"
  endOffset <- as.integer(dateAdjustment$EndOffset %||% 0L)

  sprintf(
    'dateAdjustment(startWith = %s, startOffset = %sL, endWith = %s, endOffset = %sL)',
    deparse(startWith),
    startOffset,
    deparse(endWith),
    endOffset
  )
}

userDefinedPeriodToCapr <- function(userDefinedPeriod) {
  startDateStr <- userDefinedPeriod$StartDate
  endDateStr   <- userDefinedPeriod$EndDate
  if (is.null(startDateStr) || is.null(endDateStr)) stop("UserDefinedPeriod missing StartDate or EndDate", call. = FALSE)

  startDateVal <- as.Date(startDateStr)
  endDateVal   <- as.Date(endDateStr)

  opCode <- if (identical(startDateVal, endDateVal)) {
    sprintf("eq(%s)", formatScalar(startDateVal))
  } else {
    sprintf("bt(%s, %s)", formatScalar(startDateVal), formatScalar(endDateVal))
  }

  sprintf('startDate(%s, type = "occurrence")', opCode)
}

conceptListToInlineConceptSet <- function(concepts, name = "sourceConcepts") {
  ids <- conceptListToIds(concepts)
  if (length(ids) == 0) return(NULL)
  sprintf("cs(%s, name = %s)", paste(ids, collapse = ", "), deparse(name))
}

# Keys we handle or explicitly skip per domain (used for coverage-gap detection).
# Capr ref: R/query.R (domains), R/attributes-*.R (attribute names).
getSupportedKeysForDomain <- function(domainKey) {
  baseKeys <- c(
    "CodesetId", "CodesetID", "CorrelatedCriteria",
    "First", "DateAdjustment", "Age", "Gender",
    "OccurrenceStartDate", "OccurrenceEndDate", "EraStartDate", "EraEndDate",
    "UserDefinedPeriod",
    "ConditionSourceConcept", "DrugSourceConcept", "ProcedureSourceConcept",
    "ObservationSourceConcept", "VisitSourceConcept", "MeasurementSourceConcept",
    "VisitDetailSourceConcept", "VisitType"
  )
  domainExtra <- switch(
    domainKey,
    VisitOccurrence = c("ProviderSpecialty"),
    Measurement     = c("ValueAsNumber", "RangeLow", "RangeHigh", "RangeHighRatio", "Unit", "ValueAsConcept", "MeasurementSourceConcept"),
    Observation     = c("ValueAsNumber", "Unit", "ValueAsConcept", "ValueAsString"),
    DrugExposure    = c("DaysSupply", "Refills", "Quantity"),
    DrugEra         = c("EraLength"),
    DoseEra         = c("Unit", "DoseValue", "EraLength"),
    ConditionEra    = c("OccurrenceCount"),
    VisitDetail     = c("VisitDetailSourceConcept"),
    character(0)
  )
  c(baseKeys, domainExtra)
}

#' Detect domain keys not in the supported set and emit skipOrStop for each (non-null/non-empty).
#' Prevents silent semantic drift when new Circe fields appear.
#'
#' @param domainKey Character. The domain name (e.g. \code{"ConditionOccurrence"}, \code{"Measurement"}).
#' @param domainVal Named list. The domain criterion object from the JSON.
#' @param supportedKeySet Character vector of supported attribute keys for this domain.
#' @param emitter Emitter object from \code{makeEmitter()} (handles \code{skipOrStop}).
#' @param jsonContextPath Character. Path prefix for error messages (e.g. \code{"PrimaryCriteria"}).
#' @export
detectUnsupportedKeys <- function(domainKey, domainVal, supportedKeySet, emitter, jsonContextPath = "") {
  typeLikeKeys <- grep("Type$|TypeExclude$", names(domainVal), value = TRUE)
  knownKeys <- union(supportedKeySet, typeLikeKeys)
  unknownKeys <- setdiff(names(domainVal), knownKeys)
  pathPrefix <- if (nzchar(jsonContextPath)) paste0(jsonContextPath, ".") else ""
  for (k in unknownKeys) {
    v <- domainVal[[k]]
    if (is.null(v)) next
    if (is.list(v) && length(v) == 0) next
    if (is.vector(v) && !is.list(v) && length(v) == 0) next
    emitter$skipOrStop(paste0("Unsupported key: ", pathPrefix, domainKey, ".", k))
  }
}

stopIfTypeExcludeOrTypeLists <- function(domainVal, emitter, jsonContextPath = "") {
  pathPrefix <- if (nzchar(jsonContextPath)) paste0(jsonContextPath, ".") else ""
  excludeKeys <- grep("TypeExclude$", names(domainVal), value = TRUE)
  for (k in excludeKeys) {
    if (isTRUE(domainVal[[k]])) {
      emitter$skipOrStop(paste0("TypeExclude not supported: ", pathPrefix, k, "=TRUE (strict: stop; skip: omit this constraint only)"))
    }
  }

  typeKeys <- grep("Type$", names(domainVal), value = TRUE)
  for (k in typeKeys) {
    if (k == "VisitType") next  # Handled in domainAttributesToCapr via visitTypeSet(conceptSet)
    if (is.list(domainVal[[k]]) && length(domainVal[[k]]) > 0) {
      emitter$skipOrStop(paste0("Type attribute lists require vocabulary lookup: ", pathPrefix, k, " (Capr visitType/measurementType etc. need connection, vocabularyDatabaseSchema)"))
    }
  }
}

domainAttributesToCapr <- function(domainKey, domainVal, emitter, jsonContextPath = "", conceptSetById = NULL) {
  stopIfTypeExcludeOrTypeLists(domainVal, emitter, jsonContextPath)

  attributeCalls <- c()

  # VisitOccurrence.ProviderSpecialty (concept list -> providerSpecialtyConcepts for round-trip)
  if (domainKey == "VisitOccurrence" && !is.null(domainVal$ProviderSpecialty) && length(domainVal$ProviderSpecialty) > 0) {
    ids <- conceptListToIds(domainVal$ProviderSpecialty)
    attributeCalls <- c(attributeCalls, sprintf("providerSpecialtyConcepts(%s)", paste(ids, "L", sep = "", collapse = ", ")))
  }

  # ConditionEra.OccurrenceCount (filter by condition era count, e.g. eq(0) = no eras)
  if (domainKey == "ConditionEra" && !is.null(domainVal$OccurrenceCount)) {
    attributeCalls <- c(attributeCalls, sprintf("occurrenceCount(%s)", opAttributeToCode(domainVal$OccurrenceCount)))
  }

  # DrugEra.EraLength (filter by era length in days)
  if (domainKey == "DrugEra" && !is.null(domainVal$EraLength)) {
    attributeCalls <- c(attributeCalls, sprintf("eraLength(%s)", opAttributeToCode(domainVal$EraLength)))
  }

  # DoseEra: Unit, DoseValue, EraLength
  if (domainKey == "DoseEra") {
    if (!is.null(domainVal[["Unit"]])) {
      csInline <- conceptListToInlineConceptSet(domainVal[["Unit"]], name = "Unit")
      if (!is.null(csInline)) {
        attributeCalls <- c(attributeCalls, sprintf("measurementUnit(%s)", csInline))
      }
    }
    if (!is.null(domainVal$DoseValue)) attributeCalls <- c(attributeCalls, sprintf("doseValue(%s)", opAttributeToCode(domainVal$DoseValue, integersAsNumeric = TRUE)))
    if (!is.null(domainVal$EraLength))  attributeCalls <- c(attributeCalls, sprintf("eraLength(%s)", opAttributeToCode(domainVal$EraLength)))
  }

  # Measurement.RangeHighRatio (filter by value_as_number / range_high ratio; use [[""]] to avoid partial match with RangeHigh)
  if (domainKey == "Measurement" && !is.null(domainVal[["RangeHighRatio"]])) {
    attributeCalls <- c(attributeCalls, sprintf("rangeHighRatio(%s)", opAttributeToCode(domainVal[["RangeHighRatio"]], integersAsNumeric = TRUE)))
  }

  # Logic: First occurrence
  if (isTRUE(domainVal$First %||% FALSE)) {
    attributeCalls <- c(attributeCalls, "firstOccurrence()")
  }

  # ConditionTypeExclude: preserve when FALSE so round-trip JSON matches (CirceR SQL differs if missing)
  if (domainKey == "ConditionOccurrence" && identical(domainVal$ConditionTypeExclude, FALSE)) {
    attributeCalls <- c(attributeCalls, "conditionTypeExclude(FALSE)")
  }
  # DeathTypeExclude: same for Death domain
  if (domainKey == "Death" && identical(domainVal$DeathTypeExclude, FALSE)) {
    attributeCalls <- c(attributeCalls, "deathTypeExclude(FALSE)")
  }
  # MeasurementTypeExclude: same for Measurement domain
  if (domainKey == "Measurement" && identical(domainVal$MeasurementTypeExclude, FALSE)) {
    attributeCalls <- c(attributeCalls, "measurementTypeExclude(FALSE)")
  }
  # SpecimenTypeExclude: same for Specimen domain
  if (domainKey == "Specimen" && identical(domainVal$SpecimenTypeExclude, FALSE)) {
    attributeCalls <- c(attributeCalls, "specimenTypeExclude(FALSE)")
  }

  # DateAdjustment
  if (!is.null(domainVal$DateAdjustment)) {
    attributeCalls <- c(attributeCalls, dateAdjustmentToCapr(domainVal$DateAdjustment))
  }

  # Age (cross-domain)
  if (!is.null(domainVal$Age)) {
    attributeCalls <- c(attributeCalls, sprintf("age(%s)", opAttributeToCode(domainVal$Age)))
  }

  # Gender (demographic on query: male=8507, female=8532)
  if (!is.null(domainVal$Gender) && length(domainVal$Gender) > 0) {
    genderIds <- as.integer(vapply(domainVal$Gender, function(g) getConceptId(g), numeric(1)))
    genderSet <- sort(unique(genderIds))
    if (identical(genderSet, sort(c(8507L, 8532L)))) {
      attributeCalls <- c(attributeCalls, "genderConcepts(8507L, 8532L)")
    } else if (identical(genderSet, 8507L)) {
      attributeCalls <- c(attributeCalls, "male()")
    } else if (identical(genderSet, 8532L)) {
      attributeCalls <- c(attributeCalls, "female()")
    } else {
      emitter$skipOrStop(paste0("Unsupported Gender concept ids: ", paste(genderSet, collapse = ", ")))
    }
  }

  # Date attrs (cross-domain)
  addDateOpAttr <- function(jsonKey, caprAttrFun, type) {
    if (is.null(domainVal[[jsonKey]])) return()
    opCode <- opAttributeToCode(domainVal[[jsonKey]])
    attributeCalls <<- c(attributeCalls, sprintf('%s(%s, type = %s)', caprAttrFun, opCode, deparse(type)))
  }

  if (!is.null(domainVal$OccurrenceStartDate)) addDateOpAttr("OccurrenceStartDate", "startDate", "occurrence")
  if (!is.null(domainVal$OccurrenceEndDate))   addDateOpAttr("OccurrenceEndDate",   "endDate",   "occurrence")
  if (!is.null(domainVal$EraStartDate))        addDateOpAttr("EraStartDate",        "startDate", "era")
  if (!is.null(domainVal$EraEndDate))          addDateOpAttr("EraEndDate",          "endDate",   "era")

  # ObservationPeriod.UserDefinedPeriod
  if (domainKey == "ObservationPeriod" && !is.null(domainVal$UserDefinedPeriod)) {
    attributeCalls <- c(attributeCalls, userDefinedPeriodToCapr(domainVal$UserDefinedPeriod))
  }

  # SourceConcept attributes (Capr conceptSetAttribute constructors)
  # Value can be a single CodesetId (integer) referencing a concept set, or a list of concept objects.
  sourceConceptMap <- list(
    ConditionSourceConcept = "conditionSourceConcept",
    DrugSourceConcept      = "drugSourceConcept",
    ProcedureSourceConcept = "procedureSourceConcept",
    ObservationSourceConcept = "observationSourceConcept",
    VisitSourceConcept     = "visitSourceConcept",
    MeasurementSourceConcept = "measurementSourceConcept",
    VisitDetailSourceConcept = "visitDetailSourceConcept"
  )

  for (jsonKey in names(sourceConceptMap)) {
    val <- domainVal[[jsonKey]]
    if (is.null(val) || length(val) == 0) next
    # Single integer = CodesetId reference (concept set id in cohort JSON)
    if (length(val) == 1L && is.numeric(val) && !is.null(conceptSetById)) {
      csVar <- conceptSetById[[as.character(val)]]
      if (!is.null(csVar)) {
        attributeCalls <- c(attributeCalls, sprintf("%s(%s)", sourceConceptMap[[jsonKey]], csVar))
        next
      }
    }
    csInline <- conceptListToInlineConceptSet(val, name = jsonKey)
    if (!is.null(csInline)) {
      attributeCalls <- c(attributeCalls, sprintf("%s(%s)", sourceConceptMap[[jsonKey]], csInline))
    }
  }

  # VisitType (filter by visit_concept_id): list of concepts or CodesetId
  if (!is.null(domainVal[["VisitType"]]) && length(domainVal[["VisitType"]]) > 0) {
    val <- domainVal[["VisitType"]]
    if (length(val) == 1L && is.numeric(val) && !is.null(conceptSetById)) {
      csVar <- conceptSetById[[as.character(val)]]
      if (!is.null(csVar)) {
        attributeCalls <- c(attributeCalls, sprintf("visitTypeSet(%s)", csVar))
      }
    } else {
      csInline <- conceptListToInlineConceptSet(val, name = "VisitType")
      if (!is.null(csInline)) {
        attributeCalls <- c(attributeCalls, sprintf("visitTypeSet(%s)", csInline))
      }
    }
  }

  # Measurement (use [["key"]] to avoid partial matching e.g. RangeHigh vs RangeHighRatio)
  if (domainKey == "Measurement") {
    if (!is.null(domainVal[["ValueAsNumber"]])) attributeCalls <- c(attributeCalls, sprintf("valueAsNumber(%s)", opAttributeToCode(domainVal[["ValueAsNumber"]], integersAsNumeric = TRUE)))
    if (!is.null(domainVal[["RangeLow"]]))      attributeCalls <- c(attributeCalls, sprintf("rangeLow(%s)", opAttributeToCode(domainVal[["RangeLow"]], integersAsNumeric = TRUE)))
    if (!is.null(domainVal[["RangeHigh"]]))     attributeCalls <- c(attributeCalls, sprintf("rangeHigh(%s)", opAttributeToCode(domainVal[["RangeHigh"]], integersAsNumeric = TRUE)))

    if (!is.null(domainVal[["Unit"]])) {
      csInline <- conceptListToInlineConceptSet(domainVal[["Unit"]], name = "Unit")
      if (!is.null(csInline)) {
        attributeCalls <- c(attributeCalls, sprintf("measurementUnit(%s)", csInline))
      }
    }

    if (!is.null(domainVal[["ValueAsConcept"]])) {
      val <- domainVal[["ValueAsConcept"]]
      if (length(val) == 1L && is.numeric(val) && !is.null(conceptSetById) && !is.null(conceptSetById[[as.character(val)]])) {
        attributeCalls <- c(attributeCalls, sprintf("valueAsConceptSet(%s)", conceptSetById[[as.character(val)]]))
      } else {
        csInline <- conceptListToInlineConceptSet(val, name = "ValueAsConcept")
        if (!is.null(csInline)) attributeCalls <- c(attributeCalls, sprintf("valueAsConceptSet(%s)", csInline))
      }
    }
  }

  # Observation (ValueAsNumber, Unit, ValueAsConcept — same pattern as Measurement)
  if (domainKey == "Observation") {
    if (!is.null(domainVal[["ValueAsNumber"]])) attributeCalls <- c(attributeCalls, sprintf("valueAsNumber(%s)", opAttributeToCode(domainVal[["ValueAsNumber"]], integersAsNumeric = TRUE)))
    if (!is.null(domainVal[["Unit"]])) {
      csInline <- conceptListToInlineConceptSet(domainVal[["Unit"]], name = "Unit")
      if (!is.null(csInline)) {
        attributeCalls <- c(attributeCalls, sprintf("measurementUnit(%s)", csInline))
      }
    }
    if (!is.null(domainVal[["ValueAsConcept"]])) {
      val <- domainVal[["ValueAsConcept"]]
      if (length(val) == 1L && is.numeric(val) && !is.null(conceptSetById) && !is.null(conceptSetById[[as.character(val)]])) {
        attributeCalls <- c(attributeCalls, sprintf("valueAsConceptSet(%s)", conceptSetById[[as.character(val)]]))
      } else {
        csInline <- conceptListToInlineConceptSet(val, name = "ValueAsConcept")
        if (!is.null(csInline)) attributeCalls <- c(attributeCalls, sprintf("valueAsConceptSet(%s)", csInline))
      }
    }
    if (!is.null(domainVal[["ValueAsString"]]) && is.list(domainVal[["ValueAsString"]])) {
      vs <- domainVal[["ValueAsString"]]
      text <- vs$Text %||% vs$text
      op <- vs$Op %||% vs$op %||% "contains"
      if (!is.null(text) && nzchar(text)) {
        attributeCalls <- c(attributeCalls, sprintf('valueAsString(%s, op = %s)', deparse(as.character(text)), deparse(as.character(op))))
      }
    }
  }

  # DrugExposure
  if (domainKey == "DrugExposure") {
    if (!is.null(domainVal$DaysSupply)) attributeCalls <- c(attributeCalls, sprintf("daysOfSupply(%s)", opAttributeToCode(domainVal$DaysSupply)))
    if (!is.null(domainVal$Refills))    attributeCalls <- c(attributeCalls, sprintf("drugRefills(%s)", opAttributeToCode(domainVal$Refills)))
    if (!is.null(domainVal$Quantity))   attributeCalls <- c(attributeCalls, sprintf("drugQuantity(%s)", opAttributeToCode(domainVal$Quantity)))
  }

  supportedKeySet <- getSupportedKeysForDomain(domainKey)
  detectUnsupportedKeys(domainKey, domainVal, supportedKeySet, emitter, jsonContextPath)

  attributeCalls
}

# =============================================================================
# Demographics
# =============================================================================

demographicCriterionToCapr <- function(demo, emitter) {
  calls <- c()

  if (!is.null(demo$Age)) {
    calls <- c(calls, sprintf("age(%s)", opAttributeToCode(demo$Age)))
  }

  if (!is.null(demo$Gender) && length(demo$Gender) > 0) {
    genderIds <- as.integer(vapply(demo$Gender, function(g) getConceptId(g), numeric(1)))
    genderSet <- sort(unique(genderIds))

    # Single criterion with Gender: [8507, 8532] so Circe emits one gender_concept_id in (...) branch
    if (identical(genderSet, sort(c(8507L, 8532L)))) {
      calls <- c(calls, "genderConcepts(8507L, 8532L)")
    } else if (identical(genderSet, 8507L)) {
      calls <- c(calls, "male()")
    } else if (identical(genderSet, 8532L)) {
      calls <- c(calls, "female()")
    } else {
      emitter$skipOrStop(paste0("Unsupported Gender concept ids: ", paste(genderSet, collapse = ", ")))
    }
  }

  # Event (index occurrence) start/end date filter, e.g. from InclusionRules "After 2020"
  if (!is.null(demo$OccurrenceStartDate)) {
    calls <- c(calls, sprintf('startDate(%s, type = "occurrence")', opAttributeToCode(demo$OccurrenceStartDate)))
  }
  if (!is.null(demo$OccurrenceEndDate)) {
    calls <- c(calls, sprintf('endDate(%s, type = "occurrence")', opAttributeToCode(demo$OccurrenceEndDate)))
  }

  unsupported <- setdiff(names(demo), c("Age", "Gender", "OccurrenceStartDate", "OccurrenceEndDate"))
  if (length(unsupported) > 0) {
    emitter$skipOrStop(paste0("Unsupported demographic keys: ", paste(unsupported, collapse = ", ")))
  }

  calls
}

# =============================================================================
# Group compilation (withAll/withAny) + empty-group warnings in skip mode
# =============================================================================

criteriaGroupToCapr <- function(group, conceptSetById, emitter, context = "group") {
  type <- group$Type %||% "ALL"
  count <- as.integer(group$Count %||% 1L)
  if (identical(type, "ANY")) {
    groupFun <- "withAny"
    groupArgs <- NULL
  } else if (identical(type, "AT_LEAST")) {
    groupFun <- "withAtLeast"
    groupArgs <- sprintf("%sL", count)
  } else if (identical(type, "AT_MOST")) {
    groupFun <- "withAtMost"
    groupArgs <- sprintf("%sL", count)
  } else {
    groupFun <- "withAll"
    groupArgs <- NULL
  }

  criteriaList <- group$CriteriaList %||% list()
  criteriaCalls <- Filter(
    Negate(is.null),
    lapply(criteriaList, criterionNodeToCapr, conceptSetById = conceptSetById, emitter = emitter, context = context)
  )

  demoList <- group$DemographicCriteriaList %||% list()
  demoCalls <- unlist(lapply(demoList, demographicCriterionToCapr, emitter = emitter), use.names = FALSE)
  demoCalls <- Filter(Negate(is.null), demoCalls)

  subGroups <- group$Groups %||% list()
  subGroupCalls <- Filter(
    Negate(is.null),
    lapply(seq_along(subGroups), function(i) {
      criteriaGroupToCapr(subGroups[[i]], conceptSetById, emitter, context = paste0(context, "/subgroup#", i))
    })
  )

  args <- c(criteriaCalls, demoCalls, subGroupCalls)
  if (length(args) == 0) {
    emitter$warnEmptyGroup(context)
    return(sprintf("%s()", groupFun))
  }

  if (!is.null(groupArgs)) {
    args <- c(groupArgs, args)
  }
  sprintf("%s(%s)", groupFun, paste(args, collapse = ", "))
}

correlatedCriteriaToCapr <- function(correlatedCriteria, conceptSetById, emitter) {
  type <- correlatedCriteria$Type %||% "ALL"
  count <- as.integer(correlatedCriteria$Count %||% 1L)
  if (identical(type, "ANY")) {
    nestedFun <- "nestedWithAny"
    nestedArgs <- NULL
  } else if (identical(type, "AT_LEAST")) {
    nestedFun <- "nestedWithAtLeast"
    nestedArgs <- sprintf("%sL", count)
  } else if (identical(type, "AT_MOST")) {
    nestedFun <- "nestedWithAtMost"
    nestedArgs <- sprintf("%sL", count)
  } else {
    nestedFun <- "nestedWithAll"
    nestedArgs <- NULL
  }

  criteriaList <- correlatedCriteria$CriteriaList %||% list()
  criteriaCalls <- Filter(
    Negate(is.null),
    lapply(criteriaList, criterionNodeToCapr, conceptSetById = conceptSetById, emitter = emitter, context = "correlatedCriteria")
  )

  demoList <- correlatedCriteria$DemographicCriteriaList %||% list()
  demoCalls <- unlist(lapply(demoList, demographicCriterionToCapr, emitter = emitter), use.names = FALSE)
  demoCalls <- Filter(Negate(is.null), demoCalls)

  subGroups <- correlatedCriteria$Groups %||% list()
  subGroupCalls <- Filter(
    Negate(is.null),
    lapply(seq_along(subGroups), function(i) {
      criteriaGroupToCapr(subGroups[[i]], conceptSetById, emitter, context = paste0("correlatedCriteria/subgroup#", i))
    })
  )

  args <- c(criteriaCalls, demoCalls, subGroupCalls)
  if (length(args) == 0) {
    emitter$warnEmptyGroup("correlatedCriteria")
    return(sprintf("%s()", nestedFun))
  }

  if (!is.null(nestedArgs)) {
    args <- c(nestedArgs, args)
  }
  sprintf("%s(%s)", nestedFun, paste(args, collapse = ", "))
}

# =============================================================================
# Criterion node -> Capr occurrence criterion
# =============================================================================

criterionNodeToCapr <- function(criterionNode, conceptSetById, emitter, context = "criterion") {
  criteriaObj <- criterionNode$Criteria %||% list()
  if (length(criteriaObj) == 0L || length(names(criteriaObj)) == 0L) return(NULL)
  domainKey <- names(criteriaObj)[[1]]
  domainVal <- criteriaObj[[1]]

  queryFun <- domainKeyToQueryFun(domainKey, emitter)
  if (is.null(queryFun)) return(NULL)

  codesetId <- domainVal$CodesetId %||% domainVal$CodesetID %||% NULL
  # Death: no concept set (any death). ObservationPeriod: no concept set (any period). Also allow when only a SourceConcept attribute references a concept set, or "any" event (e.g. inclusion rule with no CodesetId).
  sourceConceptKeysCriterion <- c("ConditionSourceConcept", "DrugSourceConcept", "ProcedureSourceConcept", "ObservationSourceConcept", "VisitSourceConcept", "MeasurementSourceConcept", "VisitDetailSourceConcept")
  allowNoCodeset <- identical(domainKey, "Death") || identical(domainKey, "ObservationPeriod")
  if (is.null(codesetId)) {
    if (!allowNoCodeset) {
      srcId <- NULL
      for (k in sourceConceptKeysCriterion) {
        v <- domainVal[[k]]
        if (length(v) == 1L && is.numeric(v) && !is.null(conceptSetById[[as.character(v)]])) {
          srcId <- as.character(v)
          break
        }
      }
      if (!is.null(srcId)) {
        conceptSetVar <- "conceptSet = NULL"
      } else {
        # No CodesetId and no SourceConcept: "any" event (e.g. inclusion rule "any condition")
        conceptSetVar <- "NULL"
      }
    } else {
      conceptSetVar <- if (identical(domainKey, "ObservationPeriod")) character(0) else "conceptSet = NULL"
    }
  } else {
    conceptSetVar <- conceptSetById[[as.character(codesetId)]]
    if (is.null(conceptSetVar)) {
      emitter$skipOrStop(paste0("CodesetId not found in ConceptSets: ", codesetId, " (", context, ")"))
      return(NULL)
    }
  }
  # ObservationPeriod() takes no concept set; Atlas may include CodesetId for empty set
  if (identical(domainKey, "ObservationPeriod")) conceptSetVar <- character(0)

  attributeCalls <- domainAttributesToCapr(domainKey, domainVal, emitter, jsonContextPath = context, conceptSetById = conceptSetById)

  # CorrelatedCriteria as nested attribute on the query
  if (!is.null(domainVal$CorrelatedCriteria) && length(domainVal$CorrelatedCriteria) > 0) {
    corr <- correlatedCriteriaToCapr(domainVal$CorrelatedCriteria, conceptSetById, emitter)
    if (!is.null(corr)) attributeCalls <- c(attributeCalls, corr)
  }

  startWindow <- criterionNode$StartWindow %||% list()
  endWindow <- criterionNode$EndWindow %||% NULL

  aperture <- apertureToCode(
    startWindow = startWindow,
    endWindow = endWindow,
    restrictVisit = criterionNode$RestrictVisit %||% FALSE,
    ignoreObservationPeriod = criterionNode$IgnoreObservationPeriod %||% FALSE
  )

  queryArgs <- c(conceptSetVar, attributeCalls)
  queryCall <- sprintf("%s(%s)", queryFun, paste(queryArgs, collapse = ", "))
  inner <- sprintf("%s, %s", queryCall, aperture)

  occurrenceWrap(criterionNode$Occurrence %||% list(Type = 2, Count = 1), inner)
}

# =============================================================================
# AdditionalCriteria / InclusionRules
# =============================================================================

additionalCriteriaToCode <- function(additionalCriteria, conceptSetById, emitter) {
  if (is.null(additionalCriteria)) return(NULL)

  hasAny <- length(additionalCriteria$CriteriaList %||% list()) > 0 ||
    length(additionalCriteria$Groups %||% list()) > 0 ||
    length(additionalCriteria$DemographicCriteriaList %||% list()) > 0

  if (!hasAny) {
    # Emit withAll() so round-trip JSON has AdditionalCriteria (Type ALL, empty lists) and Circe produces same SQL (e.g. outcome_death QualifiedLimit ordinal).
    return("withAll()")
  }

  criteriaGroupToCapr(additionalCriteria, conceptSetById, emitter, context = "additionalCriteria")
}

inclusionRulesToAttritionLines <- function(inclusionRules, expressionLimit = "First", conceptSetById, emitter) {
  expressionLimitCode <- limitToCode(expressionLimit)
  limitType <- if (is.list(expressionLimit)) expressionLimit$Type else expressionLimit
  hasRules <- length(inclusionRules %||% list()) > 0L
  if (!hasRules && (is.null(limitType) || identical(limitType, "First"))) return(NULL)
  if (!hasRules) {
    return(c("attritionObj <- attrition(",
      sprintf("  expressionLimit = %s", expressionLimitCode),
      ")", ""))
  }

  lines <- c("attritionObj <- attrition(",
    sprintf("  expressionLimit = %s,", expressionLimitCode))

  for (i in seq_along(inclusionRules)) {
    rule <- inclusionRules[[i]]
    ruleName <- rule$name %||% paste("rule", i)
    expr <- rule$expression %||% list(Type = "ALL")

    groupCall <- criteriaGroupToCapr(expr, conceptSetById, emitter, context = paste0("inclusionRule:", ruleName))

    lines <- c(
      lines,
      sprintf(
        "  %s = %s%s",
        deparse(ruleName),
        groupCall,
        if (i < length(inclusionRules)) "," else ""
      )
    )
  }

  c(lines, ")", "")
}

# =============================================================================
# Exit / EndStrategy / Censoring
# =============================================================================

endStrategyToCapr <- function(endStrategy, conceptSetById, emitter) {
  if (is.null(endStrategy) || length(endStrategy) == 0) {
    return("observationExit()")
  }

  if (!is.null(endStrategy$DateOffset)) {
    dateField <- endStrategy$DateOffset$DateField %||% "EndDate"
    offset <- as.integer(endStrategy$DateOffset$Offset %||% 0L)
    index <- if (identical(dateField, "StartDate")) "startDate" else "endDate"
    return(sprintf('fixedExit(index = %s, offsetDays = %sL)', deparse(index), offset))
  }

  if (!is.null(endStrategy$CustomEra)) {
    drugCodesetId <- endStrategy$CustomEra$DrugCodesetId %||% endStrategy$CustomEra$DrugCodesetID %||% NULL
    gapDays <- as.integer(endStrategy$CustomEra$GapDays %||% 0L)
    offset <- as.integer(endStrategy$CustomEra$Offset %||% 0L)

    if (is.null(drugCodesetId)) {
      emitter$skipOrStop("CustomEra missing DrugCodesetId")
      return("observationExit()")
    }

    conceptSetVar <- conceptSetById[[as.character(drugCodesetId)]]
    if (is.null(conceptSetVar)) {
      emitter$skipOrStop(paste0("CustomEra DrugCodesetId not found in ConceptSets: ", drugCodesetId))
      return("observationExit()")
    }

    return(sprintf(
      "drugExit(%s, persistenceWindow = %sL, surveillanceWindow = %sL)",
      conceptSetVar, gapDays, offset
    ))
  }

  emitter$skipOrStop(paste0("Unsupported EndStrategy structure: ", paste(names(endStrategy), collapse = ", ")))
  "observationExit()"
}

censoringCriteriaToCapr <- function(censoringCriteria, conceptSetById, emitter) {
  if (is.null(censoringCriteria) || length(censoringCriteria) == 0) return(NULL)

  # Each node is either { Criteria: { DomainKey: {...} }, Occurrence?, ... } (Capr/Circe) or
  # directly { DomainKey: {...} } (no Criteria wrapper).
  censorCalls <- Filter(
    Negate(is.null),
    lapply(censoringCriteria, function(node) {
      criteriaObj <- node$Criteria %||% list()
      if (length(criteriaObj) == 0L || length(names(criteriaObj)) == 0L) {
        # Atlas may export censoring items as a single domain object at top level (no Criteria wrapper)
        knownDomains <- c("ConditionOccurrence", "ConditionEra", "DrugExposure", "DrugEra",
                         "ProcedureOccurrence", "Measurement", "VisitOccurrence", "VisitDetail", "Observation",
                         "Death", "DeviceExposure", "ObservationPeriod", "DoseEra", "Specimen")
        if (length(node) > 0L && length(names(node)) > 0L &&
            names(node)[[1]] %in% knownDomains) {
          criteriaObj <- node
        } else {
          return(NULL)
        }
      }
      domainKey <- names(criteriaObj)[[1]]
      domainVal <- criteriaObj[[1]]

      queryFun <- domainKeyToQueryFun(domainKey, emitter)
      if (is.null(queryFun)) return(NULL)

      codesetId <- domainVal$CodesetId %||% domainVal$CodesetID %||% NULL
      if (is.null(codesetId)) {
        if (identical(domainKey, "Death")) {
          conceptSetVar <- "NULL"
        } else {
          return(emitter$skipOrStop(paste0("CensoringCriteria missing CodesetId for domain: ", domainKey)))
        }
      } else {
        conceptSetVar <- conceptSetById[[as.character(codesetId)]]
        if (is.null(conceptSetVar)) return(emitter$skipOrStop(paste0("CensoringCriteria CodesetId not found: ", codesetId)))
      }

      attributeCalls <- domainAttributesToCapr(domainKey, domainVal, emitter, jsonContextPath = "CensoringCriteria", conceptSetById = conceptSetById)
      queryArgs <- c(conceptSetVar, attributeCalls)
      sprintf("%s(%s)", queryFun, paste(queryArgs, collapse = ", "))
    })
  )

  if (length(censorCalls) == 0) {
    emitter$warnEmptyGroup("censoringCriteria")
    return(NULL)
  }

  sprintf("censoringEvents(%s)", paste(censorCalls, collapse = ", "))
}

# =============================================================================
# Era collapse (CollapseSettings)
# =============================================================================

collapseSettingsToEraCall <- function(collapseSettings, censorWindow = NULL, emitter) {
  collapseType <- collapseSettings$CollapseType %||% "ERA"
  if (!identical(collapseType, "ERA")) {
    emitter$skipOrStop(paste0("Unsupported CollapseType: ", collapseType))
    return("era()")
  }

  eraPad <- as.integer(collapseSettings$EraPad %||% 0L)
  studyStartDate <- censorWindow$StartDate %||% NULL
  studyEndDate <- censorWindow$EndDate %||% NULL

  args <- c(sprintf("eraDays = %sL", eraPad))
  if (!is.null(studyStartDate)) args <- c(args, sprintf("studyStartDate = %s", formatScalar(as.Date(studyStartDate))))
  if (!is.null(studyEndDate))   args <- c(args, sprintf("studyEndDate = %s", formatScalar(as.Date(studyEndDate))))

  sprintf("era(%s)", paste(args, collapse = ", "))
}
