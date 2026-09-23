# Shared utilities for round-trip and SQL equivalence tooling in extras/tools.
# Used by: analyzeOneFailure.R, analyzeSqlFailures.R, runRoundtripTest.R

`%||%` <- function(x, y) if (is.null(x)) y else x

# Sort concept_id in (id1,id2,...) and value_as_concept_id in (...) so order doesn't affect equivalence.
sortConceptIdInLists <- function(s) {
  sortIdsInPattern <- function(s, pattern, prefix) {
    m <- gregexpr(pattern, s)[[1]]
    if (m[1] < 0) return(s)
    starts <- as.integer(m)
    lens <- attr(m, "match.length")
    matches <- substring(s, starts, starts + lens - 1)
    inners <- sub(pattern, "\\1", matches)
    replacements <- vapply(inners, function(inner) {
      nums <- sort(as.integer(strsplit(inner, ",", fixed = TRUE)[[1]]))
      paste0(prefix, "(", paste(nums, collapse = ","), ")")
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
  s <- sortIdsInPattern(s, "concept_id in \\(([0-9,]+)\\)", "concept_id in ")
  s <- sortIdsInPattern(s, "value_as_concept_id in \\(([0-9,]+)\\)", "value_as_concept_id in ")
  s
}

normalizeCirceSql <- function(s) {
  s <- trimws(gsub("[ \t\r\n]+", " ", s))
  s <- gsub("@codeset_[0-9]+", "@codeset_X", s)
  s <- gsub("[0-9]+ as codeset_id", "X as codeset_id", s)
  s <- gsub("codeset_id = [0-9]+", "codeset_id = X", s)
  s <- sortConceptIdInLists(s)
  s
}

# Fingerprint a concept set for matching (ignore id, name, concept metadata).
conceptSetFingerprint <- function(cs) {
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
# If preserveOriginalIds is TRUE (default), concept set id and all CodesetId references use the
# original JSON's IDs (e.g. 4, 14, 15) so the output matches the original numbering.
reorderRoundtripConceptSetsToMatchOriginal <- function(originalJsonStr, roundTripJsonStr, preserveOriginalIds = TRUE) {
  orig <- jsonlite::fromJSON(originalJsonStr, simplifyVector = FALSE)
  rt <- jsonlite::fromJSON(roundTripJsonStr, simplifyVector = FALSE)
  origSets <- orig$ConceptSets %||% list()
  rtSets <- rt$ConceptSets %||% list()
  if (length(origSets) == 0 || length(rtSets) == 0) return(roundTripJsonStr)
  origFp <- lapply(origSets, conceptSetFingerprint)
  rtFp <- lapply(rtSets, conceptSetFingerprint)
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
  origIds <- vapply(seq_along(origSets), function(k) as.integer(origSets[[k]]$id), integer(1))
  if (preserveOriginalIds) {
    for (k in seq_along(reordered)) reordered[[k]]$id <- origIds[k]
    idMap <- stats::setNames(origIds, as.character(oldIds))
  } else {
    for (k in seq_along(reordered)) reordered[[k]]$id <- k - 1L
    idMap <- stats::setNames(seq_along(reordered) - 1L, as.character(oldIds))
  }
  rt$ConceptSets <- reordered
  codesetIdKeys <- c(
    "CodesetId", "CodesetID", "DrugCodesetId",
    "ObservationSourceConcept", "VisitSourceConcept", "ConditionSourceConcept",
    "DrugSourceConcept", "ProcedureSourceConcept", "MeasurementSourceConcept",
    "VisitDetailSourceConcept"
  )
  replaceCodesetIds <- function(x, map) {
    if (is.null(x)) return(x)
    if (!is.list(x)) return(x)
    for (key in codesetIdKeys) {
      if (!is.null(x[[key]]) && is.numeric(x[[key]]) && length(x[[key]]) == 1L) {
        m <- map[as.character(x[[key]])]
        if (length(m) > 0L && !is.na(m[1L])) x[[key]] <- m[1L]
      }
    }
    x[] <- lapply(x, replaceCodesetIds, map = map)
    x
  }
  rt <- replaceCodesetIds(rt, idMap)
  as.character(jsonlite::toJSON(rt, auto_unbox = TRUE))
}

# Return list with position and surrounding context for first character diff between two normalized SQL strings.
firstSqlDiff <- function(normSqlOrig, normSqlRt, contextChars = 45L) {
  n <- min(nchar(normSqlOrig), nchar(normSqlRt))
  charsO <- strsplit(normSqlOrig, "")[[1]]
  charsR <- strsplit(normSqlRt, "")[[1]]
  diffs <- which(charsO[seq_len(n)] != charsR[seq_len(n)])
  if (length(diffs) == 0L) return(list(position = NA_integer_, orig = "", rt = ""))
  i <- diffs[1L]
  list(
    position = i,
    orig = substr(normSqlOrig, max(1L, i - contextChars), i + 55L),
    rt = substr(normSqlRt, max(1L, i - contextChars), i + 55L)
  )
}
