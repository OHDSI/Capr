# R/print.R
# Human-readable console printing (show) and summarising (summary) for Capr S4 classes.
#
# Design (see hidden/plan-caprPrintSummary.prompt.md):
#  - show()    : compact, scannable, 1-3 lines per class. cli rule for the Cohort header.
#  - summary() : fuller nested view, still scannable.
#  - fmt_*()   : pure one-line renderers shared by both show and summary.

# ---- one-line renderers ------------------------------------------------------

plural <- function(n, word, plural_word = NULL) {
  if (length(n) == 0L || is.na(n) || n == 1L) {
    word
  } else {
    if (is.null(plural_word)) paste0(word, "s") else plural_word
  }
}

fmt_endpoint <- function(x) {
  d <- x@days
  c <- x@coeff
  if (is.infinite(d)) return(if (c < 0) "-Inf" else "Inf")
  n <- as.integer(d)
  if (n == 0L) return("0")
  paste0(if (c < 0) "-" else "+", n)
}

fmt_eventwindow <- function(x) {
  fn <- if (identical(x@event, "start")) "eventStarts" else "eventEnds"
  glue::glue("{fn}({fmt_endpoint(x@start)}, {fmt_endpoint(x@end)})")
}

is_empty_aperture <- function(x) {
  is.na(x@startWindow@event) && is.na(x@endWindow@event)
}

fmt_aperture <- function(x) {
  if (is_empty_aperture(x)) return("all time")
  args <- fmt_eventwindow(x@startWindow)
  if (!is.na(x@endWindow@event)) {
    args <- paste0(args, ", endWindow = ", fmt_eventwindow(x@endWindow))
  }
  if (isTRUE(x@restrictVisit)) args <- paste0(args, ", restrictVisit = TRUE")
  if (isTRUE(x@ignoreObservationPeriod)) args <- paste0(args, ", ignoreObservationPeriod = TRUE")
  glue::glue("duringInterval({args})")
}

fmt_observationwindow <- function(x) {
  glue::glue("obsWindow({x@priorDays}d, {x@postDays}d)")
}

fmt_occurrence <- function(x) {
  t <- x@type
  if (is.na(t)) return("occurrence")
  base <- if (t %in% c("all", "any")) t else paste0(t, " ", x@count)
  if (isTRUE(x@isDistinct)) {
    base <- paste0(base, " (distinct by ", x@countColumn, ")")
  }
  base
}

fmt_conceptset <- function(x) {
  nm <- x@Name
  nm <- if (is.na(nm) || nm == "") "<unnamed>" else nm
  n <- length(x@Expression)
  glue::glue("\"{nm}\" ({n} {plural(n, \"concept\")})")
}

fmt_attribute_names <- function(x) {
  if (length(x) == 0L) return("")
  nms <- vapply(x, function(a) {
    s <- tryCatch(as.character(a@name), error = function(e) NA_character_)
    if (length(s) == 0L || is.na(s)) class(a)[1] else s
  }, character(1))
  paste(nms, collapse = ", ")
}

fmt_query <- function(x) {
  txt <- glue::glue("{x@domain} {fmt_conceptset(x@conceptSet)}")
  if (length(x@attributes) > 0L) {
    txt <- paste0(txt, " [", fmt_attribute_names(x@attributes), "]")
  }
  txt
}

fmt_criteria <- function(x) {
  glue::glue("{fmt_occurrence(x@occurrence)} \u00d7 {fmt_query(x@query)} [{fmt_aperture(x@aperture)}]")
}

group_op <- function(x) {
  t <- x@occurrence@type
  if (is.na(t)) t <- "all"
  switch(t,
    all = "withAll",
    any = "withAny",
    atLeast = paste0("withAtLeast(", x@occurrence@count, ")"),
    atMost = paste0("withAtMost(", x@occurrence@count, ")"),
    t)
}

fmt_group <- function(x) {
  nc <- length(x@criteria)
  ng <- length(x@group)
  glue::glue("{group_op(x)} ({nc} {plural(nc, \"criterion\", \"criteria\")}, ",
             "{ng} {plural(ng, \"sub-group\")})")
}

fmt_endstrategy <- function(x) {
  cls <- class(x)[1]
  if (cls == "ObservationExit") {
    return("observationExit (end of continuous observation)")
  }
  if (cls == "FixedDurationExit") {
    return(glue::glue("fixedExit({x@index} + {x@offsetDays}d)"))
  }
  if (cls == "DrugExposureExit") {
    cs <- x@conceptSet@Name
    cs <- if (is.na(cs) || cs == "") "<unnamed>" else cs
    return(glue::glue("drugExit(\"{cs}\", persist {x@persistenceWindow}d, surveil {x@surveillanceWindow}d)"))
  }
  cls
}

fmt_censoring <- function(x) {
  glue::glue("{length(x@criteria)} censoring {plural(length(x@criteria), \"event\")}")
}

fmt_entry <- function(x) {
  n <- length(x@entryEvents)
  txt <- glue::glue("{n} {plural(n, \"event\")}, limit = {x@primaryCriteriaLimit}, ",
                    "{fmt_observationwindow(x@observationWindow)}")
  if (length(x@additionalCriteria@criteria) + length(x@additionalCriteria@group) > 0L) {
    txt <- paste0(txt, ", additional: ", fmt_group(x@additionalCriteria))
  }
  txt
}

fmt_attrition <- function(x) {
  n <- length(x@rules)
  glue::glue("{n} {plural(n, \"rule\")}, expressionLimit = {x@expressionLimit}")
}

fmt_exit <- function(x) {
  txt <- fmt_endstrategy(x@endStrategy)
  if (length(x@censoringCriteria@criteria) > 0L) {
    txt <- paste0(txt, ", censor: ", fmt_censoring(x@censoringCriteria))
  }
  txt
}

fmt_era <- function(x) {
  txt <- glue::glue("gap {x@eraDays}d")
  if (!is.na(x@studyStartDate)) txt <- paste0(txt, ", ", format(x@studyStartDate, "%Y-%m-%d"))
  if (!is.na(x@studyEndDate)) txt <- paste0(txt, " \u2192 ", format(x@studyEndDate, "%Y-%m-%d"))
  txt
}

# ---- describe_capr(): canonical one-line description of any Capr object ------
# Inheritance-aware dispatch (most specific first). Used by show() and summary().

describe_capr <- function(x) {
  if (methods::is(x, "Cohort")) {
    return(glue::glue("<Capr Cohort> entry: {fmt_entry(x@entry)}; ",
                      "attrition: {fmt_attrition(x@attrition)}; ",
                      "exit: {fmt_exit(x@exit)}; era: {fmt_era(x@era)}"))
  }
  if (methods::is(x, "CohortEntry")) return(fmt_entry(x))
  if (methods::is(x, "CohortAttrition")) return(fmt_attrition(x))
  if (methods::is(x, "CohortExit")) return(fmt_exit(x))
  if (methods::is(x, "CohortEra")) return(fmt_era(x))
  if (methods::is(x, "Query")) return(fmt_query(x))
  if (methods::is(x, "Criteria")) return(fmt_criteria(x))
  if (methods::is(x, "Group")) return(fmt_group(x))
  if (methods::is(x, "Occurrence")) return(fmt_occurrence(x))
  if (methods::is(x, "ObservationWindow")) return(fmt_observationwindow(x))
  if (methods::is(x, "EventAperture")) return(fmt_aperture(x))
  if (methods::is(x, "EventWindow")) return(fmt_eventwindow(x))
  if (methods::is(x, "Endpoint")) return(fmt_endpoint(x))
  if (methods::is(x, "ObservationExit")) return(fmt_endstrategy(x))
  if (methods::is(x, "FixedDurationExit")) return(fmt_endstrategy(x))
  if (methods::is(x, "DrugExposureExit")) return(fmt_endstrategy(x))
  if (methods::is(x, "CensoringCriteria")) return(fmt_censoring(x))
  if (methods::is(x, "ConceptSet")) return(glue::glue("ConceptSet {fmt_conceptset(x)}"))
  if (methods::is(x, "conceptSetSelectionAttribute")) {
    return(glue::glue("{x@name}: {fmt_conceptset(x@conceptSet)} (excl = {x@isExclusion})"))
  }
  if (methods::is(x, "conceptSetAttribute")) {
    return(glue::glue("{x@name}: {fmt_conceptset(x@conceptSet)}"))
  }
  if (methods::is(x, "conceptAttribute")) {
    return(glue::glue("{x@name}: {length(x@conceptSet)} {plural(length(x@conceptSet), \"concept\")}"))
  }
  if (methods::is(x, "opAttributeSuper")) {
    return(glue::glue("{x@name} {x@op} {x@value}"))
  }
  if (methods::is(x, "nestedAttribute")) {
    return(glue::glue("nested ({x@name}): {fmt_group(x@group)}"))
  }
  if (methods::is(x, "logicAttribute")) return(x@name)
  if (methods::is(x, "keyValueAttribute")) return(glue::glue("{x@key} = {x@value}"))
  if (methods::is(x, "dateAdjustmentAttribute")) {
    return(glue::glue("dateAdjustment(start = {x@startWith}{x@startOffset}, ",
                      "end = {x@endWith}{x@endOffset})"))
  }
  class(x)[1]
}

# ---- helpers for expanding nested groups in summary --------------------------

# Print the nested (correlated criteria) attributes on a Query, indented.
print_nested_attributes <- function(query, indent = "  ") {
  if (!methods::is(query, "Query")) return(invisible(NULL))
  for (a in query@attributes) {
    if (methods::is(a, "nestedAttribute")) {
      cat(indent, "nested (", a@name, "):\n", sep = "")
      print_group(a@group, indent = paste0(indent, "  "))
    }
  }
  invisible(NULL)
}

# Recursively print a Group (criteria + sub-groups), indented.
print_group <- function(g, indent = "  ") {
  cat(indent, fmt_group(g), "\n", sep = "")
  for (i in seq_along(g@criteria)) {
    cr <- g@criteria[[i]]
    cat(indent, "  criterion [", i, "]: ", describe_capr(cr), "\n", sep = "")
    if (methods::is(cr, "Criteria")) {
      print_nested_attributes(cr@query, indent = paste0(indent, "    "))
    }
  }
  for (i in seq_along(g@group)) {
    cat(indent, "  sub-group [", i, "]:\n", sep = "")
    print_group(g@group[[i]], indent = paste0(indent, "    "))
  }
  invisible(g)
}

# ---- show() methods (compact, scannable) ------------------------------------

setMethod("show", "Cohort", function(object) {
  cli::cat_rule("<Capr Cohort>")
  cat("  Entry    : ", fmt_entry(object@entry), "\n", sep = "")
  cat("  Attrition: ", fmt_attrition(object@attrition), "\n", sep = "")
  cat("  Exit     : ", fmt_exit(object@exit), "\n", sep = "")
  cat("  Era      : ", fmt_era(object@era), "\n", sep = "")
  invisible(object)
})

setMethod("show", "CohortEntry", function(object) {
  cat("Capr CohortEntry: ", fmt_entry(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "CohortAttrition", function(object) {
  cat("Capr CohortAttrition: ", fmt_attrition(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "CohortExit", function(object) {
  cat("Capr CohortExit: ", fmt_exit(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "CohortEra", function(object) {
  cat("Capr CohortEra: ", fmt_era(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "Criteria", function(object) {
  cat("Capr Criteria: ", fmt_criteria(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "Group", function(object) {
  cat("Capr Group: ", fmt_group(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "Occurrence", function(object) {
  cat("Capr Occurrence: ", fmt_occurrence(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "ObservationWindow", function(object) {
  cat("Capr ObservationWindow: ", fmt_observationwindow(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "EventAperture", function(object) {
  cat("Capr EventAperture: ", fmt_aperture(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "EventWindow", function(object) {
  cat("Capr EventWindow: ", fmt_eventwindow(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "Endpoint", function(object) {
  cat("Capr Endpoint: ", fmt_endpoint(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "ObservationExit", function(object) {
  cat("Capr ObservationExit: ", fmt_endstrategy(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "FixedDurationExit", function(object) {
  cat("Capr FixedDurationExit: ", fmt_endstrategy(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "DrugExposureExit", function(object) {
  cat("Capr DrugExposureExit: ", fmt_endstrategy(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "CensoringCriteria", function(object) {
  cat("Capr CensoringCriteria: ", fmt_censoring(object), "\n", sep = "")
  invisible(object)
})

setMethod("show", "nestedAttribute", function(object) {
  cat("Capr nestedAttribute (", object@name, "): ", fmt_group(object@group), "\n", sep = "")
  invisible(object)
})

setMethod("show", "logicAttribute", function(object) {
  cat("Capr logicAttribute: ", object@name, "\n", sep = "")
  invisible(object)
})

setMethod("show", "keyValueAttribute", function(object) {
  cat("Capr keyValueAttribute: ", object@key, " = ", object@value, "\n", sep = "")
  invisible(object)
})

setMethod("show", "dateAdjustmentAttribute", function(object) {
  cat("Capr dateAdjustmentAttribute: ", describe_capr(object), "\n", sep = "")
  invisible(object)
})

# ---- helpers for the concept-set section in summary() -----------------------

# Alias so summary(x, listConceptSets = TRUE) can call the internal listConceptSets
# generic without the argument name shadowing it.
capr_concept_sets <- function(x) listConceptSets(x)

# Print the flat list of concept sets (from listConceptSets) with id, per-item
# concept id/name, and descendant/exclude/mapped logic.
print_concept_sets <- function(cs_list) {
  for (i in seq_along(cs_list)) {
    cs <- cs_list[[i]]
    nm <- cs$name
    nm <- if (is.null(nm) || length(nm) == 0L || is.na(nm) || !nzchar(nm)) "<unnamed>" else nm
    id <- cs$id
    id <- if (is.null(id) || length(id) == 0L || is.na(id)) "<no id>" else id
    cat("  [", i, "] \"", nm, "\"  (id: ", id, ")\n", sep = "")
    for (it in cs$expression$items) {
      cid <- it$concept$CONCEPT_ID
      cname <- it$concept$CONCEPT_NAME
      flags <- character(0)
      if (isTRUE(it$includeDescendants)) flags <- c(flags, "descendants")
      if (isTRUE(it$isExcluded)) flags <- c(flags, "excluded")
      if (isTRUE(it$includeMapped)) flags <- c(flags, "mapped")
      line <- paste0("      ", cid)
      if (!is.null(cname) && length(cname) == 1L && !is.na(cname) && nzchar(cname)) {
        line <- paste0(line, "  ", cname)
      }
      if (length(flags) > 0L) line <- paste0(line, "  [", paste(flags, collapse = ", "), "]")
      cat(line, "\n", sep = "")
    }
  }
  invisible(cs_list)
}

# ---- summary() generic + methods --------------------------------------------

#' Summarise a Capr object
#'
#' Prints a fuller, nested view of a Capr S4 object than \code{show()} does.
#' Every Capr \code{summary} method prints to the console and returns the object
#' invisibly.
#'
#' @param object A Capr object (e.g. \code{Cohort}, \code{CohortEntry},
#'   \code{Query}, \code{Criteria}, \code{Group}).
#' @param ... Additional arguments (currently unused).
#' @return The object, invisibly.
#' @export
setGeneric("summary")

setMethod("summary", "Cohort", function(object, ..., listConceptSets = FALSE) {
  cli::cat_rule("<Capr Cohort>")
  cat("\n")
  summary(object@entry)
  cat("\n")
  summary(object@attrition)
  cat("\n")
  summary(object@exit)
  cat("\n")
  summary(object@era)
  if (isTRUE(listConceptSets)) {
    cat("\nConcept Sets:\n")
    print_concept_sets(capr_concept_sets(object))
  }
  invisible(object)
})

setMethod("summary", "CohortEntry", function(object, ...) {
  cat("Entry: ", fmt_entry(object), "\n", sep = "")
  for (i in seq_along(object@entryEvents)) {
    ev <- object@entryEvents[[i]]
    cat("  [", i, "] ", describe_capr(ev), "\n", sep = "")
    if (methods::is(ev, "Query")) {
      print_nested_attributes(ev, indent = "    ")
    }
  }
  if (length(object@additionalCriteria@criteria) + length(object@additionalCriteria@group) > 0L) {
    cat("  additional criteria: ", describe_capr(object@additionalCriteria), "\n", sep = "")
    print_group(object@additionalCriteria, indent = "    ")
  }
  invisible(object)
})

setMethod("summary", "CohortAttrition", function(object, ...) {
  cat("Attrition: ", fmt_attrition(object), "\n", sep = "")
  nms <- names(object@rules)
  for (i in seq_along(object@rules)) {
    lbl <- if (!is.null(nms) && length(nms) >= i && !is.na(nms[i]) && nzchar(nms[i])) {
      paste0("\"", nms[i], "\" ")
    } else {
      ""
    }
    cat("  [", i, "] ", lbl, describe_capr(object@rules[[i]]), "\n", sep = "")
  }
  invisible(object)
})

setMethod("summary", "CohortExit", function(object, ...) {
  cat("Exit: ", fmt_exit(object), "\n", sep = "")
  cat("  endStrategy: ", fmt_endstrategy(object@endStrategy), "\n", sep = "")
  if (length(object@censoringCriteria@criteria) > 0L) {
    cat("  censoring:\n")
    for (i in seq_along(object@censoringCriteria@criteria)) {
      cat("    [", i, "] ", describe_capr(object@censoringCriteria@criteria[[i]]), "\n", sep = "")
    }
  }
  invisible(object)
})

setMethod("summary", "CohortEra", function(object, ...) {
  cat("Era: ", fmt_era(object), "\n", sep = "")
  invisible(object)
})

setMethod("summary", "Query", function(object, ...) {
  cat("Query: ", fmt_query(object), "\n", sep = "")
  if (length(object@attributes) > 0L) {
    cat("  attributes:\n")
    for (i in seq_along(object@attributes)) {
      a <- object@attributes[[i]]
      if (methods::is(a, "nestedAttribute")) {
        cat("    [", i, "] nested (", a@name, ")\n", sep = "")
        print_group(a@group, indent = "      ")
      } else {
        cat("    [", i, "] ", describe_capr(a), "\n", sep = "")
      }
    }
  }
  invisible(object)
})

setMethod("summary", "Criteria", function(object, ...) {
  cat("Criteria: ", fmt_criteria(object), "\n", sep = "")
  cat("  occurrence: ", fmt_occurrence(object@occurrence), "\n", sep = "")
  cat("  query     : ", describe_capr(object@query), "\n", sep = "")
  print_nested_attributes(object@query, indent = "    ")
  cat("  window    : ", fmt_aperture(object@aperture), "\n", sep = "")
  invisible(object)
})

setMethod("summary", "Group", function(object, ...) {
  cat("Group: ", fmt_group(object), "\n", sep = "")
  print_group(object, indent = "  ")
  invisible(object)
})

setMethod("summary", "nestedAttribute", function(object, ...) {
  cat("nested (", object@name, "):\n", sep = "")
  print_group(object@group, indent = "  ")
  invisible(object)
})

setMethod("summary", "ConceptSet", function(object, ...) {
  cat("ConceptSet ", fmt_conceptset(object), "\n", sep = "")
  print(as.data.frame(object))
  invisible(object)
})
