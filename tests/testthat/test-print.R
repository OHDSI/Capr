# test-print.R
# Guards the console print (show) and summary output for the user-facing Capr
# classes. Keeps the "compact show / fuller summary" contract from regressing
# (e.g. raw S4 str dumps, dropped fields, or the issue #123 nested-criteria case).

t2dm <- cs(descendants(201826L), name = "T2DM")
ins  <- cs(descendants(1503297L), name = "Metformin")
hf   <- cs(descendants(316139L), name = "Heart failure")

make_cohort <- function() {
  cohort(
    entry = entry(
      conditionOccurrence(t2dm, male(), firstOccurrence()),
      conditionOccurrence(hf, nestedWithAll(atLeast(1, conditionOccurrence(hf)))),
      observationWindow = continuousObservation(365L, 0L),
      primaryCriteriaLimit = "First"
    ),
    attrition = attrition(
      "no prior insulin" = withAll(
        exactly(0, drugExposure(ins), duringInterval(eventStarts(-Inf, -1)))
      ),
      "heart failure ever" = withAll(
        atLeast(1, conditionOccurrence(hf))
      ),
      expressionLimit = "All"
    ),
    exit = exit(
      endStrategy = fixedExit(index = "startDate", offsetDays = 30L),
      censor = censoringEvents(conditionOccurrence(hf))
    ),
    era = era(eraDays = 30L)
  )
}

expect_output_has <- function(out, pattern) {
  expect_true(
    any(grepl(pattern, out, fixed = TRUE)),
    info = sprintf("output should contain: %s", pattern)
  )
}

test_that("show(Cohort) is compact and informative", {
  out <- capture_output_lines(show(make_cohort()))

  expect_output_has(out, "<Capr Cohort>")
  expect_output_has(out, "Entry    : 2 events, limit = First")
  expect_output_has(out, "obsWindow(365d, 0d)")
  expect_output_has(out, "Attrition: 2 rules, expressionLimit = All")
  expect_output_has(out, "fixedExit(startDate + 30d)")
  expect_output_has(out, "1 censoring event")
  expect_output_has(out, "Era      : gap 30d")
})

test_that("summary(Cohort) expands the structure, including nested criteria", {
  out <- capture_output_lines(summary(make_cohort()))

  expect_output_has(out, "<Capr Cohort>")
  # entry events listed with their domain + concept set
  expect_output_has(out, "ConditionOccurrence \"T2DM\" (1 concept) [Gender, First]")
  # nested correlated criteria are expanded, not hidden behind an attribute name
  expect_output_has(out, "nested (CorrelatedCriteria):")
  expect_output_has(out, "withAll (1 criterion, 0 sub-groups)")
  expect_output_has(out, "atLeast 1 × ConditionOccurrence \"Heart failure\"")
  # named attrition rule label is preserved
  expect_output_has(out, "\"no prior insulin\" withAll")
  # exit details
  expect_output_has(out, "fixedExit(startDate + 30d)")
  expect_output_has(out, "endStrategy: fixedExit")
  expect_output_has(out, "censoring:")
  # era
  expect_output_has(out, "gap 30d")
})

test_that("summary returns the object invisibly (no double print)", {
  expect_invisible(summary(make_cohort()))
})

test_that("Cohort component show()/summary() work", {
  cd <- make_cohort()

  # CohortEntry
  expect_output_has(capture_output_lines(show(cd@entry)), "2 events, limit = First")
  e_out <- capture_output_lines(summary(cd@entry))
  expect_output_has(e_out, "Entry: 2 events")
  expect_output_has(e_out, "nested (CorrelatedCriteria):")

  # CohortAttrition
  expect_output_has(capture_output_lines(show(cd@attrition)), "2 rules, expressionLimit = All")
  expect_output_has(capture_output_lines(summary(cd@attrition)), "\"no prior insulin\" withAll")

  # CohortExit
  expect_output_has(capture_output_lines(show(cd@exit)), "fixedExit(startDate + 30d)")
  expect_output_has(capture_output_lines(summary(cd@exit)), "censoring:")

  # CohortEra
  expect_output_has(capture_output_lines(show(cd@era)), "gap 30d")
})

test_that("Query show()/summary() include concept set and attributes", {
  q <- conditionOccurrence(t2dm, male(), firstOccurrence())

  expect_output_has(capture_output_lines(show(q)), "ConditionOccurrence \"T2DM\" (1 concept)")
  expect_output_has(capture_output_lines(show(q)), "[Gender, First]")

  q_out <- capture_output_lines(summary(q))
  expect_output_has(q_out, "Query: ConditionOccurrence \"T2DM\"")
  expect_output_has(q_out, "Gender: 1 concept")
  expect_output_has(q_out, "First")
})

test_that("Criteria show()/summary() include occurrence, query, and window", {
  cr <- exactly(0, drugExposure(ins), duringInterval(eventStarts(-Inf, -1)))

  s_out <- capture_output_lines(show(cr))
  expect_output_has(s_out, "exactly 0")
  expect_output_has(s_out, "DrugExposure \"Metformin\" (1 concept)")
  expect_output_has(s_out, "duringInterval(eventStarts(-Inf, -1))")

  m_out <- capture_output_lines(summary(cr))
  expect_output_has(m_out, "occurrence: exactly 0")
  expect_output_has(m_out, "query     : DrugExposure \"Metformin\"")
  expect_output_has(m_out, "window    : duringInterval(eventStarts(-Inf, -1))")
})

test_that("Group and nestedAttribute show()/summary() reveal contents", {
  g <- withAll(exactly(0, drugExposure(ins)))

  expect_output_has(capture_output_lines(show(g)), "withAll (1 criterion, 0 sub-groups)")
  g_out <- capture_output_lines(summary(g))
  expect_output_has(g_out, "criterion [1]: exactly 0 × DrugExposure")

  na <- nestedWithAll(atLeast(1, conditionOccurrence(hf)))
  expect_output_has(capture_output_lines(show(na)), "nestedAttribute (CorrelatedCriteria)")
  na_out <- capture_output_lines(summary(na))
  expect_output_has(na_out, "nested (CorrelatedCriteria):")
  expect_output_has(na_out, "criterion [1]: atLeast 1")
})

test_that("exit strategies show() readably", {
  expect_output_has(capture_output_lines(show(observationExit())),
                    "observationExit (end of continuous observation)")
  expect_output_has(capture_output_lines(show(fixedExit(offsetDays = 30L))),
                    "fixedExit(startDate + 30d)")
  expect_output_has(capture_output_lines(show(drugExit(ins, persistenceWindow = 30L))),
                    "drugExit(\"Metformin\", persist 30d")
})

test_that("windows and censoring show() readably", {
  expect_output_has(capture_output_lines(show(continuousObservation(365L, 0L))),
                    "obsWindow(365d, 0d)")
  expect_output_has(capture_output_lines(show(duringInterval(eventStarts(-365, 0)))),
                    "duringInterval(eventStarts(-365, 0))")
  expect_output_has(capture_output_lines(show(censoringEvents(conditionOccurrence(hf)))),
                    "1 censoring event")
})

test_that("summary(Cohort, listConceptSets = TRUE) lists concept sets with id and logic", {
  out <- capture_output_lines(summary(make_cohort(), listConceptSets = TRUE))

  expect_output_has(out, "Concept Sets:")
  # each concept set listed with its name and id
  expect_output_has(out, "\"T2DM\"  (id:")
  expect_output_has(out, "\"Heart failure\"  (id:")
  # per-item concept id + descendant logic
  expect_output_has(out, "201826")
  expect_output_has(out, "descendants")
  # default (unhydrated) shows no concept names
  expect_false(any(grepl("Type 2 diabetes", out, fixed = TRUE)))
})

test_that("summary(Cohort) omits the concept-set section by default", {
  out <- capture_output_lines(summary(make_cohort()))
  expect_false(any(grepl("Concept Sets:", out, fixed = TRUE)))
})

test_that("summary(ConceptSet) prints the concept table", {
  out <- capture_output_lines(summary(t2dm))
  expect_output_has(out, "ConceptSet \"T2DM\"")
  expect_output_has(out, "conceptId")
})
