# test-getConceptSetDetails.R
# Tests for the concept-set "hydration" feature: getConceptSetDetails() now works
# on both a single ConceptSet and a whole Cohort (walks entry/attrition/exit,
# nested criteria, and concept-set attributes, hydrating each unique set once).

make_hydration_cohort <- function() {
  t2dm <- cs(descendants(201826L), name = "T2DM")
  ins  <- cs(descendants(1503297L), name = "Metformin")
  hf   <- cs(descendants(316139L), name = "Heart failure")

  cohort(
    entry = entry(
      conditionOccurrence(t2dm, male(), conditionSourceConcept(cs(4112752L, name = "DM source"))),
      conditionOccurrence(hf, nestedWithAll(atLeast(1, conditionOccurrence(hf))))
    ),
    attrition = attrition(
      "no prior insulin" = withAll(exactly(0, drugExposure(ins)))
    ),
    exit = exit(endStrategy = drugExit(ins, persistenceWindow = 30L))
  )
}

test_that("hydrate_concept_sets walks the whole cohort, hydrating each unique set once", {
  cd <- make_hydration_cohort()

  calls <- new.env(parent = emptyenv())
  hydrate_one <- function(cs) {
    key <- as.character(cs@id)
    calls[[key]] <- (if (is.null(calls[[key]])) 0L else calls[[key]]) + 1L
    cs@Name <- paste0(cs@Name, "*")
    cs
  }

  out <- Capr:::hydrate_concept_sets(cd, hydrate_one)

  # Every concept set reached: entry, nested criteria, concept-set attribute,
  # attrition, and the drug-exit concept set.
  hydrated <- Capr:::listConceptSets(out)
  expect_true(all(grepl("\\*$", vapply(hydrated, function(s) s$name, character(1)))))

  # Dedupe: the same set (e.g. "Heart failure" in entry AND nested) is hydrated once.
  expect_equal(sum(unlist(as.list(calls))), 4L)

  # Concept set ids are preserved (hydration only fills detail slots).
  before <- Capr:::listConceptSets(cd)
  expect_true(identical(
    sort(vapply(before, function(s) as.character(s$id), character(1))),
    sort(vapply(hydrated, function(s) as.character(s$id), character(1)))
  ))
})

test_that("hydrate_concept_sets leaves non-concept-bearing leaves unchanged", {
  occ <- methods::new("Occurrence", type = "exactly", count = 1L)
  no_cs <- Capr:::hydrate_concept_sets(occ, function(cs) stop("should not be called"))
  expect_s4_class(no_cs, "Occurrence")
})

test_that("getConceptSetDetails(Cohort) validates the connection", {
  cd <- make_hydration_cohort()
  expect_error(getConceptSetDetails(cd, con = NULL), "dbIsValid")
})

test_that("getConceptSetDetails(Cohort) is an S4 generic dispatching on Cohort and ConceptSet", {
  expect_true(methods::isGeneric("getConceptSetDetails"))
  expect_true(methods::hasMethod("getConceptSetDetails", "ConceptSet"))
  expect_true(methods::hasMethod("getConceptSetDetails", "Cohort"))
})

test_that("getConceptSetDetails(Cohort) hydrates with a real vocab (Eunomia)", {
  skip_if_not_installed("Eunomia")
  skip_if_not_installed("DatabaseConnector")

  con <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
  on.exit(DatabaseConnector::disconnect(con), add = TRUE)

  gi_bleed <- cs(descendants(192671L), name = "GI Bleed")
  cd <- cohort(entry(conditionOccurrence(gi_bleed)))

  hydrated <- getConceptSetDetails(cd, con, vocabularyDatabaseSchema = "main")
  concept <- hydrated@entry@entryEvents[[1]]@conceptSet@Expression[[1]]@Concept
  expect_true(nchar(concept@concept_name) > 0)
  expect_true(concept@domain_id != "")
})
