# test the listConceptSets() function

# Query, group

test_that("listConceptSets - Query", {
  conceptSets <- listConceptSets(conditionOccurrence(cs(1, name = "test")))
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Query no concept", {
  conceptSets <- listConceptSets(observationPeriod())
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
  expect_equal(conceptSets[[1]]$id, NULL)
})



test_that("listConceptSets - Criteria", {
  conceptSets <- listConceptSets(atLeast(1, conditionOccurrence(cs(1, name = "test"))))
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Group", {
  g <- withAll(
    atLeast(1, conditionOccurrence(cs(1, name = "test"))),
    exactly(0, conditionOccurrence(cs(2, name = "test")))
  )
  conceptSets <- listConceptSets(g)
  expect_length(conceptSets, 2)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Entry", {
  # constructed via new(): entry() only accepts Query objects in `...`, but
  # listConceptSets must still recurse over group-shaped entry contents
  e <- methods::new("CohortEntry", entryEvents = list(withAll(
    atLeast(1, conditionOccurrence(cs(1, name = "test"))),
    exactly(0, conditionOccurrence(cs(2, name = "test")))
  )))
  conceptSets <- listConceptSets(e)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Entry with multiple entryEvents that each have nested criteria", {
  # Regression test: previously, listConceptSets(CohortEntry) only flattened
  # across entryEvents when their per-entryEvent concept-set counts weren't
  # all equal to a hardcoded 3 - so 2+ entryEvents that each happened to
  # contribute exactly 3 (e.g. one query + 2 nestedWithAny alternatives)
  # silently produced a nested (unflattened) list, and every concept set was
  # then dropped downstream by listConceptSets(Cohort)'s `$id` filter.
  cs_a <- cs(1, name = "a")
  e <- entry(
    conditionOccurrence(cs_a, nestedWithAny(
      atLeast(1, conditionOccurrence(cs_a)),
      atLeast(1, observation(cs_a))
    )),
    observation(cs_a, nestedWithAny(
      atLeast(1, conditionOccurrence(cs_a)),
      atLeast(1, observation(cs_a))
    )),
    primaryCriteriaLimit = "First"
  )
  conceptSets <- listConceptSets(e)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
  expect_true(length(conceptSets) > 0)

  cd <- cohort(entry = e, attrition = attrition(expressionLimit = "First"),
               exit = exit(endStrategy = observationExit()))
  compiledConceptSets <- jsonlite::fromJSON(toCohortJson(cd), simplifyVector = FALSE)$ConceptSets
  expect_length(compiledConceptSets, 1)
})

test_that("listConceptSets - Attrition", {
  x <- attrition(withAll(
    atLeast(1, conditionOccurrence(cs(1, name = "test"))),
    exactly(0, conditionOccurrence(cs(2, name = "test")))
  ))

  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 2)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Attrition", {

  x <- attrition(
    'no t1d' = withAll(
      exactly(0,
              conditionOccurrence(cs(descendants(201254L), name = "test")),
              duringInterval(eventStarts(-Inf, -1))
      )
    ),
    'abnormal hba1c' = withAll(
      atLeast(1,
              measurement(
                cs(descendants(4184637L), name = "test"),
                valueAsNumber(lt(13)),
                measurementUnit(8713L)
              ),
              duringInterval(eventStarts(-Inf, -1))
      )
    )
  )

  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 2)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})




test_that("listConceptSets - CohortExit", {
  e <- exit(drugExit(cs(1,2,5, name = "test")))
  expect_s4_class(e, "CohortExit")
  conceptSets <- listConceptSets(e)
  expect_length(conceptSets, 1)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Query with nested criteria", {
  x <- visit(cs(1, name = "test"),
             nestedWithAll(atLeast(1, conditionOccurrence(cs(9, name = "test"))),
                           atLeast(1, drugExposure(cs(1:5, name = "test")))))

  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 3)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Query with double nested criteria", {
  x <- visit(cs(1, name = "test"),
         nestedWithAll(
           atLeast(1, drugExposure(cs(21:24, name = "test"))),
           atLeast(1, conditionOccurrence(cs(9, name = "test"),
             nestedWithAll(atLeast(1, drugExposure(cs(11, name = "test"))))
      ))))

  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 4)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - nested Query", {
  
  # this works fine
  x <- visit(cs(descendants(9201, 9203, 262), name = "test"),
    nestedWithAll(
      atLeast(1,
        conditionOccurrence(cs(descendants(316139), name = "heart failure"),
          attributes = nestedWithAll(
            atLeast(1,
              conditionOccurrence(cs(descendants(316139), name = "heart failure"))
            )
          )
        )
      )
    )
  )


  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 3)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))

  # this works add in an attribute
  x <- visit(cs(descendants(9201, 9203, 262), name = "test"),
    nestedWithAll(
      atLeast(1,
        conditionOccurrence(cs(descendants(316139), name = "heart failure"),
          attributes = male(), nestedWithAll(
            atLeast(1,
              conditionOccurrence(cs(descendants(316139), name = "heart failure"))
            )
          )
        )
      )
    )
  )


  conceptSets <- listConceptSets(x) # error
  expect_length(conceptSets, 3)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets invariant: flat, well-formed sets for adversarial nesting", {
  # Regression guard for issue #123: multi-layer nested criteria where an outer
  # nest has a single element must never return an empty list or nested shapes.
  shapes <- list(
    # single-element outer nest (the issue #123 trigger)
    visit(cs(1L, name = "test"),
          nestedWithAll(
            atLeast(1, conditionOccurrence(cs(3L, name = "test3"),
              nestedWithAll(atLeast(1, conditionOccurrence(cs(1L, name = "test")))))))),
    # deep single-element chain, distinct sets at every level
    visit(cs(1L, name = "cs1"),
          nestedWithAll(atLeast(1, conditionOccurrence(cs(2L, name = "cs2"),
            nestedWithAll(atLeast(1, conditionOccurrence(cs(3L, name = "cs3"),
              nestedWithAll(atLeast(1, conditionOccurrence(cs(4L, name = "cs4"),
                nestedWithAll(atLeast(1, conditionOccurrence(cs(5L, name = "cs5")))))))))))))),
    # outer nest mixing a criterion and a nested sub-group
    visit(cs(1L, name = "test"),
          nestedWithAll(
            atLeast(1, conditionOccurrence(cs(2L, name = "test2"))),
            withAll(atLeast(1, conditionOccurrence(cs(3L, name = "test3"))))))
  )

  for (x in shapes) {
    conceptSets <- listConceptSets(x)
    # not empty (issue #123 symptom)
    expect_gt(length(conceptSets), 0L)
    # every element is a flat concept set, i.e. no nested list-of-sets survived
    expect_true(all(purrr::map_lgl(
      conceptSets,
      ~all(names(.) == c("id", "name", "expression"))
    )), info = "every element must be a flat concept set (id/name/expression)")
  }
})


test_that("listConceptSets - Cohort", {

  cd <- cohort(
    entry = entry(
      conditionOccurrence(cs(descendants(201826L), name = "test"), male()),
      observationWindow = continuousObservation(365, 0)
    ),
    attrition = attrition(
      'no t1d' = withAll(
        exactly(0,
                conditionOccurrence(cs(descendants(201254L), name = "test")),
                duringInterval(eventStarts(-Inf, -1))
        )
      ),
      'abnormal hba1c' = withAll(
        atLeast(1,
                measurement(
                  cs(descendants(4184637L), name = "test"),
                  valueAsNumber(lt(13)),
                  measurementUnit(8713L)
                ),
                duringInterval(eventStarts(-Inf, -1))
        )
      )
    )
  )

  conceptSets <- listConceptSets(cd)
  expect_length(conceptSets, 3)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Cohort 2", {

  cd <- cohort(
    entry = entry(
      conditionOccurrence(cs(descendants(201826L), name = "test"), male()),
      observationWindow = continuousObservation(365, 0)
    ),
    attrition = attrition(
      'no t1d' = withAll(
        exactly(0,
                conditionOccurrence(cs(descendants(201254L), name = "test")),
                duringInterval(eventStarts(-Inf, -1))
        )
      ),
      'abnormal hba1c' = withAll(
        atLeast(1,
                measurement(
                  cs(descendants(4184637L), name = "test"),
                  valueAsNumber(lt(13)),
                  measurementUnit(8713L)
                ),
                duringInterval(eventStarts(-Inf, -1))
        )
      )
    ),
    exit = exit(
      endStrategy = observationExit(),
      censor = censoringEvents(death())
    )
  )

  conceptSets <- listConceptSets(cd)
  expect_length(conceptSets, 3)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

# Tests for conditionSourceConcept attribute and conceptSetAttribute class
test_that("conditionSourceConcept creates conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts")
  attr <- conditionSourceConcept(test_cs)

  expect_s4_class(attr, "conceptSetAttribute")
  expect_equal(attr@name, "ConditionSourceConcept")
  expect_equal(attr@conceptSet@id, test_cs@id)
})

test_that("conditionSourceConcept requires ConceptSet", {
  expect_error(conditionSourceConcept("not a concept set"),
               "conditionSourceConcept requires a ConceptSet object")
  expect_error(conditionSourceConcept(123),
               "conditionSourceConcept requires a ConceptSet object")
})

test_that("collectGuid works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts")
  attr <- conditionSourceConcept(test_cs)

  guid_result <- collectGuid(attr)

  expect_s3_class(guid_result, "data.frame")
  expect_equal(guid_result$guid, test_cs@id)
  expect_equal(nrow(guid_result), 1)
})

test_that("replaceCodesetId works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts")
  attr <- conditionSourceConcept(test_cs)

  guide_table <- data.frame(
    guid = test_cs@id,
    codesetId = 42L
  )

  replaced_attr <- replaceCodesetId(attr, guide_table)

  expect_s4_class(replaced_attr, "conceptSetAttribute")
  expect_equal(replaced_attr@conceptSet@id, 42L)
})

test_that("listConceptSets works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts")
  attr <- conditionSourceConcept(test_cs)

  concept_sets <- listConceptSets(attr)

  # Leaf methods return a flat list of concept sets (invariant), so unwrap one
  # level to get the single set.
  expect_true(is.list(concept_sets))
  expect_length(concept_sets, 1)
  expect_equal(concept_sets[[1]]$id, test_cs@id)
  expect_equal(concept_sets[[1]]$name, "test source concepts")
})

test_that("as.list works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts")
  attr <- conditionSourceConcept(test_cs)

  list_result <- as.list(attr)

  expect_true(is.list(list_result))
  expect_equal(list_result$ConditionSourceConcept, test_cs@id)
})

test_that("conceptSetAttribute integrates with Query attributes", {
  # Test that conditionSourceConcept can be used as an attribute in a query
  test_cs <- cs(c(123, 456), name = "source concepts")
  source_attr <- conditionSourceConcept(test_cs)

  # Create a query with the conditionSourceConcept attribute
  main_cs <- cs(c(789, 101112), name = "main condition")
  query <- conditionOccurrence(main_cs, source_attr)

  expect_s4_class(query, "Query")
  expect_length(query@attributes, 1)
  expect_s4_class(query@attributes[[1]], "conceptSetAttribute")
})

test_that("collectGuid works with Query containing conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "source concepts")
  source_attr <- conditionSourceConcept(test_cs)

  main_cs <- cs(c(789, 101112), name = "main condition")
  query <- conditionOccurrence(main_cs, source_attr)

  guid_result <- collectGuid(query)

  expect_s3_class(guid_result, "data.frame")
  expect_equal(sort(guid_result$guid), sort(c(main_cs@id, test_cs@id)))
})

test_that("conditionOccurrence gives clear error when conceptSet is missing", {
  expect_error(
    conditionOccurrence(),
    "conceptSet argument is required. If you don't want to specify a concept set use: conceptSet = NULL",
    fixed = TRUE
  )
})

test_that("conditionOccurrence works with conceptSet = NULL", {
  expect_no_error({
    query <- conditionOccurrence(conceptSet = NULL)
  })

  query <- conditionOccurrence(conceptSet = NULL)
  expect_s4_class(query, "Query")
  expect_equal(query@domain, "ConditionOccurrence")
})
