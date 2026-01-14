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
  e <- entry(withAll(
    atLeast(1, conditionOccurrence(cs(1, name = "test"))),
    exactly(0, conditionOccurrence(cs(2, name = "test")))
  ))
  conceptSets <- listConceptSets(e)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
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
  skip("failing test") # TODO fix listConceptSets so this passes

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

  # this does not work
  x <- visit(cs(descendants(9201, 9203, 262), name = "test"),
    nestedWithAll(
      atLeast(1,
        conditionOccurrence(cs(descendants(316139), name = "heart failure"),
          attributes = list(male(), nestedWithAll(
            atLeast(1,
              conditionOccurrence(cs(descendants(316139), name = "heart failure"))
            )
          ))
        )
      )
    )
  )

  # str(a, max.level = 5) # I'm not sure if this Capr object is correct - attribute under attribute
  conceptSets <- listConceptSets(x) # error
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
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
  test_cs <- cs(c(123, 456), name = "test source concepts", id = "test-id-123")
  attr <- conditionSourceConcept(test_cs)

  expect_s4_class(attr, "conceptSetAttribute")
  expect_equal(attr@name, "ConditionSourceConcept")
  expect_equal(attr@conceptSet@id, "test-id-123")
})

test_that("conditionSourceConcept requires ConceptSet", {
  expect_error(conditionSourceConcept("not a concept set"),
               "conditionSourceConcept requires a ConceptSet object")
  expect_error(conditionSourceConcept(123),
               "conditionSourceConcept requires a ConceptSet object")
})

test_that("collectGuid works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts", id = "test-id-123")
  attr <- conditionSourceConcept(test_cs)

  guid_result <- collectGuid(attr)

  expect_s3_class(guid_result, "data.frame")
  expect_equal(guid_result$guid, "test-id-123")
  expect_equal(nrow(guid_result), 1)
})

test_that("replaceCodesetId works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts", id = "test-guid-456")
  attr <- conditionSourceConcept(test_cs)

  # Create a guide table for replacement
  guide_table <- data.frame(
    guid = "test-guid-456",
    codesetId = 42L
  )

  replaced_attr <- replaceCodesetId(attr, guide_table)

  expect_s4_class(replaced_attr, "conceptSetAttribute")
  expect_equal(replaced_attr@conceptSet@id, 42L)
})

test_that("listConceptSets works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts", id = "test-id-789")
  attr <- conditionSourceConcept(test_cs)

  concept_sets <- listConceptSets(attr)

  expect_true(is.list(concept_sets))
  expect_equal(concept_sets$id, "test-id-789")
  expect_equal(concept_sets$name, "test source concepts")
})

test_that("as.list works with conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "test source concepts", id = "test-id-999")
  attr <- conditionSourceConcept(test_cs)

  list_result <- as.list(attr)

  expect_true(is.list(list_result))
  expect_equal(list_result$ConditionSourceConcept, "test-id-999")
})

test_that("conceptSetAttribute integrates with Query attributes", {
  # Test that conditionSourceConcept can be used as an attribute in a query
  test_cs <- cs(c(123, 456), name = "source concepts", id = "source-id-123")
  source_attr <- conditionSourceConcept(test_cs)

  # Create a query with the conditionSourceConcept attribute
  main_cs <- cs(c(789, 101112), name = "main condition")
  query <- conditionOccurrence(main_cs, source_attr)

  expect_s4_class(query, "Query")
  expect_length(query@attributes, 1)
  expect_s4_class(query@attributes[[1]], "conceptSetAttribute")
})

test_that("collectGuid works with Query containing conceptSetAttribute", {
  test_cs <- cs(c(123, 456), name = "source concepts", id = "source-id-456")
  source_attr <- conditionSourceConcept(test_cs)

  main_cs <- cs(c(789, 101112), name = "main condition", id = "main-id-789")
  query <- conditionOccurrence(main_cs, source_attr)

  guid_result <- collectGuid(query)

  expect_s3_class(guid_result, "data.frame")
  expect_equal(sort(guid_result$guid), sort(c("main-id-789", "source-id-456")))
  # Query collectGuid now collects both the main conceptSet ID and attribute IDs
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
