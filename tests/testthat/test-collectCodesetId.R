
# test the listConceptSets() function

# Query, group

test_that("listConceptSets - Query", {
  conceptSets <- listConceptSets(condition(cs(1)))
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Criteria", {
  conceptSets <- listConceptSets(atLeast(1, condition(cs(1))))
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Group", {
  g <- withAll(
    atLeast(1, condition(cs(1))),
    exactly(0, condition(cs(2)))
  )
  conceptSets <- listConceptSets(g)
  expect_length(conceptSets, 2)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Entry", {
  e <- entry(withAll(
    atLeast(1, condition(cs(1))),
    exactly(0, condition(cs(2)))
  ))
  conceptSets <- listConceptSets(e)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Attrition", {
  x <- attrition(withAll(
    atLeast(1, condition(cs(1))),
    exactly(0, condition(cs(2)))
  ))

  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 2)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Attrition", {

  x <- attrition(
    'no t1d' = withAll(
      exactly(0,
              condition(cs(descendants(201254L))),
              duringInterval(eventStarts(-Inf, -1))
      )
    ),
    'abnormal hba1c' = withAll(
      atLeast(1,
              measurement(
                cs(descendants(4184637L)),
                valueAsNumber(lt(13)),
                unit(8713L)
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
  e <- exit(drugExit(cs(1,2,5)))
  expect_s4_class(e, "CohortExit")
  conceptSets <- listConceptSets(e)
  expect_length(conceptSets, 1)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Query with nested criteria", {
  x <- visit(cs(1),
             nestedWithAll(atLeast(1, condition(cs(9))),
                           atLeast(1, drug(cs(1:5)))))

  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 3)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - Query with double nested criteria", {
  x <- visit(cs(1),
         nestedWithAll(
           atLeast(1, drug(cs(21:24))),
           atLeast(1, condition(cs(9),
             nestedWithAll(atLeast(1, drug(cs(11))))
      ))))

  conceptSets <- listConceptSets(x)
  expect_length(conceptSets, 4)
  expect_true(all(purrr::map_lgl(conceptSets, ~all(names(.) == c("id", "name", "expression")))))
})

test_that("listConceptSets - nested Query", {
  skip("failing test") # TODO fix listConceptSets so this passes

  # this works fine
  x <- visit(cs(descendants(9201, 9203, 262)),
    nestedWithAll(
      atLeast(1,
        condition(cs(descendants(316139), name = "heart failure"),
          attributes = nestedWithAll(
            atLeast(1,
              condition(cs(descendants(316139), name = "heart failure"))
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
  x <- visit(cs(descendants(9201, 9203, 262)),
    nestedWithAll(
      atLeast(1,
        condition(cs(descendants(316139), name = "heart failure"),
          attributes = list(male(), nestedWithAll(
            atLeast(1,
              condition(cs(descendants(316139), name = "heart failure"))
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
      condition(cs(descendants(201826L)), male()),
      observationWindow = continuousObservation(365, 0)
    ),
    attrition = attrition(
      'no t1d' = withAll(
        exactly(0,
                condition(cs(descendants(201254L))),
                duringInterval(eventStarts(-Inf, -1))
        )
      ),
      'abnormal hba1c' = withAll(
        atLeast(1,
                measurement(
                  cs(descendants(4184637L)),
                  valueAsNumber(lt(13)),
                  unit(8713L)
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

