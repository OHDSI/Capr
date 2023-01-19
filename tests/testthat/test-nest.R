test_that("Nesting Criteria works", {

  query <- visit(cs(descendants(9201, 9203, 262)),
                 nestedWithAll(
                   atLeast(1,
                           condition(cs(descendants(316139), name = "heart failure")),
                           duringInterval(startWindow = eventStarts(0, Inf),
                                          endWindow = eventStarts(-Inf, 0, index = "endDate")
                           )
                   )
                 )
  )
  expect_s4_class(query@attributes[[1]], "nestedAttribute")
   #chekc coercion
  query_list <- Capr:::as.list(query)
  expect_equal(names(query_list$VisitOccurrence)[2], "CorrelatedCriteria")
})
