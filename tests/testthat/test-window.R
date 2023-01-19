test_that("EventWindow works", {

  #Test 1: negative value yields -1 coeff
  ew <- eventStarts(-Inf, -1)
  expect_s4_class(ew, "EventWindow")
  expect_equal(ew@start@coeff, -1)

  #Test 2: positive value yields 1 coeff
  ew <- eventStarts(1, Inf)
  expect_equal(ew@start@coeff, 1)

  # Test 3: change index to end
  ew <- eventStarts(Inf, 0, index = "endDate")
  expect_equal(ew@index, "endDate")

  # Test 4: zero input forces a -1
  ew <- eventStarts(Inf, 0, index = "endDate")
  expect_equal(ew@end@coeff, -1)

})


test_that("duringInterval work", {

  #check that no end window yields empty
  di <- duringInterval(startWindow = eventStarts(-Inf, -1))
  expect_equal(di@endWindow@end@days, NA_integer_)

  #check if add endWindow
  di <- duringInterval(startWindow = eventStarts(0, Inf),
                       endWindow = eventStarts(-Inf, 0, index = "endDate"))
  expect_equal(di@startWindow@index, "startDate")
  expect_equal(di@endWindow@end@coeff, -1)
  expect_equal(di@endWindow@index, "endDate")

})
