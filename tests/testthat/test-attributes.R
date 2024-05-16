test_that("first occurrence works", {
  tt <- firstOccurrence()
  expect_equal(tt@name, "First")
})

test_that("op integer methods build", {
  #
  t1 <- lt(65L)
  expect_s4_class(t1, "opAttributeInteger")
  expect_equal(t1@op, "lt")
  expect_equal(t1@value, 65L)

  t2 <- lte(65L)
  expect_s4_class(t2, "opAttributeInteger")
  expect_equal(t2@op, "lte")
  expect_equal(t2@value, 65L)

  t3 <- gt(18L)
  expect_s4_class(t3, "opAttributeInteger")
  expect_equal(t3@op, "gt")
  expect_equal(t3@value, 18L)

  t4 <- gte(18L)
  expect_s4_class(t4, "opAttributeInteger")
  expect_equal(t4@op, "gte")
  expect_equal(t4@value, 18L)

  t5 <- eq(18L)
  expect_s4_class(t5, "opAttributeInteger")
  expect_equal(t5@op, "eq")
  expect_equal(t5@value, 18L)

  t6 <- bt(18L, 65L)
  expect_s4_class(t6, "opAttributeInteger")
  expect_equal(t6@op, "bt")
  expect_equal(t6@value, 18L)
  expect_equal(t6@extent, 65L)

  t7 <- nbt(18L, 65L)
  expect_s4_class(t7, "opAttributeInteger")
  expect_equal(t7@op, "!bt")
  expect_equal(t7@value, 18L)
  expect_equal(t7@extent, 65L)

})


test_that("op numeric methods build", {
  #
  t1 <- lt(65)
  expect_s4_class(t1, "opAttributeNumeric")
  expect_equal(t1@op, "lt")
  expect_equal(t1@value, 65)

  t2 <- lte(65)
  expect_s4_class(t2, "opAttributeNumeric")
  expect_equal(t2@op, "lte")
  expect_equal(t2@value, 65)

  t3 <- gt(18)
  expect_s4_class(t3, "opAttributeNumeric")
  expect_equal(t3@op, "gt")
  expect_equal(t3@value, 18)

  t4 <- gte(18)
  expect_s4_class(t4, "opAttributeNumeric")
  expect_equal(t4@op, "gte")
  expect_equal(t4@value, 18)

  t5 <- eq(18)
  expect_s4_class(t5, "opAttributeNumeric")
  expect_equal(t5@op, "eq")
  expect_equal(t5@value, 18)

  t6 <- bt(18, 65)
  expect_s4_class(t6, "opAttributeNumeric")
  expect_equal(t6@op, "bt")
  expect_equal(t6@value, 18)
  expect_equal(t6@extent, 65)

  t7 <- nbt(18, 65)
  expect_s4_class(t7, "opAttributeNumeric")
  expect_equal(t7@op, "!bt")
  expect_equal(t7@value, 18)
  expect_equal(t7@extent, 65)

})


test_that("op date methods build", {

  highDate <- lubridate::as_date("2020-01-01")
  lowDate <- lubridate::as_date("2010-01-01")

  t1 <- lt(highDate)
  expect_s4_class(t1, "opAttributeDate")
  expect_equal(t1@op, "lt")
  expect_equal(t1@value, highDate)

  t2 <- lte(highDate)
  expect_s4_class(t2, "opAttributeDate")
  expect_equal(t2@op, "lte")
  expect_equal(t2@value, highDate)

  t3 <- gt(lowDate)
  expect_s4_class(t3, "opAttributeDate")
  expect_equal(t3@op, "gt")
  expect_equal(t3@value, lowDate)

  t4 <- gte(lowDate)
  expect_s4_class(t4, "opAttributeDate")
  expect_equal(t4@op, "gte")
  expect_equal(t4@value, lowDate)

  t5 <- eq(lowDate)
  expect_s4_class(t5, "opAttributeDate")
  expect_equal(t5@op, "eq")
  expect_equal(t5@value, lowDate)

  t6 <- bt(lowDate, highDate)
  expect_s4_class(t6, "opAttributeDate")
  expect_equal(t6@op, "bt")
  expect_equal(t6@value, lowDate)
  expect_equal(t6@extent, highDate)

  t7 <- nbt(lowDate, highDate)
  expect_s4_class(t7, "opAttributeDate")
  expect_equal(t7@op, "!bt")
  expect_equal(t7@value, lowDate)
  expect_equal(t7@extent, highDate)

})


test_that("user op integer work", {

  # test age
  t1 <- age(gte(18L))
  expect_s4_class(t1, "opAttributeInteger")
  expect_equal(t1@name, "Age")
  expect_equal(t1@op, "gte")
  expect_equal(t1@value, 18L)

  # test days of suply
  t2 <- daysOfSupply(eq(2L))
  expect_s4_class(t2, "opAttributeInteger")
  expect_equal(t2@name, "DaysSupply")
  expect_equal(t2@op, "eq")
  expect_equal(t2@value, 2L)

  # test refills
  t3 <- drugRefills(eq(2L))
  expect_s4_class(t3, "opAttributeInteger")
  expect_equal(t3@name, "Refills")
  expect_equal(t3@op, "eq")
  expect_equal(t3@value, 2L)



})

test_that("user op numeric work", {
  # test range High
  t4 <- rangeHigh(bt(2L, 4L))
  expect_s4_class(t4, "opAttributeNumeric")
  expect_equal(t4@name, "RangeHigh")
  expect_equal(t4@op, "bt")
  expect_equal(t4@value, 2L)
  expect_equal(t4@extent, 4L)

  # test range Low
  t5 <- rangeLow(bt(2L, 4L))
  expect_s4_class(t5, "opAttributeNumeric")
  expect_equal(t5@name, "RangeLow")
  expect_equal(t5@op, "bt")
  expect_equal(t5@value, 2L)
  expect_equal(t5@extent, 4L)

  # test range Low
  t6 <- drugQuantity(eq(2L))
  expect_s4_class(t6, "opAttributeNumeric")
  expect_equal(t6@name, "Quantity")
  expect_equal(t6@op, "eq")
  expect_equal(t6@value, 2L)
})


test_that("user op date work", {
  # test start date
  dd <- lubridate::as_date("2010-01-01")
  t1 <- startDate(gte(dd))
  expect_s4_class(t1, "opAttributeDate")
  expect_equal(t1@name, "OccurrenceStartDate")
  expect_equal(t1@op, "gte")
  expect_equal(t1@value, dd)

  # test end date
  ee <- lubridate::as_date("2020-01-01")
  t2 <- endDate(lte(ee))
  expect_s4_class(t2, "opAttributeDate")
  expect_equal(t2@name, "OccurrenceEndDate")
  expect_equal(t2@op, "lte")
  expect_equal(t2@value, ee)
})


test_that("coersion works for op", {
  t1 <- age(bt(18L, 65L)) %>%
    listOpAttribute()
  expect_named(t1, "Age")
  expect_equal(t1$Age$Extent, 65L)


  dd <- lubridate::as_date("2010-01-01")
  t1 <- startDate(gte(dd)) %>%
    listOpAttribute()
  expect_named(t1, "OccurrenceStartDate")
  expect_equal(t1$OccurrenceStartDate$Value, dd)

})

test_that("concept attributes build", {

  t1 <- female()
  expect_s4_class(t1, "conceptAttribute")
  expect_equal(t1@name, "Gender")
  expect_equal(t1@conceptSet[[1]]@concept_name, "FEMALE")
  expect_equal(t1@conceptSet[[1]]@concept_id, 8532L)

  jj <- as.list(t1)
  expect_named(jj, "Gender")
  expect_equal(jj$Gender[[1]]$CONCEPT_ID, 8532L)


  #test units
  tt <- unit(8713L) #gram per deciliter
  expect_s4_class(tt, "conceptAttribute")
  expect_equal(tt@name, "Unit")
  expect_equal(tt@conceptSet[[1]]@concept_id, 8713L)

  tt <- unit("%") #gram per deciliter
  expect_s4_class(tt, "conceptAttribute")
  expect_equal(tt@name, "unit")
  expect_equal(tt@conceptSet[[1]]@concept_id, 8554L)
  expect_equal(tt@conceptSet[[1]]@concept_name, "%")
})

test_that("logical attributes build", {

  t1 <- firstOccurrence()
  expect_s4_class(t1, "logicAttribute")
  expect_equal(t1@name, "First")

  t2 <- as.list(t1)
  expect_named(t2, "First")
  expect_equal(t2$First, TRUE)
})



test_that("dateAdjustment attributes build", {

  t1 <- dateAdjustment(startWith = "START_DATE",
                       startOffset = 30L,
                       endWith = "END_DATE",
                       endOffset = 30L)
  expect_s4_class(t1, "dateAdjustmentAttribute")
  expect_equal(t1@name, "DateAdjustment")
  expect_equal(t1@startOffset, 30L)


  t2 <- as.list(t1)
  expect_named(t2, "DateAdjustment")
  expect_equal(t2$DateAdjustment$StartOffset, 30L)
})
