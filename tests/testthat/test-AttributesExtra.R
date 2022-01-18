library(Capr)

#function used for all op
mapOperator <- function(op){
  opdf <- data.frame(symb = c("<", "<=", ">", ">=", "==", "--", "!-"),
                     text = c("less than", "less than or equal to", "greater than",
                              "greater than or equal to", "equal to", "between", "not between"),
                     short = c("lt", "lte", "gt", "gte", "eq", "bt", "!bt"),
                     idx = 1:7,
                     stringsAsFactors = FALSE)
  jval <- c("lt", "lte", "gt", "gte", "eq", "bt", "!bt")

  jval[which(apply(opdf, 2, "%in%", table = op), arr.ind = T)[1]]
}


test_that("test listAttributeOptions",{
  attOp <- listAttributeOptions()
  expect_equal(class(attOp), "list")

  attOpCO <- listAttributeOptions(domain = "ConditionOccurrence")
  expect_true(length(attOpCO) == 11)
})

## Test all date op attributes -----------------------------
test_that("test createOccurrenceStartDateAttribute", {
  DateAtt <- createOccurrenceStartDateAttribute(Op = "gte", Value = "2018-12-31")

  expect_s4_class(DateAtt, "Component") #check its component class
  expect_s4_class(DateAtt@CriteriaExpression[[1]], DateAtt@MetaData@Name)
  expect_equal(componentType(DateAtt), "Attribute")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Value, "2018-12-31")

  #check for error returns
  expect_error(createOccurrenceStartDateAttribute(Op = "gte", Value = 18L))
  expect_error(createOccurrenceStartDateAttribute(Op = "gte",
                                            Value = "2018-12-31",
                                            Extent = "2019-12-31"))
  expect_error(createOccurrenceStartDateAttribute(Op = "bt", Value = "2018-12-31"))
  expect_error(createOccurrenceStartDateAttribute(Op = "bt",
                                                  Value = "2018-12-31",
                                                  Extent = 18L))
})


test_that("test createOccurrenceEndDateAttribute", {
  DateAtt <- createOccurrenceEndDateAttribute(Op = "gte", Value = "2018-12-31")
  expect_s4_class(DateAtt, "Component") #check its component class
  expect_s4_class(DateAtt@CriteriaExpression[[1]], DateAtt@MetaData@Name)
  expect_equal(componentType(DateAtt), "Attribute")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Value, "2018-12-31")

  #check for error returns
  expect_error(createOccurrenceEndDateAttribute(Op = "gte", Value = 18L))
  expect_error(createOccurrenceEndDateAttribute(Op = "gte",
                                            Value = "2018-12-31",
                                            Extent = "2019-12-31"))
  expect_error(createOccurrenceEndDateAttribute(Op = "bt", Value = "2018-12-31"))
  expect_error(createOccurrenceEndDateAttribute(Op = "bt",
                                                  Value = "2018-12-31",
                                                  Extent = 18L))
})


test_that("test createEraStartDateAttribute", {
  DateAtt <- createEraStartDateAttribute(Op = "gte", Value = "2018-12-31")
  expect_s4_class(DateAtt, "Component") #check its component class
  expect_s4_class(DateAtt@CriteriaExpression[[1]], DateAtt@MetaData@Name)
  expect_equal(componentType(DateAtt), "Attribute")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Value, "2018-12-31")

  #check for error returns
  expect_error(createEraStartDateAttribute(Op = "gte", Value = 18L))
  expect_error(createEraStartDateAttribute(Op = "gte",
                                            Value = "2018-12-31",
                                            Extent = "2019-12-31"))
  expect_error(createEraStartDateAttribute(Op = "bt", Value = "2018-12-31"))
  expect_error(createEraStartDateAttribute(Op = "bt",
                                                  Value = "2018-12-31",
                                                  Extent = 18L))
})


test_that("test createEraEndDateAttribute", {
  DateAtt <- createEraEndDateAttribute(Op = "gte", Value = "2018-12-31")
  expect_s4_class(DateAtt, "Component") #check its component class
  expect_s4_class(DateAtt@CriteriaExpression[[1]], DateAtt@MetaData@Name)
  expect_equal(componentType(DateAtt), "Attribute")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Value, "2018-12-31")

  #check for error returns
  expect_error(createEraEndDateAttribute(Op = "gte", Value = 18L))
  expect_error(createEraEndDateAttribute(Op = "gte",
                                            Value = "2018-12-31",
                                            Extent = "2019-12-31"))
  expect_error(createEraEndDateAttribute(Op = "bt", Value = "2018-12-31"))
  expect_error(createEraEndDateAttribute(Op = "bt",
                                                  Value = "2018-12-31",
                                                  Extent = 18L))
})

test_that("test createPeriodStartDateAttribute", {
  DateAtt <- createPeriodStartDateAttribute(Op = "gte", Value = "2018-12-31")
  expect_s4_class(DateAtt, "Component") #check its component class
  expect_s4_class(DateAtt@CriteriaExpression[[1]], DateAtt@MetaData@Name)
  expect_equal(componentType(DateAtt), "Attribute")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Value, "2018-12-31")

  #check for error returns
  expect_error(createPeriodStartDateAttribute(Op = "gte", Value = 18L))
  expect_error(createPeriodStartDateAttribute(Op = "gte",
                                            Value = "2018-12-31",
                                            Extent = "2019-12-31"))
  expect_error(createPeriodStartDateAttribute(Op = "bt", Value = "2018-12-31"))
  expect_error(createPeriodStartDateAttribute(Op = "bt",
                                                  Value = "2018-12-31",
                                                  Extent = 18L))
})

test_that("test createPeriodEndDateAttribute", {
  DateAtt <- createPeriodEndDateAttribute(Op = "gte", Value = "2018-12-31")
  expect_s4_class(DateAtt, "Component") #check its component class
  expect_s4_class(DateAtt@CriteriaExpression[[1]], DateAtt@MetaData@Name)
  expect_equal(componentType(DateAtt), "Attribute")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Value, "2018-12-31")

  #check for error returns
  expect_error(createPeriodEndDateAttribute(Op = "gte", Value = 18L))
  expect_error(createPeriodEndDateAttribute(Op = "gte",
                                            Value = "2018-12-31",
                                            Extent = "2019-12-31"))
  expect_error(createPeriodEndDateAttribute(Op = "bt", Value = "2018-12-31"))
  expect_error(createPeriodEndDateAttribute(Op = "bt",
                                                  Value = "2018-12-31",
                                                  Extent = 18L))
})

### Test all Numeric Ranges --------------------------

test_that("test createAgeAttribute", {

  NumAtt <- createAgeAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createEraLengthAttribute", {

  NumAtt <- createEraLengthAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createAgeAtStartAttribute", {

  NumAtt <- createAgeAtStartAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createAgeAtEndAttribute", {

  NumAtt <- createAgeAtEndAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})

test_that("test createGapDaysAttribute", {

  NumAtt <- createGapDaysAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})

test_that("test createRefillsAttribute", {

  NumAtt <- createRefillsAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createQuantityAttribute", {

  NumAtt <- createQuantityAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createDaysSupplyAttribute", {

  NumAtt <- createDaysSupplyAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createEffectiveDrugDoseAttribute", {

  NumAtt <- createEffectiveDrugDoseAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createValueAsNumberAttribute", {

  NumAtt <- createValueAsNumberAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})

test_that("test createRangeLowAttribute", {

  NumAtt <- createRangeLowAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createRangeHighAttribute", {

  NumAtt <- createRangeHighAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})


test_that("test createRangeHighRatioAttribute", {

  NumAtt <- createRangeHighRatioAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})

test_that("test createRangeLowRatioAttribute", {

  NumAtt <- createRangeLowRatioAttribute(Op = "gte", Value = 18L)
  # check values in NumAtt are set
  expect_s4_class(NumAtt, "Component") #check its component class
  expect_s4_class(NumAtt@CriteriaExpression[[1]], NumAtt@MetaData@Name)
  expect_equal(componentType(NumAtt), "Attribute")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(NumAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)
})

## Test logical ------------------------
test_that("test FirstAttribute", {
  logicAtt <- createFirstAttribute()
  expect_true(logicAtt@CriteriaExpression[[1]]@Logic)
  expect_equal(logicAtt@CriteriaExpression[[1]]@Name, "First")
})

