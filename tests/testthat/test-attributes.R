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


  # test start date with type era
  dd <- lubridate::as_date("2010-01-01")
  t1 <- startDate(gte(dd), type = "era")
  expect_s4_class(t1, "opAttributeDate")
  expect_equal(t1@name, "EraStartDate")
  expect_equal(t1@op, "gte")
  expect_equal(t1@value, dd)

  # test end date
  ee <- lubridate::as_date("2020-01-01")
  t2 <- endDate(lte(ee))
  expect_s4_class(t2, "opAttributeDate")
  expect_equal(t2@name, "OccurrenceEndDate")
  expect_equal(t2@op, "lte")
  expect_equal(t2@value, ee)


  # test end date with type era
  ee <- lubridate::as_date("2020-01-01")
  t2 <- endDate(lte(ee), type = "era")
  expect_s4_class(t2, "opAttributeDate")
  expect_equal(t2@name, "EraEndDate")
  expect_equal(t2@op, "lte")
  expect_equal(t2@value, ee)
})


test_that("coersion works for op", {
  t1 <- age(bt(18L, 65L)) |>
    listOpAttribute()
  expect_named(t1, "Age")
  expect_equal(t1$Age$Extent, 65L)


  dd <- lubridate::as_date("2010-01-01")
  t1 <- startDate(gte(dd)) |>
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
  tt <- measurementUnit(8713L)
  expect_s4_class(tt, "conceptAttribute")
  expect_equal(tt@name, "Unit")
  expect_equal(tt@conceptSet[[1]]@concept_id, 8713L)

  tt <- measurementUnit(c(8554L, 8713L))
  expect_s4_class(tt, "conceptAttribute")
  expect_equal(tt@name, "Unit")
  expect_equal(purrr::map_int(tt@conceptSet, ~.@concept_id), c(8554L, 8713L))

  # Concept ids are the only supported input - everything else errors
  expect_error(measurementUnit(cs(8554L, name = "%")), "concept ids")
  expect_error(measurementUnit("%"))
})

test_that("type attributes build from ids without a connection", {

  # attribute names must match the Circe JSON keys exactly (case-sensitive)
  expected <- c(
    conditionType = "ConditionType",
    conditionStatus = "ConditionStatus",
    drugType = "DrugType",
    visitType = "VisitType",
    measurementType = "MeasurementType",
    observationType = "ObservationType",
    procedureType = "ProcedureType",
    deathType = "DeathType",
    deviceType = "DeviceType",
    specimenType = "SpecimenType",
    observationPeriodType = "PeriodType",
    valueAsConcept = "ValueAsConcept"
  )

  for (fn in names(expected)) {
    tt <- do.call(fn, list(32817L))
    expect_s4_class(tt, "conceptAttribute")
    expect_equal(tt@name, expected[[fn]])
    expect_equal(tt@conceptSet[[1]]@concept_id, 32817L)
    expect_true(is.na(tt@conceptSet[[1]]@concept_name))
  }

  # multiple ids
  tt <- conditionType(c(32817L, 32810L))
  expect_equal(purrr::map_int(tt@conceptSet, ~.@concept_id), c(32817L, 32810L))

  # serialization keys the attribute by its Circe name
  jj <- as.list(measurementType(32817L))
  expect_named(jj, "MeasurementType")
  expect_equal(jj$MeasurementType[[1]]$CONCEPT_ID, 32817L)

  # invalid inputs error
  expect_error(conditionType(cs(32817L, name = "EHR")), "concept ids")
  expect_error(drugType("EHR"))
})

test_that("type attributes survive Circe SQL generation", {
  skip_if_not_installed("CirceR")

  ch <- cohort(
    entry = entry(
      measurement(cs(3004410L, name = "hba1c"), measurementType(32817L), measurementUnit(8554L))
    )
  )
  sql <- CirceR::cohortExpressionFromJson(as.json(ch)) |>
    CirceR::buildCohortQuery(CirceR::createGenerateOptions(generateStats = FALSE))
  expect_true(grepl("measurement_type_concept_id", sql))
  expect_true(grepl("32817", sql))
  expect_true(grepl("unit_concept_id", sql))
  expect_true(grepl("8554", sql))

  ch <- cohort(
    entry = entry(
      observationPeriod(observationPeriodType(32817L))
    )
  )
  sql <- CirceR::cohortExpressionFromJson(as.json(ch)) |>
    CirceR::buildCohortQuery(CirceR::createGenerateOptions(generateStats = FALSE))
  expect_true(grepl("period_type_concept_id", sql))
  expect_true(grepl("32817", sql))
})

test_that("conceptSetAttribute builds", {
  test_cs <- cs(c(123, 456), name = "test source concepts")
  attr <- conditionSourceConcept(test_cs)
  
  expect_s4_class(attr, "conceptSetAttribute")
  expect_equal(attr@name, "ConditionSourceConcept")
  expect_s4_class(attr@conceptSet, "ConceptSet")
  expect_equal(attr@conceptSet@id, test_cs@id)
  
  # Test as.list conversion
  as_list <- as.list(attr)
  expect_named(as_list, "ConditionSourceConcept")
  expect_equal(as_list$ConditionSourceConcept, test_cs@id)
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

# --- New op integer attributes (era / period / visit) -------------------------

test_that("ageAtStart, ageAtEnd, visitLength, periodLength build", {
  tt <- ageAtStart(gte(18L))
  expect_s4_class(tt, "opAttributeInteger")
  expect_equal(tt@name, "AgeAtStart")
  expect_equal(tt@op, "gte")
  expect_equal(tt@value, 18L)

  tt <- ageAtEnd(lte(65L))
  expect_s4_class(tt, "opAttributeInteger")
  expect_equal(tt@name, "AgeAtEnd")
  expect_equal(tt@op, "lte")
  expect_equal(tt@value, 65L)

  tt <- visitLength(gt(1L))
  expect_s4_class(tt, "opAttributeInteger")
  expect_equal(tt@name, "VisitLength")
  expect_equal(tt@op, "gt")
  expect_equal(tt@value, 1L)

  tt <- periodLength(bt(30L, 365L))
  expect_s4_class(tt, "opAttributeInteger")
  expect_equal(tt@name, "PeriodLength")
  expect_equal(tt@op, "bt")
  expect_equal(tt@value, 30L)
  expect_equal(tt@extent, 365L)
})

# --- New op numeric attribute (quantityValue) ---------------------------------

test_that("quantityValue builds as opAttributeNumeric", {
  tt <- quantityValue(gt(0))
  expect_s4_class(tt, "opAttributeNumeric")
  expect_equal(tt@name, "Quantity")
  expect_equal(tt@op, "gt")
  expect_equal(tt@value, 0)

  jj <- listOpAttribute(tt)
  expect_named(jj, "Quantity")
})

# --- New text filter attributes (TextFilter / opAttributeCharacter) -----------

test_that("stopReason, uniqueDeviceId, specimenSourceId build as opAttributeCharacter", {
  tt <- stopReason(stringContains("adverse"))
  expect_s4_class(tt, "opAttributeCharacter")
  expect_equal(tt@name, "StopReason")
  expect_equal(tt@op, "contains")
  expect_equal(tt@value, "adverse")

  tt <- uniqueDeviceId(stringStartsWith("DV"))
  expect_s4_class(tt, "opAttributeCharacter")
  expect_equal(tt@name, "UniqueDeviceId")
  expect_equal(tt@op, "startsWith")

  tt <- specimenSourceId(stringEndsWith("123"))
  expect_s4_class(tt, "opAttributeCharacter")
  expect_equal(tt@name, "SourceId")
  expect_equal(tt@op, "endsWith")
})

# --- New demographic concept attributes (race, ethnicity) ---------------------

test_that("raceConcepts and ethnicityConcepts build as conceptAttribute", {
  tt <- raceConcepts(8527L)
  expect_s4_class(tt, "conceptAttribute")
  expect_equal(tt@name, "Race")
  expect_equal(tt@conceptSet[[1]]@concept_id, 8527L)

  tt <- raceConcepts(c(8527L, 8516L))
  expect_equal(purrr::map_int(tt@conceptSet, ~.@concept_id), c(8527L, 8516L))

  jj <- as.list(tt)
  expect_named(jj, "Race")

  tt <- ethnicityConcepts(38003563L)
  expect_s4_class(tt, "conceptAttribute")
  expect_equal(tt@name, "Ethnicity")
  expect_equal(tt@conceptSet[[1]]@concept_id, 38003563L)

  jj <- as.list(tt)
  expect_named(jj, "Ethnicity")
})

# --- New concept array attributes (domain-specific) --------------------------

test_that("new domain concept attributes build with correct Circe names", {
  expected <- c(
    routeConcept         = "RouteConcept",
    doseUnit             = "DoseUnit",
    measurementOperator  = "Operator",
    observationQualifier = "Qualifier",
    procedureModifier    = "Modifier",
    placeOfService       = "PlaceOfService",
    specimenAnatomicSite = "AnatomicSite",
    specimenDiseaseStatus = "DiseaseStatus"
  )
  for (fn in names(expected)) {
    tt <- do.call(fn, list(99999L))
    expect_s4_class(tt, "conceptAttribute")
    expect_equal(tt@name, expected[[fn]])
    expect_equal(tt@conceptSet[[1]]@concept_id, 99999L)
  }
})

# --- New TypeExclude boolean attributes ---------------------------------------

test_that("TypeExclude keyValueAttributes build with correct names and values", {
  expected <- c(
    drugTypeExclude        = "DrugTypeExclude",
    deviceTypeExclude      = "DeviceTypeExclude",
    observationTypeExclude = "ObservationTypeExclude",
    procedureTypeExclude   = "ProcedureTypeExclude",
    visitTypeExclude       = "VisitTypeExclude"
  )
  for (fn in names(expected)) {
    tt <- do.call(fn, list(FALSE))
    expect_s4_class(tt, "keyValueAttribute")
    expect_equal(tt@name, expected[[fn]])
    expect_false(tt@value)

    tt_true <- do.call(fn, list(TRUE))
    expect_true(tt_true@value)

    jj <- as.list(tt)
    expect_named(jj, expected[[fn]])
    expect_false(jj[[expected[[fn]]]])
  }
})

# --- New conceptSetSelectionAttribute (TypeCS variants) -----------------------

test_that("conditionTypeCS builds as conceptSetSelectionAttribute", {
  cs1 <- cs(32817L, name = "EHR type")
  tt <- conditionTypeCS(cs1)
  expect_s4_class(tt, "conceptSetSelectionAttribute")
  expect_equal(tt@name, "ConditionTypeCS")
  expect_false(tt@isExclusion)

  tt_excl <- conditionTypeCS(cs1, isExclusion = TRUE)
  expect_true(tt_excl@isExclusion)

  jj <- as.list(tt)
  expect_named(jj, "ConditionTypeCS")
  expect_false(jj$ConditionTypeCS$IsExclusion)
  expect_equal(jj$ConditionTypeCS$CodesetId, cs1@id)
})

test_that("all TypeCS functions build with correct Circe names", {
  cs1 <- cs(99999L, name = "test")
  fns <- c("conditionTypeCS", "drugTypeCS", "measurementTypeCS",
           "observationTypeCS", "procedureTypeCS", "deviceTypeCS",
           "deathTypeCS", "specimenTypeCS", "visitTypeCS",
           "periodTypeCS", "genderCS", "raceCS", "ethnicityCS",
           "unitCS", "doseUnitCS", "routeConceptCS",
           "measurementOperatorCS", "observationQualifierCS",
           "procedureModifierCS", "placeOfServiceCS",
           "providerSpecialtyCS", "conditionStatusCS",
           "specimenAnatomicSiteCS", "specimenDiseaseStatusCS",
           "visitDetailTypeCS")
  for (fn in fns) {
    tt <- do.call(fn, list(cs1))
    expect_s4_class(tt, "conceptSetSelectionAttribute",
                    label = paste(fn, "returns conceptSetSelectionAttribute"))
    jj <- as.list(tt)
    expect_true("CodesetId" %in% names(jj[[tt@name]]),
                label = paste(fn, "serializes CodesetId"))
    expect_true("IsExclusion" %in% names(jj[[tt@name]]),
                label = paste(fn, "serializes IsExclusion"))
  }
})

# --- PayerPlanPeriod concept attributes (integer CodesetId references) --------

test_that("payerPlanPeriod concept reference attributes build", {
  cs1 <- cs(99999L, name = "payer")
  expected <- c(
    payerConcept           = "PayerConcept",
    planConcept            = "PlanConcept",
    sponsorConcept         = "SponsorConcept",
    stopReasonConcept      = "StopReasonConcept",
    payerSourceConcept     = "PayerSourceConcept",
    planSourceConcept      = "PlanSourceConcept",
    sponsorSourceConcept   = "SponsorSourceConcept",
    stopReasonSourceConcept = "StopReasonSourceConcept"
  )
  for (fn in names(expected)) {
    tt <- do.call(fn, list(cs1))
    expect_s4_class(tt, "conceptSetAttribute",
                    label = paste(fn, "returns conceptSetAttribute"))
    expect_equal(tt@name, expected[[fn]])
  }
})
