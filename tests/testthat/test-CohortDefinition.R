library(Capr)

test_that("Create Cohort Definition", {

  ## Create Primary Criteria -------------------------

  #get concepts for inpatient visit
  IpVisit <- readRDS("resources/conceptSetsList.rds")$visitCodes

  #create inpatient visit query
  IpVisitQuery <- IpVisit %>%
    createConceptSetExpression(Name = "Inpatient Visit",
                               includeDescendants = TRUE) %>%
    createVisitOccurrence()

  #create primary criteira
  pc <- createPrimaryCriteria(Name = "Inpatient Visit Primary Criteria",
                              ComponentList = list(IpVisitQuery),
                              ObservationWindow = createObservationWindow(PriorDays = 90L,
                                                                          PostDays = 15L),
                              Limit = "All")

  ## Create Additional Criteria -------------------------
  ## Create demographic criteria
  AgeAtt <- createAgeAttribute(Op = "gte", Value = 18L)

  ## Create Measurement group
  HbA1cMeas <- readRDS("resources/conceptSetsList.rds")$HbA1cMeas
  HbA1cQuery <- HbA1cMeas %>%
    createConceptSetExpression(Name = "HbA1c Measure",
                               includeDescendants = TRUE) %>%
    createMeasurement(conceptSetExpression = .,
                      attributeList = list(createValueAsNumberAttribute(Op = "gte", Value = 6L)))

  # create a timeline 1
  Timeline1 <- createWindow(StartDays = "All",
                            StartCoeff = "Before",
                            EndDays = 0L,
                            EndCoeff = "After") %>%
    createTimeline(StartWindow = .)

  HbA1cCount <- createCount(Query = HbA1cQuery,
                            Logic = "at_least",
                            Count = 1L,
                            Timeline = Timeline1)

  AdultHbA1cGroup <- createGroup(Name = "Adult HbA1c Group",
                               type = "ALL",
                               criteriaList = list(HbA1cCount),
                               demographicCriteriaList = list(AgeAtt))

  ac <- createAdditionalCriteria(Name = "Adult HbA1c AC",
                                 Contents = AdultHbA1cGroup,
                                 Limit = "All")


  ## Create Inclusion Rules -----------------------------

  ## Create First count No t2DM -------------------
  #get concept set for type 2 diabetes mellitus
  T2DMDx <- readRDS("resources/conceptSetsList.rds")$T2DMDx
  #create query
  T2DMDxQuery <- T2DMDx %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Mellitus Diagnosis",
                               includeDescendants = TRUE) %>%
    createConditionOccurrence(conceptSetExpression = .)


  #create Count
  T2DMDxCount <- createCount(Query = T2DMDxQuery,
                             Logic = "exactly",
                             Count = 0L,
                             Timeline = Timeline1,
                             Name = "Type 2 DM Dx Count",
                             Description = "A count component identifying no presence of type 2 diabetes before initial event and 0 days after")

  ## Create second Count at least 1 t2dm rx occurrence ---------
  T2DMMeds <- readRDS("resources/conceptSetsList.rds")$T2DMMeds

  #concept set expression with all concepts include decesendants
  T2DMMedsQuery <- T2DMMeds %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Meds",
                               includeDescendants = TRUE) %>%
    createDrugExposure(conceptSetExpression = .)

  # create a timeline 2
  Timeline2 <- createWindow(StartDays = "All",
                            StartCoeff = "Before",
                            EndDays = "All",
                            EndCoeff = "After") %>%
    createTimeline(StartWindow = .)

  #create Count
  T2DMRxCount <- createCount(Query = T2DMMedsQuery,
                             Logic = "at_least",
                             Count = 1L,
                             Timeline = Timeline2,
                             Name = "Type 2 DM Rx Count",
                             Description = "A count component identifying presence of at least 1 exposure to a T2DMRx any time")

  ## Create group component -----------------
  T2DMGroup <- createGroup(Name = "T2DMGroup",
                           type = "ALL",
                           criteriaList = list(T2DMDxCount, T2DMRxCount))

  irs <- createInclusionRules(Name = "T2DM Inclusion Rules",
                              Contents = list(T2DMGroup),
                              Limit = "All")

  ## Create End Strategy --------------------

  #create date offset end strategy
  es <- createDateOffsetEndStrategy(offset = 21L,
                                     eventDateOffset = "StartDate")

  ## Create Cohort Definition ---------------

  cd <- createCohortDefinition(Name = "Inpatients who take T2DM medications",
                               PrimaryCriteria = pc,
                               AdditionalCriteria = ac,
                               InclusionRules = irs,
                               EndStrategy = es)

  expect_s4_class(cd, "CohortDefinition")
  expect_equal(componentType(cd@PrimaryCriteria), "PrimaryCriteria")
  expect_equal(componentType(cd@AdditionalCriteria), "AdditionalCriteria")
  expect_equal(componentType(cd@InclusionRules), "InclusionRules")
  expect_equal(componentType(cd@EndStrategy), "EndStrategy")
  expect_equal(componentType(cd@CohortEra), "CohortEra")

})

test_that("cohort definition compiles", {

  ## Create Primary Criteria -------------------------

  #get concepts for inpatient visit
  IpVisit <- readRDS("resources/conceptSetsList.rds")$visitCodes

  #create inpatient visit query
  IpVisitQuery <- IpVisit %>%
    createConceptSetExpression(Name = "Inpatient Visit",
                               includeDescendants = TRUE) %>%
    createVisitOccurrence()

  #create primary criteira
  pc <- createPrimaryCriteria(Name = "Inpatient Visit Primary Criteria",
                              ComponentList = list(IpVisitQuery),
                              ObservationWindow = createObservationWindow(PriorDays = 90L,
                                                                          PostDays = 15L),
                              Limit = "All")

  ## Create Additional Criteria -------------------------
  ## Create demographic criteria
  AgeAtt <- createAgeAttribute(Op = "gte", Value = 18L)

  ## Create Measurement group
  HbA1cMeas <- readRDS("resources/conceptSetsList.rds")$HbA1cMeas
  HbA1cQuery <- HbA1cMeas %>%
    createConceptSetExpression(Name = "HbA1c Measure",
                               includeDescendants = TRUE) %>%
    createMeasurement(conceptSetExpression = .,
                      attributeList = list(createValueAsNumberAttribute(Op = "gte", Value = 6L)))

  # create a timeline 1
  Timeline1 <- createWindow(StartDays = "All",
                            StartCoeff = "Before",
                            EndDays = 0L,
                            EndCoeff = "After") %>%
    createTimeline(StartWindow = .)

  HbA1cCount <- createCount(Query = HbA1cQuery,
                            Logic = "at_least",
                            Count = 1L,
                            Timeline = Timeline1)

  AdultHbA1cGroup <- createGroup(Name = "Adult HbA1c Group",
                                 type = "ALL",
                                 criteriaList = list(HbA1cCount),
                                 demographicCriteriaList = list(AgeAtt))

  ac <- createAdditionalCriteria(Name = "Adult HbA1c AC",
                                 Contents = AdultHbA1cGroup,
                                 Limit = "All")


  ## Create Inclusion Rules -----------------------------

  ## Create First count No t2DM -------------------
  #get concept set for type 2 diabetes mellitus
  T2DMDx <- readRDS("resources/conceptSetsList.rds")$T2DMDx
  #create query
  T2DMDxQuery <- T2DMDx %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Mellitus Diagnosis",
                               includeDescendants = TRUE) %>%
    createConditionOccurrence(conceptSetExpression = .)


  #create Count
  T2DMDxCount <- createCount(Query = T2DMDxQuery,
                             Logic = "exactly",
                             Count = 0L,
                             Timeline = Timeline1,
                             Name = "Type 2 DM Dx Count",
                             Description = "A count component identifying no presence of type 2 diabetes before initial event and 0 days after")

  ## Create second Count at least 1 t2dm rx occurrence ---------
  T2DMMeds <- readRDS("resources/conceptSetsList.rds")$T2DMMeds

  #concept set expression with all concepts include decesendants
  T2DMMedsQuery <- T2DMMeds %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Meds",
                               includeDescendants = TRUE) %>%
    createDrugExposure(conceptSetExpression = .)

  # create a timeline 2
  Timeline2 <- createWindow(StartDays = "All",
                            StartCoeff = "Before",
                            EndDays = "All",
                            EndCoeff = "After") %>%
    createTimeline(StartWindow = .)

  #create Count
  T2DMRxCount <- createCount(Query = T2DMMedsQuery,
                             Logic = "at_least",
                             Count = 1L,
                             Timeline = Timeline2,
                             Name = "Type 2 DM Rx Count",
                             Description = "A count component identifying presence of at least 1 exposure to a T2DMRx any time")

  ## Create group component -----------------
  T2DMGroup <- createGroup(Name = "T2DMGroup",
                           type = "ALL",
                           criteriaList = list(T2DMDxCount, T2DMRxCount))

  irs <- createInclusionRules(Name = "T2DM Inclusion Rules",
                              Contents = list(T2DMGroup),
                              Limit = "All")

  ## Create End Strategy --------------------

  #create date offset end strategy
  es <- createDateOffsetEndStrategy(offset = 21L,
                                    eventDateOffset = "StartDate")

  ## Create Cohort Definition ---------------

  cd <- createCohortDefinition(Name = "Inpatients who take T2DM medications",
                               PrimaryCriteria = pc,
                               AdditionalCriteria = ac,
                               InclusionRules = irs,
                               EndStrategy = es)

  #compile cohort definition
  cdCompile <- compileCohortDefinition(CohortDefinition = cd)
})
