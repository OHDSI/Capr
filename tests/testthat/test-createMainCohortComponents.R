library(Capr)
test_that("Creation of Primary Criteria is correct",{
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
                              ObservationWindow = createObservationWindow(PriorDays = 90L, PostDays = 15L),
                              Limit = "All")
  
  # check values in query1 are set
  expect_s4_class(pc, "Component") #check its component class
  expect_s4_class(pc@CriteriaExpression$CriteriaList[[1]], "Query")
  expect_s4_class(pc@CriteriaExpression[[2]], "ObservationWindow")
  expect_equal(componentType(pc), "PrimaryCriteria")
  expect_equal(checkConceptIds(pc)[[1]], IpVisit$conceptId)
  expect_equal(pc@CriteriaExpression$ObservationWindow@PriorDays, 90L)
  expect_equal(pc@Limit$PrimaryCriteriaLimit@Type, "All")
})

test_that("Creation of Additional Criteria is Correct", {
  ## Create First count No t2DM -------------------
  #get concept set for type 2 diabetes mellitus
  T2DMDx <- readRDS("resources/conceptSetsList.rds")$T2DMDx
  #create query
  T2DMDxQuery <- T2DMDx %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Mellitus Diagnosis",
                               includeDescendants = TRUE) %>%
    createConditionOccurrence(conceptSetExpression = .)
  
  # create a timeline 1
  Timeline1 <- createWindow(StartDays = "All",
                            StartCoeff = "Before",
                            EndDays = 0L,
                            EndCoeff = "After") %>%
    createTimeline(StartWindow = .)
  
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
  
  ac <- createAdditionalCriteria(Name = "T2DM AC",
                                 Contents = T2DMGroup,
                                 Limit = "All")
  
  # check values in ac are set
  expect_s4_class(ac, "Component") #check its component class
  expect_s4_class(ac@CriteriaExpression[[1]], "Group")
  expect_equal(componentType(ac), "AdditionalCriteria")
  expect_equal(checkConceptIds(ac)[[2]], T2DMMeds$conceptId)
  expect_equal(ac@Limit$QualifiedLimit@Type, "All")
})

test_that("Creation of Inclusion Rules is correct", {
  ## Create First count No t2DM -------------------
  #get concept set for type 2 diabetes mellitus
  T2DMDx <- readRDS("resources/conceptSetsList.rds")$T2DMDx
  #create query
  T2DMDxQuery <- T2DMDx %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Mellitus Diagnosis",
                               includeDescendants = TRUE) %>%
    createConditionOccurrence(conceptSetExpression = .)
  
  # create a timeline 1
  Timeline1 <- createWindow(StartDays = "All",
                            StartCoeff = "Before",
                            EndDays = 0L,
                            EndCoeff = "After") %>%
    createTimeline(StartWindow = .)
  
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
  
  # check values in irs are set
  expect_s4_class(irs, "Component") #check its component class
  expect_equal(componentType(irs@CriteriaExpression[[1]]), "Group")
  expect_equal(componentType(irs), "InclusionRules")
  #ensure the inclusion rules component has no cse.
  expect_equal(checkConceptIds(irs), list())
  #the individual rules contains the cses
  expect_equal(checkConceptIds(irs@CriteriaExpression[[1]])[[2]], T2DMMeds$conceptId)
  #check the limit
  expect_equal(irs@Limit$ExpressionLimit@Type, "All")
})

test_that("Create Censoring Criteria is correct", {
  
  ## Create second Count at least 1 t2dm rx occurrence ---------
  T2DMMeds <- readRDS("resources/conceptSetsList.rds")$T2DMMeds
  
  #concept set expression with all concepts include decesendants
  glimepirideQuery <- T2DMMeds %>%
    filter(conceptName == "glimepiride") %>%
    createConceptSetExpression(Name = "Glimepiride Rx",
                               includeDescendants = TRUE) %>%
    createDrugExposure(conceptSetExpression = .)
  
  cen <- createCensoringCriteria(Name = "Censoring Criteria for Glimepiride",
                                 ComponentList = list(glimepirideQuery))
  # check values in cen are set
  expect_s4_class(cen, "Component") #check its component class
  expect_s4_class(cen@CriteriaExpression[[1]], "Query")
  expect_equal(componentType(cen), "CensoringCriteria")
  #ensure the inclusion rules component has no cse.
  expect_equal(checkConceptIds(cen)[[1]], 1597756)
  
})

test_that("Check that Cohort Era is created correctly",{
  cera <- createCohortEra(EraPadDays = 3L,
                          LeftCensorDate = "2014-01-01",
                          RightCensorDate = "2019-12-31")
  
  #check that cera is correct
  expect_s4_class(cera, "Component")
  expect_equal(componentType(cera), "CohortEra")
  expect_s4_class(cera@CriteriaExpression$CollapseSettings, "CollapseSettings")
  expect_equal(cera@CriteriaExpression$CollapseSetting@EraPad, 3L)
  expect_s4_class(cera@CriteriaExpression$CensorWindow, "CensorWindow")
  expect_equal(cera@CriteriaExpression$CensorWindow@StartDate, "2014-01-01")
})