#context("Create Cohort Components")#depreciated testthat v3
library(Capr)

test_that("creation of windows and timelines is correct", {

  #create observationWindow
  ow <- createObservationWindow(PriorDays = 7L,
                                PostDays = 14L)

  #Create a start Window
  StartWindow1 <- createWindow(StartDays = 21, StartCoeff = "Before",#start 21 days before index start
                               EndDays = "All",EndCoeff = "After", #end all days after index start
                               EventStarts = TRUE, #event starts if true
                               IndexStart = TRUE) #using the index event as the start point

  #create an endwindow
  EndWindow1 <- createWindow(StartDays = "All", StartCoeff = "Before", #end all days before index start
                             EndDays = 0, EndCoeff = "After", #0 days after index start
                             EventStarts = FALSE, #event end if false
                             IndexStart = TRUE) #using the index event as the start point

  # create a timeline
  Timeline1 <- createTimeline(StartWindow = StartWindow1, #start window of timeline
                              EndWindow = EndWindow1, #end window of timeline
                              RestrictVisit = FALSE, # do not restrict to same visit
                              IgnoreObservationPeriod = FALSE) # do not ignore observation period

  #check values in observation window are set


  # check values in StartWindow are set
  expect_s4_class(ow, "ObservationWindow")
  expect_equal(ow@PriorDays, 7L)
  expect_equal(ow@PostDays, 14L)

  # check values in EndWindow are set
  expect_s4_class(EndWindow1, "Window")
  expect_equal(EndWindow1@Event, "EventEnds")
  expect_equal(EndWindow1@Start$Days, "All")

  # check values in Timeline are set
  expect_s4_class(Timeline1, "Timeline")
  expect_s4_class(Timeline1@StartWindow, "Window")
  expect_false(Timeline1@RestrictVisit)
  expect_equal(Timeline1@StartWindow@Start$Days, 21L)

})

#tests for concept set expression creation
test_that("creation of concept set expressions is correct", {

  T2DMMeds <- readRDS("resources/conceptSetsList.rds")$T2DMMeds

  #concept set expression with all concepts include decesendants
  T2DMMedsCSE1 <- T2DMMeds %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Meds",
                               includeDescendants = TRUE)
  #get logic for include Descendants in cse1
  includeDescendantsLogic1 <- purrr::map_lgl(T2DMMedsCSE1@ConceptSetExpression[[1]]@Expression,
                                            ~slot(.x, "includeDescendants"))

  #get logic for is excluded in cse 1
  isExcludedLogic1 <- purrr::map_lgl(T2DMMedsCSE1@ConceptSetExpression[[1]]@Expression,
                                     ~slot(.x, "isExcluded"))

  #concept set expression with custom mapping
  conceptMapping <- createConceptMapping(n = nrow(T2DMMeds)) %>% #set all to no mapping
    toggleConceptMapping(pos = seq(2, nrow(T2DMMeds), by = 2),
                         mapping = "includeDescendants") %>% #map even entries to include decendancts
    toggleConceptMapping(pos = seq(1, nrow(T2DMMeds) - 1, by = 2),  #map odd entries to exclude
                         mapping = "isExcluded")
  #create cse 2
  T2DMMedsCSE2 <- T2DMMeds %>%
    createConceptSetExpressionCustom(Name = "Type 2 Diabetes Meds",
                                     conceptMapping = conceptMapping)
  #get logic for include Descendants in cse2
  includeDescendantsLogic2 <- purrr::map_lgl(T2DMMedsCSE2@ConceptSetExpression[[1]]@Expression,
                                             ~slot(.x, "includeDescendants"))

  #get logic for is excluded in cse 1
  isExcludedLogic2 <- purrr::map_lgl(T2DMMedsCSE2@ConceptSetExpression[[1]]@Expression,
                                     ~slot(.x, "isExcluded"))

  # check values in cse1 are set
  expect_s4_class(T2DMMedsCSE1, "Component")
  expect_equal(componentType(T2DMMedsCSE1), "ConceptSetExpression")
  expect_equal(checkConceptIds(T2DMMedsCSE1)[[1]], T2DMMeds$conceptId)
  expect_identical(includeDescendantsLogic1, rep(T, nrow(T2DMMeds)))
  expect_identical(isExcludedLogic1, rep(F, nrow(T2DMMeds)))

  # check values in cse2 are set
  expect_s4_class(T2DMMedsCSE2, "Component")
  expect_equal(componentType(T2DMMedsCSE2), "ConceptSetExpression")
  expect_equal(checkConceptIds(T2DMMedsCSE2)[[1]], T2DMMeds$conceptId)
  expect_identical(includeDescendantsLogic2,
                   purrr::map_lgl(conceptMapping, ~getElement(.x, "includeDescendants")))
  expect_identical(isExcludedLogic2,
                   purrr::map_lgl(conceptMapping, ~getElement(.x, "isExcluded")))

})

test_that("Query Components are generated correctly", {

  #get concepts for inpatient visit
  IpVisit <- readRDS("resources/conceptSetsList.rds")$visitCodes

  #create inpatient visit query
  IpVisitQuery <- IpVisit %>%
    createConceptSetExpression(Name = "Inpatient Visit",
                               includeDescendants = TRUE) %>%
    createVisitOccurrence()

  # check values in query1 are set
  expect_s4_class(IpVisitQuery, "Component") #check its component class
  expect_s4_class(IpVisitQuery@CriteriaExpression[[1]], "Query")
  expect_equal(componentType(IpVisitQuery), "Query")
  expect_equal(checkConceptIds(IpVisitQuery)[[1]], IpVisit$conceptId)
  expect_equal(IpVisitQuery@CriteriaExpression[[1]]@Domain, "VisitOccurrence")


})


test_that("Count Components are generated correctly", {

  #get concept set for type 2 diabetes mellitus
  T2DMDx <- readRDS("resources/conceptSetsList.rds")$T2DMDx
  #create query
  T2DMDxQuery <- T2DMDx %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Mellitus Diagnosis",
                               includeDescendants = TRUE) %>%
    createConditionOccurrence(conceptSetExpression = .)

  # create a timeline
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

  # check values in count are set
  expect_s4_class(T2DMDxCount, "Component") #check its component class
  expect_s4_class(T2DMDxCount@CriteriaExpression[[1]], "Count")
  #check the component type is a count
  expect_equal(componentType(T2DMDxCount), "Count")
  #check that the concept ids match the original input concept set
  expect_equal(checkConceptIds(T2DMDxCount)[[1]], T2DMDx$conceptId)
  #check that occurrence is exactly 0
  expect_equal(T2DMDxCount@CriteriaExpression[[1]]@Occurrence@Type, "exactly")
  expect_equal(T2DMDxCount@CriteriaExpression[[1]]@Occurrence@Count, 0L)
  # check values in Timeline are set
  expect_s4_class(T2DMDxCount@CriteriaExpression[[1]]@Timeline, "Timeline")
  expect_s4_class(T2DMDxCount@CriteriaExpression[[1]]@Timeline@StartWindow, "Window")
})


test_that("Group Components are generated Correctly",{

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

  ## Create demographic criteria
  AgeAtt <- createAgeAttribute(Op = "gte", Value = 18L)

  ## Create Measurement group
  HbA1cMeas <- readRDS("resources/conceptSetsList.rds")$HbA1cMeas
  HbA1cQuery <- HbA1cMeas %>%
    createConceptSetExpression(Name = "HbA1c Measure",
                               includeDescendants = TRUE) %>%
    createMeasurement(conceptSetExpression = .,
                      attributeList = list(createValueAsNumberAttribute(Op = "gte", Value = 6L)))
  HbA1cCount <- createCount(Query = HbA1cQuery,
                            Logic = "at_least",
                            Count = 1L,
                            Timeline = Timeline1)


  T2DMFullGroup <- createGroup(Name = "T2DM Full Group",
                               type = "ALL",
                               criteriaList = list(HbA1cCount),
                               demographicCriteriaList = list(AgeAtt),
                               Groups = list(T2DMGroup))

  # check values in T2DMGroup are set
  expect_s4_class(T2DMGroup, "Component") #check its component class
  expect_s4_class(T2DMGroup@CriteriaExpression[[1]], "Group")
  #check the component type is a count
  expect_equal(componentType(T2DMGroup), "Group")
  #check that the concept ids match the original input concept set
  expect_equal(checkConceptIds(T2DMGroup)[[1]], T2DMDx$conceptId)
  #expect_equal(checkConceptIds(T2DMGroup)[[2]], T2DMMeds$conceptId)

  #check that group type is all
  expect_equal(T2DMGroup@CriteriaExpression[[1]]@Type@Type, "ALL")

  #check values in full group properly set
  expect_s4_class(T2DMFullGroup, "Component") #check its component class
  expect_s4_class(T2DMFullGroup@CriteriaExpression[[1]], "Group")
  #check the component type is a count
  expect_equal(componentType(T2DMFullGroup), "Group")
  #check Demogrpahic Criteria
  expect_s4_class(T2DMFullGroup@CriteriaExpression[[1]]@DemographicCriteriaList[[1]],
                  is(AgeAtt@CriteriaExpression[[1]]))
  #check sub group is a group
  expect_s4_class(T2DMFullGroup@CriteriaExpression[[1]]@Groups[[1]],
                  is(T2DMGroup@CriteriaExpression[[1]]))


})


test_that("Check create end strategy options",{

  #create date offset end strategy
  es1 <- createDateOffsetEndStrategy(offset = 21L,
                              eventDateOffset = "StartDate")

  #create custom era end strategy
  T2DMMeds <- readRDS("resources/conceptSetsList.rds")$T2DMMeds

  #concept set expression with all concepts include decesendants
  T2DMMedsCSE <- T2DMMeds %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Meds",
                               includeDescendants = TRUE)
  es2 <- createCustomEraEndStrategy(ConceptSetExpression = T2DMMedsCSE,
                                    gapDays = 3L,
                                    offset = 14L)

  #check that end strategy 1 component is correct
  expect_s4_class(es1, "Component") #check its component class
  expect_s4_class(es1@CriteriaExpression[[1]], "DateOffsetEndStrategy")
  #check the component type is a count
  expect_equal(componentType(es1), "EndStrategy")
  #check offset days is expected
  expect_equal(es1@CriteriaExpression[[1]]@Offset, 21L)
  expect_equal(es1@CriteriaExpression[[1]]@DateField, "StartDate")


  #check that end strategy 2 component is correct
  expect_s4_class(es2, "Component") #check its component class
  expect_s4_class(es2@CriteriaExpression[[1]], "CustomEraEndStrategy")
  #check the component type is a count
  expect_equal(componentType(es2), "EndStrategy")
  #check offset days is expected
  expect_equal(es2@CriteriaExpression[[1]]@Offset, 14L)
  expect_equal(es2@CriteriaExpression[[1]]@GapDays, 3L)
  expect_equal(checkConceptIds(es2)[[1]], T2DMMeds$conceptId)
})
