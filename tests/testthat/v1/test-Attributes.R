library(Capr)

test_that("Op Attribute function works properly", {
  AgeAtt <- createAgeAttribute(Op = "gte", Value = 18L)
  DateAtt <- createOccurrenceStartDateAttribute(Op = "bt", Value = "2016-01-01",
                                                Extent = "2019-12-31")

  # check values in AgeAtt are set
  expect_s4_class(AgeAtt, "Component") #check its component class
  expect_s4_class(AgeAtt@CriteriaExpression[[1]], AgeAtt@MetaData@Name)
  expect_equal(componentType(AgeAtt), "Attribute")
  expect_equal(AgeAtt@CriteriaExpression[[1]]@Op, "gte")
  expect_equal(AgeAtt@CriteriaExpression[[1]]@Contents$Value, 18L)
  expect_equal(AgeAtt@CriteriaExpression[[1]]@Contents$Extent, NA_integer_)

  # check values in DateAtt are set
  expect_s4_class(DateAtt, "Component") #check its component class
  expect_s4_class(DateAtt@CriteriaExpression[[1]], DateAtt@MetaData@Name)
  expect_equal(componentType(DateAtt), "Attribute")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Op, "bt")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Value, "2016-01-01")
  expect_equal(DateAtt@CriteriaExpression[[1]]@Contents$Extent, "2019-12-31")
})

test_that("Concept Attribute works properly",{

  #set up connection details to test db
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    port = 5432
  )
  #connect to test db
  connection <- DatabaseConnector::connect(connectionDetails)
  #set schema for vocabulary
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")

  #create concept attribute for female gender
  genderAtt <- createGenderAttribute(conceptIds = 8532,
                                     connection = connection,
                                     vocabularyDatabaseSchema = vocabularyDatabaseSchema)

  # check values in genderAtt are set
  expect_s4_class(genderAtt, "Component") #check its component class
  expect_s4_class(genderAtt@CriteriaExpression[[1]], genderAtt@MetaData@Name)
  expect_equal(componentType(genderAtt), "Attribute")
  expect_equal(genderAtt@CriteriaExpression[[1]]@Name, "Gender")
  expect_equal(genderAtt@CriteriaExpression[[1]]@Concepts[[1]]$CONCEPT_ID, 8532L)
  expect_equal(genderAtt@CriteriaExpression[[1]]@Concepts[[1]]$CONCEPT_NAME, "FEMALE")
})


test_that("Source Concept Attribute works properly", {

  #call source concept set

  SourceT2DM <- readRDS("resources/conceptSetsList.rds")$SourceT2DM

  #concept set expression with only source concept from icd10
  SourceT2DMCSE <- SourceT2DM %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Source ICD10",
                               includeDescendants = FALSE)

  SourceT2DMAttribute <- createConditionSourceConceptAttribute(
    ConceptSetExpression = SourceT2DMCSE
    )

  # check values in SourceT2DMAttribute are set
  expect_s4_class(SourceT2DMAttribute, "Component") #check its component class
  expect_s4_class(SourceT2DMAttribute@CriteriaExpression[[1]], SourceT2DMAttribute@MetaData@Name)
  expect_equal(componentType(SourceT2DMAttribute), "Attribute")
  expect_equal(SourceT2DMAttribute@CriteriaExpression[[1]]@Name, "ConditionSourceConcept")
  expect_equal(checkConceptIds(SourceT2DMAttribute)[[1]], SourceT2DM$conceptId)
  expect_equal(checkConceptField(SourceT2DMAttribute, field = "CONCEPT_CODE")[[1]],
               SourceT2DM$conceptCode)
  expect_equal(checkConceptField(SourceT2DMAttribute, field = "VOCABULARY_ID")[[1]],
               SourceT2DM$vocabularyId)
})

test_that("Logical Attributes work properly",{

  #create attribute indicating a first occurrence
  firstOcc <- createFirstAttribute()

  #check logic attribute
  expect_s4_class(firstOcc, "Component") #check its component class
  expect_s4_class(firstOcc@CriteriaExpression[[1]], firstOcc@MetaData@Name)
  expect_equal(componentType(firstOcc), "Attribute")
  expect_equal(firstOcc@CriteriaExpression[[1]]@Name, "First")

})


test_that("Correlated Criteria Attribute works properly", {


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

  ## Create Correlated Criteria
  corrAtt <- createCorrelatedCriteriaAttribute(Group = T2DMGroup)

  #check correlated criteria attribute
  expect_s4_class(corrAtt, "Component") #check its component class
  expect_s4_class(corrAtt@CriteriaExpression[[1]], corrAtt@MetaData@Name)
  expect_equal(componentType(corrAtt), "Attribute")
  expect_equal(corrAtt@CriteriaExpression[[1]]@Name, "CorrelatedCriteria")
  #check group
  expect_s4_class(corrAtt@CriteriaExpression[[1]]@Group, "Group")
  expect_equal(corrAtt@CriteriaExpression[[1]]@Group, T2DMGroup@CriteriaExpression[[1]])

})

test_that("Attribute is added to a Query", {

  #create some attributes
  firstOcc <- createFirstAttribute()
  AgeAtt <- createAgeAttribute(Op = "gte", Value = 18L)
  DateAtt <- createOccurrenceStartDateAttribute(Op = "bt", Value = "2016-01-01",
                                                Extent = "2019-12-31")

  ## Create second Count at least 1 t2dm rx occurrence ---------
  T2DMMeds <- readRDS("resources/conceptSetsList.rds")$T2DMMeds

  #concept set expression with all concepts include decesendants
  T2DMMedsQuery <- T2DMMeds %>%
    createConceptSetExpression(Name = "Type 2 Diabetes Meds",
                               includeDescendants = TRUE) %>%
    createDrugExposure(conceptSetExpression = .,
                       attributeList = list(AgeAtt,
                                            DateAtt))

  T2DMMedsQueryUpdt <- addAttributeToQuery(query = T2DMMedsQuery,
                                           attribute = firstOcc)

  #check query for changes
  expect_s4_class(T2DMMedsQuery, "Component") #check its component class
  expect_equal(componentType(T2DMMedsQuery), "Query")
  expect_equal(length(T2DMMedsQuery@CriteriaExpression[[1]]@Attributes), 2)
  #check that attribute 1 in the query is the same as the contents of the age att
  expect_equal(T2DMMedsQuery@CriteriaExpression[[1]]@Attributes[[1]],
               AgeAtt@CriteriaExpression[[1]])


  #check query for changes after addition of attribute
  expect_s4_class(T2DMMedsQueryUpdt, "Component") #check its component class
  expect_equal(componentType(T2DMMedsQueryUpdt), "Query")
  expect_equal(length(T2DMMedsQueryUpdt@CriteriaExpression[[1]]@Attributes), 3)
  #check that attribute 1 in the query is the same as the contents of the age att
  expect_equal(T2DMMedsQueryUpdt@CriteriaExpression[[1]]@Attributes[[3]],
               firstOcc@CriteriaExpression[[1]])


})
