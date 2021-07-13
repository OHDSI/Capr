library(Capr)

test_that("Save and Load functions working properly", {

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

  ## Save Component --------------------
  #set up save space for capr


  saveComponent(T2DMGroup, saveName = "T2DMGroup",
                savePath = Sys.getenv("Capr_Save_Space"))

  #readLines("~/Documents/github/Capr/tests/testthat/resources/T2DMGroup.json")

  #read json file
  pathJson <- file.path(Sys.getenv("Capr_Save_Space"), "T2DMGroup.json")
  componentJsonCheck <- paste(readLines(pathJson), "\n")
  expect_true(all(nchar(componentJsonCheck, keepNA = TRUE) > 0))

  #create s3 lists of json
  componentListBaseline <- jsonlite::read_json("resources/T2DMGroup.json")
  componentListCheck <- jsonlite::read_json(
    file.path(Sys.getenv("Capr_Save_Space"), "T2DMGroup.json")
    )
  # check save matches baseline. note concept ids regenerate per create

  #component type same
  expect_equal(componentListCheck$MetaData$ComponentType,
               componentListBaseline$MetaData$ComponentType)

  #names the same
  expect_equal(names(componentListCheck), names(componentListBaseline))

  #domain in first criteria is same
  expect_equal(componentListCheck$CriteriaExpression[[1]]$CriteriaList[[1]]$Criteria$Domain,
               componentListBaseline$CriteriaExpression[[1]]$CriteriaList[[1]]$Criteria$Domain)

  #concept id of second concept set expression in 5th concept is same
  expect_equal(componentListCheck$ConceptSetExpression[[2]]$Expression[[5]]$Concept$CONCEPT_ID,
               componentListBaseline$ConceptSetExpression[[2]]$Expression[[5]]$Concept$CONCEPT_ID)

  # Load Saved Component
  loadCaprObj <- loadComponent(
    file.path(Sys.getenv("Capr_Save_Space"), "T2DMGroup.json")
  )

  #check load is equivalent
  expect_equal(loadCaprObj, T2DMGroup)

})


test_that("Read in Circe json creates Capr Cohort",{

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    port = 5432
  )
  #set schema for vocabulary
  vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")

  # path <- "~/Documents/github/Capr/tests/testthat/resources/cohortTest.json"
  # cd <- readInCirce(jsonPath = path,
  #                   connectionDetails = connectionDetails,
  #                   vocabularyDatabaseSchema = vocabularyDatabaseSchema)

  cd <- readInCirce(jsonPath = "resources/cohortTest.json",
                    connectionDetails = connectionDetails,
                    vocabularyDatabaseSchema = vocabularyDatabaseSchema)


  #import the test json
  kk <- jsonlite::read_json("resources/cohortTest.json")
  #extract concept sets
  cidKK <- sapply(lapply(kk$ConceptSets[[1]]$expression$items, getElement, "concept"),
                  getElement, "CONCEPT_ID")

  cdOut <- jsonlite::parse_json(compileCohortDefinition(cd))
  #extract the concept ids
  cidCdOut <- sapply(lapply(cdOut$ConceptSets[[1]]$expression$items, getElement, "concept"),
                     getElement, "CONCEPT_ID")

  #check cohort class object
  expect_s4_class(cd, "CohortDefinition")
  #all the concept ids are the same
  expect_true(all(cidCdOut, cidKK))
  expect_equal(cdOut$PrimaryCriteria, kk$PrimaryCriteria)

})


test_that("Write Capr call works properly", {

  writeCaprCall(jsonPath = "resources/cohortTest.json",
                txtPath = file.path(Sys.getenv("Capr_Save_Space"), "cohortTest.txt"))

  pathTxt <- file.path(Sys.getenv("Capr_Save_Space"), "cohortTest.txt")
  writeTxtCheck <- paste(readLines(pathTxt), "\n")
  expect_true(all(nchar(writeTxtCheck, keepNA = TRUE) > 0))
})
