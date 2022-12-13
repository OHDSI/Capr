## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, eval=FALSE--------------------------------------------------------
#  library(Capr) #the CAPR package to build cohorts in R
#  library(DatabaseConnector) #to connect to our OMOP CDM
#  library(CirceR) #access the circe engine to generate ohdisql
#  library(magrittr) #leverage piping
#  
#  connectionDetails <- createConnectionDetails(dbms="postgresql",
#                                               server="example.com/datasource",
#                                               user="me",
#                                               password="secret",
#                                               schema="cdm",
#                                               port="5432")
#  conn <- connect(connectionDetails)

## ----invisible setup, echo=FALSE, eval=TRUE, message=FALSE--------------------
library(Capr) #the CAPR package to build cohorts in R
library(DatabaseConnector) #to connect to our OMOP CDM
library(CirceR) #access the circe engine to generate ohdisql
library(magrittr) #leverage piping

PcCovidDiag <- loadComponent(system.file("extdata","PcCovidDiag.json", package = "Capr"))
AcCovidDiag <- loadComponent(system.file("extdata","AcCovidDiag.json", package = "Capr"))
IrsCovidDiag <- loadComponent(system.file("extdata","IrsCovidDiag.json", package = "Capr"))
EsCovidDiag <- loadComponent(system.file("extdata","EsCovidDiag.json", package = "Capr"))
cen <- loadComponent(system.file("extdata","exampleCen.json", package = "Capr"))
CovidCountCriteria1 <- loadComponent(system.file("extdata","CovidCountCriteria1.json", package = "Capr"))
CovidDiagGroup <- loadComponent(system.file("extdata","CovidDiagGroup.json", package = "Capr"))
IpVisitQuery <- loadComponent(system.file("extdata","IpVisitQuery.json", package = "Capr"))
IpVisitCSE <- loadComponent(system.file("extdata","IpVisitCSE.json", package = "Capr"))
exampleConceptLookup <- readRDS(system.file("extdata","exampleConceptLookup.rds", package = "Capr"))

## ----lookup ids, eval=FALSE---------------------------------------------------
#  getConceptIdDetails(conceptIds = c(262, #ERand IP Visit
#                                   9201), #IP Visit
#                                   connectionDetails = NULL,
#                                   connection = connection, #use connection since it is already open
#                                   vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                   oracleTempSchema = NULL,
#                                   mapToStandard = TRUE)

## ----echo = FALSE, eval=TRUE--------------------------------------------------
exampleConceptLookup[[1]]

## ----lookup codes1, eval=FALSE------------------------------------------------
#  getConceptCodeDetails(conceptCode = "E11",
#                               vocabulary = "ICD10CM",
#                               connectionDetails = NULL,
#                               connection = connection, #use connection since it is already open
#                               vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                               oracleTempSchema = NULL,
#                               mapToStandard = FALSE)
#  

## ----echo = FALSE, eval=TRUE--------------------------------------------------
exampleConceptLookup[[2]]

## ----lookup codes2------------------------------------------------------------
#  getConceptCodeDetails(conceptCode = "E11",
#                               vocabulary = "ICD10CM",
#                               connectionDetails = NULL,
#                               connection = connection, #use connection since it is already open
#                               vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                               oracleTempSchema = NULL,
#                               mapToStandard = TRUE)

## ----echo = FALSE, eval=TRUE--------------------------------------------------
exampleConceptLookup[[3]]

## ----lookup keywords, warning=FALSE-------------------------------------------
#  Diabetes <- lookupKeyword(keyword = "Diabetes",
#                            searchType = "any",
#                            connectionDetails = NULL,
#                            connection = connection, #use connection since it is already open
#                            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                            oracleTempSchema = NULL)
#  Diabetes$.[1:5,] #need dot to call data frame from object

## ----echo = FALSE, eval=TRUE--------------------------------------------------
diabetes <- exampleConceptLookup[[4]]
diabetes$CONCEPT_NAME <- substr(diabetes$CONCEPT_NAME, 1, 35)
diabetes

## ----cse----------------------------------------------------------------------
#  IpVisitCSE <- getConceptIdDetails(conceptIds = c(262, #ERand IP Visit
#                                                   9201), #IP Visit
#                                    connectionDetails = NULL,
#                                    connection = connection, #use connection since it is already open
#                                    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                    oracleTempSchema = NULL,
#                                    mapToStandard = TRUE) %>%
#    createConceptSetExpression(Name = "InpatientVisit", includeDescendants = TRUE)
#  

## ----echo=FALSE, eval=TRUE----------------------------------------------------
str(IpVisitCSE)

## ----custom concept set items-------------------------------------------------
#  cid <- c(37310282L, 37310281L, 756055L)
#  nm <- "COVID-19 specific testing (pre-coordinated Measurements excluded)"
#  n <- length(cid)
#  #lookup up covid19 meausres
#  MeasureOfCovid19 <- getConceptIdDetails(conceptIds = cid,
#                            connectionDetails = NULL,
#                            connection = connection, #use connection since it is already open
#                            vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                            oracleTempSchema = NULL,
#                            mapToStandard = FALSE)
#  #create a custom mapping list
#  conceptMapping <- createConceptMapping(n = n,
#                                          includeDescendants = rep(TRUE,n),
#                                          isExcluded = c(TRUE,TRUE,FALSE))
#  
#  MeasureOfCovid19CSE <- MeasureOfCovid19 %>%
#    createConceptSetExpressionCustom(Name = nm, conceptMapping = conceptMapping)

## ----create an attribute------------------------------------------------------
#  DateAtt <- createOccurrenceStartDateAttribute(Op = "gt", Value = "2019-12-01")

## ----create a query-----------------------------------------------------------
#  IpVisitQuery <- createVisitOccurrence(conceptSetExpression = IpVisitCSE,
#                                        attributeList = list(DateAtt))

## ----echo=FALSE, eval=TRUE----------------------------------------------------
str(IpVisitQuery)

## ----create a primary criteria------------------------------------------------
#  PcCovidDiag <- createPrimaryCriteria(Name = "Inpatient Visit Primary Criteria",
#                              ComponentList = list(IpVisitQuery),
#                              ObservationWindow = createObservationWindow(0L,0L),
#                              Limit = "All")

## ----echo=FALSE, eval=TRUE----------------------------------------------------
str(PcCovidDiag)

## ----create a count-----------------------------------------------------------
#  #lookup covid diagnosis in vocabulary
#  CovidDiag <- getConceptIdDetails(conceptIds = 37311061,
#                                   connectionDetails = NULL,
#                                   connection = connection, #use connection since it is already open
#                                   vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                   oracleTempSchema = NULL,
#                                   mapToStandard = TRUE)
#  #create concept set expression including descendant concepts
#  CovidDiagCSE <- CovidDiag %>%
#    createConceptSetExpression(Name="COVID-19 (including asymptomatic)",
#                               includeDescendants = TRUE)
#  #create a conditionOccurrence query
#  CovidDiagQuery <- createConditionOccurrence(conceptSetExpression = CovidDiagCSE)
#  
#  #create start window for timeline
#  #start window to count occurrences from inpatient event (cohort entry)
#  StartWindow1 <- createWindow(StartDays = 21, StartCoeff = "Before", # 21 days before IpVisit index start
#                               EndDays = "All",EndCoeff = "After") #to all days after IpVisit index start
#  #create end window for timeline
#  #end window to count occurrences from inpatient event (cohort entry)
#  EndWindow1 <- createWindow(StartDays = "All", StartCoeff = "Before", #end all days before IpVisit index start
#                             EndDays = 0, EndCoeff = "After", #0 days after IpVisit index start
#                             IndexStart = FALSE) #toggle index end
#  #create timeline
#  Timeline1 <- createTimeline(StartWindow = StartWindow1,
#                       EndWindow = EndWindow1)
#  #create a count criteria
#  #at least 1 occurrence of a covid diagnosis occurring
#  #between a) 21 days before and all days after initial inpatient visit index start date
#  #and b) all days before and 0 days after initial inpatient visit index end date
#  CovidCountCriteria1 <- createCount(Query = CovidDiagQuery,
#                            Logic = "at_least",
#                            Count = 1,
#                            Timeline = Timeline1)
#  

## ----echo=FALSE, eval=TRUE----------------------------------------------------
str(CovidCountCriteria1)

## ----create source concept criteria-------------------------------------------
#  CovidSourceConcepts <- getConceptIdDetails(conceptIds = c(710158L, 710155L, 710156L,
#                                                            710159L, 45542411L, 710160L,
#                                                            45756093L, 42501115L, 586414L,
#                                                            45600471L, 586415L, 710157L),
#                                             connectionDetails = NULL,
#                                             connection = connection, #use connection since it is already open
#                                             vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                             oracleTempSchema = NULL,
#                                             mapToStandard = FALSE)
#  #create a default mapping list includeDescendants, isExcluded, includeMapped all FALSE
#  ConceptMappingSourceConcepts <- createConceptMapping(n = nrow(CovidSourceConcepts))
#  #toggle TRUE the positions needed to alter
#  ConceptMappingSourceConcepts <- toggleConceptMapping(conceptMapping = ConceptMappingSourceConcepts,
#                                                       pos = c(5,9:12), mapping = "isExcluded")
#  
#  #Create Concept Set Expression with custom mapping
#  CovidSourceConceptCSE <- CovidSourceConcepts %>%
#    createConceptSetExpressionCustom(Name = "COVID-19 source codes",
#                                     conceptMapping = ConceptMappingSourceConcepts)
#  
#  #Create a Source Concept Attribute for query
#  SourceConceptAttribute <- createConditionSourceConceptAttribute(ConceptSetExpression = CovidSourceConceptCSE)
#  
#  #Create Query
#  CovidSourceConceptQuery <- createConditionOccurrence(attributeList = list(SourceConceptAttribute))
#  
#  #create Count for covid source concepts
#  CovidCountCriteria2 <- createCount(Query = CovidSourceConceptQuery,
#                                     Logic = "at_least",
#                                     Count = 1,
#                                     Timeline = Timeline1)

## ----using piping-------------------------------------------------------------
#  CovidCountCriteria3 <- getConceptIdDetails(conceptIds = 37310282,
#                                   connectionDetails = NULL,
#                                   connection = connection, #use connection since it is already open
#                                   vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                   oracleTempSchema = NULL,
#                                   mapToStandard = TRUE) %>%
#    createConceptSetExpression(Name = "COVID-19 specific test") %>%
#    createMeasurement(conceptSetExpression = .) %>%
#    createCount(Logic = "at_least",
#                Count = 1,
#                Timeline = Timeline1)
#  componentType(CovidCountCriteria3)

## -----------------------------------------------------------------------------
#  getConceptIdDetails(conceptIds = c(4126681L,45877985L, 9191L, 4181412L, 45879438L, 45884084L),
#                                   connectionDetails = NULL,
#                                   connection = connection, #use connection since it is already open
#                                   vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                   oracleTempSchema = NULL,
#                                   mapToStandard = TRUE)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
exampleConceptLookup[[5]]

## ----create a concept attribute-----------------------------------------------
#  ValueAsConceptAtt <- createValueAsConceptAttribute(conceptIds = c(4126681L,45877985L, 9191L,
#                                                                    4181412L, 45879438L, 45884084L),
#                                                     connectionDetails = NULL,
#                                                     connection = connection, #use connection since it is already open
#                                                     vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                                     oracleTempSchema = NULL,
#                                                     mapToStandard = TRUE)
#  
#  Covid19MeasuresQuery <- createMeasurement(conceptSetExpression = MeasureOfCovid19CSE,
#                                             attributeList = list(ValueAsConceptAtt))
#  CovidCountCriteria4 <- createCount(Query = Covid19MeasuresQuery,
#                                     Logic = "at_least",
#                                     Count=1, Timeline = Timeline1)

## ----reusing a concept set for different domain-------------------------------
#  Covid19ObservationMeasureQuery <- createObservation(conceptSetExpression = MeasureOfCovid19CSE,
#                                             attributeList = list(ValueAsConceptAtt))
#  
#  CovidCountCriteria5A <- createCount(Query = Covid19ObservationMeasureQuery,
#                                     Logic = "at_least",
#                                     Count = 1,
#                                     Timeline = Timeline1)
#  

## -----------------------------------------------------------------------------
#  CovidCountCriteria5 <- CovidCountCriteria4
#  CovidCountCriteria5@CriteriaExpression[[1]]@Criteria@Domain <- "Observation"

## ----creating the group-------------------------------------------------------
#  CovidSourceConceptQueryObs <- createObservation(attributeList = list(
#    createObservationSourceConceptAttribute(CovidSourceConceptCSE)))
#  CovidCountCriteria6 <- createCount(Query = CovidSourceConceptQueryObs,
#                            Logic = "at_least",
#                            Count =1,
#                            Timeline = Timeline1)
#  
#  CovidDiagGroup <- createGroup(Name = "COVID-19 Diag",
#                            type = "ANY",
#                            criteriaList = list(CovidCountCriteria1, CovidCountCriteria2,
#                                                CovidCountCriteria3, CovidCountCriteria4,
#                                                CovidCountCriteria5, CovidCountCriteria6))
#  saveComponent(CovidDiagGroup,
#                saveName = "CovidDiagGroup",
#                savePath = "~/Documents")

## ----creating an additional criteria------------------------------------------
#  AcCovidDiag <- createAdditionalCriteria(Name = "Additional Crit for COVID cohort",
#                                 Contents = CovidDiagGroup,
#                                 Limit = "All")

## ----creating a correlated criteria-------------------------------------------
#  #load an existing component
#  CovidDiagGroupLoad <- loadComponent("~/Documents/CovidDiagGroup.json")
#  CorrelatedCriteriaAtt <- createCorrelatedCriteriaAttribute(CovidDiagGroupLoad)
#  InpatientVisitWCovidDiagQuery <- createVisitOccurrence(conceptSetExpression = IpVisitCSE,
#                                       attributeList = list(CorrelatedCriteriaAtt))
#  Timeline2 <- createTimeline(StartWindow = createWindow(StartDays = 180, StartCoeff = "Before",
#                                                   EndDays = 1, EndCoeff = "Before"))
#  
#  CovidCountInclusionRule3 <- createCount(Query = InpatientVisitWCovidDiagQuery,
#                              Logic = "exactly",
#                              Count = 0,
#                              Timeline = Timeline2)
#  CovidHospitalizationGroup <- createGroup(Name ="does not have hospitalization for COVID19 in the 6 months preceding admission",
#                             type = "ALL",
#                             criteriaList = list(CovidCountInclusionRule3),
#                             demographicCriteriaList = NULL,
#                             Groups = NULL)

## ----creating demographic criteria--------------------------------------------
#  AgeAtt <- createAgeAttribute(Op = "gte", Value = 18)
#  Age18AndOlderGroup <- createGroup(Name = ">=18 years old",
#                             type="ALL",
#                             criteriaList = NULL,
#                             demographicCriteriaList = list(AgeAtt),
#                             Groups = NULL)

## ----creating inclusion rules-------------------------------------------------
#  Timeline3 <- createTimeline(StartWindow = createWindow(StartDays = "All", StartCoeff = "Before",
#                                                         EndDays = 365, EndCoeff = "Before"),
#                        EndWindow = createWindow(StartDays = 0, StartCoeff = "Before",
#                                                 EndDays = "All", EndCoeff = "After",
#                                                 IndexStart = FALSE, EventStarts = FALSE))
#  
#  CovidCountInclusionRule2 <- createCount(Query = createObservationPeriod(),
#                              Logic = "at_least",
#                              Count =1,
#                              Timeline = Timeline3)
#  Has365DaysObsGroup <- createGroup(Name = "Has >= 365 of observation",
#                             type = "ALL",
#                             criteriaList = list(CovidCountInclusionRule2))
#  
#  IrsCovidDiag <- createInclusionRules(Name = "Inclusion Rules for covid Cohort",
#                              Contents = list(Age18AndOlderGroup,
#                                              Has365DaysObsGroup,
#                                              CovidHospitalizationGroup),
#                              Limit ="First")

## ----creating a Date Offset---------------------------------------------------
#  EsCovidDiag <- createDateOffsetEndStrategy(offset = 0, eventDateOffset = "EndDate")

## ----creating a custom era----------------------------------------------------
#  WarfarinCSE <- getConceptIdDetails(conceptIds = 1310149,
#                                    connectionDetails = NULL,
#                                    connection = connection, #use connection since it is already open
#                                    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                    oracleTempSchema = NULL,
#                                    mapToStandard = TRUE) %>%
#    createConceptSetExpression(Name = "Warfarin", includeDescendants = TRUE)
#  
#  EndStrategyWithDrug <- createCustomEraEndStrategy(WarfarinCSE,
#                                                    gapDays = 3,
#                                                    offset = 0)

## ----creating a censoring criteria--------------------------------------------
#  InfliximabCSE <- getConceptIdDetails(conceptIds = c(937368,937369),
#                                    connectionDetails = NULL,
#                                    connection = connection, #use connection since it is already open
#                                    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#                                    oracleTempSchema = NULL,
#                                    mapToStandard = TRUE) %>%
#    createConceptSetExpression(Name = "Infliximab", includeDescendants = TRUE)
#  infliximabQuery <-createDrugEra(conceptSetExpression = InfliximabCSE)
#  cen <-createCensoringCriteria(Name = "prev inflix", ComponentList =  list(infliximabQuery))

## ----creating a cohort Era----------------------------------------------------
#  cohortEra <- createCohortEra(LeftCensorDate = "2019-12-17")

## ----create the cohort definition---------------------------------------------
#  desc <- "This cohort counts the number of inpatient visits from patients with COVID-19, this is counted as a covid diagnosis or from a measured covid test"
#  cd <- createCohortDefinition(Name = "Inpatient COVID-19 Diag",
#                               Description = desc,
#                               PrimaryCriteria = PcCovidDiag,
#                               AdditionalCriteria = AcCovidDiag,
#                               InclusionRules = IrsCovidDiag,
#                               EndStrategy = EsCovidDiag)

## ----compile Cohort Definition------------------------------------------------
#  genOp <- CirceR::createGenerateOptions(cohortIdFieldName = "cohort_definition_id",
#                                 cohortId = 9999,
#                                 cdmSchema = connectionDetails$schema,
#                                 targetTable = "cohort",
#                                 resultSchema = "results",
#                                 vocabularySchema = connectionDetails$schema,
#                                 generateStats = F)
#  cohortInfo <- compileCohortDefinition(cd, genOp)

## ----eval=FALSE---------------------------------------------------------------
#  DatabaseConnector::executeSql(connection = conn, sql=cohortInfo$ohdiSQL)

## ----save, eval=FALSE---------------------------------------------------------
#  saveComponent(AcCovidDiag,
#                saveName = "covidAC",
#                savePath = "~/Documents/ComponentLibrary/CovidInpatient")

## ----load---------------------------------------------------------------------
#  acCovidIp <- loadComponent("~/Documents/ComponentLibrary/CovidInpatient/covidAC.json")
#  
#  
#  cd2 <- createCohortDefinition(Name = "DifferentcovidDiagCohort",
#                                PrimaryCriteria = pcCovidOw365,
#                                AdditionalCriteria = acCovidIp,
#                                InclusionRules = irs,
#                                EndStrategy = es)

## ----import json cohort-------------------------------------------------------
#  cd2 <- readInCirce("~/Documents/NetworkStudy/COVID/inpatientCovid.json")
#  pc2 <- loadComponent("~/Documents/ComponentLibrary/CovidInpatient/covidPC.json")
#  pc2@CriteriaExpression$ObservationWindow@PostDays <- 365L
#  cd2@PrimaryCriteria <- pc2
#  cohortInfo2 <- compileCohortDefinition(cd2, genOp)

## -----------------------------------------------------------------------------
#  writeCaprCall(jsonPath = "~/Documents/NetworkStudy/COVID/inpatientCovid.json",
#                txtPath = "~/Documents/NetworkStudy/COVID/inpatientCovid.txt")

