## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  eval = FALSE
)

## ----T2d Pathways Image, eval=TRUE, echo=FALSE--------------------------------
knitr::include_graphics(
  system.file("images", "t2d_001.png" , package = "Capr")
  )

## ----setup--------------------------------------------------------------------
#  library(Capr)
#  library(DatabaseConnector)
#  
#  #set database connection details
#  connectionDetails <- createConnectionDetails(
#    dbms = "postgresql",
#    server = "example.com/datasource",
#    user = "me",
#    password = "secret",
#    port = "5432")
#  
#  #set connection
#  connection <- connect(connectionDetails)
#  #identify vocabulary schema
#  vocabularyDatabaseSchema <- "vocab"

## ----lookup Concepts by Id----------------------------------------------------
#  #Type 2 Diabetes Diagnosis
#  T2Dx <- getConceptIdDetails(
#    conceptIds = 201826,
#    connection = connection,
#    vocabularyDatabaseSchema = vocabularyDatabaseSchema) %>%
#    createConceptSetExpression(
#      Name = "Type 2 Diabetes Diagnosis",
#      includeDescendants = TRUE)
#  
#  #Type 2 Diabetes Medications concept IDS
#  T2RxIds <- c(1502809L, 1502826L, 1503297L, 1510202L,
#               1515249L, 1516766L, 1525215L, 1529331L,
#               1530014L, 1547504L, 1559684L, 1560171L,
#               1580747L, 1583722L, 1594973L, 1597756L)
#  
#  #create concept set expression
#  T2Rx <- getConceptIdDetails(
#    conceptIds = T2RxIds,
#    connection = connection,
#     vocabularyDatabaseSchema = vocabularyDatabaseSchema) %>%
#    createConceptSetExpression(
#      Name = "Type 2 Diabetes Medications",
#      includeDescendants = TRUE)
#  
#  #Type 1 Diabetes Diagnosis
#  T1Dx <- getConceptIdDetails(
#    conceptIds = 201254,
#    connection = connection,
#    vocabularyDatabaseSchema = vocabularyDatabaseSchema) %>%
#    createConceptSetExpression(
#      Name = "Type 1 Diabetes Diagnosis",
#      includeDescendants = TRUE)
#  

## ----lookup concepts by codes-------------------------------------------------
#  #Type 1 Diabetes Medications
#  T1DRxNormCodes <- paste(c(139825,274783,314684,
#                            352385,400008,51428,
#                            5856,86009,139953))
#  T1Rx <- getConceptCodeDetails(
#    conceptCode = T1DRxNormCodes,
#    vocabulary = "RxNorm",
#    connection = connection,
#    vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#    mapToStandard = TRUE) %>%
#    createConceptSetExpression(
#      Name = "Type 1 Diabetes Medications",
#      includeDescendants = TRUE)
#  
#  #Abnormal Lab
#  AbLabHbA1c <- c("4548-4", "17856-6", "4549-2", "17855-8") %>%
#    getConceptCodeDetails(
#      conceptCode = ., #places the lhs vector to the rhs of the pipe
#      vocabulary = "LOINC",
#      connection = connection,
#      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#      mapToStandard = TRUE) %>%
#    createConceptSetExpression(
#      Name = "Abnormal Lab HbA1c",
#      includeDescendants = TRUE)
#  
#  #Ab Lab for Random Glucose (>= 200 mg/dl)
#  AbLabRandomGluc <- c("2339-0", "2345-7") %>%
#    getConceptCodeDetails(
#      conceptCode = .,
#      vocabulary = "LOINC",
#      connection = connection,
#      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#      mapToStandard = TRUE) %>%
#    createConceptSetExpression(
#      Name = "Abnormal Lab Random Glucose",
#      includeDescendants = TRUE)
#  
#  #Ab Lab for Fasting Glucose (>= 125 mg/dl)
#  
#  AbLabFastingGluc <- c("1558-6") %>%
#    getConceptCodeDetails(
#      conceptCode = .,
#      vocabulary = "LOINC",
#      connection = connection,
#      vocabularyDatabaseSchema = vocabularyDatabaseSchema,
#      mapToStandard = TRUE) %>%
#    createConceptSetExpression(
#      Name = "Abnormal Lab Fasting Glucose",
#      includeDescendants = TRUE)

## ----create queries-----------------------------------------------------------
#  
#  #T2Rx Drug Exposure Query
#  T2RxQuery <- createDrugExposure(conceptSetExpression = T2Rx)
#  
#  
#  #T1Rx Drug Exposure Query
#  T1RxQuery <- createDrugExposure(conceptSetExpression = T1Rx)
#  
#  
#  #T2Dx Condition Occurrence Query
#  T2DxQuery <- createConditionOccurrence(conceptSetExpression = T2Dx)
#  
#  
#  #T1Dx Condition Occurrence Query
#  T1DxQuery <- createConditionOccurrence(conceptSetExpression = T1Dx)
#  

## ----queries with attributes--------------------------------------------------
#  #HbA1c Query with value attribute
#  AbLabHbA1cQuery <- createMeasurement(
#    conceptSetExpression = AbLabHbA1c,
#    attributeList = list(
#    #add attribute of >= 6% HbA1c
#    createValueAsNumberAttribute(Op = "gte", Value = 6)
#    ))
#  
#  #RandomGluc Query with value attribute
#  AbLabRandomGlucQuery <- createMeasurement(
#    conceptSetExpression = AbLabRandomGluc,
#    attributeList = list(
#    #add attribute of >= 200 mg/dl
#    createValueAsNumberAttribute(Op = "gte", Value = 200L)
#  ))
#  
#  #FastingGluc Query with value attribute
#  AbLabFastingGlucQuery <- createMeasurement(
#    conceptSetExpression = AbLabFastingGluc,
#    attributeList = list(
#    #add attribute of >= 125 mg/dl
#    createValueAsNumberAttribute(Op = "gte", Value = 125L)
#  ))

## ----create timeline----------------------------------------------------------
#  #create timeline
#  tl1 <- createTimeline(StartWindow = createWindow(
#    StartDays = "All", StartCoeff = "Before",
#    EndDays = 0L, EndCoeff = "After"))

## ----create counts------------------------------------------------------------
#  #no occurrence of T1 Diabetes
#  noT1DxCount <- createCount(Query = T1DxQuery,
#                             Logic = "exactly",
#                             Count = 0L,
#                             Timeline = tl1)
#  
#  #no occurrence of T2 Diabetes
#  noT2DxCount <- createCount(Query = T2DxQuery,
#                             Logic = "exactly",
#                             Count = 0L,
#                             Timeline = tl1)
#  
#  
#  #at least 1 occurrence of T2 Diabetes
#  atLeast1T2DxCount <- createCount(Query = T2DxQuery,
#                                   Logic = "at_least",
#                                   Count = 1L,
#                                   Timeline = tl1)
#  
#  #at least 2 occurrence of T2 Diabetes
#  atLeast2T2DxCount <- createCount(Query = T2DxQuery,
#                                   Logic = "at_least",
#                                   Count = 2L,
#                                   Timeline = tl1)
#  
#  ##################
#  #Medication Counts
#  ##################
#  
#  
#  #at least 1 T2DM medication
#  atLeast1T2RxCount <- createCount(Query = T2RxQuery,
#                                   Logic = "at_least",
#                                   Count = 1L,
#                                   Timeline = tl1)
#  
#  #no exposure to T2DM medication
#  noT2RxCount <- createCount(Query = T2RxQuery,
#                             Logic = "exactly",
#                             Count = 0L,
#                             Timeline = tl1)
#  
#  #at least 1 T1DM medication
#  atLeast1T1RxCount <- createCount(Query = T1RxQuery,
#                                   Logic = "at_least",
#                                   Count = 1L,
#                                   Timeline = tl1)
#  
#  #no exposure to T1DM medication
#  noT1RxCount <- createCount(Query = T1RxQuery,
#                             Logic = "exactly",
#                             Count = 0L,
#                             Timeline = tl1)
#  
#  #################
#  #AbLab Counts
#  #################
#  
#  #at least 1 abnormal HbA1c Lab
#  atLeast1AbLabHbA1cCount <- createCount(Query = AbLabHbA1cQuery,
#                                         Logic = "at_least",
#                                         Count = 1L,
#                                         Timeline = tl1)
#  
#  #at least 1 abnormal Fasting Glucose Lab
#  atLeast1AbLabFastingGlucCount <- createCount(Query = AbLabFastingGlucQuery,
#                                               Logic = "at_least",
#                                               Count = 1L,
#                                               Timeline = tl1)
#  
#  #at least 1 abnormal Random Glucose Lab
#  atLeast1AbLabRandomGlucCount <- createCount(Query = AbLabRandomGlucQuery,
#                                              Logic = "at_least",
#                                              Count = 1L,
#                                              Timeline = tl1)

## ----create Primary Criteria--------------------------------------------------
#  PrimaryCriteria <- createPrimaryCriteria(
#    Name = "PC for T2DM Case Phenotype",
#    ComponentList = list(T2DxQuery,T2RxQuery,AbLabHbA1cQuery,
#                         AbLabFastingGlucQuery,AbLabRandomGlucQuery),
#    ObservationWindow = createObservationWindow(
#      PriorDays = 0L,
#      PostDays = 0L
#    ),
#    Limit = "All")

## ----create Additional Criteria-----------------------------------------------
#  #No T1Dx at any point in patient history
#  NoT1DxGroup <- createGroup(Name = "No Diagnosis of Type 1 Diabetes",
#                             type = "ALL",
#                             criteriaList = list(noT1DxCount))
#  
#  #create additional Criteria
#  #further restrict the initial capture to people with no T1Dx
#  AdditionalCriteria <- createAdditionalCriteria(
#    Name = "AC for T2DM Case Phenotype",
#    Contents = NoT1DxGroup,
#    Limit = "First"
#  )

## ----AbLab Group--------------------------------------------------------------
#  atLeast1AbLabGroup <- createGroup(
#    Name = "Abnormal labs for HbA1c, Fasting+Random Glucose",
#    type = "ANY",
#    criteriaList = list(
#        atLeast1AbLabHbA1cCount,
#        atLeast1AbLabFastingGlucCount,
#        atLeast1AbLabRandomGlucCount)
#    )

## ----T2DM Pathways------------------------------------------------------------
#  
#  #Path 1: 0 T2Dx, 1+ T2Rx and 1+ AbLab
#  Pathway1T2DMGroup <- createGroup(
#    Name = "Pathway1",
#    Description = "0 T2Dx, 1+ T2Rx and 1+ AbLab",
#    type = "ALL",
#    criteriaList = list(noT2DxCount, atLeast1T2RxCount),
#    Groups = list(atLeast1AbLabGroup))
#  
#  #Path 2: 1+ T2Dx, 0 T1Rx, 0 T2Rx, and 1+ AbLab
#  Pathway2T2DMGroup <- createGroup(
#    Name = "Pathway2",
#    Description = "1+ T2Dx, 0 T1Rx, 0 T2Rx, and 1+ AbLab",
#    type = "ALL",
#    criteriaList = list(atLeast1T2DxCount, noT1RxCount, noT2RxCount),
#    Groups = list(atLeast1AbLabGroup))
#  
#  #Path 3: 1+ T2Dx, 0 T1Rx, and 1+ T2Rx
#  Pathway3T2DMGroup <- createGroup(
#    Name = "Pathway3",
#    Description = "1+ T2Dx, 0 T1Rx, and 1+ T2Rx",
#    type = "ALL",
#    criteriaList = list(atLeast1T2DxCount, noT1RxCount, atLeast1T2RxCount)
#  )
#  
#  #Path 5: 1+ T2Dx, 1+ T1Rx, 0 T2Rx and 2+ T2Dx
#  Pathway5T2DMGroup <- createGroup(
#    Name = "Pathway5",
#    Description = "1+ T2Dx, 1+ T1Rx, 0 T2Rx and 2+ T2Dx",
#    type = "ALL",
#    criteriaList = list(atLeast1T2DxCount, atLeast1T1RxCount,
#                        noT2RxCount, atLeast2T2DxCount)
#  )
#  

## ----nested criteria----------------------------------------------------------
#  tl2 <- createTimeline(StartWindow = createWindow(
#    StartDays = "All", StartCoeff = "Before",
#    EndDays = 1L, EndCoeff = "Before"))
#  
#  PriorT2RxCount <- createCount(
#    Query = T2RxQuery,
#    Logic = "at_least",
#    Count = 1L,
#    Timeline = tl2
#  )
#  
#  PriorT2RxNestedGroup <- createCorrelatedCriteriaAttribute(
#    createGroup(
#      Name = "Nested Group T2Rx before T1Rx",
#      type = "ALL",
#      criteriaList = list(PriorT2RxCount)
#    )
#  )
#  
#  T2RxBeforeT1RxCount <- createDrugExposure(
#    conceptSetExpression = T1Rx,
#    attributeList = list(PriorT2RxNestedGroup)) %>%
#    createCount(Logic = "at_least", Count = 1L,
#                Timeline = tl1)
#  

## ----Pathway 4 nested---------------------------------------------------------
#  #Path 4: 1+ T2Dx, 1+ T1Rx, 1+T2Rx, and 1+ T2Rx < T1Rx
#  Pathway4T2DMGroup <- createGroup(
#    Name = "Pathway4",
#    Description = "1+ T2Dx, 1+ T1Rx, 1+T2Rx, and 1+ T2Rx < T1Rx",
#    type = "ALL",
#    criteriaList = list(atLeast1T2DxCount, atLeast1T1RxCount,
#                        T2RxBeforeT1RxCount)
#  )

## ----create Inclusion Rules---------------------------------------------------
#  #T2DM Case Group
#  T2DMCase <- createGroup(
#    Name = "Case for T2DM using algorithm",
#    type = "ANY",
#    Groups = list(Pathway1T2DMGroup, Pathway2T2DMGroup, Pathway3T2DMGroup,
#                  Pathway4T2DMGroup, Pathway5T2DMGroup)
#  )
#  
#  #keep T2DM cases if they meet 1 of the 5 pathways
#  InclusionRules <- createInclusionRules(
#    Name = "IRs for T2DM Case Phenotype",
#    Contents = list(T2DMCase),
#    Limit = "First"
#  )
#  

## ----create Censoring Criteria------------------------------------------------
#  #person exits cohort if there is a diagnosis of T1DM
#  CensoringCriteria <- createCensoringCriteria(
#    Name = "Censor of T1DM cases",
#    ComponentList = list(T1DxQuery)
#  )

## ----create Cohort Definition-------------------------------------------------
#  T2DMPhenotype <- createCohortDefinition(
#    Name = "PheKB T2DM Definition",
#    PrimaryCriteria = PrimaryCriteria,
#    AdditionalCriteria = AdditionalCriteria,
#    InclusionRules = InclusionRules,
#    CensoringCriteria = CensoringCriteria
#  )

## ----get JSON for cohort definition-------------------------------------------
#  T2DMPhenotypeJson <- compileCohortDefinition(T2DMPhenotype)

## ----capture json, echo=FALSE, eval=TRUE, comment=NA--------------------------
T2DMPhenotypeJson <- readr::read_file(
  system.file("extdata", "cohortT2DMTest.json", package = "Capr")
)
cat(T2DMPhenotypeJson)

