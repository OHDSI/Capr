## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE---------------------------------------------------------------
AttributeOptions <- list('Op' =c("Age", "OccurrenceStartDate", "OccurrenceEnd", "AgeAtEnd",
                               "AgeAtStart", "PeriodLength", "ValueAsNumber", "RangeLow",
                               "RangeHigh", "RangeLowRatio", "RangeHighRatio",
                               "EraStartDate", "EraEndDate", "OccurrenceCount",
                               "EraLength", "Refills", "Quantity", "DaysSupply",
                               "EffectiveDrugDose", "VisitLength"),
                       'Concept' = c("ConditionType", "Gender", "VisitType",
                                     "DrugType", "RouteConcepts", "DoseUnit",
                                     "ProviderSepcialty", "PlaceOfService",
                                     "ProcedureType", "Modifier", "ObservationType",
                                     "ValueAsConcept", "Qualifier", "Unit",
                                     "MeasurementType", "Operator", "DeathType",
                                     "DeviceType"),
                       'Logical' =c("First", "DrugTypeExclude", "ConditionTypeExclude",
                                    "VisitTypeExclude", "ProcedureTypeExclude",
                                    "ObservationTypeExclude", "MeasurementTypeExclude",
                                    "Abnormal", "DeathTypeExclude", "DeviceTypeExclude"),
                       'SourceConcept' = c("VisitSourceConcept","DrugSourceConcept",
                                           "ConditionSourceConcept", "ProcedureSourceConcept",
                                           "ObservationSourceConcept", "MeasurementSourceConcept",
                                           "DeathSourceConcept", "DeviceSourceConcept"),
                       'TextFilter' =c("ValueAsString", "StopReason", "UniqueDeviceId"))

## ----setup--------------------------------------------------------------------
library(Capr)
AttributeOptions

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(data.frame(symb = c("<", "<=", ">", ">=", "==", "--", "!-"),
                     text = c("less than", "less than or equal to", "greater than",
                              "greater than or equal to", "equal to", "between", "not between"),
                     short = c("lt", "lte", "gt", "gte", "eq", "bt", "!bt"),
                     idx = 1:7,
                     stringsAsFactors = FALSE))

## -----------------------------------------------------------------------------
opExample <- createOpAttribute(Name = "RangeHigh", Op = ">=", Value = 5L)
str(opExample)

## -----------------------------------------------------------------------------
opExample2 <- createOpAttribute(Name = "OccurrenceStartDate", Op = "greater than", Value = "2018-12-31")
str(opExample2)

## ----eval=FALSE---------------------------------------------------------------
#  createConceptAttribute(conceptIds = 8507, name = "Gender")

