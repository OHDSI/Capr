# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of Capr
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
##UI Wrappers Attributes

#' List Attribute options
#' @param  domain the attribute options within the domain, default is NULL then all options printed
#' @return A dataframe with the list of options for attributes we can use specified per domain.
#' @export
listAttributeOptions <- function(domain=NULL){
  attOps <- list('ConditionOccurrence' = c("First", "OccurrenceStartDate", "OccurrenceEndDate", "ConditionType",
                                         "ConditionTypeExclude", "StopReason", "ConditionSourceConcept", "Age",
                                         "Gender", "ProviderSpecialty", "VisitType"),
                 'ConditionEra' = c("First", "EraStartDate", "EraEndDate", "OccurrenceCount", "EraLength",
                                  "AgeAtStart","AgeAtEnd", "Gender"),
                 'Death' = c("First", "OccurrenceStartDate", "DeathType",
                           "DeathTypeExclude","DeathSourceConcept", "Age","Gender"),
                 'DeviceExposure' = c("First", "OccurrenceStartDate", "OccurrenceEndDate", "DeviceType",
                                    "DeviceTypeExclude", "UniqueDeviceId", "DeviceSourceConcept", "Age",
                                    "Gender", "ProviderSpecialty", "VisitType","Quantity"),
                 'DoseEra' = c("First", "EraStartDate", "EraEndDate", "Unit","DoseValue", "EraLength",
                            "AgeAtStart","AgeAtEnd", "Gender"),
                 'DrugEra'  = c("First", "EraStartDate", "EraEndDate", "OccurrenceCount","GapDays", "EraLength",
                              "AgeAtStart","AgeAtEnd", "Gender"),
                 'DrugExposure' = c("First", "OccurrenceStartDate", "OccurrenceEndDate", "DrugType",
                                  "DrugTypeExclude", "StopReason", "DrugSourceConcept", "Age",
                                  "Gender", "ProviderSpecialty", "VisitType",
                                  "Refills", "Quantity", "DaysSupply", "RouteConcept", "EffectiveDrugDose",
                                  "DoseUnit", "LotNumber"),
                 'Measurement' = c("First", "OccurrenceStartDate",  "MeasurementType",
                                 "MeasurementTypeExclude", "Operator", "MeasurementSourceConcept", "Age",
                                 "Gender", "ProviderSpecialty", "VisitType",
                                 "ValueAsNumber", "ValueAsConcept", "Unit", "RangeLow", "RangeHigh",
                                 "RangeLowRatio", "RangeHighRatio", "Abnormal"),
                 'Observation' = c("First", "OccurrenceStartDate", "ObservationType",
                                 "ObservationTypeExclude",  "ObservationSourceConcept", "Age",
                                 "Gender", "ProviderSpecialty", "VisitType",
                                 "ValueAsNumber", "ValueAsConcept","ValueAsString", "Qualifier"),
                 'ObservationPeriod' = c("First", "PeriodStartDate", "PeriodEndDate", "PeriodType",
                                       "PeriodLength", "AgeAtStart", "AgeAtEnd"),
                 'ProcedureOccurrence' = c("First", "OccurrenceStartDate", "ProcedureType",
                                         "ProcedureTypeExclude", "Modifier", "ProcedureSourceConcept", "Age",
                                         "Gender", "ProviderSpecialty", "VisitType"),
                 'VisitOccurrence' =  c("First", "OccurrenceStartDate", "OccurrenceEndDate", "VisitType",
                                      "VisitTypeExclude",  "VisitSourceConcept","VisitLength", "Age",
                                      "Gender", "ProviderSpecialty", "PlaceOfService")
  )
  if (!is.null(domain)){
    attOps <- attOps[[domain]]
  }
  return(attOps)


}
##########----------------createOpAttributes-------------################
##########----------------Dates---------------------------##############

#Occurrence Start Date Attribute wrapped for createOpAttribute
#' create occurrence Start Date Attribute
#'
#' This function creates an Operator attribute for the occurrence start date. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value a character string of the date
#' @param Extent a character string of the extent only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createOccurrenceStartDateAttribute <- function(Op, Value, Extent = NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.character(Value)) { #error handler for character date string
    stop("Value must be a character string to use the dat attribute")
  }
  if (!is.null(Extent)) { #error handler for not null extent
    if (!grepl("bt", Op)) { #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.character(Extent)) { #error handler for character date string or null
      stop("Extent must be a character string if not NULL to use the date attribute")
    }
  }
  createOpAttribute(Name = "OccurrenceStartDate", #create op attribute with domain already set up
                     Op = Op, #determin the operator
                     Value = Value, #add the value
                     Extent = Extent) #if between or not between determin extent
}


#' create occurrence End Date Attribute
#'
#' This function creates an Operator attribute for the occurrence end date. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value a character string of the date
#' @param Extent a character string of the extent only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createOccurrenceEndDateAttribute <- function(Op, Value, Extent = NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.character(Value)){ #error handler for character date string
    stop("Value must be a character string to use the dat attribute")
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.character(Extent)){ #error handler for character date string or null
      stop("Extent must be a character string if not NULL to use the date attribute")
    }
  }
  createOpAttribute(Name = "OccurrenceEndDate", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}


#' create Era start Date Attribute
#'
#' This function creates an Operator attribute for the era start date. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value a character string of the date
#' @param Extent a character string of the extent only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createEraStartDateAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.character(Value)){ #error handler for character date string
    stop("Value must be a character string to use the dat attribute")
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.character(Extent)){ #error handler for character date string or null
      stop("Extent must be a character string if not NULL to use the date attribute")
    }
  }
  createOpAttribute(Name = "EraStartDate", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create era End Date Attribute
#'
#' This function creates an Operator attribute for the era end date. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value a character string of the date
#' @param Extent a character string of the extent only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createEraEndDateAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.character(Value)){ #error handler for character date string
    stop("Value must be a character string to use the dat attribute")
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.character(Extent)){ #error handler for character date string or null
      stop("Extent must be a character string if not NULL to use the date attribute")
    }
  }
  createOpAttribute(Name = "EraEndDate", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create period Start Date Attribute
#'
#' This function creates an Operator attribute for the period start date. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value a character string of the date
#' @param Extent a character string of the extent only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createPeriodStartDateAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.character(Value)){ #error handler for character date string
    stop("Value must be a character string to use the dat attribute")
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.character(Extent)){ #error handler for character date string or null
      stop("Extent must be a character string if not NULL to use the date attribute")
    }
  }
  createOpAttribute(Name = "PeriodStartDate", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create period End Date Attribute
#'
#' This function creates an Operator attribute for the period end date. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value a character string of the date
#' @param Extent a character string of the extent only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createPeriodEndDateAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.character(Value)){ #error handler for character date string
    stop("Value must be a character string to use the dat attribute")
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.character(Extent)){ #error handler for character date string or null
      stop("Extent must be a character string if not NULL to use the date attribute")
    }
  }
  createOpAttribute(Name = "PeriodEndDate", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

##########----------------Numeric Range---------------------------##############

#' create Age Attribute
#'
#' This function creates an Operator attribute for person age. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the age
#' @param Extent an integer for the age only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createAgeAttribute <- function(Op, Value, Extent = NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "Age", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create EraLength Attribute
#'
#' This function creates an Operator attribute for person EraLength. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the EraLength
#' @param Extent an integer for the EraLength only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createEraLengthAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "EraLength", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create AgeAtStart Attribute
#'
#' This function creates an Operator attribute for person AgeAtStart. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the AgeAtStart
#' @param Extent an integer for the AgeAtStart only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createAgeAtStartAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "AgeAtStart", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create AgeAtEnd Attribute
#'
#' This function creates an Operator attribute for person AgeAtEnd. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the AgeAtEnd
#' @param Extent an integer for the AgeAtEnd only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createAgeAtEndAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "AgeAtEnd", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create GapDays Attribute
#'
#' This function creates an Operator attribute for person GapDays. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the GapDays
#' @param Extent an integer for the GapDays only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createGapDaysAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "GapDays", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create Refills Attribute
#'
#' This function creates an Operator attribute for person Refills. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the Refills
#' @param Extent an integer for the Refills only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createRefillsAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "Refills", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create Quantity Attribute
#'
#' This function creates an Operator attribute for person Quantity. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the Quantity
#' @param Extent an integer for the Quantity only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createQuantityAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "Quantity", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create DaysSupply Attribute
#'
#' This function creates an Operator attribute for person DaysSupply. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the DaysSupply
#' @param Extent an integer for the DaysSupply only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createDaysSupplyAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "DaysSupply", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create EffectiveDrugDose Attribute
#'
#' This function creates an Operator attribute for person EffectiveDrugDose. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the EffectiveDrugDose
#' @param Extent an integer for the EffectiveDrugDose only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createEffectiveDrugDoseAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "EffectiveDrugDose", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create ValueAsNumber Attribute
#'
#' This function creates an Operator attribute for person ValueAsNumber. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the ValueAsNumber
#' @param Extent an integer for the ValueAsNumber only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createValueAsNumberAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "ValueAsNumber", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create RangeLow Attribute
#'
#' This function creates an Operator attribute for person RangeLow. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the RangeLow
#' @param Extent an integer for the RangeLow only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createRangeLowAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "RangeLow", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create RangeHigh Attribute
#'
#' This function creates an Operator attribute for person RangeHigh. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the RangeHigh
#' @param Extent an integer for the RangeHigh only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createRangeHighAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "RangeHigh", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create RangeHighRatio Attribute
#'
#' This function creates an Operator attribute for person RangeHighRatio. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the RangeHighRatio
#' @param Extent an integer for the RangeHighRatio only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createRangeHighRatioAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "RangeHighRatio", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

#' create RangeLowRatio Attribute
#'
#' This function creates an Operator attribute for person RangeLowRatio. The user selects the type of
#' operator, value which is the minimal bound and extent which is the end point of a between bound. Extent is
#' only used if the op is bt or !bt.
#' @param Op defines logic for interpreting the numeric or date value.
#' @param Value an integer for the RangeLowRatio
#' @param Extent an integer for the RangeLowRatio only used if the op is bt or !bt
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createRangeLowRatioAttribute <- function(Op, Value, Extent=NULL){
  Op <- mapOperator(Op) #map operator to handle multiple inputs
  if (!is.integer(Value)){ #error handler for character date string
    Value <- as.integer(Value)
  }
  if (!is.null(Extent)){ #error handler for not null extent
    if (!grepl("bt", Op)){ #error handler for bt and !bt
      stop("Extent can only be used for bt and !bt")
    }
    if (!is.integer(Extent)){ #error handler for character date string or null
      Extent <- as.integer(Extent)
    }
  }
  createOpAttribute(Name = "RangeLowRatio", #create op attribute with domain already set up
                    Op = Op, #determin the operator
                    Value = Value, #add the value
                    Extent = Extent) #if between or not between determin extent
}

##########----------------createConceptAttributes-------------################
#' create condition source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createConditionSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Condition", ConceptSetExpression = ConceptSetExpression)
}

#' create procedure source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createProcedureSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Procedure", ConceptSetExpression = ConceptSetExpression)
}

#' create measurement source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createMeasurementSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Measurement", ConceptSetExpression = ConceptSetExpression)
}

#' create observation source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createObservationSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Observation", ConceptSetExpression = ConceptSetExpression)
}

#' create Drug source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createDrugSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Drug", ConceptSetExpression = ConceptSetExpression)
}

#' create Death source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createDeathSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Death", ConceptSetExpression = ConceptSetExpression)
}

#' create Device source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createDeviceSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Device", ConceptSetExpression = ConceptSetExpression)
}

#' create Visit source concept
#'
#' @param ConceptSetExpression the concept set expression we wish to deploy as a source concept attribute
#' This concept set expression should contain source codes, which may be non-standard.
#' @return a source concept attribute component
#' @include LowLevelCreateFn.R
#' @export
createVisitSourceConceptAttribute <- function(ConceptSetExpression){
  createSourceConceptAttribute("Visit", ConceptSetExpression = ConceptSetExpression)
}

##########----------------createConceptAttributes-------------################
#Value as Concept Attribute
#' create value as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createValueAsConceptAttribute <- function(conceptIds,
                                          connectionDetails = NULL,
                                          connection = NULL,
                                          vocabularyDatabaseSchema = NULL,
                                          tempEmulationSchema = NULL,
                                          mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "ValueAsConcept")
}


#' create gender as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createGenderAttribute <- function(conceptIds,
                                  connectionDetails = NULL,
                                  connection = NULL,
                                  vocabularyDatabaseSchema = NULL,
                                  tempEmulationSchema = NULL,
                                  mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "Gender")
}


#' create VisitType as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createVisitTypeAttribute <- function(conceptIds,
                                  connectionDetails = NULL,
                                  connection = NULL,
                                  vocabularyDatabaseSchema = NULL,
                                  tempEmulationSchema = NULL,
                                  mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "VisitType")
}

#' create DrugType as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createDrugTypeAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "DrugType")
}

#' create ProcedureType as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createProcedureTypeAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "ProcedureType")
}

#' create ObservationType as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createObservationTypeAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "ObservationType")
}

#' create MeasurementType as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createMeasurementTypeAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "MeasurementType")
}

#' create DeathType as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createDeathTypeAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "DeathType")
}

#' create DeviceType as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createDeviceTypeAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "DeviceType")
}

#' create ProviderSpecialty as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createProviderSpecialtyAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "ProviderSpecialty")
}

#' create PlaceOfService as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createPlaceOfServiceAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "PlaceOfService")
}

#' create Modifier as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createModifierAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "Modifier")
}

#' create Qualifier as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createQualifierAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "Qualifier")
}

#' create Unit as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createUnitAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "Unit")
}

#' create Operator as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createOperatorAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "Operator")
}

#' create RouteConcepts as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createRouteConceptsAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "RouteConcepts")
}

#' create DoseUnit as a concept Attribute
#'
#' This function creates an attribute out of concept values. input concept ids to actionize them within the attribute.
#' One must clarify if the concept ids should be mapped to standard (default is TRUE) or leave them as is. User needs
#' to be connected to an OMOP vocabulary to use the lookup functions.
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param        tempEmulationSchema  Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @param conceptIds a vector of concept ids. Must be connected to an OMOP vocabulary to use function
#' @param mapToStandard a logical that indicates whether the concept Ids should be mapped to standard concepts
#' @include LowLevelCreateFn.R
#' @return a componet of attribute class
#' @export
createDoseUnitAttribute <- function(conceptIds,
                                         connectionDetails = NULL,
                                         connection = NULL,
                                         vocabularyDatabaseSchema = NULL,
                                         tempEmulationSchema = NULL,
                                         mapToStandard =TRUE){
  createConceptAttribute(conceptIds = conceptIds,
                         connectionDetails = connectionDetails,
                         connection = connection,
                         vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         mapToStandard = mapToStandard,
                         name = "DoseUnit")
}

##########----------------createCorrelatedCriteriaAttribute-------------################

#'  Function to create an attribute for a correlated criteria
#'
#' @param Group a group object to add
#' @include LowLevelClasses.R
#' @include LowLevelCreateFn.R
#' @importFrom methods new
#' @return a correlated criteria attribute component
#' @export
createCorrelatedCriteriaAttribute <- function(Group){
  att <- new("CorrelatedCriteriaAttribute",
             Name = "CorrelatedCriteria",
             Group = Group@CriteriaExpression[[1]])
  comp <- createComponent(Name = "CorrelatedCriteriaAttribute",
                          ComponentType = "Attribute",
                          CriteriaExpression = list(att),
                          ConceptSetExpression = Group@ConceptSetExpression)
  return(comp)
}

##########----------------createLogicalAttributes-------------################
#First Attribute
#' create First Occurrence Attribute
#'
#' This function creates a attribute for first occurrence
#' @param logic toggle TRUE for first occurence
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createFirstAttribute <- function(logic =TRUE){
  createLogicalAttribute(name = "First", logic = logic)
}


#DrugTypeExclude Attribute
#' create exclude attribute for drug type
#'
#' This function creates a attribute for exclusion
#' @param logic toggle FALSE to not exclude
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createDrugTypeExcludeAttribute <- function(logic=FALSE){
  createLogicalAttribute(name = "DrugTypeExclude", logic = logic)
}


#VisitTypeExclude Attribute
#' create exclude attribute for visit type
#'
#' This function creates a attribute for exclusion
#' @param logic toggle FALSE to not exclude
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createVisitTypeExcludeAttribute <- function(logic=FALSE){
  createLogicalAttribute(name = "VisitTypeExclude", logic = logic)
}


#ConditionTypeExclude Attribute
#' create exclude attribute for condition type
#'
#' This function creates a attribute for exclusion
#' @param logic toggle FALSE to not exclude
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createConditionTypeExcludeAttribute <- function(logic=FALSE){
  createLogicalAttribute(name = "ConditionTypeExclude", logic = logic)
}


#ObservationTypeExclude Attribute
#' create exclude attribute for observation type
#'
#' This function creates a attribute for exclusion
#' @param logic toggle FALSE to not exclude
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createObservationTypeExcludeAttribute <- function(logic=FALSE){
  createLogicalAttribute(name = "ObservationTypeExclude", logic = logic)
}

#ProcedureTypeExclude Attribute
#' create exclude attribute for procedure type
#'
#' This function creates a attribute for exclusion
#' @param logic toggle FALSE to not exclude
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createProcedureTypeExcludeAttribute <- function(logic=FALSE){
  createLogicalAttribute(name = "ProcedureTypeExclude", logic = logic)
}

#MeasurementTypeExclude Attribute
#' create exclude attribute for measurement type
#'
#' This function creates a attribute for exclusion
#' @param logic toggle FALSE to not exclude
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createMeasurementTypeExcludeAttribute <- function(logic=FALSE){
  createLogicalAttribute(name = "MeasurementTypeExclude", logic = logic)
}

#DeathTypeExclude Attribute
#' create exclude attribute for death type
#'
#' This function creates a attribute for exclusion
#' @param logic toggle FALSE to not exclude
#' @include LowLevelCreateFn.R
#' @return a component of attribute class
#' @export
createDeathTypeExcludeAttribute <- function(logic=FALSE){
  createLogicalAttribute(name = "DeathTypeExclude", logic = logic)
}



