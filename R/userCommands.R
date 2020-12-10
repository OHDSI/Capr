# Copyright 2020 Observational Health Data Sciences and Informatics
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
######
#UI Functions

#' Function to save component
#'
#' This function saves the component as a json file. The component is converted from s4 to s3 to
#' fit the jsonlite function
#' @param x the component you wish to save
#' @param saveName a name for the function you want to save
#' @param savePath a path to a file to save. Default is the active working directory
#' @return no return in r. json file written to a save point
#' @include lowLevelSaveFn.R
#' @export
saveComponent <- function(x, saveName, savePath = getwd()){
  sc <- saveState(x) #run save state for component
  objjson <- jsonlite::toJSON(sc, pretty=T, auto_unbox = T) #convert to json
  write(objjson, file=file.path(savePath,paste0(saveName, ".json"))) #if a savePath is provided append to name
  invisible(sc)
}


#' Function to load component
#'
#' This function loads the component from a json file to its s4 componentclass
#' @param path a path to the file we wish to load
#' @return returns a component
#' @include lowLevelLoadFn.R
#' @importFrom jsonlite read_json
#' @export
loadComponent <- function(path){
  json <- jsonlite::read_json(path)
  comp <- as.ComponentLoad(json)
  return(comp)
}


##################-------------read json Cohort-----------------##################
#' Function to read in a circe json
#'
#' This function reads a circe json an builds the cohort definition in an execution space
#' @param jsonPath a path to the file we wish to import
#' @return returns the cohort definition
#' @include lowLevelBuildLangFn.R
#' @importFrom jsonlite read_json
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
readInCirce <- function(jsonPath){
  cohort <- jsonlite::read_json(jsonPath) #read in json from file path
  cohortBuild <- getCohortDefinitionCall(cohort)$createCDCall #get the functions needed to build the cohort
  cohortCaller <- getCohortDefinitionCall(cohort)$CohortCall # get the caller function to make final cohort
  cohortBuild <- Filter(Negate(is.null),cohortBuild) #remove null spaces
  exeEnv <- new.env() #create an execution environemnt
  for(i in seq_along(cohortBuild)){ #for each item in list
    purrr::map(cohortBuild[[i]], ~eval(.x, envir = exeEnv)) #evaluate expression in execution environment
  }
  eval(cohortCaller, envir = exeEnv) #evaluate the cohort Caller in the execution environemnt
  return(exeEnv$CohortDefinition) #return the cohort definition as CAPR object
}


##############--------write R function calls -----------###################
#' Function to write capr calls from a circe json
#'
#' This function writes the CAPR calls used to build the cohort definition
#' defined in the circe json . The ouput is a txt file with executable R language
#' @param jsonPath a path to the file we wish to import
#' @param txtPath a path to the txt file we wish to save
#' @return no return but saves the CAPR calls to build a cohort in a txt file
#' @include lowLevelBuildLangFn.R
#' @importFrom jsonlite read_json
#' @importFrom purrr map
#' @export
writeCaprCall <- function(jsonPath, txtPath){
  cohort <- jsonlite::read_json(jsonPath) #read in json from file path
  cohortBuilder <- getCohortDefinitionCall(cohort) #build cohort definition call
  tt <- unlist(cohortBuilder, use.names = FALSE) #unlist list
  sink(txtPath) #create place to save txt
  for(i in seq_along(tt)){
    print(tt[[i]])#print through loop o R language
  }
  sink() #close file conn
}


########------compile cohort definition------------#################

#' Convert cohort definition object to CIRCE and run through circe compiler
#'
#' This function converts a Cohort Definition class object to a CIRCE expression, creates the json and compiles the
#' circe json to create ohdisql to run queries against a dbms containing OMOP cdm data
#'
#' @param CohortDefinition input cohort Definition class object
#' @param generateOptions the options for building the ohdisql using CirceR::createGenerateOptions
#' @include lowLevelCoercionFn.R
#' @importFrom CirceR cohortExpressionFromJson cohortPrintFriendly buildCohortQuery
#' @importFrom RJSONIO toJSON
#' @return A three tiered list containing the circe converted cohort definition, the circe json and ohisql.
#' If an error occurs the ohdisql slot will be NA and the user should review the circe cohort definition for potential errors.
#' @export
compileCohortDefinition <- function(CohortDefinition, generateOptions){
  circeS3 <- convertCohortDefinitionToCIRCE(CohortDefinition) #convert cohort definition to circe s3 object
  circeJson <- RJSONIO::toJSON(circeS3)
  circeJson2 <- CirceR::cohortExpressionFromJson(circeJson)
  cohortRead <- CirceR::cohortPrintFriendly(circeJson2)
  ohdisql <- CirceR::buildCohortQuery(circeJson2, generateOptions)

  #old
  #circeJson <-jsonlite::toJSON(circe, pretty=T, auto_unbox = TRUE) #convert circe object to json
  # ohdisql <- tryCatch(CirceCompileR::compile(circeJson), #run circe compiler
  #                     error =function(x) { #if an error occurs in compiler send error message
  #                       #error message
  #                       message("Circe Object can not Compile. Please review circe cohort definition to find error")
  #                       NA_character_#return NA since compilation failed
  #                     })#end try catch error

  #create list with cohort definition converted to circe, circe json and ohdiSQL
  cohortList <- list('circeS3' = circeS3,
                     'circeJson' = circeJson,
                     'cohortRead' = cohortRead,
                     'ohdiSQL' = ohdisql)
  #return cohort list
  return(cohortList)
  #end of function
}
