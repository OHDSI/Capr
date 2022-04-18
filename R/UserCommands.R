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
######
#UI Functions

#' Function to save component
#'
#' This function saves the component as a json file. The component is converted from s4 to s3 to
#' fit the jsonlite function
#' @param obj the component you wish to save
#' @param saveName a name for the function you want to save
#' @param savePath a path to a file to save. Default is the active working directory
#' @return no return in r. json file written to a save point
#' @include LowLevelSaveFn.R
#' @export
saveComponent <- function(obj, saveName, savePath = getwd()){
  sc <- saveState(obj) #run save state for component
  objjson <- jsonlite::toJSON(sc, pretty=T, auto_unbox = T) #convert to json
  write(objjson, file=file.path(savePath,paste0(saveName, ".json"))) #if a savePath is provided append to name
  invisible(sc)
}


#' Function to load component
#'
#' This function loads the component from a json file to its s4 componentclass
#' @param path a path to the file we wish to load
#' @return returns a component
#' @include LowLevelLoadFn.R
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
#' @template     Connection
#' @template     VocabularyDatabaseSchema
#' @param jsonPath a path to the file we wish to import
#' @param returnHash if true returns a has table with all components necessary to build the
#' cohort definition including the cohort definition
#' @return returns the cohort definition
#' @include LowLevelBuildLangFn.R
#' @importFrom jsonlite read_json
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
readInCirce <- function(jsonPath,
                        connectionDetails,
                        connection = NULL,
                        vocabularyDatabaseSchema = NULL,
                        #tempEmulationSchema = NULL,
                        returnHash = FALSE){
  cohort <- jsonlite::read_json(jsonPath) #read in json from file path
  dbConnection <- createDatabaseConnectionLang(connectionDetails = connectionDetails,
                                               vocabularyDatabaseSchema = vocabularyDatabaseSchema)
  cohortBuild <- getCohortDefinitionCall(cohort)$createCDCall #get the functions needed to build the cohort
  cohortCaller <- getCohortDefinitionCall(cohort)$CohortCall # get the caller function to make final cohort
  cohortBuild <- Filter(Negate(is.null),cohortBuild) #remove null spaces
  exeEnv <- new.env() #create an execution environemnt
  for (i in seq_along(dbConnection)){
    eval(dbConnection[[i]], envir = exeEnv) #run connection setup in execution environment
  }

  for(i in seq_along(cohortBuild)){ #for each item in list
    purrr::map(cohortBuild[[i]], ~eval(.x, envir = exeEnv)) #evaluate expression in execution environment
  }
  DatabaseConnector::disconnect(exeEnv$connection) #disconnect
  eval(cohortCaller, envir = exeEnv) #evaluate the cohort Caller in the execution environemnt
  if (returnHash) {
    rm(connection, connectionDetails, vocabularyDatabaseSchema,
       #tempEmulationSchema,
       envir = exeEnv)
    ret <- exeEnv
  } else {
    ret <- exeEnv$CohortDefinition #if return Hash is false returns the cohort definition
  }
  return(ret) #return the cohort definition as CAPR object
}


##############--------write R function calls -----------###################
#' Function to write capr calls from a circe json
#'
#' This function writes the CAPR calls used to build the cohort definition
#' defined in the circe json . The ouput is a txt file with executable R language
#' @param jsonPath a path to the file we wish to import
#' @param txtPath a path to the txt file we wish to save
#' @return no return but saves the CAPR calls to build a cohort in a txt file
#' @include LowLevelBuildLangFn.R
#' @importFrom jsonlite read_json
#' @importFrom purrr map
#' @export
writeCaprCall <- function(jsonPath, txtPath){
  cohort <- jsonlite::read_json(jsonPath) #read in json from file path
  dbConnection <- unlist(createDatabaseConnectionLang(),use.names = FALSE) #use dummy credentials
  cohortBuilder <- getCohortDefinitionCall(cohort) #build cohort definition call
  tt <- unlist(cohortBuilder, use.names = FALSE) #unlist list
  sink(txtPath) #create place to save txt
  for (i in seq_along(dbConnection)){
    print(dbConnection[[i]]) #print through dummy credentials
  }
  for (i in seq_along(tt)){
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
#' If generateOptions is left NULL, then this function will give a lite return of just the json to be activated.
#' with circe R.
#' @include LowLevelCoercionFn.R
#' @importFrom CirceR cohortExpressionFromJson cohortPrintFriendly buildCohortQuery
#' @importFrom RJSONIO toJSON
#' @return If the generate options is supplied this function returns a three
#' tiered list containing the the circe json, a text read and ohisql.
#' If an error occurs the ohdisql slot will be NA and the user should review the
#'  circe cohort definition for potential errors.
#' If the generateOptions is not supplied it will just return the json
#' @export
compileCohortDefinition <- function(CohortDefinition, generateOptions = NULL){

  circeS3 <- convertCohortDefinitionToCIRCE(CohortDefinition) #convert cohort definition to circe s3 object
  circeJson <- RJSONIO::toJSON(circeS3)
  if (!is.null(generateOptions)) {
    circeJson2 <- CirceR::cohortExpressionFromJson(circeJson)
    cohortRead <- CirceR::cohortPrintFriendly(circeJson2)
    ohdisql <- CirceR::buildCohortQuery(circeJson2, generateOptions)
    #create list with cohort definition converted to circe, circe json and ohdiSQL
    cohortList <- list('circeJson' = circeJson,
                       'cohortRead' = cohortRead,
                       'ohdiSQL' = ohdisql)
  } else {
    cohortList <- circeJson
  }

  #return cohort list
  return(cohortList)

  #old
  #circeJson <-jsonlite::toJSON(circe, pretty=T, auto_unbox = TRUE) #convert circe object to json
  # ohdisql <- tryCatch(CirceCompileR::compile(circeJson), #run circe compiler
  #                     error =function(x) { #if an error occurs in compiler send error message
  #                       #error message
  #                       message("Circe Object can not Compile. Please review circe cohort definition to find error")
  #                       NA_character_#return NA since compilation failed
  #                     })#end try catch error



  #end of function
}


########------Create Cohort Dataframe for generation------------#################

#' Create a dataframe that can be used by cohort generator to build cohorts into backend table
#'
#' This function converts a list of Cohort definitions into a dataframe that can be used by cohort
#' generator and cohort diagnostics to build and evaluate cohorts. The dataframe returns an atlasId column and
#' a cohortId column which are equivalent. Further it creates a column for the ohdisql, json, read and name of
#' cohort.
#'
#' @param cohortList a list of cohorts to turn into a dataframe
#' @param generateStats select true if you want inclusion rule stats or false for no stats
#' @importFrom CirceR ccreateGenerateOptions
#' @importFrom purrr map_chr
#' @return A data frame of cohort definitions containing meta data
#' @export
createCohortDataframe <- function(cohortList, generateStats = TRUE) {

  check <- purrr::map_chr(cohortList, ~methods::is(.x))

  if(!all(check == "CohortDefinition")) {
    stop("all cohorts need to be a Capr CohortDefinition class")
  }

  # get cohort names
  cohortName <-purrr::map_chr(cohortList, ~slot(slot(.x, "CohortDetails"), "Name"))

  #set basic genops for CirceR
  genOp <- CirceR::createGenerateOptions(generateStats = generateStats)

  #get the info from compiler
  cohortInfo <- purrr::map(cohortList, ~compileCohortDefinition(.x,
                                                                generateOptions = genOp))

  #get each element from compile
  cohortSql <- purrr::map_chr(cohortInfo, ~getElement(.x, "ohdiSQL"))
  cohortJson <- purrr::map_chr(cohortInfo, ~getElement(.x, "circeJson"))
  cohortRead <- purrr::map_chr(cohortInfo, ~getElement(.x, "cohortRead"))


  #create Data frame
  df <- data.frame('atlasId' = seq_along(cohortList),
                   'cohortId' = seq_along(cohortList),
                   'cohortName'= cohortName,
                   'sql'= cohortSql,
                   'json' = cohortJson,
                   'read' = cohortRead)
  return(df)
}

