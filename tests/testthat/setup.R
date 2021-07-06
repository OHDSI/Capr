# Download the JDBC drivers used in the tests

oldJarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempfile("jdbcDrivers"))
downloadJdbcDrivers("postgresql")
downloadJdbcDrivers("sql server")
downloadJdbcDrivers("oracle")

#set up save space for capr
Sys.setenv("Capr_Save_Space" = tempdir("saveCapr"))

withr::defer({
  #remove temp files for database connector
  unlink(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"), recursive = TRUE, force = TRUE)
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = oldJarFolder)

  #remove temp dir for Capr Save space used for testing
  unlink(Sys.getenv("Capr_Save_Space"), recursive = TRUE, force = TRUE)

}, testthat::teardown_env())

