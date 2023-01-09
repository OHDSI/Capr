#test_inquire
## Test Ariadne idea
library(CDMConnector)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = strsplit(keyring::key_get("cdm_server"), "/")[[1]][[1]],
  dbname   = strsplit(keyring::key_get("cdm_server"), "/")[[1]][[2]],
  user     = keyring::key_get("cdm_user"),
  password = keyring::key_get("cdm_password"),
  port     = "5441"
)
cdm <- cdm_from_con(con,
                    cdm_schema = "cdm_531",
                    cdm_tables = tbl_group("all"),
                    write_schema = "martin_lavallee_results")

source("R/inquire.R")



inquire(t2d_cse, cdm)

inquire(t2d_query1, cdm)

inquire(t2d_query2, cdm)
