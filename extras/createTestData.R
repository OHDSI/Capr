# Create some test data for readConceptSet
library(CDMConnector)
library(CodelistGenerator)
con <- DBI::dbConnect(duckdb::duckdb(), eunomia_dir())

cdm_schema <- "main"

cdm <- cdm_from_con(con = con,
                    cdm_schema = cdm_schema,
                    cdm_tables = tbl_group("vocab"))

cdm$concept %>%
  filter(concept_id %in% c(35208414)) %>%
  collect() %>%
  readr::write_csv("inst/extdata/selectionFromConceptTable.csv")

asthma1 <- getCandidateCodes(cdm = cdm,
                             keywords = "asthma",
                             domains = "Condition")

readr::write_csv(asthma1, "inst/extdata/codelistGeneratorOutputExample.csv")
