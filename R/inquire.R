setGeneric('inquire', function(capr_obj, cdm) {standardGeneric("inquire")})

setMethod('inquire',"Concept",
          function(capr_obj, cdm) {
            conceptId <- capr_obj@conceptId

            if (capr_obj@includeDescendants) {
              concept_tbl <- cdm$concept %>%
                dplyr::inner_join(cdm$concept_ancestor,
                                  by = c("concept_id" = "descendant_concept_id")) %>%
                dplyr::filter(ancestor_concept_id %in% conceptId) %>%
                dplyr::select(concept_id)

            } else {
              concept_tbl <- cdm$concept %>%
                dplyr::filter(concept_id %in% conceptId) %>%
                dplyr::select(concept_id)
            }
          }
)

setMethod('inquire', 'ConceptSet',
          function(capr_obj, cdm) {
            id <- capr_obj@id

            purrr::map(capr_obj@conceptSet, ~inquire(.x, cdm)) %>%
              purrr::reduce(dplyr::union)
          })


setMethod('inquire', 'ConceptSet',
          function(capr_obj, cdm) {
            id <- capr_obj@id

            purrr::map(capr_obj@conceptSet, ~inquire(.x, cdm)) %>%
              purrr::reduce(dplyr::union)
          })

setMethod('inquire',
          'conceptAttribute',
          function(capr_obj, cdm){
            concepts <- purrr::map(capr_obj@conceptSet, ~inquire(.x, cdm)) %>%
              purrr::reduce(dplyr::union) %>%
              dplyr::collect() %>%
              as.integer()

            #get correct attribute set up

            # Person Table ----------------
            if (capr_obj@name %in% c("male", "female")) {
              tb <- cdm$person %>%
                dplyr::filter(gender_concept_id %in% concepts) %>%
                dplyr::select(person_id)
              return(tb)
            }

            #Visit Table---------------------
            if (capr_obj@name %in% c("visitType")) {
              tb <- cdm$visit_occurrence %>%
                dplyr::filter(visit_concept_id %in% concepts) %>%
                dplyr::select(person_id)
              return(tb)
            }

          })



setMethod('inquire',
          'Query',
          function(capr_obj, cdm) {
            domain <- capr_obj@domain %>%
              SqlRender::camelCaseToSnakeCase()
            cse <- inquire(capr_obj@conceptSet, cdm)

            # Join on clinical table first -----------------------
            #symbols for specific columns
            concept_column <- gsub("occurrence", "concept_id", domain) %>%
              rlang::sym()
            table_id <- paste(domain, "id", sep = "_") %>%
              rlang::sym()
            start_date <- gsub("occurrence", "start_date", domain) %>%
              rlang::sym()
            end_date <- gsub("occurrence", "end_date", domain) %>%
              rlang::sym()

            tb <- cdm[[domain]] %>%
              dplyr::rename(concept_id = !! concept_column,
                            event_id = !! table_id,
                            start_date = !! start_date,
                            end_date = !! end_date) %>%
              dplyr::inner_join(cse, by = c("concept_id")) %>%
              dplyr::mutate(
                end_date = as.Date(dplyr::coalesce(end_date, start_date + lubridate::days(1)))
              ) %>%
              dplyr::select(person_id, event_id, start_date, end_date)


            # # Deal with attributes ----------------------------
            if (length(capr_obj@attributes)) {
              atb <- purrr::map(capr_obj@attributes, ~inquire(.x, cdm)) %>%
                purrr::reduce(dplyr::union)

              tb <- tb %>%
                dplyr::semi_join(atb, by = c("person_id"))

            }

            return(tb)

          }
)
