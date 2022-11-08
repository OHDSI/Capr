
query <- function(domain, conceptSet, ...) {

  #bundle attributes as a list
  atb <- list(...)

  query <- new("Query",
               domain = domain,
               conceptSet = conceptSet,
               attributes = atb)
  return(query)

}

conditionOccurrence <- function(conceptSet, ...) {

  query(domain = "ConditionOccurrence",
        conceptSet = conceptSet,
        ...)

}

drugExposure <- function(conceptSet, ...) {

  query(domain = "DrugExposure",
        conceptSet = conceptSet,
        ...)

}

measurement <- function(conceptSet, ...) {

  query(domain = "Measurement",
        conceptSet = conceptSet,
        ...)

}

procedureOccurrence <- function(conceptSet, ...) {

  query(domain = "ProcedureOccurrence",
        conceptSet = conceptSet,
        ...)

}

