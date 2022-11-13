
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

  query(domain = "conditionOccurrence",
        conceptSet = conceptSet,
        ...)

}

drugExposure <- function(conceptSet, ...) {

  query(domain = "drugExposure",
        conceptSet = conceptSet,
        ...)

}

measurement <- function(conceptSet, ...) {

  query(domain = "measurement",
        conceptSet = conceptSet,
        ...)

}

procedureOccurrence <- function(conceptSet, ...) {

  query(domain = "procedureOccurrence",
        conceptSet = conceptSet,
        ...)

}

