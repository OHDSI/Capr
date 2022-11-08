entry <- function(...,
                  observationWindow = observeWindow(),
                  primaryCriteriaLimit = "First",
                  additionalCriteria = NULL,
                  qualifiedLimit = "First") {

  cohort_entry <- new("CohortEntry",
                      entryEvents = list(...),
                      observationWindow = observationWindow,
                      primaryCriteriaLimit = primaryCriteriaLimit,
                      qualifiedLimit = qualifiedLimit
  )

  if (!is.null(additionalCriteria)) {
    cohort_entry@additionalCriteria <- additionalCriteria
  }

  return(cohort_entry)

}


cohort <- function(entry,
                   irs = NULL,
                   exit = NULL,
                   era = NULL) {

  cd <- new("Cohort", entry = entry)

  if (!is.null(irs)) {
    cd@irs <- irs
  }

  if (!is.null(exit)) {
    cd@exit <- new("Exit")
  }

  if (!is.null(era)) {
    cd@era <- new("Era")
  }

  return(cd)

}
