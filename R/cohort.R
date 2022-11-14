#' Create a cohort entry criteria
#'
#' @param ... Capr Queries
#' @param observationWindow
#' @param primaryCriteriaLimit
#' @param additionalCriteria
#' @param qualifiedLimit
#'
#' @return
#' @export
#'
#' @examples
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

  # Entry should be a list of queries or groups
  if (is(entry, "Query")) entry <- entry(entry)

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

writeCohort <- function(x, path) {
  checkmate::assertClass(x, "Cohort")
  checkmate::assertCharacter(path, len = 1, min.chars = "1", pattern = "\\.json$")

}

