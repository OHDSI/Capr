observeWindow <- function(priorDays = 0L,
                          postDays = 0L) {

  new("ObservationWindow",
      priorDays = as.integer(priorDays),
      postDays = as.integer(postDays))

}
