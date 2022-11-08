observeWindow <- function(priorDays = 0L,
                          postDays = 0L) {

  new("ObservationWindow",
      priorDays = priorDays,
      postDays = postDays)

}
