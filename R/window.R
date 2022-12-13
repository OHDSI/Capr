setClass("ObservationWindow",
         slots = c(priorDays = "integer",
                   postDays = "integer"),
         prototype = list(
           priorDays = 0L,
           postDays = 0L
         )
)


observeWindow <- function(priorDays = 0L,
                          postDays = 0L) {

  new("ObservationWindow",
      priorDays = as.integer(priorDays),
      postDays = as.integer(postDays))

}
