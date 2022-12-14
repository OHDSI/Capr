# Classes -----------------------

## Occurrence --------------
setClass("Occurrence",
         slots = c(
           type = "character",
           count = "integer"
         ),
         prototype = list(
           type = NA_character_,
           count = NA_integer_
         )
)
## Count ----------------
setClass("Count",
         slots = c(
           occurrence = 'Occurrence',
           query = 'Query',
           aperture = 'EventAperture'),
         prototype = list(
           occurrence = new("Occurrence"),
           query = new("Query"),
           aperture = new("EventAperture")
         )
)
## Gorup ----------------
setClass("Group",
         slots = c(
           occurrence = 'Occurrence',
           criteria = 'list',
           group = 'list'),
         prototype = list(
           occurrence = new("Occurrence"),
           criteria = list(),
           group = list()
         )
)


# Constructors -----------------------

## Occurrences ----------------
exactly <- function(x) {
  new("Occurrence",
      type = "exactly",
      count = as.integer(x))
}

atLeast <- function(x) {
  new("Occurrence",
      type = "atLeast",
      count = as.integer(x))
}

atMost <- function(x) {
  new("Occurrence",
      type = "atMost",
      count = as.integer(x))
}

## Criteria -------------------
criteria <- function(occurrence,
                     query,
                     aperture) {
  new("Count",
      occurrence = occurrence,
      query = query,
      aperture = aperture)
}

## Group -------

is.group <- function(x) {
  methods::is(x) == "Group"
}

withAll <- function(...){
  items <- list(...)
  new("Group",
      occurrence = new("Occurrence", type = "all"),
      criteria = purrr::discard(items, is.group),
      group = purrr::keep(items, is.group)
      )
}


withAny <- function(...){
  items <- list(...)
  new("Group",
      occurrence = new("Occurrence", type = "any"),
      criteria = purrr::discard(items, is.group),
      group = purrr::keep(items, is.group)
  )
}

withAtLeast <- function(x, ...){
  items <- list(...)
  new("Group",
      occurrence = new("Occurrence", type = "atLeast", count = as.integer(x)),
      criteria = purrr::discard(items, is.group),
      group = purrr::keep(items, is.group)
  )
}

withAtLeast <- function(x, ...){
  items <- list(...)
  new("Group",
      occurrence = new("Occurrence", type = "atMost", count = as.integer(x)),
      criteria = purrr::discard(items, is.group),
      group = purrr::keep(items, is.group)
  )
}
