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

setClass("Count",
         slots = c(
           query = 'Query',
           window = 'Window',
           occurrence = 'Occurrence'),
         prototype = list(
           query = new("Query"),
           window = new("Window"),
           occurrence = new("Occurrence")
         )
)



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



