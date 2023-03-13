

x <- cs(1,descendants(9),3)
y <- cs(1,3,descendants(9))


x == y

writeConceptSet(x, "example.json")

z <- uniqueConceptSets(list(x, y))

length(z)


as.data.frame(z[[1]])



