# test as.list and collectConcepts

#testing script
source("R/conceptSet.R")
source("R/query.R")
source("R/attributes-concept.R")
source("R/attributes-op.R")
source("R/window.R")
source("R/criteria.R")
source("R/exit.R")
source("R/cohort.R")
library(tidyverse)


#attribute class --------------

## op attribute numeric  ----------
x <- valueAsNumber(lt(13))
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)
## op attribute date ----------
x <- startDate(gt(lubridate::date("2012-01-01")))
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)
## concept attribute ------------
x <- male()
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)

x <- unit(8713L, 8636L)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)
# query class -----------

## simple test --------
x <- condition(cs(descendants(201826L)), male())
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)
## test with multiple attributes -------------
x <- measurement(
  cs(descendants(4184637L)),
  valueAsNumber(lt(13)),
  unit(8713L)
)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)

#windows
x <- eventStarts(-Inf, -1)
x <- duringInterval(eventStarts(-Inf, -1))

# criteria classes ---------------
## occurrence-------------
x <- exactly(0)
as.list(x)

## criteria-----------
x <- criteria(
  exactly(0),
  condition(cs(descendants(201254L))),
  duringInterval(eventStarts(-Inf, -1))
)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)
## group---------
x <- withAll(
  criteria(
    exactly(0),
    condition(cs(descendants(201254L))),
    duringInterval(eventStarts(-Inf, -1))
  ),
  criteria(
    atLeast(1),
    measurement(
      cs(descendants(4184637L)),
      valueAsNumber(lt(13)),
      unit(8713L)),
    duringInterval(eventStarts(-Inf, -1))
  )
)
tt <- as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)
# exit classes --------

## fixed duration ------
x <- fixedExit(index = "startDate", offsetDays = 40L)
as.list(x)

## cts Drug -----------
x <- drugExit(conceptSet = cs(descendants(1503297L)),
              persistenceWindow = 30L,
              surveillanceWindow = 7L)
as.list(x)

## observation exit --------
x <- observationExit()
as.list(x)

## censoring criteria------------
x <- censoringEvents(
  death(),
  drug(cs(descendants(1503297L)))
)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)

# cohort classes ---------

## entry -----------
x <- entry(
  condition(cs(descendants(201826L)), male()),
  observationWindow = continuousObservation(365, 0)
)

tt <- as.list(x)

## attrition -------

### with names -----
x <- attrition(
  'no t1d' = withAll(
    criteria(
      exactly(0),
      condition(cs(descendants(201254L))),
      duringInterval(eventStarts(-Inf, -1))
    )
  ),
  'abnormal hba1c' = withAll(
    criteria(
      atLeast(1),
      measurement(
        cs(descendants(4184637L)),
        valueAsNumber(lt(13)),
        unit(8713L)),
      duringInterval(eventStarts(-Inf, -1))
    )
  )
)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)


### without names ----

x <- attrition(
  withAll(
    criteria(
      exactly(0),
      condition(cs(descendants(201254L))),
      duringInterval(eventStarts(-Inf, -1))
    )
  ),
  withAll(
    criteria(
      atLeast(1),
      measurement(
        cs(descendants(4184637L)),
        valueAsNumber(lt(13)),
        unit(8713L)),
      duringInterval(eventStarts(-Inf, -1))
    )
  )
)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)

## exit -------
### test with a fixed exit and censor-----
x <- exit(
  endStrategy = fixedExit(index = "startDate", offsetDays = 30L),
  censor = censoringEvents(
    death(),
    drug(cs(descendants(1503297L)))
  )
)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)

### test with observation exit ---------
x <- exit(
  endStrategy = observationExit()
)
as.list(x)

### test with drug exposure----
x <- exit(
  endStrategy = drugExit(conceptSet = cs(descendants(1503297L)),
                         persistenceWindow = 30L,
                         surveillanceWindow = 7L)
)
as.list(x)

### test with drug exposure and censor----
x <- exit(
  endStrategy = drugExit(conceptSet = cs(descendants(1503297L)),
                         persistenceWindow = 30L,
                         surveillanceWindow = 7L),
  censor = censoringEvents(
    condition(cs(descendants(201254L)))
  )
)
## era ----------

### pad days ----
x <- era(30L)
as.list(x)

#### censor window -----

x <- era(30L, studyStartDate = lubridate::date("2012-01-01"))
as.list(x)

## cohort ----
x <- cohort(
  entry = entry(
    condition(cs(descendants(201826L)), male()),
    observationWindow = continuousObservation(365, 0)
  ),
  attrition = attrition(
    'no t1d' = withAll(
      criteria(
        exactly(0),
        condition(cs(descendants(201254L))),
        duringInterval(eventStarts(-Inf, -1))
      )
    ),
    'abnormal hba1c' = withAll(
      criteria(
        atLeast(1),
        measurement(
          cs(descendants(4184637L)),
          valueAsNumber(lt(13)),
          unit(8713L)),
        duringInterval(eventStarts(-Inf, -1))
      )
    )
  )
)
as.list(x)
jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE)
