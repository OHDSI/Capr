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




# create cohort example
#index event t2d with 365 pior observation
# inclusion rules: 1) exactly 0 t1d 2) at least 1 abnormal lab
cd <- cohort(
  entry = entry(
    # condition occurrence of t2d in males
    condition(
      #concept set
      cs(descendants(201826L)),
      #additional attributes
      male()),
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
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)


# individual pieces
#create concepts
t2d_cse <- cs(descendants(201826L))
hba1c_cse <- cs(descendants(4184637L))

#create queries
t2d_query <- condition(t2d_cse)
t2d_query2 <- condition(t2d_cse, male())

hba1c_query <- measurement(hba1c_cse)
hba1c_query <- measurement(hba1c_cse,
                           valueAsNumber(lt(13)),
                           unit(8713L))

# test count
crit1 <- criteria(
  exactly(0),
  condition(t2d_cse, male()),
  duringInterval(eventStarts(allDaysBefore(), offset(-1)))
)

crit2 <- criteria(
  exactly(0),
  condition(hba1c_cse, male()),
  duringInterval(eventStarts(allDaysBefore(), offset(-1)))
)


gg <- withAll(crit1, crit2)
