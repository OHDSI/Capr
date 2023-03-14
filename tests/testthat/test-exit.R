test_that("Drug Exit Works", {
  aceI <- c(1335471, # benzipril
            1334456) # ramipril

  aceCS <- cs(aceI, name = "Ace Inhibitors")

  ee1 <- exit(
    endStrategy = drugExit(
      conceptSet = aceCS,
      persistenceWindow = 30L,
      surveillanceWindow = 7L)
  )
  expect_s4_class(ee1, "CohortExit")
  expect_s4_class(ee1@endStrategy, "DrugExposureExit")

  cd <- cohort(
    entry = entry(
      drug(aceCS),
      observationWindow = continuousObservation(priorDays = 0L, postDays = 0L),
      primaryCriteriaLimit = "First"
    ),
    exit = ee1
  )

  jj <- toCirce(cd)
  expect_named(jj$EndStrategy, c("CustomEra"))

})

test_that("Fixed Duration Exit Works", {

  aceI <- c(1335471, # benzipril
            1334456) # ramipril

  aceCS <- cs(aceI, name = "Ace Inhibitors")

  ee1 <- exit(
    endStrategy = fixedExit(
      index = "startDate",
      offsetDays = 30L
    )
  )
  expect_s4_class(ee1, "CohortExit")
  expect_s4_class(ee1@endStrategy, "FixedDurationExit")

  cd <- cohort(
    entry = entry(
      drug(aceCS),
      observationWindow = continuousObservation(priorDays = 0L, postDays = 0L),
      primaryCriteriaLimit = "First"
    ),
    exit = ee1
  )

  jj <- toCirce(cd)
  expect_named(jj$EndStrategy, c("DateOffset"))

})

