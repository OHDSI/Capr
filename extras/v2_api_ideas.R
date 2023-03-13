

# Anemia cohort --------
# text description
# Cohort Entry Events
# People enter the cohort when observing any of the following:
#
# condition occurrences of 'condition - anemia'.
# measurements of 'Hemoglobin measurement', who are male; numeric value < 13; unit: "gram per deciliter".
# measurements of 'Hemoglobin measurement', who are female; numeric value < 12; unit: "gram per deciliter".
# measurements of 'Hemoglobin measurement', who are male; numeric value < 130; unit: "gram per liter".
# measurements of 'Hemoglobin measurement', who are female; numeric value < 120; unit: "gram per liter".
# measurements of 'Hemoglobin measurement', with value as concept: "below reference range" or "abnormally low".
# observations of 'observation anemia'.
#
# Cohort Exit
# The cohort end date will be offset from index event's end date plus 1 day.
#
# Cohort Eras
# Entry events will be combined into cohort eras if they are within 90 days of each other.


# Capr r code -----------------

condition_anemia <- cs(descendants(439777,4013073,4013074))
observation_anemia <- cs(descendants(2617148,2617149,2617150,4028717,4028718,4029669,4029670,4264446,4288089,45766614))
hemoglobin_measurement <- cs(descendants(1234,1234,1234,1234,1234,1234,1234,1234))

# exclude example
cs <- cs(439777,4013073, exclude(descendants(4013074)))

# create a query - example
condition(condition_anemia)

# create a cohort
cohort(conditionOccurrence(condition_anemia))

anemia <- cohort(entry = entry(
  condition(condition_anemia),
  measurement(hemoglobin_measurement, male(), numeric_value(lt(13)), unit("gram per deciliter")), # unit can be text (for common units) or a concept set
  measurement(hemoglobin_measurement, male(), numeric_value(lt(130)), unit("gram per liter")),
  measurement(hemoglobin_measurement, female(), value(lt(12)), unit("gram per deciliter")), # possibly make value more generic
  measurement(hemoglobin_measurement, female(), value(lt(120)), unit("gram per liter")),
  measurement(hemoglobin_measurement, value(concept("below reference range", "abnormally low"))), # one option..
  ),
  exit = offest(day = 1, from = "index event end date")
)

# --------- other options
# numeric operators
# lt, gt, lte, gte, between, !between, eq


# some other options
anemia <- newCohort(entry = list(
  condition(condition_anemia),
  measurement(hemoglobin_measurement, value(concept("below reference range", "abnormally low"))), # one option. allow measurement concepts to be sepcified by name.
  measurement(hemoglobin_measurement, value(meas_value("below reference range", "abnormally low"))), # one option. allow measurement concepts to be sepcified by name.
  measurement(hemoglobin_measurement, value(meas_value$`below reference range`, meas_value$`abnormally low`)), # another option that would help users with autocomplete
  measurement(hemoglobin_measurement, value(voc$meas_value$`below reference range`, voc$meas_value$`abnormally low`)), # maybe put attribute concepts in a special object
  ),
  inlcusion = list(),
  exit = offest(day = 1, from = "index event end date"),
  era = 90 # how to include censoring?
)
