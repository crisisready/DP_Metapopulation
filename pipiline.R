# Data aggregation
require(dplyr)
require(bigrquery)

# Make the query from bigquery

query <- "select
activity_day as date,
concat(from_state_fips, from_county_fips) as from_fips,
concat(to_state_fips, to_county_fips) as to_fips,
sum(transitions) as count,
from `hangar-covid-19.camber_covid_aggregations.county_matrix_v4`
where from_state_fips = '36' and to_state_fips = '36' and activity_day >= '2020-09-01' and activity_day <= '2020-11-01'
group by(activity_day, from_fips, to_fips) "

# save data in a temporal file

data <- bq_dataset_query(x = "hangar-covid-19.camber_covid_aggregations", query = query) %>%
  bq_table_download(max_results = Inf)

# Overview of the data
data.cont <- data %>%
  group_by(count, activity_day) %>%
  tally()
movt.larg <- data.cont %>%
  filter(n >= 10) %>%
  print()

# Eliminate improbable movements

#  Data winsorizing
require(DescTools)
win.data <- data %>%
  mutate(win.var = Winsorize(data$count, minval = tail(Small(data$count, k = 3), 1), maxval = head(Large(data$count, k = 3), 1))) %>%
  print() # Here I do not know the reasonable range for a good winsorization
