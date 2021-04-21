#sandbox

#Camber data pipeline 
# Data aggregation
library(tidyverse)
library(bigrquery)
library(tidycensus)
library(yaml)

# Query for BigQuery
query <- "with tmp as (select activity_day,
concat(from_state_fips, from_county_fips) as from_fips,
concat(to_state_fips, to_county_fips) as to_fips, transitions,
from `hangar-covid-19.camber_covid_aggregations.county_modal_matrix_v4`
where from_state_fips = '36' and to_state_fips = '36' and activity_day >= '2020-09-01' and activity_day <= '2020-11-01')
select activity_day, from_fips, to_fips, avg(transitions) as transitions from tmp group by activity_day, from_fips, to_fips"

# pull data
data <- bq_dataset_query(x = "hangar-covid-19.camber_covid_aggregations", query = query) %>%
  bq_table_download(max_results = Inf)

rm(query)

# pull unique dates 
dates <- unique(data$activity_day) %>% sort()

# pull unique fips 
fips <- c(data$from_fips, data$to_fips) %>% unique()

#acs pop
census_api_key(read_yaml("../resources/auth.yaml")$census_api_key)
census_pop <- get_estimates(geography = "county", product = "population", state = "NY") %>%
  filter(variable == "POP") %>%
  dplyr::select("from_fips" = GEOID, "acs_pop" = value)

#calculate average Camber population 
rescale_value <- data %>%
  group_by(activity_day, to_fips) %>%
  summarise(pop = sum(transitions)) %>%
  ungroup() %>%
  group_by(to_fips) %>%
  summarise(pop = mean(pop)) %>%
  ungroup() %>%
  left_join(census_pop, by = c("to_fips" = "from_fips")) %>%
  rename("from_fips" = "to_fips") %>%
  mutate(rescale = acs_pop / pop)

write.csv(rescale_value,"../resources/camber_pop_rescale.csv", row.names = F)



