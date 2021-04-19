#Camber data pipeline 
# Data aggregation
library(tidyverse)
library(bigrquery)
library(here)
library(reshape2)

#source code 
source(here("src","utils.R"))

# Query for BigQuery
query <- "with tmp as (select activity_day,
concat(from_state_fips, from_county_fips) as from_fips,
concat(to_state_fips, to_county_fips) as to_fips, transitions,
from `hangar-covid-19.camber_covid_aggregations.county_modal_matrix_v4`
where from_state_fips = '36' and to_state_fips = '36' and activity_day >= '2020-09-01' and activity_day <= '2020-11-01')
select activity_day, from_fips, to_fips, sum(transitions) as transitions from tmp group by activity_day, from_fips, to_fips"

# pull data
data <- bq_dataset_query(x = "hangar-covid-19.camber_covid_aggregations", query = query) %>%
  bq_table_download(max_results = Inf)

rm(query)

# pull unique dates 
dates <- unique(data$activity_day) %>% sort()

# pull unique fips 
fips <- c(data$from_fips, data$to_fips) %>% unique()

#rescale Camber values 
rescale_values <- read.csv("../resources/camber_pop_rescale.csv") %>%
  as_tibble() %>%
  mutate(from_fips = as.character(from_fips))

rescaled_data <- data %>%
  left_join(rescale_values, by = "from_fips") %>% 
  mutate(transitions = round(transitions * rescale, 0)) %>%
  dplyr::select(activity_day, from_fips, to_fips, transitions)

#create list of transition matrices
M <- lapply(dates, function(x) convertToMatrix(dat = rescaled_data, day = x))

