#### Creating function to load data 

source(here("src","utils.R"))

create_seir_matrices <- function(mechanism, epsilon, itr_num){
  
  #### Loading data downloaded from GCS through metapopulation.py script
  noisy_data_loc <- here(sprintf("./data/mob-dp/noise_type=%s/ep=%s", mechanism, epsilon), sprintf("iteration=%s.csv", itr_num))
  print(noisy_data_loc)
  
  #### Pull data
  data<-read_csv(noisy_data_loc)
  
  #### Concating columns
  data =  mutate(data, 
                 from_fips = paste(from_state_fips, from_county_fips, sep=""), 
                 to_fips = paste(to_state_fips, to_county_fips, sep=""))
  
  #### Selecting only necessary columns
  data = data %>% select(activity_day, from_fips, to_fips, transitions)
  
  #### Pull unique dates 
  dates <- unique(data$activity_day) %>% sort()
  
  #### Pull unique fips 
  fips <- c(data$from_fips, data$to_fips) %>% unique()
  
  #### Rescale Camber values 
  rescale_values <- read.csv("./resources/camber_pop_rescale.csv") %>%
    as_tibble() %>%
    mutate(from_fips = as.character(from_fips))
  
  rescaled_data <- data %>%
    left_join(rescale_values, by = "from_fips") %>% 
    mutate(transitions = round(transitions * rescale, 0)) %>%
    dplyr::select(activity_day, from_fips, to_fips, transitions)
  
  #### Create list of transition matrices
  M <- lapply(dates, function(x) convertToMatrix(dat = rescaled_data, day = x))
  
  return(M)
  
}
