#### Creating function to load data 

source(here("src","utils.R"))

create_seir_matrices <- function(mechanism, epsilon, itr_num){
  
  #### Loading data downloaded from GCS through metapopulation.py script
  noisy_data_loc <- here(sprintf("data/mob-dp/noise_type=%s/ep=%s", mechanism, epsilon), sprintf("iteration=%s.csv", itr_num))

  #### Pull data
  data<-read_csv(noisy_data_loc)
  
  #### Replicate entries within the dataset
  replicate_data <- replicate(n = 500,filter(data, activity_day == sample(min(data$activity_day):max(data$activity_day), size = 1)), simplify = F)
  
  #### Specifying the max date - needs to be dynamic
  # Dynamic implementation - max_activity_date <- data$activity_date
  max_activity_date<-as.Date("2020-11-15", format="%Y-%m-%d")

  replicate_data$new_date<-c()
  
  for (i in 1:500){
    replicate_data[[i]]$new_date<-max_activity_date + days(1)
    max_activity_date = max_activity_date + days(1)
  }
  
  #### Binding Rows
  replicate_data <- replicate_data %>% bind_rows()
  
  #### Changing orientation
  replicate_data$activity_day <- replicate_data$new_date
  replicate_data$new_date <- NULL
  data$...1<-NULL
  replicate_data$...1<-NULL
  
  #### Binding with the original dataset
  data <- rbind(data, replicate_data)
  
  #### Concating columns
  data =  mutate(data, 
                 from_fips = paste(from_state_fips, from_county_fips, sep=""), 
                 to_fips = paste(to_state_fips, to_county_fips, sep=""))
  
  #### Averaging across the time-window
  data <- data %>% group_by(from_fips, to_fips, activity_day) %>% summarise(transitions = mean(transitions)) %>% ungroup() 
  
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
