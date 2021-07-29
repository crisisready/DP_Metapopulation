#dependencies
source("src/dependencies.R")

for(itr_num in 1:as.numeric(iterations)){
  
  metrics <- run_seir_model() %>%
    run_metrics()
  
  #### Saving the output
  output_directory <- here(sprintf("data/seir-dp-metrics/noise_type=%s/ep=%s/", mechanism, epsilon))
  
  #### Creating directory if it doesnt exist
  if(!dir.exists(output_directory)){
    dir.create(file.path(output_directory))
  }
  
  #### Writing file within directory
  write_rds(metrics, sprintf("%s/iterations=%s.rds",output_directory, itr_num))
  
}

