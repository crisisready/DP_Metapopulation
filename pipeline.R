#dependencies
source("src/dependencies.R")

mechanism <- as.character(commandArgs(TRUE)[2], sep =",")
epsilon <- as.numeric((as.character(commandArgs(TRUE)[3], sep =",")))
iterations <- as.numeric((as.character(commandArgs(TRUE)[4], sep =",")))

for(itr_num in 1:as.numeric(iterations)){
  
  seir_matrix <- run_seir_model()
  metrics <- run_metrics(seir_matrix)
  
  #### Saving the output
  output_directory <- here(sprintf("data/seir-dp-metrics/noise_type=%s/ep=%s", mechanism, epsilon))
  
  #### Creating directory if it doesnt exist
  if(!dir.exists(output_directory)){
    dir.create(file.path(output_directory))
  }
  
  #### Writing file within directory
  write_rds(metrics, sprintf("%s/iteration=%s.rds",output_directory, itr_num))
  
}
