#### Dependencies
source("src/dependencies.R")

#### Taking commands from the metapopulation.py script
mechanism <- as.character(commandArgs(TRUE)[6], sep =",")
epsilon <- as.numeric((as.character(commandArgs(TRUE)[7], sep =",")))
iterations <- as.numeric((as.character(commandArgs(TRUE)[9], sep =",")))

#### Running model output and metric calculations for each iteration 
for(itr_num in 1:as.numeric(iterations)){
  seir_matrix <- run_seir_model()
  model_output_loc <- here(sprintf("data/model-output/noise_type=%s/ep=%s/iteration=%s.rds", mechanism, epsilon, itr_num))
  if(!dir.exists(model_output_loc)){
    dir.create(model_output_loc)
  }
  write_rds(seir_matrix, model_output_loc)
  print("Saved model outputs to path!")
  metrics <- run_metrics(seir_matrix)
  
  #### Saving the output
  seir_matrix_directory<- here(sprintf("data/seir-model-outputs/noise_type=%s/ep=%s", mechanism, epsilon))
  output_directory <- here(sprintf("data/seir-dp-metrics/noise_type=%s/ep=%s", mechanism, epsilon))
  
  #### Creating directory if it doesnt exist
  if(!dir.exists(seir_matrix_directory)){
    dir.create(file.path(seir_matrix_directory))
  }
  write_rds(seir_matrix, sprintf("%s/iteration=%s.rds",seir_matrix_directory, itr_num))
  if(!dir.exists(output_directory)){
    dir.create(file.path(output_directory))
  }
  
  #### Writing file within directory
  write_rds(metrics, sprintf("%s/iteration=%s.rds",output_directory, itr_num))
}
