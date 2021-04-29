#dependencies
source("src/dependencies.R")

#model
run_seir_model() %>%
  run_metrics()
