final_calc <- function(in_file_loc, out_file_loc, baseline_noise){
  #throws error if input file doesn't exist
  if(!dir.exists(in_file_loc)){
    print("Data folder does not exist, please verify.")
    print(paste0("Folder: ", in_file_loc))
    break()
  }
  
  #check to see if output file exists and creates if not
  if(!dir.exists(out_file_loc)){
    dir.create(out_file_loc)
  }
  
  #create data table of runs
  runs <- tibble("eps_file" = dir(in_file_loc)) %>%
    rowwise() %>%
    mutate(eps = str_split(eps_file, "=")[[1]][2])
  runs <- lapply(
    runs$eps_file, 
    function(x){
      dir(paste0(in_file_loc,"//", x)) %>% 
        tibble() %>%
        mutate(eps = x) %>%
        set_names(c("itr_file", "eps_file"))
    }
  ) %>%
    bind_rows() %>%
    right_join(runs, by = "eps_file") %>%
    rowwise() %>%
    mutate(itr = str_split(itr_file, "=")[[1]][2] %>% str_split("\\.") %>% {.[[1]][1]})
  #check that there are the same number of iterations in each folder 
  check <- runs %>%
    group_by(eps_file) %>%
    summarise(count = n()) %>%
    mutate(count = count == max(count))
  
  if(nrow(filter(check, !count)) > 0){
    error <- filter(check, !count)
    print("Some folders don't have the same # of iterations, these have been output in the `error` object")
    break()
  }
  
  #run calculations
  lapply(
    unique(runs$itr), 
    function(x){
      itr_run(baseline_noise, itr_num = x, runs = runs, in_file_loc = in_file_loc, out_file_loc = out_file_loc)
    }
  )
  
}

#run calculations 
itr_run <- function(baseline_noise, itr_num, runs, in_file_loc = in_file_loc, out_file_loc = out_file_loc){
  
  itr_list <- filter(runs, itr == itr_num) %>%
    mutate(file_loc = paste0(in_file_loc,"/",eps_file,"/",itr_file))
  
  out <- lapply(
    itr_list$file_loc,
    function(x){
      metrics <- read_rds(x)
      metrics$static_metrics %>% 
        dplyr::select("obs_epi_size", "sym_epi_size", "asym_epi_size",
                      "R0", "rate_of_spread", "prop_counties_1_case",
                      "peak_epi_day", "peak_epi_size", "max_exposure_from_last_case",
                      "min_exposure_from_first_case", "avg_exposure_from_peak")
    }
  ) %>%
    bind_rows() %>%
    {bind_cols(itr_list, .)}
  
  rsme_list <- filter(itr_list, eps != baseline_noise)
  
  baseline <- filter(itr_list, eps == baseline_noise) %>% pull(file_loc)
  comp <- rsme_list$file_loc[1]
  
  out <- lapply(
    rsme_list$file_loc, 
    function(x){
      rmse_comp(baseline, x)
    }
  ) %>%
    bind_rows() %>%
    {bind_cols(rsme_list, .)} %>%
    full_join(out, by = c("itr_file", "eps_file", "eps", "itr", "file_loc"))
  
  write_rds(out, paste0(out_file_loc,"//","itr_",itr_num,".rds"))
  return(paste0("Iteration: ", itr_num, " -- Completed"))
}



rmse_comp <- function(baseline, comp){
  x <- read_rds(baseline)
  y <- read_rds(comp)
  
  tibble(
    "rmse_mean_import_rate" = sqrt(mean((unlist(x$Mean_import_rate) - unlist(y$Mean_import_rate))^2)),
    "rmse_risk_of_import" = sqrt(mean((unlist(x$risk_of_import) - unlist(y$risk_of_import))^2, na.rm = T)),
    "rmse_Re" = sqrt(mean((unlist(log(x$Re)) - unlist(log(y$Re)))^2, na.rm = T)), 
    "rmse_prob_of_infec" =   full_join(x$Probabilty_infection, y$Probabilty_infection, by = c("location", "day")) %>%
      drop_na() %>%
      mutate(metric = (Pi_t.x - Pi_t.y)^2) %>%
      pull(metric) %>%
      mean() %>%
      sqrt()
  )
}



