# script for master metrics function along with all metrics 

run_metrics <- function(x){
   
   #epidemic size
   epi_size <- calc_epidemic_size(x)
   
   #reproduction dynamics
   Rep_number<- Rep_numb(x)
   
   #rate of spread
   ros <- rate_of_spread(x)
   
   #output formatted as a tibble
   return(
      list(
         "static_metrics" =       tibble(
            "mob_mat" = x$M_loc, 
            "obs_epi_size" = epi_size$obs, 
            "sym_epi_size" = epi_size$sym, 
            "asym_epi_size" = epi_size$asym,
            "R0" = Rep_number$R0, 
            "rate_of_spread" = ros
         ), 
         "Re" = Rep_number$Re
      )
   )
   
}

#calculate epidemic size
calc_epidemic_size <- function(x){
   obs_epi_size <- x$compartments[x$obsidx,] %>% rowSums() %>% sum()
   sym_epi_size <- x$compartments[x$Isidx,] %>% rowSums() %>% sum()
   asym_epi_size <- x$compartments[x$Iaidx,] %>% rowSums() %>% sum()
   
   return(list(
      "obs" = obs_epi_size, 
      "sym" = sym_epi_size,
      "asym" = asym_epi_size
   ))
}


# Reproduction numbers 

Rep_numb <-function(x){
   R0 = x$alpha * x$beta * x$delta + (1 - x$alpha) * x$mu * x$beta * x$delta
   Re = R0*tail(x$compartments[x$Sidx,], n = 1) / tail( x$pop, n = 1)
   return(list(
      "R0" = R0, 
      "Re" = c(Re)
   ))
   
}

# Rate of spread
rate_of_spread <- function(x){
   y <- x$compartments[x$obsidx,] 
   colnames(y) <- 1:ncol(y)
   
   prop_epi <- as_tibble(y) %>%
      mutate(location = row_number()) %>%
      gather("day", "value", `1`:`61`) %>%
      mutate(day = as.numeric(day)) %>%
      arrange(day) %>%
      group_by(location) %>%
      mutate(cum_value = value + lag(value, default = value[1])) %>%
      ungroup() %>%
      mutate(flag = cum_value >= 1) %>%
      group_by(day) %>%
      summarise(prop_epi = mean(flag))
   
   lm(prop_epi ~ day, data = prop_epi) %>%
      coef() %>%
      {.[2]} %>%
      return()
}

