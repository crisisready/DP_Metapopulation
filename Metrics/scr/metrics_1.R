# script for master metrics function along with all metrics 

run_metrics <- function(x){
  
  #epidemic size
  epi_size <- calc_epidemic_size(x)
  Rep_number<- Rep_numb(x)
  #output formatted as a tibble
  return(
    tibble(
      "mob_mat" = x$M_loc, 
      "obs_epi_size" = epi_size$obs, 
      "sym_epi_size" = epi_size$sym, 
      "asym_epi_size" = epi_size$asym,
      "R0" = Rep_number$R0,
      "Re" = c(Rep_number$Re)
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

