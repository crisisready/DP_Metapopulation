# script for master metrics function along with all metrics 

run_metrics <- function(x){
   
   #epidemic size
   epi_size <- calc_epidemic_size(x)
   
   #output formatted as a tibble
   return(
      tibble(
         "mob_mat" = x$M_loc, 
         "obs_epi_size" = epi_size$obs, 
         "sym_epi_size" = epi_size$sym, 
         "asym_epi_size" = epi_size$asym
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


# R0
R0 <-function(parms){
     R0 = unlist(parms[1])*unlist(parms[2])*unlist(parms[3]) + (1 - unlist(parms[1]))*unlist(parms[4])*unlist(parms[2])*unlist(parms[3])
     cat("R0 = ", R0, "\n")
}
#or
#R0 <- x[alphaidx]*x[betaidx]*x[Didx] + (1 - x[alphaidx])*x[muidx]*x[betaidx]*x[Didx]
# Re
Re<- function(R0, x, N){
     x = tail(x,n=1) / N 
     Re = R0*x
     cat("Re = ", Re, "\n")
   }
#or 
#Re <- R0* tail(x[Sidx], n = 1) /  tail (pop, n = 1)
