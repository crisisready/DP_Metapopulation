# load function
library(here)
library(tidyverse)

source(here('src', 'spatial_seir.R'))

run_seir_model <- function(
  census_pop = read_rds("../resources/nyc_census_data.rds"), 
  M_loc = "../resources/test_matrices.rds",
  seedid = c(24,41),
  num_tsteps = 60,
  beta = 0.8, #0.52 # transmission rate due to symptomatic individuals
  mu = 0.5, # the multiplicative factor reducing the transmission rate of asymptomatic individuals
  theta = 1, # mult. factor, which is >1 to reflect underreporting of human movement
  zeta = 3.6, # average latency period
  alpha = 0.65, # fraction of documented (or symptomatic) infections
  delta = 3.14 # average duration of infection
){
  #initialize
  num_loc = nrow(census_pop) # number of locations
  
  #transition matrix 
  M <- read_rds(M_loc)
  
  # indices for each compartment + fixed parameters
  Sidx = seq(from =1, to = 5*num_loc, by = 5)
  Eidx = seq(from =2, to = 5*num_loc, by = 5)
  Isidx = seq(from =3, to = 5*num_loc, by = 5)
  Iaidx = seq(from =4, to = 5*num_loc, by = 5)
  obsidx = seq(from =5, to = 5*num_loc, by = 5)
  betaidx = 5*num_loc + 1
  muidx = 5*num_loc + 2
  thetaidx = 5*num_loc + 3
  Zidx = 5*num_loc + 4
  alphaidx = 5*num_loc + 5
  Didx = 5*num_loc + 6
  
  
  
  #parameters that the function will need
  pop0 = census_pop$acs_pop
  pop = census_pop$acs_pop
  
  x = rep(NA, length =5*num_loc + 6) #vector of initial conditions
  ts = 1 # the current time step for starting the integration
  
  seedid  = c(24, 41) #index of locations to seed
  
  x[Eidx] = rep(0, num_loc)
  x[((seedid - 1)*5)+2] = 20 # seed 20 infected in latent compartment, in metapopulations defined by seedid
  x[Sidx] = pop -  x[Eidx]   # everyone except 1 susceptible in the beginning
  x[Isidx] <- rep(0, num_loc)
  x[Iaidx] = rep(0,num_loc)
  x[obsidx] = rep(0,num_loc)
  x[betaidx] = beta   #0.52 # transmission rate due to symptomatic individuals
  x[muidx] = mu    # the multiplicative factor reducing the transmission rate of asymptomatic individuals
  x[thetaidx] = theta    # mult. factor, which is >1 to reflect underreporting of human movement
  x[Zidx] = zeta      # average latency period
  x[alphaidx] = alpha # fraction of documented (or symptomatic) infections
  x[Didx] = delta     # average duration of infection
  
  num_tsteps = 60
  x.save <- matrix(NA, nrow = length(x), ncol = num_tsteps+1)
  pop.save <- matrix(NA, nrow = length(pop), ncol = num_tsteps+1)
  
  x.save[,1] <- x
  pop.save[,1] <- pop0
  
  for (ts in 1:num_tsteps) {
    sim.tmp <- spatial_seir(x, M, pop, ts, pop0)
    # update values
    x  = x.save[,ts+1] = sim.tmp$x
    pop = pop.save[,ts+1] = sim.tmp$pop
    
    
  }
  
  return(list("compartments" = x.save, 
              "pop" = pop.save, 
              "num_tsteps"= num_tsteps, 
              "beta" = beta, 
              "mu" = mu, 
              "theta" = theta, 
              "zeta" = zeta, 
              "alpha" = alpha, 
              "delta" = delta, 
              "Sidx" = seq(from =1, to = 5*num_loc, by = 5),
              "Eidx" = seq(from =2, to = 5*num_loc, by = 5),
              "Isidx" = seq(from =3, to = 5*num_loc, by = 5),
              "Iaidx" = seq(from =4, to = 5*num_loc, by = 5),
              "obsidx" = seq(from =5, to = 5*num_loc, by = 5),
              "betaidx" = 5*num_loc + 1,
              "muidx" = 5*num_loc + 2,
              "thetaidx" = 5*num_loc + 3,
              "Zidx" = 5*num_loc + 4,
              "alphaidx" = 5*num_loc + 5,
              "Didx" = 5*num_loc + 6, 
              "M_loc" = M_loc))
  
}

