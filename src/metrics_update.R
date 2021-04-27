# Reproduction numbers

source(here("src", "NY_model.R"))

x <- run_seir_model()

# R0
R0 <-function(delta = 3.14, alpha = 0.65, mu = 0.5, beta = 0.8){
  R0 = alpha * beta * delta + (1 - alpha) * mu * beta * delta 
  return(R0)
}


R0_ts <- c()
num_tsteps = 60
beta = x[[1]][seq(311, length (x[[1]]), by=316)]
mu = x[[1]][seq(312, length (x[[1]]), by=316)]
theta = x[[1]][seq(313, length (x[[1]]), by=316)]
zeta = x[[1]][seq(314, length (x[[1]]), by=316)]
alpha = x[[1]][seq(315, length (x[[1]]), by=316)]
delta = x[[1]][seq(316, length (x[[1]]), by=316)]

for (i in 1:num_tsteps){
  R0 = function(...){
    R0 = alpha * beta * delta + (1 - alpha) * mu * beta * delta 
    return(R0)
  }
  R0_ts = R0(alpha, beta, delta, mu) 
  return(R0)
}

R0_ts

