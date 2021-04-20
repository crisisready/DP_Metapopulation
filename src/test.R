# load function
library(here)

source(here('src', 'spatial_seir.R'))

#initialize
num_loc = 500 # number of locations

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
pop0 = rep(100000, num_loc)
pop = rep(100000, num_loc)
M = list()

m <- matrix(NA,nrow = num_loc, ncol = num_loc)
for (r in 1:nrow(m)){
  for (c in 1:ncol(m)) {
    m[r,c] <- round(runif(1,1,10))
  }
}
diag(m) <- 0

for (n in 1:num_loc) {
  M[[n]] <- m
}

# mobility matrix must have zero along diagonals

#M[[1]] = matrix(c(0,0.005,0.005, 0.007,0,0.003,0.015,0.005,0), nrow = 3, byrow=TRUE)*pop #diagonals have to be zero
# M_ij = traveling from j to i (i = row, j  = column)
x = rep(NA, length =5*num_loc + 6) #vector of initial conditions
ts = 1 # the current time step for starting the integration

seedid  = c(1, 15, 80) #index of locations to seed

x[Eidx] = rep(0, num_loc)
x[((seedid - 1)*5)+2] = 20 # seed 20 infected in latent compartment, in metapopulations defined by seedid
x[Sidx] = pop -  x[Eidx]   # everyone except 1 susceptible in the beginning
x[Isidx] <- rep(0, num_loc)
x[Iaidx] = rep(0,num_loc)
x[obsidx] = rep(0,num_loc)
x[betaidx] = 0.8   #0.52 # transmission rate due to symptomatic individuals
x[muidx] = 0.50    # the multiplicative factor reducing the transmission rate of asymptomatic individuals
x[thetaidx] = 1    # mult. factor, which is >1 to reflect underreporting of human movement
x[Zidx] = 3.6      # average latency period
x[alphaidx] = 0.65 # fraction of documented (or symptomatic) infections
x[Didx] = 3.14     # average duration of infection

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

# Incidence/ obs
plot(x.save[5,c(1:num_tsteps)])

Re = x[alphaidx]*x[betaidx]*x[Didx] + (1 - x[alphaidx])*x[muidx]*x[betaidx]*x[Didx]
Re
