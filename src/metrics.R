# Reproduction numbers


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
