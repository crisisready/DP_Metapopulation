# Reproduction numbers


# R0
R0 <-function(parms){
     R0 = unlist(parms[1])*unlist(parms[2])*unlist(parms[3]) + (1 - unlist(parms[1]))*unlist(parms[4])*unlist(parms[2])*unlist(parms[3])
     cat("R0 = ", R0, "\n")
}

# Re
Re<- function(R0, x, N){
     x = tail(x,n=1) / N 
     Re = R0*x
     cat("Re = ", Re, "\n")
   }