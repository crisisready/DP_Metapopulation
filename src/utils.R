#### function that converts the long format from Camber data to a matrix ####
convertToMatrix <- function(dat, day) {
  tmp <- dat %>% filter(activity_day == as.Date(day)) %>%
    mutate(from_fips = as.character(from_fips), to_fips = as.character(to_fips)) %>%
    dplyr::select(from_fips, to_fips, transitions) %>%
    arrange(from_fips, to_fips) %>%
    filter(!is.na(from_fips), !is.na(to_fips)) %>%
    group_by(from_fips, to_fips) %>%
    summarize(Count = sum(Count, na.rm = TRUE))

  tmp$transitions[is.na(tmp$transitions)] <- 0

  mat = acast(tmp[, c("from_fips", "to_fips", "transitions")], from_fips~to_fips)
  diag(mat) = 0

  return(mat)
}

#### function to adjust camber data to the transformations required 



#### functions for extracting various objects from simulation run ####
extractObs <- function(x.save, num_loc, num_tsteps, start.date = "2020-03-23", prov_code_sim) {

  obs <- matrix(NA, nrow = num_loc, ncol = num_tsteps)
  for (i in 1:num_loc) {
    obs[i,] <- x.save[obsidx[i],c(1:num_tsteps)]
  }
  obs.df <- as.data.frame(obs)
  names(obs.df) <- seq(from=as.Date(start.date), to = as.Date((as.Date("2020-03-23")+ num_tsteps -1)), by = 1)

  obs.df$Code = prov_code_sim
  return(obs.df)

}


extractPrev <- function(x.save, num_loc, num_tsteps, start.date = "2020-03-23", prov_code_sim) {

  prev <- matrix(NA, nrow = num_loc, ncol = num_tsteps)
  for (i in 1:num_loc) {
    prev[i,] <- x.save[Isidx[i],c(1:num_tsteps)] + x.save[Iaidx[i],c(1:num_tsteps)]
  }
  prev.df <- as.data.frame(prev)
  names(prev.df) <- seq(from=as.Date(start.date), to = as.Date((as.Date("2020-03-23")+ num_tsteps -1)), by = 1)

  prev.df$Code = prov_code_sim
  return(prev.df)

}

extractStartI <- function(x.save, num_loc, num_tsteps, start.date = "2020-03-23", prov_code_sim) {

  startI <- rep(NA, length = num_loc)
  for (i in 1:num_loc) {
    startI[i] <- x.save[Isidx[i],1] + x.save[Iaidx[i],1]
  }
  startI.df <- as.data.frame(startI)

  startI.df$Code = prov_code_sim
  return(startI.df)

}

#utility functions to extract metrics of interest from the simulation results
calcR0 <- function(x, alphaidx, betaidx, muidx, Didx){
  R0 = x[alphaidx]*x[betaidx]*x[Didx] + (1 - x[alphaidx])*x[muidx]*x[betaidx]*x[Didx]
  return(R0)
}
