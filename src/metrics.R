# script for master metrics function along with all metrics 

run_metrics <- function(x){
   
   #epidemic size
   epi_size <- calc_epidemic_size(x)
   
   #reproduction dynamics
   Rep_number<- Rep_numb(x)
   
   #rate of spread
   ros <- rate_of_spread(x)
   
   #prop counties with at least 1 case
   prop_c_1_case <- prop_counties_1_case(x)
   
   #peak epidemic day and size
   peak_epi <- peak_epi_day_size(x)
   
   #exposure window
   exposure <- exposure_window(x)
   
   #importation rate
   import <- import_rate(x)
   
   #prediction 
   prediction <- prediction(x)
   
   #output formatted as a tibble
   return(
      list(
         "static_metrics" =       tibble(
            "mob_mat" = x$M_loc, 
            "obs_epi_size" = epi_size$obs, 
            "sym_epi_size" = epi_size$sym, 
            "asym_epi_size" = epi_size$asym,
            "R0" = Rep_number$R0, 
            "rate_of_spread" = ros, 
            "prop_counties_1_case" = prop_c_1_case, 
            "peak_epi_day" = peak_epi$peak_epi_day, 
            "peak_epi_size" = peak_epi$peak_epi_size,
            "max_exposure_from_last_case" = exposure$max_exposure_from_last_case,
            "min_exposure_from_first_case" = exposure$min_exposure_from_first_case,
            "avg_exposure_from_peak" = exposure$avg_exposure_from_peak
         ), 
         "Re" = Rep_number$Re,
         "Mean_import_rate" = import,
         "Probabilty_infection" = prediction$prob_infection, 
         "Predictability" = prediction$pred, 
         "Synchrony" = prediction$spatial_synch
         
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
      gather("day", "value", `1`:`91`) %>%
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

#proportion of counties with at least 1 case
prop_counties_1_case <- function(x){
   ((x$compartments[x$obsidx,] %>%
        rowSums()) > 0) %>% mean() %>%
      return()
}

#average time of peak of the epidemic
peak_epi_day_size <- function(x){
   y <- x$compartments[x$obsidx,]
   colnames(y) <- 1:91
   
   a <- y %>%
      as_tibble() %>%
      mutate(location = row_number()) %>%
      gather("day", "value", `1`:`91`) %>%
      group_by(location) %>%
      mutate(flag = (value == max(value) & value != 0)) %>%
      filter(flag) %>%
      ungroup() %>%
      mutate(day = as.integer(day))
   
   peak_day <- a %>%
      summarise(avg_peak_day = mean(day)) %>%
      pull(avg_peak_day)
   
   peak_size <- a %>%
      summarise(avg_peak_size = mean(value)) %>%
      pull(avg_peak_size)
   
   return(list(
      "peak_epi_day" = peak_day, 
      "peak_epi_size" = peak_size
   ))
}

# mean exposure window of the epidemic

exposure_window <- function(x){
   y <- x$compartments[x$obsidx,]
   colnames(y) <- 1:91
   
   a <- y %>%
      as_tibble() %>%
      mutate(location = row_number()) %>%
      gather("day", "value", `1`:`91`) %>%
      group_by(location) %>%
      filter(value > 0) %>%
      summarise(min_day = min(day), max_day = max(day), peak_day = tail(day[which.max(value)])) %>%
      mutate(min_day = as.integer(min_day), max_day = as.integer(max_day), peak_day = as.integer(peak_day), min_incubation = as.integer(min_day - 2), max_incubation = as.integer(max_day - 14), avg_incubation = as.integer(peak_day - 5), flag = (min_day != max_day))  %>%
      filter(flag)
   
   min_exposure_from_first_case <- a %>%
      summarise(mean(min_incubation)) %>%
      pull()
   
   max_exposure_from_last_case <- a %>%
      summarise(mean(max_incubation)) %>%
      pull()
   
   avg_exposure_from_peak <- a %>%
      summarise(mean(avg_incubation)) %>%
      pull()
   
   return(list(
      "min_exposure_from_first_case" = min_exposure_from_first_case,
      "max_exposure_from_last_case" = max_exposure_from_last_case,
      "avg_exposure_from_peak" = avg_exposure_from_peak
   ))
   
}

# Importation rate for each pair of county and for each time step
import_rate<-function(x){
   y <- lapply(x$M_trans, function(x) x/ifelse(rowSums(x)==0,1,rowSums(x)))
   prop_inf <- x$compartments[x$obsidx,] /x$pop 
   import_rate <- lapply(1:60, function(x) y[[x]] * prop_inf[,x+1] * 1000000)
   total_import_rate<-sapply(import_rate, function(x)rowSums(x)) 
   total_import_rate%>%
      tibble()%>%
      summarise(mean_imp = apply(total_import_rate, 1, function (x) mean(x)))%>%
      return()
}

#Probability of infection; predictability of an outbreak; spatial synchrony 

prediction<-function(x){
   inf1 <- x$compartments[x$Isidx,]
   colnames(inf1) <- 1:ncol(inf1)
   
   infected1 <-inf1%>%
      as_tibble()%>%
      mutate(location = row_number())%>%
      gather("day", "value1", `1`:`91`)%>%
      mutate(day = as.numeric(day))%>%
      arrange(day)
   
   inf2<-x$compartments[x$Iaidx,]
   colnames(inf2) <- 1:ncol(inf2)
   infected2 <-inf2%>%
      as_tibble()%>% 
      mutate(location = row_number())%>%
      gather("day", "value2", `1`:`91`) %>%
      mutate(day = as.numeric(day)) %>%
      arrange(day)
   
   inf3<-x$compartments[x$obsidx,]
   colnames(inf3) <- 1:ncol(inf3)
   infected3 <-inf3%>%
      as_tibble()%>%
      mutate(location = row_number())%>%
      gather("day", "value3", `1`:`91`) %>%
      mutate(day = as.numeric(day)) %>%
      arrange(day)
   
   infected<-  left_join(infected1,infected2)
   infectedR<- left_join(infected, infected3)
   
   Infected<-infectedR%>%
      mutate(value = value1 + value2 + value3)%>%
      mutate(prevalence = value/ max(rowSums(x$pop)))%>%
      group_by(day)%>%
      mutate(sumV=sum(value))%>%
      ungroup()%>%
      group_by(location, day)%>%
      mutate(Pi_t = value/ifelse(sumV == 0, 1, sumV))
   
   prob_infection<- Infected %>%
      filter(Pi_t>0)%>%
      select(location, day, Pi_t) %>%
      filter(day == max(day))
     
      
   # the variable Pi_t represents the probability that a given infected is at the location   i at time t
   #%>%ggplot(aes(x = day, y = Pi_t))+  geom_line()+  facet_wrap(~location)
   
   similarity<-Infected%>%
      group_by(location)%>%
      filter(Pi_t>0)%>%
      mutate(Hellinger = sum(sqrt(prod(max(Pi_t)))))
   
   pred <-similarity%>%
      group_by(location, day)%>%
      summarise(overlap = Hellinger * sum(sqrt(prod(max(prevalence)))))%>%
      filter(overlap == max(overlap)) %>%
      ungroup()%>%
      group_by(location)%>%
      filter(day == max(day))%>%
      select(location, day)
   
   spatial_synch<-Infected %>%
      rename(Is = value1, Ia = value2, obs = value3, Total_inf = value)%>%
      pivot_wider(id_cols = day, names_from = location, values_from = Total_inf) %>%
      cor()
      
     
   
      
      
   return(list(
      "Probabilty_infection" = prob_infection, 
      "Predictability" = pred, 
      "Synchrony" = spatial_synch
   ))
   
}

