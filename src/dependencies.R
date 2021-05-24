#' A function to simplify dependencies
#'
#' This function allows you to call a package and install if not present
#' @param package Package name which must be in a string
#' @keywords library install
#' @export
#' @examples
#' lib_eval()

#evaluate installed packages and install if needed before execution
lib_eval <- function(package){
  
  tryCatch(  
    {a <- as.data.frame(as.character(package))},
    error=function(cond) {
      message("Package must be a character, ex: 'RCurl' instead of RCurl")
      message("Here's the original error message:")
      stop(cond)
      # Choose a return value in case of error
      return(NA)
    }
    
    
    
  )
  
  if(as.character(a[1,1]) %in% installed.packages()[,"Package"]) {
    
    message(paste(package, " found in system, initializing..."))
    library(package, character.only = T)
    message("Done!")
    
  }else{
    
    message(paste(package, " not found in system, installing and initializing..."))
    install.packages(as.character(package), repos='http://cran.us.r-project.org')
    library(package, character.only = T)
    message(paste(package, " installed and initialized!"))
    
  }
  
}

#Dependencies for R 
sapply(c("tidyverse","reshape2", "here", "bigrquery","igraph"), lib_eval)

#load local packages
source(here("src", "NY_model.R"))
source(here("src", "metrics.R"))
source(here("src", "utils.R"))
