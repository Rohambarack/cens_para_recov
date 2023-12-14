#######get estimates#########
library(brms)
library(tidyverse)

###### extract function #######

get_estimates <- function(list_data_model){
  
  #simplify names: m for model d for data
  d <- list_data_model[[1]]
  m <- list_data_model[[2]]
  
  #extract set simulation values from data
  #only takes seed, mu, mu_sd, b, b_sd and sigma
  sim <- d[1,c(1,3:6,9)]
  
  sim <- sim %>% 
    ungroup() %>% 
    rename_all(~paste("sim",sep = "_",names(sim)))
  
  #extract model estimates
  
  #help with group level estimates
  m_help <- as_draws(m)
  
  mod <- tibble(pop_mu = fixef(m)[1],
                pop_mu_sd = mean(c(m_help[[1]]$sd_Pair__Intercept,
                                   m_help[[2]]$sd_Pair__Intercept,
                                   m_help[[3]]$sd_Pair__Intercept,
                                   m_help[[4]]$sd_Pair__Intercept)),
                pop_beta = fixef(m)[3],
                pop_beta_sd = mean(c(m_help[[1]]$sd_Pair__beta_Intercept,
                                     m_help[[2]]$sd_Pair__beta_Intercept,
                                     m_help[[3]]$sd_Pair__beta_Intercept,
                                     m_help[[4]]$sd_Pair__beta_Intercept)),
                sigma = fixef(m)[2]
  )
  
  mod <- mod %>% 
    rename_all(~paste("mod",sep = "_",names(mod)))
  
  #put the two togetther into 1 row
  
  fin <- cbind(sim,mod)
  
  return(fin)
  
  
}
####### loop extract function  for lists #####
get_list_estimates <- function(lfin){
  for (i in 1:length(lfin)){
    
    #first run
    
    if (i == 1){
      
      f_df <- get_estimates(lfin[[i]])
    } else {
      t_df <- get_estimates(lfin[[i]])
      f_df <- rbind(f_df,t_df)
    }
    
  }
  
  return(f_df)
}

#### function to append multiple long lists and return estimates ###
extract_estimates <- function(directory){
  #file names
  fl <- list.files(path = directory)
  
  #loopit, extract them
  for (i in fl){
    
    #define exact file
    path <- paste(directory,sep = "",i)
    ll1 <- read_rds(path)
    
    if (i == fl[1]) {
      
      f_df <- get_list_estimates(ll1)
    } else {
      
      t_df <- get_list_estimates(ll1)
      f_df <- rbind(f_df,t_df)
    }  
    
    
  }
  
  return(f_df)
}

#test
f_df <- extract_estimates("/work/addrun/mods/")


