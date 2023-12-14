#install.packages("brms")

library(brms)
library(tidyverse)

#### simulate function #####
sim_func <- function(n,
                     s,
                     pop_mu,
                     pop_mu_sd,
                     pop_beta,
                     pop_beta_sd,
                     sigma,
                     seed){
  
  
  #n pairs
  
  DF_sim <- tibble(
    Pair = seq(1:n),
  )
  
  
  #add  population + ind exgaussan parameters
  DF_sim <- DF_sim %>%
    rowwise() %>% 
    mutate(
      seed = seed,
      pop_mu = pop_mu,
      pop_mu_sd = pop_mu_sd,
      pop_beta = pop_beta,
      pop_beta_sd = pop_beta_sd,
      ind_mu = rnorm(1,pop_mu,pop_mu_sd),
      ind_beta = rnorm(1,pop_beta,pop_beta_sd),
      sigma = sigma
      
    )
  
  
  #extend to  turns and add latencies
  DF_sim <- DF_sim %>% expand(nesting(seed,
                                      Pair,
                                      pop_mu,
                                      pop_mu_sd,
                                      pop_beta,
                                      pop_beta_sd,
                                      ind_mu,
                                      ind_beta,
                                      sigma), Turn = 1:s)
  
  DF_sim <- DF_sim %>%
    rowwise() %>% 
    mutate( ind_lat = rexgaussian(1, ind_mu, sigma, exp(ind_beta))
    )
  
  #make censoring
  
  DF_sim <- DF_sim %>% 
    mutate( ind_lat_2 = ifelse(ind_lat > 0,ind_lat, 0),
            cen1 = if_else(ind_lat < 0, "left", "none"))
  
  return(DF_sim)
  
}
############ define vectors ##########
mean_vector <- c(400,500)
mean_sd_vector <- c(50,100,200)
beta_vector <- c(7,7.2,7.4,7.6)
beta_sd_vector <- c(.1,.2)
seed_vector <- c(1)

#### for loops ####

for (m in mean_vector) {
  for(m_sd in mean_sd_vector){
    for(b in beta_vector){
      for(b_sd in beta_sd_vector){
        
        for(seed in seed_vector){
          
          
          ###set seed#####
          set.seed(seed)
          ### clock ###
          
          if (m == mean_vector[1] && 
              m_sd == mean_sd_vector[1] &&
              b == beta_vector[1] &&
              b_sd == beta_sd_vector[1] &&
              seed == seed_vector[1]){
            
            t1 <- Sys.time()
            
          } else if (m == mean_vector[2] && 
                     m_sd == mean_sd_vector[3] &&
                     b == beta_vector[4] &&
                     b_sd == beta_sd_vector[2] &&
                     seed == seed_vector[1]){
            t2<- Sys.time()
            
            
          }
          
            
            #### simulate data ####
            DF_sim <- sim_func(35,200,m,m_sd,b,b_sd,100,seed)
            #### model it ####
            
            ## formula ##
            formula_b_s <- bf(ind_lat_2 | cens(cen1) ~ (1|Pair) ,
                              sigma ~ 1,
                              beta ~ (1|Pair))
            ## stanvars ##
            
            stanvars <- stanvar(b, name = "b") + stanvar(b_sd, name = "b_sd")
            
            ## priors ##
            prior_b_s <- c(
              
              prior(normal(0, 1000), class = Intercept), #mu
              prior(normal(0, 100), class = sd), #mu_sd
              prior(normal(b,.1), class = Intercept, dpar = beta),# beta
              prior(normal(b_sd,.1), class = sd, dpar = beta), #beta_sd
              prior(normal(4.5,.1), class = Intercept, dpar = sigma)# sigma
            )
            
            ## fit ##
            mod_sim <- brm(
              formula = formula_b_s,
              data= DF_sim,
              family = exgaussian(),
              prior = prior_b_s,
              stanvars = stanvars,
              sample_prior = T,
              #backend = "cmdstanr",
              chains = 4,
              init = 0,
              cores = 64,
              iter = 1500,
              warmup = 500
              
            )
            
            
          
          ### save both data and model ###
          
          ## first and second run specifies a list of lists, the rest "appends"
          if (m == mean_vector[1] && 
              m_sd == mean_sd_vector[1] &&
              b == beta_vector[1] &&
              b_sd == beta_sd_vector[1] &&
              seed == seed_vector[1]){
            
            l1 <- list(DF_sim, mod_sim)
            
          } else if (m == mean_vector[1] && 
                     m_sd == mean_sd_vector[1] &&
                     b == beta_vector[1] &&
                     b_sd == beta_sd_vector[2] &&
                     seed == seed_vector[1]){
            
            l2 <- list(DF_sim, mod_sim)
            
            lfin <- list(l1,l2)
          } else {
            
            l <- list(DF_sim, mod_sim)
            lfin[[length(lfin)+1]] <- l
            
          }
          
          
          
          
          
        }
        
      }
    }
  }
}

#saveRDS(lfin,"s1_inc48.rds")
