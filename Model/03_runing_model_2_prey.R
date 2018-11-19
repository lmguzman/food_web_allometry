source('Model/02_model_functions.R')
library(dplyr)
library(gtools)
library(parallel)
library(purrr)

#### This script runs all parameter combinations for two prey 

## needs 5 cores

bm_all <- seq(0.05, 1, 0.01)

bm_combs <- combinations(length(bm_all), 2, bm_all, repeats.allowed = TRUE)

run_stoch_2_10 <- function(bm_combs_vec){
  final_abundances <- data.frame(NULL)
  
  for(j in 1:10){
    model_output <- run_stochastic_model(bm_combs_vec, 500, 0)
    final_abundances <- bind_rows(final_abundances, data.frame(model_output[501,-1], t(bm_combs_vec), rep = j)) 
  }
  return(final_abundances)
}


final_abun_list <- mclapply(1:nrow(bm_combs), FUN = function(x){run_stoch_2_10(bm_combs[x,])}, mc.cores = 5)

final_abundances <- final_abun_list %>%
  map_df(~ data.frame(.x))

write.csv(final_abundances, "Results/two_prey_outcomes.csv")


