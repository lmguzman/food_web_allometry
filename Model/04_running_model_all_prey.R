source('Model/02_model_functions.R')
library(purrr)
library(dplyr)
library(parallel)

#### This script runs the model for all prey
## needs 10 cores


########## For all species to survive###########

#### symmetric competition

params <- list(min.value = 0.02, max.value = 1, step.size = 0.3, start.value = 0.1)

prey <- 1:21

testFunction <- function(prey, param_general, type, Be) {
  return(tryCatch(get_final_output(prey, param_general, type, Be), error=function(e) NULL))
}

survived_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 1, Be = 0, mc.cores = 10, mc.preschedule = FALSE)

saveRDS(survived_output, "Results/survived_1_sym.RDS")


## survived 2

params <- list(min.value = 0.02, max.value = 1, step.size = 0.5, start.value = 0.1)

prey <- 3:21

survived_more_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 1, Be = 0, mc.cores = 10, mc.preschedule = FALSE)

saveRDS(survived_more_output, "Results/survived_2_sym.RDS")


##### Assymetrical


params <- list(min.value = 0.02, max.value = 1, step.size = 0.3, start.value = 0.1)

prey <- 1:21

testFunction <- function(prey, param_general, type, Be) {
  return(tryCatch(get_final_output(prey, param_general, type, Be), error=function(e) NULL))
}

survived_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 1, Be = 0.5, mc.cores = 10, mc.preschedule = FALSE)

saveRDS(survived_output, "Results/survived_1_asym.RDS")


## survived 2

params <- list(min.value = 0.02, max.value = 1, step.size = 0.5, start.value = 0.1)

prey <- 3:21

survived_more_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 1, Be = 0.5, mc.cores = 10, mc.preschedule = FALSE)

saveRDS(survived_more_output, "Results/survived_2_asym.RDS")



########## For the predator and at least one prey to survive###########

#### symmetric competition



params <- list(min.value = 0.02, max.value = 1, step.size = 0.3, start.value = 0.1)

prey <- 1:21

testFunction <- function(prey, param_general, type, Be) {
  return(tryCatch(get_final_output(prey, param_general, type, Be), error=function(e) NULL))
}

vertical_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 4, Be = 0,  mc.cores = 10, mc.preschedule = FALSE)

saveRDS(vertical_output, "Results/vertical_1_sym.RDS")


## vertical 2

params <- list(min.value = 0.02, max.value = 1, step.size = 0.5, start.value = 0.1)

prey <-3:21

vertical_more_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 4, Be = 0, mc.cores = 10, mc.preschedule = FALSE)

saveRDS(vertical_more_output, "Results/vertical_2_sym.RDS")


##### Assymetrical

params <- list(min.value = 0.02, max.value = 1, step.size = 0.3, start.value = 0.1)

prey <- 1:21

vertical_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 4, Be = 0.5,  mc.cores = 10, mc.preschedule = FALSE)

saveRDS(vertical_output, "Results/vertical_1_asym.RDS")


## vertical 2

params <- list(min.value = 0.02, max.value = 1, step.size = 0.5, start.value = 0.1)

prey <-3:21

vertical_more_output <- mclapply(prey, FUN = testFunction, param_general = params, type = 4, Be = 0.5, mc.cores = 10, mc.preschedule = FALSE)

saveRDS(vertical_more_output, "Results/vertical_2_asym.RDS")

