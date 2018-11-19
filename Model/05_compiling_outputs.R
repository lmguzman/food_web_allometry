library(dplyr)
library(purrr)
library(moments)

#### this script compiles the results 

survived1 <- readRDS("Results/survived_1_asym.RDS")

prey <- c(1:length(survived1))[!(sapply(survived1, is.null))]

reps <- length(survived1[[1]])

compiled <- data.frame(NULL)
for(i in prey){
  for(j in 1:reps){
    
    compiled <- bind_rows(compiled, data.frame(t(survived1[[i]][[j]]$tried), Fail = survived1[[i]][[j]]$failed, prey_num = i, survived1[[i]][[j]][[4]]))
    
  }
}


survived1_mean <- compiled %>% 
  mutate(X1 = ifelse(is.na(X1), `t.survived1..i....j...tried.`, X1)) %>%
  dplyr::select(-`t.survived1..i....j...tried.`) %>%
  rowwise() %>% 
  mutate(mean_bm = lift_vd(mean, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21), 
         var_bm = lift_vd(var, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21)) 



survived2 <- readRDS("Results/survived_2_asym.RDS")

prey <- c(1:length(survived2))[!(sapply(survived2, is.null))]

reps <- length(survived2[[1]])

compiled <- data.frame(NULL)
for(i in prey){
  for(j in 1:reps){
    
    compiled <- bind_rows(compiled, data.frame(t(survived2[[i]][[j]]$tried), Fail = survived2[[i]][[j]]$failed, prey_num = i+2, survived2[[i]][[j]][[4]]))
    
  }
}


survived2_mean <- compiled %>% 
  rowwise() %>% 
  mutate(mean_bm = lift_vd(mean, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21), 
         var_bm = lift_vd(var, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21)) 


survived_all <- bind_rows(survived1_mean, survived2_mean)

write.csv(survived_all, row.names = FALSE, "Results/survived_correctA_asymetrical.csv")


rm(list = ls())

gc()
###### vertical

vertical1 <- readRDS("Results/vertical_1_asym.RDS")

prey <- c(1:length(vertical1))[!(sapply(vertical1, is.null))]

reps <- length(vertical1[[1]])

compiled <- data.frame(NULL)
for(i in prey){
  for(j in 1:reps){
    
    compiled <- bind_rows(compiled, data.frame(t(vertical1[[i]][[j]]$tried), Fail = vertical1[[i]][[j]]$failed, prey_num = i, vertical1[[i]][[j]][[4]]))
    
  }
}


vertical1_mean <- compiled %>% 
  mutate(X1 = ifelse(is.na(X1), `t.vertical1..i....j...tried.`, X1)) %>%
  dplyr::select(-`t.vertical1..i....j...tried.`) %>%
  rowwise() %>% 
  mutate(mean_bm = lift_vd(mean, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21), 
         var_bm = lift_vd(var, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21)) 




vertical2 <- readRDS("Results/vertical_2_asym.RDS")

prey <- c(1:length(vertical2))[!(sapply(vertical2, is.null))]

reps <- length(vertical2[[1]])

compiled <- data.frame(NULL)
for(i in prey){
  for(j in 1:reps){
    
    compiled <- bind_rows(compiled, data.frame(t(vertical2[[i]][[j]]$tried), Fail = vertical2[[i]][[j]]$failed, prey_num = i+2, vertical2[[i]][[j]][[4]]))
    
  }
}


vertical2_mean <- compiled %>% 
  rowwise() %>% 
  mutate(mean_bm = lift_vd(mean, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21), 
         var_bm = lift_vd(var, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21)) 


vertical_all <- bind_rows(vertical1_mean, vertical2_mean)

write.csv(vertical_all, row.names = FALSE, "Results/vertical_correctA_asymetrical.csv")

