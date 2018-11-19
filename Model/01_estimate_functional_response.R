library(dplyr)

######## This script fits a type 2 functional response to the feeding trial data #####

### First it fits a power function following Yodzis and Innes and then an allometric scaling function 
### following Rall et al. 


### load feeding trial data

species <- read.csv("Data/Feeding_trial_bm.csv")


#Type two functional response

type_two <- function(param) {
  N <- seq(0,25,1)
  Ne <- (param[1] * N)/(1+ param[1] * param[2] * N)
  return(Ne)
}


### Functions required for maximum likelihood to estimate from the raw data 

##power####
attack_p <- function(a0, m) {
  a0*m^-1
}

handling_p <- function(h0, m){
  h0*m
}

gradfun_p <- function(t,y,parms) {
  with(as.list(c(y,parms)),
       { A  <- attack_p(a0, m)
         H <- handling_p(h0, m)
         grad <- -A*N/(1+A*H*N)
         list(grad,NULL)
       })
}


fun2 <- function(a0,h0,T,m, P,N0) {
  library(deSolve)
  L <- lsoda(c(N=N0),
             times=c(0,T),
             func=gradfun_p,
             parms=c(a0 = a0, h0 = h0, P=P, m = m))
  N.final <- L[2,2]
  N0-N.final
}


NLL.oneT = function(a0, h0, T = 4, P = 1) {
  a0 <- a0
  h0 <- h0
  m <- Mass
  
  prop.exp <- numeric(length=length(Initial))
  
  prop.exp <- mapply(fun2, Initial, m = m, a0=a0, h0=h0, P=P, T=T)/Initial
  - sum(dbinom(Killed, prob = prop.exp, size = Initial,
               log = TRUE))
}



OneTreatmentModel <- function(offered, eaten, bm, starts){
  
  library(bbmle)

  dd.ml = list(Initial=offered,
               Killed=eaten,
               Mass = bm)

  #Using maximum likelyhood and NLL.oneT
  full.mod <- mle2(NLL.oneT, start=starts, data = dd.ml,
                   method="L-BFGS-B",
                   lower=c(a0 = 1e-10, h0 = 1e-10),
                   upper=c(a0 = 3, h0 = 8))
  
  return(list(summary(full.mod), full.mod))
}

model_output <- OneTreatmentModel(offered = species$N_given, eaten = species$N_eaten, bm = species$body_mass, starts = list(a0 = 0.2, h0 = 0.05))


##### RALL ####


attack_r <- function(a0, br, e, m) {
  a0*(m^br)*(3.3/(m))*exp(e*(3.3/m))
}

handling_r <- function(h0,al, m){
  h0*(m^al)
}

gradfun_r <- function(t,y,parms) {
  with(as.list(c(y,parms)),
       { A  <- attack_r(a0, br, e, m)
       H <- handling_r(h0,al, m)
       grad <- -A*N/(1+A*H*N)
       list(grad,NULL)
       })
}


fun2_r <- function(a0, br, e, h0, al,T,m, P,N0) {
  library(deSolve)
  L <- lsoda(c(N=N0),
             times=c(0,T),
             func=gradfun_r,
             parms=c(a0 = a0, br = br, e = e, al = al, h0 = h0, P=P, m = m))
  N.final <- L[2,2]
  N0-N.final
}


NLL.oneT = function(a0, br, e, al, h0, T = 4, P = 1) {
  a0 <- a0
  h0 <- h0
  br <- br
  e <- e
  al <- al
  m <- Mass
  
  prop.exp <- numeric(length=length(Initial))
  
  prop.exp <- mapply(fun2_r, Initial, m = m, a0=a0, br = br, e = e, al = al, h0=h0, P=P, T=T)/Initial
  - sum(dbinom(Killed, prob = prop.exp, size = Initial,
               log = TRUE))
}

OneTreatmentModel <- function(offered, eaten, bm, starts){
  
  library(bbmle)
  
  dd.ml = list(Initial=offered,
               Killed=eaten,
               Mass = bm)
  
  #Using maximum likelyhood and NLL.oneT
  full.mod <- mle2(NLL.oneT, start=starts, data = dd.ml,
                   method="L-BFGS-B",
                   lower=c(a0 = 1e-10, h0 = 1e-10, br = 1e-10, e = 1e-10, al = 1e-10),
                   upper=c(a0 = 1, h0 = 50, br = 1.3, e = 0.6, al = 10))
  
  return(list(summary(full.mod), full.mod))
}


### best:
model_output_rall_correct <- OneTreatmentModel(offered = species$N_given, 
                                               eaten = species$N_eaten, bm = species$body_mass, starts = list(a0 = 0.01, h0 = 19,
                                                                                                                  br = 0.3, e = 0.04, al = 2.5))


### Comparing models ####

BIC(model_output[[2]], model_output_rall_correct[[2]])

model_coefficients_rall <- attributes(model_output_rall_correct[[1]])$coef

write.csv(model_coefficients_rall, 'Results/model_coefficients_rall.csv')
