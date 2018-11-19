library(ggplot2)
library(cowplot)
library(dplyr)
library(viridis)
library(purrr)
library(RColorBrewer)

##### This script produces figure 1 of the manuscript and figure A1 from supplementary materia 1###


### define attack and handling time, carrying capacity and growth rate functions ###

attack_r <- function(a0, br, e, m) {
  a0*(m^br)*(3.3/(m))*exp(e*(3.3/m))
}

handling_r <- function(h0,al, m){
  h0*(m^al)
}

carrying <- function(k0, k, m){
  K <- k0* m ^ k
}

growth_rate <- function(rmax, r, m){
  growthrate <-  rmax * m ^ r
}


### Define numerical integration functions ###

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

## load feeind trial data

species <- read.csv("Data/Feeding_trial_bm.csv")

N_given <- 1:60

bms <- species %>% 
  select(Prey, N_given, body_mass) %>% 
  group_by(Prey) %>% 
  filter(N_given == max(N_given)) %>% 
  unique() %>% ungroup() %>% select(body_mass) %>% unlist() %>% round(2)

x <- seq(0, 60, by=0.1)
m <- unique(species$bm)

N_e <- data.frame()

for(i in m){
  
  n_e <- sapply(x,fun2_r,a0 = 0.01286926, T = 4, br = 1.30000000, e = 0.07172922 , h0 = 19.83745207, al = 2.41854333, P = 1,
                m = i)
  
  N_e <- bind_rows(N_e, data.frame(N_given = x,N_eaten = n_e,bm= i))  
}

#### estimae number of prey eaten given the numerical integration

N_eaten <- N_e %>%
  mutate(size_class = case_when(bm < 0.13 ~"Small",
                                bm > 0.13 & bm < 0.3 ~ "Medium", 
                                bm > 0.3 ~ "Large")) %>% 
  mutate(size_class = factor(size_class, levels = c("Small", "Medium", "Large")))


### plot figure 1

prey_eaten <- N_eaten %>% 
  ggplot(aes(x = N_given, y = N_eaten)) + geom_path(aes(group = bm, colour = as.factor(bm))) +
  geom_point(data = species, aes(colour = as.factor(bm))) + facet_wrap(~size_class, scales = 'free') +
  xlab("Number of prey offered") + ylab("Number of prey consumed") + 
  theme(strip.background =element_rect(fill="white")) +
  scale_colour_manual(name = "Prey body mass",values = c(brewer.pal(4, "BrBG")[-1],
                                 brewer.pal(4, "PiYG")[-1],
                                 brewer.pal(4, "PuOr")[-c(1,2)]))

ggsave(prey_eaten, file = 'Figures/Figure1.jpeg', width = 10)



##### Define parameters given bm

parameters_given_bm <- function(m){
  a <- attack_r(a0 = 0.01286926, br = 1.30000000 ,e = 0.07172922 , m)
  h <- handling_r(h0 = 19.83745207, al = 2.41854333, m)
  a2 <- attack_r(a0 = 0.0005, br = 1.30000000 ,e = 0.07172922  , m)
  h2 <- handling_r(h0 = 5, al = 2.41854333, m)
  K <- carrying(k0 = 6, k = -1, m)
  gr <- growth_rate(rmax = 0.05, r = -0.20, m)
  return(list(a, a2, h, h2, K, gr))
}

bms_all <- seq(0.05, 1, 0.01)

#### Calculate parameters

all_ps <- parameters_given_bm(bms_all) %>%
  map_df(~data.frame(value = .x)) %>% 
  mutate(bms = rep(bms_all, 6)) %>% 
  mutate(parameter = rep(c("Attack rate", "Attack rate model", "Handling time", "Handling time model", 
                           "Carrying capacity", "Growth rate"), each  = 96))

### plot parameters

parameters <- all_ps %>%
  ggplot(aes(x = bms, y = value)) + geom_line() + facet_wrap(~parameter, scales = 'free') +
  ylab("") + xlab("Prey body mass")  + 
  theme(strip.background =element_rect(fill="white"))

ggsave(parameters, file = 'Figures/FigureA1.jpeg', width = 8)
  



