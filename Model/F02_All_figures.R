source('Model/02_model_functions.R')
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(cowplot)
library(purrr)

vertical <- read.csv("Results/vertical_correctA_symetrical.csv")

survived <- read.csv("Results/survived_correctA_symetrical.csv")


##### Fig A2 #####

vertical_cover <- vertical %>%
  filter(prey_num > 2) %>% 
  ggplot(aes(x = X1, y = X3)) + geom_point(colour = 'darkgrey') +
  facet_wrap(~prey_num) + ylab("Body Mass 2") + xlab("Body Mass 1") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        panel.border = element_rect(colour = "black")) +scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))


ggsave(vertical_cover, file = "Figures/FigureA2.jpeg", width = 14)


##### FIGURE 4######

min_max_vertical <- vertical %>% 
  rowwise() %>% 
  mutate(min_bm = lift_vd(min, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21),
         max_bm = lift_vd(max, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21))  


bm_min_max_vertical <- min_max_vertical %>% 
  select(starts_with("X"))

min_max_vertical$number_below <- apply(bm_min_max_vertical, 1, function(x){sum(x < 0.09, na.rm = TRUE)})

min_max_vertical$prey_num <- apply(bm_min_max_vertical, 1, function(x){sum(x != 0, na.rm = TRUE)})


stable_min_max <- min_max_vertical %>% 
  mutate(proportion_unstable = number_below/prey_num)

total_v_prey_num <- min_max_vertical%>% 
  group_by(prey_num) %>% 
  summarise(total_all = n())

prey_num_proportion <- min_max_vertical %>% 
  group_by(Fail, prey_num) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_prey_num) %>% 
  mutate(proportion = total/total_all) 


total_v_num_below <- min_max_vertical%>%
  group_by(number_below) %>% 
  summarise(total_all = n())

num_below_proportion <- min_max_vertical %>% 
  group_by(Fail, number_below) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_num_below) %>% 
  mutate(proportion = total/total_all) 

total_v_num_below_prey_num <- min_max_vertical%>%
  group_by(number_below, prey_num) %>% 
  summarise(total_all = n())

num_below_proportion <- min_max_vertical %>% 
  group_by(Fail, number_below, prey_num) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_num_below_prey_num) %>% 
  mutate(proportion = total/total_all, proportion_community = number_below/prey_num) 


prey_num_proportion <- min_max_vertical %>% 
  group_by(Fail, prey_num) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_prey_num) %>% 
  mutate(proportion = total/total_all) 

prey_num_proportion_plot <-prey_num_proportion %>% 
  filter(Fail == FALSE) %>% 
  ggplot(aes(x = prey_num, y = proportion)) + geom_line() + xlab("Initial prey diversity") + ylab("Proportion of runs where predator persists") 

Prey_community_prortion <- num_below_proportion %>% 
  ungroup() %>% 
  mutate(bined_proportion = .bincode(num_below_proportion$proportion_community, c(0, 0.25, 0.5, 0.75, 1), right = TRUE, include.lowest = TRUE)) %>% 
  filter(Fail == FALSE) %>% 
  ggplot(aes(x = prey_num, y = proportion, colour = proportion_community)) + geom_point() +
  scale_colour_viridis(name = "Proportion of \nsmall prey in \nthe community") + xlab("Initial prey diversity") + ylab("Proportion of runs where predator persists") 

ggsave('Figures/Figure4b.jpeg', Prey_community_prortion, width = 6.5)
ggsave('Figures/Figure4a.jpeg', prey_num_proportion_plot, width = 5)


##########FIGURE 4 and A6##############

min_max_vertical$number_above <- apply(bm_min_max_vertical, 1, function(x){sum(x > 0.5, na.rm = TRUE)})

min_max_vertical$duplicated <- apply(bm_min_max_vertical, 1, function(x){sum(duplicated(x, incomparables=NA), na.rm = TRUE)})


total_v_num_above_prey_num <- min_max_vertical%>%
  group_by(number_above, prey_num) %>% 
  summarise(total_all = n())

num_above_proportion <- min_max_vertical %>% 
  group_by(Fail, number_above, prey_num) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_num_above_prey_num) %>% 
  mutate(proportion = total/total_all, proportion_community = number_above/prey_num) 

fig_num_above_proportion <- num_above_proportion %>% 
  ungroup() %>% 
  filter(Fail == FALSE) %>% 
  ggplot(aes(x = prey_num, y = proportion, colour = proportion_community)) + geom_point() +
  scale_colour_viridis(name = 'Proportion of \nlarge prey in \nthe community') + xlab("Initial prey diversity") + ylab("Proportion of runs where predator persists") 

ggsave('Figures/Figure4c.jpeg', fig_num_above_proportion, width = 6.5)


total_v_num_duplicated_prey_num <- min_max_vertical%>%
  group_by(duplicated, prey_num) %>% 
  summarise(total_all = n())

num_duplicated_proportion <- min_max_vertical %>% 
  group_by(Fail, duplicated, prey_num) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_num_duplicated_prey_num) %>% 
  mutate(proportion = total/total_all, proportion_community = duplicated/prey_num) 

fig_num_duplicated_proportion <- num_duplicated_proportion %>% 
  ungroup() %>% 
  filter(Fail == FALSE) %>% 
  ggplot(aes(x = prey_num, y = proportion, colour = proportion_community)) + geom_point() +
  scale_colour_viridis(name = 'Proportion of \nduplicate prey in \nthe community') + xlab("Initial prey diversity") + ylab("Proportion of runs where predator persists") 

ggsave('Figures/FigureA6.jpeg', fig_num_duplicated_proportion, width = 6.5)


#### two prey scenario Figure 3 #####

#symetric

two_prey <- read.csv("Results/two_prey_outcomes_symetric.csv")


scenarios <- two_prey %>%
  mutate(all_survive = ifelse(R1 > 1 & R2 > 1 & P > 1, TRUE, FALSE)) %>%
  mutate(horizontal = ifelse(R1 > 1 & R2 > 1 & P < 1, TRUE, FALSE)) %>%
  mutate(pred_max = ifelse(P > 10, TRUE, FALSE)) %>%
  mutate(vertical1 = ifelse(R1 > 1 & R2 < 1 & P > 1, TRUE, FALSE)) %>%
  mutate(vertical2 = ifelse(R1 < 1 & R2 > 1 & P > 1, TRUE, FALSE)) %>%
  mutate(vertical = ifelse(vertical1 == TRUE | vertical2 == TRUE, TRUE, FALSE))


percentage_scenarios <- scenarios %>%
  group_by(X1, X2) %>%
  summarise(p_survive = sum(all_survive)/10, p_horizontal = sum(horizontal)/10, 
            p_pred_max = sum(pred_max)/10, p_vertical = sum(vertical)/10) 
  
survive_percentage <- percentage_scenarios %>%
  ggplot(aes(x = X1, y = X2)) + geom_point(aes(color=p_survive), shape=15, size=5) +
  scale_color_viridis() + theme(legend.position="none") + xlab("Body mass 1") + ylab("Body mass 2")


horizontal_percentage <- percentage_scenarios %>%
  ggplot(aes(x = X1, y = X2)) + geom_point(aes(color=p_horizontal), shape=15, size=5) +
  scale_color_viridis() + theme(legend.position="none") + xlab("Body mass 1") + ylab("")

vertical_percentage <- percentage_scenarios %>%
  ggplot(aes(x = X1, y = X2)) + geom_point(aes(color=p_vertical), shape=15, size=5) +
  scale_color_viridis() + theme(legend.position="none") + xlab("Body mass 1") + ylab("")

two_prey_plot <- plot_grid(survive_percentage, horizontal_percentage, vertical_percentage, nrow = 1)

ggsave('Figures/Figure3.jpeg', two_prey_plot, width = 12, height  = 4)


legend_two_all <- percentage_scenarios %>%
  ggplot(aes(x = X1, y = X2)) + geom_point(aes(color=p_horizontal), shape=15, size=5) +
  scale_color_viridis(breaks = c(0, 0.25, 0.5, 0.75, 1))  +
  theme(legend.position="bottom")

ggsave('Figures/legendFigure3.jpeg', legend_two_all )


# Asymetric Figure A9


two_prey <- read.csv("Results/two_prey_outcomes_asymetric.csv")


scenarios <- two_prey %>%
  mutate(all_survive = ifelse(R1 > 1 & R2 > 1 & P > 1, TRUE, FALSE)) %>%
  mutate(horizontal = ifelse(R1 > 1 & R2 > 1 & P < 1, TRUE, FALSE)) %>%
  mutate(pred_max = ifelse(P > 10, TRUE, FALSE)) %>%
  mutate(vertical1 = ifelse(R1 > 1 & R2 < 1 & P > 1, TRUE, FALSE)) %>%
  mutate(vertical2 = ifelse(R1 < 1 & R2 > 1 & P > 1, TRUE, FALSE)) %>%
  mutate(vertical = ifelse(vertical1 == TRUE | vertical2 == TRUE, TRUE, FALSE))


percentage_scenarios <- scenarios %>%
  group_by(X1, X2) %>%
  summarise(p_survive = sum(all_survive)/10, p_horizontal = sum(horizontal)/10, 
            p_pred_max = sum(pred_max)/10, p_vertical = sum(vertical)/10) 

survive_percentage <- percentage_scenarios %>%
  ggplot(aes(x = X1, y = X2)) + geom_point(aes(color=p_survive), shape=15, size=5) +
  scale_color_viridis() + theme(legend.position="none") + xlab("Body mass 1") + ylab("Body mass 2")


horizontal_percentage <- percentage_scenarios %>%
  ggplot(aes(x = X1, y = X2)) + geom_point(aes(color=p_horizontal), shape=15, size=5) +
  scale_color_viridis() + theme(legend.position="none") + xlab("Body mass 1") + ylab("")

vertical_percentage <- percentage_scenarios %>%
  ggplot(aes(x = X1, y = X2)) + geom_point(aes(color=p_vertical), shape=15, size=5) +
  scale_color_viridis() + theme(legend.position="none") + xlab("Body mass 1") + ylab("")

two_prey_plot <- plot_grid(survive_percentage, horizontal_percentage, vertical_percentage, nrow = 1)

ggsave('Figures/FigureA9.jpeg', two_prey_plot, width = 12,height  = 4)



##### Figure 5####

min_max_survived <- survived %>% 
  filter(Fail == FALSE) %>% 
  rowwise() %>% 
  mutate(min_bm = lift_vd(min, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21),
         max_bm = lift_vd(max, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21)) 

min_bm_support <- min_max_survived %>%  
  ggplot(aes(x = min_bm, y = prey_num, colour = prey_num)) + geom_point() + scale_colour_viridis() +
  xlab('Minimum body mass to support community') + ylab('Prey diversity') + theme(legend.position="none") + scale_x_continuous(limits = c(0,0.5)) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16))

ggsave('Figures/Figure5a.jpeg', min_bm_support, height = 8)



#### Figure 5 b anc c and Figure A5 #####


survived_a <- survived %>% 
  #slice(1:2) %>% 
  filter(prey_num < 14) %>% 
  select(starts_with("X"), mean_bm) %>% 
  mutate_all(attack_r, a0 = 0.0005, br = 1.30000000,e = 0.07172922) %>% 
  rowwise() %>% 
  mutate(mean_a = lift_vd(mean, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, 
                                              X18, X19, X20, X21)) %>% as.data.frame()

survived_h <- survived %>% 
  filter(prey_num < 14) %>% 
  select(starts_with("X"), mean_bm) %>% 
  mutate_all(handling_r, h0 = 5, al = 2.41854333) %>% 
  rowwise() %>% 
  mutate(mean_h = lift_vd(mean, na.rm = TRUE)(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, 
                                              X18, X19, X20, X21)) %>% as.data.frame()
  
colnames(survived_a) <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14","a15","a16","a17","a18",
                          "a19","a20","a21","a_mean_bm", "mean_a")

colnames(survived_h) <- c("h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", "h10", "h11", "h12", "h13", "h14","h15","h16","h17","h18",
                          "h19","h20","h21", "h_mean_bm", "mean_h")


a_h_survived <- survived %>% 
  filter(prey_num < 14) %>% 
  select(Fail, prey_num) %>% 
  bind_cols(survived_a, survived_h)

ah_mean_bm <- a_h_survived %>% 
  filter(prey_num %in% c(1:13), Fail == FALSE) %>% 
  ggplot(aes(x = a_mean_bm, y = h_mean_bm, colour = prey_num)) + geom_point() +
  scale_colour_viridis() + xlab(expression(paste("a(",bar("m"),")"))) + ylab(expression(paste("h(",bar("m"),")"))) +
  scale_y_continuous(limits = c(0, 3))+ theme(legend.position = "none") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))

mean_ah <- a_h_survived %>% 
  filter(prey_num %in% c(1:13), Fail == FALSE) %>% 
  ggplot(aes(x = mean_a, y = mean_h, colour = prey_num)) + geom_point() +
  scale_colour_viridis() + scale_y_continuous(limits = c(0, 3)) + ylab(expression(bar("h(m)")) ) +
  xlab(expression(bar("a(m)")) ) + theme(legend.position = "none")+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))


h_plot <-a_h_survived %>% 
  filter(prey_num %in% c(1:13), Fail == FALSE) %>% 
  ggplot(aes(x = h_mean_bm, y = mean_h, colour = prey_num)) + geom_point() +
  scale_colour_viridis() + scale_x_continuous(limits = c(0, 3)) + scale_y_continuous(limits = c(0, 3)) +
  geom_abline(slope = 1, intercept = 0) + theme(legend.position = "none") + xlab(expression(paste("h(",bar("m"),")"))) +
  ylab(expression(bar("h(m)")) )+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))

a_plot <-a_h_survived %>% 
  filter(prey_num %in% c(1:13), Fail == FALSE) %>% 
  ggplot(aes(x = a_mean_bm, y = mean_a, colour = prey_num)) + geom_point() +
  scale_colour_viridis()  +
  geom_abline(slope = 1, intercept = 0) + theme(legend.position = "none") + xlab(expression(paste("a(",bar("m"),")"))) +
  ylab(expression(bar("a(m)")) ) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=26))


legend_number <-a_h_survived %>% 
  filter(prey_num %in% c(1:13), Fail == FALSE) %>% 
  ggplot(aes(x = a_mean_bm, y = mean_a, colour = prey_num)) + geom_point() +
  scale_colour_viridis(name = "Prey diversity", breaks = c(0, 2, 4, 6, 8, 10, 12))  +
  geom_abline(slope = 1, intercept = 0) + theme() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20))


ggsave('Figures/FigureA5a.jpeg', ah_mean_bm)
ggsave('Figures/FigureA5b.jpeg', mean_ah)
ggsave('Figures/Figure5c.jpeg', h_plot)
ggsave('Figures/Figure5b.jpeg', a_plot)
ggsave('Figures/legendFigure5.jpeg', legend_number)



##### FIGURE A4####
two_small <- run_stochastic_model(c(0.1, 0.15), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('Abundance') + xlab("") +
  theme( axis.text.x = element_text(size = 10)) 

two_medium <- run_stochastic_model(c(0.15, 0.2), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('') + xlab("")+
  theme( axis.text.x = element_text(size = 10)) 

two_medium_l <- run_stochastic_model(c(0.2, 0.35), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('Abundance') + xlab("Time")+
  theme( axis.text.x = element_text(size = 10)) 

two_large <- run_stochastic_model(c(0.35, 0.4), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('') + xlab("Time")+
  theme( axis.text.x = element_text(size = 10)) 



two_prey_plot_time_series <- plot_grid(two_small, two_medium, two_medium_l, two_large, 
                           labels = c('R1 = 0.1, R2 = 0.15', 'R1 = 0.15, R2 = 0.2', 'R1 = 0.2, R2 = 0.35', 
                                      'R1 = 0.35, R2 = 0.4'))

ggsave('Figures/FigureA4.jpeg', two_prey_plot_time_series )


#### FIGURE A3 #####

two_medium_same <- run_stochastic_model(c(0.2, 0.20), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('Abundance') + xlab("") +
  theme( axis.text.x = element_text(size = 10)) 

two_medium_s_diff <- run_stochastic_model(c(0.2, 0.1), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line()+ ylab('') + xlab("")+
  theme( axis.text.x = element_text(size = 10)) 

two_large_same <- run_stochastic_model(c(0.4, 0.4), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('Abundance') + xlab("Time")+
  theme( axis.text.x = element_text(size = 10)) 


two_large_s_diff <- run_stochastic_model(c(0.4, 0.41), 500, 0) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line()+ ylab('') + xlab("Time")+
  theme( axis.text.x = element_text(size = 10)) 



two_prey_plot_time_series_similar <- plot_grid(two_medium_same, two_medium_s_diff, two_large_same, two_large_s_diff, 
                                       labels = c('R1 = 0.2, R2 = 0.2', 'R1 = 0.2, R2 = 0.21', 'R1 = 0.4, R2 = 0.4', 
                                                  'R1 = 0.4, R2 = 0.41'))

ggsave('Figures/FigureA3.jpeg', two_prey_plot_time_series_similar)



#### Asymetric time series ####


two_small <- run_stochastic_model(c(0.08, 0.15), 500, 0.5) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('Abundance') + xlab("") +
  theme( axis.text.x = element_text(size = 10)) 

two_medium <- run_stochastic_model(c(0.15, 0.2), 500, 0.5) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('') + xlab("")+
  theme( axis.text.x = element_text(size = 10)) 

two_medium_l <- run_stochastic_model(c(0.14, 0.15), 500, 0.5) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('Abundance') + xlab("Time")+
  theme( axis.text.x = element_text(size = 10)) 

two_large <- run_stochastic_model(c(0.35, 0.4), 500, 0.5) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab('') + xlab("Time")+
  theme( axis.text.x = element_text(size = 10)) 



two_prey_plot_time_series <- plot_grid(two_small, two_medium, two_medium_l, two_large, 
                                       labels = c('R1 = 0.08, R2 = 0.15', 'R1 = 0.15, R2 = 0.2', 'R1 = 0.14, R2 = 0.15', 
                                                  'R1 = 0.35, R2 = 0.4'))

ggsave('Figures/FigureA8.jpeg', two_prey_plot_time_series )




##### Figure 2 ####


small <- run_stochastic_model(c(0.07), 500) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab("Abundance") + xlab("") +
  theme(legend.position="none")

medium <- run_stochastic_model(c(0.12), 500) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab("") + xlab("Time") + 
  theme(legend.position="none")

large <- run_stochastic_model(c(0.25), 500) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab("") + xlab("") + 
  theme(legend.position="none")

one_prey <- plot_grid(small, medium, large, nrow = 1)

ggsave('Figures/Figure2.jpeg', one_prey, width = 10)

legend_one_prey <- run_stochastic_model(c(0.25), 500) %>%
  gather(species, abundance, -time) %>%
  ggplot(aes(x = time, y = abundance, colour = species)) + geom_line() + ylab("") + xlab("")

ggsave('Figures/legendFigure2.jpeg', legend_one_prey)



######below and above, num and survive #####

total_v_num_below_prey_num <- min_max_vertical%>%
  group_by(number_below, number_above, prey_num, prey_survive) %>% 
  summarise(total_all = n())

num_below_proportion <- min_max_vertical %>% 
  group_by(Fail,number_below, number_above, prey_num, prey_survive) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_num_below_prey_num) %>% 
  mutate(proportion = total/total_all, proportion_below_community = number_below/prey_num, proportion_above_community = number_above/prey_num) 

below_end <- num_below_proportion %>% 
  ungroup() %>% 
  filter(Fail == FALSE) %>% 
  ggplot(aes(x = prey_survive, y = proportion, colour = proportion_below_community)) + geom_point(position = 'jitter') +
  scale_colour_viridis(name = 'Proportion of \nsmall prey in\nthe community') +  xlab("Prey diversity at the end of the simulation") + ylab("Proportion of runs where predator survives") 

above_end <- num_below_proportion %>% 
  ungroup() %>% 
  filter(Fail == FALSE) %>% 
  ggplot(aes(x = prey_survive, y = proportion, colour = proportion_above_community)) + geom_point(position = 'jitter') +
  scale_colour_viridis(name = 'Proportion of \nlarge prey in\nthe community') +  xlab("Prey diversity at the end of the simulation") + ylab("Proportion of runs where predator survives") 

above_below_survive <- plot_grid(below_end, above_end)

ggsave('Figures/FigureA7a.jpeg', above_below_survive, width = 12)




######above, survive FIGURE S6 part 2 #####

total_v_num_below_prey_num <- min_max_vertical%>%
  group_by(number_above, prey_num, prey_survive) %>% 
  summarise(total_all = n())

num_below_proportion <- min_max_vertical %>% 
  group_by(Fail,number_above, prey_num, prey_survive) %>% 
  summarise(total = n()) %>% 
  left_join(total_v_num_below_prey_num) %>% 
  mutate(proportion = total/total_all, proportion_above_community = number_above/prey_num) 

above_survive <- num_below_proportion %>% 
  ungroup() %>% 
  filter(Fail == FALSE) %>% 
  ggplot(aes(x = prey_survive, y = proportion, colour = proportion_above_community)) + geom_point(position = 'jitter') +
  scale_colour_viridis(name = 'Proportion of \nlarge prey in\nthe community') +  xlab("Prey diversity at the end of the simulation") + ylab("Proportion of runs where predator persists") 


ggsave('Figures/FigureA7b.jpeg', above_survive, width = 6.5)



