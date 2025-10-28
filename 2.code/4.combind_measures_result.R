rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes","patchwork","metR")
pacman::p_load(pkgs,character.only = T)


# 1.loading results -------------------------------------------------------
source("2.code/result_sim.R")
source("2.code/plot function.R")
result_without_intervention <- readRDS("3.result/result_without_intervention.rds") 
result_combind_measures_control_time <- readRDS("3.result/result_combind_measures_control_time.rds")
result_combind_measures_control_time_summary <- result_sim3(result_combind_measures_control_time)

times <- 1:180
start_date <- as.Date("2025-06-16")

# 2.baseline scenario -----------------------------------------------------

result_without_intervention <- data.frame(day = result_without_intervention$day,
                                          ua_c = 0,um_c =0,R_bite = 0,transmission_time= 7,control_time =31) %>% 
  cbind(result_without_intervention[,c("low","high","mid","total_low","total_high","total_mid")])

result_without_intervention_summary <- result_sim3(result_without_intervention)
Np <- 9698900
result_without_intervention_summary$total_cases_mid / Np*100
result_without_intervention_summary$total_cases_low / Np*100
result_without_intervention_summary$total_cases_high / Np*100

p_without_intervention <- p_without_intervention_plot(data = result_without_intervention)

ggsave(filename = "3.result/plot fig/Fig. S2 p_without_intervention.png",p_without_intervention,width = 18,height = 8,dpi = 400,units = "cm")

# 3.intervention scenarios --------------------------------------------------------
## scenario 1 : single measure ####
#larval elimination, LE
result_foshan_data_single_measure_aquatic_stage <- subset(result_combind_measures_control_time,ua_c !=0 & um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==31) %>% 
  rbind(result_without_intervention,.)
result_foshan_data_single_measure_aquatic_stage_summary <- subset(result_combind_measures_control_time_summary,ua_c !=0 & um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==31) %>% 
  rbind(result_without_intervention_summary,.)
#adult mosquito elimination, AME
result_foshan_data_single_measure_adult_stage <- subset(result_combind_measures_control_time,ua_c ==0 & um_c !=0 & R_bite == 0 & transmission_time == 7 & control_time ==31) %>% 
  rbind(result_without_intervention,.)
result_foshan_data_single_measure_adult_stage_summary <- subset(result_combind_measures_control_time_summary,ua_c ==0 & um_c !=0 & R_bite == 0 & transmission_time == 7 & control_time ==31) %>% 
  rbind(result_without_intervention_summary,.)
#adult mosquito elimination, AME -- sub
result_foshan_data_single_measure_adult_stage_sub <- result_foshan_data_single_measure_adult_stage %>% 
  .[!(.$um_c %in% c("0", "0.1", "0.2")),]
# mosquito bite rate reduction, BRR
result_foshan_data_single_measure_Rbite <- subset(result_combind_measures_control_time,ua_c ==0 & um_c ==0 & R_bite != 0 & transmission_time == 7 & control_time ==31) %>% 
  rbind(result_without_intervention,.)
result_foshan_data_single_measure_Rbite_summary <- subset(result_combind_measures_control_time_summary,ua_c ==0 & um_c ==0 & R_bite != 0 & transmission_time == 7 & control_time ==31) %>% 
  rbind(result_without_intervention_summary,.)

#infectious period shortening, IPS
result_foshan_data_single_measure_shorten_time <- subset(result_combind_measures_control_time,ua_c ==0 & um_c ==0 & R_bite == 0 & transmission_time != 7 & control_time ==31) %>% 
  rbind(result_without_intervention)
result_foshan_data_single_measure_shorten_time$transmission_time <- factor(result_foshan_data_single_measure_shorten_time$transmission_time,levels = c("7","6","5","4","3","2","1"))
result_foshan_data_single_measure_shorten_time_summary <- subset(result_combind_measures_control_time_summary,ua_c ==0 & um_c ==0 & R_bite == 0 & transmission_time != 7 & control_time ==31) %>% 
  rbind(result_without_intervention_summary)
result_foshan_data_single_measure_shorten_time_summary$transmission_time <- factor(result_foshan_data_single_measure_shorten_time_summary$transmission_time,levels = c("7","6","5","4","3","2","1"))


#line plot
p_aquatic_stage <- p_line_func(result_foshan_data_single_measure_aquatic_stage,title = "a")

p_adult_stage <- p_line_func(result_foshan_data_single_measure_adult_stage[,-c(2)],title = "b")

p_Rbite <- p_line_func(result_foshan_data_single_measure_Rbite[,-c(2,3)],title = "c")

p_shorten_time <- p_line_func(result_foshan_data_single_measure_shorten_time[,-c(2,3,4)],"d")

#line plot--sub
names(result_foshan_data_single_measure_adult_stage_sub)[3] <- "group"
result_foshan_data_single_measure_adult_stage_sub$group <- as.factor(result_foshan_data_single_measure_adult_stage_sub$group)
p_adult_stage_sub <- p_adult_stage_sub_plot(result_foshan_data_single_measure_adult_stage_sub)
p_adult_stage_sub
ggsave(filename = "3.result/plot fig/single_intervention_adult_stage_sub.png",p_adult_stage_sub,width = 12,height = 8,dpi = 400,units = "cm")

#bar plot--sub
result_foshan_data_single_measure_adult_stage_sub_summary <- result_foshan_data_single_measure_adult_stage_summary %>% 
  .[!(.$um_c %in% c("0", "0.1", "0.2")),]
names(result_foshan_data_single_measure_adult_stage_sub_summary)[2] <- "group"
result_foshan_data_single_measure_adult_stage_sub_summary$group  <- as.factor(result_foshan_data_single_measure_adult_stage_sub_summary$group)
p_adult_stage_sub_bar <- p_adult_stage_sub_bar_plot(result_foshan_data_single_measure_adult_stage_sub_summary)
ggsave(filename = "3.result/plot fig/single_intervention_adult_stage_sub_bar.png",p_adult_stage_sub_bar,width = 8,height = 8,dpi = 400,units = "cm")

#bar plot

names(result_foshan_data_single_measure_aquatic_stage_summary)[1] <- "group"
result_foshan_data_single_measure_aquatic_stage_summary$group <- as.factor(result_foshan_data_single_measure_aquatic_stage_summary$group)
p_aquatic_stage_bar <- p_bar_func_total(result_foshan_data_single_measure_aquatic_stage_summary)

names(result_foshan_data_single_measure_adult_stage_summary)[2] <- "group"
result_foshan_data_single_measure_adult_stage_summary$group <- as.factor(result_foshan_data_single_measure_adult_stage_summary$group)
p_adult_stage_bar <- p_bar_func_total(result_foshan_data_single_measure_adult_stage_summary)

names(result_foshan_data_single_measure_Rbite_summary)[3] <- "group"
result_foshan_data_single_measure_Rbite_summary$group <- as.factor(result_foshan_data_single_measure_Rbite_summary$group)
p_Rbite_bar <- p_bar_func_total(result_foshan_data_single_measure_Rbite_summary)

names(result_foshan_data_single_measure_shorten_time_summary)[4] <- "group"
result_foshan_data_single_measure_shorten_time_summary$group <- as.factor(result_foshan_data_single_measure_shorten_time_summary$group)
p_shorten_time_bar <- p_bar_func_total(result_foshan_data_single_measure_shorten_time_summary)

combined <- (p_aquatic_stage + p_aquatic_stage_bar + p_adult_stage+ p_adult_stage_bar + p_Rbite+ p_Rbite_bar + p_shorten_time +p_shorten_time_bar) + 
  plot_layout(ncol = 2,widths = c(3, 1))
combined
ggsave(filename = "3.result/plot fig/Fig.2 single_intervention.png",combined,width = 18,height = 18,dpi = 400,units = "cm")


## scenario 2 : single measure & initial intervention time ####

#larval elimination & initial intervention time
result_foshan_data_single_measure_aquatic_stage_control_time <- subset(result_combind_measures_control_time,ua_c !=0 & um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time !=31 ) 
result_foshan_data_single_measure_aquatic_stage_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c !=0 & um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time !=31)
#adult mosquito elimination & initial intervention time
result_foshan_data_single_measure_adult_stage_control_time <- subset(result_combind_measures_control_time,ua_c ==0 & um_c !=0 & R_bite == 0 & transmission_time == 7 & control_time !=31)
result_foshan_data_single_measure_adult_stage_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c ==0 & um_c !=0 & R_bite == 0 & transmission_time == 7 & control_time !=31)

# mosquito bite rate reduction & initial intervention time
result_foshan_data_single_measure_Rbite_control_time <- subset(result_combind_measures_control_time,ua_c ==0 & um_c ==0 & R_bite != 0 & transmission_time == 7 & control_time !=31)
result_foshan_data_single_measure_Rbite_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c ==0 & um_c ==0 & R_bite != 0 & transmission_time == 7 & control_time !=31) 

#infectious period shortening & initial intervention time
result_foshan_data_single_measure_shorten_time_control_time <- subset(result_combind_measures_control_time,ua_c ==0 & um_c ==0 & R_bite == 0 & control_time !=31) 
result_foshan_data_single_measure_shorten_time_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c ==0 & um_c ==0 & R_bite == 0 & control_time !=31)

result_foshan_data_single_measure_control_time_summary <- rbind(result_foshan_data_single_measure_aquatic_stage_control_time_summary,
                                                                result_foshan_data_single_measure_adult_stage_control_time_summary,
                                                                result_foshan_data_single_measure_Rbite_control_time_summary,
                                                                result_foshan_data_single_measure_shorten_time_control_time_summary)

## scenario 2 : single measure & initial intervention time plot####
#cumulative cases
names(result_foshan_data_single_measure_aquatic_stage_control_time_summary)[1] <- "group"
result_foshan_data_single_measure_aquatic_stage_control_time_summary$group <- as.factor(result_foshan_data_single_measure_aquatic_stage_control_time_summary$group)
p_bar_aquatic_mosquitoes_total <- p_bar_func_total(result_foshan_data_single_measure_aquatic_stage_control_time_summary,title = "a",legend.position= "right")
#
names(result_foshan_data_single_measure_adult_stage_control_time_summary)[2] <- "group"
result_foshan_data_single_measure_adult_stage_control_time_summary$group <- as.factor(result_foshan_data_single_measure_adult_stage_control_time_summary$group)
p_bar_adult_mosquitoes_total <- p_bar_func_total(result_foshan_data_single_measure_adult_stage_control_time_summary,title = "b",legend.position= "right")

#
names(result_foshan_data_single_measure_Rbite_control_time_summary)[3] <- "group"
result_foshan_data_single_measure_Rbite_control_time_summary$group <- as.factor(result_foshan_data_single_measure_Rbite_control_time_summary$group)
p_bar_Rbite_total <- p_bar_func_total(result_foshan_data_single_measure_Rbite_control_time_summary,title = "c",legend.position= "right")

#
names(result_foshan_data_single_measure_shorten_time_control_time_summary)[4] <- "group"
result_foshan_data_single_measure_shorten_time_control_time_summary$group <- factor(result_foshan_data_single_measure_shorten_time_control_time_summary$group,levels = c(c("7","6","5","4","3","2","1")))

p_bar_shorten_time_total <- p_bar_func_total(result_foshan_data_single_measure_shorten_time_control_time_summary,title = "d",legend.position= "right")

p_bar_total_bind <- p_bar_aquatic_mosquitoes_total/p_bar_adult_mosquitoes_total/p_bar_Rbite_total/p_bar_shorten_time_total

ggsave(filename = "3.result/plot fig/Fig.3(a-d) p_bar_total_bind.png",p_bar_total_bind,width = 18,height = 11,dpi = 400,units = "cm")

#peak value
p_aquatic_stage_bar <- p_bar_func_peak(result_foshan_data_single_measure_aquatic_stage_control_time_summary,title = "e")
p_adult_stage_bar <- p_bar_func_peak(result_foshan_data_single_measure_adult_stage_control_time_summary,title = "f")
p_Rbite_bar <- p_bar_func_peak(result_foshan_data_single_measure_Rbite_control_time_summary,title = "g")
p_shorten_time_bar <- p_bar_func_peak(result_foshan_data_single_measure_shorten_time_control_time_summary,x_lab = "Intial intervention time",title = "h")

p_bar_peak_bind <- p_aquatic_stage_bar/p_adult_stage_bar /p_Rbite_bar/p_shorten_time_bar
p_bar_peak_bind 

ggsave(filename = "3.result/plot fig/Fig.3(e-h) p_bar_peak_bind.png",p_bar_peak_bind,width = 18,height = 11,dpi = 400,units = "cm")

## scenario 3 : Pairwise combination measures ####
result_data_aquatic_adult <- subset(result_combind_measures_control_time_summary,R_bite==0 & transmission_time ==7 &control_time == 31)
result_data_aquatic_Rbite <- subset(result_combind_measures_control_time_summary,um_c==0 & transmission_time ==7&control_time == 31)
result_data_aquatic_transmissiontime <- subset(result_combind_measures_control_time_summary,um_c==0 & R_bite ==0 &control_time == 31)

result_data_adult_Rbite <- subset(result_combind_measures_control_time_summary,ua_c==0 & transmission_time ==7 & control_time == 31)
result_data_adult_transmissiontime <- subset(result_combind_measures_control_time_summary,ua_c==0 & R_bite ==0 & control_time == 31)

result_data_Rbite_transmissiontime <- subset(result_combind_measures_control_time_summary,ua_c==0 & um_c==0 & control_time == 31)

result_data_Pairwise_combination_summary <- rbind(result_data_aquatic_adult,
                                                  result_data_aquatic_Rbite,
                                                  result_data_aquatic_transmissiontime,
                                                  result_data_adult_Rbite,
                                                  result_data_adult_transmissiontime,
                                                  result_data_Rbite_transmissiontime,
                                                  result_data_Rbite_transmissiontime)

#### scenario 3 : Pairwise combination measures  plot####
p_Contour_aquatic_adult <- p_Contour_func(data = result_data_aquatic_adult , x = ua_c, y = um_c, title1 = "a",title2 = "g", 
                                          xlab = "Larval elimination", ylab = "Adult mosquito elimination",
                                          xlabel = seq(0,0.9,0.1),ylabel = seq(0,0.9,0.1))
p_Contour_aquatic_Rbite <- p_Contour_func(data = result_data_aquatic_Rbite , x = ua_c, y = R_bite, title1 = "b",title2 = "h",xlab = "Larval elimination", 
                                          ylab = "Mosquito bite rate reduction",xlabel = seq(0,0.9,0.1),ylabel = seq(0,0.9,0.1) )
p_Contour_aquatic_transmissiontime <- p_Contour_func(data = result_data_aquatic_transmissiontime , x = ua_c, y = transmission_time, xlab = "Larval elimination", 
                                                     ylab = "Infectious period shortening", title1 = "c",title2 = "i",
                                                     xlabel = seq(0,0.9,0.1),ylabel = seq(0,7,1))

p_Contour_adult_transmissiontime <- p_Contour_func(data = result_data_adult_transmissiontime , x = um_c, y = transmission_time, 
                                                   xlab = "Adult mosquito elimination", ylab = "Infectious period shortening",
                                                   title1 = "d",title2 = "j",
                                                   xlabel = seq(0,0.9,0.1),ylabel = seq(0,7,1))
p_Contour_adult_Rbite <- p_Contour_func(data = result_data_adult_Rbite , x = um_c, y = R_bite, xlab = "Adult mosquito elimination", 
                                        ylab = "Mosquito bite rate reduction",title1 = "e",title2 = "k",
                                        xlabel = seq(0,0.9,0.1),ylabel = seq(0,0.9,0.1))

p_Contour_Rbite_transmission_time <- p_Contour_func(data = result_data_Rbite_transmissiontime , x = R_bite, y = transmission_time,
                                                    xlab = "Mosquito bite rate reduction", ylab = "Infectious period shortening",title1 = "f",title2 = "l",
                                                    xlabel = seq(0,0.9,0.1),ylabel = seq(0,7,1))

#total
p_Contour_total <- (p_Contour_aquatic_adult[[1]]+p_Contour_aquatic_Rbite[[1]])/(p_Contour_aquatic_transmissiontime[[1]]+p_Contour_adult_transmissiontime[[1]])/(p_Contour_adult_Rbite[[1]] + p_Contour_Rbite_transmission_time[[1]])

ggsave(filename = "3.result/plot fig/Fig. 4(a–f) p_Contour_total.png",p_Contour_total,width = 18,height = 18,dpi = 400,units = "cm")


#peak
p_Contour_peak <- (p_Contour_aquatic_adult[[2]]+p_Contour_aquatic_Rbite[[2]])/(p_Contour_aquatic_transmissiontime[[2]]+p_Contour_adult_transmissiontime[[2]])/(p_Contour_adult_Rbite[[2]] + p_Contour_Rbite_transmission_time[[2]])
ggsave(filename = "3.result/plot fig/Fig. 4(g–l) p_Contour_peak.png",p_Contour_peak,width = 17,height = 18,dpi = 400,units = "cm")


## scenario 4 : Pairwise combination measures & initial intervention time####
result_data_aquatic_adult_control_time_summary <- subset(result_combind_measures_control_time_summary,R_bite==0 & transmission_time ==7) %>% 
  mutate(facet_label = factor(paste("initial intervention time =",control_time),levels = paste("initial intervention time =",control_time)))
result_data_aquatic_Rbite_control_time_summary <- subset(result_combind_measures_control_time_summary,um_c==0 & transmission_time ==7) %>% 
  mutate(facet_label = factor(paste("initial intervention time =",control_time),levels = paste("initial intervention time =",control_time)))
result_data_aquatic_transmissiontime_control_time_summary <- subset(result_combind_measures_control_time_summary,um_c==0 & R_bite ==0)%>% 
  mutate(facet_label = factor(paste("initial intervention time =",control_time),levels = paste("initial intervention time =",control_time)))

result_data_adult_Rbite_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c==0 & transmission_time ==7 )%>% 
  mutate(facet_label = factor(paste("initial intervention time =",control_time),levels = paste("initial intervention time =",control_time)))
result_data_adult_transmissiontime_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c==0 & R_bite ==0 )%>% 
  mutate(facet_label = factor(paste("initial intervention time =",control_time),levels = paste("initial intervention time =",control_time)))

result_data_Rbite_transmissiontime_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c==0 & um_c==0 )%>% 
  mutate(facet_label = factor(paste("initial intervention time =",control_time),levels = paste("initial intervention time =",control_time)))

result_data_two_measures_control_time_summary <- rbind(result_data_aquatic_adult_control_time_summary,
                                                       result_data_aquatic_Rbite_control_time_summary,
                                                       result_data_aquatic_transmissiontime_control_time_summary,
                                                       result_data_adult_Rbite_control_time_summary,
                                                       result_data_adult_transmissiontime_control_time_summary,
                                                       result_data_Rbite_transmissiontime_control_time_summary)
write.xlsx(result_data_two_measures_control_time_summary,"3.result/result_data_two_measures_control_time_summary.xlsx")

#### scenario 4 : Pairwise combination measures & initial intervention time plot####

dir.create("3.result/plot fig/Fig. S3-8 two_measures_intervention_time")
p_Contour_aquatic_adult_grid <- p_Contour_func_grid(data = result_data_aquatic_adult_control_time_summary , x = ua_c, y = um_c, 
                                                    xlab = "Larval elimination", ylab = "Adult mosquito elimination",
                                                    xlabel = seq(0,0.8,0.1),ylabel = seq(0,0.8,0.1))
p_Contour_aquatic_adult_grid_bind <- p_Contour_aquatic_adult_grid[[1]]/p_Contour_aquatic_adult_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S3-8 two_measures_intervention_time/Fig. S3 p_Contour_aquatic_adult_grid_bind.png",p_Contour_aquatic_adult_grid_bind,width = 28,height = 25,dpi = 400,units = "in")

p_Contour_aquatic_Rbite_grid <- p_Contour_func_grid(data = result_data_aquatic_Rbite_control_time_summary , xlabel = seq(0,0.8,0.1),ylabel = seq(0,0.8,0.1),x = ua_c, y = R_bite, xlab = "Larval elimination", ylab = "Mosquito bite rate reduction" )
p_Contour_aquatic_Rbite_grid_bind <- p_Contour_aquatic_Rbite_grid[[1]]/p_Contour_aquatic_Rbite_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S3-8 two_measures_intervention_time/Fig. S4 p_Contour_aquatic_Rbite_grid_bind.png",p_Contour_aquatic_Rbite_grid_bind,width = 28,height = 25,dpi = 400,units = "in")

p_Contour_aquatic_transmissiontime_grid <- p_Contour_func_grid(data = result_data_aquatic_transmissiontime_control_time_summary , 
                                                               x = ua_c, y = transmission_time, xlabel = seq(0,0.8,0.1),ylabel = seq(0,7,1),
                                                               xlab = "Larval elimination", ylab = "Infectious period shortening" )
p_Contour_aquatic_transmissiontime_grid_bind <- p_Contour_aquatic_transmissiontime_grid[[1]]/p_Contour_aquatic_transmissiontime_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S3-8 two_measures_intervention_time/Fig. S5 p_Contour_aquatic_transmissiontime_grid_bind.png",p_Contour_aquatic_transmissiontime_grid_bind,width = 28,height = 25,dpi = 400,units = "in")


p_Contour_adult_transmissiontime_grid <- p_Contour_func_grid(data = result_data_adult_transmissiontime_control_time_summary ,xlabel = seq(0,0.8,0.1),ylabel = seq(0,7,1),
                                                             x = um_c, y = transmission_time, xlab = "Adult mosquito elimination", ylab = "Infectious period shortening" )
p_Contour_adult_transmissiontime_grid_bind <- p_Contour_adult_transmissiontime_grid[[1]]/p_Contour_adult_transmissiontime_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S3-8 two_measures_intervention_time/Fig. S6 p_Contour_adult_transmissiontime_grid_bind.png",p_Contour_adult_transmissiontime_grid_bind,width = 28,height = 25,dpi = 400,units = "in")

p_Contour_adult_Rbite_grid <- p_Contour_func_grid(data = result_data_adult_Rbite_control_time_summary , xlabel = seq(0,0.8,0.1),ylabel = seq(0,0.8,0.1),
                                                  x = um_c, y = R_bite, xlab = "Adult mosquito elimination", ylab = "Mosquito bite rate reduction" )
p_Contour_adult_Rbite_grid_bind <- p_Contour_adult_Rbite_grid[[1]]/p_Contour_adult_Rbite_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S3-8 two_measures_intervention_time/Fig. S7 p_Contour_adult_Rbite_grid_bind.png",p_Contour_adult_Rbite_grid_bind,width = 28,height = 25,dpi = 400,units = "in")

p_Contour_Rbite_transmissiontime_grid <- p_Contour_func_grid(data = result_data_Rbite_transmissiontime_control_time_summary , xlabel = seq(0,0.8,0.1),ylabel = seq(0,7,1),
                                                             x = R_bite, y = transmission_time, xlab = "Mosquito bite rate reduction", ylab = "Infectious period shortening" )
p_Contour_Rbite_transmissiontime_grid_bind <- p_Contour_Rbite_transmissiontime_grid[[1]]/p_Contour_Rbite_transmissiontime_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S3-8 two_measures_intervention_time/Fig. S8 p_Contour_Rbite_transmissiontime_grid_bind.png",p_Contour_Rbite_transmissiontime_grid_bind,width = 28,height = 25,dpi = 400,units = "in")


#### scenario 5 : Combination of three intervention measures ####
# LE & AME & BRR
result_data_aquatic_adult_R_bite_summary <- subset(result_combind_measures_control_time_summary, 
                                                   ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&R_bite %in% c(0.2,0.5,0.8) &transmission_time == 7 & control_time == 31)

# LE & AME & IPS
result_data_aquatic_adult_transmission_time_summary <- subset(result_combind_measures_control_time_summary,
                                                              ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)& R_bite == 0 & control_time == 31)

# LE & BRR & IPS
result_data_aquatic_R_bite_transmission_time_summary <- subset(result_combind_measures_control_time_summary,ua_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& um_c == 0 & control_time == 31)

# AME & BRR & IPS
result_data_adult_R_bite_transmission_time_summary <- subset(result_combind_measures_control_time_summary,um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& ua_c == 0 & control_time == 31)

write.xlsx(result_data_aquatic_adult_R_bite_summary,"3.result/result_data_aquatic_adult_R_bite_summary.xlsx")
write.xlsx(result_data_aquatic_adult_transmission_time_summary,"3.result/result_data_aquatic_adult_transmission_time_summary.xlsx")
write.xlsx(result_data_aquatic_R_bite_transmission_time_summary,"3.result/result_data_aquatic_R_bite_transmission_time_summary.xlsx")
write.xlsx(result_data_adult_R_bite_transmission_time_summary,"3.result/result_data_adult_R_bite_transmission_time_summary.xlsx")


#### scenario 6 : Combination of three intervention measures & initial intervention time####
# LE & AME & BRR
result_data_aquatic_adult_R_bite_control_time_summary <- subset(result_combind_measures_control_time_summary, 
                                                                ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&R_bite %in% c(0.2,0.5,0.8) &transmission_time == 7 & control_time %in% c(7,15,21)) %>% 
  mutate(x = paste0("(",paste(ua_c,um_c,R_bite,sep = ","),")"))

# LE & AME & IPS
result_data_aquatic_adult_transmission_time_control_time_summary <- subset(result_combind_measures_control_time_summary,
                                                                           ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)& R_bite == 0 & control_time %in% c(7,15,21))%>% 
  mutate(x = paste0("(",paste(ua_c,um_c,transmission_time,sep = ","),")"))

# LE & BRR & IPS
result_data_aquatic_R_bite_transmission_time_control_time_summary <- subset(result_combind_measures_control_time_summary,ua_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& um_c == 0 & control_time %in% c(7,15,21))%>% 
  mutate(x = paste0("(",paste(ua_c,R_bite,transmission_time,sep = ","),")"))

# AME & BRR & IPS
result_data_adult_R_bite_transmission_time_control_time_summary <- subset(result_combind_measures_control_time_summary,um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& ua_c == 0 & control_time %in% c(7,15,21))%>% 
  mutate(x = paste0("(",paste(um_c,R_bite,transmission_time,sep = ","),")"))


write.xlsx(result_data_aquatic_adult_R_bite_control_time_summary,"3.result/result_data_aquatic_adult_R_bite_control_time_summary.xlsx")
write.xlsx(result_data_aquatic_adult_transmission_time_control_time_summary,"3.result/result_data_aquatic_adult_transmission_time_control_time_summary.xlsx")
write.xlsx(result_data_aquatic_R_bite_transmission_time_control_time_summary,"3.result/result_data_aquatic_R_bite_transmission_time_control_time_summary.xlsx")
write.xlsx(result_data_adult_R_bite_transmission_time_control_time_summary,"3.result/result_data_adult_R_bite_transmission_time_control_time_summary.xlsx")


#cumulative cases
p_data_aquatic_adult_R_bite_control_time_summary<- p_bar_func_total_three(result_data_aquatic_adult_R_bite_control_time_summary,title= "a")
p_data_aquatic_adult_transmission_time_control_time_summary<- p_bar_func_total_three(result_data_aquatic_adult_transmission_time_control_time_summary,title= "b")
p_data_aquatic_R_bite_transmission_time_control_time_summary<- p_bar_func_total_three(result_data_aquatic_R_bite_transmission_time_control_time_summary,title= "c")
p_data_adult_R_bite_transmission_time_control_time_summary<- p_bar_func_total_three(result_data_adult_R_bite_transmission_time_control_time_summary,title= "d")

p_three_total_bind <- p_data_aquatic_adult_R_bite_control_time_summary/p_data_aquatic_adult_transmission_time_control_time_summary/p_data_aquatic_R_bite_transmission_time_control_time_summary/p_data_adult_R_bite_transmission_time_control_time_summary

ggsave(filename = "3.result/plot fig/Fig. S10 p_three_total_bind.png",p_three_total_bind,width = 20,height = 20,dpi = 400,units = "in")

#peak
p_data_aquatic_adult_R_bite_control_time_summary<- p_bar_func_peak_three(result_data_aquatic_adult_R_bite_control_time_summary,title= "a ")
p_data_aquatic_adult_transmission_time_control_time_summary<- p_bar_func_peak_three(result_data_aquatic_adult_transmission_time_control_time_summary,title= "b ")
p_data_aquatic_R_bite_transmission_time_control_time_summary<- p_bar_func_peak_three(result_data_aquatic_R_bite_transmission_time_control_time_summary,title= "c ")
p_data_adult_R_bite_transmission_time_control_time_summary<- p_bar_func_peak_three(result_data_adult_R_bite_transmission_time_control_time_summary,title= "d ")

p_three_total_bind_peak <- p_data_aquatic_adult_R_bite_control_time_summary/p_data_aquatic_adult_transmission_time_control_time_summary/p_data_aquatic_R_bite_transmission_time_control_time_summary/p_data_adult_R_bite_transmission_time_control_time_summary

ggsave(filename = "3.result/plot fig/Fig. S11 p_three_total_bind_peak.png",p_three_total_bind_peak,width = 20,height = 20,dpi = 400,units = "in")



