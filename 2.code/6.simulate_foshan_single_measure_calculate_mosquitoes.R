rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes")
pacman::p_load(pkgs,character.only = T)

#lood model
source("2.code/CHIKV-model.R")

#load initial values and fixed parameters
source("2.code/init_value & fixed parameters.R")

#simulate-max day
max_day <- 180
#read posterior_samples
set.seed(2025)
posterior_samples <- readRDS("3.result/posterior_samples_foshan.rds") %>% 
  .[sample(nrow(.), 100), ]
times = 1:max_day 


# 1.Intervention: Eliminate mosquitoes in the aquatic stage ---------------
#form 10% - 90%

result_aquatic_stage <- c()
control_time <- 31
um_c <- 0
R_bite <- 0
transmission_time <- 7
R_gamma1  <-  R_gamma <- 1/transmission_time

pro_aquatic_data <- c()
pro_aquatic1 <- c()
for (ua_c in seq(0,0.9,0.1)) {
  pro_aquatic <- c()
  for(i in 1:nrow(posterior_samples)){
    parms <- c(parms.fixed, posterior_samples[i,]) 
    k <- parms["k"]
    Np <- 9698900
    Nm <- Np*as.numeric(k)
    init_value <- c(Sa = Nm, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1,Maqua=0 , Madult=0)
    out   <- ode(y   = init_value,
                 times = times,
                 func  = CHIKV_model_with_combind_intervention, #model change
                 parms = parms)
    sim_maqua <- diff(out[, "Maqua"]) %>%
      c(out[1, "Maqua"], .)
    sum_aquatic <- data.frame(out) %>% 
      mutate(aquatic = Sa+Ia)
    pro_aquatic0 <- sim_maqua/c(1,sum_aquatic[1:179,"aquatic"])
    pro_aquatic <- cbind(pro_aquatic,pro_aquatic0)
  }
  pro_aquatic1 <- data.frame(group = ua_c, pro_aquatic)
  pro_aquatic_data <- rbind(pro_aquatic_data,pro_aquatic1)
}



pro_aquatic_data <- pro_aquatic_data %>% 
  mutate(mid = apply(.[,2:101],1,median),
         low   = apply(.[,2:101], 1, quantile, probs = 0.025),
         high  = apply(.[,2:101], 1, quantile, probs = 0.975)) %>% 
  .[,c("group","mid","low","high")]
pro_aquatic_data$day <- rep(1:180,10)

# 2.Intervention: Eliminate mosquitoes in the adult stage ---------------
#form 10% - 90%
result_adult_stage <- c()
control_time <- 31
ua_c <- 0
R_bite <- 0
transmission_time <- 7
R_gamma1  <-  R_gamma <- 1/transmission_time

pro_adult_data <- c()
pro_adult1 <- c()
for (um_c in seq(0,0.9,0.1)) {
  pro_adult <- c()
  for(i in 1:nrow(posterior_samples)){
    parms <- c(parms.fixed, posterior_samples[i,]) 
    k <- parms["k"]
    Np <- 9698900
    Nm <- Np*as.numeric(k)
    init_value <- c(Sa = Nm, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1,Maqua=0 , Madult=0)
    out   <- ode(y   = init_value,
                 times = times,
                 func  = CHIKV_model_with_combind_intervention, #model change
                 parms = parms)
    sim_madult <- diff(out[, "Madult"]) %>%
      c(out[1, "Madult"], .)
    sum_adult <- data.frame(out) %>% 
      mutate(adult = Sm+Em+Im)
    pro_adult0 <- sim_madult/c(1,sum_adult[1:179,"adult"])
    pro_adult <- cbind(pro_adult,pro_adult0)
  }
  pro_adult1 <- data.frame(group = um_c, pro_adult)
  pro_adult_data <- rbind(pro_adult_data,pro_adult1)
}



pro_adult_data <- pro_adult_data %>% 
  mutate(mid = apply(.[,2:101],1,median),
         low   = apply(.[,2:101], 1, quantile, probs = 0.025),
         high  = apply(.[,2:101], 1, quantile, probs = 0.975)) %>% 
  .[,c("group","mid","low","high")]
pro_adult_data$day <- rep(1:180,10)


# 3.plot ------------------------------------------------------------------

#### scenario 1 : single measure plot####
start_date <- as.Date("2025-6-16")
p_line_func_uc <- function(data,title = NULL,x_lab = NULL,ylab = NULL){
  data$group = as.factor(data$group)
  p <- ggplot(data, aes(x = start_date+day-1,group = group,colour = group,fill = group)) +
    geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2,color = NA) +
    geom_line(aes(y = mid),  linewidth = 1) +
    scale_color_npg()+
    labs(title = title,
         y = ylab,
         x = x_lab) +
    theme_bw()+
    theme(
      axis.text.x = element_text(size = 8, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
      axis.text.y = element_text(size = 8, colour = "black",family = "sans"),
      plot.title = element_text(size = 12, face = "bold"),  
      axis.title.x = element_text(size = 8), 
      axis.title.y = element_text(size = 8), 
      legend.text = element_text(size = 8,family = "sans"), 
      legend.position = "right",  
      strip.text = element_text(size = 8, face = "bold"), 
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank()   
    )+
    scale_x_date(date_labels = "%b %d\n2025", date_breaks = "15 days")+
    scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,1,0.1))
  return(p)
}
pro_aquatic_data_plot <- p_line_func_uc(pro_aquatic_data,title = "a",ylab = "Actual daily larval elimination proportion per day")
pro_adult_data_plot <- p_line_func_uc(pro_adult_data,title = "b",ylab = "Actual daily adult mosquito elimination proportion per day")
pro_aquatic_adult_data_plot <- pro_aquatic_data_plot/pro_adult_data_plot
ggsave(filename = "3.result/plot fig/Fig. S14 pro_aquatic_adult_data_plot.png",pro_aquatic_adult_data_plot,width = 17,height = 16,dpi = 400,units = "cm")

