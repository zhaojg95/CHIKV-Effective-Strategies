rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

#library packages
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork")
pacman::p_load(pkgs,character.only = T)

source("2.code/init_value & fixed parameters.R")

init_value <- c(Sa = Nm, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)

calculateRt <- function(control_time = max_day ,beta_mp ,beta_pm, k , Np = 9698900){
  #calculate Rt
  Rt_values_1 = c()
  for (i in 1:control_time) {
    T_shift = 30
    T_period = 365 
    c <- 0.5 * (cos(2 * pi * (i - T_shift) / T_period) + 1)
    # init_value
    list2env(as.list(init_value), envir = .GlobalEnv)
    #  parms.fixed
    list2env(as.list(parms.fixed), envir = .GlobalEnv)
    Nm <- k*Np
    Rt_values_1[i]  = sqrt((Nm * a* beta_mp * beta_pm * e_p *  c *lambda* omega_m  * (gamma1 - gamma1 * q + gamma * q)) /
                              (Np * gamma1 * gamma * (b + omega_m)*(e_p*lambda - e_p + 1 ))) / b
  }
  
  Rt_before_control <- median(Rt_values_1)
}

set.seed(2025)
posterior_samples <- readRDS("3.result/posterior_samples_foshan.rds") %>% 
  .[sample(nrow(.), 100), ]

R0_result <- c()
for (m in 1:nrow(posterior_samples)) {
  max_day = 31
  beta_mp = as.numeric(posterior_samples[m,"beta_mp"])
  beta_pm = as.numeric(posterior_samples[m,"beta_pm"])
  k = as.numeric(posterior_samples[m,"k"])
  R0_result0 <- calculateRt(control_time = max_day ,beta_mp ,beta_pm, k)
  R0_result <- c(R0_result,R0_result0)
}

median(R0_result)
quantile(R0_result,c(0.025,0.975))



