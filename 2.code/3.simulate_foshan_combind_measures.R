rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes")
pacman::p_load(pkgs,character.only = T)

#load model
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

# 1. Simulation without intervention measures -----------------------------

sim_cases <- apply(posterior_samples, 1, function(x) {
  parms <- c(parms.fixed, as.list(x))   
  parms <- c(parms.fixed, as.list(x)) 
  k <- parms["k"]
  Np <- 9698900
  Nm <- Np*as.numeric(k)
  init_value <- c(Sa = Nm, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
  out   <- ode(y   = init_value,
               times = times,
               func  = CHIKV_model,
               parms = parms)
  simulate_cases <- diff(out[, "X"]) %>%
    c(out[1, "X"], .)                                  
})

sum_sim_cases <- apply(sim_cases,MARGIN = 2, sum)

df <- data.frame(
  day   = times,
  intervention = NA,
  low   = apply(sim_cases, 1, quantile, probs = 0.025),
  high  = apply(sim_cases, 1, quantile, probs = 0.975),
  mid   = apply(sim_cases, 1, median) ,
  total_low = as.numeric(quantile(sum_sim_cases,probs = 0.025)),
  total_high  = as.numeric(quantile(sum_sim_cases,probs = 0.975)),
  total_mid   = as.numeric(quantile(sum_sim_cases,probs = 0.5)) ,
  type  = "without invervention"
)

result_without_intervention <- df

saveRDS(result_without_intervention, file = "3.result/result_without_intervention.rds")  

# 2.Simulation with interventions ---------------------------------

ua_c <- seq(0,0.9,0.1)   # ua_c: larval elimination, LE

um_c <- seq(0,0.9,0.1)  # um_c: adult mosquito elimination, AME

R_bite <- seq(0,0.9,0.1) #R_bite: mosquito bite rate reduction, BRR

transmission_time <- 1:7 # transmission_time: infectious period shortening, IPS

control_time <- seq(3,31,2) # control_time: initial intervention time

combind_intervention_measures_control_time <- expand.grid(ua_c,um_c,R_bite,transmission_time,control_time) %>% 
  as.data.frame()
names(combind_intervention_measures_control_time) <- c("ua_c","um_c","R_bite","transmission_time","control_time")

result_combind_measures_control_time <- c()

t_total <- 0 
for (i in 1:nrow(combind_intervention_measures_control_time)) {
  t_start <- Sys.time()
  ua_c <- combind_intervention_measures_control_time[i,"ua_c"]
  um_c <- combind_intervention_measures_control_time[i,"um_c"]
  R_bite <- combind_intervention_measures_control_time[i,"R_bite"]
  transmission_time <- combind_intervention_measures_control_time[i,"transmission_time"]
  R_gamma1  <-  R_gamma <- 1/transmission_time
  control_time <- combind_intervention_measures_control_time[i,"control_time"]
  
  sim_cases <- apply(posterior_samples, 1, function(x) {
    parms <- c(parms.fixed, as.list(x))        
    k <- parms["k"]
    Np <- 9698900
    Nm <- Np*as.numeric(k)
    init_value <- c(Sa = Nm, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1,Maqua=0 , dMadult=0)
    out   <- ode(y   = init_value,
                 times = times,
                 func  = CHIKV_model_with_combind_intervention, #model change
                 parms = parms)
    simulate_cases <- diff(out[, "X"]) %>%
      c(out[1, "X"], .)
  }) 
  
  sum_sim_cases <- apply(sim_cases,MARGIN = 2, sum)
  
  df <- data.frame(
    day   = times,
    ua_c = combind_intervention_measures_control_time[i,"ua_c"],
    um_c = combind_intervention_measures_control_time[i,"um_c"],
    R_bite = combind_intervention_measures_control_time[i,"R_bite"],
    transmission_time = combind_intervention_measures_control_time[i,"transmission_time"],
    control_time = combind_intervention_measures_control_time[i,"control_time"],
    low   = apply(sim_cases, 1, quantile, probs = 0.025),
    high  = apply(sim_cases, 1, quantile, probs = 0.975),
    mid   = apply(sim_cases, 1, median),
    total_low = as.numeric(quantile(sum_sim_cases,probs = 0.025)),
    total_high  = as.numeric(quantile(sum_sim_cases,probs = 0.975)),
    total_mid   = as.numeric(quantile(sum_sim_cases,probs = 0.5))
  )
  
  result_combind_measures_control_time <- rbind(result_combind_measures_control_time,df)
  
  t_end <- Sys.time()
  t_total <- as.numeric(t_total + t_end - t_start, units = "secs")
  cat(sprintf("Completed the %d-th run, took %.2f seconds\n", i, t_total))
}


#save results
saveRDS(result_combind_measures_control_time, file = "3.result/result_combind_measures_control_time.rds")  

source("2.code/result_sim.R")
result_combind_measures_control_time_summary <- result_sim3(result_combind_measures_control_time)
saveRDS(result_combind_measures_control_time_summary, file = "3.result/result_combind_measures_control_time_summary.rds") 




