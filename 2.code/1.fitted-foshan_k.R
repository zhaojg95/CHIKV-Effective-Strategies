rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME")
pacman::p_load(pkgs,character.only = T)

# 1. loading data ---------------------------------------------------------

cases <- Foshan_cases <- read.xlsx("1.data/Foshan_cases.xlsx", detectDates = T)

#load model
source("2.code/CHIKV-model.R")

#load initial values and fixed parameters
source("2.code/init_value & fixed parameters.R")

# fitted parameters
set.seed(2025)
parms.fit <- c(beta_mp = 0.26,
               beta_pm = 0.31,
               k = 15)

# 4.fitting model (from June 16, 2025 to July 16, 2025 ) -------------------------------------------------
times <- 1:31

model_cost <- function(p) {
  parms <- c(parms.fixed, p)
  k <- parms["k"]
  Np <- 9698900
  Nm <- as.numeric(Np*k)
  init_value <- c(Sa = Nm, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
  out   <- data.frame(ode(y = init_value, times = times, func = CHIKV_model, parms = parms))
  fitInfected <- diff(out[, "X"]) %>% 
    c(out[1, "X"], .)
  rmse <- RMSE(fitInfected[times],cases[times,"Cases"])
  return(rmse)
}

fit_result <- modFit(f = model_cost,
                     p = parms.fit,
                     method = "L-BFGS-B",
                     lower = c(beta_mp = 0.000001, beta_pm = 0.000001, k = 5),
                     upper = c(beta_mp = Inf, beta_pm = Inf, k = 15))


new_parms <- c(parms.fixed,fit_result$par )

##MCMC##

mcmc_result <- modMCMC(f = model_cost,
                       p = fit_result$par,   # using the result of modFit as the initial value
                       lower = c(beta_pm = 0.000001,beta_mp = 0.000001,k = 5),
                       upper = c(beta_mp = Inf, beta_pm = Inf,k = 15),
                       niter = 1000,
                       burninlength = 200,
                       jump = 0.01)         

#calculating the median and 95%CI of fitted parameters
posterior_samples  <-  mcmc_result$pars
ci_95 <- apply(posterior_samples, 2, quantile, probs = c(0.025,0.5, 0.975));ci_95

#simulating the trend of disease based on the fitted parameters 
sim_cases <- apply(posterior_samples, 1, function(x) {
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

ci_low  <- apply(sim_cases, 1, quantile, probs = 0.025)
ci_high <- apply(sim_cases, 1, quantile, probs = 0.975)
median  <- apply(sim_cases, 1, median) 

plot_df <- data.frame(
  day   = cases$real.time,
  low   = ci_low,
  high  = ci_high,
  mid   = median,
  cases = cases$Cases[times]
)


# plot fig
p <- ggplot(plot_df, aes(x = day)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5,fill= "skyblue") +
  geom_line(aes(y = mid, color = "Fitted"), linewidth = 1.2) +
  geom_point(aes(y = cases, color = "Reported"), size = 1.1) +
  labs(
    y = "Number of reported cases",
    x = "",
    title = " " ,
    fill = "",  
    color = "" 
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 6, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
    axis.text.y = element_text(size = 6, colour = "black",family = "sans"),
    plot.title  = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 7),  
    axis.title.y = element_text(size = 7),  
    legend.title = element_blank(), 
    legend.text = element_text(size = 7,family = "sans"), 
    legend.position = c(0.2, 0.6),  
    legend.key.size = unit(0.8, "cm"),  
    legend.spacing.x = unit(0.8, 'cm'),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()   
  ) +
  scale_color_manual(values = c("Fitted" = "steelblue", "Reported" = "red")) +
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "4 days")

ggsave(filename = "3.result/plot fig/Fig S1. fitted_result.png",p,width = 15,height = 7,dpi = 400,units = "cm")

#Evaluation index
eval_func <- function(y_true,y_pred){
  R2 = MLmetrics::R2_Score(y_pred,y_true)
  RMSE = MLmetrics::RMSE(y_pred,y_true)
  MAPE = MLmetrics::MAPE(y_pred,y_true)
  MAE = MLmetrics::MAE(y_pred,y_true)
  print(plot(y_true,y_pred),
        abline(a = 0, b = 1, lty = 2, col = "red"))
  return(cat("R2 = ",R2,"\n",
             "RMSE = ",RMSE,"\n",
             "MAPE = ",MAPE*100,"%\n",
             "MAE = ",MAE,"\n"))
}

y_true <- cases$Cases[times]
y_pred <- plot_df$mid
eval_func(y_true,y_pred)

saveRDS(mcmc_result, file = "3.result/mcmc_result_foshan.rds")
saveRDS(posterior_samples, file = "3.result/posterior_samples_foshan.rds")
saveRDS(sim_cases, file = "3.result/sim_cases_foshan.rds")


# trends plot new ---------------------------------------------------------

Foshan_cases2 <- read.xlsx("1.data/Foshan_cases.xlsx", sheet = 2,detectDates = T)
names(Foshan_cases2)[3] <-  "Date"
dates <- seq(from = as.Date("2025-06-16"), to = as.Date("2025-08-16"), by = "8 days")
p1 <- ggplot(Foshan_cases2, aes(x = Date)) +
  geom_bar(aes(y = Cases), stat = "identity", fill = "red", color = "black",alpha = 0.5) +
  scale_fill_manual(values = c("red")) +  
  labs(
    y = "Number of reported cases",
    x = " ",
    title = " "
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white"),  
    panel.background = element_rect(fill = "white"),  
    axis.text.x = element_text(size = 6, colour = "black", family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),  
    axis.text.y = element_text(size = 6, colour = "black", family = "sans"),
    plot.title  = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 10), 
    axis.title.y = element_text(size = 7),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 7, family = "sans"),  
    legend.position = "none",  
    axis.ticks = element_line(color = "black"),  
    axis.ticks.length = unit(0.2, "cm"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()   
  )+
  scale_y_continuous(limits = c(0,1000))+
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "4 days",limits = c(as.Date("2025-06-15"),as.Date("2025-08-18")));p1

ggsave(filename = "3.result/plot fig/Fig.1 trend_result.png",p1,width = 15,height = 8,dpi = 400,units = "cm")


