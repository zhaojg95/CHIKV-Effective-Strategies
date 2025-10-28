rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes","patchwork","metR","ggpubr")
pacman::p_load(pkgs,character.only = T)

# 1.loading results -------------------------------------------------------
source("2.code/result_sim.R")
result_without_intervention <- readRDS("3.result/result_without_intervention.rds") 
result_combind_measures_control_time <- readRDS("3.result/result_combind_measures_control_time.rds")
result_combind_measures_control_time_summary <- result_sim3(result_combind_measures_control_time)
Foshan_cases <- read.xlsx("1.data/Foshan_cases.xlsx", detectDates = T,sheet = 2)

times <- 1:180

result_combind_measures_control_time <- result_combind_measures_control_time %>% 
  filter(control_time == 31,day <= 62,ua_c > 0,um_c > 0,R_bite > 0 ,transmission_time<7) %>% 
  mutate(type = paste0('A=',ua_c,', B=',um_c,', C=',R_bite,', D=',transmission_time)) 
RMSE <- result_combind_measures_control_time %>% 
  group_by(type) %>% 
  filter(day > 31) %>% 
  summarise(rmse = round(RMSE(mid,Foshan_cases$Cases[-c(1:31)]),2),R2 = round(R2_Score(mid,Foshan_cases$Cases[-c(1:31)]),2)) %>% 
  filter(R2 >= 0.7) %>% 
  arrange(desc(R2))


# 2. plot fig -------------------------------------------------------------

data <- subset(result_combind_measures_control_time, type %in% RMSE$type) %>% 
  mutate(cases = rep(Foshan_cases$Cases,nrow(RMSE))) %>% 
  left_join(RMSE,by = "type") %>% 
  arrange(desc(R2))

data$type <- factor(data$type,levels = RMSE$type)
data$type1 <- rep(letters[1:24],each = 62)

data$day <- as.Date("2025-6-16")+data$day-1
fitted <- ggplot(data, aes(x = day)) +
  geom_col(aes(y = cases, fill = ifelse(day > as.Date("2025-7-16"), "Blue cases", "Reported cases")),size = 1,alpha = 1) +
  geom_vline(xintercept = 31, linetype = "dashed", color = "grey90")+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.6, fill = "pink") +
  scale_fill_manual(values = c("Reported cases" = "#E64B35FF", "Blue cases" = "#3C5488FF")) +
  geom_line(aes(y = mid), linewidth = 0.6,alpha = 0.8, color = "black") +
  labs(y = "Daily cases", x = " ") +
  theme_bw() +
  facet_wrap(~ type1, ncol = 4) + 
  theme(
    axis.text.x = element_text(size = 6, colour = "black", vjust = 0.5, hjust = 0.5, angle = 0,family = "sans"),
    axis.text.y = element_text(size = 9, colour = "black",family = "sans"),
    plot.title = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.position = "",
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_blank(),  
    strip.title = element_text(size = 8, face = "bold"),
    strip.background = element_rect(fill = "lightblue"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "15 days")+
  geom_text(aes(label = paste("RMSE:", round(rmse, 3),"\nR²:", round(R2, 3)),  x = as.Date("2025-6-16")+80, y = 480),
            fontface = "italic",family = "sans",
            hjust = 1.1, vjust = 1.1, size = 2.0, color = "black", inherit.aes = FALSE)  
fitted

ggsave(filename = "3.result/plot fig/Fig.6.png",fitted,width = 18,height = 12,dpi = 800,units = "cm")


