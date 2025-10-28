
result_sim3 <- function(data){
  #Calculate the total number of cases, the peak value and time, the duration of outbreak
  
  total_cases  <-  data %>% 
    group_by(data[2:6]) %>% 
    summarise(total_cases_mid = mean(total_mid),
              total_cases_low = mean(total_low),
              total_cases_high = mean(total_high))%>% 
    mutate(totalcases_reduce_proportion = round((mean(result_without_intervention$total_mid) - total_cases_mid)/mean(result_without_intervention$total_mid)*100,2))
  
  peak_time  <-  data %>% 
    group_by(data[2:6]) %>% 
    summarise(peak_value = max(mid, na.rm = T),
              peak_value_low = low[which(mid == peak_value)], 
              peak_value_high = high[which(mid == peak_value)],
              peak_time = day[which.max(mid)])%>% 
    mutate(peakvalue_reduce_proportion = round((max(result_without_intervention$mid)-peak_value)/max(result_without_intervention$mid)*100,2))
  
  DO <- data %>%                      
    group_by(data[2:6]) %>% 
    arrange(day) %>%                    
    summarise(
      start_day = first(day),            
      peak_day  = day[which.max(mid)],   
      end_day   = day[day > peak_day & mid < 1][1],  
      duration  = end_day - start_day,   
      max_day   = max(day),
      .groups = "drop"
    ) %>%
    mutate(
      duration = replace(duration, is.na(duration), unique(max_day))
    )
  grp_cols <- names(data)[2:6]
  result_bind <- total_cases %>% 
    left_join(peak_time,by = grp_cols) %>% 
    left_join(DO,by = grp_cols)
  
  return(result_bind)
}

