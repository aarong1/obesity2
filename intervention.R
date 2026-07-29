




bmi_down_one_level <- function(current_population,
                               subset = FALSE,
                               subset_name = 'target') {
  if(subset==TRUE){
    
    current_population <- current_population %>%
      mutate(
        bmi = case_when(
          bmi=='obese' & target == TRUE ~ "overweight",
          bmi=='overweight'  & target == TRUE~ "normal",
          TRUE ~ bmi  # Keep original if something unexpected
        )
      )
    
  } else{
    
    current_population <- current_population %>%
      mutate(
        bmi = case_when(
          bmi=='obese' & target == TRUE ~ "overweight",
          bmi=='overweight'  & target == TRUE~ "normal",
          TRUE ~ bmi  # Keep original if something unexpected
        )
      )
  }
  return(current_population)
}


draggable_data <- qread('draggable_data.qs')

intervention_df <- draggable_data[[2]] %>% 
  unlist() %>% 
  matrix( byrow = T,ncol = 2, nrow = 20) %>% 
  as.data.frame(col.names = c('year','effect')) %>% 
  setnames(c('year','effect'))


#-------------------------------------------------
#-------------------------------------------------
#-------------------------------------------------

current_population <- initial_time_zero_population

# Top precentile
current_population %>% 
  mutate(year= 2028) %>% 
  left_join(intervention_df ) %>% 
  mutate(target = bmi_percentile>effect) %>% 
  bmi_down_one_level(subset=T) 

# Top Risk
current_population %>% 
  mutate(year= 2028) %>% 
  left_join(intervention_df ) %>% 
  mutate(rk = rank(qrisk_score,ties.method = 'random')) %>% 
  mutate(risk_percentile = rk/max(rk)) %>% #select(rk,qrisk_score) %>%  View()
  mutate(target = risk_percentile > effect) %>% 
  bmi_down_one_level(subset=T)
  
# Random
current_population %>% 
  mutate(year= 2028) %>% 
  left_join(intervention_df ) %>% 
  mutate(target = ifelse(bmi %in% c('overweight','obese'), 
                         1==rbinom(size = 1,
                                   n=n(), 
                                   prob = (1 - effect)), F)
         ) %>% 
  bmi_down_one_level(subset=T)


apply_bmi_intervention <- function(current_population, mode = c(1,2,3),intervention_df){
  
  current_population <- current_population %>% 
    mutate(year= 2028) %>% 
    left_join(intervention_df ) %>% 
  if(mode == 1){
    # Top precentile
      current_population <-current_population %>% 
      mutate(target = bmi_percentile>effect) %>% 
      bmi_down_one_level(subset=T) 
      
  } else if(mode == 2){
    # Top Risk
    current_population <-current_population %>% 
      mutate(rk = rank(qrisk_score,ties.method = 'random')) %>% 
      mutate(risk_percentile = rk/max(rk)) %>% #select(rk,qrisk_score) %>%  View()
      mutate(target = risk_percentile > effect) %>% 
      bmi_down_one_level(subset=T)
    
  } else if(mode == 3){
    # Random
    
    current_population <- current_population %>% 
      mutate(target = ifelse(bmi %in% c('overweight','obese'), 
                             1==rbinom(size = 1,
                                       n=n(), 
                                       prob = (1 - effect)), F)
      ) %>% 
      bmi_down_one_level(subset=T)
  }
}



