osteoarthritis_incidence <- tibble::tribble(
  ~age,  ~Males, ~Females,
  "0-19",   0.0,    0.0,
  "20–24",  0.54,   0.63,
  "25–29",  0.91,   0.82,
  "30–34",  1.54,   1.55,
  "35–39",  2.55,   2.59,
  "40–44",  4.05,   4.09,
  "45–49",  6.22,   7.32,
  "50–54",   8.1,  12.04,
  "55–59", 11.36,  18.21,
  "60–64", 14.66,  22.48,
  "65–69", 17.59,  26.93,
  "70–74",  20.3,     31,
  "75–79", 23.26,  34.47,
  "80–84", 24.22,  33.77,
  "85–89", 25.77,  33.42,
  "90-110", 25.54,  31.55
) %>% 
  pivot_longer(-1,names_to = 'sex',values_to = 'per1k') %>% 
  mutate(osteoarthritis_year_risk = per1k/1000) %>% 
  select(-per1k)

# current_population %>% select(starts_with('osteoarthritis'))
# apply_osteoarthritis_risk(osteoarthritis_incidence)

apply_osteoarthritis_risk <- function(current_population, osteoarthritis_incidence){
  
  # year = max(current_population$year)
  
  current_population <- current_population %>% 
  select(-any_of('osteoarthritis_year_risk'))
  
  current_population <- current_population %>% 
    mutate(age_ostearthritis = cut(age,breaks = c(-Inf, 20,25,30,35,40,45,50,55,60,65,70,75,80,85,90, Inf),
                                 labels =c("0-19","20–24","25–29","30–34","35–39","40–44","45–49","50–54",
                                           "55–59","60–64","65–69","70–74","75–79","80–84","85–89","90-110")
    )
  ) %>% 
    left_join(osteoarthritis_incidence,by = c('age_ostearthritis' = 'age','sex'='sex'))
  # 
  # current_population <- current_population %>% 
  #   mutate(
  #     osteoarthritis = (runif(n()) < osteoarthritis_year_risk)*year
  #   ) %>% 
  #   select(-any_of(c('age_ostearthritis')))
  
  current_population <- current_population %>% 
    replace_na(list(osteoarthritis_year_risk = 0))
  
}

# initial_time_zero_population <- initial_time_zero_population %>% 
#   select(-any_of('osteoarthritis_year_risk'))
# 
# initial_time_zero_population <- initial_time_zero_population %>% 
#   mutate(age_ostearthritis = cut(age,breaks = c(-Inf, 20,25,30,35,40,45,50,55,60,65,70,75,80,85,90, Inf),
#                                  labels =c("0-19","20–24","25–29","30–34","35–39","40–44","45–49","50–54",
#                                            "55–59","60–64","65–69","70–74","75–79","80–84","85–89","90-110")
#   )
#   ) %>% 
#   left_join(osteoarthritis_incidence,by = c('age_ostearthritis' = 'age','sex'='sex'))
# 
# sum(initial_time_zero_population$osteoarthritis_year_risk)
# initial_time_zero_population <- declare_absolute_incident_morbidity(initial_time_zero_population,'osteoarthritis')
# 
# sum(initial_time_zero_population$osteoarthritis!=0)
