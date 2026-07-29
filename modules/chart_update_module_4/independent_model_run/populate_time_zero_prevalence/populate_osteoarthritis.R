#https://www.sciencedirect.com/science/article/pii/S1063458409002829

osteoarthritis_prevalence <- tibble::tribble(
  ~age_group, ~Males, ~Females,
      "0-19",  0,    0,
      "20-29",  0.1,    0.1,
      "30-39",  0.9,    0.9,
      "40-49",  2.9,      3,
      "50-59",  7.9,    8.8,
      "60-69", 16.8,   22.2,
      "70-79", 29.2,   39.8,
      "80-89",   43,   56.1,
      "90-110", 54.8,   68.2) %>% 
    pivot_longer(-1,names_to = 'sex',values_to = 'prob') %>% 
    mutate(osteoarthritis_prevalence_prob= prob/100) %>% 
  select(-prob)

apply_osteoarthritis_prevalence <- function(current_population){
  
  year  = min(current_population$year)
  
  current_population <- current_population %>% 
    mutate(age_group = cut(age,breaks = c(-Inf,20,30,40,50,60,70,80,90, Inf),
                           labels =c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-110")
    )) %>%
    group_by(age,sex) %>% 
    mutate(osteoarthritis_percentile = frank(ties.method = 'random',osteoarthritis_year_risk)/max(frank(ties.method = 'random',osteoarthritis_year_risk))) %>% 
    left_join(osteoarthritis_prevalence) %>% 
    mutate(osteoarthritis = ifelse(runif(n()) < osteoarthritis_prevalence_prob/0.5 * osteoarthritis_percentile ,year,0) )
}
