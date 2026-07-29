# see equations 

# Prevalence and factors associated with NAFLD detected by vibration controlled transient elastography among US adults: Results from NHANES 2017–2018
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252164#pone.0252164.s003 




apply_nafld_risk <- function(input_population){
  
  nafld_incidence <- tibble::tribble(
    ~sex, ~age_group, ~nafld_year_risk,
    "Females", "18-34",       0.0000174,
    "Females", "35-44",       0.0000651,
    "Females", "45-54",       0.000147,
    "Females", "55-64",       0.000189,
    "Females", "65-74",       0.000205,
    "Females", "75-84",       0.000208,
    "Females", "85-110",      0.000139,
    "Males",   "18-34",       0.0000249,
    "Males",   "35-44",       0.0000932,
    "Males",   "45-54",       0.000210,
    "Males",   "55-64",       0.000271,
    "Males",   "65-74",       0.000293,
    "Males",   "75-84",       0.000299,
    "Males",   "85-110",      0.000199
  ) %>% 
    select(age_nafld = age_group, 
           sex = sex,
           nafld_year_risk )
  
  year = max(input_population$year)
  
  input_population <- input_population %>% 
    filter(year == min(year)) %>% 
    mutate(age_nafld = cut(age,breaks = c(-Inf,18, 35,45,55,65,75,85, Inf),
                          labels = c('0-18','18-34','35-44','45-54','55-64','65-74','75-84','85-110')
    )) %>%
    select(-any_of('nafld_year_risk'))
  
  input_population <- input_population %>% left_join(nafld_incidence, by = c(  'age_nafld' , 'sex'))
  
  input_population <- input_population %>% select(- c(age_nafld))
  
  input_population <- input_population %>% 
    replace_na(list(nafld_year_risk = 0)) 
}
