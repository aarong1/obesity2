message('done pre main 36')

source('./3_pre_main/pre_main_35_deaths.R')
source('./3_pre_main/pre_main_deaths_15.R')
source('./3_pre_main/main_deaths_dt.R')

time_one_population <- read.fst('./3_pre_main/intermediate_populations/time_one_population.fst')

time_one_population <- time_one_population %>% 
  mutate(age_band_death =
           cut(age, include.lowest = T,
               breaks = c(-Inf, 0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
               labels = c('0', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39' ,'40-44' ,'45-49' ,'50-54' ,'55-59' ,'60-64', '65-69' ,'70-74', '75-79' ,'80-84', '85-89' ,'90+')
           )) %>%
  left_join(ages_df)

time_one_population <- time_one_population %>% 
  apply_deaths_modelled_deaths(other_deaths_df = other_deaths_df, fatality_wide = fatality_wide) #%>% 
  # count(death_reason)

time_one_population_w_deaths <- time_one_population 

write.fst(time_one_population_w_deaths,
          paste0('./3_pre_main/intermediate_populations/time_one_population_w_deaths.fst'))


message('done pre main 36')


"chd"
"chronic_kidney_disease"
"dementia"
"diabetes"
"heart_failure"
"stroke"
"asthma"
"colorectal_cancer"
"copd"

"female_breast_cancer"
"lung_cancer"
"ovarian_cancer"
"pancreatic_cancer"
"prostate_cancer"
"renal_cancer"
"uterine_cancer"

"other"
"survive"

# initial_time_zero_population <- initial_time_zero_population %>% 
#   apply_age_sex_death(apply_death = T) 

# initial_time_zero_population <- initial_time_zero_population %>%
#   
#   # apply_case_death( morbidity = 'cancer') %>%
#   apply_case_death( morbidity = 'stroke') %>%
#   apply_case_death( morbidity = 'chd') %>%
#   
#   apply_case_death( morbidity = 'diabetes') %>%
#   apply_case_death( morbidity = 'asthma') %>%
#   apply_case_death( morbidity = 'copd') %>%
#   
#   apply_case_death( morbidity = 'chronic_kidney_disease') %>%
#   apply_case_death( morbidity = 'dementia') %>%
#   apply_case_death( morbidity = 'heart_failure')
# 
# initial_time_zero_population <- initial_time_zero_population %>%
#   apply_case_death( morbidity = 'lung_cancer') %>%
#   apply_case_death( morbidity = 'colorectal_cancer') %>%
#   apply_case_death( morbidity = 'oral_cancer') %>%
#   apply_case_death( morbidity = 'pancreatic_cancer') %>%
#   apply_case_death( morbidity = 'uterine_cancer') %>%
#   # apply_case_death( morbidity = 'blood_cancer') %>%
#   apply_case_death( morbidity = 'ovarian_cancer') %>%
#   
#   # apply_case_death( morbidity = 'cervical_cancer') %>%
#   # apply_case_death( morbidity = 'brain_cancer') %>%
#   
#   # apply_case_death( morbidity = 'osteogastric_cancer') %>%
#   
#   apply_case_death( morbidity = 'prostate_cancer') %>%
#   apply_case_death( morbidity = 'female_breast_cancer') %>%
#   apply_case_death( morbidity = 'renal_cancer')

# "lung_cancer",
# "colorectal_cancer",
# "oral_cancer",
# "pancreatic_cancer",
# "uterine_cancer",
# "ovarian_cancer",

# "prostate_cancer",
# "female_breast_cancer",
# "renal_cancer"

