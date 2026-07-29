#main_depression
library(fst)
library(tidyverse)
library(readxl)
library(echarts4r)
library(data.table)
select <- dplyr::select
pop <- read.fst('./main/initial_time_zero_population10down.fst')

ppp <- read_xlsx(path =  'data/ni/ni_ppp_machine_readable.xlsx', sheet = 'Mortality_assumptions')

names(ppp)[-c(1,2)] <- str_extract( pattern = '[0-9]*', string = names(ppp)[-c(1,2)])

ppp <- ppp %>% 
  select(sex = Sex, age = Age, q = `2022`) %>% 
  mutate(q = q/100000) %>% 
  mutate(age = as.numeric(age))

source('./risk_exposure_prevalence/wellbeing_depression/apply_wellbeing_depression_lifestyle_parameter_rank_stability_long_form.R')
source('./reindex_risk_percentile.R')
source('./risk_joint_estimation/smoking.R') #check
source('./risk_joint_estimation/alcohol.R') #check
source('./risk_joint_estimation/adult_obesity.R') #check
source('./risk_joint_estimation/PA.R') 
source('./risk_joint_estimation/diabetes.R')
source('./main/main_utils_2_4.R')

crc_incidence <- tribble(
  ~age_band_incidence, ~Males, ~Females,
  '0 to 34',   1.0,  1.9, 
  '35 to 39',   9.1,  8.9, 
  '40 to 44',   18.1,  12.9, 
  '45 to 49',   30.9,  29.6, 
  '50 to 54',   47.4,  45.1, 
  '55 to 59',   92.8,  68.5, 
  '60 to 64',   181.1,  110.5, 
  '65 to 69',   213.2,  133.7, 
  '70 to 74',   294.9,  178.4, 
  '75 to 79',   345.5,  229.8, 
  '80 to 84',   501.7,  303.5, 
  '85 to 89',   526.3,  362.9, 
  '90 and over',   438.9,  332.4
) %>% 
  pivot_longer(cols = -age_band_incidence, names_to = 'sex', values_to = 'incidence')%>%
  mutate(crc_prob = incidence/100000)


( crc_mortality <- tribble(
  ~age_band_mortality, ~Males, ~Females,
  '0 to 44', 0.8, 0.7,
  '45 to 49', 7.9, 7.0,
  '50 to 54', 13.3, 10.2,
  '55 to 59', 21.7, 11.8,
  '60 to 64', 42.3, 30.3,
  '65 to 69', 62.3, 33.7,
  '70 to 74', 86.9, 50.4,
  '75 to 79', 133.7, 81.4,
  '80 to 84', 226.4, 124.3,
  '85 to 89', 332.5, 227.6,
  '90 and over', 478.8, 365.5
) %>% 
    pivot_longer(cols = -age_band_mortality, names_to = 'sex', values_to = 'incidence')
  %>% mutate(prob_mort = incidence/100000)
)

crc_prevalence <- tibble::tribble(
  ~age_band_prevalence, ~sex,  ~`10yr`, ~`25yr`,
  '0 to 54', 'Males', 40.7, 48.8, 
  '55 to 64', 'Males', 579.4, 709.1, 
  '65 to 74', 'Males', 1284.6, 1742.8, 
  '75 and over', 'Males', 2242.7, 3749.3, 
  '0 to 54', 'Females', 46.7, 54.5,
  '55 to 64', 'Females', 408.6, 531.6,
  '65 to 74', 'Females', 836.8, 1231.1,
  '75 and over', 'Females', 1439.3, 2426.8
) %>% 
  mutate(`10yr` = `10yr`/ 100000,
         `25yr` = `25yr`/ 100000
  ) %>%
  pivot_longer(-c(1,2))


# Females
f_crc_deaths <- tibble::tribble(
  ~sex, ~type,  ~All.Ages, ~`0`, ~`1-4`, ~`5-9`, ~`10-14`, ~`15-24`, ~`25-34`, ~`35-44`, ~`45-54`, ~`55-64`, ~`65-74`, ~`75-84`, ~`85-89`, ~`90+`,
  'Females', 'colon',    13L,  0L,  0L,  0L,  0L,  0L,  0L,  0L,  4L,  11L,  36L,  89L, 128L, 292L,
  'Females', 'rectum',    15L,  0L,  0L,  0L,  0L,  1L,  0L,  4L,  8L,  22L,  44L,  71L, 110L, 180L
)

# Males
m_crc_deaths <- tibble::tribble(
  ~sex, ~type,   ~All.Ages, ~`0`, ~`1-4`, ~`5-9`, ~`10-14`, ~`15-24`, ~`25-34`, ~`35-44`, ~`45-54`, ~`55-64`, ~`65-74`, ~`75-84`, ~`85-89`, ~`90+`,
  'Males', 'colon',  12L,  0L,  0L,  0L,  0L,  0L,  0L,  2L,  1L,  13L,  25L,  62L, 115L, 191L,
  'Males', 'rectum',  13L,  0L,  0L,  0L,  0L,  0L,  0L,  2L, 10L,  16L,  25L,  51L,  79L, 201L
)

crc_deaths <-  rbind( f_crc_deaths, m_crc_deaths) %>% 
  group_by(sex) %>% 
  summarise(across(is.numeric, sum)) %>% 
  pivot_longer(-1,names_to = 'age_band_deaths', values_to = 'crc_deaths_rate') %>% 
  filter(age_band_deaths != 'All.Ages')




all_deaths_males <- tibble::tribble(
  ~`sex`, ~All.Ages, ~`0`, ~`1-4`, ~`5-9`, ~`10-14`, ~`15-24`, ~`25-34`, ~`35-44`, ~`45-54`, ~`55-64`, ~`65-74`, ~`75-84`, ~`85-89`, ~`90+`,
  "Males",      907L, 472L,     4L,    10L,       5L,      51L,     101L,     139L,     332L,     712L,    1827L,    5141L,   12764L, 24753L
)

all_deaths_females <- tibble::tribble(
  ~`sex`, ~All.Ages, ~`0`, ~`1-4`, ~`5-9`, ~`10-14`, ~`15-24`, ~`25-34`, ~`35-44`, ~`45-54`, ~`55-64`, ~`65-74`, ~`75-84`, ~`85-89`, ~`90+`,
  "Females",      891L, 316L,     0L,     3L,       3L,      29L,      39L,      77L,     245L,     540L,    1307L,    3792L,   10181L, 21380L
)

all_deaths <- rbind(
  all_deaths_males,
  all_deaths_females) %>%  
  group_by(sex) %>% 
  summarise(across(is.numeric, sum)) %>% 
  pivot_longer(-1,names_to = 'age_band_deaths', values_to = 'all_deaths_rate') %>% 
  filter(age_band_deaths != 'All.Ages')


# mutate(last15yr = `25yr` - `10yr`) %>% 
# mutate(cumdist =  c(last15yr, `10yr`) )

# mutate(`25yr` = `25yr` - `10yr`) #%>% 


# bowel_risks <- read_csv("bowel_risks.csv")
# 
# #Alcohol
# bowel_risks %>% filter(str_starts(string= risk, 'Alco') ) %>% 
# mutate(alcohol=recode_values(category,
# '60 g/day' ~ 'higher_risk',
# '36 g/day' ~ 'increased_risk',
# '12 g/day' ~ 'low_risk',
# '0 g/day' ~ 'no_risk')) %>% #View
#   select(age = Age,rr,alcohol,sex)
# 
# #Smoking
# bowel_risks %>% 
#   filter(str_starts(string= risk, 'Smoki') ) %>% 
#   mutate(smoking=recode_values(category,
#                                '0 Pack Years' ~ 'never_smoked',
#                                '20 Pack Years' ~ 'current_smoker',
#                                ' ' ~ ' ')) %>% 
#   filter(!is.na(smoking)) %>% View
#   select(age = Age,rr,smoking,sex)
#   
# #Diabetes
# bowel_risks %>% 
#   filter(str_starts(string = risk, 'High fasting') ) %>% 
#   mutate(diabetes = recode_values(category,
#                                'Diabetic' ~ 'diagnosed_diabetes',
#                                'Not diabetic' ~ 'no_diabetes')) %>% 
#   filter(!is.na(diabetes)) %>% View
#   select(age = Age,rr,diabetes,sex)
# 
# #PA
# bowel_risks %>% 
#   filter(str_starts(string = risk, 'Low') ) %>% 
#   mutate(pa = recode_values(category,
#                                   '0 METs' ~ 'inactive',
#                                   '1800 METs' ~ 'low_activity',
#                                   '2400 METs' ~ 'some_activity',
#                                   '3600 METs' ~ 'meets_rec')) %>% 
#   filter(!is.na(pa)) %>% 
#   select(age = Age,rr,pa,sex)
# 
# #BMI
# bowel_risks %>% 
#   filter(str_starts(string= risk, 'High body') ) %>% #View()
#   select(age = Age,rr,sex)
# 
# Alcohol
# 1.468, higher_risk,   
# 1.237, increased_risk,
# 1.078, low_risk,      
# 1.0,   no_risk
# 
# PA
# 1.0, inactive,     
# 0.933, low_activity, 
# 0.883, some_activity,
# 0.831, meets_rec
# 
# Smoking
# Both 1.527
# 
# Diabetes
# Both 1.527
# 
# BMI
# Males 1.177
# Females 1.059

pop <- pop %>%   
  mutate(
  crc_rr_obese = case_when(
    bmi == 'overweight' & sex == 'Males' ~ 1.177,
    bmi == 'overweight' & sex == 'Females' ~ 1.059,
    bmi == 'obese' & sex == 'Males' ~ 1.177^3,
    bmi == 'obese' & sex == 'Females' ~ 1.059^3,
    bmi == 'normal' ~ 1 ,
    T~1
    )) %>% 
  mutate(
    crc_rr_diabetes = 1.527) %>% 
  mutate(
    crc_rr_smoking = 1.527) %>% 
  mutate(
    crc_rr_active = case_when(
         pa == 'inactive' ~ 1.0,     
         pa == 'low_activity' ~ 0.933, 
         pa == 'some_activity' ~ 0.883,
         pa == 'meets_rec' ~ 0.831,
         T~1)
    ) %>% 
  mutate(
    crc_rr_alcohol = 
           case_when(
             alcohol == 'higher_risk' ~ 1.468 ,   
             alcohol == 'increased_risk' ~ 1.237 ,
             alcohol == 'lower_risk' ~ 1.078 ,      
             alcohol == 'no_risk' ~ 1.0   ,
             T~1)
         ) #%>% count(crc_rr_active, pa)

# all cause mortality
# obese * 1.20 + overweight * 1.07 + normal * 1  )
# diabetes 1.7
# smoking 3.7
# active 1.86
# alcohol 1.36

pop <- pop %>% 
  mutate(all_cause_rr_obese = case_when(bmi == 'obese' ~ 1.20,
                                        bmi == 'overweight' ~ 1.07,
                                        bmi == 'normal' ~ 1  )) %>% 
  mutate(all_cause_rr_diabetes = ifelse(diabetes_status != 'no_diabetes',1.7,1) ) %>% 
  mutate(all_cause_rr_smoking = ifelse(smoking == 'current_smoker',3.7,1) ) %>% 
  mutate(all_cause_rr_active = ifelse(pa != 'meets_recommendations',1.86,1) ) %>% 
  mutate(all_cause_rr_alcohol = ifelse(alcohol %in% c('increased_risk','higher_risk'),1.36,1) )


pop <- pop %>% 
  mutate(age_band_incidence = cut(age, 
                                 breaks = c(-Inf, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                                 
                                 labels =c( '0 to 34',
                                            '35 to 39',
                                            '40 to 44',
                                            '45 to 49',
                                            '50 to 54',
                                            '55 to 59',
                                            '60 to 64',
                                            '65 to 69',
                                            '70 to 74',
                                            '75 to 79',
                                            '80 to 84',
                                            '85 to 89',
                                            '90 and over')
                         ))  %>% 
  mutate(age_band_mortality = cut(age, 
                                  breaks = c(-Inf,  44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                                  labels =c( '0 to 44',
                                             '45 to 49',
                                             '50 to 54',
                                             '55 to 59',
                                             '60 to 64',
                                             '65 to 69',
                                             '70 to 74',
                                             '75 to 79',
                                             '80 to 84',
                                             '85 to 89',
                                             '90 and over')
  )) %>% 
  mutate(age_band_prevalence = cut(age, 
                                  breaks = c(-Inf,  54,  64,  74,  Inf),
                                  labels =c( '0 to 54',
                                             '55 to 64',
                                             '65 to 74',
                                             '75 and over')
  ))


pop <- pop %>% 
  mutate(age_band_deaths = 
           cut(age, 
               labels =c('0', '1-4' , '5-9' , '10-14' , '15-24' , '25-34' , '35-44' , '45-54' , '55-64' , '65-74' , '75-84' , '85-89' , '90+' ),
               breaks = c(-Inf, 1, 4, 9, 14, 24, 34, 44, 54, 64, 74, 84, 89, Inf)
           )) 

# count(pop,age,age_band_all_cause)

pop <- pop %>% 
  mutate(crc_percentile = runif(n())) %>% 
  left_join(crc_prevalence,
            relationship = 'many-to-one',
            multiple = 'first',
            by = join_by('sex', 
                         'age_band_prevalence',  
                         crc_percentile<value))
  
pop %>% 
  count(age_band_prevalence,sex,name) %>% 
  add_count(age_band_prevalence,sex,wt=n) %>% 
  mutate(round(n/nn*1000)/1000)

# (
#   crc_case_fatality <- 
#   pop %>% 
#   count(age_band_mortality,sex,name) %>% 
#   add_count(age_band_mortality,sex,wt=n) %>% 
#   left_join(crc_mortality) %>% 
#   filter(name == '10yr') %>% 
#   mutate(deaths=nn*prob_mort) %>% 
#   mutate(cf = deaths/n)
#   )

(
  crc_case_fatality <- 
    pop %>% 
    count(age_band_deaths,sex,name) %>% 
    add_count(age_band_deaths,sex,wt=n) %>% 
    mutate(n=n * 10, nn = nn * 10) %>% 
    left_join(crc_deaths) %>% 
    filter(name == '10yr') %>% 
    mutate(crc_deaths = crc_deaths_rate/100000 * nn) %>% 
    # mutate(deaths=nn*prob_mort) %>% 
    mutate(cf = crc_deaths/n)
)





pop <- pop %>% 
  left_join(crc_mortality, by=c('age_band_mortality', 'sex')) %>% 
  left_join(crc_deaths, by=c('age_band_deaths', 'sex')) %>% 
  left_join(all_deaths, by=c('age_band_deaths', 'sex')) %>% 
  left_join(crc_case_fatality[c('age_band_deaths', 'sex','cf')], by=c('age_band_deaths', 'sex')) 
  
  
order_age_band_deaths <- function(df){
df %>% 
    mutate( age_band_deaths = factor(age_band_deaths, 
                                     levels = c('0','1-4','5-9','10-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85-89','90+')
                                     ))
}

 pop %>%
  count(age_band_deaths,sex,all_deaths_rate) %>% 
  mutate(all_deaths = all_deaths_rate/1e5*n*10) %>% 
  right_join(crc_case_fatality,by=c('age_band_deaths', 'sex')) %>% 
   ggplot() +  
   geom_line(aes(age_band_deaths, all_deaths, group=sex), col = 'blue') +
   geom_line(aes(age_band_deaths, crc_deaths, group=sex), col = 'red') 
   
pop %>%
   count(age_band_deaths,sex,all_deaths_rate,crc_deaths_rate,cf) %>% 
     order_age_band_deaths() %>% 
   mutate(all_deaths_rate = all_deaths_rate/1e5) %>%
   mutate(crc_deaths_rate = crc_deaths_rate/1e5) %>%
   ggplot() +  
   geom_line(aes(age_band_deaths, all_deaths_rate, group=sex, lty =sex), col = 'blue') +
   geom_line(aes(age_band_deaths, crc_deaths_rate, group=sex, lty =sex), col = 'red') +
   geom_line(aes(age_band_deaths, cf, group=sex, lty =sex), col = 'green')

# prob_to_rate = 1 - log(1 - prob/ 1)
# rate_to_prob=  1 - exp(-rate*time)
# odds_to_prob = odds / (1 + odds)
# HazardByRate = 1 - (1 - value) ^ hazard_ratio
# value =  hazard ( or survival)

all_cause_paf <- pop %>% 
  group_by(age_band_incidence, sex) %>% 
  summarise(paf_all_cause= 1 - (n()/ sum(
            all_cause_rr_obese *
            all_cause_rr_diabetes *
            all_cause_rr_smoking *
            all_cause_rr_active *
            all_cause_rr_alcohol)
            )) %>% 
  replace_na(list(paf_all_cause = 0))

crc_paf <- pop %>% 
  group_by(age_band_incidence, sex) %>% 
  summarise(paf_crc = 1 - (n()/ sum(  crc_rr_obese *
                                 crc_rr_diabetes *
                                 crc_rr_smoking *
                                 crc_rr_active *
                                 crc_rr_alcohol)
  )
  ) %>% 
  replace_na(list(paf_crc = 0))


pop <- pop %>% 
  left_join(ppp) %>% 
  left_join(all_cause_paf) %>% 
  # left_join(crc_deaths)
  
  mutate(TM_all_cause = q*(1 - paf_all_cause)) %>% 
  mutate(TM_all_cause = all_deaths_rate/100000*(1 - paf_all_cause)) %>% 
  
  left_join(crc_incidence, by = join_by(sex, age_band_incidence)) %>% 
  left_join(crc_paf) %>% 
  mutate(TM_crc =  crc_prob * (1 - paf_crc))


count(pop, sex, age_band_incidence, TM_crc)
count(pop, sex, age_band_incidence, TM_all_cause)

pop <- pop %>% 
  mutate(  crc_year_risk =  TM_crc * 
            crc_rr_obese *
            crc_rr_diabetes *
            crc_rr_smoking *
            crc_rr_active *
            crc_rr_alcohol) %>% 
  
  mutate(  q_prob =  TM_all_cause * 
            all_cause_rr_obese *
            all_cause_rr_diabetes *
            all_cause_rr_smoking *
            all_cause_rr_active *
            all_cause_rr_alcohol) 

 # pop <- pop %>% 
 #  left_join(crc_case_fatality,
 #            by = c('age_band_mortality', 'sex') 
 #            ) 
 
 # pop <- pop %>% 
 #   left_join(crc_case_fatality,
 #             by = c('age_band_deaths', 'sex') 
 #   ) 

pop %>% 
  ggplot()+
  geom_line(aes(age, cf, lty = sex), color = 'blue')+
  geom_line(aes(age, q, lty = sex), color='green')+
  geom_line(aes(age, all_deaths_rate/100000, lty = sex), color='red')+
  geom_line(aes(age, q_prob, lty = sex), color='yellow')




pop %>% 
  mutate(crc_recovered = 0) %>% 
  replace_na(list(name = '0')) %>%
  mutate(crc  = name) %>% 
  declare_absolute_incident_morbidity_alt('crc') %>% 
  count(crc)


if cf > q
sample(c('crc_death', 'survival'), size = nrow(pop), replace = TRUE, prob =c(cf, 1-cf)
       else 
         sample(c('crc_death','other_death','survival'), size = nrow(pop), replace = TRUE, prob =c(cf, q_prob-cf,1-q_prob))
       
rbinom(1) #rbinom(crc_prob)
sample(c('crc_death','other_death','survival'), size = nrow(pop), replace = TRUE, prob =c(q_prob,cf)

# crc      norm
(10/50) / (100/1000)
(1 - (1 - 0.2)^2 )

# underlying cause
# directly attributable
# excess mortality


  


   8.341529e03/ 168.4
   1.834587e04/ 267.7
   4.210599e04/ 398.6 
   
   
   pop %>% 
     mutate(death = 0) %>% 
     mutate(death_scheme = ifelse(crc_year_risk > q_prob, 'scheme1', 'scheme1')) %>% 
     count(death_scheme)
   
     crc_year_risk
     q_prob
   
   if cf > q
   sample(c('crc_death', 'survival'), size = nrow(pop), replace = TRUE, prob =c(cf, 1-cf)
          else 
            sample(c('crc_death','other_death','survival'), size = nrow(pop), replace = TRUE, prob =c(cf, q_prob-cf,1-q_prob))
          

apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
  apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
  combine_child_adult_bmi() %>% 
  apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
  apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>%
  apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
  apply_diabetes_physiological_parameter_rank_stability(diabetes_joint_estimation_results_df)





anti_depressive_prescription <- tribble(
  ~age_band, ~Males, ~Females,
  '0-17', 0.5, 0.8,
  '18-24', 8.8, 17.3,
  '25-34', 15.2, 24.0,
  '35-44', 19.7, 30.5,
  '45-64', 24.9, 39.6,
  '65-74', 24.5, 38.8,
  '75-84', 22.9, 37.0,
  '85-110', 23.1, 34.6
) %>%
  mutate(
    age_min = as.numeric(str_extract(age_band, "^\\d+")),
    age_max = ifelse(str_detect(age_band, "\\+"), 120, as.numeric(str_extract(age_band, "\\d+$")))
  ) %>%
  pivot_longer(cols = c(Males, Females), names_to = "sex", values_to = "prescription_pct") %>%
  mutate(prescription_prob = prescription_pct / 100)

# Function to populate synthetic population with anti-depression prescriptions
populate_antidepressant_prescription <- function(population_df, prescription_data = anti_depressive_prescription) {
  
  # Create a copy to avoid modifying the original
  pop_with_rx <- population_df %>%
    mutate(
      # Assign age band for matching with prescription data
      age_band = case_when(
        age >= 0 & age <= 17 ~ "0-17",
        age >= 18 & age <= 24 ~ "18-24",
        age >= 25 & age <= 34 ~ "25-34",
        age >= 35 & age <= 44 ~ "35-44",
        age >= 45 & age <= 64 ~ "45-64",
        age >= 65 & age <= 74 ~ "65-74",
        age >= 75 & age <= 84 ~ "75-84",
        age >= 85 ~ "85-110",
        TRUE ~ NA_character_
      )
    )
  
  # Join with prescription probabilities
  pop_with_rx <- pop_with_rx %>%
    left_join(
      prescription_data %>% select(age_band, sex, prescription_prob) ,
      by = c("age_band", "sex")
    ) %>%
    mutate(
      # Adjust probability based on wellbeing status
      # Those with poor wellbeing are more likely to have prescriptions
      # Those with good wellbeing are less likely
      adjusted_prob = case_when(
        is.na(wellbeing) | is.na(prescription_prob) ~ 0,  # No prescription if no wellbeing data
        wellbeing == "poor_wellbeing" ~ prescription_prob * 2.5,  # 2.5x more likely
        wellbeing == "moderate_wellbeing" ~ prescription_prob * 1.5,  # 1.5x more likely
        wellbeing == "good_wellbeing" ~ prescription_prob * 0.3,  # Much less likely
        TRUE ~ prescription_prob
      ),
      # Cap probability at 1.0
      adjusted_prob = pmin(adjusted_prob, 1.0),
      # Assign prescription status probabilistically
      # Note: Children rarely receive antidepressants, so age 0-17 rates are very low
      antidepressant_prescription = runif(n()) < adjusted_prob
    ) %>%
    select(-age_band, -prescription_prob, -adjusted_prob)
  
  return(pop_with_rx)
}

# Example usage:
# Load your synthetic population
# pop <- read.fst('./synthetic_population/pop.fst')

# # Populate with antidepressant prescriptions
# pop_with_prescriptions <- populate_antidepressant_prescription(pop)

# # Save the result
# write.fst(pop_with_prescriptions, './synthetic_population/pop_with_antidepressants.fst')

# # Check the results
# pop_with_prescriptions %>%
#   group_by(sex, wellbeing) %>%
#   summarise(
#     n = n(),
#     pct_prescribed = mean(antidepressant_prescription, na.rm = TRUE) * 100
#   )

# Note: Antidepressant use (versus no antidepressant use) 
# significantly lower all‐cause mortality in people with depression 
# Hazard ratio: 0.79

# Populate depression for children and adolescents
# Point prevalence: 2.8% children, 5.6% adolescents
# Wellbeing is populated for 16+ only, so we need to add it for younger ages
pop <- pop %>%
  mutate(
    wellbeing = case_when(
      # Keep existing wellbeing for 16+ (already populated)
      age >= 16 & !is.na(wellbeing) ~ wellbeing,
      
      # Children (ages 0-12): 2.8% prevalence
      age < 4  ~'good_wellbeing',
      
      age >= 4 & age <= 12 ~ ifelse(runif(n()) < 0.028, 'poor_wellbeing', 'good_wellbeing'),
      
      # Adolescents (ages 13-15): 5.6% prevalence
      age >= 13 & age <= 15 ~ ifelse(runif(n()) < 0.056, 'poor_wellbeing', 'good_wellbeing'),
      
      # Default to good wellbeing if somehow missing
      TRUE ~ 'good_wellbeing'
    )
  ) %>% 
  replace_na(list(wellbeing='good_wellbeing'))

pop <- populate_antidepressant_prescription(pop)

suicide_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                            sheet = "Table 6.4a", range = "AA4:AY845")%>%
  mutate(sex ='Males') %>% 
  filter(Block == 'Intentional self-harm (X60-X84)'& 
           ICD != 'All')

suicide_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                              sheet = "Table 6.4a", range = "BA4:BY845") %>%
  mutate(sex ='Females') %>% 
  filter(Block == 'Intentional self-harm (X60-X84)'& 
           ICD != 'All')

suicide_females
# A tibble: 21 × 26
# Chapter   Block ICD   Description `All Ages`   `0` `1-4` `5-9` `10-14` `15-19` `20-24`
# <chr>     <chr> <chr> <chr>            <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
#   1 Chapter … Inte… X60   Intentiona…          0     0     0     0       0       0       0
# 2 Chapter … Inte… X61   Intentiona…          0     0     0     0       0       0       0
# 3 Chapter … Inte… X62   Intentiona…          0     0     0     0       0       0       0
# 4 Chapter … Inte… X63   Intentiona…          0     0     0     0       0       0       0
# 5 Chapter … Inte… X64   Intentiona…          5     0     0     0       0       0       0
# 6 Chapter … Inte… X66   Intentiona…          0     0     0     0       0       0       0
# 7 Chapter … Inte… X67   Intentiona…          0     0     0     0       0       0       0
# 8 Chapter … Inte… X68   Intentiona…          0     0     0     0       0       0       0
# 9 Chapter … Inte… X69   Intentiona…          0     0     0     0       0       0       0
# 10 Chapter … Inte… X70   Intentiona…         37     0     0     0       1       3  


natural_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                            sheet = "Table 6.4a", range = "AA4:AY845")%>%
  mutate(sex ='Males') %>% 
  filter(substr(ICD, 1, 1) != "V" &
           substr(ICD, 1, 1) != "W" &
           substr(ICD, 1, 1) != "X" &
           substr(ICD, 1, 1) != "Y"& 
           ICD != 'All')
natural_males
# Chapter   Block ICD   Description `All Ages`   `0` `1-4` `5-9` `10-14` `15-19` `20-24`
# <chr>     <chr> <chr> <chr>            <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
#   1 Chapter … Inte… A02   "Other sal…          0     0     0     0       0       0       0
#  2 Chapter … Inte… A04   "Other bac…         11     0     0     0       0       0       0
# 3 Chapter … Inte… A08   "Viral and…          0     0     0     0       0       0       0
#  4 Chapter … Inte… A09   "Diarrhoea…         14     0     0     0       0       0       0
# 5 Chapter … Tube… A16   "Respirato…          2     0     0     0       0       0       0
#  6 Chapter … Tube… A17   "Tuberculo…          0     0     0     0       0       0       0
# 7 Chapter … Tube… A18   "Tuberculo…          0     0     0     0       0       0       0
#  8 Chapter … Tube… A19   "Miliary t…          0     0     0     0       0       0       0
# 9 Chapter … Othe… A31   "Infection…          2     0     0     0       0       0       0
# 10 Chapter … Othe… A32   "Listerios…          1     0     0     0       0       0       0

natural_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                              sheet = "Table 6.4a", range = "BA4:BY845") %>%
  mutate(sex ='Females') %>% 
  filter(substr(ICD, 1, 1) != "V" &
           substr(ICD, 1, 1) != "W" &
           substr(ICD, 1, 1) != "X" &
           substr(ICD, 1, 1) != "Y" & 
           ICD != 'All')

# Derived mortality rates
# Suicide mortality: RR = 9.89 (depression vs no depression)
# Natural mortality: RR = 1.63 (depression vs no depression)
# Antidepressant protective effect: HR = 0.79

suicide_rr <- data.frame(
  condition = c('no_depression', 'depression_no_rx', 'depression_with_rx'),
  rr_suicide = c(1.0, 9.89, 9.89 ),
  rr_natural = c(1.0, 1.63, 1.63 )
)

# Process suicide deaths by age and sex
suicide_deaths <- bind_rows(suicide_males, suicide_females) %>%
  select(sex, ICD, Description, `All Ages`:`90+`) %>%
  pivot_longer(cols = `All Ages`:`90+`, names_to = 'age_group', values_to = 'deaths') %>%
  group_by(sex, age_group) %>%
  summarise(suicide_deaths = sum(deaths, na.rm = TRUE), .groups = 'drop')

# Process natural deaths by age and sex
natural_deaths <- bind_rows(natural_males, natural_females) %>%
  select(sex, ICD, Description, `All Ages`:`90+`) %>%
  pivot_longer(cols = `All Ages`:`90+`, names_to = 'age_group', values_to = 'deaths') %>%
  group_by(sex, age_group) %>%
  summarise(natural_deaths = sum(deaths, na.rm = TRUE), .groups = 'drop')

# Load all deaths (including 'All' row for totals)
all_deaths_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                               sheet = "Table 6.4a", range = "AA4:AY845")%>%
  mutate(sex ='Males') %>% 
  filter(ICD == 'All')

all_deaths_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                 sheet = "Table 6.4a", range = "BA4:BY845") %>%
  mutate(sex ='Females') %>% 
  filter(ICD == 'All')

# Process total deaths
total_deaths <- bind_rows(all_deaths_males, all_deaths_females) %>%
  select(sex, `All Ages`:`90+`) %>%
  pivot_longer(cols = `All Ages`:`90+`, names_to = 'age_group', values_to = 'total_deaths')

# Get population by age and sex from synthetic population
pop <- pop %>%
  mutate(age_group = case_when(
    age == 0 ~ "0",
    age >= 1 & age <= 4 ~ "1-4",
    age >= 5 & age <= 9 ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    age >= 90 ~ "90+",
    TRUE ~ "All Ages"
  )) 

population_by_age <- pop %>% 
  count(sex, age_group, name = 'population') %>%
  mutate(population = population * 10)  # Adjust for population scaling

# Combine death types with population
death_rates_by_age <- total_deaths %>%
  full_join(natural_deaths, by = c('sex', 'age_group')) %>%
  full_join(suicide_deaths, by = c('sex', 'age_group')) %>%
  left_join(population_by_age, by = c('sex', 'age_group')) %>%
  replace_na(list(suicide_deaths = 0, natural_deaths = 0, total_deaths = 0)) %>%
  mutate(
    # Calculate death probabilities (deaths per population)
    qx_suicide = ifelse(!is.na(population) & population > 0, suicide_deaths / population, 0),
    prob_not_suicide = ifelse(!is.na(population) & population > 0, (total_deaths -suicide_deaths) / population, 0),
    qx_natural = ifelse(!is.na(population) & population > 0, natural_deaths / population, 0),
    prob_not_natural = ifelse(!is.na(population) & population > 0, (total_deaths -natural_deaths) / population, 0),
    qx_total = ifelse(!is.na(population) & population > 0, total_deaths / population, 0),
    prob_surv = ifelse(!is.na(population) & population > 0, (population-total_deaths) / population, 0),
    # Calculate percentages of death types
    pct_suicide = ifelse(total_deaths > 0, suicide_deaths / total_deaths * 100, 0),
    pct_natural = ifelse(total_deaths > 0, natural_deaths / total_deaths * 100, 0)
  )


pop <- pop %>% 
  left_join(death_rates_by_age)

pop <- pop %>% 
  mutate(depressed = ifelse(wellbeing == 'poor_wellbeing','depressed','healthy'))

paf <-
  pop %>% 
  count(age_group,sex,antidepressant_prescription, depressed  ) %>% 
  mutate(n = n*10) %>% 
  # filter(!is.na(depressed)) %>% 
  pivot_wider(id_cols = c(age_group,sex,antidepressant_prescription),
              names_from = depressed,
              values_from = n,values_fill = 0) %>%# View()
  mutate(paf_natural = (1 - (depressed + healthy)/ (depressed * 1.63 + healthy * 1  ) )) %>% 
  mutate(paf_suicide = (1 - (depressed + healthy)/ (depressed * 9.89 + healthy * 1 ) )) %>% 
  mutate(paf_all_cause = (1 - (depressed + healthy)/ ((depressed * 2.10  * 0.79) * antidepressant_prescription 
                                                      + (depressed * 2.10 ) * (!antidepressant_prescription)   +
                                                        healthy * 1 ) )) %>% 
  mutate(paf_all_cause_wo_antidepressives = (1 - (depressed + healthy)/ (depressed * 2.1 + healthy * 1 ) )) %>%  
  mutate(paf_all_cause_comorbid_matched = (1 - (depressed + healthy)/ (depressed * 1.29 + healthy * 1 ) )) %>% 
  mutate(paf_all_cause_q = (1 - (depressed + healthy)/ (depressed * 2.1 + healthy * 1 ) )) 

paf_min_df <- paf[c('sex',
                    'age_group',
                    'antidepressant_prescription',
                    'paf_suicide',
                    'paf_natural',
                    'paf_all_cause',
                    'paf_all_cause_comorbid_matched',
                    'paf_all_cause_wo_antidepressives',
                    'paf_all_cause_q')] %>% 
  
  left_join(death_rates_by_age[c('age_group',
                                 'sex',
                                 'qx_suicide',
                                 'qx_natural',
                                 'qx_total')],
            by = c('sex', 'age_group')) %>% 
  mutate(q_min_suicide = qx_suicide * (1- paf_suicide )) %>% 
  mutate(q_min_natural = qx_natural * (1- paf_natural )) %>% 
  mutate(q_min_all_cause = qx_total * (1- paf_all_cause )) %>% 
  mutate(q_min_all_cause_comorbid_matched  = qx_total * (1- paf_all_cause_comorbid_matched)) %>% 
  mutate(q_min_all_cause_wo_antidepressives = qx_total * (1 - paf_all_cause_wo_antidepressives)) %>% 
  mutate(q_min_all_cause_q = qx_total * (1 - paf_all_cause_q))

pp <- ppp %>% 
  filter(age!= 'Birth') %>% 
  mutate(age = as.numeric(age)) %>% 
  select(qx = `2024`, sex, age) %>% 
  mutate(qx=qx/100000)

sya_paf_min_df <- pop %>% 
  count(age, sex, depressed) %>% 
  left_join(pp) %>% 
  pivot_wider(id_cols = c(age, sex, qx),names_from = depressed, values_from = n) %>% 
  replace_na( list(depressed = 0) ) %>% 
  mutate(paf_q = (1 - (depressed + healthy)/ (depressed * 2.1 + healthy * 1 ) )) %>% 
  mutate(min_q = qx * (1 - paf_q)) 

# ex     age   `2022` `2023` `2024` `2025` `2026` `2027` `2028` `2029` `2030` `2031`
# <chr>   <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#   1 Females Birth 203.   193.   187.   181.   176.   172.   167.   164.   160.   157.  
# 2 Females 0      44.6   42.2   40.9   39.7   38.6   37.6   36.7   35.8   35.0   34.3 
# 3 Females 1      14.1   13.3   12.9   12.5   12.2   11.8   11.5   11.3   11.0   10.8 
# 4 Females 2       9.70   9.18   8.90   8.63   8.37   8.15   7.94   7.76   7.58   7.42
# 5 Females 3       8.11   7.68   7.45   7.23   7.01   6.83   6.66   6.50   6.35   6.22
# 6 Females 4       7.32   6.92   6.71   6.52   6.33   6.17   6.01   5.87   5.74   5.62
# 7 Females 5       6.60   6.22   6.03   5.85   5.69   5.55   5.41   5.29   5.17   5.06
# 8 Females 6       5.68   5.35   5.18   5.02   4.88   4.76   4.65   4.54   4.45   4.36
# 9 Females 7       4.84   4.56   4.40   4.26   4.14   4.04   3.95   3.87   3.78   3.71
# 10 Females 8       4.55   4.29   4.14   4.01   3.89   3.79   3.71   3.63   3.56   3.49

y <- pop %>% 
  select(id,age,  age_group, sex, year, bmi, depressed, antidepressant_prescription) %>% 
  mutate(year = as.character(year)) %>% 
  left_join(paf_min_df[
    c(
      'sex', 
      'age_group',
      'antidepressant_prescription',
      'q_min_suicide',
      'q_min_natural',
      'q_min_all_cause',
      'q_min_all_cause_comorbid_matched',
      'q_min_all_cause_q')
  ],
  relationship = "many-to-one" ,
  by = c('age_group','sex','antidepressant_prescription')) %>% 
  left_join(pp) %>% 
  
  mutate(rr_all_cause_comorbid_matched = case_when(
    depressed == 'depressed' & antidepressant_prescription == FALSE ~ 1.29,
    depressed == 'depressed' & antidepressant_prescription == TRUE ~ 1.29,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>%
  
  mutate(rr_all_cause = case_when(
    depressed == 'depressed' & antidepressant_prescription == FALSE ~ 2.1,
    depressed == 'depressed' & antidepressant_prescription == TRUE ~ 2.1 * 0.79,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>%
  
  mutate(rr_suicide = case_when(
    depressed == 'depressed'  ~ 9.89,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>% 
  
  mutate(rr_natural = case_when(
    depressed == 'depressed'  ~   1.63,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>% 
  
  mutate(rr = rr_all_cause_comorbid_matched ) %>% 
  mutate(q_min =  q_min_all_cause_comorbid_matched) %>% 
  # mutate(rr_intervene =  1 + ( ( 1 - rr) * 0.5 )) %>%
  mutate( rr_intervene = 1) %>% 
  
  mutate(q_modelled_all_cause = q_min_all_cause * rr_all_cause ) %>%
  mutate(q_modelled_all_cause_comorbid_matched = q_min_all_cause_comorbid_matched * rr_all_cause_comorbid_matched ) %>%
  mutate(q_modelled_suicide = q_min_suicide * rr_suicide ) %>%
  mutate(q_modelled_natural = q_min_natural * rr_natural ) %>%
  
  mutate(q_modelled_other = max(0, q_modelled_all_cause - q_modelled_natural - q_modelled_suicide) )%>% 
  mutate(q_surv = 1 - q_modelled_all_cause ) %>% 
  rowwise() %>% 
  mutate(q_all = list(list(q_modelled_other = q_modelled_other,
                           q_modelled_natural = q_modelled_natural,
                           q_modelled_suicide = q_modelled_suicide,
                           q_surv = q_surv))) %>%
  ungroup() %>% 
  # mutate(q_modelled = q_min * rr ) %>%
  # mutate(q_modelled = q_min * rr_intervene ) %>%
  # mutate(bern_trial = runif( n = n() )) %>% 
  # mutate(death = (bern_trial < q_modelled )) %>% 
  rowwise() %>% 
  mutate(death = sample( sample(size = 1, x = names(q_all),prob = unlist(q_all)) ) ) %>% 
  ungroup()

y %>%
  count(death)

y %>% 
  slice_sample(prop = 0.01) %>% 
  # mutate(q_rec = q_modelled_other + q_modelled_natural + q_modelled_suicide + q_surv) %>% 
  mutate(q_rec = qx ) %>%
  
  # group_by(sex) %>%
  e_charts(age) %>% 
  
  e_scatter(q_modelled_natural) %>%
  e_scatter(q_modelled_suicide) %>% 
  e_scatter(q_modelled_other) %>%
  
  e_scatter(q_surv) %>% 
  # e_scatter(q_rec) %>% 
  e_scatter(qx) %>% 
  
  e_tooltip(trigger= 'axis') %>% 
  e_theme('walden')

pop1 <- pop

pop_d1 = data.frame(); pop_a1 = data.frame(); for( k in  1:5){ #1:20
  
  message(k)
  
  for(j in c('intervention','non-intervention')){
    
    message(j)
    pop1 <- pop
    
    for( i in 2023:2045){#2023:2050
      
      message(i)
      pop1 <- pop1 %>%
        reindex_risk_percentile() %>%
        apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) %>%
        mutate(
          wellbeing = case_when(
            age >= 16 & !is.na(wellbeing) ~ wellbeing,
            age < 4  ~'good_wellbeing',
            age >= 4 & age <= 12 ~ ifelse(runif(n()) < 0.028, 'poor_wellbeing', 'good_wellbeing'),
            age >= 13 & age <= 15 ~ ifelse(runif(n()) < 0.056, 'poor_wellbeing', 'good_wellbeing'),
            TRUE ~ 'good_wellbeing'
          )
        ) %>% 
        replace_na(list(wellbeing='good_wellbeing')) %>% 
        mutate(depressed = ifelse(wellbeing == 'poor_wellbeing','depressed','healthy'))
      
      pop2 <- pop1 %>% 
        select(id,age, 
               mdm_quintile_soa_name, HSCT, Urban_mixed_rural_status, 
               age_group,depression_percentile, sex, year, bmi, depressed, antidepressant_prescription) %>% 
        mutate(year = i) %>%
        mutate(intervene = j) %>%
        mutate(age = age + 1) %>% 
        mutate(year = as.character(year)) %>% 
        left_join(paf_min_df[
          c(
            'sex', 
            'age_group',
            'antidepressant_prescription',
            'q_min_suicide',
            'q_min_natural',
            'q_min_all_cause',
            'q_min_all_cause_comorbid_matched',
            'q_min_all_cause_wo_antidepressives',
            'q_min_all_cause_q')
        ],
        relationship = "many-to-one" ,
        by = c('age_group','sex','antidepressant_prescription')) %>% # %>% #count(is.na(q_adj)) # "many-to-one"
        # left_join(rr_df) %>% 
        mutate(rr = case_when(depressed == 'depressed' ~ 2.1,
                              depressed == 'healthy'  ~ 1,
                              T  ~ 1
        )) %>% 
        left_join(sya_paf_min_df[c('age','sex','min_q')]) %>% 
        mutate(q_modelled = min_q * rr ) 
      # mutate(q_modelled = q_min_all_cause_wo_antidepressives * rr ) 
      
      if(j == 'intervention' & i >= 2026 & i <= 2030){
        
        pop2 <- pop2 %>% 
          mutate(q_modelled = min_q * (1 + ( rr - 1 ) * 0.5) )
        # mutate(q_modelled = q_min_all_cause * (1 + ( rr - 1 ) * 0.5) )
        
        # mutate(q_modelled = q_min_all_cause_wo_antidepressives * (1 ) ) 
      }
      
      pop2 <- pop2 %>% 
        mutate(bern_trial = runif(n=n())) %>% 
        mutate(death = (bern_trial<q_modelled)) 
      
      # print('add births')
      # pop2 <- pop2 %>%
      #   mutate(year = as.numeric(year)) %>%
      #   mutate(id = as.character(id ))%>%
      #   asfr_births( fertility = fertility)
      
      pop_dead <- pop2 %>%
        filter(death == T ) %>% 
        filter(age > 30) %>%
        count(year,intervene, run=k)
      
      pop1 <- pop2 %>% 
        filter(death == F ) 
      
      pop_alive <- pop1 %>% 
        filter(age > 30) %>%
        count(year,intervene,run = k)
      
      pop_d1 <- rbind( pop_d1, pop_dead ) 
      pop_a1 <- rbind( pop_a1, pop_alive ) 
      
    }
  }
}

ggplot(pop_d1) +
  geom_point(aes(year, n, colour = as.character(run) )) +
  geom_line(aes(year, n, group = paste(run, intervene), lty=as.character(intervene) ,colour = as.character(run)))

ggplot(pop_a1) +
  geom_point(aes(year, n, colour = as.character(run) )) +
  geom_line(aes(year, n, group = paste(run, intervene), lty=as.character(intervene) ,colour = as.character(run)))


pop_a1 %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene, year) %>% 
  summarise(n = mean(n)) %>%
  e_charts( year) %>% 
  e_tooltip(trigger = 'axis') %>% 
  e_y_axis(min=110000) %>%
  e_line(n)

pop_a1 %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene, year) %>% 
  summarise(n = mean(n)) %>%
  pivot_wider(names_from = intervene, values_from = n) %>% 
  mutate(diff =  intervention -  `non-intervention`) %>% 
  e_charts( year) %>% 
  e_tooltip(trigger = 'axis') %>% 
  # e_y_axis(min=110000) %>%
  # e_line(intervention) %>% 
  # e_line(`non-intervention`) %>% 
  e_bar(diff)


ggplot(pop_d1) +
  geom_point(aes(year, n, colour = intervene)) #+
# geom_line(aes(year, n, group = intervene)) +
# geom_rect(aes(xmin = '2026', xmax = '2030', ymin = 1401, ymax = 2600), fill = 'green', alpha = 0.005) +
#ylim(c(1400, 2600))

pop_a1 %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene,run) %>% 
  e_charts( year) %>% 
  e_line(n) %>% 
  e_y_axis(min=110000) 

pop_a1 %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  ggplot() +
  geom_point(aes(year, n, colour = intervene)) +
  geom_line(aes(year, n, group = intervene, colour = intervene))

pop_d1 %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot() +
  geom_rect(aes(xmin = 2025, xmax = 2030, ymin = 601, ymax = 2000),
            colour = 'white', 
            fill = 'green', alpha = 0.01) +
  # ylim(c(1400, 2600))+
  geom_point(aes(year, n, colour = intervene))+
  geom_line(aes(year, n, group = intervene, colour = intervene)) +
  # scale_x_continuous(
  #   breaks = seq(2023, 2065, by = 5),
  #   labels = scales::label_number()
  # )+
  theme_minimal(base_family = "Graphik")

natural_var <- pop_d1 %>% 
  group_by(year,intervene) %>%
  summarise(nn=n(), sd =sd(n), n = mean(n)) %>% 
  mutate( error = sd / sqrt(nn) ) %>% 
  ungroup() %>% 
  summarise(error = mean(error)) %>% 
  pull(error)
# ungroup() %>% 
# ggplot() + 
# geom_point(aes(year, error)) 

x <- pop_d1 %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  pivot_wider(names_from=intervene,values_from = n) %>% 
  mutate(delta = intervention - `non-intervention`) %>% 
  ungroup() %>% 
  mutate(cum_delta = cumsum(delta))

x %>% 
  ggplot() +
  geom_col(aes(year, delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'none',
    delta - natural_var * 1.96 < 0 ~ 'down',
    delta - natural_var * 1.96 > 0 ~ 'up'
  )))  +
  scale_fill_manual(values = c("up" = "salmon", "down" = "lightgreen",'none' = 'black'))+
  geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  geom_smooth(method = 'loess',data  = x %>% filter(year>2030), aes(y =delta, x = year)) +
  theme_minimal() +
  labs(
    fill = 'title_text',
    x = "Year",
    y = "Delta"
  ) +
  # geom_bar(aes(year, delta )) +
  geom_line(aes(year, delta )) #+ 
# geom_rect(aes(xmin = '2026', xmax = '2030', ymin = 01, ymax = 2600), fill = 'green', alpha = 0.005) +
# ylim(c(1400, 2600))


x %>% 
  ggplot() +
  geom_col(aes(year, cum_delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'none',
    delta - natural_var * 1.96 < 0 ~ 'down',
    delta - natural_var * 1.96 > 0 ~ 'up'
  )))  +
  scale_fill_manual(values = c("up" = "salmon", "down" = "lightgreen",'none' = 'black'))+
  geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  geom_smooth(method = 'loess', data  = x %>% filter(year>2030), aes(y =delta, x = year)) +
  theme_minimal() +
  labs(
    fill = 'title_text',
    x = "Year",
    y = "Delta"
  ) 

geom_bar(aes(year, delta )) +
  geom_line(aes(year, delta )) 


apply_age_sex_death<- function(current_population, apply_death = F){
  
  year1 <- max(current_population$year)
  
  current_population <- select(current_population, - any_of( 'qx') )
  
  current_population <- as.data.table(current_population)[ as.data.table(lifetables), on = .( age, sex), nomatch = 0 ]
  
  current_population$bern_trial <- runif(n=length(current_population$qx))
  
  current_population = current_population[, `:=` (death = year1 * (bern_trial<qx))] 
  current_population[ , death_reason := ifelse(year1==death, 'age_sex_std', NA)]
  
  current_population <- as.data.frame(current_population) 
}

library(readxl)
official <- read_excel("data/ni/ni_ppp_machine_readable.xlsx", 
                       sheet = "Deaths")

official %>% 
  pivot_longer(-c(Sex,Age), names_to = 'year', values_to = 'pop') %>% 
  count(Age, year, wt = pop) %>% 
  filter(Age>30) %>%
  filter(year<2055) %>% 
  count(year, wt= n) %>% 
  ggplot() +
  geom_point(aes(year, n))

