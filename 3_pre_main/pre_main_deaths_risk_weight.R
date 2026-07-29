  library(readxl)
ppp <- read_excel("data/ni/ni_ppp_machine_readable.xlsx", 
                  sheet = "Mortality_assumptions")

ppp <- ppp %>% 
  mutate(survival_prob = 1 - `2022 - 2023`/1e5, mortality = `2022 - 2023`/1e5,Age = as.numeric(Age)) %>% 
  select(sex = Sex, age = Age, mortality_prob = mortality , survival_prob) 

pop <- time_one_population %>% 
  #relative Rsiks take from main_bowel_cancers.R
  #see there for references
  left_join(ppp) %>% 
  mutate(all_cause_rr_obese = case_when(bmi == 'obese' ~ 1.20,
                                        bmi == 'overweight' ~ 1.07,
                                        bmi == 'normal' ~ 1  )) %>% 
  mutate(all_cause_rr_diabetes = ifelse(diabetes_status != 'no_diabetes',1.7,1) ) %>% 
  mutate(all_cause_rr_smoking = ifelse(smoking == 'current_smoker',3.7,1) ) %>% 
  mutate(all_cause_rr_active = ifelse(pa != 'meets_recommendations',1.86,1) ) %>% 
  mutate(all_cause_rr_alcohol = ifelse(alcohol %in% c('increased_risk','higher_risk'),1.36,1) )

all_cause_paf <- pop %>% 
  group_by(age, sex) %>% 
  summarise(paf_all_cause= 1 - (n()/ sum(
    all_cause_rr_obese *
      all_cause_rr_diabetes *
      all_cause_rr_smoking *
      all_cause_rr_active *
      all_cause_rr_alcohol)
  )) %>% 
  replace_na(list(paf_all_cause = 0))


# pop %>% 
#   left_join(all_cause_paf) %>% 
#   mutate(tm_mortality_prob = mortality_prob * (1 - paf_all_cause)) %>%
#   mutate(risk_weight_mortality_prob = tm_mortality_prob*all_cause_rr_obese *all_cause_rr_diabetes *all_cause_rr_smoking *all_cause_rr_active *all_cause_rr_alcohol) %>% 
#   mutate(risk_weight_survival_prob = 1 - risk_weight_mortality_prob) 

