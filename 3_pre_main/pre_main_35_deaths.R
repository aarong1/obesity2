message('started pre main 35 deaths')

time_one_population <- read.fst('./3_pre_main/intermediate_populations/time_one_population.fst')

select <- dplyr::select

# 'Cerebrovascular disease (I60-I69)',
# 'Diabetes mellitus (E10-E14)',
# 'Asthma (J45)',
# 'Malignant neoplasms (C00-C97)'

disease_lookups <- tribble(
  ~std, ~disease,
  # 'atrial_fibrillation', 'Atrial fibrillation and flutter',
  'copd','Emphysema',
  'copd', 'Other chronic obstructive pulmonary disease',
  #'Other interstitial pulmonary diseases',
  'chronic_kidney_disease', 'Chronic kidney disease',
  'dementia','Vascular dementia',
  'dementia','Unspecified dementia',
  'chd', 'Angina pectoris',
  'chd', 'Acute myocardial infarction',
  'chd', 'Other acute ischaemic heart diseases',
  'stroke', 'Cerebrovascular diseases (I60-I69)',
  'diabetes', 'Diabetes mellitus (E10-E14)',
  'asthma', 'Asthma (J45)',
  'asthma', 'Asthma',
  'cancer', 'Malignant neoplasms (C00-C97)',
  'heart_failure', 'Heart failure'
  
  # 'Chronic liver disease (K70 K73-K74)',
  # 'Acute myocardial infarction (I21)' ,
)

disease_w_modelled_deaths <- c("chd",
                               "stroke",
                               "heart_failure",     
                               "diabetes",              
                               "chronic_kidney_disease",
                               "dementia",              
                               "asthma",                
                               "copd",                                      
                               # "atrial_fibrillation',
                               "lung_cancer",
                               "colorectal_cancer",
                               "prostate_cancer",
                               "female_breast_cancer",
                               "oral_cancer",
                               # "bladder_cancer",
                               # "brain_cancer",
                               "pancreatic_cancer",
                               "uterine_cancer",
                               "ovarian_cancer",
                               # "cervical_cancer",
                               "kidney_cancer")

detailed_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                             sheet = "Table 6.4a", range = 'AA4:AY845') %>% 
  mutate(sex = 'Males')

detailed_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                               sheet = "Table 6.4a", range = 'BA4:BY845') %>% 
  mutate(sex = 'Females')

detailed <- rbind(detailed_males, detailed_females)

all_deaths <- detailed %>% 
  filter(Block == 'All') %>% 
  mutate(std='all') %>% 
  group_by(std,sex) %>%
  summarise(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-19', '20-24',
                     '25-29', '30-34', '35-39', '40-44',
                     '45-49', '50-54', '55-59', '60-64', '65-69',
                     '70-74', '75-79', '80-84', '85-89', '90+'),
                   ~sum(.x)))      

modelled_deaths <- detailed %>% 
  filter(Description %in% disease_lookups$disease | Block %in% c( 'Diabetes mellitus (E10-E14)',
                                                                  'Cerebrovascular diseases (I60-I69)',
                                                                  'Asthma (J45)') | ICD %in%  c( 'I60',
                                                                                                 'I61',
                                                                                                 'I62')) %>%#View()
  left_join(disease_lookups, by = c('Description' = 'disease')) %>% #View()
  left_join(disease_lookups, by = c('Block' = 'disease')) %>% 
  mutate(std = coalesce(std.x,std.y)) %>% 
  select(-c(std.x, std.y)) %>% #View()
  
  # mutate(std = ifelse(Block %in% c('Diabetes mellitus (E10-E14)'),'diabetes',std)) %>% #View()
  # mutate(std = ifelse(Block %in% c('Cerebrovascular disease (I60-I69)'),'stroke',std)) %>% #View()
  # mutate(std = ifelse(Block %in% c('Asthma (J45)'),'asthma',std)) %>% View()
    
    
  group_by(std,sex) %>%
  summarise(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-19','20-24',
                     '25-29', '30-34', '35-39', '40-44',
                     '45-49', '50-54', '55-59', '60-64', '65-69',
                     '70-74', '75-79', '80-84', '85-89', '90+'),
                   ~sum(.x)))      

cancer_deaths <- detailed %>% 
  mutate(std = case_when(
    ICD %in% c('C33','C34') ~ 'lung_cancer',
    ICD %in% c('C18','C19','C20') ~ 'colorectal_cancer',
    ICD %in% c('C61') ~ 'prostate_cancer',
    ICD %in% c('C50') ~ 'female_breast_cancer',
    ICD %in% c('C00-C14') ~ 'oral_cancer',
    # ICD %in% c('C67') ~ 'bladder_cancer',
    # ICD %in% c('C70','C71','C72','C75') ~ 'brain_cancer',
    ICD %in% c('C25') ~ 'pancreatic_cancer',
    ICD %in% c('C54','C55') ~ 'uterine_cancer',
    ICD %in% c('C56','C57') ~ 'ovarian_cancer', 
    # ICD %in% c('C53') ~ 'cervical_cancer',
    ICD %in% c('C64') ~ 'kidney_cancer')) %>% 
  filter(!is.na(std)) %>% 
  group_by(std,sex) %>%

  summarise(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-19','20-24',
                     '25-29', '30-34', '35-39', '40-44',
                     '45-49', '50-54', '55-59', '60-64', '65-69',
                     '70-74', '75-79', '80-84', '85-89', '90+'),
                   ~sum(.x)))   

all_modelled_deaths <- rbind(
  modelled_deaths,
  cancer_deaths
) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  summarise(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-19','20-24',
                     '25-29', '30-34', '35-39', '40-44',
                     '45-49', '50-54', '55-59', '60-64', '65-69',
                     '70-74', '75-79', '80-84', '85-89', '90+'),
                   ~sum(.x)))   

not_modelled_deaths <- rbind(
  all_modelled_deaths,
  select(ungroup(all_deaths),-std) ) %>% 
  group_by(sex) %>% 
  summarise(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-19','20-24',
                                '25-29', '30-34', '35-39', '40-44',
                                '45-49', '50-54', '55-59', '60-64', '65-69',
                                '70-74', '75-79', '80-84', '85-89', '90+'),
                              ~sum(.x[2] - .x[1] )))   

deaths_by_age_cause <- rbind(
  modelled_deaths,
  cancer_deaths,
  not_modelled_deaths %>% 
    mutate(std = 'other',.before = 1)
)

deaths_by_age_cause_long <- deaths_by_age_cause %>% 
  select(-`All Ages`) %>% 
  pivot_longer(-c(1,2), 
               names_to = 'age_band_death', 
               values_to = 'deaths')

all_deaths_by_age_cause <- deaths_by_age_cause %>% 
  select(std,all_ages = `All Ages`,sex) %>% 
  arrange(desc(all_ages))

ages_df <- data.frame(
age_band_death =   c('0', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39' ,'40-44' ,'45-49' ,'50-54' ,'55-59' ,'60-64', '65-69' ,'70-74', '75-79', '80-84', '85-89' ,'90+'),
age_band_death10 = c('0', '1-9', '1-9', '10-19', '10-19', '20-29', '20-29', '30-39', '30-39', '40-49' ,'40-49' ,'50-59' ,'50-59' ,'60-69' ,'60-69' ,'70-79', '70-79', '80-89', '80-89' ,'90+'),
age_band_death20 = c('0', '1-19','1-19','1-19',   '1-19', '20-39' ,'20-39' ,'20-39' ,'20-39' ,'40-59' ,'40-59' ,'40-59' ,'40-59' ,'60-69' ,'60-69', '70-79', '70-79', '80-89', '80-89' ,'90+')
)

fatality <- time_one_population %>% 
  # populate_prostate_cancer() %>%
  mutate(age_band_death =
           cut(age, include.lowest = T,
               breaks = c(-Inf, 0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
               labels = c('0', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39' ,'40-44' ,'45-49' ,'50-54' ,'55-59' ,'60-64', '65-69' ,'70-74', '75-79' ,'80-84', '85-89' ,'90+')
           )) %>% 
  group_by(age_band_death,
           sex) %>% 
  summarise(across(disease_w_modelled_deaths, ~sum(.x!=0))) %>% #View()
  pivot_longer(cols = -c( age_band_death,sex),names_to = 'std',values_to = 'disease') %>% 
  full_join(deaths_by_age_cause_long) %>% 
  mutate(fatality = deaths/disease/model_specification$population$scale_down_factor)

fatality <- fatality %>% 
  left_join(ages_df) %>% 
  group_by(std,sex,age_band_death10) %>% 
  summarise(across(c(disease,deaths), ~sum(.x, na.rm = T))) %>% 
  mutate(fatality = deaths/disease/model_specification$population$scale_down_factor) #%>% View()

fatality <- fatality %>%
  replace_na(list(fatality=0))

fatality %>% filter(fatality>0.9) %>% arrange(desc(fatality)) %>% print(n=40)

# 1 dementia          Females 50-59                  0      2   Inf   
#dementia is just not prevalent at that age according to our sources
# 24 pancreatic_cancer Females 50-59                  0      9   Inf   
# 25 pancreatic_cancer Females 90+                    0     12   Inf   
# 26 pancreatic_cancer Males   50-59                  0     13   Inf   
# 33 pancreatic_cancer Males   80-89                  4     38     0.95
# 29 pancreatic_cancer Males   60-69                  1     29     2.9 
# 30 pancreatic_cancer Females 60-69                  1     22     2.2 
#swap out pancreatic cancer fatality for average - just edge cases of prevalence not distriuted correctly
# 31 lung_cancer       Females 50-59                  2     33     1.65
# 32 lung_cancer       Males   50-59                  4     47     1.18
#swap out lung cancer prevalence for 
# 27 stroke            Males   0                      0      1   Inf   
# 28 stroke            Males   20-29                  0      1   Inf   

fatality <- fatality %>%
  mutate(fatality = ifelse( fatality>0.9, 0.9, fatality)) 
  
  # mutate(fatality = ifelse(std == 'pancreatic_cancer' & fatality>0.9, 0.9, fatality)) %>% 
  # mutate(fatality = ifelse(std == 'lung_cancer' & fatality>0.9, 0.9, fatality)) %>% 
  # mutate(fatality = ifelse(std == 'dementia' & fatality>1, 0.9, fatality)) %>% 
  # mutate(fatality = ifelse(std == 'stroke' &  fatality>1, 0.9, fatality))

fatality %>% filter(fatality>0.9) %>% arrange(desc(fatality)) %>% print(n=40)

message('done pre main 35 deaths')



time_one_population <- read.fst('./3_pre_main/intermediate_populations/time_one_population.fst')

time_one_population <- time_one_population %>%
  mutate(age_band_death =
           cut(age, include.lowest = T,
               breaks = c(-Inf, 0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
               labels = c('0', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39' ,'40-44' ,'45-49' ,'50-54' ,'55-59' ,'60-64', '65-69' ,'70-74', '75-79' ,'80-84', '85-89' ,'90+')
           )) %>%
  left_join(ages_df)

pop <- time_one_population %>% 
  count(sex,age_band_death10) %>% 
  mutate(n=n*model_specification$population$scale_down_factor) %>% 
  pull(n)

other_deaths_df <- fatality %>% 
  filter(std == 'other') %>%
  ungroup() %>% 
  mutate(disease = pop) %>% 
  mutate(other_fatality = deaths/disease) %>% 
  select(age_band_death10,sex,other_fatality)

fatality_wide <- fatality %>% 
  pivot_wider(id_cols = c(sex, age_band_death10), names_glue = '{std}_fatality',names_from = std, values_from = fatality)

write.fst(fatality_wide, './1_2_utils/data/wide_fatality.fst')

message('write pre main 35 deaths files')


