message('start pre main 35')

initial_time_zero_population <- read.fst('./3_pre_main/intermediate_populations/initial_time_zero_population.fst')

# initial_time_zero_population <- initial_time_zero_population %>% 
#   mutate(bmi = 'normal')

initial_time_zero_population <- initial_time_zero_population %>% 
  # mutate(age = age + 1) %>% 
  mutate(year = year + 1) %>% 
  mutate(
    age20 = cut(age, include.lowest = T,
                breaks = seq(0,120,20),
                labels = c('0-20','20-40','40-60','60-80','80-100','100-120')
    ),
    ageKid = cut(age, include.lowest = T,
                 breaks = c(-Inf, 1, 10, 15, Inf),
                 labels = c('0-1', '2-10', '11-15', NA),
                 right = TRUE),
    age10 = cut(age, include.lowest = T,
                breaks = c(-Inf, 15, 34, 44, 54, 64, 74, Inf),
                labels = c('0-15', '16-34', '35-44', '45-54', '55-64', '65-74', '75-110'),
                right = TRUE)
  )

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(SETTLEMENT2015_name = ifelse(is.na(SETTLEMENT2015_name),
                                      paste(HSCT,'country'),
                                      SETTLEMENT2015_name))

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate( mdm_quintile_soa_verbose = case_when(mdm_quintile_soa_name == 'Most Deprived'~  'Most Deprived',
                                               mdm_quintile_soa_name == 'Quintile 2' ~ '2nd Most deprived',
                                               mdm_quintile_soa_name == 'Quintile 3' ~ 'Middle deprived',
                                               mdm_quintile_soa_name == 'Quintile 4' ~ '2nd Least Deprived',
                                               mdm_quintile_soa_name == 'Least Deprived'~ 'Least Deprived'
  ))

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                      ordered=T,
                                      levels=c('Least Deprived',
                                               'Quintile 4',
                                               'Quintile 3',
                                               'Quintile 2',
                                               'Most Deprived'
                                      )))

####################################################################

# initial_time_zero_population$year <- model_specification$model$start_year 

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell)

####################################################################
# Initialise deaths
####################################################################
initial_time_zero_population$death <- 0
initial_time_zero_population$death_reason <- '-1'
initial_time_zero_population$qmortality_risk <- 0.0
initial_time_zero_population$qx <- 0.0

initial_time_zero_population <- initial_time_zero_population %>%
  filter(is.na(death)| is.null(death)| death==0)

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_age_sex_death(apply_death = T) #%>% 
# apply_qmortality_mortality(apply_death = F)

####################################################################
# Initialise Births
####################################################################

initial_time_zero_population$mothers_age <- NA
initial_time_zero_population$mothers_id <- '-1'

initial_time_zero_population$id <- as.character(initial_time_zero_population$id)

initial_time_zero_population <- initial_time_zero_population %>% 
  asfr_births(fertility )# %>% 
  # apply_low_birth_weight_risk() %>% 
  # apply_preterm_birth_risk()
#https://pmc.ncbi.nlm.nih.gov/articles/PMC5860004/#kwx177s3

####################################################################
# Initialise population
####################################################################

names(initial_time_zero_population)[str_ends(names(initial_time_zero_population),'year_risk')]

initial_time_zero_population <- initial_time_zero_population %>% 
  declare_absolute_incident_morbidity(morbidity = "stroke") %>% 
  declare_absolute_incident_morbidity(morbidity = "chd") %>% 
  declare_absolute_incident_morbidity(morbidity = "diabetes") %>% 
  declare_absolute_incident_morbidity(morbidity = "dementia") %>% 
  declare_absolute_incident_morbidity(morbidity = "heart_failure") %>% 
  
  declare_absolute_incident_morbidity(morbidity = "chronic_kidney_disease") %>% 
  
  declare_absolute_incident_morbidity(morbidity = "atrial_fibrillation") %>% 
  declare_absolute_incident_morbidity(morbidity = "hypertension") %>% 
  
  declare_absolute_incident_morbidity_alt(morbidity = "asthma") %>%
  declare_absolute_incident_morbidity_alt(morbidity = "copd") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "cancer") %>%
  
  declare_absolute_incident_morbidity_alt(morbidity = "depression") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "non_diabetic_hyperglycaemia") %>%
  
  declare_absolute_incident_morbidity_alt(morbidity = "epilepsy") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "rheumatoid_arthritis") %>%
  declare_absolute_incident_morbidity_alt(morbidity = "osteoarthritis") %>%
  
  declare_absolute_incident_morbidity_alt(morbidity = "osteoporosis") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "hypothyroidism") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "pad") 

names(initial_time_zero_population)[startsWith(prefix = 'nafld',names(initial_time_zero_population))]

initial_time_zero_population <- initial_time_zero_population %>% 
  # declare_absolute_incident_morbidity(morbidity = "arld") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "nafld") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "sle") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "ibd") 
  
initial_time_zero_population$fracture4 <- 0 
initial_time_zero_population$nof <- 0

initial_time_zero_population <- initial_time_zero_population %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "fracture4") %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "nof") 

# names(initial_time_zero_population)[startsWith(prefix = 'prostate_cancer',names(initial_time_zero_population))]
# names(initial_time_zero_population)[startsWith(prefix = 'female_breast_cancer',names(initial_time_zero_population))]

initial_time_zero_population <- initial_time_zero_population %>% 
  rename(kidney_cancer = renal_cancer)
  
  
initial_time_zero_population <- initial_time_zero_population %>% 
  # apply_lung_cancer_risk_factors(., lung_cancer_theoretical_minimum) %>% 
  declare_absolute_incident_morbidity_alt(morbidity = "lung_cancer") %>%
  declare_absolute_incident_morbidity_alt(morbidity = "colorectal_cancer") %>% 
  # declare_absolute_incident_morbidity_alt("stomach_cancer") %>%
  # declare_absolute_incident_morbidity_alt("osteogastric_cancer") %>%
  
  declare_absolute_incident_morbidity_alt("prostate_cancer") %>%
  declare_absolute_incident_morbidity_alt("female_breast_cancer") %>%
  declare_absolute_incident_morbidity_alt("pancreatic_cancer") %>% 
  declare_absolute_incident_morbidity_alt("kidney_cancer") %>% 
  
  # declare_absolute_incident_morbidity_alt("oesophageal_cancer") %>%        
  # declare_absolute_incident_morbidity_alt("oesphageal_gastric_cancer") %>% 
  
  declare_absolute_incident_morbidity_alt("cervical_cancer") %>%   
  declare_absolute_incident_morbidity_alt("brain_cancer") %>%   
  
  declare_absolute_incident_morbidity_alt("oral_cancer") %>%               
  declare_absolute_incident_morbidity_alt("uterine_cancer")

initial_time_zero_population <-
  initial_time_zero_population %>% 
  # declare_absolute_incident_morbidity_alt("blood_cancer") %>%              
  apply_ovarian_risk_factors( ovarian_theoretical_minimum) %>% #count(age,is.na(ovarian_cancer_year_risk)) %>% View()
  declare_absolute_incident_morbidity_alt("ovarian_cancer") 


time_one_population <- initial_time_zero_population

write.fst(time_one_population,
          paste0('./3_pre_main/intermediate_populations/time_one_population.fst'))

message('done pre main 35')

