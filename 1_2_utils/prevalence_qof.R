df <- c(
  # 'lung_cancer' = 'Cancer',
  'atrial_fibrillation' = 'Atrial Fibrillation',
  'cancer' = 'Cancer',
  'chronic_kidney_disease' = 'Chronic Kidney Disease',
  'chd' = 'Coronary Heart Disease',
  'dementia' = 'Dementia',
  'hypertension' = 'Hypertension',
  'stroke' = 'Stroke & TIA',
  'diabetes' = 'Diabetes Mellitus',
  
  # 'epilepsy' = 'Epilepsy',
  # 'pad' = 'Peripheral Arterial Disease',
  # 'rheumatoid_arthritis' = 'Rheumatoid Arthritis',
  
  'heart_failure' = 'Heart Failure 1',
  'non_diabetic_hyperglycaemia' = 'Non-Diabetic Hyperglycaemia',
  # 'depression'='Depression',
  'asthma' = 'Asthma',
  'copd' = 'Chronic Obstructive Pulmonary Disease') %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  setnames(c('disease','Disease'))

base_population <- read.fst('./3_pre_main/intermediate_populations/time_one_population_w_deaths.fst')

hsct_pop_count <- base_population %>% 
  count(HSCT) %>% 
  mutate(pop = n*model_specification$population$scale_down_factor) %>% 
  select(-n)

prevalence_hsct <- read_excel("1_2_utils/data/DisPrevHsct_nisra_2324.xlsx", 
                              sheet = "Unpivoted") |> 
  filter(`Financial Year` == max(`Financial Year`),
         # `Statistic Label` == 'Raw disease prevalence per 1,000 patients'
         `Statistic Label` == 'Number of patients on the register'
  ) %>% 
  mutate(no = VALUE,#prob = VALUE/1000,
         'HSCT' = `Health and Social Care Trust`,
         Disease = Disease,
         Year=`Financial Year`,
         .keep = 'none') %>% 
  mutate(HSCT = case_when(
    HSCT == 'Belfast' ~ 'BHSCT',
    HSCT == 'Northern' ~ 'NHSCT',
    HSCT == 'Southern' ~ 'SHSCT',
    HSCT == 'South Eastern' ~ 'SEHSCT',
    HSCT == 'Western' ~ 'WHSCT',
    HSCT == 'NI Total' ~ 'Northern Ireland',
    TRUE ~ HSCT
  )) %>% 
  filter(HSCT !=  'Northern Ireland') %>% 
  left_join(df) %>% 
  left_join(hsct_pop_count) %>% 
  filter(!is.na(disease)) %>% 
  filter(!is.na(no))
# filter(!is.na(prob))

year = min(base_population$year)

np <- prevalence_hsct %>%
  mutate(prob=no/pop) %>% 
  pivot_wider(id_cols = HSCT, 
              names_from = disease, 
              values_from = no) 

names(np)[-1] <- paste0(names(np)[-1],'_prevalence_prob')


apply_qof_prevalence <- function(base_population){
  
  setDT(base_population)
  
  base_population <- base_population[as.data.table(np),on='HSCT']
  
  # Calculate percentiles only for rows with non-zero risk
  base_population[af_risk!=0, `:=` (atrial_fibrillation_disease_percentile = frank(ties.method = 'random',af_risk)/max(frank(ties.method = 'random',af_risk)),
                                    atrial_fibrillation_prevalence_prob = atrial_fibrillation_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[ckd_risk!=0, `:=` (chronic_kidney_disease_disease_percentile = frank(ties.method = 'random',ckd_risk)/max(frank(ties.method = 'random',ckd_risk)),
                                     chronic_kidney_disease_prevalence_prob = chronic_kidney_disease_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[chd_risk!=0, `:=` (chd_disease_percentile = frank(ties.method = 'random',chd_risk)/max(frank(ties.method = 'random',chd_risk)),
                                     chd_prevalence_prob = chd_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[dementia_risk!=0, `:=` (dementia_disease_percentile = frank(ties.method = 'random',dementia_risk)/max(frank(ties.method = 'random',dementia_risk)),
                                          dementia_prevalence_prob = dementia_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[hypertension_risk!=0, `:=` (hypertension_disease_percentile = frank(ties.method = 'random',hypertension_risk)/max(frank(ties.method = 'random',hypertension_risk)),
                                              hypertension_prevalence_prob = hypertension_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[stroke_risk!=0, `:=` (stroke_disease_percentile = frank(ties.method = 'random',stroke_risk)/max(frank(ties.method = 'random',stroke_risk)),
                                        stroke_prevalence_prob = stroke_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[diabetes_risk!=0, `:=` (diabetes_disease_percentile = frank(ties.method = 'random',diabetes_risk)/max(frank(ties.method = 'random',diabetes_risk)),
                                          diabetes_prevalence_prob = diabetes_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[heart_failure_year_risk!=0, `:=` (heart_failure_disease_percentile = frank(ties.method = 'random',heart_failure_year_risk)/max(frank(ties.method = 'random',heart_failure_year_risk)),
                                                    heart_failure_prevalence_prob = heart_failure_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[non_diabetic_hyperglycaemia_year_risk!=0, `:=` (non_diabetic_hyperglycaemia_disease_percentile = frank(ties.method = 'random',non_diabetic_hyperglycaemia_year_risk)/max(frank(ties.method = 'random',non_diabetic_hyperglycaemia_year_risk)),
                                                                  non_diabetic_hyperglycaemia_prevalence_prob = non_diabetic_hyperglycaemia_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[asthma_year_risk!=0, `:=` (asthma_disease_percentile = frank(ties.method = 'random',asthma_year_risk)/max(frank(ties.method = 'random',asthma_year_risk)),
                                             asthma_prevalence_prob = asthma_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  base_population[copd_year_risk!=0, `:=` (copd_disease_percentile = frank(ties.method = 'random',copd_year_risk)/max(frank(ties.method = 'random',copd_year_risk)),
                                           copd_prevalence_prob = copd_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
  
  # Initialize percentiles and prevalence_prob to 0 for rows with zero risk
  base_population[af_risk==0, `:=` (atrial_fibrillation_disease_percentile = 0, atrial_fibrillation_prevalence_prob = 0)]
  base_population[ckd_risk==0, `:=` (chronic_kidney_disease_disease_percentile = 0, chronic_kidney_disease_prevalence_prob = 0)]
  base_population[chd_risk==0, `:=` (chd_disease_percentile = 0, chd_prevalence_prob = 0)]
  base_population[dementia_risk==0, `:=` (dementia_disease_percentile = 0, dementia_prevalence_prob = 0)]
  base_population[hypertension_risk==0, `:=` (hypertension_disease_percentile = 0, hypertension_prevalence_prob = 0)]
  base_population[stroke_risk==0, `:=` (stroke_disease_percentile = 0, stroke_prevalence_prob = 0)]
  base_population[diabetes_risk==0, `:=` (diabetes_disease_percentile = 0, diabetes_prevalence_prob = 0)]
  base_population[heart_failure_year_risk==0, `:=` (heart_failure_disease_percentile = 0, heart_failure_prevalence_prob = 0)]
  base_population[non_diabetic_hyperglycaemia_year_risk==0, `:=` (non_diabetic_hyperglycaemia_disease_percentile = 0, non_diabetic_hyperglycaemia_prevalence_prob = 0)]
  base_population[asthma_year_risk==0, `:=` (asthma_disease_percentile = 0, asthma_prevalence_prob = 0)]
  base_population[copd_year_risk==0, `:=` (copd_disease_percentile = 0, copd_prevalence_prob = 0)]
  
  # Assign disease status probabilistically
  base_population <- base_population[,
                                     `:=` (
                                       atrial_fibrillation = fifelse(runif(.N) <  atrial_fibrillation_prevalence_prob/0.5 * atrial_fibrillation_disease_percentile ,year,0),
                                       chronic_kidney_disease = fifelse(runif(.N) <  chronic_kidney_disease_prevalence_prob/0.5 * chronic_kidney_disease_disease_percentile ,year,0),
                                       chd = fifelse(runif(.N) <  chd_prevalence_prob/0.5 * chd_disease_percentile ,year,0),
                                       dementia = fifelse(runif(.N) <  dementia_prevalence_prob/0.5 * dementia_disease_percentile ,year,0),
                                       hypertension = fifelse(runif(.N) <  hypertension_prevalence_prob/0.5 * hypertension_disease_percentile ,year,0),
                                       stroke = fifelse(runif(.N) <  stroke_prevalence_prob/0.5 * stroke_disease_percentile ,year,0),
                                       diabetes = fifelse(runif(.N) <  diabetes_prevalence_prob/0.5 * diabetes_disease_percentile ,year,0),
                                       heart_failure = fifelse(runif(.N) <  heart_failure_prevalence_prob/0.5 * heart_failure_disease_percentile ,year,0),
                                       non_diabetic_hyperglycaemia = fifelse(runif(.N) <  non_diabetic_hyperglycaemia_prevalence_prob/0.5 * non_diabetic_hyperglycaemia_disease_percentile ,year,0),
                                       asthma = fifelse(runif(.N) <  asthma_prevalence_prob/0.5 * asthma_disease_percentile ,year,0),
                                       copd = fifelse(runif(.N) <  copd_prevalence_prob/0.5 * copd_disease_percentile ,year,0)
                                     )
  ]
  
  base_population <- base_population[, c(
    'cancer_disease_percentile',
    'atrial_fibrillation_disease_percentile',
    'chronic_kidney_disease_disease_percentile',
    'chd_disease_percentile',
    'dementia_disease_percentile',
    'hypertension_disease_percentile',
    'stroke_disease_percentile',
    'diabetes_disease_percentile',
    'heart_failure_disease_percentile',
    'non_diabetic_hyperglycaemia_disease_percentile',
    'asthma_disease_percentile',
    'copd_disease_percentile') := NULL]
  
  base_population <- base_population[, c(
    'cancer_prevalence_prob',
    'atrial_fibrillation_prevalence_prob',
    'chronic_kidney_disease_prevalence_prob',
    'chd_prevalence_prob',
    'dementia_prevalence_prob',
    'hypertension_prevalence_prob',
    'stroke_prevalence_prob',
    'diabetes_prevalence_prob',
    'heart_failure_prevalence_prob',
    'non_diabetic_hyperglycaemia_prevalence_prob',
    'asthma_prevalence_prob',
    'copd_prevalence_prob') := NULL]
  
  
return(base_population)
}
