library(readxl)
library(tidyverse)


proj18 <- read_excel("data/SNPP18_SYA_Age_Bands.xlsx", 
                     sheet = "Tabular Single Year of Age", 
                     skip = 15)

births <- proj18 |> 
  filter(!Gender %in% c('All Persons','All Persons - All ages'),
         Area %in% '2. Local Government Districts',
         Age == 0) |> 
  mutate(Gender = ifelse(Gender == 'Female', 'Females', 'Males')) |> 
  pivot_longer(cols = -c(1:4),names_to = 'year',values_to='births') |> 
  mutate(year = as.numeric(year)) |> 
  select(lgd14 = 2, sex = Gender, year, age = Age, , births) |> 
  mutate(synth_scale_births = births/ model_specification$population$scale_down_factor) |> 
  select(-age)
  # mutate()

# count(births, lgd14)


new_people <- initial_time_zero_population |> 
  filter(sex == 'Females', between(age, 15, 44)) |>
  mutate(LGD2014NAME = str_remove_all(LGD2014NAME,',')) |> 
  left_join(births, 
            by = join_by(sex, year, LGD2014NAME == lgd14),
            relationship = 'many-to-many',keep=F) |> #View()
  # count(LGD2014NAME,is.na(synth_scale_births)) |> 
  group_by(sex, LGD2014NAME) |> 
  mutate(
    synth_scale_prop_births = births/ n() /model_specification$population$scale_down_factor) |> 
  # count(LGD2014NAME,
  #            sex,
  #            synth_scale_prop_births) |>  count(LGD2014NAME,
  #                                               sex) |> 
  rowwise() |> 
    mutate(prop = sample(x = c(1,0),
                        prob = c(synth_scale_prop_births,
                                 1-synth_scale_prop_births),
                        size=1) ) |> 
  filter(prop==1) |> 
 select(
   !c(births,
      synth_scale_births, 
      synth_scale_prop_births,
      prop)
        )

# { View(.); . } |>

names(new_people)

new_people <- new_people |> 
  mutate(id = runif(n())*100000000,
         age = 0, 
         age20 = '0-20', 
         age10 = '0-10',
         qrisk_score = NA,
         qrisk_year_risk = NA,
         stroke_risk = NA,
         stroke_year_risk = NA,
         chd_risk = NA,
         chd_year_risk = NA,
         diabetes_risk = NA,
         diabetes_year_risk = NA,
         dementia_risk = NA,
         dementia_year_risk = NA,
         hf_risk = NA,
         heart_failure_year_risk = NA,
         hypertension_risk = NA,
         hypertension_year_risk = NA,
         af_risk = NA,
         atrial_fibrillation_year_risk = NA,
         ckd_risk = NA,
         chronic_kidney_disease_year_risk = NA,
         pad_risk = NA,
         pad_year_risk = NA,
         vte_risk = NA,
         vte_year_risk = NA,
         liver_risk = NA,
         liver_year_risk = NA,
         lungcancer_risk = NA,
         lung_cancer_year_risk = NA,
         death = NA,
         death_reason = NA,
         qmortality_risk = NA,
         qx = NA,
         bmi = NA,
         smoking = NA,
         percentage = NA,
         alcohol = NA,
         diet = NA,
         pa = NA,
         stroke = NA,
         chd = NA,
         diabetes = NA,
         dementia = NA,
         heart_failure = NA,
         atrial_fibrillation = NA,
         hypertension = NA,
         chronic_kidney_disease = NA,
         lung_cancer = NA,
         stroke_recovered = NA,
         chd_recovered = NA,
         diabetes_recovered = NA,
         dementia_recovered = NA,
         heart_failure_recovered = NA,
         atrial_fibrillation_recovered = NA,
         hypertension_recovered = NA,
         chronic_kidney_disease_recovered = NA,
         lung_cancer_recovered = NA,) |> 
  
  select(
id,
year,                            
run,                             
soa_code,                        
soa_name,                        
sa_name,                         
sa_code,                         
LGD2014NAME,                     
HSCT,                            
DEA2014_name,                    
DEA2014_code,                    
AA2008,                          
Urban,                           
sex,                             
age,
sa_pop,                          
mdm_rank,                        
townsend_score,                  
townsend_quintile,               
mdm_quintile,                    
mdm_decile,                      
income_dm_quintile,              
income_dm_decile,                
employment_dm_quintile,          
employment_dm_decile,            
sa_population_density,           
soa_population_density,          
age20,
ethnicity,                       
broad_ethnicity,                 

stroke,
chd,
diabetes,
dementia,
heart_failure,
atrial_fibrillation,
hypertension,
chronic_kidney_disease,
lung_cancer,
stroke_recovered,
chd_recovered,
diabetes_recovered,
dementia_recovered,
heart_failure_recovered,
atrial_fibrillation_recovered,
hypertension_recovered,
chronic_kidney_disease_recovered,
lung_cancer_recovered,

ethrisk,                         
bmi_percentile,                  
smoking_percentile,              
depression_percentile,           
alcohol_percentile,              
diet_percentile,                 
physical_activity_percentile,    
age10,                       
deprivation,                     
hsct,                            
geo,            

bmi,
smoking,
percentage,
alcohol,
diet,
pa,

qrisk_score,
qrisk_year_risk,
stroke_risk,
stroke_year_risk,
chd_risk,
chd_year_risk,
diabetes_risk,
diabetes_year_risk,
dementia_risk,
dementia_year_risk,
hf_risk,
heart_failure_year_risk,
hypertension_risk,
hypertension_year_risk,
af_risk,
atrial_fibrillation_year_risk,
ckd_risk,
chronic_kidney_disease_year_risk,
pad_risk,
pad_year_risk,
vte_risk,
vte_year_risk,
liver_risk,
liver_year_risk,
lungcancer_risk,
lung_cancer_year_risk,
death,
death_reason,
qmortality_risk,
qx,
intervention
)

rbind(new_people,initial_time_zero_population) |> View()

add_yearly_births <- function(current_population){
  
  
  
  new_people <- current_population |> 
    filter(sex == 'Females', between(age, 15, 44)) |>
    mutate(LGD2014NAME = str_remove_all(LGD2014NAME,',')) |> 
    left_join(births, 
              by = join_by(sex, year, LGD2014NAME == lgd14),
              relationship = 'many-to-many',keep=F) |> #View()
    # count(LGD2014NAME,is.na(synth_scale_births)) |> 
    group_by(sex, LGD2014NAME) |> 
    mutate(
      synth_scale_prop_births = births/ n() /model_specification$population$scale_down_factor) |> 
    # count(LGD2014NAME,
    #            sex,
    #            synth_scale_prop_births) |>  count(LGD2014NAME,
    #                                               sex) |> 
    rowwise() |> 
    mutate(prop = sample(x = c(1,0),
                         prob = c(synth_scale_prop_births,
                                  1-synth_scale_prop_births),
                         size=1) ) |> 
    filter(prop==1) |> 
    select(
      !c(births,
         synth_scale_births, 
         synth_scale_prop_births,
         prop)
    )
  
  # { View(.); . } |>
  
  names(new_people)
  
  new_people <- new_people |> 
    mutate(id = runif(n())*100000000,
           age = 0, 
           age20 = '0-20', 
           age10 = '0-10',
           qrisk_score = NA,
           qrisk_year_risk = NA,
           stroke_risk = NA,
           stroke_year_risk = NA,
           chd_risk = NA,
           chd_year_risk = NA,
           diabetes_risk = NA,
           diabetes_year_risk = NA,
           dementia_risk = NA,
           dementia_year_risk = NA,
           hf_risk = NA,
           heart_failure_year_risk = NA,
           hypertension_risk = NA,
           hypertension_year_risk = NA,
           af_risk = NA,
           atrial_fibrillation_year_risk = NA,
           ckd_risk = NA,
           chronic_kidney_disease_year_risk = NA,
           pad_risk = NA,
           pad_year_risk = NA,
           vte_risk = NA,
           vte_year_risk = NA,
           liver_risk = NA,
           liver_year_risk = NA,
           lungcancer_risk = NA,
           lung_cancer_year_risk = NA,
           death = NA,
           death_reason = NA,
           qmortality_risk = NA,
           qx = NA,
           bmi = NA,
           smoking = NA,
           percentage = NA,
           alcohol = NA,
           diet = NA,
           pa = NA,
           stroke = NA,
           chd = NA,
           diabetes = NA,
           dementia = NA,
           heart_failure = NA,
           atrial_fibrillation = NA,
           hypertension = NA,
           chronic_kidney_disease = NA,
           lung_cancer = NA,
           stroke_recovered = NA,
           chd_recovered = NA,
           diabetes_recovered = NA,
           dementia_recovered = NA,
           heart_failure_recovered = NA,
           atrial_fibrillation_recovered = NA,
           hypertension_recovered = NA,
           chronic_kidney_disease_recovered = NA,
           lung_cancer_recovered = NA,) |> 
    
    select(
      id,
      year,                            
      run,                             
      soa_code,                        
      soa_name,                        
      sa_name,                         
      sa_code,                         
      LGD2014NAME,                     
      HSCT,                            
      DEA2014_name,                    
      DEA2014_code,                    
      AA2008,                          
      Urban,                           
      sex,                             
      age,
      sa_pop,                          
      mdm_rank,                        
      townsend_score,                  
      townsend_quintile,               
      mdm_quintile,                    
      mdm_decile,                      
      income_dm_quintile,              
      income_dm_decile,                
      employment_dm_quintile,          
      employment_dm_decile,            
      sa_population_density,           
      soa_population_density,          
      age20,
      ethnicity,                       
      broad_ethnicity,                 
      
      stroke,
      chd,
      diabetes,
      dementia,
      heart_failure,
      atrial_fibrillation,
      hypertension,
      chronic_kidney_disease,
      lung_cancer,
      stroke_recovered,
      chd_recovered,
      diabetes_recovered,
      dementia_recovered,
      heart_failure_recovered,
      atrial_fibrillation_recovered,
      hypertension_recovered,
      chronic_kidney_disease_recovered,
      lung_cancer_recovered,
      
      ethrisk,                         
      bmi_percentile,                  
      smoking_percentile,              
      depression_percentile,           
      alcohol_percentile,              
      diet_percentile,                 
      physical_activity_percentile,    
      age10,                       
      deprivation,                     
      hsct,                            
      geo,            
      
      bmi,
      smoking,
      percentage,
      alcohol,
      diet,
      pa,
      
      qrisk_score,
      qrisk_year_risk,
      stroke_risk,
      stroke_year_risk,
      chd_risk,
      chd_year_risk,
      diabetes_risk,
      diabetes_year_risk,
      dementia_risk,
      dementia_year_risk,
      hf_risk,
      heart_failure_year_risk,
      hypertension_risk,
      hypertension_year_risk,
      af_risk,
      atrial_fibrillation_year_risk,
      ckd_risk,
      chronic_kidney_disease_year_risk,
      pad_risk,
      pad_year_risk,
      vte_risk,
      vte_year_risk,
      liver_risk,
      liver_year_risk,
      lungcancer_risk,
      lung_cancer_year_risk,
      death,
      death_reason,
      qmortality_risk,
      qx,
      intervention
    )
  

  rbind(new_people,current_population)
  
  
}

initial_time_zero_population |> 
  add_yearly_births() |> 
  View()

add_yearly_births <- function(current_population){
  
  
  
  new_people <- current_population |> 
    filter(sex == 'Females', between(age, 15, 44)) |>
    mutate(LGD2014NAME = str_remove_all(LGD2014NAME,',')) |> 
    left_join(births, 
              by = join_by(sex, year, LGD2014NAME == lgd14),
              relationship = 'many-to-many',keep=F) |> #View()
    # count(LGD2014NAME,is.na(synth_scale_births)) |> 
    group_by(sex, LGD2014NAME) |> 
    mutate(
      synth_scale_prop_births = births/ n() /model_specification$population$scale_down_factor) |> 
    # count(LGD2014NAME,
    #            sex,
    #            synth_scale_prop_births) |>  count(LGD2014NAME,
    #                                               sex) |> 
    rowwise() |> 
    mutate(prop = sample(x = c(1,0),
                         prob = c(synth_scale_prop_births,
                                  1-synth_scale_prop_births),
                         size=1) ) |> 
    filter(prop==1) |> 
    select(
      !c(births,
         synth_scale_births, 
         synth_scale_prop_births,
         prop)
    )
  
  # { View(.); . } |>
  
  names(new_people)
  
  new_people <- new_people |> 
    mutate(id = runif(n())*100000000,
           age = 0, 
           age20 = '0-20', 
           age10 = '0-10',
           qrisk_score = NA,
           qrisk_year_risk = NA,
           stroke_risk = NA,
           stroke_year_risk = NA,
           chd_risk = NA,
           chd_year_risk = NA,
           diabetes_risk = NA,
           diabetes_year_risk = NA,
           dementia_risk = NA,
           dementia_year_risk = NA,
           hf_risk = NA,
           heart_failure_year_risk = NA,
           hypertension_risk = NA,
           hypertension_year_risk = NA,
           af_risk = NA,
           atrial_fibrillation_year_risk = NA,
           ckd_risk = NA,
           chronic_kidney_disease_year_risk = NA,
           pad_risk = NA,
           pad_year_risk = NA,
           vte_risk = NA,
           vte_year_risk = NA,
           liver_risk = NA,
           liver_year_risk = NA,
           lungcancer_risk = NA,
           lung_cancer_year_risk = NA,
           death = NA,
           death_reason = NA,
           qmortality_risk = NA,
           qx = NA,
           bmi = NA,
           smoking = NA,
           percentage = NA,
           alcohol = NA,
           diet = NA,
           pa = NA,
           stroke = NA,
           chd = NA,
           diabetes = NA,
           dementia = NA,
           heart_failure = NA,
           atrial_fibrillation = NA,
           hypertension = NA,
           chronic_kidney_disease = NA,
           lung_cancer = NA,
           stroke_recovered = NA,
           chd_recovered = NA,
           diabetes_recovered = NA,
           dementia_recovered = NA,
           heart_failure_recovered = NA,
           atrial_fibrillation_recovered = NA,
           hypertension_recovered = NA,
           chronic_kidney_disease_recovered = NA,
           lung_cancer_recovered = NA,) |> 
    
    select(
      id,
      year,                            
      run,                             
      soa_code,                        
      soa_name,                        
      sa_name,                         
      sa_code,                         
      LGD2014NAME,                     
      HSCT,                            
      DEA2014_name,                    
      DEA2014_code,                    
      AA2008,                          
      Urban,                           
      sex,                             
      age,
      sa_pop,                          
      mdm_rank,                        
      townsend_score,                  
      townsend_quintile,               
      mdm_quintile,                    
      mdm_decile,                      
      income_dm_quintile,              
      income_dm_decile,                
      employment_dm_quintile,          
      employment_dm_decile,            
      sa_population_density,           
      soa_population_density,          
      age20,
      ethnicity,                       
      broad_ethnicity,                 
      
      stroke,
      chd,
      diabetes,
      dementia,
      heart_failure,
      atrial_fibrillation,
      hypertension,
      chronic_kidney_disease,
      lung_cancer,
      stroke_recovered,
      chd_recovered,
      diabetes_recovered,
      dementia_recovered,
      heart_failure_recovered,
      atrial_fibrillation_recovered,
      hypertension_recovered,
      chronic_kidney_disease_recovered,
      lung_cancer_recovered,
      
      ethrisk,                         
      bmi_percentile,                  
      smoking_percentile,              
      depression_percentile,           
      alcohol_percentile,              
      diet_percentile,                 
      physical_activity_percentile,    
      age10,                       
      deprivation,                     
      hsct,                            
      geo,            
      
      bmi,
      smoking,
      percentage,
      alcohol,
      diet,
      pa,
      
      qrisk_score,
      qrisk_year_risk,
      stroke_risk,
      stroke_year_risk,
      chd_risk,
      chd_year_risk,
      diabetes_risk,
      diabetes_year_risk,
      dementia_risk,
      dementia_year_risk,
      hf_risk,
      heart_failure_year_risk,
      hypertension_risk,
      hypertension_year_risk,
      af_risk,
      atrial_fibrillation_year_risk,
      ckd_risk,
      chronic_kidney_disease_year_risk,
      pad_risk,
      pad_year_risk,
      vte_risk,
      vte_year_risk,
      liver_risk,
      liver_year_risk,
      lungcancer_risk,
      lung_cancer_year_risk,
      death,
      death_reason,
      qmortality_risk,
      qx,
      intervention
    )
  
  print(names(new_people))
  rbind(new_people,current_population)
  
  
}

initial_time_zero_population |> 
  add_yearly_births() |> 
  View()


add_basic_yearly_births <- function(current_population){
  
  
  
  new_people <- current_population |> 
    filter(sex == 'Females', between(age, 15, 44)) |>
    mutate(LGD2014NAME = str_remove_all(LGD2014NAME,',')) |> 
    left_join(births, 
              by = join_by(sex, year, LGD2014NAME == lgd14),
              relationship = 'many-to-many',keep=F) |> #View()
    # count(LGD2014NAME,is.na(synth_scale_births)) |> 
    group_by(sex, LGD2014NAME) |> 
    mutate(
      synth_scale_prop_births = births/ n() /model_specification$population$scale_down_factor) |> 
    # count(LGD2014NAME,
    #            sex,
    #            synth_scale_prop_births) |>  count(LGD2014NAME,
    #                                               sex) |> 
    rowwise() |> 
    mutate(prop = sample(x = c(1,0),
                         prob = c(synth_scale_prop_births,
                                  1-synth_scale_prop_births),
                         size=1) ) |> 
    filter(prop==1) |> 
    select(
      !c(births,
         synth_scale_births, 
         synth_scale_prop_births,
         prop)
    )
  
  # { View(.); . } |>
  
  names(new_people)
  
  new_people <- new_people |> 
    mutate(id = runif(n())*100000000,
           age = 0, 
           age20 = '0-20', 
           age10 = '0-10',
           qrisk_score = NA,
           qrisk_year_risk = NA,
           stroke_risk = NA,
           stroke_year_risk = NA,
           chd_risk = NA,
           chd_year_risk = NA,
           diabetes_risk = NA,
           diabetes_year_risk = NA,
           dementia_risk = NA,
           dementia_year_risk = NA,
           hf_risk = NA,
           heart_failure_year_risk = NA,
           hypertension_risk = NA,
           hypertension_year_risk = NA,
           af_risk = NA,
           atrial_fibrillation_year_risk = NA,
           ckd_risk = NA,
           chronic_kidney_disease_year_risk = NA,
           pad_risk = NA,
           pad_year_risk = NA,
           vte_risk = NA,
           vte_year_risk = NA,
           liver_risk = NA,
           liver_year_risk = NA,
           lungcancer_risk = NA,
           lung_cancer_year_risk = NA,
           death = NA,
           death_reason = NA,
           qmortality_risk = NA,
           qx = NA,
           bmi = NA,
           smoking = NA,
           percentage = NA,
           alcohol = NA,
           diet = NA,
           pa = NA,
           stroke = NA,
           chd = NA,
           diabetes = NA,
           dementia = NA,
           heart_failure = NA,
           atrial_fibrillation = NA,
           hypertension = NA,
           chronic_kidney_disease = NA,
           lung_cancer = NA,
           stroke_recovered = NA,
           chd_recovered = NA,
           diabetes_recovered = NA,
           dementia_recovered = NA,
           heart_failure_recovered = NA,
           atrial_fibrillation_recovered = NA,
           hypertension_recovered = NA,
           chronic_kidney_disease_recovered = NA,
           lung_cancer_recovered = NA,) |> 
    
    select(
      id,                              
      year,                            
      run,                             
      soa_code,                        
      soa_name,                        
      sa_name,                         
      sa_code,                         
      LGD2014NAME,                     
      HSCT,                            
      DEA2014_name,                    
      DEA2014_code,                    
      AA2008,                          
      Urban,                           
      sex,                             
      age,                             
      sa_pop,                          
      mdm_rank,                        
      townsend_score,                  
      townsend_quintile,               
      mdm_quintile,                    
      mdm_decile,                      
      income_dm_quintile,              
      income_dm_decile,                
      employment_dm_quintile,          
      employment_dm_decile,            
      sa_population_density,           
      soa_population_density,          
      age20,                           
      ethnicity,                       
      broad_ethnicity,                 
      stroke,                          
      chd,                             
      diabetes,                        
      dementia,                        
      heart_failure,                   
      atrial_fibrillation,             
      hypertension,                    
      chronic_kidney_disease,          
      lung_cancer,                     
      stroke_recovered,                
      chd_recovered,                   
      diabetes_recovered,              
      dementia_recovered,              
      heart_failure_recovered,         
      atrial_fibrillation_recovered,   
      hypertension_recovered,          
      chronic_kidney_disease_recovered,
      lung_cancer_recovered,           
      ethrisk,                         
      death,                           
      death_reason,                    
      qmortality_risk,                 
      qx,                              
      intervention
    )
  
  
  rbind(new_people,current_population)
  
  
}

initial_time_zero_population |> 
  add_basic_yearly_births() |> 
  View()



