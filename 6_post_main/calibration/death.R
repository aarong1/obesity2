'site_cancers'

'Stroke'
'CHD'
'Heart Failure'
'Atrial Fibrillation' #### NOT MODELLED ####
'Hypertension'  #### NOT MODELLED ####
'Diabetes (1/2)'
'NDH' #### NOT MODELLED ####
'Cancer'  ### Site specific cancers ###

'Asthma' 
'COPD' 
'Dementia'

#### NOT MODELLED ####

'Hypothyroidism'
'Epilepsy'
'Osteoporosis'
'Rhematoid Arthritis'

'Oesteoarthritis'
'Abdominal Aortic Aneurysm '
'Systemic Lupus Erythematosus '
'Inflammatory Bowel Disease '
'Intersistal Lung Disease '
'Liver Disease' 

'~/Documents/SIB/PHM/PHModel/new_files/osteoarthritis.R'
'~/Documents/SIB/PHM/PHModel/new_files/abdominal_aortic_aneurysm.R'
'~/Documents/SIB/PHM/PHModel/new_files/sle.R'
'~/Documents/SIB/PHM/PHModel/new_files/IBD.R'
'~/Documents/SIB/PHM/PHModel/new_files/interstital_lung_disease.R'
'~/Documents/SIB/PHM/PHModel/new_files/liver_cirrhosis.R'

#### NOT MODELLED ####

all_cancers <- read_excel("data/NIcancer_registry/all_cancers_data_tables.xlsx", sheet = "T19", skip = 5)          %>% select(year=1,deaths=2)                   
bladder_cancer <- read_excel("data/NIcancer_registry/Bladder cancer data tables.xlsx", sheet = "T21", skip = 5)                          %>% select(year=1,deaths=2)           
blood_leukaemia_cancer <- read_excel("data/NIcancer_registry/Blood_leukaemia_data_tables.xlsx", sheet = "T19", skip = 5)                         %>% select(year=1,deaths=2)           
blood_lymphoma_cancer <- read_excel("data/NIcancer_registry/Blood_lymphoma_data_tables.xlsx", sheet = "T21", skip = 5)                          %>% select(year=1,deaths=2)           
blood_Multiple_cancer <- read_excel("data/NIcancer_registry/Blood_Multiple_myeloma_data_tables.xlsx", sheet = "T19", skip = 5)                  %>% select(year=1,deaths=2)           
brain_cancer <- read_excel("data/NIcancer_registry/Brain cancer data tables.xlsx", sheet = "T19", skip = 5)                            %>% select(year=1,deaths=2)           
cervical_cancer <- read_excel("data/NIcancer_registry/Cervical cancer data tables.xlsx", sheet = "T21", skip = 5)                         %>% select(year=1,deaths=2)           
female_breast <- read_excel("data/NIcancer_registry/Female_breast_cancer_data_tables.xlsx", sheet = "T25", skip = 5)                    %>% select(year=1,deaths=2)           
colorectal_cancer <- read_excel("data/NIcancer_registry/Colorectal_cancer_data_tables.xlsx", sheet = "T25", skip = 5)                %>% select(year=1,deaths=2)           
# Female_breast_insitu_tumour <-  read_excel("data/NIcancer_registry/Female_breast_insitu_tumour_data_tables.xlsx", sheet = "T13", skip = 5)           %>% select(year=1,deaths=2)             
gallbladder_cancer <- read_excel("data/NIcancer_registry/Gallbladder and other biliary cancer data tables.xlsx", sheet = "T19", skip = 5)    %>% select(year=1,deaths=2)           
kidney_cancer <- read_excel("data/NIcancer_registry/Kidney_cancer_data_tables.xlsx", sheet = "T21", skip = 5)                           %>% select(year=1,deaths=2)           
liver_cancer <- read_excel("data/NIcancer_registry/Liver cancer data tables.xlsx", sheet = "T21", skip = 5)                            %>% select(year=1,deaths=2)           
lung_cancer <- read_excel("data/NIcancer_registry/Lung_cancer_data_tables.xlsx", sheet = "T25", skip = 5)                             %>% select(year=1,deaths=2)           
#Male_breast_cancer_data_tables.xlsx <-  read_excel("data/NIcancer_registry/Male_breast_cancer_data_tables.xlsx", sheet = "T19", skip = 5)                    %>% select(year=1,deaths=2)             
oesophageal_cancer <- read_excel("data/NIcancer_registry/Oesophageal_cancer_data_tables.xlsx", sheet = "T23", skip = 5)                      %>% select(year=1,deaths=2)           
oral_cancer <- read_excel("data/NIcancer_registry/Oral_cancer_data_tables.xlsx", sheet = "T19", skip = 5)                             %>% select(year=1,deaths=2)           
ovarian_cancer <- read_excel("data/NIcancer_registry/Ovarian_cancer_data_tables.xlsx", sheet = "T21", skip = 5)                          %>% select(year=1,deaths=2)           
pancreatic_cancer <- read_excel("data/NIcancer_registry/Pancreatic_cancer_data_tables.xlsx", sheet = "T21", skip = 5)                       %>% select(year=1,deaths=2)           
prostate_cancer<- read_excel("data/NIcancer_registry/Prostate_cancer_data_tables.xlsx", sheet = "T23", skip = 5)                         %>% select(year=1,deaths=2)           
stomach_cancer <- read_excel("data/NIcancer_registry/Stomach_cancer_data_tables.xlsx", sheet = "T23", skip = 5)                          %>% select(year=1,deaths=2)           
testicular_cancer <- read_excel("data/NIcancer_registry/Testicular_cancer_data_tables.xlsx", sheet = "T20", skip = 5)                       %>% select(year=1,deaths=2)           
thyroid_cancer <- read_excel("data/NIcancer_registry/Thyroid cancer data tables.xlsx", sheet = "T21", skip = 5)                          %>% select(year=1,deaths=2)           
uterine_cancer <- read_excel("data/NIcancer_registry/Uterine_cancer_data_tables.xlsx", sheet = "T21", skip = 5)      %>% select(year=1,deaths=2)           

disease_lookups <- tribble(
  ~std, ~disease,
  'atrial_fibrillation', 'Atrial fibrillation and flutter',
  'COPD','Emphysema',
  'COPD', 'Other chronic obstructive pulmonary disease',
  #'Other interstitial pulmonary diseases',
  'chronic_kidney_disease', 'Chronic kidney disease',
  'dementia','Vascular dementia',
  'dementia','Unspecified dementia',
  'CHD', 'Angina pectoris',
  'CHD', 'Acute myocardial infarction',
  'CHD', 'Other acute ischaemic heart diseases',
  'stroke', 'Cerebrovascular disease (I60-I69)',
  'diabetes', 'Diabetes mellitus (E10-E14)',
  'asthma', 'Asthma (J45)',
  # 'Chronic liver disease (K70 K73-K74)',
  'cancer', 'Malignant neoplasms (C00-C97)',
  # 'Acute myocardial infarction (I21)' ,
  'heart_failure', 'Heart failure',
  'liver_disease', 'Chronic liver disease (K70 K73-K74)',
  "hypertension", 'Hypertensive diseases (I10-I15)',
  'intersistal_lung_disease', 'Other interstitial pulmonary diseases',
  'Inflammatory Bowel Disease ' = "Crohn's disease [regional enteritis]",
  'Inflammatory Bowel Disease ' = "Ulcerative colitis",
  'Inflammatory Bowel Disease ' = "Other noninfective gastroenteritis and colitis",
  'atrial_fibrillation' = 'Atrial fibrillation and flutter',
  'aortic_aneurysm' = 'Aortic aneurysm and dissection'
  
)

deaths_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                           sheet = "Table 6.2", range = 'A64:P122')%>% mutate(sex ='Males')

deaths_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                             sheet = "Table 6.2", range = 'A124:P182') %>% mutate(sex ='Females')

names(deaths_males) <- names(deaths_females)
deaths <- rbind(deaths_males, deaths_females)
deaths <- deaths %>% 
  mutate(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '85-89','90+'),~ .x * 19 )) %>%
  select(ICD10 = `ICD-10 Codes`,
                            Cause = `Cause of Death (ICD Code)`,
                            everything())

other_death <- deaths %>% 
  filter(!(Cause %in% disease_lookups$disease)) %>% 
  mutate(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '85-89','90+'),~ .x * 19)) %>% 
  summarise(deaths = sum(`All Ages`))  

detailed_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                             sheet = "Table 6.4a", range = 'AA4:AY845') %>% mutate(sex = 'Males')

detailed_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                               sheet = "Table 6.4a", range = 'BA4:BY845') %>% mutate(sex = 'Females')

detailed <- rbind(detailed_males, detailed_females)

deaths <- deaths %>% rename(Description = Cause) 

detailed <- detailed %>% rename(ICD10 = ICD) %>% select(-c(Chapter,Block))

#different gradations of rage band !!

deaths <- deaths %>%  select(ICD10, Description, `All Ages`)
detailed <- detailed %>%  select(ICD10, Description, `All Ages`)

all_diseases <- rbind(deaths, 
           detailed) %>% 
  filter(Description %in% disease_lookups$disease) %>% 
  left_join(disease_lookups, by = c('Description' = 'disease')) %>% 
  # mutate(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '85-89','90+'),~ .x*1)) %>% 
  group_by(std) %>%
  summarise(deaths = sum(`All Ages`))  


past_populations %>%
  count(death,run,death_reason) %>% 
  group_by(death,run,death_reason) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor) %>% 
  filter(death!=0)

bladder_cancer <- bladder_cancer %>% mutate(morbidity = 'bladder_cancer')
blood_leukaemia_cancer <- blood_leukaemia_cancer %>% mutate(morbidity = 'blood_leukaemia_cancer')
blood_lymphoma_cancer <- blood_lymphoma_cancer %>% mutate(morbidity = 'blood_lymphoma_cancer')
blood_Multiple_cancer <- blood_Multiple_cancer %>% mutate(morbidity = 'blood_Multiple_cancer')
brain_cancer <- brain_cancer %>% mutate(morbidity = 'brain_cancer')
cervical_cancer <- cervical_cancer %>% mutate(morbidity = 'cervical_cancer')
female_breast <- female_breast %>% mutate(morbidity = 'female_breast')
colorectal_cancer <- colorectal_cancer %>% mutate(morbidity = 'colorectal_cancer')
gallbladder_cancer <- gallbladder_cancer %>% mutate(morbidity = 'gallbladder_cancer')
kidney_cancer <- kidney_cancer %>% mutate(morbidity = 'kidney_cancer')
liver_cancer <- liver_cancer %>% mutate(morbidity = 'liver_cancer')
lung_cancer <- lung_cancer %>% mutate(morbidity = 'lung_cancer')
oesophageal_cancer <- oesophageal_cancer %>% mutate(morbidity = 'oesophageal_cancer')
oral_cancer <- oral_cancer %>% mutate(morbidity = 'oral_cancer')
ovarian_cancer <- ovarian_cancer %>% mutate(morbidity = 'ovarian_cancer')
pancreatic_cancer <- pancreatic_cancer %>% mutate(morbidity = 'pancreatic_cancer')
prostate_cancer <- prostate_cancer %>% mutate(morbidity = 'prostate_cancer')
stomach_cancer <- stomach_cancer %>% mutate(morbidity = 'stomach_cancer')

