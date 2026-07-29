# PAF

library(echarts4r)
##PAF

# BMIs were categorised as obesity (≥30·0 kg/m2), overweight (25·0–29·9 kg/m2), healthy weight (18·5–24·9 kg/m2), and underweight (<18·5 kg/m2). 

lookups <- tribble(
  ~broad_disease, ~disease, ~disease_pretty,
  'CVD', 'stroke', 'Stroke',
  'CVD', 'chd', 'CHD',
  'CVD', 'diabetes', 'Diabetes',
  'CVD', 'heart_failure', 'Heart Failure',
  'CVD', 'chronic_kidney_disease', 'Chronic Kidney Disease',
  'CVD', 'atrial_fibrillation', 'Atrial Fibrillation',
  'CVD', 'hypertension', 'Hypertension',
  'Neurological', 'dementia', 'Dementia',
  'Respiratory', 'asthma', 'Asthma',
  'Respiratory', 'copd', 'COPD',
  'MSK', 'rheumatoid_arthritis', 'Rheumatoid Arthritis',
  'MSK', 'osteoarthritis', 'Osteoarthritis',
  'Cancer', 'cancer', 'All Cancer',
  'Cancer', 'lung_cancer', 'Lung Cancer',
  'Cancer', 'colorectal_cancer', 'Colorectal Cancer',
  'Cancer', 'renal_cancer', 'Renal Cancer'  )


m <- c('stroke',
       'chd',
       'diabetes',
       'heart_failure',
       'chronic_kidney_disease',
       'dementia',
       
       'atrial_fibrillation',
       'hypertension',
       'asthma',
       'copd',
       
       'rheumatoid_arthritis',
       'osteoarthritis',
       
       'cancer',
       'lung_cancer',
       'colorectal_cancer',
       'renal_cancer')


w_bmi <- read.fst(paste0('./PAF_intermediate_populations/w_bmi.fst'))
no_bmi <- read.fst(paste0('./PAF_intermediate_populations/no_bmi.fst'))

w_smoking <- read.fst(paste0('./PAF_intermediate_populations/w_smoking.fst'))
no_smoking <- read.fst(paste0('./PAF_intermediate_populations/no_smoking.fst'))


count(w_bmi,stroke)

count(w_bmi, bmi)
count(no_bmi,bmi)

count(w_smoking, smoking)
count(no_smoking,smoking)

w <- w_bmi %>% 
  summarise(across(.cols = paste0(m,'_year_risk'),.fns = function(x)sum(x)))

wo <- no_bmi %>% 
  summarise(across(.cols = paste0(m,'_year_risk'),.fns = function(x)sum(x)))

ws <- w_smoking %>% 
  summarise(across(.cols = paste0(m,'_year_risk'),.fns = function(x)sum(x)))

wos <- no_smoking %>% 
  summarise(across(.cols = paste0(m,'_year_risk'),.fns = function(x)sum(x)))


data.frame(d = names(w), w = unlist(w), wo = unlist(wo), ws = unlist(ws), wos = unlist(wos))

xy <- cbind(w = t(w), wo = t(wo), wos = t(wos), ws = t(ws)) %>% 
  as.data.frame() %>% 
  setNames(c('w','wo','ws','wos')) %>% 
  rownames_to_column('disease') %>% 
  mutate(disease = str_remove(disease,'_year_risk')) %>%
  left_join(lookups, by = c('disease'='disease'))


# BMI

paf_bmi <-  xy %>% 
  group_by(disease_pretty) %>% 
  mutate(diff= (w-wo)/w) %>% 
  filter(diff>0) %>%
  e_charts(disease_pretty) %>% 
  e_bar(diff, stack = 'b') %>% 
  e_grid(bottom='20%') %>% 
  e_flip_coords() %>% 
  e_axis( axis = 'x', formatter = e_axis_formatter('percent')) %>% 
  e_aria(enabled = T, decal = list(show = TRUE)) %>%
  
  e_theme('azul')  %>% 
  
  e_tooltip(backgroundColor = 'white',
            formatter=e_tooltip_item_formatter('percent'))


# Same PAF metric, grouped by broad disease and colored by disease type
absf_bmi <- xy %>%
  mutate(diff = ifelse(w > 0, (w - wo)*model_specification$population$scale_down_factor , NA_real_)) %>%
  filter(diff != 0) %>%
  # mutate(diff = diff * model_specification$population$scale_down_factor) %>% 
  group_by(disease_pretty) %>% 
  e_charts(broad_disease) %>%
  e_bar(diff, stack = 'd', bind = disease_pretty,endLabel = list(show = T)) %>%
  e_flip_coords() %>% 
  e_grid(bottom='20%') %>% 
  e_aria(enabled = T, decal = list(show = TRUE)) %>%
  e_theme('azul')  %>% 
  
  # e_labels() %>% 
  e_tooltip(backgroundColor = 'white',formatter = e_tooltip_item_formatter('decimal',digits=0))

#smoking ----

xy %>% 
  group_by(disease_pretty) %>% 
  mutate(diff= (wos-ws)/wos) %>% 
  filter(diff>0) %>%
  e_charts(disease_pretty) %>% 
  e_bar(diff, stack = 'b') %>%
  e_grid(bottom='20%') %>% 
  e_flip_coords() %>% 
  e_axis( axis = 'x', formatter = e_axis_formatter('percent')) %>% 
  e_tooltip()


# Same PAF metric, grouped by broad disease and colored by disease type
xy %>%
  mutate(diff = ifelse(w > 0, (wos - ws)*10 , NA_real_)) %>%
  filter(diff != 0) %>%
  group_by(disease_pretty) %>% 
  e_charts(broad_disease) %>%
  e_bar(diff, stack = 'd', bind = disease_pretty,endLabel = list(show = T)) %>%
  e_flip_coords() %>% 
  e_grid(bottom='20%') %>% 
  # e_labels() %>% 
  e_tooltip()


write_rds(x = paf_bmi, './stored_results/paf_bmi.rds')
write_rds(x = absf_bmi, './stored_results/absf_bmi.rds')
          
          

