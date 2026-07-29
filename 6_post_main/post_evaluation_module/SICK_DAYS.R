# cancer
# CVD
# respiratory

# The HIS provides information on admitted patient care delivered by Health and Social Care
# Hospitals in Northern Ireland. It is a patient-level administrative data source and each record
# relates to an individual consultant episode. During a single hospital admission, a patient may
# be transferred from the care of one consultant to another, generating an additional consultant
# episode. Episode-based data is therefore not equivalent to admission-based data, as each
# admission may be made up of one or more consultant episodes.
## https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-episode-based-activity-additional-info-explanatory-notes.pdf -----

unique(past_populations$year)
unique(past_populations$run)

morb_cols <- c(
'cvd'='pad',
'cvd'='chronic_kidney_disease',
'resp'='copd',                                 
'resp'='asthma',                               
'other'='depression',                           
'msk'='osteoporosis',                         
'cancer'='cancer',                               
'msk'='osteoarthritis',             
'msk'='rheumatoid_arthritis',       
'other'='epilepsy',                  
'other'='hypothyroidism',
'resp'='asthma',                     
'resp'='copd',                       
'other'='depression',                 
'cvd'='non_diabetic_hyperglycaemia',
'cancer'='colorectal_cancer',          
'cancer'='prostate_cancer',           
'cancer'='female_breast_cancer',      
'cancer'='renal_cancer',                         
'cancer'='oesophageal_cancer',                   
'cancer'='stomach_cancer',                       
'cancer'='osteogastric_cancer',                  
'cancer'='oral_cancer',                          
'cancer'='pancreatic_cancer',                    
'cancer'='uterine_cancer',            
'cancer'='blood_multiple_myeloma',               
'cancer'='blood_lymphoma',                       
'cancer'='blood_leukaemia',                      
'cancer'='blood_cancer',                         
'cancer'='ovarian_cancer',            
'cancer'='lung_cancer',
'cvd'='stroke',
'cvd'='chd',
'cvd'='diabetes',
'other'='dementia',
'cvd'='heart_failure',
'cvd'='atrial_fibrillation',
'cvd'='hypertension',
'cvd'='chronic_kidney_disease')


#' morb_cols <- c(
#'  'cvd'= 'pad',
#'  'cvd'= 'chronic_kidney_disease',
#'   #'cvd'= 'vte',
#'  'cvd'='diabetes',
#'   'rheumatoid_arthritis',                 
#'   'copd',                                 
#'   'asthma',                               
#'   'depression',                           
#'   'non_diabetic_hyperglycaemia',          
#'   'osteoporosis',                         
#'   'cancer',                               
#'   'osteoporosis',               
#'   'osteoarthritis',             
#'   'rheumatoid_arthritis',       
#'   'epilepsy',                  
#'   'hypothyroidism',
#'   'asthma',                     
#'   'copd',                       
#'   'depression',                 
#'   'non_diabetic_hyperglycaemia',
#'   'colorectal_cancer',          
#'   'prostate_cancer',           
#'   'female_breast_cancer',      
#'   'renal_cancer',                         
#'   'oesophageal_cancer',                   
#'   'stomach_cancer',                       
#'   'osteogastric_cancer',                  
#'   'oral_cancer',                          
#'   'pancreatic_cancer',                    
#'   'uterine_cancer',            
#'   'blood_multiple_myeloma',               
#'   'blood_lymphoma',                       
#'   'blood_leukaemia',                      
#'   'blood_cancer',                         
#'   'ovarian_cancer',            
#'   'colorectal_cancer',  
#'   'lung_cancer',
#'   'stroke',
#'   'chd',
#'   'diabetes',
#'   'dementia',
#'   'heart_failure',
#'   'atrial_fibrillation',
#'   'hypertension',
#'   'chronic_kidney_disease')

# melt to long
m <- melt(
  past_populations,
  id.vars = c("year", "run"),
  measure.vars = morb_cols,
  variable.name = "variable",
  value.name = "value"
)


m[data.table(variable = unname(morb_cols), 
              broad = names(morb_cols)), on = .(variable), broad := i.broad]

# count non-zero per year/run/variable
m[, n := sum(value != 0), by = .(year, run, variable,broad)]

# keep unique rows
# m <- unique(m[, .(year, run, variable,broad, n)])

# complete grid of year/run/variable
grid <- CJ(
  year     = unique(m$year),
  run      = unique(m$run),
  variable = unique(m$variable)
)

m2 <- m[grid, on = .(year, run, variable), nomatch=0L]
m2[is.na(n), n := 0L]

# mean across runs
res_dt <- m2[, .(prevalence = mean(n)), by = .(year, variable, broad)]

res_dt <- res_dt[, .(prevalence = sum(prevalence)), by = .(year, broad)]



working_days_lost = 308601.83
working_days_lost_per_staff_yr = 13.809

staff_years = working_days_lost/working_days_lost_per_staff_yr
#22347.88

lost_cost =  43.988e6

avg_spells_per_staff_year = 0.651
avg_spells_per_staff_year_short_term = 0.485
avg_spells_per_staff_year_long_term = avg_spells_per_staff_year- avg_spells_per_staff_year_short_term
# 0.166

resp_perc_spells = 5.517
resp_perc_days = 11.664

cvd_perc_spells = 4.387
cvd_perc_days = 1.973

cancer_perc_spells = 3.621
cancer_perc_days = 1.1

msk_perc_spells = 2.509
msk_perc_days = 4.244	

# short_term
# average_duration

# long_term
# average_duration

Cancers_perc_spells_long_term = 4.263
Cancers_perc_days_long_term = 3.269

resp_perc_spells_long_term = 3.562
resp_perc_days_long_term = 4.458

cvd_perc_spells_long_term = 4.913         
cvd_perc_days_long_term = 3.972

msk_perc_spells_long_term = 4.323	
msk_perc_days_long_term = 4.574	

cost_per_day = lost_cost/working_days_lost
cost = days_lost * cost_per_day

res_dt2 <- res_dt[year == min(year),][ data.table(
  days_lost = c(working_days_lost * resp_perc_days/100,
                working_days_lost * cvd_perc_days/100,
                working_days_lost * cancer_perc_days/100,
                working_days_lost * msk_perc_days/100),
  sick_spells = c(
    avg_spells_per_staff_year * staff_years * resp_perc_spells/100,
    avg_spells_per_staff_year * staff_years * cvd_perc_spells/100,
    avg_spells_per_staff_year * staff_years * cancer_perc_spells/100,
    avg_spells_per_staff_year * staff_years * msk_perc_spells/100 
  ),
  
  broad = c('resp','cvd','cancer','msk')), 
  on = .(broad)][,
    .(broad,
      sick_spells_per_case = sick_spells/(prevalence*model_specification$population$scale_down_factor),
      days_lost_per_case = days_lost/(prevalence*model_specification$population$scale_down_factor),
      cost_per_case = cost_per_day * days_lost/(prevalence*model_specification$population$scale_down_factor))
    ]





metric_card <- function( top ='top', 
                         change = 'change', 
                         text ='text',
                         change_icon =  '',
                         color = 'red',
                         opacity='opacity-50'){
  
  change_class = if(change_icon=='negative'){  "fa-arrow-down me-1"
  }else if(change_icon=='negative'){ "fa-arrow-up me-1"
  }else{''}
  
  color_class = case_when(color == '#8F00FF' ~ 'theme-purple',
                          color == 'teal' ~ 'theme-teal',
                          color == 'steelblue' ~ 'theme-teal',
                          
                          
                          color == '#dc3545' ~ 'theme-red', 
                          color == 'mediumseagreen' ~ 'theme-green'
  )
  
  
  div(class = paste("grid-item grid-item--small",opacity),
      div(class = "grid-item-content",
          div(class = "metric-card",
              #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
              div(class = "metric-value", style = paste("color:",color), format(top,big.mark = ',',digits=3)),
              div(class = "metric-label", text),
              div(class = paste("metric-change", change_icon),
                  tags$i(class = change_class, change)
              )
          )
      )
  )
}

library(shiny)

#Lost Labour

metric_card_days_lost_obesity <- metric_card(days_lost, 'Obesity', 'NICS Days Lost','Due to obesity')
metric_card_spells_obesity <- metric_card(spells, 'Obesity', 'NICS Obesity Spells','Long and short term')
metric_card_cost_obesity <- metric_card(cost, 'Obesity', 'NICS Obesity Cost','£')

metric_card_days_lost_population <- metric_card(days_lost * 32,'Obesity','Total Days Lost','Due to obesity',color='teal')
metric_card_spells_population <- metric_card(spells * 32,'Obesity','Population Spells Off' ,'Long and short term',color='teal')
metric_card_cost_population <- metric_card(cost * 32,'Obesity','Population Cost','£',color='teal')

