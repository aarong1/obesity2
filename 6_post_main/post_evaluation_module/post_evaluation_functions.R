# source('./post_evaluation_module/SICK_DAYS.R')
# source('./post_evaluation_module/BED_DAYS.R')

source('./post_evaluation_module/MULTIMORBIDITY.R')
source('./post_evaluation_module/CMMS.R')

source('./post_evaluation_module/COST.R')
source('./post_evaluation_module/DALYS-YLD.R')
source('./post_evaluation_module/DALYS-YLL.R')
source('./post_evaluation_module/QALYs.R')

# source('./post_evaluation_module/DISEASE.R')

# source('./post_evaluation_module/QALYS-YLD.R')
# source('./post_evaluation_module/HRUPoRT.R')
#
# average DALYs - total expected burden on the individual. how age is extended
# individual DALYs but also the collective DALYs.
#
# plot the expected length of life against DALYS - not linear.
#
# collective QALYs
# total daly - burden of disease
#
library(data.table)
#
#sick days
#
#
sick_days_fn <- function(past_populations, group_vars = as.character(), year_cut_off = NULL){
  morb_cols <- c('cvd'='pad', 
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
                 'cancer'='kidney_cancer',         
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
  
  sick_days_matrix <- tibble::tribble(
    ~broad, ~sick_spells_per_case, ~days_lost_per_case, ~cost_per_case,
    "resp",         0.0025372668,        0.113786804,     16.219132,
    "cvd",         0.0007710649,        0.007355829,      1.048497,
    "cancer",         0.0031595995,        0.020359984,      2.902105,
    "msk",         0.0020023097,        0.071843454,     10.240542
  )
  
  setDT(sick_days_matrix)
  
  past_populations <- past_populations[age<68&age<20,]
if(!is.null(year_cut_off)){
  past_populations <- past_populations[year == year_cut_off,]
  }
  
  m <- melt(
    past_populations,
    id.vars = c("year", "run",'intervention',group_vars),
    measure.vars = unname(morb_cols),
    variable.name = "variable",
    value.name = "value"
  )
  
  m[data.table(variable = unname(morb_cols), 
               broad = names(morb_cols)), on = .( variable), broad := i.broad]
  
  # count non-zero per year/run/variable
  m <- m[, .(n = sum(value != 0,na.rm = T)), by = c('year',group_vars, 'run','intervention','broad')]
  
  # complete grid of year/run/variable
  # grid <- CJ(
  #   year     = unique(m$year),
  #   run      = unique(m$run),
  #   broad = unique(m$broad)
  # )
  
  # do.call(CJ, list(c(5, NA, 1), c(1, 3, 2)))  # same as above
  
 grid <-  do.call(CJ, 
  lapply(X = c('year','run',group_vars, 'broad'), 
         function(x){c(unique(m[[x]]))}) 
  )
 
  names(grid) <- c('year', 'run', group_vars, 'broad')
  
  grid <- grid[, intervention := ifelse((run>max(run)/2), 'intervention','non-intervention')]
  
  m2 <- m[grid, on = c('year', 'run', group_vars, 'intervention', 'broad'), nomatch=0L]
  m2[is.na(n), n := 0L]
  
  # mean across runs
  
  res_dt <- m2[, .(prevalence = mean(n)), by = c('year', group_vars, 'intervention', 'broad')]
  
  res_dt <- res_dt[, .(prevalence = sum(prevalence)*model_specification$population$scale_down_factor), by = c('year', group_vars, 'intervention', 'broad')]
  
  res_dt
  
  res_dt[sick_days_matrix, on  = .(broad), 
            `:=`(sick_spells_per_case = i.sick_spells_per_case,
                 days_lost_per_case = i.days_lost_per_case,
                 cost_per_case = i.cost_per_case)
  ] 
  
  res_dt[, `:=`(days_lost = days_lost_per_case*prevalence*model_specification$population$scale_down_factor, 
                   cost = cost_per_case*prevalence*model_specification$population$scale_down_factor, 
                   sick_spells = sick_spells_per_case*prevalence*model_specification$population$scale_down_factor)]
}



bed_days_fn <- function(past_populations,group_vars = as.character(), year_cut_off = NULL){

#bed days
bed_days_df <- tibble::tribble(
       ~broad, ~bed_days_per_case, ~admissions_per_case, ~emergency_admissions_per_case,
  "cancer",         0.6203923,          0.45678642,          0.05017094,
  "cvd",         0.2090028,          0.03813758,          0.04001619,
  "resp",         0.1116457,          0.01755706,          0.03485174,
  "msk",         0.2636094,          0.07721338,          0.04428415
  )
morb_cols <- c('cvd'='pad', 'msk'='osteoporosis',     'cancer'='cancer',                               'msk'='osteoarthritis',             'msk'='rheumatoid_arthritis',       'other'='epilepsy',                  'other'='hypothyroidism','resp'='asthma',                     'resp'='copd',                       'other'='depression',                 'cvd'='non_diabetic_hyperglycaemia','cancer'='colorectal_cancer',          'cancer'='prostate_cancer',           'cancer'='female_breast_cancer',      'cancer'='kidney_cancer',                         'cancer'='oesophageal_cancer',                   'cancer'='stomach_cancer',                       'cancer'='osteogastric_cancer',                  'cancer'='oral_cancer',                          'cancer'='pancreatic_cancer',                    'cancer'='uterine_cancer',            'cancer'='blood_multiple_myeloma',               'cancer'='blood_lymphoma',                       'cancer'='blood_leukaemia',                      'cancer'='blood_cancer',                         'cancer'='ovarian_cancer',            'cancer'='lung_cancer','cvd'='stroke','cvd'='chd','cvd'='diabetes','other'='dementia','cvd'='heart_failure','cvd'='atrial_fibrillation','cvd'='hypertension','cvd'='chronic_kidney_disease')


# sick_days_matrix <- tibble::tribble(
#   ~broad, ~sick_spells_per_case, ~days_lost_per_case, ~cost_per_case,
#   "resp",         0.0025372668,        0.113786804,     16.219132,
#   "cvd",         0.0007710649,        0.007355829,      1.048497,
#   "cancer",         0.0031595995,        0.020359984,      2.902105,
#   "msk",         0.0020023097,        0.071843454,     10.240542
# )

setDT(bed_days_df)

if(!is.null(year_cut_off)){
  past_populations <- past_populations[year == year_cut_off,]
}

m <- melt(
  past_populations,
  id.vars = c("year", "run",'intervention',group_vars),
  measure.vars = unname(morb_cols),
  variable.name = "variable",
  value.name = "value"
)

m[data.table(variable = unname(morb_cols), 
             broad = names(morb_cols)), on = .( variable), broad := i.broad]

# count non-zero per year/run/variable
m <- m[, .(n = sum(value != 0,na.rm = T)), by = c('year',group_vars, 'run','intervention','broad')]

# complete grid of year/run/variable
# grid <- CJ(
#   year     = unique(m$year),
#   run      = unique(m$run),
#   broad = unique(m$broad)
# )

# do.call(CJ, list(c(5, NA, 1), c(1, 3, 2)))  # same as above

grid <-  do.call(CJ, 
                 lapply(X = c('year','run',group_vars, 'broad'), 
                        function(x){c(unique(m[[x]]))}) 
)

names(grid) <- c('year', 'run', group_vars, 'broad')

grid <- grid[, intervention := ifelse((run>max(run)/2), 'intervention','non-intervention')]

m2 <- m[grid, on = c('year', 'run', group_vars, 'intervention', 'broad'), nomatch=0L]
m2[is.na(n), n := 0L]

# mean across runs

res_dt <- m2[, .(prevalence = mean(n)), by = c('year', group_vars, 'intervention', 'broad')]

res_dt <- res_dt[, .(prevalence = sum(prevalence)*model_specification$population$scale_down_factor), by = c('year', group_vars, 'intervention', 'broad')]

res_dt
res_dt[bed_days_df, on  = .(broad), 
       `:=`(bed_days_per_case = i.bed_days_per_case,
            admissions_per_case = i.admissions_per_case,
            emergency_admissions_per_case = i.emergency_admissions_per_case)
] 

res_dt[, `:=`(bed_days = bed_days_per_case*prevalence*model_specification$population$scale_down_factor, 
              admissions = admissions_per_case*prevalence*model_specification$population$scale_down_factor, 
              emergency_admissions = emergency_admissions_per_case*prevalence*model_specification$population$scale_down_factor)]
res_dt
}

sick_days_mdm_quintile <- sick_days_fn(past_populations,group_vars = 'mdm_quintile_soa_name',year_cut_off = 2024)
bed_days_age20 <- bed_days_fn(past_populations = past_populations,'age20',year_cut_off = 2024)

costs <- calculate_costs_fn(past_populations, group_vars = 'HSCT', year_cut_off = 2024)

past_populations <- compute_cmms_dt(past_populations)
past_populations <- add_multimorbidity_fn(past_populations)

daly_yld_fn(past_populations ,'mdm_quintile_soa_name',year_cut_off = 2024)
calculate_daly_yll(past_populations,group_vars = 'mdm_quintile_soa_name',year_cut_off = 2024)

args <- list( past_populations = past_populations, group_vars = c("broad_ethnicity"), year_cut_off = 2025)
qalys_ethnicity <- do.call(qaly_yld_fn, args)
qalys_ethnicity[disease == "combined_uw", .(total = sum(total_uw))]



