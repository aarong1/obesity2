

bmi_levels = c('normal','overweight','obese')
source('1_2_utils/main_configuration.R')

pop_scale_up = model_specification$population$scale_down_factor

# past_populations <- read.fst('./3_pre_main/intermediate_populations/full_history_past_populations.fst')


# past_populations <- past_populations %>% mutate(age10 = replace_values(age10,
#                                                                        from = '75+',
#                                                                        to = '75-110')) %>% 
#   mutate(age10 = case_when(age==0~'0-15',
#                            age==1~'0-15',
#                            T ~ age10))



# initial_time_zero_population <- read.fst('./3_pre_main/intermediate_populations/initial_time_zero_population.fst')
# first_population <- read.fst('./3_pre_main/intermediate_populations/first_population.fst')
# past_populations <- read.fst('./3_pre_main/intermediate_populations/time_one_population.fst')
# population_w_established_prevalence <- read.fst('./3_pre_main/intermediate_populations/population_w_established_prevalence.fst')


# time_one_population_w_deaths <- read.fst('./3_pre_main/intermediate_populations/time_one_population_w_deaths.fst')

# write.fst(population_w_established_prevalence %>% slice_sample(prop = 0.5), './3_pre_main/intermediate_populations/population_w_established_prevalence.fst')
# write.fst(time_one_population_w_deaths %>% slice_sample(prop = 0.5), './3_pre_main/intermediate_populations/time_one_population_w_deaths.fst')
# write.fst(initial_time_zero_population %>% slice_sample(prop = 0.5), './3_pre_main/intermediate_populations/initial_time_zero_population.fst')
# write.fst(first_population %>% slice_sample(prop = 0.5), './3_pre_main/intermediate_populations/first_population.fst')
# write.fst(past_populations %>% slice_sample(prop = 0.5), './3_pre_main/intermediate_populations/time_one_population.fst')

dir.create('./3_pre_main/intermediate_populations')

# download.file(
#   "https://storage.googleapis.com/time_one_population_w_deaths/populations/full_history_past_populations.fst",
#   destfile = "./3_pre_main/intermediate_populations/full_history_past_populations.fst",
#   mode = "wb"
# )

# full_history_past_populations <- read.fst('3_pre_main/intermediate_populations/full_history_past_populations.fst')




# download.file(
#   "https://storage.googleapis.com/time_one_population_w_deaths/populations/first_population.fst",
#   destfile = "./3_pre_main/intermediate_populations/first_population.fst",
#   mode = "wb"
# )
 
# first_population <- read.fst('3_pre_main/intermediate_populations/first_population.fst')
 

# download.file(
#   "https://storage.googleapis.com/time_one_population_w_deaths/populations/initial_time_zero_population.fst",
#   destfile = "./3_pre_main/intermediate_populations/initial_time_zero_population.fst",
#   mode = "wb"
# )
 
# initial_time_zero_population <- read.fst('3_pre_main/intermediate_populations/initial_time_zero_population.fst')
 

# download.file(
#   "https://storage.googleapis.com/time_one_population_w_deaths/populations/base_population_w_physiological_and_modifiable_risk_factors.fst",
#   destfile = "./3_pre_main/intermediate_populations/base_population_w_physiological_and_modifiable_risk_factors.fst",
#   mode = "wb"
# )
 
# base_population_w_physiological_and_modifiable_risk_factors <- read.fst('3_pre_main/intermediate_populations/base_population_w_physiological_and_modifiable_risk_factors.fst')


# download.file(
#   "https://storage.googleapis.com/time_one_population_w_deaths/populations/time_one_population_w_deaths.fst",
#   destfile = "./3_pre_main/intermediate_populations/time_one_population_w_deaths.fst",
#   mode = "wb"
# )
# time_one_population_w_deaths <- read.fst('3_pre_main/intermediate_populations/time_one_population_w_deaths.fst')


# download.file(
#   "https://storage.googleapis.com/time_one_population_w_deaths/populations/time_one_population.fst",
#   destfile = "./3_pre_main/intermediate_populations/time_one_population.fst",
#   mode = "wb"
# )
 
past_populations <- read.fst('./3_pre_main/intermediate_populations/time_one_population.fst')

past_populations$intervention = 'non-intervention'

source('./post_evaluation_functions.R')

# disease_costs <- calculate_costs_fn(as.data.table(past_populations))
# sick_days <- sick_days_fn((past_populations))
# bed_days <- bed_days_fn(as.data.table(past_populations))

# qalys <- qaly_yld_fn(as.data.table(past_populations))
# yld <- daly_yld_fn(as.data.table(past_populations))
# yll <- calculate_daly_yll(as.data.table(past_populations))

past_populations <- compute_cmms(past_populations)
past_populations <- add_multimorbidity_fn(past_populations)

pop <- past_populations %>%
  filter(year == min(year)) %>%
  filter(run == min(run))

pop <- pop |> 
mutate(qrisk_percentile = rank(qrisk_score)/max(rank(qrisk_score))) 

pop <- pop %>% 
  mutate(
    bmi = factor(
      bmi,
      levels = c(
        c('normal','overweight','obese'),
        setdiff(sort(unique(as.character(bmi))), bmi_levels)
      ),
      ordered = TRUE
    ) 
    ) %>% 
  mutate( mdm_quintile_soa_name =
            factor(  mdm_quintile_soa_name,
                     ordered = T,
                     levels = rev(c('Most Deprived',     
                                'Quintile 2',        
                                'Quintile 3',        
                                'Quintile 4',        
                                'Least Deprived'    )),
                     labels = rev(c('Most Deprived',     
                                'Quintile 2',        
                                'Quintile 3',        
                                'Quintile 4',        
                                'Least Deprived'    ))))

pop <- pop %>% 
  mutate(Urban_mixed_rural_status = 
           factor(  Urban_mixed_rural_status,
                    ordered = T,
                    levels = c('Rural',
                               'Mixed',
                               'Urban'   ),
                    labels = c('Rural',
                               'Mixed',
                               'Urban')))

corisks <- pop |> 
  mutate(.keep = 'none',
         
         diabetes_status = diabetes_status != 'no_diabetes',
         af_status = af_status=='af',
         pad_status = pad_status=='pad',
         ckd_status = ckd_status=='ckd',
         hypertension_status = hypertension_status!='normotensive_untreated',
         cholesterol_status = cholesterol_status=='normal_cholesterol'#,
         
  ) %>% rowSums() #%>% table()

pop <- pop %>% 
  mutate(
    corisks = corisks
  )


corisk_morbidity <- pop |> 
  mutate(.keep = 'none',
         
         diabetes_status = diabetes_status != 'no_diabetes',
         af_status = af_status=='af',
         pad_status = pad_status=='pad',
         ckd_status = ckd_status=='ckd',
         hypertension_status = hypertension_status!='normotensive_untreated',
         cholesterol_status = cholesterol_status=='normal_cholesterol'#,
  ) %>% rowSums()

pop <- pop %>% 
  mutate(
    corisk_morbidity = corisk_morbidity
  )


corisk_modifiable <- pop |> 
  mutate( .keep = 'none',
          bmi = (bmi %in% c('obese','overweight')),
          smoking = (smoking == 'current_smoker' ),
          alcohol = (alcohol %in% c('higher_risk', 'increased_risk')),
          diet = (diet == 'below_5_a_day' ),
          pa = (pa != 'meets_rec')
  ) %>% rowSums(na.rm = T) 

pop <- pop %>% 
  mutate(corisk_modifiable = corisk_modifiable)



# load( file = "data/csv_pts_wgs84.RData") #csv_pts_wgs84
print(paste('running','pages_prep.R'));source('pages_prep.R')
print(paste('running','bed_days_estimate.R'));source("bed_days_estimate.R")
print(paste('running','risk_stratification.R'));source('risk_stratification.R')
print(paste('running','obesity_causes.R'));source('obesity_causes.R')
print(paste('running','comorbidity.R'));source('comorbidity.R')
print(paste('running','deprivation.R'));source('deprivation.R')
# print(paste('running','tables.R')); source('tables.R')
# print(paste('running','sick_days_estimate.R'));source("sick_days_estimate.R")
print(paste('running','infographics.R'));source('infographics.R')
print(paste('running','obesity_prevalence_tables.R'));source('obesity_prevalence_tables.R')
print(paste('running','app_prep.R'));source('app_prep.R')
# print(paste('running','pages_prep_geo.R'));source('pages_prep_geo.R')
print(paste('running','inequality_charts.R'));source('inequality_charts.R')
print(paste('running','PAF.R'));source('PAF.R')
print(paste('running','obesigenic.R'));source('obesigenic.R')
print(paste('running','INT.R'));source('INT.R')
print(paste('running','pie_chart.R'));source('pie_chart.R')
print(paste('running','Morbidity_risk_highlights.R'));source('Morbidity_risk_highlights.R')

start <- Sys.time()
load('./preprocess/pages_prep.RData')
load('./preprocess/comorbidity.RData')               
load('./preprocess/infographics.RData')              
load('./preprocess/obesity_causes.RData')
load('./preprocess/deprivation.RData')               
load('./preprocess/int.RData')                       
load('./preprocess/risk_stratification.RData')
load('./preprocess/inequality_charts.RData')         
load('./preprocess/obesigenic.RData')
load('./preprocess/obesity_prevalence_tables.RData')



paf_bmi <- read_rds('./stored_results/paf_bmi.rds')
absf_bmi <- read_rds('./stored_results/absf_bmi.rds')
stop <- Sys.time()


print(stop-start)
# mutate(
#   bmi = factor(
#     bmi,
#     levels = c(
#       c('normal','overweight','obese'),
#       setdiff(sort(unique(as.character(bmi))), bmi_levels)
#     ),
#     ordered = TRUE
#   )
# )
  


  