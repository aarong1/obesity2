# Lung Cancer Engine
# Import cancer registry data and calculate lung cancer risk by age, sex, and pollution

library(dplyr)
library(tidyr)
library(readxl)

# # Import lung cancer incidence data from cancer registry
# lung_cancer_data_ages <- read_excel( "data/NIcancer_registry/Lung_cancer_data_tables.xlsx",
#    'T02', range = 'A6:A20' )
# 
# # Clean age labels: hyphenate ranges and convert "90 and over" to "90-110"
# lung_cancer_data_ages <- lung_cancer_data_ages %>%
#     rename(age_group = 1) %>%
#     mutate(
#         age_group = trimws(age_group),
#         age_group = ifelse(is.na(age_group) | age_group == "All ages", NA, age_group),
#         age_group = gsub("(?i)\\s*to\\s*", "-", age_group, perl = TRUE),
#         age_group = gsub("(?i)^90\\s*and\\s*over$", "90-110", age_group, perl = TRUE)
#     ) %>%
#     filter(!is.na(age_group))
# 
# lung_cancer_data_males <- read_excel( "data/NIcancer_registry/Lung_cancer_data_tables.xlsx",
#    'T02', range = 'E6:G20' ) %>%
#       rename( per100k_Males = `Age-specific incidence rate per 100,000 males` )
# 
# lung_cancer_data_females <- read_excel( "data/NIcancer_registry/Lung_cancer_data_tables.xlsx",
#    'T02', range = 'H6:J20' )  %>%
#    rename( per100k_Females = `Age-specific incidence rate per 100,000 females` )
# 
# lung_cancer_data <- cbind(lung_cancer_data_ages,
#     lung_cancer_data_males[-c(1,2),],
#     lung_cancer_data_females[-c(1,2),]) %>%
#     select(age_group, per100k_Males, per100k_Females) %>%
#     pivot_longer(cols = starts_with("per100k"),names_sep = '_', names_to = c(NA,"sex"), values_to = "incidence_rate")
#     # rename(age = `Age at diagnosis`)

lung_cancer_data <- tibble::tribble(
      ~age, ~sex ,~per100k,
 "0-39"  ,    "Males",              0.5,
 "0-39"  ,    "Females",            0.8,
 "40-44" ,    "Males",              4.4,
 "40-44" ,    "Females",            5.5,
 "45-49" ,    "Males",             17.1,
 "45-49" ,    "Females",           10.8,
 "50-54" ,    "Males",             36,
 "50-54" ,    "Females",           35,
 "55-59" ,    "Males",             61.9,
 "55-59" ,    "Females",           65.7,
 "60-64" ,    "Males",            148.,
 "60-64" ,    "Females",          126.,
 "65-69" ,    "Males",            224.,
 "65-69" ,    "Females",          228.,
 "70-74" ,    "Males",            360.,
 "70-74" ,    "Females",          278.,
 "75-79" ,    "Males",            440.,
 "75-79" ,    "Females",          341,
 "80-84" ,    "Males",            524,
 "80-84" ,    "Females",          331.,
 "85-89" ,    "Males",            528.,
 "85-89" ,    "Females",          332.,
 "90-110",    "Males",            509.,
 "90-110",    "Females",          258.

  )

NCD prime
Lung cancer RR for 106g increase in fruit intake	
0.94

Lung cancer incidence for 22.5METhrs/wk increase
0.74

NCD prime
current smokers 8.96
former smokers 3.85

#https://www.dynamo-hia.eu/sites/default/files/2018-04/BMI_WP7-datareport_20100317.pdf
Disease RR overweight
BMI 25-29.9
Normal weight =
  1.0
RR obesity
BMI 30 or more
Normal weight =
  1.0
Age
adjustments*
  (multiplier of
   differential risk)
Smoking
adjustments
*
  (never smoker
   =1.0)
men women men women
0.80 0.88 0.65 0.70

# DYNAMO-HIA
Outcome	Male aged 35 and above 			 	Female aged 35 and above		
Never Smoker	Current Smoker	Former Smoker		Never Smoker	Current Smoker	Former Smoker
"  Persons Aged 35–39
  Persons Aged 40-44
  Persons Aged 45–49
  Persons Aged 50–54
  Persons Aged 55–59
  Persons Aged 60-64
  Persons Aged 65+"	"1.00
1.00
1.00
1.00
1.00
1.00
1.00"	"1.30
1.00
5.78
24.97
34.02
31.47
28.40"	"1.00
1.00
2.37
10.70
11.66
11.71
9.70"		"1.00
1.00
1.00
1.00
1.00
1.00
1.00"	"2.00
1.00
18.08
11.14
17.87
13.32
17.49"	"1.00
1.00
8.07
3.28
5.33
4.91
5.54"



# Calculate baseline lung cancer risk by age and sex
apply_lung_cancer_risk_wo_risk_factors_paf <- function(input_population){
  
  input_population[,age_match := cut( age,
                                    breaks = c(-Inf, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                                    labels = c("0-39", "40-44", "45-49", "50-54",
                                               "55-59", "60-64", "65-69", "70-74", "75-79",
                                               "80-84", "85-89", "90-110"),)]

  input_population[lung_cancer_data,on=.(age_match = age ,sex), lung_cancer_year_risk := per100k/100000]
  
}

initial_time_zero_population$lung_cancer = 0
initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(year=2021)
initial_time_zero_population <- initial_time_zero_population %>% 
  populate_lung_cancer_prevalence() 

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(year=2022) %>% 
  as.data.table() %>%  
  apply_lung_cancer_risk_wo_risk_factors()

initial_time_zero_population <- declare_absolute_incident_morbidity(initial_time_zero_population,'lung_cancer')
count(initial_time_zero_population,lung_cancer)
# Define relative risk for pollution exposure

initial_time_zero_population$pm25g
pollution_rr <- 1.09


lung_cancer_theoretical_minimum <-
  initial_time_zero_population  |> 
  group_by(age_match,sex) |> 
  summarise(AF = 1-n()/sum(pollution_rr^( (pm25g)/10))) |> #-min(pm25g)
  left_join(
    lung_cancer_data,
    by= c('age_match' = 'age', 'sex')
  ) %>% 
  mutate(min_prob = per100k/100000 * (1-AF) ) 

  # mutate( (1-AF) ) |>
  # left_join(
  #   lung_cancer_incidence
  # ) |>
  # mutate(lung_cancer_prob_min = lung_cancer_prob * (1-AF) ) |> 
  
  # View() 


apply_lung_cancer_risk_w_pollution <- function(input_population){
  
  input_population <- input_population %>% 
    mutate(age_match = cut( age,
         breaks = c(-Inf, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
         labels = c("0-39", "40-44", "45-49", "50-54",
                    "55-59", "60-64", "65-69", "70-74", "75-79",
                    "80-84", "85-89", "90-110"))) %>% 
    mutate(RR_pm25 = pollution_rr^( (pm25g)/10)) %>% 
    left_join(lung_cancer_theoretical_minimum[c('age_match', 'sex', 'min_prob')],
              by = c('age_match','sex')) %>% 
    mutate(lung_cancer_year_risk = min_prob * RR_pm25) 
  
  input_population <- input_population %>% 
    select(-c(min_prob, RR_pm25, age_match))
  
}

initial_time_zero_population %>% 
  mutate(pm25g = 0.5*pm25g) %>% 
  apply_lung_cancer_risk_w_pollution() %>% 
  count(wt = lung_cancer_year_risk)



