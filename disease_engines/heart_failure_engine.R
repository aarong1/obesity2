library(data.table)

# HF <- tribble(				
# ~age, ~sex,	~n, ~'Total PY',	~'Incident',	~per1000,	~CI,	~HFrEF,	~HFpEF,	~Unknown,
# '18-45',  'Males',	3030042,	9083853.9,	3118,	0.34,	"0.33-0.36",	17.8,	10.7,	71.5,
# '18-45',  'Females',	3121844,	8666833.9,	2164,	0.25,	"0.24-0.26",	13.4,	8.3,	78.3,
# '45-64',  'Males',	1994544,	6778722.1,	19405,	2.86,	"2.82-2.90",	17.6,	9.9,	72.6,
# '45-64',  'Females',	1916814,	6592247.7,	11021,	1.67,	"1.64-1.70",	13.0,	13.5,	73.6,
# '65-74',  'Males',	749922,	2233188.5,	22029,	9.86,	"9.74-10.00",	15.5,	7.6,	76.9,
# '65-74',  'Females',	803145,	2429971.2,	15884,	6.54,	"6.44-6.64",	11.0,	12.1,	76.9,
# '75-84',  'Males',	418537,	1142713.4,	27861,	24.38,	"24.10-24.67",	12.5,	6.3,	81.2,
# '75-84',  'Females',	520555,	1443004.6,	27663,	19.17,	"18.95-19.40",	8.0,	9.6,	82.5,
# '85-110',  'Males',	146703,	339066.6,	18722,	55.22,	"54.43-56.01",	7.4,	3.8,	88.8,
# '85-110',  'Females',	259333,	620985.1,	27923,	44.97,	"44.44-45.50",	4.2,	4.6,	91.2
# )


# apply_heart_failure_wo_risk_factors <- function(input_population) {
#     # Accept Data.Table
#     dt <- as.data.table(input_population)
# 
#     # Prepare incidence table
#     # Age groups in chd_incidence_per100k: '30-54', '55-64', '65-74', '75-84', '85-110'
#     # We assume 0 incidence for age < 30
# 
#     # Create mapping for age groups
#     dt[, age_group_inc := cut(age, 
#                               breaks = c(-Inf, 18, 44, 64, 74, 84, 110),
#                               labels = c("0-18", "45-64", "65-74", "65-74", "75-84", "85-110"),
#                               right = TRUE)]
#     
#     # Join incidence
#     dt[HF, on = .(age_group_inc = age, sex), heart_failure_year_risk := per1000 / 1000
# ]
#     # dt[, heart_failure_year_risk := per1000 / 1000]
#     
#     # Fill NA with 0 (for age 0-29 etc)
#     dt[is.na(heart_failure_year_risk), heart_failure_year_risk := 0]
#     
#     # Cleanup
#     dt[, age_group_inc := NULL]
#     
#     return(dt)
# }



#BMI
# https://www.hopkinsmedicine.org/health/wellness-and-prevention/weight-a-silent-heart-risk
# For every five-point increase in BMI, the risk of heart failure rose by 32 percent in the study. 
# 1.32 per 5 kgm-2

#PA
# https://www.ahajournals.org/doi/10.1161/circulationaha.115.015853
# 0.81 - meets recommendations

#hypertension
#Blood pressure, hypertension, and the risk of heart failure: a systematic review and meta-analysis of cohort studies
# https://pubmed.ncbi.nlm.nih.gov/37939784/
# 1.71 - hypertension

#smoking
# current 1.61
# former  1.21

# T2DM
# Diabetes and risk of heart failure in people with and without cardiovascular disease: systematic review and meta-analysis
#https://www.diabetesresearchclinicalpractice.com/article/S0168-8227(23)00817-3/fulltext
# 2.02 -DM



# heart_failure_theoretical_minimum <-
#   initial_time_zero_population  |> 
#   group_by(age_match,sex) |> 
#   summarise(AF = 1-n()/sum(pollution_rr^( (pm25g)/10))) |> #-min(pm25g)
#   left_join(
#     lung_cancer_data,
#     by= c('age_match' = 'age', 'sex')
#   ) %>% 
#   mutate(min_prob = per100k/100000 * (1-AF) ) 
# 
# # mutate( (1-AF) ) |>
# # left_join(
# #   lung_cancer_incidence
# # ) |>
# # mutate(lung_cancer_prob_min = lung_cancer_prob * (1-AF) ) |> 
# 
# # View() 


# fff----
  
library(tibble)
library(dplyr)
library(data.table)

# ============================================================================
# HEART FAILURE RISK ENGINE
# ============================================================================

# Data Definitions ----

# Heart Failure Incidence Data (per 1000 person-years)
heart_failure_incidence_per1000 <- tribble(				
  ~age, ~sex, ~n, ~'Total PY', ~'Incident', ~per1000, ~CI, ~HFrEF, ~HFpEF, ~Unknown,
  '18-45',  'Males',   3030042, 9083853.9, 3118,  0.34,  "0.33-0.36", 17.8, 10.7, 71.5,
  '18-45',  'Females', 3121844, 8666833.9, 2164,  0.25,  "0.24-0.26", 13.4,  8.3, 78.3,
  '45-64',  'Males',   1994544, 6778722.1, 19405, 2.86,  "2.82-2.90", 17.6,  9.9, 72.6,
  '45-64',  'Females', 1916814, 6592247.7, 11021, 1.67,  "1.64-1.70", 13.0, 13.5, 73.6,
  '65-74',  'Males',    749922, 2233188.5, 22029, 9.86,  "9.74-10.00", 15.5, 7.6, 76.9,
  '65-74',  'Females',  803145, 2429971.2, 15884, 6.54,  "6.44-6.64", 11.0, 12.1, 76.9,
  '75-84',  'Males',    418537, 1142713.4, 27861, 24.38, "24.10-24.67", 12.5, 6.3, 81.2,
  '75-84',  'Females',  520555, 1443004.6, 27663, 19.17, "18.95-19.40",  8.0, 9.6, 82.5,
  '85-110', 'Males',    146703,  339066.6, 18722, 55.22, "54.43-56.01",  7.4, 3.8, 88.8,
  '85-110', 'Females',  259333,  620985.1, 27923, 44.97, "44.44-45.50",  4.2, 4.6, 91.2
)

# Risk Factor Relative Risks for Heart Failure ----

# BMI - Relative Risk per 5 kg/m² increase
# Source: https://www.hopkinsmedicine.org/health/wellness-and-prevention/weight-a-silent-heart-risk
# For every five-point increase in BMI, the risk of heart failure rose by 32%
rr_heart_failure_bmi <- 1.32

# Physical Activity - Relative Risk (PROTECTIVE FACTOR)
# Source: https://www.ahajournals.org/doi/10.1161/circulationaha.115.015853
# Meeting recommendations is protective (RR = 0.81)

rr_heart_failure_physical_activity <- tibble::tribble(
  ~activity, ~RR,
  "meets_rec", 0.81,        # Protective effect
  "low_activity", 1.0,      # Reference
  "some_activity", 1.0,     # Reference
  "inactive", 1.0           # Reference
)

# Hypertension - Relative Risk
# Source: https://pubmed.ncbi.nlm.nih.gov/37939784/
rr_heart_failure_hypertension <- tibble::tribble(
  ~hypertension, ~RR,
  'normotensive_untreated', 1.0,
  'hypertensive_controlled', 1.0,
  'hypertensive_uncontrolled', 1.71,
  'hypertensive_untreated', 1.71
)

# Smoking - Relative Risk
rr_heart_failure_smoking <- tibble::tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.21,
  "current_smoker", 1.61
)

# Diabetes - Relative Risk
# Source: https://www.diabetesresearchclinicalpractice.com/article/S0168-8227(23)00817-3/fulltext
rr_heart_failure_diabetes <- tibble::tribble(
  ~diabetes, ~RR,
  "no_diabetes", 1.0,
  "undiagnosed_diabetes", 2.02,
  "diagnosed_diabetes", 2.02
)

# Convert incidence to long format for easy merging
HF_inc_dt <- heart_failure_incidence_per1000 %>%
  select(age, sex, per1000) %>% 
  as.data.table()

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_heart_failure_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Age groups: '18-45', '45-64', '65-74', '75-84', '85-110'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 64, 74, 84, 110),
                            labels = c("18-45", "45-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
  
  HF_inc_temp <- as.data.table(heart_failure_incidence_per1000)[, .(age, sex, per1000)]
  dt[HF_inc_temp, on = .(age_group_inc = age, sex), heart_failure_year_risk := i.per1000 / 1000]
  dt[is.na(heart_failure_year_risk), heart_failure_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}


# Function 2: Calculate PAF and Theoretical Minimum
calculate_heart_failure_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("heart_failure" %in% names(dt)) {
    dt <- dt[heart_failure == 0]
  }
  
  # 1. BMI RR - Continuous relationship
  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 27.5,
    bmi == "obese", 35,
    default = 20
  )]
  
  dt[, RR_bmi_indiv := rr_heart_failure_bmi^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_heart_failure_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 3. Physical Activity RR (Protective factor)
  rr_pa_dt <- as.data.table(rr_heart_failure_physical_activity)
  dt[rr_pa_dt, on = .(pa = activity), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 4. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_heart_failure_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status = hypertension), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 5. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_heart_failure_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status = diabetes), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_pa_indiv * 
                      RR_hypertension_indiv * RR_diabetes_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 64, 74, 84, 110),
                            labels = c("18-45", "45-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
                            
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  # Merge with incidence
  HF_inc_source <- as.data.table(heart_failure_incidence_per1000)[, .(age, sex, per1000)]
  min_dt <- merge(HF_inc_source, paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, heart_failure_prob_min := (per1000 / 1000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, heart_failure_prob_min)])
}

# Function 3: Apply Risk Factors
apply_heart_failure_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR - Continuous relationship
  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 27.5,
    bmi == "obese", 35,
    default = 20
  )]
  
  dt[, RR_bmi_indiv := rr_heart_failure_bmi^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_heart_failure_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 3. Physical Activity RR (Protective factor)
  rr_pa_dt <- as.data.table(rr_heart_failure_physical_activity)
  dt[rr_pa_dt, on = .(pa = activity), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 4. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_heart_failure_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status = hypertension), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 5. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_heart_failure_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status = diabetes), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_pa_indiv * 
                      RR_hypertension_indiv * RR_diabetes_indiv]
  
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 64, 74, 84, 110),
                            labels = c("18-45", "45-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
                            
  dt[min_dt, on = .(age_group_inc, sex), heart_failure_prob_min := i.heart_failure_prob_min]
  dt[is.na(heart_failure_prob_min), heart_failure_prob_min := 0]
  
  dt[, heart_failure_year_risk := heart_failure_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("bmi_val", "RR_bmi_indiv", "RR_smoking_indiv", 
         "RR_pa_indiv", "RR_hypertension_indiv", "RR_diabetes_indiv", 
         "RR_combined", "age_group_inc", "heart_failure_prob_min") := NULL]
         
  return(dt)
}



store_unit_test <- function(){
  
  x <- initial_time_zero_population %>% 
    select(-heart_failure_year_risk) %>% 
    apply_heart_failure_wo_risk_factors()
  
  initial_time_zero_population
  
  
  
  x <- past_populations %>% 
    filter(year ==min(year))
  
  t = calculate_heart_failure_theoretical_min(x)
  
  y <- apply_heart_failure_risk_factors(x,theoretical_min_table = t)
  
  y <- y %>% mutate(age1=cut(age, 
                             breaks = c(-Inf, 45, 64, 74, 84, 110),
                             labels = c("18-45", "45-64", "65-74", "75-84", "85-110"),
                             right = FALSE))
  
  y %>% 
    group_by(age1, sex) %>% 
    summarise(n=n(), wt=sum( heart_failure_year_risk)) %>% 
    mutate(heart_failure_year_risk_per1000 = wt/n*1000)
  
  input_population = past_populations %>% 
    filter(year ==min(year))
  
}

