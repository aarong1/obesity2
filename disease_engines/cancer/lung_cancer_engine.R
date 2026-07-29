# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# Lung Cancer Incidence Data
# Source: NI Cancer Registry
# Import lung cancer incidence data from cancer registry
# File: data/NIcancer_registry/Lung_cancer_data_tables.xlsx
# Table T02
# Age-specific incidence rates per 100,000 by sex

# Lung Cancer Incidence (per 100,000):
# Age         Males    Females
# 0-39         0.5      0.8
# 40-44        4.4      5.5
# 45-49       17.1     10.8
# 50-54       36.0     35.0
# 55-59       61.9     65.7
# 60-64      148.0    126.0
# 65-69      224.0    228.0
# 70-74      360.0    278.0
# 75-79      440.0    341.0
# 80-84      524.0    331.0
# 85-89      528.0    332.0
# 90-110     509.0    258.0

# Risk Factors for Lung Cancer:

# NCD Prime:
# - Lung cancer RR for 106g increase in fruit intake: 0.94
# - Lung cancer incidence for 22.5 MET hrs/wk increase: 0.74
# - Smoking: current smokers 8.96, former smokers 3.85

# DYNAMO-HIA BMI Risk:
# Source: https://www.dynamo-hia.eu/sites/default/files/2018-04/BMI_WP7-datareport_20100317.pdf
# Disease RR overweight (BMI 25-29.9), Normal weight = 1.0
# Disease RR obesity (BMI 30 or more), Normal weight = 1.0
# Age adjustments (multiplier of differential risk)
# Smoking adjustments (never smoker = 1.0)
#              Overweight RR    Obesity RR
# Men              0.80           0.65
# Women            0.88           0.70

# DYNAMO-HIA Age-Sex-Specific Smoking Risk:
# Male aged 35 and above:
# Age          Never    Current   Former
# 35-39        1.00     1.30      1.00
# 40-44        1.00     1.00      1.00
# 45-49        1.00     5.78      2.37
# 50-54        1.00    24.97     10.70
# 55-59        1.00    34.02     11.66
# 60-64        1.00    31.47     11.71
# 65+          1.00    28.40      9.70
# Female aged 35 and above:
# Age          Never    Current   Former
# 35-39        1.00     2.00      1.00
# 40-44        1.00     1.00      1.00
# 45-49        1.00    18.08      8.07
# 50-54        1.00    11.14      3.28
# 55-59        1.00    17.87      5.33
# 60-64        1.00    13.32      4.91
# 65+          1.00    17.49      5.54

# PM2.5 Pollution Risk:
# RR = 1.09 per 10 μg/m³

# ============================================================================
# LUNG CANCER RISK ENGINE
# ============================================================================

# Lung Cancer Risk Factors

# Risk Factors Included:
# - PM2.5 pollution exposure
# - Smoking (NCD Prime)
# - BMI (sex-specific, DYNAMO-HIA)

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Lung Cancer Incidence Data (per 100,000)
# Source: NI Cancer Registry
lung_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0-39",     0.5,     0.8,
  "40-44",    4.4,     5.5,
  "45-49",   17.1,    10.8,
  "50-54",   36.0,    35.0,
  "55-59",   61.9,    65.7,
  "60-64",  148.0,   126.0,
  "65-69",  224.0,   228.0,
  "70-74",  360.0,   278.0,
  "75-79",  440.0,   341.0,
  "80-84",  524.0,   331.0,
  "85-89",  528.0,   332.0,
  "90-110", 509.0,   258.0
)

# Risk Factor Relative Risks for Lung Cancer ----

# BMI - Relative Risk (sex-specific)
# Source: DYNAMO-HIA
rr_lung_bmi <- tribble(
  ~bmi, ~sex, ~RR,
  "normal", "Males", 1.0,
  "normal", "Females", 1.0,
  "overweight", "Males", 0.80,
  "overweight", "Females", 0.88,
  "obese", "Males", 0.65,
  "obese", "Females", 0.70
)

# Smoking - Relative Risk
# Source: NCD Prime
rr_lung_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 3.85,
  "current_smoker", 8.96
)

# PM2.5 - Relative Risk per 10 μg/m³
rr_lung_pm25 <- 1.09

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_lung_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(lung_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', 
  #             '70-74', '75-79', '80-84', '85-89', '90-110'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                      "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), lung_cancer_year_risk := i.incidence / 100000]
  dt[is.na(lung_cancer_year_risk), lung_cancer_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_lung_cancer_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("lung_cancer" %in% names(dt)) {
    dt <- dt[lung_cancer == 0]
  }
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_lung_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_lung_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 3. PM2.5 RR
  dt[, RR_pm25_indiv := rr_lung_pm25^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_pm25_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                      "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(lung_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, lung_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, lung_prob_min)])
}

# Function 3: Apply Risk Factors
apply_lung_cancer_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_lung_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_lung_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 3. PM2.5 RR
  dt[, RR_pm25_indiv := rr_lung_pm25^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_pm25_indiv]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                      "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), lung_prob_min := i.lung_prob_min]
  dt[is.na(lung_prob_min), lung_prob_min := 0]
  
  # Calculate individual risk
  dt[, lung_cancer_year_risk := lung_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "RR_smoking_indiv", "RR_pm25_indiv", "RR_combined", 
         "age_group_inc", "lung_prob_min") := NULL]
  
  return(dt)
}

# Example usage stored for testing
store_unit_tests <- function(){
  
  x <- past_populations %>% 
    filter(year == min(year))
  
  count(x, wt = lung_cancer_year_risk)
  
  apply_lung_risk_engine_age_sex(x) %>% 
    count(wt = lung_cancer_year_risk)
  
  t = calculate_lung_cancer_theoretical_min(x)
  lung_theoretical_min_table <- calculate_lung_cancer_theoretical_min(current_population)
  y <- apply_lung_cancer_risk_factors(x, theoretical_min_table = t)
  
  y <- y %>% mutate(age1 = cut(age, 
                                breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                                labels = c("0-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                                          "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                                right = FALSE))
  
  y %>% 
    group_by(age1, sex) %>% 
    summarise(n = n(), wt = sum(lung_cancer_year_risk)) %>% 
    mutate(lung_cancer_year_risk_per100k = wt/n*100000)
  
  input_population = past_populations %>% 
    filter(year == min(year))
  
}
