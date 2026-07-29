# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# BMI Relative Risks for Pancreatic Cancer
# Age groups: 0-17 (RR=1.0 for all BMI), 18-100 (RR varies by BMI)

# BMI
# groups
# (kg/m2)
# Pancreatic cancer
# Age groups
# 0-17 0-17 18-100 18-100
# M F M F
# <25 1.000 1.000 1.000 1.000
# 25-30 1.000 1.000 1.140 1.140
# >30 1.000 1.000 1.300 1.300

# Smoking Relative Risk for Pancreatic Cancer
# Source: SAPM
# Pancreatic cancer	1.9	All	Both	smokes	Smoking	morbidity	SAPM

# Pancreatic Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# cancer_site,age_group,all_rate,male_rate,female_rate
# Pancreatic Cancer,45 to 49,4.3,5.2,3.4
# Pancreatic Cancer,50 to 54,6.5,7.8,5.4
# Pancreatic Cancer,55 to 59,13.5,17.3,9.8
# Pancreatic Cancer,60 to 64,23.3,30.2,16.7
# Pancreatic Cancer,65 to 69,40.4,45.8,35.2
# Pancreatic Cancer,70 to 74,63.6,75.9,52.5
# Pancreatic Cancer,75 to 79,73.6,85.8,63.3
# Pancreatic Cancer,80 to 84,96.1,105.7,88.9
# Pancreatic Cancer,85 to 89,117.9,148,99.7
# Pancreatic Cancer,90 and over,98.9,110.5,94.1

# ============================================================================
# PANCREATIC CANCER RISK ENGINE
# ============================================================================

# Pancreatic Cancer Risk Factors

# Risk Factors Included:
# - BMI (age-specific: RR varies for ages 18+)
# - Smoking (RR = 1.9 from SAPM)

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Pancreatic Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
pancreatic_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0 to 44", 0, 0,  # No data for younger ages
  "45 to 49", 5.2, 3.4,
  "50 to 54", 7.8, 5.4,
  "55 to 59", 17.3, 9.8,
  "60 to 64", 30.2, 16.7,
  "65 to 69", 45.8, 35.2,
  "70 to 74", 75.9, 52.5,
  "75 to 79", 85.8, 63.3,
  "80 to 84", 105.7, 88.9,
  "85 to 89", 148.0, 99.7,
  "90 and over", 110.5, 94.1
)

# Risk Factor Relative Risks for Pancreatic Cancer ----

# BMI - Relative Risk (age-specific)
rr_pancreatic_bmi <- tribble(
  ~bmi, ~age_group_bmi, ~RR,
  "normal", "0-17", 1.0,
  "overweight", "0-17", 1.0,
  "obese", "0-17", 1.0,
  "normal", "18-110", 1.0,
  "overweight", "18-110", 1.14,
  "obese", "18-110", 1.30
)

# Smoking - Relative Risk
# Source: SAPM
rr_pancreatic_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.9,
  "current_smoker", 1.9
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_pancreatic_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(pancreatic_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0 to 44', '45 to 49', '50 to 54', '55 to 59', '60 to 64', 
  #             '65 to 69', '70 to 74', '75 to 79', '80 to 84', '85 to 89', 
  #             '90 and over'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                      "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                                      "90 and over"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), pancreatic_year_risk := i.incidence / 100000]
  dt[is.na(pancreatic_year_risk), pancreatic_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_pancreatic_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("pancreatic" %in% names(dt)) {
    dt <- dt[pancreatic == 0]
  }
  
  # Create age groups for BMI risk (0-17 vs 18+)
  dt[, age_group_bmi := ifelse(age < 18, "0-17", "18-110")]
  
  # 1. BMI RR (age-specific)
  rr_bmi_dt <- as.data.table(rr_pancreatic_bmi)
  dt[rr_bmi_dt, on = .(bmi, age_group_bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_pancreatic_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                      "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                                      "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(pancreatic_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, pancreatic_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, pancreatic_prob_min)])
}

# Function 3: Apply Risk Factors
apply_pancreatic_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # Create age groups for BMI risk (0-17 vs 18+)
  dt[, age_group_bmi := ifelse(age < 18, "0-17", "18-110")]
  
  # 1. BMI RR (age-specific)
  rr_bmi_dt <- as.data.table(rr_pancreatic_bmi)
  dt[rr_bmi_dt, on = .(bmi, age_group_bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_pancreatic_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                      "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                                      "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), pancreatic_prob_min := i.pancreatic_prob_min]
  dt[is.na(pancreatic_prob_min), pancreatic_prob_min := 0]
  
  # Calculate individual risk
  dt[, pancreatic_cancer_year_risk := pancreatic_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("age_group_bmi", "RR_bmi_indiv", "RR_smoking_indiv", "RR_combined", 
         "age_group_inc", "pancreatic_prob_min") := NULL]
  
  return(dt)
}

# Testing ----
# Example usage:
# pancreatic_theoretical_min_table <- calculate_pancreatic_theoretical_min(current_population)
# result <- apply_pancreatic_risk_factors(current_population, pancreatic_theoretical_min_table)