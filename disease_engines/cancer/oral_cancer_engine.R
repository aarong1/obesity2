# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# BMI Relative Risks for Oral Cancer
# Source: DYNAMO-HIA
# https://www.dynamo-hia.eu/sites/default/files/2018-04/BMI_WP7-datareport_20100317.pdf
# Note: BMI is a PROTECTIVE factor for oral cancer (RR < 1.0)

# RR overweight
# BMI 25-29.9
#
# RR obesity
# BMI 30 or more
#
# men women men women
# Cancer - Oral 0.80 0.88 0.65 0.70
# 
# Normal weight = 1.0

# Oral Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# cancer_site,age_group,all_rate,male_rate,female_rate
# Oral Cancer,0 to 34,0.7,0.6,0.7
# Oral Cancer,35 to 39,3.2,4.3,2.2
# Oral Cancer,40 to 44,6.4,10.1,3.0
# Oral Cancer,45 to 49,11.3,16.7,6.2
# Oral Cancer,50 to 54,16.1,23.0,9.5
# Oral Cancer,55 to 59,31.8,45.8,18.3
# Oral Cancer,60 to 64,38.8,49.7,28.2
# Oral Cancer,65 to 69,38.4,56.1,21.3
# Oral Cancer,70 to 74,39.6,56.3,24.4
# Oral Cancer,75 to 79,39.7,50.6,30.4
# Oral Cancer,80 to 84,36.3,56.2,21.4
# Oral Cancer,85 to 89,30.3,46.5,20.4
# Oral Cancer,90 and over,41.3,45.2,39.7

# ============================================================================
# ORAL CANCER RISK ENGINE
# ============================================================================

# Oral Cancer Risk Factors

# Risk Factors Included:
# - BMI (sex-specific categorical: protective factor with RR < 1.0)

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Oral Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
oral_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0 to 34", 0.6, 0.7,
  "35 to 39", 4.3, 2.2,
  "40 to 44", 10.1, 3.0,
  "45 to 49", 16.7, 6.2,
  "50 to 54", 23.0, 9.5,
  "55 to 59", 45.8, 18.3,
  "60 to 64", 49.7, 28.2,
  "65 to 69", 56.1, 21.3,
  "70 to 74", 56.3, 24.4,
  "75 to 79", 50.6, 30.4,
  "80 to 84", 56.2, 21.4,
  "85 to 89", 46.5, 20.4,
  "90 and over", 45.2, 39.7
)

# Risk Factor Relative Risks for Oral Cancer ----

# BMI - Relative Risk (sex-specific, PROTECTIVE factor)
# Source: DYNAMO-HIA
rr_oral_bmi <- tribble(
  ~bmi, ~sex, ~RR,
  "normal", "Males", 1.0,
  "normal", "Females", 1.0,
  "overweight", "Males", 0.80,
  "overweight", "Females", 0.88,
  "obese", "Males", 0.65,
  "obese", "Females", 0.70
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_oral_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(oral_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0 to 34', '35 to 39', '40 to 44', '45 to 49', '50 to 54', 
  #             '55 to 59', '60 to 64', '65 to 69', '70 to 74', '75 to 79', 
  #             '80 to 84', '85 to 89', '90 and over'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                      "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), oral_year_risk := i.incidence / 100000]
  dt[is.na(oral_year_risk), oral_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_oral_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("oral" %in% names(dt)) {
    dt <- dt[oral == 0]
  }
  
  # BMI RR (sex-specific, protective factor)
  rr_bmi_dt <- as.data.table(rr_oral_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                      "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_bmi_indiv, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(oral_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, oral_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, oral_prob_min)])
}

# Function 3: Apply Risk Factors
apply_oral_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # BMI RR (sex-specific, protective factor)
  rr_bmi_dt <- as.data.table(rr_oral_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                      "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), oral_prob_min := i.oral_prob_min]
  dt[is.na(oral_prob_min), oral_prob_min := 0]
  
  # Calculate individual risk
  dt[, oral_cancer_year_risk := oral_prob_min * RR_bmi_indiv]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "age_group_inc", "oral_prob_min") := NULL]
  
  return(dt)
}

# Testing ----
# Example usage:
# oral_theoretical_min_table <- calculate_oral_theoretical_min(current_population)
# result <- apply_oral_risk_factors(current_population, oral_theoretical_min_table)