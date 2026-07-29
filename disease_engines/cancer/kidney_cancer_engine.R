# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# BMI Relative Risks for Renal (Kidney) Cancer
# Sex-specific categorical BMI risk

# BMI
# bmi, males, females,
# overweight, 1.24, 1.32 
# obese, 1.55, 1.80

# Kidney Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# cancer_site,age_group,all_rate,male_rate,female_rate
# Kidney Cancer,0 to 39,1.1,1,1.3
# Kidney Cancer,40 to 44,6.4,7.7,5.3
# Kidney Cancer,45 to 49,11.8,15.7,8.1
# Kidney Cancer,50 to 54,18.4,25.8,11.3
# Kidney Cancer,55 to 59,27.3,36.6,18.3
# Kidney Cancer,60 to 64,37.1,54.6,20
# Kidney Cancer,65 to 69,44,64.6,23.9
# Kidney Cancer,70 to 74,55.8,75.9,37.5
# Kidney Cancer,75 to 79,61,79.4,45.4
# Kidney Cancer,80 to 84,71.6,120.1,35.4
# Kidney Cancer,85 to 89,74.1,120.5,46
# Kidney Cancer,90 and over,67.9,105.5,52.3

# ============================================================================
# KIDNEY CANCER RISK ENGINE
# ============================================================================

# Kidney Cancer Risk Factors

# Risk Factors Included:
# - BMI (sex-specific categorical: overweight and obese)

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Kidney Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
kidney_cancer_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0 to 39", 1.0, 1.3,
  "40 to 44", 7.7, 5.3,
  "45 to 49", 15.7, 8.1,
  "50 to 54", 25.8, 11.3,
  "55 to 59", 36.6, 18.3,
  "60 to 64", 54.6, 20.0,
  "65 to 69", 64.6, 23.9,
  "70 to 74", 75.9, 37.5,
  "75 to 79", 79.4, 45.4,
  "80 to 84", 120.1, 35.4,
  "85 to 89", 120.5, 46.0,
  "90 and over", 105.5, 52.3
)

# Risk Factor Relative Risks for Kidney Cancer ----

# BMI - Relative Risk (sex-specific)
rr_kidney_bmi <- tribble(
  ~bmi, ~sex, ~RR,
  "normal", "Males", 1.0,
  "normal", "Females", 1.0,
  "overweight", "Males", 1.24,
  "overweight", "Females", 1.32,
  "obese", "Males", 1.55,
  "obese", "Females", 1.80
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_kidney_cancer_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(kidney_cancer_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0 to 39', '40 to 44', '45 to 49', '50 to 54', '55 to 59', 
  #             '60 to 64', '65 to 69', '70 to 74', '75 to 79', '80 to 84', 
  #             '85 to 89', '90 and over'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
                                      "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                      "85 to 89", "90 and over"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), kidney_year_risk := i.incidence / 100000]
  dt[is.na(kidney_year_risk), kidney_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_kidney_cancer_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("kidney_cancer" %in% names(dt)) {
    dt <- dt[kidney_cancer == 0]
  }
  
  # BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_kidney_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
                                      "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                      "85 to 89", "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_bmi_indiv, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(kidney_cancer_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, kidney_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, kidney_prob_min)])
}

# Function 3: Apply Risk Factors
apply_kidney_cancer_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_kidney_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59",
                                      "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                                      "85 to 89", "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), kidney_prob_min := i.kidney_prob_min]
  dt[is.na(kidney_prob_min), kidney_prob_min := 0]
  
  # Calculate individual risk
  dt[, kidney_cancer_year_risk := kidney_prob_min * RR_bmi_indiv]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "age_group_inc", "kidney_prob_min") := NULL]
  
  return(dt)
}

# Testing ----
# Example usage:
# kidney_theoretical_min_table <- calculate_kidney_theoretical_min(current_population)
# result <- apply_kidney_risk_factors(current_population, kidney_theoretical_min_table)