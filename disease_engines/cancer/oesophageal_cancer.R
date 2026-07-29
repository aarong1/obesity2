# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# BMI Relative Risks for Oesophageal Cancer
# Sex-specific categorical BMI risk
# UKHF model takes the negative risk factor for oesophageal cancer 
# DYNAMO_HIA actually has two squamous cell and adenocarcinoma, one positive and one negative

# bmi, males, females
# overweight, 1.60, 1.50
# obese, 2.45, 2.15


# Oesophageal Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# cancer_site,age_group,all_rate,male_rate,female_rate
# Oesophageal Cancer,0 to 54,1.6,2.5,0.6
# Oesophageal Cancer,55 to 59,14.3,23,5.9
# Oesophageal Cancer,60 to 64,26.6,43.8,10
# Oesophageal Cancer,65 to 69,38.6,58,19.6
# Oesophageal Cancer,70 to 74,43.9,72.2,17.7
# Oesophageal Cancer,75 to 79,56.5,87.6,29.8
# Oesophageal Cancer,80 to 84,52.3,74.4,35.5
# Oesophageal Cancer,85 to 89,51.5,75.5,36.7
# Oesophageal Cancer,90 and over,64.2,89.8,53.7

# ============================================================================
# OESOPHAGEAL CANCER RISK ENGINE
# ============================================================================

# Oesophageal Cancer Risk Factors

# Risk Factors Included:
# - BMI (sex-specific categorical: overweight and obese)

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Oesophageal Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
oesophageal_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0 to 54", 2.5, 0.6,
  "55 to 59", 23.0, 5.9,
  "60 to 64", 43.8, 10.0,
  "65 to 69", 58.0, 19.6,
  "70 to 74", 72.2, 17.7,
  "75 to 79", 87.6, 29.8,
  "80 to 84", 74.4, 35.5,
  "85 to 89", 75.5, 36.7,
  "90 and over", 89.8, 53.7
)

# Risk Factor Relative Risks for Oesophageal Cancer ----

# BMI - Relative Risk (sex-specific)
rr_oesophageal_bmi <- tribble(
  ~bmi, ~sex, ~RR,
  "normal", "Males", 1.0,
  "normal", "Females", 1.0,
  "overweight", "Males", 1.60,
  "overweight", "Females", 1.50,
  "obese", "Males", 2.45,
  "obese", "Females", 2.15
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_oesophageal_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(oesophageal_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0 to 54', '55 to 59', '60 to 64', '65 to 69', '70 to 74', 
  #             '75 to 79', '80 to 84', '85 to 89', '90 and over'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), oesophageal_year_risk := i.incidence / 100000]
  dt[is.na(oesophageal_year_risk), oesophageal_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_oesophageal_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("oesophageal" %in% names(dt)) {
    dt <- dt[oesophageal == 0]
  }
  
  # BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_oesophageal_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_bmi_indiv, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(oesophageal_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, oesophageal_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, oesophageal_prob_min)])
}

# Function 3: Apply Risk Factors
apply_oesophageal_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_oesophageal_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), oesophageal_prob_min := i.oesophageal_prob_min]
  dt[is.na(oesophageal_prob_min), oesophageal_prob_min := 0]
  
  # Calculate individual risk
  dt[, oesophageal_cancer_year_risk := oesophageal_prob_min * RR_bmi_indiv]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "age_group_inc", "oesophageal_prob_min") := NULL]
  
  return(dt)
}

# Testing ----
# Example usage:
# oesophageal_theoretical_min_table <- calculate_oesophageal_theoretical_min(current_population)
# result <- apply_oesophageal_risk_factors(current_population, oesophageal_theoretical_min_table)

