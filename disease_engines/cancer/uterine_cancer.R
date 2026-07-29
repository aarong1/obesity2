# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# Uterine (Endometrial) Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# cancer_site,age_group,all_rate,male_rate,female_rate
# Uterine Cancer,0 to 34,0.5,NA,0.5
# Uterine Cancer,35 to 39,2.2,NA,2.2
# Uterine Cancer,40 to 44,5.300000000000001,NA,5.300000000000001
# Uterine Cancer,45 to 49,15.9,NA,15.9
# Uterine Cancer,50 to 54,39.1,NA,39.1
# Uterine Cancer,55 to 59,56,NA,56
# Uterine Cancer,60 to 64,69.4,NA,69.4
# Uterine Cancer,65 to 69,83.10000000000001,NA,83.10000000000001
# Uterine Cancer,70 to 74,88.60000000000001,NA,88.60000000000001
# Uterine Cancer,75 to 79,105.60000000000001,NA,105.60000000000001
# Uterine Cancer,80 to 84,90.60000000000001,NA,90.60000000000001
# Uterine Cancer,85 to 89,61.300000000000004,NA,61.300000000000004
# Uterine Cancer,90 and over,46,NA,46

# BMI Relative Risks for Endometrial (Uterine) Cancer
# Source: Categorical BMI data
# For females age 20+:
#   overweight: 1.50 
#   obese: 2.50
# For age 0-19: RR = 1.0 (no effect)
# Males: RR = 1.0 (uterine cancer affects females only)

# ============================================================================
# UTERINE CANCER RISK ENGINE
# ============================================================================

# Risk Factors Included:
# - BMI (categorical, age-specific, females only)

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Uterine Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# Note: Uterine cancer affects females only
uterine_incidence_per100k <- tribble(
  ~age, ~Females,
  "0 to 34", 0.5,
  "35 to 39", 2.2,
  "40 to 44", 5.3,
  "45 to 49", 15.9,
  "50 to 54", 39.1,
  "55 to 59", 56.0,
  "60 to 64", 69.4,
  "65 to 69", 83.1,
  "70 to 74", 88.6,
  "75 to 79", 105.6,
  "80 to 84", 90.6,
  "85 to 89", 61.3,
  "90 and over", 46.0
)

# Risk Factor Relative Risks for Uterine Cancer ----

# BMI - Relative Risk (age-specific, females only)
rr_uterine_bmi <- tribble(
  ~bmi, ~age_group_bmi, ~RR,
  "normal", "0-19", 1.0,
  "overweight", "0-19", 1.0,
  "obese", "0-19", 1.0,
  "normal", "20-110", 1.0,
  "overweight", "20-110", 1.50,
  "obese", "20-110", 2.50
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_uterine_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  # Males have zero risk (uterine cancer is female-only)
  inc_dt <- as.data.table(uterine_incidence_per100k) %>%
    mutate(Males = 0) %>%
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
  
  dt[inc_dt, on = .(age_group_inc = age, sex), uterine_year_risk := i.incidence / 100000]
  dt[is.na(uterine_year_risk), uterine_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_uterine_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("uterine" %in% names(dt)) {
    dt <- dt[uterine == 0]
  }
  
  # Create age groups for BMI risk (0-19 vs 20+)
  dt[, age_group_bmi := ifelse(age < 20, "0-19", "20-110")]
  
  # BMI Relative Risk (categorical, age-specific, females only)
  # Males get RR = 1.0 regardless of BMI
  rr_bmi_dt <- as.data.table(rr_uterine_bmi)
  dt[rr_bmi_dt, on = .(bmi, age_group_bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Set males to RR = 1.0 (uterine cancer is female-only)
  dt[sex == "Males", RR_bmi_indiv := 1.0]
  
  # Combined RR (only BMI for uterine cancer)
  dt[, RR_combined := RR_bmi_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                      "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt_local <- as.data.table(uterine_incidence_per100k) %>%
    mutate(Males = 0) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt_local), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, uterine_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, uterine_prob_min)])
}

# Function 3: Apply Risk Factors
apply_uterine_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # Create age groups for BMI risk (0-19 vs 20+)
  dt[, age_group_bmi := ifelse(age < 20, "0-19", "20-110")]
  
  # BMI Relative Risk (categorical, age-specific, females only)
  # Males get RR = 1.0 regardless of BMI
  rr_bmi_dt <- as.data.table(rr_uterine_bmi)
  dt[rr_bmi_dt, on = .(bmi, age_group_bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Set males to RR = 1.0 (uterine cancer is female-only)
  dt[sex == "Males", RR_bmi_indiv := 1.0]
  
  # Combined RR (only BMI for uterine cancer)
  dt[, RR_combined := RR_bmi_indiv]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                      "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), uterine_prob_min := i.uterine_prob_min]
  dt[is.na(uterine_prob_min), uterine_prob_min := 0]
  
  # Calculate individual risk
  dt[, uterine_cancer_year_risk := uterine_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("age_group_bmi", "RR_bmi_indiv", "RR_combined", "age_group_inc", "uterine_prob_min") := NULL]
  
  return(dt)
}

# Testing ----
# Example usage:
# uterine_theoretical_min_table <- calculate_uterine_theoretical_min(current_population)
# result <- apply_uterine_risk_factors(current_population, uterine_theoretical_min_table)
