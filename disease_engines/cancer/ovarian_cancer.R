# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# BMI Relative Risks for Ovarian Cancer
# Age groups: 0-17 (RR=1.0 for all BMI), 18-100 (RR varies by BMI, females only)

# BMI
# groups
# (kg/m2)
# Ovarian cancer
# Age groups
# 0-17 0-17 18-100 18-100
# M F M F
# <22.5 1.000 1.000 1.000 1.000
# 22.5-25 1.000 1.000 1.000 1.010
# 25-27.5 1.000 1.000 1.000 1.050
# 27.5-30 1.000 1.000 1.000 1.100
# 30-32.5 1.000 1.000 1.000 1.170
# >32.5 1.000 1.000 1.000 1.280

# Ovarian Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data (females only)
# Ovarian Cancer,0 to 19,0.6,NA,0.6
# Ovarian Cancer,20 to 24,5.1,NA,5.1
# Ovarian Cancer,25 to 29,3.3,NA,3.3
# Ovarian Cancer,30 to 34,5.6,NA,5.6
# Ovarian Cancer,35 to 39,12.8,NA,12.8
# Ovarian Cancer,40 to 44,8.9,NA,8.9
# Ovarian Cancer,45 to 49,19.7,NA,19.7
# Ovarian Cancer,50 to 54,28,NA,28
# Ovarian Cancer,55 to 59,35.4,NA,35.4
# Ovarian Cancer,60 to 64,35.2,NA,35.2
# Ovarian Cancer,65 to 69,49.6,NA,49.6
# Ovarian Cancer,70 to 74,54.9,NA,54.9
# Ovarian Cancer,75 to 79,74,NA,74
# Ovarian Cancer,80 to 84,67.5,NA,67.5
# Ovarian Cancer,85 to 89,74.1,NA,74.1
# Ovarian Cancer,90 and over,54.4,NA,54.4

# ============================================================================
# OVARIAN CANCER RISK ENGINE
# ============================================================================

# Ovarian Cancer Risk Factors

# Risk Factors Included:
# - BMI (age-specific, females only: RR increases with BMI for ages 18+)

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Ovarian Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data (females only - males cannot get ovarian cancer)
ovarian_incidence_per100k <- tribble(
  ~age, ~Females,
  "0 to 19", 0.6,
  "20 to 24", 5.1,
  "25 to 29", 3.3,
  "30 to 34", 5.6,
  "35 to 39", 12.8,
  "40 to 44", 8.9,
  "45 to 49", 19.7,
  "50 to 54", 28.0,
  "55 to 59", 35.4,
  "60 to 64", 35.2,
  "65 to 69", 49.6,
  "70 to 74", 54.9,
  "75 to 79", 74.0,
  "80 to 84", 67.5,
  "85 to 89", 74.1,
  "90 and over", 54.4
)

# Risk Factor Relative Risks for Ovarian Cancer ----

# BMI - Relative Risk (age-specific, females only)
# Based on BMI ranges from original data
rr_ovarian_bmi <- tribble(
  ~bmi, ~age_group_bmi, ~RR,
  "normal", "0-17", 1.0,
  "overweight", "0-17", 1.0,
  "obese", "0-17", 1.0,
  "normal", "18-110", 1.0,
  "overweight", "18-110", 1.10,   
  "obese", "18-110", 1.28          
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_ovarian_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format (females only)
  inc_dt <- as.data.table(ovarian_incidence_per100k)
  inc_dt[, sex := "Females"]
  setnames(inc_dt, "Females", "incidence")
  
  # Age groups for ovarian cancer (16 groups)
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39",
                                      "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                      "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                                      "90 and over"),
                            right = FALSE)]
  
  # Only females can get ovarian cancer
  dt[sex == "Females", ovarian_year_risk := 0]
  dt[sex == "Females" & !is.na(age_group_inc), 
     ovarian_year_risk := inc_dt[age == age_group_inc, incidence] / 100000]
  dt[sex == "Males", ovarian_year_risk := 0]
  dt[is.na(ovarian_year_risk), ovarian_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_ovarian_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Only process females
  dt <- dt[sex == "Females"]
  
  # Exclude prevalent cases
  if ("ovarian" %in% names(dt)) {
    dt <- dt[ovarian == 0]
  }
  
  # Create age groups for BMI risk (0-17 vs 18+)
  dt[, age_group_bmi := ifelse(age < 18, "0-17", "18-110")]
  
  # BMI RR (age-specific, females only)
  rr_bmi_dt <- as.data.table(rr_ovarian_bmi)
  dt[rr_bmi_dt, on = .(bmi, age_group_bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # PAF grouping (using incidence age groups)
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39",
                                      "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                      "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                                      "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_bmi_indiv, na.rm = TRUE)), by = .(age_group_inc)]
  
  inc_dt_local <- as.data.table(ovarian_incidence_per100k)
  setnames(inc_dt_local, "Females", "incidence")
  
  min_dt <- merge(inc_dt_local, paf_dt, by.x = "age", by.y = "age_group_inc")
  
  min_dt[, ovarian_prob_min := (incidence / 100000) * (1 - AF)]
  min_dt[, sex := "Females"]
  
  return(min_dt[, .(age_group_inc = age, sex, ovarian_prob_min)])
}

# Function 3: Apply Risk Factors
apply_ovarian_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # Initialize risk to 0 for all
  dt[, ovarian_cancer_year_risk := 0]
  
  # Only process females
  dt_females <- dt[sex == "Females"]
  
  if (nrow(dt_females) > 0) {
    # Create age groups for BMI risk (0-17 vs 18+)
    dt_females[, age_group_bmi := ifelse(age < 18, "0-17", "18-110")]
    
    # BMI RR (age-specific, females only)
    rr_bmi_dt <- as.data.table(rr_ovarian_bmi)
    dt_females[rr_bmi_dt, on = .(bmi, age_group_bmi), RR_bmi_indiv := i.RR]
    dt_females[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # Assign incidence age groups
    dt_females[, age_group_inc := cut(age, 
                              breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                              labels = c("0 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39",
                                        "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                        "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                                        "90 and over"),
                              right = FALSE)]
    
    # Join with theoretical minimum
    dt_females[min_dt, on = .(age_group_inc, sex), ovarian_prob_min := i.ovarian_prob_min]
    dt_females[is.na(ovarian_prob_min), ovarian_prob_min := 0]
    
    # Calculate individual risk
    dt_females[, ovarian_cancer_year_risk := ovarian_prob_min * RR_bmi_indiv]
    
    # Clean up temporary columns
    dt_females[, c("age_group_bmi", "RR_bmi_indiv", "age_group_inc", "ovarian_prob_min") := NULL]
    
    # Update main data table with female risks
    dt[sex == "Females", ovarian_cancer_year_risk := dt_females$ovarian_cancer_year_risk]
    
    dt[sex == "Males", ovarian_cancer_year_risk := 0]
    
  }
  
  return(dt)
}

# Testing ----
# Example usage:
# ovarian_theoretical_min_table <- calculate_ovarian_theoretical_min(current_population)
# result <- apply_ovarian_risk_factors(current_population, ovarian_theoretical_min_table)