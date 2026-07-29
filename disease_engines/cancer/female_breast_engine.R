# The fraction of cancer attributable to modifiable risk factors in England, Wales, Scotland, Northern Ireland,                    and the United Kingdom in 2015
# Supplementary Material A-E

# Female Breast Cancer Risk Factors

# Risk Factors Included:
# - BMI (age-dependent: elevated risk only for females aged 50+)
# - Alcohol consumption
# - Smoking status

# ============================================================================
# FEMALE BREAST CANCER RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Female Breast Cancer Incidence Data (per 100,000)
# Note: This applies to females only; males have negligible breast cancer risk
female_breast_incidence_per100k <- tribble(
  ~age, ~Females,
  "0 to 29", 2.2,
  "30 to 34", 33.9,
  "35 to 39", 80.0,
  "40 to 44", 125.0,
  "45 to 49", 202.2,
  "50 to 54", 279.9,
  "55 to 59", 270.4,
  "60 to 64", 332.2,
  "65 to 69", 395.1,
  "70 to 74", 340.5,
  "75 to 79", 385.7,
  "80 to 84", 411.7,
  "85 to 89", 445.1,
  "90 and over", 419.1
)

# Risk Factor Relative Risks for Female Breast Cancer ----

# Alcohol - Relative Risk

rr_breast_alcohol <- tribble(
  ~alcohol, ~RR,
  "no_risk", 1.0,       # Reference
  "lower_risk", 1.04,   # Light
  "increased_risk", 1.23, # Medium  
  "higher_risk", 1.60    # Heavy
)

# Smoking - Relative Risk

rr_breast_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,   # Reference
  "former", 1.13,         # Former/Current
  "current_smoker", 1.20  # Heavy
)

# BMI - Relative Risk (age-dependent)
# Source: https://www.dynamo-hia.eu/sites/default/files/2018-04/BMI_WP7-datareport_20100317.pdf
# Original data:
# BMI groups (kg/m2), Breast cancer, Age groups
# 0-49 0-49 50-110 50-110
# M    F    M      F
# <25:   1.000 1.000 1.000 1.000
# 25-30: 1.000 1.000 1.000 1.120
# >30:   1.000 1.000 1.000 1.250
# NOTE: Elevated risk only applies to females aged 50+

rr_breast_bmi <- tribble(
  ~bmi, ~age_group, ~RR,
  "normal", "0-49", 1.000,
  "overweight", "0-49", 1.000,
  "obese", "0-49", 1.000,
  "normal", "50-110", 1.000,
  "overweight", "50-110", 1.120,
  "obese", "50-110", 1.250
)

# Functions ----

# Function 1: Apply risk based on age and sex alone (females only)
apply_breast_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to data.table for merging
  inc_dt <- as.data.table(female_breast_incidence_per100k)
  
  # Keep original dataset for recombining
  dt_original <- copy(dt)
  
  # Filter to females only (breast cancer is overwhelmingly a female disease)
  if ("sex" %in% names(dt)) {
    dt <- dt[sex == "Females"]
  }
  
  # Age groups matching incidence table
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                      "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age), breast_year_risk := i.Females / 100000]
  dt[is.na(breast_year_risk), breast_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  # Recombine with original dataset
  if ("sex" %in% names(dt_original)) {
    # Add breast_year_risk column to original with 0 for males
    dt_original[sex != "Females", breast_year_risk := 0]
    # Update females with calculated risk
    dt_original[dt, on = names(dt)[names(dt) != "breast_year_risk"], breast_year_risk := i.breast_year_risk]
    return(dt_original)
  } else {
    return(dt)
  }
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_breast_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Filter to females only (calculation only relevant for females)
  if ("sex" %in% names(dt)) {
    dt <- dt[sex == "Females"]
  }
  
  # Exclude prevalent cases
  if ("breast_cancer" %in% names(dt)) {
    dt <- dt[breast_cancer == 0]
  }
  
  # 1. BMI RR (age-dependent: elevated risk only for age 50+)
  # Create age group for BMI risk
  dt[, age_group_bmi := ifelse(age < 50, "0-49", "50-110")]
  
  rr_bmi_dt <- as.data.table(rr_breast_bmi)
  dt[rr_bmi_dt, on = .(bmi, age_group_bmi = age_group), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  dt[, age_group_bmi := NULL]  # Clean up temporary variable
  
  # 2. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_breast_alcohol)
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 3. Smoking RR
  rr_smoking_dt <- as.data.table(rr_breast_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_alcohol_indiv * RR_smoking_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                      "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc)]
  
  inc_dt <- as.data.table(female_breast_incidence_per100k)
  
  min_dt <- merge(inc_dt, paf_dt, by.x = "age", by.y = "age_group_inc")
  
  min_dt[, breast_prob_min := (Females / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, breast_prob_min)])
}

# Function 3: Apply Risk Factors
apply_breast_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # Keep original dataset for recombining
  dt_original <- copy(dt)
  
  # Filter to females only
  if ("sex" %in% names(dt)) {
    dt <- dt[sex == "Females"]
  }
  
  # 1. BMI RR (age-dependent: elevated risk only for age 50+)
  # Create age group for BMI risk
  dt[, age_group_bmi := ifelse(age < 50, "0-49", "50-110")]
  
  rr_bmi_dt <- as.data.table(rr_breast_bmi)
  dt[rr_bmi_dt, on = .(bmi, age_group_bmi = age_group), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  dt[, age_group_bmi := NULL]  # Clean up temporary variable
  
  # 2. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_breast_alcohol)
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 3. Smoking RR
  rr_smoking_dt <- as.data.table(rr_breast_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_alcohol_indiv * RR_smoking_indiv]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                      "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc), breast_prob_min := i.breast_prob_min]
  dt[is.na(breast_prob_min), breast_prob_min := 0]
  
  # Calculate individual risk
  dt[, female_breast_cancer_year_risk := breast_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "RR_alcohol_indiv", 
         "RR_smoking_indiv", "RR_combined", 
         "age_group_inc", "breast_prob_min") := NULL]
  
  # Recombine with original dataset
  if ("sex" %in% names(dt_original)) {
    # Add female_breast_cancer_year_risk column to original with 0 for males
    dt_original[sex != "Females", female_breast_cancer_year_risk := 0]
    # Update females with calculated risk
    dt_original[dt, on = names(dt)[names(dt) != "female_breast_cancer_year_risk"], female_breast_cancer_year_risk := i.female_breast_cancer_year_risk]
    return(dt_original)
  } else {
    return(dt)
  }
}

# Example usage stored for testing
store_unit_tests <- function(){
  
  x <- past_populations %>% 
    filter(year == min(year), sex == "Females")
  
  # Test age/sex only
  y1 <- apply_breast_risk_engine_age_sex(x)
  count(y1, wt = breast_year_risk)
  
  # Test theoretical minimum calculation
  t <- calculate_breast_theoretical_min(x)
  breast_theoretical_min_table <- calculate_breast_theoretical_min(current_population)
  
  # Test risk factor application
  y2 <- apply_breast_risk_factors(x, theoretical_min_table = t)
  
  y2 <- y2 %>% 
    mutate(age1 = cut(age, 
                      breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                      labels = c("0 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                                "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                      right = FALSE))
  
  y2 %>% 
    group_by(age1) %>% 
    summarise(n = n(), wt = sum(female_breast_cancer_year_risk)) %>% 
    mutate(female_breast_cancer_year_risk_per100k = wt/n*100000)
  
  input_population <- past_populations %>% 
    filter(year == min(year), sex == "Females")
  
}
