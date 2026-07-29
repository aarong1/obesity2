# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# Source: DYNAMO-HIA
# https://www.dynamo-hia.eu/sites/default/files/2018-04/BMI_WP7-datareport_20100317.pdf
# Original data:
# RR overweight (BMI 25-29.9)     RR obesity (BMI 30 or more)
# men    women                     men    women
# 1.05   1.35                      1.25   1.85

# Gallbladder Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# Age         Males    Females
# 0 to 54      0.4      0.6
# 55 to 59     3.2      4.0
# 60 to 64     7.0      12.1
# 65 to 69     10.9     13.7
# 70 to 74     18.2     24.3
# 75 to 79     30.7     31.5
# 80 to 84     34.0     45.2
# 85 to 89     49.0     54.4
# 90 and over  54.9     55.7

# ============================================================================
# GALLBLADDER CANCER RISK ENGINE
# ============================================================================

# Gallbladder Cancer Risk Factors

# Risk Factors Included:
# - BMI (sex-specific)

# Source: DYNAMO-HIA
# https://www.dynamo-hia.eu/sites/default/files/2018-04/BMI_WP7-datareport_20100317.pdf
# RR overweight (BMI 25-29.9)     RR obesity (BMI 30 or more)
# men    women                     men    women
# 1.05   1.35                      1.25   1.85

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Gallbladder Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
gallbladder_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0 to 54", 0.4, 0.6,
  "55 to 59", 3.2, 4.0,
  "60 to 64", 7.0, 12.1,
  "65 to 69", 10.9, 13.7,
  "70 to 74", 18.2, 24.3,
  "75 to 79", 30.7, 31.5,
  "80 to 84", 34.0, 45.2,
  "85 to 89", 49.0, 54.4,
  "90 and over", 54.9, 55.7
)

# Risk Factor Relative Risks for Gallbladder Cancer ----

# BMI - Relative Risk (sex-specific)
# Source: DYNAMO-HIA
rr_gallbladder_bmi <- tribble(
  ~bmi, ~sex, ~RR,
  "normal", "Males", 1.0,
  "normal", "Females", 1.0,
  "overweight", "Males", 1.05,
  "overweight", "Females", 1.35,
  "obese", "Males", 1.25,
  "obese", "Females", 1.85
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_gallbladder_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(gallbladder_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0 to 54', '55 to 59', '60 to 64', '65 to 69', '70 to 74', 
  #             '75 to 79', '80 to 84', '85 to 89', '90 and over'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), gallbladder_year_risk := i.incidence / 100000]
  dt[is.na(gallbladder_year_risk), gallbladder_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_gallbladder_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("gallbladder_cancer" %in% names(dt)) {
    dt <- dt[gallbladder_cancer == 0]
  }
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_gallbladder_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Combine all RRs (only BMI for gallbladder cancer)
  dt[, RR_combined := RR_bmi_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(gallbladder_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, gallbladder_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, gallbladder_prob_min)])
}

# Function 3: Apply Risk Factors
apply_gallbladder_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_gallbladder_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Combine all RRs (only BMI for gallbladder cancer)
  dt[, RR_combined := RR_bmi_indiv]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                      "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), gallbladder_prob_min := i.gallbladder_prob_min]
  dt[is.na(gallbladder_prob_min), gallbladder_prob_min := 0]
  
  # Calculate individual risk
  dt[, gallbladder_cancer_year_risk := gallbladder_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "RR_combined", 
         "age_group_inc", "gallbladder_prob_min") := NULL]
  
  return(dt)
}

# Example usage stored for testing
store_unit_tests <- function(){
  
  x <- past_populations %>% 
    filter(year == min(year))
  
  count(x, wt = gallbladder_cancer_year_risk)
  
  apply_gallbladder_risk_engine_age_sex(x) %>% 
    count(wt = gallbladder_year_risk)
  
  t = calculate_gallbladder_theoretical_min(x)
  gallbladder_theoretical_min_table <- calculate_gallbladder_theoretical_min(current_population)
  y <- apply_gallbladder_risk_factors(x, theoretical_min_table = t)
  
  y <- y %>% mutate(age1 = cut(age, 
                                breaks = c(-Inf, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                                labels = c("0 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74",
                                          "75 to 79", "80 to 84", "85 to 89", "90 and over"),
                                right = FALSE))
  
  y %>% 
    group_by(age1, sex) %>% 
    summarise(n = n(), wt = sum(gallbladder_cancer_year_risk)) %>% 
    mutate(gallbladder_cancer_year_risk_per100k = wt/n*100000)
  
  input_population = past_populations %>% 
    filter(year == min(year))
  
}