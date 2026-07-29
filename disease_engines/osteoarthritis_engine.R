# ============================================================================
# OSTEOARTHRITIS (OA) RISK ENGINE
# ============================================================================

# Original Raw Incidence Data ----
# osteoarthritis_incidence <- tibble::tribble(
#   ~age,  ~Males, ~Females,
#   "0-19",   0.0,    0.0,
#   "20–24",  0.54,   0.63,
#   "25–29",  0.91,   0.82,
#   "30–34",  1.54,   1.55,
#   "35–39",  2.55,   2.59,
#   "40–44",  4.05,   4.09,
#   "45–49",  6.22,   7.32,
#   "50–54",   8.1,  12.04,
#   "55–59", 11.36,  18.21,
#   "60–64", 14.66,  22.48,
#   "65–69", 17.59,  26.93,
#   "70–74",  20.3,     31,
#   "75–79", 23.26,  34.47,
#   "80–84", 24.22,  33.77,
#   "85–89", 25.77,  33.42,
#   "90-110", 25.54,  31.55
# ) %>% 
#   pivot_longer(-1,names_to = 'sex',values_to = 'per1k') %>% 
#   mutate(osteoarthritis_year_risk = per1k/1000) %>% 
#   select(-per1k)

# Risk Factors:
# BMI (sex-specific): 
# Source: https://www.sciencedirect.com/science/article/pii/S1063458409002829#tbl3
# 	Underweight BMI < 18.5	Normal weight 18.5 ≤ BMI < 25	Overweight 25 ≤ BMI < 30	Obese BMI ≥ 30
# Females	0.33	1.0	1.76	2.03
# Males	0.00	1.0	1.07	1.69

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# OA Incidence Data (per 1,000 person-years)
oa_incidence_per1k <- tribble(
  ~age, ~Males, ~Females,
  "0-19", 0.0, 0.0,
  "20-24", 0.54, 0.63,
  "25-29", 0.91, 0.82,
  "30-34", 1.54, 1.55,
  "35-39", 2.55, 2.59,
  "40-44", 4.05, 4.09,
  "45-49", 6.22, 7.32,
  "50-54", 8.1, 12.04,
  "55-59", 11.36, 18.21,
  "60-64", 14.66, 22.48,
  "65-69", 17.59, 26.93,
  "70-74", 20.3, 31.0,
  "75-79", 23.26, 34.47,
  "80-84", 24.22, 33.77,
  "85-89", 25.77, 33.42,
  "90-110", 25.54, 31.55
)

# Risk Factor Relative Risks for OA ----

# BMI - Relative Risk (sex-specific)
# Source: https://www.sciencedirect.com/science/article/pii/S1063458409002829#tbl3
rr_oa_bmi <- tribble(
  ~bmi, ~sex, ~RR,
  "underweight", "Females", 0.33,
  "underweight", "Males", 1.0,  # Original showed 0.00, using 1.0 (reference)
  "normal", "Females", 1.0,
  "normal", "Males", 1.0,
  "overweight", "Females", 1.76,
  "overweight", "Males", 1.07,
  "obese", "Females", 2.03,
  "obese", "Males", 1.69
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_oa_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(oa_incidence_per1k)
  inc_dt <- melt(inc_dt, id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54',
  #             '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-110'
  dt[, age_group_oa := cut(age, 
                           breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                           labels = c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                     "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                           right = FALSE)]
  
  dt[inc_dt, on = .(age_group_oa = age, sex), oa_year_risk := i.incidence / 1000]
  dt[is.na(oa_year_risk), oa_year_risk := 0]
  
  dt[, age_group_oa := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_oa_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("osteoarthritis" %in% names(dt)) {
    dt <- dt[osteoarthritis == 0]
  }
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_oa_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Combine all RRs (currently only BMI)
  dt[, RR_combined := RR_bmi_indiv]
  
  # PAF grouping
  dt[, age_group_oa := cut(age, 
                           breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                           labels = c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                     "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                           right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_oa, sex)]
  
  inc_dt <- as.data.table(oa_incidence_per1k)
  inc_dt <- melt(inc_dt, id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, 
                  by.x = c("age", "sex"), by.y = c("age_group_oa", "sex"))
  
  min_dt[, oa_prob_min := (incidence / 1000) * (1 - AF)]
  
  return(min_dt[, .(age_group_oa = age, sex, oa_prob_min)])
}

# Function 3: Apply Risk Factors
apply_oa_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_oa_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # Combine all RRs (currently only BMI)
  dt[, RR_combined := RR_bmi_indiv]
  
  # Assign age groups
  dt[, age_group_oa := cut(age, 
                           breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                           labels = c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                     "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                           right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_oa, sex), oa_prob_min := i.oa_prob_min]
  dt[is.na(oa_prob_min), oa_prob_min := 0]
  
  # Calculate individual risk
  dt[, osteoarthritis_year_risk := oa_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "RR_combined", "age_group_oa", "oa_prob_min") := NULL]
  
  return(dt)
}

# Unit Tests and Examples ----
store_unit_tests <- function() {
  
  # Example usage with historical population
  # x <- past_populations %>% 
  #     filter(year == min(year))
  
  # Test age/sex only
  # y_agesex <- apply_oa_risk_engine_age_sex(x)
  
  # Calculate theoretical minimum
  # oa_theoretical_min <- calculate_oa_theoretical_min(x)
  
  # Apply risk factors
  # y <- apply_oa_risk_factors(x, oa_theoretical_min)
  
  # Check aggregated risk by age group
  # y %>% 
  #     mutate(age1 = cut(age, 
  #                      breaks = c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
  #                      labels = c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
  #                                "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
  #                      right = FALSE)) %>%
  #     group_by(age1, sex) %>% 
  #     summarise(n = n(), wt = sum(osteoarthritis_year_risk)) %>% 
  #     mutate(oa_year_risk_per1k = wt / n * 1000)
}
