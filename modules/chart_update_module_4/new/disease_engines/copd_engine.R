library(tibble)
library(dplyr)
library(data.table)

# Data Definitions

copd_incidence_per100k <- 
  tribble(
    ~'age', ~'Males', ~'Females',
    '0-4', 1.2, 0.0,
    '5-9', 2.6, 3.4,
    '10-14', 2.1, 1.6,
    '15-19', 0.0, 0.0,
    '20-24', 1.8, 1.3,
    '25-29', 3.4, 3.4,
    '30-34', 7.9, 9.8,
    '35-39', 25.0, 28.2,
    '40-44', 52.0, 60.4,
    '45-49', 106.0, 121.2,
    '50-54', 205.2, 174.5,
    '55-59', 298.0, 235.4,
    '60-64', 491.9, 326.9,
    '65-69', 437.5, 216.8,
    '70-74', 334.8, 228.0,
    '75-79', 263.4, 0.0,
    '80-84', 0.0, 0.0,
    '85-110', 0.0, 0.0
  )

pm25_copd_rr <- 1.06 # per 5 ug/m3

inc_dt <- as.data.table(copd_incidence_per100k) %>%
  melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Function 1: Apply risk based on age and gender alone
apply_copd_risk_base <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Define breaks for 0-4, 5-9 ... 85-110
  # Using right=FALSE: [0,5) -> 0,1,2,3,4.
  dt[, age_group_inc := cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 111),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85-110"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), copd_year_risk := i.incidence / 100000]
  dt[is.na(copd_year_risk), copd_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum (Combined)
calculate_copd_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Filter for population at risk (exclude prevalent cases)
  if ("copd" %in% names(dt)) {
    dt <- dt[copd == 0]
  }
  
  # RR Smoke
  # Current: 3.5, Former: 1.9, Never: 1.0
  dt[, RR_smoke := fcase(
      smoking == "current_smoker", 3.5,
      smoking %in% c("former_regular", "former_irregular", "former"), 1.9,
      smoking == "never_smoked", 1.0,
      default = 1.0
  )]
  
  # RR PM2.5 (1.06 per 5 ug/m3)
  dt[, RR_pm25 := pm25_copd_rr^(pm25g / 5)]
  
  dt[, RR_combined := RR_smoke * RR_pm25]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 111),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85-110"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(copd_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, copd_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, copd_prob_min)])
}

# Function 3: Apply Risk using Factors (Combined)
apply_copd_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # RR Smoke
  dt[, RR_smoke := fcase(
      smoking == "current_smoker", 3.5,
      smoking %in% c("former_regular", "former_irregular", "former"), 1.9,
      smoking == "never_smoked", 1.0,
      default = 1.0
  )]
  
  # RR PM2.5 (1.06 per 5 ug/m3)
  dt[, RR_pm25 := pm25_copd_rr^(pm25g / 5)]
  
  dt[, RR_combined := RR_smoke * RR_pm25]
  
  dt[, age_group_inc := cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 111),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85-110"),
                            right = FALSE)]
  
  dt[min_dt, on = .(age_group_inc, sex), copd_prob_min := i.copd_prob_min]
  dt[is.na(copd_prob_min), copd_prob_min := 0]
  
  dt[, copd_year_risk := copd_prob_min * RR_combined]
  
  dt[, c("RR_smoke", "RR_pm25", "RR_combined", "age_group_inc", "copd_prob_min") := NULL]
  
  return(dt)
}

# Function 4: Calculate PAF and Theoretical Minimum (Pollution Only)
calculate_copd_theoretical_min_pollution_only <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Filter for population at risk
  if ("copd" %in% names(dt)) {
    dt <- dt[copd == 0]
  }
  
  # Only consider PM2.5 risk
  dt[, RR_pm25 := pm25_copd_rr^(pm25g / 5)]
  dt[, RR_combined := RR_pm25] 
  
  dt[, age_group_inc := cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 111),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85-110"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(copd_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, copd_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, copd_prob_min)])
}

# Function 5: Apply Risk using Pollution Factor Only
apply_copd_risk_pollution_only <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # Only RR PM2.5
  dt[, RR_pm25 := pm25_copd_rr^(pm25g / 5)]
  dt[, RR_combined := RR_pm25]
  
  dt[, age_group_inc := cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 111),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85-110"),
                            right = FALSE)]
  
  dt[min_dt, on = .(age_group_inc, sex), copd_prob_min := i.copd_prob_min]
  dt[is.na(copd_prob_min), copd_prob_min := 0]
  
  dt[, copd_year_risk := copd_prob_min * RR_combined]
  
  dt[, c("RR_pm25", "RR_combined", "age_group_inc", "copd_prob_min") := NULL]
  
  return(dt)
}


# Function 6: Calculate PAF per Risk Factor
calculate_copd_paf_by_risk_factor <- function(input_population) {
  dt <- as.data.table(input_population)
  
  if ("copd" %in% names(dt)) {
    dt <- dt[copd == 0]
  }

  dt[, RR_smoke := fcase(
      smoking == "current_smoker", 3.5,
      smoking %in% c("former_regular", "former_irregular", "former"), 1.9,
      smoking == "never_smoked", 1.0,
      default = 1.0
  )]
  
  dt[, RR_pm25 := pm25_copd_rr^(pm25g / 5)]
  dt[, RR_current := RR_smoke * RR_pm25]
  dt[, RR_no_smoking := 1.0 * RR_pm25]
  dt[, RR_no_pm25 := RR_smoke * 1.0]
  dt[, RR_min := 1.0 * 1.0]
  
  sum_RR_current <- sum(dt$RR_current, na.rm = TRUE)
  sum_RR_no_smoking <- sum(dt$RR_no_smoking, na.rm = TRUE)
  sum_RR_no_pm25 <- sum(dt$RR_no_pm25, na.rm = TRUE)
  sum_RR_min <- sum(dt$RR_min, na.rm = TRUE)
  
  paf_smoking <- (sum_RR_current - sum_RR_no_smoking) / sum_RR_current
  paf_pm25 <- (sum_RR_current - sum_RR_no_pm25) / sum_RR_current
  paf_total <- (sum_RR_current - sum_RR_min) / sum_RR_current
  
  result <- data.table(
    risk_factor = c("smoking", "pm25", "total"),
    paf = c(paf_smoking, paf_pm25, paf_total)
  )
  
  return(result)
}

# Examples
if (interactive()) {
  
  # Sample Population
  sample_pop <- data.table(
    pid = 1:10,
    age = rep(c(40, 50, 60, 70, 80), each = 2),
    sex = rep(c("Males", "Females"), 5),
    smoking = sample(c("current_smoker", "former_regular", "never_smoked"), 10, replace = TRUE),
    pm25g = runif(10, 5, 20),
    copd = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0) # One prevalent case
  )
  
  # Example 1: apply_copd_risk_base
  pop_base_risk <- apply_copd_risk_base(sample_pop)
  print("Base Risk (First 5):")
  print(pop_base_risk[1:5, .(pid, age, sex, copd_year_risk)])
  
  # Example 2: calculate_copd_theoretical_min (Combined)
  theo_min_table <- calculate_copd_theoretical_min(sample_pop)
  print("Theoretical Minimum Table (Combined - First 5):")
  print(theo_min_table[1:5, .(age_group_inc, sex, copd_prob_min)])
  
  # Example 3: apply_copd_risk_factors (Combined)
  pop_factor_risk <- apply_copd_risk_factors(sample_pop, theo_min_table)
  print("Factor-Adjusted Risk (Combined - First 5):")
  print(pop_factor_risk[1:5, .(pid, age, sex, smoking, pm25g, copd_year_risk)])
  
  # Example 4: Consistency Check
  pop_factor_risk[, age_group := cut(age, 
                                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 111),
                                     labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                                "80-84", "85-110"),
                                     right = FALSE)]
  
  mean_risk_by_group <- pop_factor_risk[, .(calculated_mean_risk = mean(copd_year_risk)), by = .(age_group, sex)]
  
  inc_table_long <- as.data.table(copd_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence_per_100k")
  inc_table_long[, expected_risk := incidence_per_100k / 100000]
  
  comparison <- merge(mean_risk_by_group, inc_table_long, by.x = c("age_group", "sex"), by.y = c("age", "sex"), all.x = TRUE)
  print("Consistency Check:")
  print(comparison[, .(age_group, sex, calculated_mean_risk, expected_risk)])

  # Example 5: Pollution Only Functions
  theo_min_pollution <- calculate_copd_theoretical_min_pollution_only(sample_pop)
  print("Theoretical Minimum (Pollution Only - First 5):")
  print(theo_min_pollution[1:5])
  
  pop_pollution_risk <- apply_copd_risk_pollution_only(sample_pop, theo_min_pollution)
  print("Pollution-Only Risk (First 5):")
  print(pop_pollution_risk[1:5, .(pid, age, sex, pm25g, copd_year_risk)])
  
  # Example 6: PAF by Risk Factor
  paf_summary <- calculate_copd_paf_by_risk_factor(sample_pop)
  print("PAF Summary:")
  print(paf_summary)
}
