# https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002602

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions

stroke_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  '0-44', 110.0, 110.0,
  '45-54', 890.0, 790.0, 
  '55-64', 2690.0, 1960.0,
  '65-74', 6400.0, 4390.0, 
  '75-110', 14890.0, 12430.0
)

bmi_stroke_rr <- tribble(
  ~age, ~RR,
  "0-24", 1.0,
  "25-29", 2.472,
  "30-34", 2.472,
  "35-39", 2.235,
  "40-44", 1.979,
  "45-49", 1.826,
  "50-54", 1.733,
  "55-59", 1.635,
  "60-64", 1.543,
  "65-69", 1.455,
  "70-74", 1.38,
  "75-79", 1.304,
  "80-84", 1.228,
  "85-89", 1.068,
  "90-94", 1.068,
  "95-110", 1.068
)

pm25_stroke_rr <- 1.13

inc_dt <- as.data.table(stroke_incidence_per100k) %>%
  melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Function 1: Apply risk based on age and gender alone
apply_stroke_risk_base <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Incidence groups: '0-44', '45-54', '55-64', '65-74', '75-110'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 44, 54, 64, 74, 110),
                            labels = c("0-44", "45-54", "55-64", "65-74", "75-110"),
                            right = TRUE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), stroke_year_risk := i.incidence / 100000]
  dt[is.na(stroke_year_risk), stroke_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_stroke_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("stroke" %in% names(dt)) {
    dt <- dt[stroke == 0]
  }
  
  # 1. Map Age to BMI RR groups
  # 0-24, 25-29, 30-34, ..., 95-110
  breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
  labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", 
                  "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")
                  
  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
  
  rr_bmi_dt <- as.data.table(bmi_stroke_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
  
  # BMI Logic (20, 30, 37)
  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 30,
    bmi == "obese", 37,
    default = 20
  )]
  
  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # PM2.5 Logic
  dt[, RR_pm25_indiv := pm25_stroke_rr^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 44, 54, 64, 74, 110),
                            labels = c("0-44", "45-54", "55-64", "65-74", "75-110"),
                            right = TRUE)]
                            
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(stroke_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
    
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, stroke_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, stroke_prob_min)])
}


stroke_theoretical_min_table <- calculate_stroke_theoretical_min(current_population)
# Function 3: Apply Risk Factors
apply_stroke_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
  labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", 
                  "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")
  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
  
  rr_bmi_dt <- as.data.table(bmi_stroke_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
  
  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 30,
    bmi == "obese", 37,
    default = 20
  )]
  
  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  dt[, RR_pm25_indiv := pm25_stroke_rr^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
  
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 44, 54, 64, 74, 110),
                            labels = c("0-44", "45-54", "55-64", "65-74", "75-110"),
                            right = TRUE)]
                            
  dt[min_dt, on = .(age_group_inc, sex), stroke_prob_min := i.stroke_prob_min]
  dt[is.na(stroke_prob_min), stroke_prob_min := 0]
  
  dt[, stroke_year_risk := stroke_prob_min * RR_combined]
  
  dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
         "RR_pm25_indiv", "RR_combined", "age_group_inc", "stroke_prob_min") := NULL]
         
  return(dt)
}

# Examples
if (interactive()) {

  # Sample Population
  sample_pop <- data.table(
    pid = 1:10,
    age = rep(c(40, 50, 60, 70, 80), each = 2),
    sex = rep(c("Males", "Females"), 5),
    bmi = sample(c("normal", "overweight", "obese"), 10, replace = TRUE),
    pm25g = runif(10, 5, 20),
    stroke = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0) # One prevalent case
  )

  # Example 1: apply_stroke_risk_base
  pop_base_risk <- apply_stroke_risk_base(sample_pop)
  print("Base Risk Result:")
  print(pop_base_risk[, .(pid, age, sex, stroke_year_risk)])

  # Example 2: calculate_stroke_theoretical_min
  theo_min_table <- calculate_stroke_theoretical_min(sample_pop)
  print("Theoretical Minimum Table:")
  print(theo_min_table[, .(age_group_inc, sex, stroke_prob_min)])

  # Verification
  expected_combinations <- as.data.table(stroke_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex") %>%
    .[, .(age_group_inc = age, sex)]
  
  missing_combinations <- fsetdiff(expected_combinations, theo_min_table[, .(age_group_inc, sex)])
  
  if (nrow(missing_combinations) == 0) {
    print("Verification Passed: All age and sex combinations present.")
  } else {
    print("Verification Failed: Missing combinations:")
    print(missing_combinations)
  }

  # Example 3: apply_stroke_risk_factors
  pop_factor_risk <- apply_stroke_risk_factors(sample_pop, theo_min_table)
  print("Factor-Adjusted Risk Result:")
  print(pop_factor_risk[, .(pid, age, sex, bmi, pm25g, stroke_year_risk)])

  # Example 4: Consistency Check
  pop_factor_risk[, age_group := cut(age, 
                                     breaks = c(-Inf, 44, 54, 64, 74, 110),
                                     labels = c("0-44", "45-54", "55-64", "65-74", "75-110"),
                                     right = TRUE)]
  
  mean_risk_by_group <- pop_factor_risk[, .(calculated_mean_risk = mean(stroke_year_risk)), by = .(age_group, sex)]
  
  inc_table_long <- as.data.table(stroke_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence_per_100k")
  inc_table_long[, expected_risk := incidence_per_100k / 100000]
  
  comparison <- merge(mean_risk_by_group, inc_table_long, by.x = c("age_group", "sex"), by.y = c("age", "sex"), all.x = TRUE)
  
  print("Consistency Check:")
  print(comparison[, .(age_group, sex, calculated_mean_risk, expected_risk)])
}

