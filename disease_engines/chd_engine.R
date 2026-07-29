# https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002602

library(tibble)
library(dplyr)
library(data.table)

# ============================================================================
# CHD (CORONARY HEART DISEASE) RISK ENGINE
# ============================================================================

# Data Definitions ----

# CHD Incidence Data (per 100,000)
chd_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  '0-29', 0.0, 0.0,
  '30-54', 88.1, 21.2,
  '55-64', 317.0, 90.3,  
  '65-74', 533.0, 237.0,  
  '75-84', 1017.0, 597.0,  
  '85-110', 1987.0, 1395.0
)

# Risk Factor Relative Risks for CHD ----

# BMI - Relative Risk by Age Group
rr_chd_bmi <- tribble(
  ~age, ~RR,
  "0-19", 1.0,
  "20-24", 2.274,
  "25-29", 2.274,
  "30-34", 2.018,
  "35-39", 1.724,
  "40-44", 1.599,
  "45-49", 1.567,
  "50-54", 1.52,
  "55-59", 1.466,
  "60-64", 1.414,
  "65-69", 1.364,
  "70-74", 1.319,
  "75-79", 1.274,
  "80-84", 1.17,
  "85-89", 1.17,
  "90-94", 1.17,
  "95-110", 1.17
)

# Smoking - Relative Risk
rr_chd_smoking <- tibble::tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.29,
  "current_smoker", 2.14
)

# Alcohol - Relative Risk
rr_chd_alcohol <- tibble::tribble(
  ~alcohol, ~RR,
  "no_risk", 1.0,          # Never
  "lower_risk", 0.73,      # Current Low
  "increased_risk", 0.80,  # Current Moderate
  "higher_risk", 0.71      # Current High
)

# Physical Activity - Relative Risk
rr_chd_physical_activity <- tibble::tribble(
  ~activity, ~RR,
  "meets_rec", 1.0,        # High Active
  "some_activity", 1.17,   # Moderate Active
  "low_activity", 1.24,    # Not Active
  "inactive", 1.24
)

# Hypertension - Relative Risk
rr_chd_hypertension <- tibble::tribble(
  ~hypertension, ~RR,
  'normotensive_untreated', 1.0,
  'hypertensive_controlled', 1.0,
  'hypertensive_uncontrolled', 1.62,
  'hypertensive_untreated', 1.62
)

# Diabetes - Relative Risk
rr_chd_diabetes <- tibble::tribble(
  ~diabetes, ~RR,
  "no_diabetes", 1.0,
  "undiagnosed_diabetes", 1.98,
  "diagnosed_diabetes", 1.98
)

# Cholesterol - Relative Risk (Non-HDL)
rr_chd_cholesterol <- tibble::tribble(
  ~cholesterol_level, ~RR,
  "normal_cholesterol", 1.0,      # ≤3.2
  "raised_cholesterol", 1.26,     # 3.2-4.0
  "hdl/cholesterol", 1.68         # >4.0
)

# Depression - Relative Risk
# rr_chd_depression <- tibble::tribble(
#   ~depression, ~RR,
#   "No", 1.0,
#   "Yes", 1.17
# )

# PM2.5 - Relative Risk per 10 μg/m³
rr_chd_pm25 <- 1.41

# Convert incidence to long format for easy merging
inc_dt <- as.data.table(chd_incidence_per100k) %>%
  melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_chd_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Age groups: '0-29', '30-54', '55-64', '65-74', '75-84', '85-110'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 30, 55, 65, 75, 85, 110),
                            labels = c("0-29", "30-54", "55-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
  
  inc_dt_temp <- as.data.table(chd_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  dt[inc_dt_temp, on = .(age_group_inc = age, sex), chd_year_risk := i.incidence / 100000]
  dt[is.na(chd_year_risk), chd_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_chd_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("chd" %in% names(dt)) {
    dt <- dt[chd == 0]
  }
  
  # 1. BMI RR - Map Age to BMI RR groups
  breaks_bmi <- c(0, 20, seq(25, 95, by = 5), 111)
  labels_bmi <- c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                  "75-79", "80-84", "85-89", "90-94", "95-110")
                  
  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
  
  rr_bmi_dt <- as.data.table(rr_chd_bmi)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
  
  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 28,
    bmi == "obese", 32,
    default = 20
  )]
  
  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. PM2.5 RR
  dt[, RR_pm25_indiv := rr_chd_pm25^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  # 3. Smoking RR
  rr_smoking_dt <- as.data.table(rr_chd_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 4. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_chd_alcohol)
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 5. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_chd_physical_activity)
  dt[rr_pa_dt, on = .(pa = activity), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 6. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_chd_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status = hypertension), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 7. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_chd_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status = diabetes), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # 8. Cholesterol RR
  rr_cholesterol_dt <- as.data.table(rr_chd_cholesterol)
  dt[rr_cholesterol_dt, on = .(cholesterol_status = cholesterol_level), RR_cholesterol_indiv := i.RR]
  dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * 
                      RR_alcohol_indiv * RR_pa_indiv * RR_hypertension_indiv * 
                      RR_diabetes_indiv * RR_cholesterol_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 30, 55, 65, 75, 85, 110),
                            labels = c("0-29", "30-54", "55-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
                            
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(chd_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
    
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, chd_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, chd_prob_min)])
}

# Function 3: Apply Risk Factors
apply_chd_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR
  breaks_bmi <- c(0, 20, seq(25, 95, by = 5), 111)
  labels_bmi <- c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                  "75-79", "80-84", "85-89", "90-94", "95-110")
  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
  
  rr_bmi_dt <- as.data.table(rr_chd_bmi)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
  
  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 30,
    bmi == "obese", 37,
    default = 20
  )]
  
  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. PM2.5 RR
  dt[, RR_pm25_indiv := rr_chd_pm25^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  # 3. Smoking RR
  rr_smoking_dt <- as.data.table(rr_chd_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 4. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_chd_alcohol)
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 5. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_chd_physical_activity)
  dt[rr_pa_dt, on = .(pa = activity), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 6. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_chd_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status = hypertension), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 7. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_chd_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status = diabetes), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # 8. Cholesterol RR
  rr_cholesterol_dt <- as.data.table(rr_chd_cholesterol)
  dt[rr_cholesterol_dt, on = .(cholesterol_status = cholesterol_level), RR_cholesterol_indiv := i.RR]
  dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
  
  # 9. Depression RR
  # rr_depression_dt <- as.data.table(rr_chd_depression)
  # dt[rr_depression_dt, on = .(depression), RR_depression_indiv := i.RR]
  # dt[is.na(RR_depression_indiv), RR_depression_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * 
                      RR_alcohol_indiv * RR_pa_indiv * RR_hypertension_indiv * 
                      RR_diabetes_indiv * RR_cholesterol_indiv
     # * RR_depression_indiv
     ]
  
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 30, 55, 65, 75, 85, 110),
                            labels = c("0-29", "30-54", "55-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
                            
  dt[min_dt, on = .(age_group_inc, sex), chd_prob_min := i.chd_prob_min]
  dt[is.na(chd_prob_min), chd_prob_min := 0]
  
  dt[, chd_year_risk := chd_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
         "RR_pm25_indiv", "RR_smoking_indiv", "RR_alcohol_indiv", "RR_pa_indiv",
         "RR_hypertension_indiv", "RR_diabetes_indiv", "RR_cholesterol_indiv", 
         "RR_combined", "age_group_inc", "chd_prob_min") := NULL]
         
  return(dt)
}
