# ============================================================================
# ORIGINAL RAW DATA AND NOTES (COMMENTED OUT FOR REFERENCE)
# ============================================================================

# per 100000 population per year 
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4687424/
# Age‐specific incidence, risk factors and outcome of acute abdominal aortic aneurysms in a defined population
#
# aaa_incidence <- tribble(
#   ~age_aaa, ~Females, ~Males,
#   '0-44', 0, 0,
#   '45–54', 0, 0,
#   '55–64', 8, 3,
#   '65–74', 55, 11,
#   '75–84', 112, 31,
#   '85-110', 298, 82
# ) %>% 
#   pivot_longer(-1,names_to = 'sex', values_to ='per100k') %>% 
#   mutate(aaa_year_risk = per100k/100000) 
#
# Abdominal aortic aneurysm	2.41	All	Both	smokes
# SAPM

# per 10mmHg increase
# high_systolic_blood_pressure <- tribble(
#   ~age, ~RR,
# '25-29', 1.544,
# '30-34', 1.469,
# '35-39', 1.394,
# '40-44', 1.345,
# '45-49', 1.321,
# '50-54', 1.296,
# '55-59', 1.272, 
# '60-64', 1.248,
# '65-69', 1.223,
# '70-74', 1.2,
# '75-79', 1.177,
# '80-84', 1.119,
# '85-89', 1.119,
# '90-94', 1.119,
# '95+', 1.119)
#
# https://journals.lww.com/epidem/abstract/2001/01000/life_style_factors_and_risk_for_abdominal_aortic.16.aspx
# hypertension RR = 1.92
# cholesterol RR = 1.85
# smoking RR = 2.25
#
# https://pubmed.ncbi.nlm.nih.gov/25563744/
# black hr = 0.44
# body mass index ≥25 HR = 0.72
#
# https://www.nature.com/articles/s41598-020-76306-9
# PA RR = 0.70

# ============================================================================
# ABDOMINAL AORTIC ANEURYSM (AAA) RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# AAA Incidence Data (per 100,000)
# Source: https://pmc.ncbi.nlm.nih.gov/articles/PMC4687424/
# Age‐specific incidence, risk factors and outcome of acute abdominal aortic aneurysms in a defined population
aaa_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  '0-44', 0, 0,
  '45-54', 0, 0,
  '55-64', 8, 3,
  '65-74', 55, 11,
  '75-84', 112, 31,
  '85-110', 298, 82
)

# Risk Factor Relative Risks for AAA ----

# Smoking - Relative Risk
# Source: https://journals.lww.com/epidem/abstract/2001/01000/life_style_factors_and_risk_for_abdominal_aortic.16.aspx
rr_aaa_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.5,  # Assumed intermediate value
  "current_smoker", 2.25
)

# Hypertension - Relative Risk
# Source: https://journals.lww.com/epidem/abstract/2001/01000/life_style_factors_and_risk_for_abdominal_aortic.16.aspx
rr_aaa_hypertension <- tribble(
  ~hypertension_status, ~RR,
  "no_hypertension", 1.0,
  "undiagnosed_hypertension", 1.92,
  "diagnosed_hypertension", 1.92,
  "controlled_hypertension", 1.92
)

# Physical Activity - Relative Risk
# Source: https://www.nature.com/articles/s41598-020-76306-9
rr_aaa_physical_activity <- tribble(
  ~pa, ~RR,
  "inactive", 1.0,
  "low_activity", 0.85,  # Assumed intermediate values
  "some_activity", 0.77,
  "meets_rec", 0.70
)

# BMI - Relative Risk
# Source: https://pubmed.ncbi.nlm.nih.gov/25563744/
# Note: BMI ≥25 shows protective effect (HR = 0.72)
rr_aaa_bmi <- tribble(
  ~bmi, ~RR,
  "normal", 1.0,
  "overweight", 0.72,
  "obese", 0.72
)

# Cholesterol - Relative Risk
# Source: https://journals.lww.com/epidem/abstract/2001/01000/life_style_factors_and_risk_for_abdominal_aortic.16.aspx
rr_aaa_cholesterol <- tribble(
  ~cholesterol_status, ~RR,
  "normal", 1.0,
  "elevated", 1.85
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_aaa_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(aaa_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0-44', '45-54', '55-64', '65-74', '75-84', '85-110'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 55, 65, 75, 85, Inf),
                            labels = c("0-44", "45-54", "55-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), aaa_year_risk := i.incidence / 100000]
  dt[is.na(aaa_year_risk), aaa_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_aaa_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("aaa" %in% names(dt)) {
    dt <- dt[aaa == 0]
  }
  
  # 1. Smoking RR
  rr_smoking_dt <- as.data.table(rr_aaa_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 2. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_aaa_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 3. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_aaa_physical_activity)
  dt[rr_pa_dt, on = .(pa), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 4. BMI RR
  rr_bmi_dt <- as.data.table(rr_aaa_bmi)
  dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 5. Cholesterol RR (if available)
  if ("cholesterol_status" %in% names(dt)) {
    rr_cholesterol_dt <- as.data.table(rr_aaa_cholesterol)
    dt[rr_cholesterol_dt, on = .(cholesterol_status), RR_cholesterol_indiv := i.RR]
    dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
  } else {
    dt[, RR_cholesterol_indiv := 1]
  }
  
  # Combine all RRs
  dt[, RR_combined := RR_smoking_indiv * RR_hypertension_indiv * RR_pa_indiv * 
                      RR_bmi_indiv * RR_cholesterol_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 55, 65, 75, 85, Inf),
                            labels = c("0-44", "45-54", "55-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(aaa_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, aaa_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, aaa_prob_min)])
}

# Function 3: Apply Risk Factors
apply_aaa_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. Smoking RR
  rr_smoking_dt <- as.data.table(rr_aaa_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 2. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_aaa_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 3. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_aaa_physical_activity)
  dt[rr_pa_dt, on = .(pa), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 4. BMI RR
  rr_bmi_dt <- as.data.table(rr_aaa_bmi)
  dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 5. Cholesterol RR (if available)
  if ("cholesterol_status" %in% names(dt)) {
    rr_cholesterol_dt <- as.data.table(rr_aaa_cholesterol)
    dt[rr_cholesterol_dt, on = .(cholesterol_status), RR_cholesterol_indiv := i.RR]
    dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
  } else {
    dt[, RR_cholesterol_indiv := 1]
  }
  
  # Combine all RRs
  dt[, RR_combined := RR_smoking_indiv * RR_hypertension_indiv * RR_pa_indiv * 
                      RR_bmi_indiv * RR_cholesterol_indiv]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 55, 65, 75, 85, Inf),
                            labels = c("0-44", "45-54", "55-64", "65-74", "75-84", "85-110"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), aaa_prob_min := i.aaa_prob_min]
  dt[is.na(aaa_prob_min), aaa_prob_min := 0]
  
  # Calculate individual risk
  dt[, aaa_year_risk := aaa_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_smoking_indiv", "RR_hypertension_indiv", "RR_pa_indiv", 
         "RR_bmi_indiv", "RR_cholesterol_indiv", "RR_combined", 
         "age_group_inc", "aaa_prob_min") := NULL]
  
  return(dt)
}

# Unit Tests ----
store_unit_tests <- function(){
  # Example usage:
  # x <- past_populations %>% filter(year == min(year))
  # 
  # # Test baseline age-sex risk
  # apply_aaa_risk_engine_age_sex(x) %>% 
  #   count(wt = aaa_year_risk)
  # 
  # # Calculate theoretical minimum
  # aaa_theoretical_min_table <- calculate_aaa_theoretical_min(x)
  # 
  # # Apply risk factors
  # y <- apply_aaa_risk_factors(x, theoretical_min_table = aaa_theoretical_min_table)
  # 
  # # Summarize by age group and sex
  # y %>% 
  #   mutate(age_group = cut(age, 
  #                          breaks = c(-Inf, 45, 55, 65, 75, 85, Inf),
  #                          labels = c("0-44", "45-54", "55-64", "65-74", "75-84", "85-110"),
  #                          right = FALSE)) %>%
  #   group_by(age_group, sex) %>% 
  #   summarise(n = n(), wt = sum(aaa_year_risk)) %>% 
  #   mutate(aaa_year_risk_per100k = wt/n*100000)
}
