# Colorectal Cancer Risk Factors

# Risk Factors Included:
# - Diabetes type 2
# - Alcohol
# - Physical Activity
# - BMI
# - Smoking
# - Fruit
# - Veg
# - Red Meat
# - Processed Meat
# - NSAIDS
# - Postmenopausal Hormone Therapy
# - Family History
# - Inflammatory Bowel Disease

# Meta-analyses of Colorectal Cancer Risk Factors
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4161278/#T3

# BMI
# Female RR = exp (0.017*BMI)
# Male exp(0.032*(BMI-22))

# PA 
# RR = exp (−0.029*PA)
# MET Thresholds:
# 0.978 @ 600 METs
# 0.956 @ 1200 METs
# 0.933 @ 1800 METs
# 0.883 @ 2400 METs
# 0.833 @ 3000 METs
# 0.831 @ 3600 METs
# 0.829 @ 4200 METs

# Current Smokers
# 10 pack years: RR = 1.11 
# 10 years since quitting: 71.91%

# Alcohol
# exp(0.011 × drinks/wk)

# Veg
# exp(−0.030 * serv/d)

# Diabetes
# RR 1.527
# GBD
         

# PARF = 1 − (n/sum_indiduals(prod ( RRi1 ∗ RRi2 ∗ RRik))
# risk factors - k
# individuals - i

# ============================================================================
# COLORECTAL CANCER RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# CRC Incidence Data (per 100,000)
crc_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  '0 to 34',   1.0,  1.9, 
  '35 to 39',   9.1,  8.9, 
  '40 to 44',   18.1,  12.9, 
  '45 to 49',   30.9,  29.6, 
  '50 to 54',   47.4,  45.1, 
  '55 to 59',   92.8,  68.5, 
  '60 to 64',   181.1,  110.5, 
  '65 to 69',   213.2,  133.7, 
  '70 to 74',   294.9,  178.4, 
  '75 to 79',   345.5,  229.8, 
  '80 to 84',   501.7,  303.5, 
  '85 to 89',   526.3,  362.9, 
  '90 and over',   438.9,  332.4
)

# Risk Factor Relative Risks for CRC ----

# BMI - Relative Risk (sex-specific)
rr_crc_bmi <- tribble(
  ~bmi, ~sex, ~RR,
  "normal", "Males", 1.0,
  "normal", "Females", 1.0,
  "overweight", "Males", 1.177,
  "overweight", "Females", 1.059,
  "obese", "Males", 1.177^3,
  "obese", "Females", 1.059^3
)

# Physical Activity - Relative Risk
rr_crc_physical_activity <- tribble(
  ~pa, ~RR,
  "inactive", 1.0,
  "low_activity", 0.933,
  "some_activity", 0.883,
  "meets_rec", 0.831
)

# Alcohol - Relative Risk
rr_crc_alcohol <- tribble(
  ~alcohol, ~RR,
  "no_risk", 1.0,
  "lower_risk", 1.078,
  "increased_risk", 1.237,
  "higher_risk", 1.468
)

# Smoking - Relative Risk
rr_crc_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.11,
  "current_smoker", 1.527
)

# Diabetes - Relative Risk
rr_crc_diabetes <- tribble(
  ~diabetes_status, ~RR,
  "no_diabetes", 1.0,
  "undiagnosed_diabetes", 1.527,
  "diagnosed_diabetes", 1.527
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_crc_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(crc_incidence_per100k) %>%
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
  
  dt[inc_dt, on = .(age_group_inc = age, sex), crc_year_risk := i.incidence / 100000]
  dt[is.na(crc_year_risk), crc_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_crc_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("crc" %in% names(dt)) {
    dt <- dt[crc == 0]
  }
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_crc_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_crc_physical_activity)
  dt[rr_pa_dt, on = .(pa), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 3. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_crc_alcohol)
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 4. Smoking RR
  rr_smoking_dt <- as.data.table(rr_crc_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 5. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_crc_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_pa_indiv * RR_alcohol_indiv * 
                      RR_smoking_indiv * RR_diabetes_indiv]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                      "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(crc_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, crc_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, crc_prob_min)])
}

# Function 3: Apply Risk Factors
apply_crc_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR (sex-specific)
  rr_bmi_dt <- as.data.table(rr_crc_bmi)
  dt[rr_bmi_dt, on = .(bmi, sex), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_crc_physical_activity)
  dt[rr_pa_dt, on = .(pa), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 3. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_crc_alcohol)
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 4. Smoking RR
  rr_smoking_dt <- as.data.table(rr_crc_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 5. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_crc_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_pa_indiv * RR_alcohol_indiv * 
                      RR_smoking_indiv * RR_diabetes_indiv]
  
  # Assign age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
                                      "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
                                      "80 to 84", "85 to 89", "90 and over"),
                            right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_inc, sex), crc_prob_min := i.crc_prob_min]
  dt[is.na(crc_prob_min), crc_prob_min := 0]
  
  # Calculate individual risk
  dt[, colorectal_cancer_year_risk := crc_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "RR_pa_indiv", "RR_alcohol_indiv", 
         "RR_smoking_indiv", "RR_diabetes_indiv", "RR_combined", 
         "age_group_inc", "crc_prob_min") := NULL]
  
  return(dt)
} 





store_unit_tests <- function(){
  
  x <- past_populations %>% 
    filter(year ==min(year))
  
  count(x, wt = colorectal_cancer_year_risk)
  
  apply_crc_risk_engine_age_sex(x) %>% 
    count(wt = crc_year_risk)
  
  t = calculate_crc_theoretical_min(x)
  crc_theoretical_min_table <- calculate_crc_theoretical_min(current_population)
  y <- apply_crc_risk_factors(x,theoretical_min_table = t)
  
  y <- y %>% mutate(age1=cut(age, 
                             breaks = c(-Inf, 45, 55, 65, 75, 110),
                             labels = c("0-44", "45-54", "55-64", "65-74", "75-110"),
                             right = FALSE))
  
  y %>% 
    group_by(age1, sex) %>% 
    summarise(n = n(), wt = sum(colorectal_cancer_year_risk)) %>% 
    mutate(colorectal_cancer_year_risk_per100k = wt/n*100000)
  
  input_population = past_populations %>% 
    filter(year ==min(year))
  
}


