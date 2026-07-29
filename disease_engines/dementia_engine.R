# filepath: /Users/aarongorman/Documents/SIB/PHM/PHModel/disease_engines/dementia_engine.R
library(tibble)
library(dplyr)
library(data.table)

# ============================================================================
# DEMENTIA RISK ENGINE
# ============================================================================
# Literature-based risk factors from Lancet Commission on Dementia Prevention:
# Less education (RR 1.6), Hearing loss (RR 1.9), TBI (RR 1.8) - not modeled
# Hypertension (RR 1.6), Alcohol >21 units/week (RR 1.2), Obesity BMI≥30 (RR 1.6)
# Smoking (RR 1.6), Depression (RR 1.9), Social isolation (RR 1.6)
# Physical inactivity (RR 1.4), Diabetes (RR 1.5), Air pollution (RR 1.1)
# ============================================================================
# https://www.alzint.org/u/World-Alzheimer-Report-2023.pdf

# Dementia Incidence per 100,000 population
dementia_incidence_per100k <- tribble(
    ~age, ~Males, ~Females,
    '0-59', 0, 0,
    '60-64', 488.06, 515.88, 
    '65-69', 304.79, 492.8, 
    '70-74', 1278.18, 1031.38,
    '75-79', 1422.62, 2875.31, 
    '80-84', 4042.55, 2985.94, 
    '85-89', 3363.16, 8573.13,
    '90-110', 3363.16, 8573.13
)

# BMI - Relative Risk (age-stratified, per 5 kg/m² increase from reference BMI of 20)
rr_dementia_bmi <- tribble(
    ~age, ~RR,
    "0-24", 1.0,
    "25-29", 1.15,
    "30-34", 1.15,
    "35-39", 1.32,
    "40-44", 1.32,
    "45-49", 1.32,
    "50-54", 1.32,
    "55-59", 1.32,
    "60-64", 1.32,
    "65-69", 1.32,
    "70-74", 1.32,
    "75-79", 1.32,
    "80-84", 1.32,
    "85-89", 1.32,
    "90-94", 1.32,
    "95-110", 1.32
)

# PM2.5 Air Pollution - Relative Risk (per 10 μg/m³ increase)
rr_dementia_pm25 <- 1.10

#Dementia prevention, intervention, and care: 2020 report of the Lancet Commission
# https://www.thelancet.com/action/showFullTableHTML?isHtml=true&tableId=tbl1&pii=S0140-6736%2820%2930367-6

# Smoking - Relative Risk
rr_dementia_smoking <- tibble::tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.3,
  "current_smoker", 1.6
)

# Alcohol - Relative Risk (>21 units/week vs. moderate)
rr_dementia_alcohol <- tibble::tribble(
  ~alcohol, ~RR,
  "never", 1.0,
  "within_guidelines", 1.0,
  "above_guidelines", 1.2
)

# Physical Activity - Relative Risk
rr_dementia_physical_activity <- tibble::tribble(
  ~pa, ~RR,
  "meets_rec", 1.0,
  "some", 1.2,
  "low", 1.4,
  "inactive", 1.4
)

# Hypertension - Relative Risk
rr_dementia_hypertension <- tibble::tribble(
  ~hypertension_status, ~RR,
  "controlled", 1.0,
  "uncontrolled", 1.6
)

# Diabetes - Relative Risk
rr_dementia_diabetes <- tibble::tribble(
  ~diabetes_status, ~RR,
  "no", 1.0,
  "yes", 1.5
)

# Hearing Loss - Relative Risk (not currently modeled)
# rr_dementia_hearing_loss <- tibble::tribble(
#   ~hearing_loss, ~RR,
#   "No", 1.0,
#   "Yes", 1.9
# )

# Traumatic Brain Injury - Relative Risk (not currently modeled)
# rr_dementia_tbi <- tibble::tribble(
#   ~tbi, ~RR,
#   "No", 1.0,
#   "Yes", 1.8
# )

# Depression - Relative Risk (from literature, but not currently modeled)
# rr_dementia_depression <- tibble::tribble(
#   ~depression, ~RR,
#   "No", 1.0,
#   "Yes", 1.9
# )

# Social Isolation - Relative Risk (not currently modeled)
# rr_dementia_social_isolation <- tibble::tribble(
#   ~social_isolation, ~RR,
#   "No", 1.0,
#   "Yes", 1.6
# )

# ============================================================================
# FUNCTION 1: Apply dementia risk based on age and sex alone
# ============================================================================
apply_dementia_risk_engine_age_sex <- function(input_population) {
    dt <- as.data.table(input_population)
    
    inc_dt <- as.data.table(dementia_incidence_per100k) %>%
        melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
    
    # Age groups: 0-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85-89, 90-110
    dt[, age_group_inc := cut(age, 
                              breaks = c(0, 60, 65, 70, 75, 80, 85, 90, 111),
                              labels = c("0-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                              right = FALSE)]
    
    dt[inc_dt, on = .(age_group_inc = age, sex), dementia_year_risk := i.incidence / 100000]
    dt[is.na(dementia_year_risk), dementia_year_risk := 0]
    
    dt[, age_group_inc := NULL]
    
    return(dt)
}

# ============================================================================
# FUNCTION 2: Calculate PAF and theoretical minimum dementia risk
# ============================================================================
calculate_dementia_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Exclude prevalent cases
    if ("dementia" %in% names(dt)) {
        dt <- dt[dementia == 0]
    }
    
    # 1. BMI RR (age-stratified, per 5 kg/m² from reference of 20)
    dt[, age_group_bmi := cut(age, 
                              breaks = c(0, 25, seq(30, 95, by = 5), 111),
                              labels = c("0-24", "25-29", "30-34", "35-39", "40-44", 
                                        "45-49", "50-54", "55-59", "60-64", "65-69", 
                                        "70-74", "75-79", "80-84", "85-89", "90-94", "95-110"),
                              right = FALSE)]
    
    rr_bmi_dt <- as.data.table(rr_dementia_bmi)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
    dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
    
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 30,
        bmi == "obese", 37,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. PM2.5 RR (per 10 μg/m³)
    dt[, RR_pm25_indiv := rr_dementia_pm25^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    # 3. Smoking RR
    rr_smoking_dt <- as.data.table(rr_dementia_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 4. Alcohol RR
    rr_alcohol_dt <- as.data.table(rr_dementia_alcohol)
    dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
    dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
    
    # 5. Physical Activity RR
    rr_pa_dt <- as.data.table(rr_dementia_physical_activity)
    dt[rr_pa_dt, on = .(pa), RR_pa_indiv := i.RR]
    dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
    
    # 6. Hypertension RR
    rr_hypertension_dt <- as.data.table(rr_dementia_hypertension)
    dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
    dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    
    # 7. Diabetes RR
    rr_diabetes_dt <- as.data.table(rr_dementia_diabetes)
    dt[rr_diabetes_dt, on = .(diabetes_status), RR_diabetes_indiv := i.RR]
    dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
    
    # 8. Hearing Loss RR (commented out)
    # rr_hearing_loss_dt <- as.data.table(rr_dementia_hearing_loss)
    # dt[rr_hearing_loss_dt, on = .(hearing_loss), RR_hearing_loss_indiv := i.RR]
    # dt[is.na(RR_hearing_loss_indiv), RR_hearing_loss_indiv := 1]
    
    # 9. Traumatic Brain Injury RR (commented out)
    # rr_tbi_dt <- as.data.table(rr_dementia_tbi)
    # dt[rr_tbi_dt, on = .(tbi), RR_tbi_indiv := i.RR]
    # dt[is.na(RR_tbi_indiv), RR_tbi_indiv := 1]
    
    # 10. Depression RR (commented out)
    # rr_depression_dt <- as.data.table(rr_dementia_depression)
    # dt[rr_depression_dt, on = .(depression), RR_depression_indiv := i.RR]
    # dt[is.na(RR_depression_indiv), RR_depression_indiv := 1]
    
    # Combined RR for each individual
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * 
                        RR_alcohol_indiv * RR_pa_indiv * RR_hypertension_indiv * 
                        RR_diabetes_indiv] # * RR_hearing_loss_indiv * RR_tbi_indiv * RR_depression_indiv
    
    # Calculate PAF by age-sex group
    dt[, age_group_inc := cut(age, 
                              breaks = c(0, 60, 65, 70, 75, 80, 85, 90, 111),
                              labels = c("0-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                              right = FALSE)]
    
    paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
    
    # Merge incidence data with PAF
    inc_dt <- as.data.table(dementia_incidence_per100k) %>%
        melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
    
    min_dt <- merge(inc_dt, paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
    
    # Calculate theoretical minimum risk
    min_dt[, dementia_prob_min := (incidence / 100000) * (1 - AF)]
    
    return(min_dt[, .(age_group_inc = age, sex, dementia_prob_min)])
}

# ============================================================================
# FUNCTION 3: Apply individual risk factors to calculate personalized dementia risk
# ============================================================================
apply_dementia_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    # 1. BMI RR (age-stratified, per 5 kg/m² from reference of 20)
    dt[, age_group_bmi := cut(age, 
                              breaks = c(0, 25, seq(30, 95, by = 5), 111),
                              labels = c("0-24", "25-29", "30-34", "35-39", "40-44", 
                                        "45-49", "50-54", "55-59", "60-64", "65-69", 
                                        "70-74", "75-79", "80-84", "85-89", "90-94", "95-110"),
                              right = FALSE)]
    
    rr_bmi_dt <- as.data.table(rr_dementia_bmi)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
    dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
    
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 30,
        bmi == "obese", 37,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. PM2.5 RR (per 10 μg/m³)
    dt[, RR_pm25_indiv := rr_dementia_pm25^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    # 3. Smoking RR
    rr_smoking_dt <- as.data.table(rr_dementia_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 4. Alcohol RR
    rr_alcohol_dt <- as.data.table(rr_dementia_alcohol)
    dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
    dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
    
    # 5. Physical Activity RR
    rr_pa_dt <- as.data.table(rr_dementia_physical_activity)
    dt[rr_pa_dt, on = .(pa), RR_pa_indiv := i.RR]
    dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
    
    # 6. Hypertension RR
    rr_hypertension_dt <- as.data.table(rr_dementia_hypertension)
    dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
    dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    
    # 7. Diabetes RR
    rr_diabetes_dt <- as.data.table(rr_dementia_diabetes)
    dt[rr_diabetes_dt, on = .(diabetes_status), RR_diabetes_indiv := i.RR]
    dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
    
    # 8. Hearing Loss RR (commented out)
    # rr_hearing_loss_dt <- as.data.table(rr_dementia_hearing_loss)
    # dt[rr_hearing_loss_dt, on = .(hearing_loss), RR_hearing_loss_indiv := i.RR]
    # dt[is.na(RR_hearing_loss_indiv), RR_hearing_loss_indiv := 1]
    
    # 9. Traumatic Brain Injury RR (commented out)
    # rr_tbi_dt <- as.data.table(rr_dementia_tbi)
    # dt[rr_tbi_dt, on = .(tbi), RR_tbi_indiv := i.RR]
    # dt[is.na(RR_tbi_indiv), RR_tbi_indiv := 1]
    
    # 10. Depression RR (commented out)
    # rr_depression_dt <- as.data.table(rr_dementia_depression)
    # dt[rr_depression_dt, on = .(depression), RR_depression_indiv := i.RR]
    # dt[is.na(RR_depression_indiv), RR_depression_indiv := 1]
    
    # Combined RR for each individual
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * 
                        RR_alcohol_indiv * RR_pa_indiv * RR_hypertension_indiv * 
                        RR_diabetes_indiv] # * RR_hearing_loss_indiv * RR_tbi_indiv * RR_depression_indiv
    
    # Apply theoretical minimum
    dt[, age_group_inc := cut(age, 
                              breaks = c(0, 60, 65, 70, 75, 80, 85, 90, 111),
                              labels = c("0-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                              right = FALSE)]
    
    dt[min_dt, on = .(age_group_inc, sex), dementia_prob_min := i.dementia_prob_min]
    dt[is.na(dementia_prob_min), dementia_prob_min := 0]
    
    # Final individualized risk
    dt[, dementia_year_risk := dementia_prob_min * RR_combined]
    
    # Cleanup temporary columns
    dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
           "RR_pm25_indiv", "RR_smoking_indiv", "RR_alcohol_indiv", 
           "RR_pa_indiv", "RR_hypertension_indiv", "RR_diabetes_indiv",
           "RR_combined", "age_group_inc", "dementia_prob_min") := NULL]
    
    return(dt)
}

# ============================================================================
# TEST CODE: Verify PAF calculation recovers incidence
# ============================================================================
# library(data.table)
# 
# # Create test population
# test_pop <- data.table(
#     age = sample(60:90, 10000, replace = TRUE),
#     sex = sample(c("Males", "Females"), 10000, replace = TRUE),
#     bmi = sample(c("normal", "overweight", "obese"), 10000, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
#     pm25g = rnorm(10000, mean = 10, sd = 3),
#     smoking = sample(c("never", "former", "current"), 10000, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
#     alcohol = sample(c("never", "within_guidelines", "above_guidelines"), 10000, replace = TRUE, prob = c(0.2, 0.7, 0.1)),
#     pa = sample(c("meets_rec", "some", "low", "inactive"), 10000, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2)),
#     hypertension_status = sample(c("controlled", "uncontrolled"), 10000, replace = TRUE, prob = c(0.7, 0.3)),
#     diabetes_status = sample(c("no", "yes"), 10000, replace = TRUE, prob = c(0.9, 0.1))
# )
# 
# # Calculate theoretical minimum
# min_table <- calculate_dementia_theoretical_min(test_pop)
# 
# # Apply risk factors
# test_pop <- apply_dementia_risk_factors(test_pop, min_table)
# 
# # Verify: mean risk by age/sex group should approximate incidence
# test_pop[, age_group := cut(age, 
#                             breaks = c(0, 60, 65, 70, 75, 80, 85, 90, 111),
#                             labels = c("0-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
#                             right = FALSE)]
# 
# result <- test_pop[, .(dementia_year_risk_per100k = mean(dementia_year_risk, na.rm = TRUE) * 100000), 
#                    by = .(age_group, sex)]
# print(result[order(sex, age_group)])