# ============================================================================
# ORIGINAL RAW DATA AND NOTES (COMMENTED OUT FOR REFERENCE)
# ============================================================================

# Source: https://pmc.ncbi.nlm.nih.gov/articles/PMC2779855/
# Predicting risk of osteoporotic fracture in men and women in England and Wales:
# Prospective derivation and validation of QFractureScores
#
# Fracture Incidence Data (per 1,000 person-years)
# Source: QFracture Study - England and Wales cohort
# osteoporotic fracture - distal radius, hip, or vertebral
#
# fracture_incidence_per1000 <- tribble(
#     ~age, ~sex, ~osteoporotic, ~hip, 
#     # Women
#     "30-34", "Females", 0.50, 0.02,
#     "35-39", "Females", 0.62, 0.04, 
#     "40-44", "Females", 0.88, 0.09, 
#     "45-49", "Females", 1.32, 0.16, 
#     "50-54", "Females", 1.97, 0.25, 
#     "55-59", "Females", 2.70, 0.44, 
#     "60-64", "Females", 3.99, 0.92, 
#     "65-69", "Females", 5.72, 1.92, 
#     "70-74", "Females", 8.05, 3.55, 
#     "75-85", "Females", 12.11, 7.19,
#     "85-110", "Females", 12.11, 7.19, #extended to older ages
#     # Men
#     "30-34", "Males", 0.54, 0.04, 
#     "35-39", "Males", 0.57, 0.05, 
#     "40-44", "Males", 0.57, 0.08, 
#     "45-49", "Males", 0.61, 0.11, 
#     "50-54", "Males", 0.72, 0.15, 
#     "55-59", "Males", 0.87, 0.26, 
#     "60-64", "Males", 1.06, 0.40, 
#     "65-69", "Males", 1.49, 0.75, 
#     "70-74", "Males", 2.54, 1.48, 
#     "75-85", "Males", 4.35, 3.13, 
#     "85-110", "Males", 4.35, 3.13,  # Extend to older ages
# )
#
# Risk Factors from QFracture Study:
#
# BMI - osteoporotic fractures:
# normal 1.106
# overweight 1
# obese 0.90
#
# BMI - hip fractures:
# normal 1.43
# overweight 1
# obese 0.69
#
# Alcohol - hip fractures:
# no drinks 1
# 1 drinks 0.88
# 1-2 drinks 1 
# 3+ 1.39
#
# Alcohol - osteoporotic fractures:
# drink 1
#
# Smoking - osteoporotic fracture:
# current smoking 1.13
#
# Smoking - hip fracture:
# current smoking 1.6

# ============================================================================
# FRACTURE RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Fracture Incidence Data (per 1,000 person-years)
# Source: QFracture Study - England and Wales cohort

# osteoporotic fracture - distal radius, hip, or vertebral

fracture_incidence_per1000 <- tribble(
    ~age, ~sex, ~osteoporotic, ~hip, 
    # Women
    "30-34", "Females", 0.50, 0.02,
    "35-39", "Females", 0.62, 0.04, 
    "40-44", "Females", 0.88, 0.09, 
    "45-49", "Females", 1.32, 0.16, 
    "50-54", "Females", 1.97, 0.25, 
    "55-59", "Females", 2.70, 0.44, 
    "60-64", "Females", 3.99, 0.92, 
    "65-69", "Females", 5.72, 1.92, 
    "70-74", "Females", 8.05, 3.55, 
    "75-85", "Females", 12.11, 7.19,
    "85-110", "Females", 12.11, 7.19, # extended to older ages

    # Men
    "30-34", "Males", 0.54, 0.04, 
    "35-39", "Males", 0.57, 0.05, 
    "40-44", "Males", 0.57, 0.08, 
    "45-49", "Males", 0.61, 0.11, 
    "50-54", "Males", 0.72, 0.15, 
    "55-59", "Males", 0.87, 0.26, 
    "60-64", "Males", 1.06, 0.40, 
    "65-69", "Males", 1.49, 0.75, 
    "70-74", "Males", 2.54, 1.48, 
    "75-85", "Males", 4.35, 3.13, 
    "85-110", "Males", 4.35, 3.13  # extended to older ages
)

# Risk Factor Relative Risks for Osteoporotic Fractures ----
# Source: QFracture Study

# BMI - Osteoporotic Fracture Risk
rr_osteo_bmi <- tribble(
    ~bmi, ~RR,
    "normal", 1.106,     # <25 kg/m²
    "overweight", 1.0,   # 25-30 kg/m² (Reference)
    "obese", 0.90        # >30 kg/m² (Protective)
)

# Smoking - Osteoporotic Fracture Risk
rr_osteo_smoking <- tribble(
    ~smoking, ~RR,
    "never_smoked", 1.0,
    "former", 1.0,
    "current_smoker", 1.13
)

# Alcohol - Osteoporotic Fracture Risk
# Note: Original data shows no significant effect for osteoporotic fractures
rr_osteo_alcohol <- tribble(
    ~alcohol, ~RR,
    "no_risk", 1.0,
    "lower_risk", 1.0,
    "increased_risk", 1.0,
    "higher_risk", 1.0
)

# Risk Factor Relative Risks for Hip Fractures ----

# BMI - Hip Fracture Risk
rr_hip_bmi <- tribble(
    ~bmi, ~RR,
    "normal", 1.43,      # <25 kg/m² (Higher risk - low bone mass)
    "overweight", 1.0,   # 25-30 kg/m² (Reference)
    "obese", 0.69        # >30 kg/m² (Protective - padding/higher bone density)
)

# Smoking - Hip Fracture Risk
rr_hip_smoking <- tribble(
    ~smoking, ~RR,
    "never_smoked", 1.0,
    "former", 1.0,
    "current_smoker", 1.6
)

# Alcohol - Hip Fracture Risk
rr_hip_alcohol <- tribble(
    ~alcohol_detail, ~RR,
    "none", 1.0,           # No drinks
    "light", 0.88,         # 1 drink
    "moderate", 1.0,       # 1-2 drinks
    "heavy", 1.39          # 3+ drinks
)

# OSTEOPOROTIC FRACTURE Functions ----

# Function 1: Apply risk based on age and sex alone (Osteoporotic)
apply_osteoporotic_fracture_risk_engine_age_sex <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Convert incidence to long format
    inc_dt <- as.data.table(fracture_incidence_per1000)
    
    # Fracture incidence age groups: '30-34', '35-39', ..., '85-110'
    dt[, age_group_frac := cut(age, 
                              breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
                              labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                       "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
                              right = FALSE)]
    
    # Join with incidence data for osteoporotic fractures
    dt[inc_dt, on = .(age_group_frac = age, sex), 
       osteoporotic_fracture_year_risk := i.osteoporotic / 1000]
    
    # For ages <30, use 30-34 rate (lowest available)
    dt[age_group_frac == "<30" & sex == "Males", osteoporotic_fracture_year_risk := 0.54 / 1000]
    dt[age_group_frac == "<30" & sex == "Females", osteoporotic_fracture_year_risk := 0.50 / 1000]
    
    dt[is.na(osteoporotic_fracture_year_risk), osteoporotic_fracture_year_risk := 0]
    dt[, age_group_frac := NULL]
    
    return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum (Osteoporotic)
calculate_osteoporotic_fracture_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Exclude prevalent cases
    if ("osteoporotic_fracture" %in% names(dt)) {
        dt <- dt[osteoporotic_fracture == 0]
    }
    
    # 1. BMI RR
    rr_bmi_dt <- as.data.table(rr_osteo_bmi)
    dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Smoking RR
    rr_smoking_dt <- as.data.table(rr_osteo_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 3. Alcohol RR (no effect for osteoporotic)
    dt[, RR_alcohol_indiv := 1]
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_alcohol_indiv]
    
    # PAF grouping
    dt[, age_group_frac := cut(age, 
                              breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
                              labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                       "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
                              right = FALSE)]
    
    paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_frac, sex)]
    
    inc_dt <- as.data.table(fracture_incidence_per1000)
    inc_dt_osteo <- inc_dt[, .(age, sex, incidence = osteoporotic)]
    
    min_dt <- merge(inc_dt_osteo, paf_dt, 
                    by.x = c("age", "sex"), by.y = c("age_group_frac", "sex"))
    
    min_dt[, osteoporotic_fracture_prob_min := (incidence / 1000) * (1 - AF)]
    
    return(min_dt[, .(age_group_frac = age, sex, osteoporotic_fracture_prob_min)])
}

# Function 3: Apply Risk Factors (Osteoporotic)
apply_osteoporotic_fracture_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    # 1. BMI RR
    rr_bmi_dt <- as.data.table(rr_osteo_bmi)
    dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Smoking RR
    rr_smoking_dt <- as.data.table(rr_osteo_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 3. Alcohol RR (no effect)
    dt[, RR_alcohol_indiv := 1]
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_alcohol_indiv]
    
    # Assign age groups
    dt[, age_group_frac := cut(age, 
                              breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
                              labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                       "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
                              right = FALSE)]
    
    # Join with theoretical minimum
    dt[min_dt, on = .(age_group_frac, sex), 
       osteoporotic_fracture_prob_min := i.osteoporotic_fracture_prob_min]
    dt[is.na(osteoporotic_fracture_prob_min), osteoporotic_fracture_prob_min := 0]
    
    # Calculate individual risk
    dt[, fracture4_year_risk := osteoporotic_fracture_prob_min * RR_combined]
    
    # Clean up temporary columns
    dt[, c("RR_bmi_indiv", "RR_smoking_indiv", "RR_alcohol_indiv", 
           "RR_combined", "age_group_frac", "osteoporotic_fracture_prob_min") := NULL]
    
    return(dt)
}

# HIP FRACTURE Functions ----

# Function 1: Apply risk based on age and sex alone (Hip)
apply_hip_fracture_risk_engine_age_sex <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Convert incidence to long format
    inc_dt <- as.data.table(fracture_incidence_per1000)
    
    # Fracture incidence age groups: '30-34', '35-39', ..., '85-110'
    dt[, age_group_frac := cut(age, 
                              breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
                              labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                       "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
                              right = FALSE)]
    
    # Join with incidence data for hip fractures
    dt[inc_dt, on = .(age_group_frac = age, sex), 
       hip_fracture_year_risk := i.hip / 1000]
    
    # For ages <30, use 30-34 rate (lowest available)
    dt[age_group_frac == "<30" & sex == "Males", hip_fracture_year_risk := 0.04 / 1000]
    dt[age_group_frac == "<30" & sex == "Females", hip_fracture_year_risk := 0.02 / 1000]
    
    dt[is.na(hip_fracture_year_risk), hip_fracture_year_risk := 0]
    dt[, age_group_frac := NULL]
    
    return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum (Hip)
calculate_hip_fracture_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Exclude prevalent cases
    if ("hip_fracture" %in% names(dt)) {
        dt <- dt[hip_fracture == 0]
    }
    
    # 1. BMI RR
    rr_bmi_dt <- as.data.table(rr_hip_bmi)
    dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Smoking RR
    rr_smoking_dt <- as.data.table(rr_hip_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 3. Alcohol RR (if available)
    if ("alcohol_detail" %in% names(dt)) {
        rr_alcohol_dt <- as.data.table(rr_hip_alcohol)
        dt[rr_alcohol_dt, on = .(alcohol_detail), RR_alcohol_indiv := i.RR]
        dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
    } else if ("alcohol" %in% names(dt)) {
        # Map general alcohol categories to detail
        dt[, RR_alcohol_indiv := fcase(
            alcohol == "no_risk", 1.0,
            alcohol == "lower_risk", 0.88,
            alcohol == "increased_risk", 1.0,
            alcohol == "higher_risk", 1.39,
            default = 1.0
        )]
    } else {
        dt[, RR_alcohol_indiv := 1]
    }
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_alcohol_indiv]
    
    # PAF grouping
    dt[, age_group_frac := cut(age, 
                              breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
                              labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                       "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
                              right = FALSE)]
    
    paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_frac, sex)]
    
    inc_dt <- as.data.table(fracture_incidence_per1000)
    inc_dt_hip <- inc_dt[, .(age, sex, incidence = hip)]
    
    min_dt <- merge(inc_dt_hip, paf_dt, 
                    by.x = c("age", "sex"), by.y = c("age_group_frac", "sex"))
    
    min_dt[, hip_fracture_prob_min := (incidence / 1000) * (1 - AF)]
    
    return(min_dt[, .(age_group_frac = age, sex, hip_fracture_prob_min)])
}



# Function 3: Apply Risk Factors (Hip)
apply_hip_fracture_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    # 1. BMI RR
    rr_bmi_dt <- as.data.table(rr_hip_bmi)
    dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Smoking RR
    rr_smoking_dt <- as.data.table(rr_hip_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 3. Alcohol RR
    if ("alcohol_detail" %in% names(dt)) {
        rr_alcohol_dt <- as.data.table(rr_hip_alcohol)
        dt[rr_alcohol_dt, on = .(alcohol_detail), RR_alcohol_indiv := i.RR]
        dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
    } else if ("alcohol" %in% names(dt)) {
        dt[, RR_alcohol_indiv := fcase(
            alcohol == "no_risk", 1.0,
            alcohol == "lower_risk", 0.88,
            alcohol == "increased_risk", 1.0,
            alcohol == "higher_risk", 1.39,
            default = 1.0
        )]
    } else {
        dt[, RR_alcohol_indiv := 1]
    }
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_alcohol_indiv]
    
    # Assign age groups
    dt[, age_group_frac := cut(age, 
                              breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
                              labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                       "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
                              right = FALSE)]
    
    # Join with theoretical minimum
    dt[min_dt, on = .(age_group_frac, sex), 
       hip_fracture_prob_min := i.hip_fracture_prob_min]
    dt[is.na(hip_fracture_prob_min), hip_fracture_prob_min := 0]
    
    # Calculate individual risk
    dt[, nof_year_risk := hip_fracture_prob_min * RR_combined]
    
    # Clean up temporary columns
    dt[, c("RR_bmi_indiv", "RR_smoking_indiv", "RR_alcohol_indiv", 
           "RR_combined", "age_group_frac", "hip_fracture_prob_min") := NULL]
    
    return(dt)
}

# Unit Tests and Examples ----
store_unit_tests <- function() {
    
    # Example usage with historical population
    # x <- past_populations %>% 
    #     filter(year == min(year))
    
    # Test osteoporotic fractures
    # y_osteo_agesex <- apply_osteoporotic_fracture_risk_engine_age_sex(x)
    # osteo_theoretical_min <- calculate_osteoporotic_fracture_theoretical_min(x)
    # y_osteo <- apply_osteoporotic_fracture_risk_factors(x, osteo_theoretical_min)
    
    # Test hip fractures
    # y_hip_agesex <- apply_hip_fracture_risk_engine_age_sex(x)
    # hip_theoretical_min <- calculate_hip_fracture_theoretical_min(x)
    # y_hip <- apply_hip_fracture_risk_factors(x, hip_theoretical_min)
    
    # Check aggregated risk by age group for osteoporotic fractures
    # y_osteo %>% 
    #     mutate(age1 = cut(age, 
    #                      breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
    #                      labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
    #                                "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
    #                      right = FALSE)) %>%
    #     group_by(age1, sex) %>% 
    #     summarise(n = n(), wt = sum(osteoporotic_fracture_year_risk)) %>% 
    #     mutate(osteoporotic_fracture_risk_per1000 = wt / n * 1000)
    
    # Check aggregated risk by age group for hip fractures
    # y_hip %>% 
    #     mutate(age1 = cut(age, 
    #                      breaks = c(-Inf, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 85, 110),
    #                      labels = c("<30", "30-34", "35-39", "40-44", "45-49", "50-54", 
    #                                "55-59", "60-64", "65-69", "70-74", "75-85", "85-110"),
    #                      right = FALSE)) %>%
    #     group_by(age1, sex) %>% 
    #     summarise(n = n(), wt = sum(hip_fracture_year_risk)) %>% 
    #     mutate(hip_fracture_risk_per1000 = wt / n * 1000)
}
