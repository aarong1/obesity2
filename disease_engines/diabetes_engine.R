# ============================================================================
# TYPE 2 DIABETES RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Diabetes Type 2 Incidence Data (per 100,000)
diabetes_incidence_per100k <- 
    tribble(
        ~age, ~Males, ~Females,
        '0-4', 56, 53, 
        '5-9', 34, 42, 
        '10-14', 43, 40, 
        '15-19', 83, 107, 
        '20-24', 75, 145, 
        '25-29', 101, 226, 
        '30-34', 150, 242, 
        '35-39', 240, 263, 
        '40-44', 355, 333, 
        '45-49', 561, 482, 
        '50-54', 820, 636, 
        '55-59', 1068, 847, 
        '60-64', 1316, 965, 
        '65-69', 1516, 1234, 
        '70-74', 1763, 1378, 
        '75-79', 1677, 1483, 
        '80-84', 1645, 1336, 
        '85-89', 1300, 1169, 
        '90-110', 546, 440
    )

# Risk Factor Relative Risks for Type 2 Diabetes ----
# 
# This model includes 8 modifiable risk factors:
# 1. BMI (Body Mass Index) - categorical (normal/overweight/obese)
# 2. PM2.5 (Air Pollution) - per 10 μg/m³
# 3. Smoking Status - never/former/current
# 4. Physical Activity - age-stratified (25-79 years), inactive/some_activity
# 5. Sleep Duration - U-shaped relationship (5-9+ hours)
# 6. HDL Cholesterol - 4 categories (high/normal/low/very_low)
# 7. Hypertension - 3 categories (normal/pre-hypertension/hypertension)
# 8. Ethnicity - White/Asian/Hispanic/Black/Other
#
# Risk factors 5-8 are optional. If not present in the population data,
# their RR defaults to 1.0 (no effect).
# Depression risk factor is commented out but retained for reference.
# ----

# BMI - Relative Risk by Age Group (per 5 kg/m² increment) - ORIGINAL (COMMENTED OUT)
# bmi_diabetes_rr <- tribble(
#     ~age, ~RR,
#     "0-19", 1.0,          # No data for children
#     "20-24", 3.547,       # GBD 2019
#     "25-29", 3.547,       # GBD 2019
#     "30-34", 3.455,       # GBD 2019
#     "35-39", 3.349,       # GBD 2019
#     "40-44", 3.160,       # GBD 2019
#     "45-49", 2.864,       # GBD 2019
#     "50-54", 2.624,       # GBD 2019
#     "55-59", 2.417,       # GBD 2019
#     "60-64", 2.215,       # GBD 2019
#     "65-69", 2.046,       # GBD 2019
#     "70-74", 1.896,       # GBD 2019
#     "75-79", 1.740,       # GBD 2019
#     "80-84", 1.461,       # GBD 2019
#     "85-89", 1.461,       # GBD 2019
#     "90-94", 1.461,       # GBD 2019
#     "95-110", 1.461       # GBD 2019
# )

# BMI - Relative Risk by BMI Category (CURRENT)
# bmi_diabetes_rr <- tribble(
#     ~bmi_category, ~RR,
#     "<23", 1.0,
#     "23-24.9", 2.67,
#     "25-29.9", 7.59,
#     "30-34.9", 20.1,
#     ">35", 38.8
# )

# bmi_diabetes_rr <- tribble(
#     ~bmi_category, ~RR,
#     # "<23", 1.0,
#      "normal", 1.0,
# 
#     # "23-24.9", 2.67,
#     "overweight", 7.59,
#     "obese", 20.1,
#     # ">35", 38.8
# )

#The magnitude of association between overweight and obesity and the risk of diabetes: A meta-analysis of prospective cohort studies
#https://www.sciencedirect.com/science/article/abs/pii/S0168822710001944

bmi_diabetes_rr <- tribble(
    ~bmi_category, ~RR,
    # "<23", 1.0,
    "normal", 1.0,
    
    # "23-24.9", 2.67,
    "overweight", 2.99,
    "obese", 7.19,
    # ">35", 38.8
)

# Smoking - Relative Risk for Type 2 Diabetes
# Source: https://www.sciencedirect.com/science/article/pii/S2001037021000751
rr_diabetes_smoking <- tribble(
    ~sex, ~smoking, ~RR,
    "Males", "never_smoked", 1.0,
    "Females", "never_smoked", 1.0,
    "Males", "light_smoker", 1.05,      # <20 cigarettes/day
    "Females", "light_smoker", 0.98,
    "Males", "moderate_smoker", 1.19,   # 20-39 cigarettes/day
    "Females", "moderate_smoker", 1.21,
    "Males", "heavy_smoker", 1.45,      # 40+ cigarettes/day
    "Females", "heavy_smoker", 1.74,
    "Males", "former", 1.07,
    "Females", "former", 1.07
)

# Physical Activity - Relative Risk by Age Group
rr_diabetes_physical_activity <- tribble(
    ~activity, ~age, ~RR,
    "inactive", "25-29", 1.519,      # 0 METs
    "inactive", "30-34", 1.519,
    "inactive", "35-39", 1.519,
    "inactive", "40-44", 1.447,
    "inactive", "45-49", 1.323,
    "inactive", "50-54", 1.213,
    "inactive", "55-59", 1.109,
    "inactive", "60-64", 1.000,
    "inactive", "65-69", 0.900,
    "inactive", "70-74", 0.804,
    "inactive", "75-79", 0.696,
    "some_activity", "25-29", 1.490,  # 600 METs
    "some_activity", "30-34", 1.490,
    "some_activity", "35-39", 1.490,
    "some_activity", "40-44", 1.418,
    "some_activity", "45-49", 1.297,
    "some_activity", "50-54", 1.189,
    "some_activity", "55-59", 1.087,
    "some_activity", "60-64", 0.980,
    "some_activity", "65-69", 0.882,
    "some_activity", "70-74", 0.788,
    "some_activity", "75-79", 0.682
)

# Source: IHME GBD 2019 Relative Risks
# Low physical activity → Diabetes mellitus type 2
# Note: Add more activity levels (low_activity, meets_rec) with intermediate values

# PM2.5 - Relative Risk per 10 μg/m³
pm25_diabetes_rr <- 1.15

# Sleep Duration - Relative Risk for Type 2 Diabetes
# Source: NHS (Nurses' Health Study) [79, 80] 2003
# U-shaped relationship: optimal sleep at 8 hours
rr_diabetes_sleep <- tribble(
    ~sleep, ~RR,
'OSA',1.35
)

# HDL Cholesterol - Relative Risk for Type 2 Diabetes
# Source: CCHS/CGPS [154, 163, 164] 2015
# Lower HDL = higher risk
# Population data values: normal_cholesterol (n=80774), raised_cholesterol (n=59717), hdl/cholesterol (n=312)
rr_diabetes_hdl <- tribble(
    ~cholesterol_status, ~RR,
    "normal_cholesterol", 1.0,       # Reference (normal HDL and total cholesterol)
    "hdl/cholesterol", 1.0,          # HDL tracked separately (assume normal)
    "raised_cholesterol", 1.85       # Elevated total cholesterol (associated with lower HDL)
)

# Hypertension - Relative Risk for Type 2 Diabetes
# Source: Japanese cohort study
rr_diabetes_hypertension <- tribble(
    ~hypertension_status, ~RR,
    "normotensive_untreated", 1.0,      # <130/85 mmHg
    "hypertensive_controlled", 1.39,    # Controlled hypertension
    "hypertensive_uncontrolled", 1.76,  # ≥140/90 mmHg
    "hypertensive_untreated", 1.76      # Untreated hypertension
)

# Ethnicity - Relative Risk for Type 2 Diabetes
# Source: NHS (Nurses' Health Study) [176, 80] 2006
# rr_diabetes_ethnicity <- tribble(
#     ~ethnicity, ~RR,
#     "White", 1.0,      # Reference
#     "Asian", 1.94,
#     "Hispanic", 1.70,
#     "Black", 1.36,
#     "Other", 1.0       # Default to reference
# )

rr_diabetes_ethnicity <- tribble(
    ~ethnicity, ~RR,
    "White", 1.0,      # Reference
    "chinese", 1.94,
    "indian", 1.94,
    "pakistani", 1.94,
    "filipino", 1.94,
    "arab", 1.94,
    "other asian", 1.94,
    "Hispanic", 1.70,
    "black african", 1.36,
    "Other", 1.0       # Default to reference
)

# Sample population ethnicity counts:
#        ethnicity      n
# 1           arab    390
# 2  black african    378
# 3        chinese    985
# 4       filipino    312
# 5         indian    595
# 6          mixed   1008
# 7          other     78
# 8    other asian    289
# 9      pakistani    312
# 10          roma    231
# 11         white 136225

# Depression - Relative Risk for Type 2 Diabetes (COMMENTED OUT)
# Source: NHANES I [88] 2003
# rr_diabetes_depression <- tribble(
#     ~depression, ~RR,
#     "none", 1.0,       # Reference
#     "mild", 1.24,
#     "major", 2.52
# )

# Convert incidence to long format for easy merging
inc_dt <- as.data.table(diabetes_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_diabetes_risk_engine_age_sex <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Incidence age groups: '0-4', '5-9', ... '90-110'
    dt[, age_group_inc := cut(age, 
                              breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 110),
                              labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                                       "75-79", "80-84", "85-89", "90-110"),
                              right = TRUE)]
    
    inc_dt_temp <- as.data.table(diabetes_incidence_per100k) %>%
        melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
    dt[inc_dt_temp, on = .(age_group_inc = age, sex), diabetes_year_risk := i.incidence / 100000]
    dt[is.na(diabetes_year_risk), diabetes_year_risk := 0]
    
    dt[, age_group_inc := NULL]
    
    return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_diabetes_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Exclude prevalent cases
    if ("diabetes" %in% names(dt)) {
        dt <- dt[diabetes == 0]
    }
    
    # 1. BMI RR - Categorical
    if ("bmi" %in% names(dt)) {
        rr_bmi_dt <- as.data.table(bmi_diabetes_rr)
        dt[rr_bmi_dt, on = .(bmi = bmi_category), RR_bmi_indiv := i.RR]
        dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    } else {
        dt[, RR_bmi_indiv := 1]
    }
    
    # 2. PM2.5 RR
    dt[, RR_pm25_indiv := pm25_diabetes_rr^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    # 3. Smoking RR
    rr_smoking_dt <- as.data.table(rr_diabetes_smoking)
    dt[rr_smoking_dt, on = .(sex, smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 4. Physical Activity RR (age-stratified)
    # Map age to PA RR age groups (25-79, outside this range = 1.0)
    dt[, age_group_pa := cut(age,
                            breaks = c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 110),
                            labels = c("<25", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
                            right = FALSE)]
    
    rr_pa_dt <- as.data.table(rr_diabetes_physical_activity)
    dt[rr_pa_dt, on = .(pa = activity, age_group_pa = age), RR_pa_indiv := i.RR]
    dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
    
    # 5. Sleep Duration RR (if available)
    if ("sleep_hours" %in% names(dt)) {
        rr_sleep_dt <- as.data.table(rr_diabetes_sleep)
        dt[rr_sleep_dt, on = .(sleep_hours), RR_sleep_indiv := i.RR]
        dt[is.na(RR_sleep_indiv), RR_sleep_indiv := 1]
    } else {
        dt[, RR_sleep_indiv := 1]
    }
    
    # 6. HDL Cholesterol RR (if available)
    if ("cholesterol_status" %in% names(dt)) {
        rr_hdl_dt <- as.data.table(rr_diabetes_hdl)
        dt[rr_hdl_dt, on = .(cholesterol_status), RR_hdl_indiv := i.RR]
        dt[is.na(RR_hdl_indiv), RR_hdl_indiv := 1]
    } else {
        dt[, RR_hdl_indiv := 1]
    }
    
    # 7. Hypertension RR (if available)
    if ("hypertension_status" %in% names(dt)) {
        rr_hypertension_dt <- as.data.table(rr_diabetes_hypertension)
        dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
        dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    } else {
        dt[, RR_hypertension_indiv := 1]
    }
    
    # 8. Ethnicity RR (if available)
    if ("ethnicity" %in% names(dt)) {
        rr_ethnicity_dt <- as.data.table(rr_diabetes_ethnicity)
        dt[rr_ethnicity_dt, on = .(ethnicity), RR_ethnicity_indiv := i.RR]
        dt[is.na(RR_ethnicity_indiv), RR_ethnicity_indiv := 1]
    } else {
        dt[, RR_ethnicity_indiv := 1]
    }
    
    # 9. Depression RR (COMMENTED OUT)
    # if ("depression" %in% names(dt)) {
    #     rr_depression_dt <- as.data.table(rr_diabetes_depression)
    #     dt[rr_depression_dt, on = .(depression), RR_depression_indiv := i.RR]
    #     dt[is.na(RR_depression_indiv), RR_depression_indiv := 1]
    # } else {
    #     dt[, RR_depression_indiv := 1]
    # }
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * RR_pa_indiv * 
                        RR_sleep_indiv * RR_hdl_indiv * RR_hypertension_indiv * 
                        RR_ethnicity_indiv]
    
    # PAF grouping by incidence age groups
    dt[, age_group_inc := cut(age, 
                              breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 110),
                              labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                                       "75-79", "80-84", "85-89", "90-110"),
                              right = TRUE)]
    
    paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
    
    inc_dt <- as.data.table(diabetes_incidence_per100k) %>%
        melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
    
    min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
    
    min_dt[, diabetes_prob_min := (incidence / 100000) * (1 - AF)]
    
    return(min_dt[, .(age_group_inc = age, sex, diabetes_prob_min)])
}

# Function 3: Apply Risk Factors
apply_diabetes_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    # 1. BMI RR - Categorical
    if ("bmi" %in% names(dt)) {
        rr_bmi_dt <- as.data.table(bmi_diabetes_rr)
        dt[rr_bmi_dt, on = .(bmi = bmi_category), RR_bmi_indiv := i.RR]
        dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    } else {
        dt[, RR_bmi_indiv := 1]
    }
    
    # 2. PM2.5 RR
    dt[, RR_pm25_indiv := pm25_diabetes_rr^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    # 3. Smoking RR
    rr_smoking_dt <- as.data.table(rr_diabetes_smoking)
    dt[rr_smoking_dt, on = .(sex, smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 4. Physical Activity RR (age-stratified)
    dt[, age_group_pa := cut(age,
                            breaks = c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 110),
                            labels = c("<25", "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
                            right = FALSE)]
    
    rr_pa_dt <- as.data.table(rr_diabetes_physical_activity)
    dt[rr_pa_dt, on = .(pa = activity, age_group_pa = age), RR_pa_indiv := i.RR]
    dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
    
    # 5. Sleep Duration RR (if available)
    if ("sleep_hours" %in% names(dt)) {
        rr_sleep_dt <- as.data.table(rr_diabetes_sleep)
        dt[rr_sleep_dt, on = .(sleep_hours), RR_sleep_indiv := i.RR]
        dt[is.na(RR_sleep_indiv), RR_sleep_indiv := 1]
    } else {
        dt[, RR_sleep_indiv := 1]
    }
    
    # 6. HDL Cholesterol RR (if available)
    if ("cholesterol_status" %in% names(dt)) {
        rr_hdl_dt <- as.data.table(rr_diabetes_hdl)
        dt[rr_hdl_dt, on = .(cholesterol_status), RR_hdl_indiv := i.RR]
        dt[is.na(RR_hdl_indiv), RR_hdl_indiv := 1]
    } else {
        dt[, RR_hdl_indiv := 1]
    }
    
    # 7. Hypertension RR (if available)
    if ("hypertension_status" %in% names(dt)) {
        rr_hypertension_dt <- as.data.table(rr_diabetes_hypertension)
        dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
        dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    } else {
        dt[, RR_hypertension_indiv := 1]
    }
    
    # 8. Ethnicity RR (if available)
    if ("ethnicity" %in% names(dt)) {
        rr_ethnicity_dt <- as.data.table(rr_diabetes_ethnicity)
        dt[rr_ethnicity_dt, on = .(ethnicity), RR_ethnicity_indiv := i.RR]
        dt[is.na(RR_ethnicity_indiv), RR_ethnicity_indiv := 1]
    } else {
        dt[, RR_ethnicity_indiv := 1]
    }
    
    # 9. Depression RR (COMMENTED OUT)
    # if ("depression" %in% names(dt)) {
    #     rr_depression_dt <- as.data.table(rr_diabetes_depression)
    #     dt[rr_depression_dt, on = .(depression), RR_depression_indiv := i.RR]
    #     dt[is.na(RR_depression_indiv), RR_depression_indiv := 1]
    # } else {
    #     dt[, RR_depression_indiv := 1]
    # }
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * RR_pa_indiv * 
                        RR_sleep_indiv * RR_hdl_indiv * RR_hypertension_indiv * 
                        RR_ethnicity_indiv]
    
    dt[, age_group_inc := cut(age, 
                              breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 110),
                              labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                                       "75-79", "80-84", "85-89", "90-110"),
                              right = TRUE)]
    
    dt[min_dt, on = .(age_group_inc, sex), diabetes_prob_min := i.diabetes_prob_min]
    dt[is.na(diabetes_prob_min), diabetes_prob_min := 0]
    
    dt[, diabetes_year_risk := diabetes_prob_min * RR_combined]
    
    # Clean up temporary columns
    dt[, c("RR_bmi_indiv", 
           "RR_pm25_indiv", "RR_smoking_indiv", "age_group_pa", "RR_pa_indiv",
           "RR_sleep_indiv", "RR_hdl_indiv", "RR_hypertension_indiv", 
           "RR_ethnicity_indiv",
           "RR_combined", "age_group_inc", "diabetes_prob_min") := NULL]
    
    return(dt)
}

# Unit Tests and Examples ----
store_unit_tests <- function() {
    
    # Example usage with historical population
    x <- past_populations %>% 
        filter(year == min(year))
    
    t <- calculate_diabetes_theoretical_min(x)
    y <- apply_diabetes_risk_factors(x, theoretical_min_table = t)
    
    # Check aggregated risk by age group
    y <- y %>% mutate(age1 = cut(age, 
                                 breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 110),
                                 labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                          "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                                          "75-79", "80-84", "85-89", "90-110"),
                                 right = TRUE))
    
    y %>% 
        group_by(age1, sex) %>% 
        summarise(n = n(), wt = sum(diabetes_year_risk)) %>% 
        mutate(diabetes_year_risk_per100k = wt / n * 100000)
}