# ============================================================================
# KIDNEY DISEASE RISK ENGINE
# ============================================================================
# Source: THIN database
# Kidney disease incidence rates per 100,000 person-years

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Kidney Disease Incidence Data (per 100,000 person-years)
# Source: THIN database (used in calculations), QResearch data retained for reference
kidney_incidence_disease_per100k <- tribble(
    ~age, ~sex, ~qresearch_rate, ~thin_rate,
    # Women
    "35-39", "Females", 5.27, 5.57,
    "40-44", "Females", 7.11, 9.34,
    "45-49", "Females", 14.09, 14.62,
    "50-54", "Females", 24.35, 26.61,
    "55-59", "Females", 43.76, 47.98,
    "60-64", "Females", 89.55, 99.94,
    "65-69", "Females", 167.61, 182.76,
    "70-74", "Females", 291.44, 315.58,
    "75-110", "Females", 291.44, 315.58,  # Extended to older ages
    # Men
    "35-39", "Males", 4.61, 6.26,
    "40-44", "Males", 6.82, 9.92,
    "45-49", "Males", 11.70, 15.72,
    "50-54", "Males", 18.82, 24.06,
    "55-59", "Males", 33.47, 40.93,
    "60-64", "Males", 65.64, 75.49,
    "65-69", "Males", 129.01, 150.21,
    "70-74", "Males", 224.47, 248.59,
    "75-110", "Males", 224.47, 248.59   # Extended to older ages
)


# Risk Factor Relative Risks for Chronic Kidney Disease ----

# BMI - Relative Risk (simple categorical)
# Source: Obesity as a predictive factor for chronic kidney disease in adults: systematic review and meta-analysis
# https://www.bjournal.org/wp-content/uploads/articles_xml/1414-431X-bjmbr-54-4-e10022/1414-431X-bjmbr-54-4-e10022.x36241.pdf
rr_kidney_disease_bmi_simple <- tribble(
    ~bmi, ~RR,
    "normal", 1.0,
    "overweight", 1.0,
    "obese", 1.81
)

# BMI - Relative Risk (age-stratified, per 5 kg/m² increment)
# More detailed age-specific estimates

rr_kidney_disease_bmi_age <- tribble(
    ~age, ~RR,
    "0-39", 1.0,
    "40-44", 1.746,
    "45-49", 1.746,
    "50-54", 1.746,
    "55-59", 1.746,
    "60-64", 2.036,
    "65-69", 2.036,
    "70-74", 1.621,
    "75-79", 1.621,
    "80-84", 1.431,
    "85-89", 1.431,
    "90-94", 1.431,
    "95-110", 1.431
)

# Diabetes - Relative Risk (sex-specific)
# Source: Diabetes mellitus as a risk factor for incident chronic kidney disease and end-stage renal disease in women compared with men: systematic review and meta-analysis
# https://pubmed.ncbi.nlm.nih.gov/27477292/
rr_kidney_disease_diabetes <- tribble(
    ~diabetes_status, ~sex, ~RR,
    "no_diabetes", "Males", 1.0,
    "no_diabetes", "Females", 1.0,
    "undiagnosed_diabetes", "Males", 2.84,
    "undiagnosed_diabetes", "Females", 3.34,
    "diagnosed_diabetes", "Males", 2.84,
    "diagnosed_diabetes", "Females", 3.34
)

# Hypertension - Relative Risk
rr_kidney_disease_hypertension <- tribble(
    ~hypertension_status, ~RR,
    "normotensive_untreated", 1.0,
    "hypertensive_controlled", 1.283,
    "hypertensive_uncontrolled", 1.283,
    "hypertensive_untreated", 1.283
)

# Convert incidence to long format for easy merging
# Note: inc_dt used in apply_kidney_risk_engine_age_sex function
inc_dt <- as.data.table(kidney_incidence_disease_per100k)

# Functions ----

# Apply kidney disease risk based on age and sex alone
apply_kidney_risk_engine_age_sex <- function(input_population) {
    dt <- as.data.table(input_population)

    # Kidney disease age groups: '35-39', '40-44', ..., '75-110'
    dt[, age_group_kidney := cut(age, 
                                 breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 110),
                                 labels = c("<35", "35-39", "40-44", "45-49", "50-54", 
                                           "55-59", "60-64", "65-69", "70-74", "75-110"),
                                 right = FALSE)]
    
    # Join with incidence data
    inc_dt_temp <- as.data.table(kidney_incidence_disease_per100k)
    setnames(inc_dt_temp, 'thin_rate', "incidence_rate")
    dt[inc_dt_temp, on = .(age_group_kidney = age, sex), 
       kidney_year_risk := i.incidence_rate / 100000]
    
    # For ages <35, use zero (no risk)
    dt[age_group_kidney == "<35", kidney_year_risk := 0]
    
    dt[is.na(kidney_year_risk), kidney_year_risk := 0]
    
    dt[, age_group_kidney := NULL]
    
    return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_kidney_disease_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Exclude prevalent cases
    if ("kidney_disease" %in% names(dt)) {
        dt <- dt[kidney_disease == 0]
    }
    
    # 1. BMI RR - Age-stratified (per 5 kg/m² from reference of 20)
    dt[, age_group_bmi := cut(age, 
                              breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 110),
                              labels = c("0-39", "40-44", "45-49", "50-54", "55-59", 
                                        "60-64", "65-69", "70-74", "75-79", "80-84", 
                                        "85-89", "90-94", "95-110"),
                              right = FALSE)]
    
    rr_bmi_dt <- as.data.table(rr_kidney_disease_bmi_age)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
    dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
    
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 28,
        bmi == "obese", 35,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Diabetes RR (sex-specific)
    rr_diabetes_dt <- as.data.table(rr_kidney_disease_diabetes)
    dt[rr_diabetes_dt, on = .(diabetes_status, sex), RR_diabetes_indiv := i.RR]
    dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
    
    # 3. Hypertension RR
    rr_hypertension_dt <- as.data.table(rr_kidney_disease_hypertension)
    dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
    dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_diabetes_indiv * RR_hypertension_indiv]
    
    # PAF grouping by kidney disease age groups
    dt[, age_group_kidney := cut(age, 
                                 breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 110),
                                 labels = c("<35", "35-39", "40-44", "45-49", "50-54", 
                                           "55-59", "60-64", "65-69", "70-74", "75-110"),
                                 right = FALSE)]
    
    paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_kidney, sex)]
    
    # Prepare incidence data
    inc_dt_source <- as.data.table(kidney_incidence_disease_per100k)
    print(inc_dt_source)
    
    setnames(inc_dt_source, "thin_rate", "incidence_rate")
    
    # For ages <35, add zero rate
    inc_35_below <- data.table(
        age = c("<35", "<35"),
        sex = c("Males", "Females"),
        incidence_rate = c(0, 0)
    )
    
    inc_dt_full <- rbind(
        inc_dt_source[, .(age, sex, incidence_rate)],
        inc_35_below
    )
    
    min_dt <- merge(inc_dt_full, paf_dt, by.x = c("age", "sex"), by.y = c("age_group_kidney", "sex"))
    
    min_dt[, kidney_prob_min := (incidence_rate / 100000) * (1 - AF)]
    
    return(min_dt[, .(age_group_kidney = age, sex, kidney_prob_min)])
}

# Function 3: Apply Risk Factors
apply_kidney_disease_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    # 1. BMI RR - Age-stratified (per 5 kg/m² from reference of 20)
    dt[, age_group_bmi := cut(age, 
                              breaks = c(-Inf, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 110),
                              labels = c("0-39", "40-44", "45-49", "50-54", "55-59", 
                                        "60-64", "65-69", "70-74", "75-79", "80-84", 
                                        "85-89", "90-94", "95-110"),
                              right = FALSE)]
    
    rr_bmi_dt <- as.data.table(rr_kidney_disease_bmi_age)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
    dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
    
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 28,
        bmi == "obese", 35,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Diabetes RR (sex-specific)
    rr_diabetes_dt <- as.data.table(rr_kidney_disease_diabetes)
    dt[rr_diabetes_dt, on = .(diabetes_status, sex), RR_diabetes_indiv := i.RR]
    dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
    
    # 3. Hypertension RR
    rr_hypertension_dt <- as.data.table(rr_kidney_disease_hypertension)
    dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
    dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    
    # Combine all RRs
    dt[, RR_combined := RR_bmi_indiv * RR_diabetes_indiv * RR_hypertension_indiv]
    
    # Assign age groups
    dt[, age_group_kidney := cut(age, 
                                 breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 110),
                                 labels = c("<35", "35-39", "40-44", "45-49", "50-54", 
                                           "55-59", "60-64", "65-69", "70-74", "75-110"),
                                 right = FALSE)]
    
    dt[min_dt, on = .(age_group_kidney, sex), kidney_prob_min := i.kidney_prob_min]
    dt[is.na(kidney_prob_min), kidney_prob_min := 0]
    
    dt[, kidney_year_risk := kidney_prob_min * RR_combined]
    
    # Clean up temporary columns
    dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
           "RR_diabetes_indiv", "RR_hypertension_indiv", 
           "RR_combined", "age_group_kidney", "kidney_prob_min") := NULL]
    
    return(dt)
}

# Unit Tests and Examples ----
store_unit_tests <- function() {
    
    # Example usage with historical population
    x <- past_populations %>% 
        filter(year == min(year))
    
    # Apply age-sex risk (THIN data)
    y_age_sex <- apply_kidney_risk_engine_age_sex(x)
    
    # Calculate theoretical minimum
    t <- calculate_kidney_theoretical_min(x)
    
    # Apply risk factors
    y <- apply_kidney_risk_factors(x, theoretical_min_table = t)
    
    # Check aggregated risk by age group
    y <- y %>% 
        mutate(age1 = cut(age, 
                         breaks = c(-Inf, 35, 40, 45, 50, 55, 60, 65, 70, 75, 110),
                         labels = c("<35", "35-39", "40-44", "45-49", "50-54", 
                                   "55-59", "60-64", "65-69", "70-74", "75-110"),
                         right = FALSE))
    
    y %>% 
        group_by(age1, sex) %>% 
        summarise(n = n(), wt = sum(kidney_year_risk)) %>% 
        mutate(kidney_year_risk_per100k = wt / n * 100000)
}
