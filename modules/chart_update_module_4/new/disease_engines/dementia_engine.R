# filepath: /Users/aarongorman/Documents/SIB/PHM/PHModel/disease_engines/dementia_engine.R

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions

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

bmi_dementia_rr <- tribble(
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

pm25_dementia_rr <- 1.10

inc_dt <- as.data.table(dementia_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Function 1: Apply risk based on age and gender alone
apply_dementia_risk_base <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Incidence groups: '0-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-110'
    dt[, age_group_inc := cut(age, 
                                                        breaks = c(-Inf, 59, 64, 69, 74, 79, 84, 89, 110),
                                                        labels = c("0-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                                                        right = TRUE)]
    
    dt[inc_dt, on = .(age_group_inc = age, sex), dementia_year_risk := i.incidence / 100000]
    dt[is.na(dementia_year_risk), dementia_year_risk := 0]
    
    dt[, age_group_inc := NULL]
    
    return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_dementia_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Exclude prevalent cases
    if ("dementia" %in% names(dt)) {
        dt <- dt[dementia == 0]
    }
    
    # 1. Map Age to BMI RR groups
    breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
    labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                                    "45-49", "50-54", "55-59", "60-64", "65-69", 
                                    "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")
                                    
    dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
    
    rr_bmi_dt <- as.data.table(bmi_dementia_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
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
    dt[, RR_pm25_indiv := pm25_dementia_rr^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
    
    # PAF grouping
    dt[, age_group_inc := cut(age, 
                                                        breaks = c(-Inf, 59, 64, 69, 74, 79, 84, 89, 110),
                                                        labels = c("0-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                                                        right = TRUE)]
                                                        
    paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
    
    inc_dt <- as.data.table(dementia_incidence_per100k) %>%
        melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
        
    min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
    
    min_dt[, dementia_prob_min := (incidence / 100000) * (1 - AF)]
    
    return(min_dt[, .(age_group_inc = age, sex, dementia_prob_min)])
}

# Function 3: Apply Risk Factors
apply_dementia_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
    labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                                    "45-49", "50-54", "55-59", "60-64", "65-69", 
                                    "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")
    dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
    
    rr_bmi_dt <- as.data.table(bmi_dementia_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
    dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
    
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 30,
        bmi == "obese", 37,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    dt[, RR_pm25_indiv := pm25_dementia_rr^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
    
    dt[, age_group_inc := cut(age, 
                                                        breaks = c(-Inf, 59, 64, 69, 74, 79, 84, 89, 110),
                                                        labels = c("0-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"),
                                                        right = TRUE)]
                                                        
    dt[min_dt, on = .(age_group_inc, sex), dementia_prob_min := i.dementia_prob_min]
    dt[is.na(dementia_prob_min), dementia_prob_min := 0]
    
    dt[, dementia_year_risk := dementia_prob_min * RR_combined]
    
    dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
                 "RR_pm25_indiv", "RR_combined", "age_group_inc", "dementia_prob_min") := NULL]
                 
    return(dt)
}