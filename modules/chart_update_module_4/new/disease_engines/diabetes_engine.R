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

    

# Calculate diabetes risk based on age and sex
calculate_diabetes_risk <- function(age, sex) {
    # Convert age to age group
    age_group <- case_when(
        age < 5 ~ '0-4',
        age < 10 ~ '5-9',
        age < 15 ~ '10-14',
        age < 20 ~ '15-19',
        age < 25 ~ '20-24',
        age < 30 ~ '25-29',
        age < 35 ~ '30-34',
        age < 40 ~ '35-39',
        age < 45 ~ '40-44',
        age < 50 ~ '45-49',
        age < 55 ~ '50-54',
        age < 60 ~ '55-59',
        age < 65 ~ '60-64',
        age < 70 ~ '65-69',
        age < 75 ~ '70-74',
        age < 80 ~ '75-79',
        age < 85 ~ '80-84',
        age < 90 ~ '85-89',
        TRUE ~ '90-110'
    )
    
    # Get incidence rate per 100k
    rate <- diabetes_incidence_per100k %>%
        filter(age == age_group) %>%
        pull(if_else(sex == "Male", Males, Females))
    
    # Convert to probability (per person, not per 100k)
    probability <- rate / 100000
    
    return(probability)
}

bmi_diabetes_rr <- tribble(
    ~age, ~RR,
    "0-24", 1.0,
    "25-29", 2.5,
    "30-34", 2.5,
    "35-39", 2.3,
    "40-44", 2.1,
    "45-49", 1.9,
    "50-54", 1.8,
    "55-59", 1.7,
    "60-64", 1.6,
    "65-69", 1.5,
    "70-74", 1.4,
    "75-79", 1.3,
    "80-84", 1.2,
    "85-89", 1.1,
    "90-94", 1.1,
    "95-110", 1.1
)

pm25_diabetes_rr <- 1.15

inc_dt <- as.data.table(diabetes_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Function 1: Apply risk based on age and gender alone
apply_diabetes_risk_base <- function(input_population) {
    dt <- as.data.table(input_population)
    
    dt[, age_group_inc := cut(age, 
                                                        breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 110),
                                                        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                                                         "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                                                                         "75-79", "80-84", "85-89", "90-110"),
                                                        right = TRUE)]
    
    dt[inc_dt, on = .(age_group_inc = age, sex), diabetes_year_risk := i.incidence / 100000]
    dt[is.na(diabetes_year_risk), diabetes_year_risk := 0]
    
    dt[, age_group_inc := NULL]
    
    return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_diabetes_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    if ("diabetes" %in% names(dt)) {
        dt <- dt[diabetes == 0]
    }
    
    breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
    labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                                    "45-49", "50-54", "55-59", "60-64", "65-69", 
                                    "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")
                                    
    dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
    
    rr_bmi_dt <- as.data.table(bmi_diabetes_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
    dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
    
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 30,
        bmi == "obese", 37,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    dt[, RR_pm25_indiv := pm25_diabetes_rr^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
    
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

diabetes_theoretical_min_table <- calculate_diabetes_theoretical_min(initial_time_zero_population)
    

# Function 3: Apply Risk Factors
apply_diabetes_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
    labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                                    "45-49", "50-54", "55-59", "60-64", "65-69", 
                                    "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")
    dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
    
    rr_bmi_dt <- as.data.table(bmi_diabetes_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
    dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
    
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 30,
        bmi == "obese", 37,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    dt[, RR_pm25_indiv := pm25_diabetes_rr^(pm25g / 10)]
    dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
    
    dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
    
    dt[, age_group_inc := cut(age, 
                                                        breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, 110),
                                                        labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                                                         "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                                                                         "75-79", "80-84", "85-89", "90-110"),
                                                        right = TRUE)]
                                                        
    dt[min_dt, on = .(age_group_inc, sex), diabetes_prob_min := i.diabetes_prob_min]
    dt[is.na(diabetes_prob_min), diabetes_prob_min := 0]
    
    dt[, diabetes_year_risk := diabetes_prob_min * RR_combined]
    
    dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
                 "RR_pm25_indiv", "RR_combined", "age_group_inc", "diabetes_prob_min") := NULL]
                 
    return(dt)
}

# initial_time_zero_population %>% 
#     mutate(bmi = case_when(
#         bmi == 'obese ' &runif(n())>0.1 ~ "normal",
#         T ~bmi
#     ), 
#     pm25g = ) %>%
# apply_diabetes_risk_factors( theoretical_min_table) %>% 
#     pull(diabetes_year_risk) %>% 
#     sum()
    

# RR overweight
# BMI 25-29.9
# Normal weight =
#     1.0
# RR obesity
# BMI 30 or more
# Normal weight =
#     1.0
# Age
# adjustments*
#     (multiplier of
#      differential risk)
# Smoking
# adjustments
# *
#     (never smoker
#      =1.0)
# men women men women
# 2.25 2.30 5.50 7.00