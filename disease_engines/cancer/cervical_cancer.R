# ============================================================================
# CERVICAL CANCER RISK ENGINE
# ============================================================================

# Cervical Cancer Risk Factors

# Risk Factors Included:
# - Smoking (SAPM)

# Smoking Relative Risk:
# Current Smokers: RR = 1.83 (SAPM)
# Never Smokers: RR = 1.0 (reference)

# Source: SAPM (Sheffield Alcohol Policy Model)

library(data.table)
library(tibble)

# Data Definitions ----

# Smoking Relative Risk for Cervical Cancer
rr_cervical_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1,
  "current_smoker", 1.83
)

# Functions ----

# Function 1: Apply baseline risk based on age and sex alone
apply_cervical_cancer_risk_wo_risk_factors <- function(input_population) {
  
  # 1. Define incidence rates using the fread "visual table" approach
  # This replaces the bulky tribble call
cervical_age_sex_incidence <- fread("
age, sex, per100k
0-29, Females, 1.5 
30-34, Females, 19.400000000000002 
35-39, Females, 18.6 
40-44, Females, 18.1 
45-49, Females, 15.600000000000001 
50-54, Females, 10.8 
55-59, Females, 13.9 
60-64, Females, 9.600000000000001 
65-69, Females, 6.4 
70-74, Females, 7.5 
75-79, Females, 6.9 
80-84, Females, 4.800000000000001 
85-110, Females, 7.800000000000001 
  ")
  
  # Calculate annual risk decimal
cervical_age_sex_incidence[, cervical_cancer_year_risk := as.numeric(per100k) / 100000]
cervical_age_sex_incidence[, per100k := NULL]
  
  if( 'cervical_cancer_year_risk' %in% names(input_population)){
    input_population[, cervical_cancer_year_risk := NULL] 
  }
  
  # 2. Ensure input is a data.table (in-place)
  if (!is.data.table(input_population)) setDT(input_population)
  
  # 3. Create join key and perform update-on-join
  # Using the 'labels' argument in cut to match the table keys exactly
  input_population[, age_join := as.character(cut(age,
                                                  breaks = c(-Inf, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
                                                  labels = c("0-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                             "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                                             "85-110")))]
  
  # Update-on-join (assigns the new column without a full copy)
  input_population[
    cervical_age_sex_incidence, 
    on = .(age_join=age, sex), 
    cervical_cancer_year_risk := i.cervical_cancer_year_risk
  ]
  
  input_population[,age_join := NULL]
  
  input_population[is.na(cervical_cancer_year_risk), cervical_cancer_year_risk := 0]
  
  return(input_population)
}

# Function 2: Calculate PAF (Population Attributable Fraction) and Theoretical Minimum
calculate_cervical_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases if cervical cancer column exists
  if ("cervical_cancer" %in% names(dt)) {
    dt <- dt[cervical_cancer == 0]
  }
  
  # Ensure input is a data.table
  if (!is.data.table(input_population)) setDT(dt)
  
  # Apply Smoking RR
  rr_smoking_dt <- as.data.table(rr_cervical_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # For females only (cervical cancer only affects females)
  dt <- dt[sex == "Females"]
  
  # Create age groups matching the incidence data
  dt[, age_join := as.character(cut(age,
                                    breaks = c(-Inf, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
                                    labels = c("0-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                               "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                               "85-110")))]
  
  # Calculate PAF by age group
  # PAF = 1 - (n / sum(RR_combined))
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_smoking_indiv, na.rm = TRUE)), by = .(age_join)]
  
  # Define incidence rates
  cervical_incidence <- fread("
age, per100k
0-29, 1.5
30-34, 19.4
35-39, 18.6
40-44, 18.1
45-49, 15.6
50-54, 10.8
55-59, 13.9
60-64, 9.6
65-69, 6.4
70-74, 7.5
75-79, 6.9
80-84, 4.8
85-110, 7.8
  ")
  
  # Merge PAF with incidence data
  min_dt <- merge(cervical_incidence, paf_dt, by.x = "age", by.y = "age_join")
  
  # Calculate theoretical minimum risk
  min_dt[, cervical_prob_min := (per100k / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group = age, cervical_prob_min)])
}

# Function 3: Apply Risk Factors (Smoking)
apply_cervical_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # Ensure input is a data.table
  if (!is.data.table(dt)) setDT(dt)
  
  # Initialize risk to 0 for all individuals
  dt[, cervical_cancer_year_risk := 0]
  
  # Only apply risk to females
  dt_females <- dt[sex == "Females"]
  dt_males <- dt[sex == "Males"]
  
  if (nrow(dt_females) > 0) {
    # Apply Smoking RR
    rr_smoking_dt <- as.data.table(rr_cervical_smoking)
    dt_females[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt_females[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # Create age groups
    dt_females[, age_group := as.character(cut(age,
                                               breaks = c(-Inf, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
                                               labels = c("0-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                          "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                                          "85-110")))]
    
    # Join with theoretical minimum
    dt_females[min_dt, on = .(age_group), cervical_prob_min := i.cervical_prob_min]
    dt_females[is.na(cervical_prob_min), cervical_prob_min := 0]
    
    # Calculate individual risk
    dt_females[, cervical_cancer_year_risk := cervical_prob_min * RR_smoking_indiv]
    
    # Clean up temporary columns
    dt_females[, c("RR_smoking_indiv", "age_group", "cervical_prob_min") := NULL]
  }
  
  # Combine males and females back together
  result <- rbindlist(list(dt_females, dt_males), use.names = TRUE, fill = TRUE)
  
  # Ensure males have 0 risk
  result[sex == "Males" & is.na(cervical_cancer_year_risk), cervical_cancer_year_risk := 0]
  
  result[, c("RR_smoking_indiv", "age_group", "cervical_prob_min") := NULL]
  
  return(result)
}

# initial_time_zero_population = read.fst('./main/initial_time_zero_population10down.fst')

# input_population <-  initial_time_zero_population #%>%
# initial_time_zero_population %>%
#   apply_cervical_cancer_risk_wo_risk_factors()  %>%
#     count(cervical_cancer_year_risk)

# Example Usage:
# Step 1: Calculate theoretical minimum (do once per population/year)
# cervical_theoretical_min_table <- calculate_cervical_theoretical_min(current_population)
# 
# Step 2: Apply risk factors to calculate individual risk
# population_with_risk <- apply_cervical_risk_factors(current_population, cervical_theoretical_min_table)
