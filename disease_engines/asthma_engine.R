# ============================================================================
# ASTHMA RISK ENGINE
# ============================================================================
# Sources:
# - BMI RR: https://pmc.ncbi.nlm.nih.gov/articles/PMC1899288/#sec6
# - GBD BMI Relative Risks: Males 1.409, Females 1.402 per 5 kg/m2
# - Smoking RR: https://www.sciencedirect.com/science/article/pii/S2590332224004871

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Asthma Incidence Data (per 100,000 person-years)
# Note: Currently not sex-stratified
asthma_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0-5", 929.0, 929.0,
  "6-10", 561.0, 561.0,
  "11-15", 356.0, 356.0,
  "16-20", 170.0, 170.0,
  "21-30", 150.0, 150.0,
  "31-40", 180.0, 180.0,
  "41-50", 201.0, 201.0,
  "51-60", 204.0, 204.0,
  "61-70", 231.0, 231.0,
  "71-80", 194.0, 194.0,
  "81-110", 111.0, 111.0
)

# Risk Factor Relative Risks for Asthma ----

# BMI - Relative Risk
# Males: 1.409 per 5 kg/m², Females: 1.402 per 5 kg/m²
# Simplified categorical approach:

rr_asthma_bmi <- tribble(
  ~bmi, ~RR,
  "normal", 1.0,
  "overweight", 1.38,   # ~1 SD above normal
  "obese", 1.792         # ~2 SD above normal
)

# Smoking - Relative Risk
# Active smoking: RR = 1.73
# Former smoking: RR = 1.365 (50% reduction after 10 years quit)
rr_asthma_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former_irregular", 1.365,
  "former_regular", 1.365,
  "former", 1.365,
  "current_smoker", 1.73
)

# Functions ----

# Function 1: Apply asthma risk based on age and sex alone
apply_asthma_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Age groups
  dt[, age_group_asthma := cut(age, 
                               breaks = c(-Inf, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 110),
                               labels = c("0-5", "6-10", "11-15", "16-20", "21-30", "31-40",
                                        "41-50", "51-60", "61-70", "71-80", "81-110"),
                               right = FALSE)]
  
  # Join with incidence data
  inc_dt_temp <- as.data.table(asthma_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  dt[inc_dt_temp, on = .(age_group_asthma = age, sex), 
     asthma_year_risk := i.incidence / 100000]
  
  dt[is.na(asthma_year_risk), asthma_year_risk := 0]
  
  dt[, age_group_asthma := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_asthma_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("asthma" %in% names(dt)) {
    dt <- dt[asthma == 0]
  }
  
  # 1. BMI RR
  rr_bmi_dt <- as.data.table(rr_asthma_bmi)
  dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_asthma_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv]
  
  # PAF grouping by asthma age groups
  dt[, age_group_asthma := cut(age, 
                               breaks = c(-Inf, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 110),
                               labels = c("0-5", "6-10", "11-15", "16-20", "21-30", "31-40",
                                        "41-50", "51-60", "61-70", "71-80", "81-110"),
                               right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), 
               by = .(age_group_asthma, sex)]
  
  # Prepare incidence data
  inc_dt_source <- as.data.table(asthma_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(inc_dt_source, paf_dt, 
                  by.x = c("age", "sex"), 
                  by.y = c("age_group_asthma", "sex"))
  
  min_dt[, asthma_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_asthma = age, sex, asthma_prob_min)])
}

# Function 3: Apply Risk Factors
apply_asthma_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR
  rr_bmi_dt <- as.data.table(rr_asthma_bmi)
  dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_asthma_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv]
  
  # Assign age groups
  dt[, age_group_asthma := cut(age, 
                               breaks = c(-Inf, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 110),
                               labels = c("0-5", "6-10", "11-15", "16-20", "21-30", "31-40",
                                        "41-50", "51-60", "61-70", "71-80", "81-110"),
                               right = FALSE)]
  
  dt[min_dt, on = .(age_group_asthma, sex), asthma_prob_min := i.asthma_prob_min]
  dt[is.na(asthma_prob_min), asthma_prob_min := 0]
  
  dt[, asthma_year_risk := asthma_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "RR_smoking_indiv", "RR_combined", 
         "age_group_asthma", "asthma_prob_min") := NULL]
  
  return(dt)
}

# Unit Tests and Examples ----
store_unit_tests <- function() {
  
  # Example usage with historical population
  x <- past_populations %>% 
    filter(year == min(year))
  
  # Apply age-sex risk
  y_age_sex <- apply_asthma_risk_engine_age_sex(x)
  
  # Calculate theoretical minimum
  t <- calculate_asthma_theoretical_min(x)
  
  # Apply risk factors
  y <- apply_asthma_risk_factors(x, theoretical_min_table = t)
  
  # Check aggregated risk by age group
  y <- y %>% 
    mutate(age1 = cut(age, 
                     breaks = c(-Inf, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 110),
                     labels = c("0-5", "6-10", "11-15", "16-20", "21-30", "31-40",
                              "41-50", "51-60", "61-70", "71-80", "81-110"),
                     right = FALSE))
  
  y %>% 
    group_by(age1, sex) %>% 
    summarise(n = n(), wt = sum(asthma_year_risk)) %>% 
    mutate(asthma_year_risk_per100k = wt / n * 100000)
}



