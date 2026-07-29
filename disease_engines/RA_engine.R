# ============================================================================
# RHEUMATOID ARTHRITIS (RA) RISK ENGINE
# ============================================================================

# Original Raw Incidence Data ----
# Age (years)	Incidence rate (95% CI) per 10 000 person-years 
# 1994	2004	2014 
# Overall 
# <20	—	0.21 (0.11, 0.41)	0.08 (0.03, 0.25) 
# ≥20 and <30	0.79 (0.45, 1.39)	1.09 (0.83, 1.44)	0.81 (0.57, 1.16) 
# ≥30 and <40	2.47 (1.85, 3.30)	2.18 (1.82, 2.61)	2.09 (1.70, 2.57) 
# ≥40 and <50	3.82 (3.02, 4.83)	3.73 (3.29, 4.22)	3.48 (3.00, 4.03) 
# ≥50 and <60	6.60 (5.41, 8.04)	6.98 (6.30, 7.75)	5.95 (5.30, 6.67) 
# ≥60 and <70	7.35 (5.99, 9.00)	10.44 (9.55, 11.42)	7.41 (6.62, 8.29) 
# ≥70 and <80	10.27 (8.47, 12.45)	12.08 (10.86, 13.44)	10.27 (9.15, 11.53) 
# ≥80 and <90	7.39 (5.36, 10.20)	11.79 (10.30, 13.52)	6.97 (5.76, 8.44) 
# ≥90	10.81 (5.62, 20.77)	7.13 (4.55, 11.17)	3.41 (1.94, 6.01) 
# 2014 data used

# Risk Factors:
# BMI: 1.12 (95% CI, 1.04-1.20) in overweight and 1.23 in obese
# Source: https://pmc.ncbi.nlm.nih.gov/articles/PMC6634074/
# Smoking: RR 2.02 for current smokers
# Source: Rheumatoid arthritis	2.02	All	Both	smokes	Smoking	morbidity	SAPM

# Incidence Source:
# Rheumatoid arthritis is getting less frequent—results of a nationwide population-based cohort study
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5850292/#sup1

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# RA Incidence Data (per 10,000 person-years)
# Note: Sex-specific rates derived from source data
ra_incidence_per10k <- tribble(
  ~age, ~Males, ~Females,
  "0-19", 0.05, 0.11,
  "20-29", 0.40, 1.26,
  "30-39", 0.84, 3.34,
  "40-49", 1.99, 4.99,
  "50-59", 3.63, 8.32,
  "60-69", 5.82, 8.96,
  "70-79", 8.11, 12.20,
  "80-89", 4.81, 8.50,
  "90-110", 3.82, 3.24
)

# Risk Factor Relative Risks for RA ----

# BMI - Relative Risk
# Source: https://pmc.ncbi.nlm.nih.gov/articles/PMC6634074/
rr_ra_bmi <- tribble(
  ~bmi, ~RR,
  "normal", 1.0,
  "overweight", 1.12,
  "obese", 1.23
)

# Smoking - Relative Risk
# Source: SAPM (Sheffield Alcohol Policy Model)
rr_ra_smoking <- tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.0,
  "current_smoker", 2.02
)

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_ra_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(ra_incidence_per10k)
  inc_dt <- melt(inc_dt, id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90-110'
  dt[, age_group_ra := cut(age, 
                           breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                           labels = c("0-19", "20-29", "30-39", "40-49", "50-59", 
                                     "60-69", "70-79", "80-89", "90-110"),
                           right = FALSE)]
  
  dt[inc_dt, on = .(age_group_ra = age, sex), ra_year_risk := i.incidence / 10000]
  dt[is.na(ra_year_risk), ra_year_risk := 0]
  
  dt[, age_group_ra := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_ra_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("rheumatoid_arthritis" %in% names(dt)) {
    dt <- dt[rheumatoid_arthritis == 0]
  }
  
  # 1. BMI RR
  rr_bmi_dt <- as.data.table(rr_ra_bmi)
  dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_ra_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv]
  
  # PAF grouping
  dt[, age_group_ra := cut(age, 
                           breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                           labels = c("0-19", "20-29", "30-39", "40-49", "50-59", 
                                     "60-69", "70-79", "80-89", "90-110"),
                           right = FALSE)]
  
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_ra, sex)]
  
  inc_dt <- as.data.table(ra_incidence_per10k)
  inc_dt <- melt(inc_dt, id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  min_dt <- merge(as.data.table(inc_dt), paf_dt, 
                  by.x = c("age", "sex"), by.y = c("age_group_ra", "sex"))
  
  min_dt[, ra_prob_min := (incidence / 10000) * (1 - AF)]
  
  return(min_dt[, .(age_group_ra = age, sex, ra_prob_min)])
}

# Function 3: Apply Risk Factors
apply_ra_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR
  rr_bmi_dt <- as.data.table(rr_ra_bmi)
  dt[rr_bmi_dt, on = .(bmi), RR_bmi_indiv := i.RR]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. Smoking RR
  rr_smoking_dt <- as.data.table(rr_ra_smoking)
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv]
  
  # Assign age groups
  dt[, age_group_ra := cut(age, 
                           breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                           labels = c("0-19", "20-29", "30-39", "40-49", "50-59", 
                                     "60-69", "70-79", "80-89", "90-110"),
                           right = FALSE)]
  
  # Join with theoretical minimum
  dt[min_dt, on = .(age_group_ra, sex), ra_prob_min := i.ra_prob_min]
  dt[is.na(ra_prob_min), ra_prob_min := 0]
  
  # Calculate individual risk
  dt[, rheumatoid_arthritis_year_risk := ra_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("RR_bmi_indiv", "RR_smoking_indiv", "RR_combined", "age_group_ra", "ra_prob_min") := NULL]
  
  return(dt)
}

# Unit Tests and Examples ----
store_unit_tests <- function() {
  
  # Example usage with historical population
  # x <- past_populations %>% 
  #     filter(year == min(year))
  
  # Test age/sex only
  # y_agesex <- apply_ra_risk_engine_age_sex(x)
  
  # Calculate theoretical minimum
  # ra_theoretical_min <- calculate_ra_theoretical_min(x)
  
  # Apply risk factors
  # y <- apply_ra_risk_factors(x, ra_theoretical_min)
  
  # Check aggregated risk by age group
  # y %>% 
  #     mutate(age1 = cut(age, 
  #                      breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
  #                      labels = c("0-19", "20-29", "30-39", "40-49", "50-59", 
  #                                "60-69", "70-79", "80-89", "90-110"),
  #                      right = FALSE)) %>%
  #     group_by(age1, sex) %>% 
  #     summarise(n = n(), wt = sum(rheumatoid_arthritis_year_risk)) %>% 
  #     mutate(ra_year_risk_per10k = wt / n * 10000)
}


