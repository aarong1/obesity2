# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# Hypertension Incidence Data (per 100,000)
# Source: UKHF Technical Appendix
# https://static-content.springer.com/esm/art%3A10.1038%2Fs41366-021-00849-8/MediaObjects/41366_2021_849_MOESM1_ESM.pdf
#
# Age, Male, Female
# 0-17, 0.9, 3.8
# 18-29, 98.1, 9.0
# 30-39, 62.2, 73.7
# 40-49, 140.4, 114.3
# 50-59, 274.2, 360.0
# 60-110, 10.3, 42.7

# ============================================================================
# HYPERTENSION RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Hypertension Incidence Data (per 100,000)
# Source: UKHF Technical Appendix
hypertension_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0 to 17", 0.9, 3.8,
  "18 to 29", 98.1, 9.0,
  "30 to 39", 62.2, 73.7,
  "40 to 49", 140.4, 114.3,
  "50 to 59", 274.2, 360.0,
  "60 and over", 10.3, 42.7
)

# Convert incidence to long format for easy merging
inc_dt <- as.data.table(hypertension_incidence_per100k) %>%
  melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Functions ----

# Function: Apply risk based on age and sex alone
apply_hypertension_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Age groups: '0 to 17', '18 to 29', '30 to 39', '40 to 49', '50 to 59', '60 and over'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 18, 30, 40, 50, 60, Inf),
                            labels = c("0 to 17", "18 to 29", "30 to 39", "40 to 49", 
                                      "50 to 59", "60 and over"),
                            right = FALSE)]
  
  inc_dt_temp <- as.data.table(hypertension_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  dt[inc_dt_temp, on = .(age_group_inc = age, sex), hypertension_year_risk := i.incidence / 100000]
  dt[is.na(hypertension_year_risk), hypertension_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Testing ----
# Example usage:
# result <- apply_hypertension_risk_engine_age_sex(current_population)
