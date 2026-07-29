# ============================================================================
# ORIGINAL RAW DATA (preserved for reference)
# ============================================================================

# Prostate Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# cancer_site,age_group,all_rate,male_rate,female_rate
# Prostate Cancer,0 to 44,0.4,0.4,NA
# Prostate Cancer,45 to 49,17.3,17.3,NA
# Prostate Cancer,50 to 54,57.1,57.1,NA
# Prostate Cancer,55 to 59,172.60000000000002,172.60000000000002,NA
# Prostate Cancer,60 to 64,329.3,329.3,NA
# Prostate Cancer,65 to 69,602.5,602.5,NA
# Prostate Cancer,70 to 74,662.9000000000001,662.9000000000001,NA
# Prostate Cancer,75 to 79,808.4000000000001,808.4000000000001,NA
# Prostate Cancer,80 to 84,677.4000000000001,677.4000000000001,NA
# Prostate Cancer,85 to 89,758.8000000000001,758.8000000000001,NA
# Prostate Cancer,90 and over,813.9000000000001,813.9000000000001,NA

# ============================================================================
# PROSTATE CANCER RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Prostate Cancer Incidence Data (per 100,000)
# Source: Cancer Registry data
# Note: Prostate cancer affects males only
prostate_incidence_per100k <- tribble(
  ~age, ~Males,
  "0 to 44", 0.4,
  "45 to 49", 17.3,
  "50 to 54", 57.1,
  "55 to 59", 172.6,
  "60 to 64", 329.3,
  "65 to 69", 602.5,
  "70 to 74", 662.9,
  "75 to 79", 808.4,
  "80 to 84", 677.4,
  "85 to 89", 758.8,
  "90 and over", 813.9
)

# Functions ----

# Function: Apply risk based on age and sex alone
apply_prostate_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for merging
  inc_dt <- as.data.table(prostate_incidence_per100k) %>%
    mutate(Females = 0) %>%  # Females have zero risk
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0 to 44', '45 to 49', '50 to 54', '55 to 59', '60 to 64', 
  #             '65 to 69', '70 to 74', '75 to 79', '80 to 84', '85 to 89', 
  #             '90 and over'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf),
                            labels = c("0 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                                      "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                                      "90 and over"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), prostate_cancer_year_risk := i.incidence / 100000]
  dt[is.na(prostate_cancer_year_risk), prostate_cancer_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Testing ----
# Example usage:
# result <- apply_prostate_risk_engine_age_sex(current_population)