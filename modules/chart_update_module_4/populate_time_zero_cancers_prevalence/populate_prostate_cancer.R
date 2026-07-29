# =============================================================================
# Populate Prostate Cancer Prevalence
# =============================================================================
# Function to assign Prostate Cancer prevalence to a synthetic population
# Based on age × HSCT distribution from NI Cancer Registry (2022, 10-year prevalence)
# =============================================================================

library(tidyverse)

#' Populate Prostate Cancer prevalence in synthetic population
#' 
#' @param input_population Data frame with columns: PersonID, age, sex, HSCT, year
#' @param cancer_prevalence_data Optional: preloaded prevalence data (age × HSCT distribution)
#' @return input_population with added column: prostate_cancer (year of diagnosis or 0)
#' 
#' @details
#' Total 10-year prevalence (All persons): 9,093 cases
#' 
#' Age distribution:
#' - 0-54: 169 cases
#' - 55-64: 1409 cases
#' - 65-74: 3605 cases
#' - 75+: 3910 cases
#' 
#' Uses percentile ranking approach:
#' 1. Calculate cancer probability = total_prevalence / population_size
#' 2. Rank individuals by pre-existing risk score
#' 3. Assign cancer to top X% based on probability
#' 
#' @examples
#' pop <- populate_prostate_cancer(initial_population)
#' table(pop$prostate_cancer > 0)  # Count of cases

if (!file.exists("outputs/cancer_population_all_10yr.rds")) {
  stop("Cancer prevalence data not found. Run populate_all_site_cancers.R first.")
}
cancer_data <- readRDS("outputs/cancer_population_all_10yr.rds")

populate_prostate_cancer <- function(input_population) {
  
  # Validate inputs
  required_cols <- c("id", "age", "sex", "HSCT", "year")
  missing_cols <- setdiff(required_cols, names(input_population))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  year <- max(input_population$year)
  
  # Load prevalence data if not provided
  
    cancer_data <- cancer_data %>% 
      mutate(HSCT = case_when(
               HSCT == 'Belfast HSCT' ~ 'BHSCT',
               HSCT == 'Northern HSCT' ~ 'NHSCT',
               HSCT == 'South Eastern HSCT' ~ 'SEHSCT',
               HSCT == 'Southern HSCT' ~ 'SHSCT',
               HSCT == 'Western HSCT' ~ 'WHSCT',
               TRUE ~ HSCT
             ))
    cancer_prevalence_data <- cancer_data %>%
      filter(cancer_site == "Prostate Cancer") %>%
      select(age, HSCT, prevalence)
  
  
  # Calculate total prevalence and probability
  total_prevalence <- sum(cancer_prevalence_data$prevalence) 
  population_size <- nrow(input_population)*model_specification$population$scale_down_factor
  cancer_prob <- total_prevalence / population_size
  
  message(sprintf("Populating Prostate Cancer:"))
  message(sprintf("  Total prevalence: %d cases", total_prevalence))
  message(sprintf("  Population size: %d", population_size))
  message(sprintf("  Probability: %.4f", cancer_prob))
  
  # Create age groups matching prevalence data
  input_population <- input_population %>%
    mutate(
      age_group = cut(age,
                      breaks = c(0, 55, 65, 75, 110),
                      labels = c("0-54", "55-64", "65-74", "75+"),
                      include.lowest = TRUE,
                      right = FALSE)
    )
  
  # Assign percentile based on existing risk score or create random ranking
  # If you have a cancer-specific risk score, use that instead of random
  if (!"prostate_cancer_year_risk" %in% names(input_population)) {
    # Create random risk score as placeholder
    set.seed(123)  # For reproducibility
    stop(' prostate_cancer_year_risk not found')
    input_population$prostate_cancer_year_risk <- runif(nrow(input_population))
  }
  
  input_population$prostate_cancer_percentile <- 
    rank(input_population$prostate_cancer_year_risk, ties.method = "random") /
    max(rank(input_population$prostate_cancer_year_risk, ties.method = "random"))
  
  # Assign cancer to top percentile
  input_population <- input_population %>%
    mutate(prostate_cancer = ifelse(prostate_cancer_percentile < cancer_prob, year, 0))
  
  # Report results
  n_cases <- sum(input_population$prostate_cancer > 0)
  message(sprintf("  Assigned cases: %d (target: %d)", n_cases, total_prevalence))
  
  # Optional: Validate distribution against age × HSCT targets
  if (F){#(n_cases > 0) {
    actual_dist <- input_population %>%
      filter(prostate_cancer > 0) %>%
      count(age_group, HSCT, name = "actual")

    target_dist <- cancer_prevalence_data %>%
      rename(age_group = age, target = prevalence)

    comparison <- actual_dist %>%
      full_join(target_dist, by = c("age_group", "HSCT")) %>%
      replace_na(list(actual = 0, target = 0))

    # Calculate fit (correlation)
    if (nrow(comparison) > 0 && sd(comparison$target) > 0) {
      fit_cor <- cor(comparison$actual, comparison$target)
      message(sprintf("  Distribution fit (correlation): %.3f", fit_cor))
    }
  }
  
  return(input_population)
}

# -----------------------------------------------------------------------------
# Prevalence Data Reference ---------------------------------------------------
# -----------------------------------------------------------------------------

# Age × HSCT distribution (All persons, 10-year prevalence, 2022)
# Source: NI Cancer Registry
#
# A tibble: 20 × 3
#    age   HSCT               prevalence
#    <ord> <ord>                   <dbl>
#  1 0-54  Belfast HSCT               26
#  2 0-54  Northern HSCT              49
#  3 0-54  South Eastern HSCT         36
#  4 0-54  Southern HSCT              28
#  5 0-54  Western HSCT               30
#  6 55-64 Belfast HSCT              218
#  7 55-64 Northern HSCT             409
#  8 55-64 South Eastern HSCT        300
#  9 55-64 Southern HSCT             233
# 10 55-64 Western HSCT              249
# 11 65-74 Belfast HSCT              559
# 12 65-74 Northern HSCT            1045
# 13 65-74 South Eastern HSCT        768
# 14 65-74 Southern HSCT             596
# 15 65-74 Western HSCT              637
# 16 75+   Belfast HSCT              606
# 17 75+   Northern HSCT            1134
# 18 75+   South Eastern HSCT        833
# 19 75+   Southern HSCT             646
# 20 75+   Western HSCT              691

