# =============================================================================
# Populate Male Breast Cancer Prevalence
# =============================================================================
# Function to assign Male Breast Cancer prevalence to a synthetic population
# Based on age × HSCT distribution from NI Cancer Registry (2022, 10-year prevalence)
# =============================================================================

library(tidyverse)

#' Populate Male Breast Cancer prevalence in synthetic population
#' 
#' @param input_population Data frame with columns: PersonID, age, sex, HSCT, year
#' @param cancer_prevalence_data Optional: preloaded prevalence data (age × HSCT distribution)
#' @return input_population with added column: male_breast_cancer (year of diagnosis or 0)
#' 
#' @details
#' Total 10-year prevalence (All persons): 65 cases
#' 
#' Age distribution:
#' - 0-54: 9 cases
#' - 55-64: 14 cases
#' - 65-74: 11 cases
#' - 75+: 31 cases
#' 
#' Uses percentile ranking approach:
#' 1. Calculate cancer probability = total_prevalence / population_size
#' 2. Rank individuals by pre-existing risk score
#' 3. Assign cancer to top X% based on probability
#' 
#' @examples
#' pop <- populate_male_breast_cancer(initial_population)
#' table(pop$male_breast_cancer > 0)  # Count of cases
populate_male_breast_cancer <- function(input_population, 
                                    cancer_prevalence_data = NULL) {
  
  # Validate inputs
  required_cols <- c("PersonID", "age", "sex", "HSCT", "year")
  missing_cols <- setdiff(required_cols, names(input_population))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  year <- max(input_population$year)
  
  # Load prevalence data if not provided
  if (is.null(cancer_prevalence_data)) {
    if (!file.exists("outputs/cancer_population_all_10yr.rds")) {
      stop("Cancer prevalence data not found. Run populate_all_site_cancers.R first.")
    }
    cancer_data <- readRDS("outputs/cancer_population_all_10yr.rds")
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
      filter(cancer_site == "Male Breast Cancer") %>%
      select(age, HSCT, prevalence)
  }
  
  # Calculate total prevalence and probability
  total_prevalence <- sum(cancer_prevalence_data$prevalence)
  population_size <- nrow(input_population)
  cancer_prob <- total_prevalence / population_size
  
  message(sprintf("Populating Male Breast Cancer:"))
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
  if (!"male_breast_cancer_year_risk" %in% names(input_population)) {
    # Create random risk score as placeholder
    set.seed(123)  # For reproducibility
    input_population$male_breast_cancer_year_risk <- runif(nrow(input_population))
  }
  
  input_population$male_breast_cancer_percentile <- 
    rank(input_population$male_breast_cancer_year_risk, ties.method = "random") /
    max(rank(input_population$male_breast_cancer_year_risk, ties.method = "random"))
  
  # Assign cancer to top percentile
  input_population <- input_population %>%
    mutate(male_breast_cancer = ifelse(male_breast_cancer_percentile < cancer_prob, year, 0))
  
  # Report results
  n_cases <- sum(input_population$male_breast_cancer > 0)
  message(sprintf("  Assigned cases: %d (target: %d)", n_cases, total_prevalence))
  
  # Optional: Validate distribution against age × HSCT targets
  if (n_cases > 0) {
    actual_dist <- input_population %>%
      filter(male_breast_cancer > 0) %>%
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
#  1 0-54  Belfast HSCT                1
#  2 0-54  Northern HSCT               3
#  3 0-54  South Eastern HSCT          3
#  4 0-54  Southern HSCT               1
#  5 0-54  Western HSCT                1
#  6 55-64 Belfast HSCT                4
#  7 55-64 Northern HSCT               4
#  8 55-64 South Eastern HSCT          3
#  9 55-64 Southern HSCT               2
# 10 55-64 Western HSCT                1
# 11 65-74 Belfast HSCT                3
# 12 65-74 Northern HSCT               3
# 13 65-74 South Eastern HSCT          1
# 14 65-74 Southern HSCT               2
# 15 65-74 Western HSCT                2
# 16 75+   Belfast HSCT                8
# 17 75+   Northern HSCT              10
# 18 75+   South Eastern HSCT          7
# 19 75+   Southern HSCT               5
# 20 75+   Western HSCT                1

