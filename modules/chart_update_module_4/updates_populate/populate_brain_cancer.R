# =============================================================================
# Populate Brain Cancer Prevalence
# =============================================================================
# Function to assign Brain Cancer prevalence to a synthetic population
# Based on age × HSCT × sex distribution from NI Cancer Registry (2022)
# Uses 3D IPF to combine marginal distributions
# =============================================================================

library(tidyverse)

cancer_data <- readRDS("outputs/cancer_population_3d_10yr.rds")

#' Populate Brain Cancer prevalence in synthetic population
#' 
#' @param input_population Data frame with columns: PersonID, age, sex, HSCT, year
#' @return input_population with added column: brain_cancer (year of diagnosis or 0)
#' @export
populate_brain_cancer_prevalence <- function(input_population) {
  
  # Validate input
  required_cols <- c("PersonID", "age", "sex", "HSCT", "year")
  missing_cols <- setdiff(required_cols, names(input_population))
  if (length(missing_cols) > 0) {
    stop(sprintf("Input population missing required columns: %s", 
                 paste(missing_cols, collapse = ", ")))
  }
  
  # Extract and transform cancer prevalence data
  cancer_prevalence_data <- cancer_data %>%
    filter(cancer_site == "Brain Cancer") %>%
    select(age, HSCT, sex, prevalence)
  
  cancer_prevalence_data <- cancer_prevalence_data %>% 
    mutate(sex = paste0(sex, 's')) %>% 
    mutate(HSCT = case_when(
      HSCT == 'Belfast HSCT' ~ 'BHSCT',
      HSCT == 'Northern HSCT' ~ 'NHSCT',
      HSCT == 'South Eastern HSCT' ~ 'SEHSCT',
      HSCT == 'Southern HSCT' ~ 'SHSCT',
      HSCT == 'Western HSCT' ~ 'WHSCT',
      TRUE ~ HSCT
    ))
  
  # Check that data exists
  if (nrow(cancer_prevalence_data) == 0) {
    stop("No prevalence data found for Brain Cancer")
  }
  
  # Prepare population data
  pop_clean <- input_population %>%
    mutate(
      # Standardize age groups
      age_group = case_when(
        age < 55 ~ "0-54",
        age >= 55 & age < 65 ~ "55-64",
        age >= 65 & age < 75 ~ "65-74",
        age >= 75 ~ "75+",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_group))
  
  # Calculate prevalence probability for each person
  pop_with_prob <- pop_clean %>%
    left_join(
      cancer_prevalence_data %>%
        rename(age_group = age),
      by = c("age_group", "HSCT", "sex")
    ) # %>%
    # mutate(prevalence = replace_na(prevalence, 0))
  
  # Calculate probability for each person (prevalence / group_size)
  # NO ROUNDING - use fractional prevalence for probabilistic assignment
  pop_with_prob_calc <- pop_with_prob %>%
    group_by(age_group, HSCT, sex) %>%
    mutate(
      group_size = n(),
  # Probability = expected cases / population in this stratum
  cancer_prob = pmin(prevalence / (group_size * model_specification$population$scale_down_factor), 1.0),  # Cap at 100%
      # Random draw for each person
      random_draw = runif(n()),
      # Assign cancer probabilistically
      has_cancer = random_draw < cancer_prob
    ) %>%
    ungroup()
  
  # Assign diagnosis year (sample from past 10 years)
  current_year <- max(input_population$year, na.rm = TRUE)
  lookback_years <- 10
  
  result <- pop_with_prob_calc %>%
    mutate(
      brain_cancer = if_else(
        has_cancer,
        # sample(seq((current_year-1) - lookback_years, current_year-1), 
        #        size = n(), replace = TRUE),
        rep(min(input_population$year, na.rm = TRUE), n()),
        0L
      )
    ) %>%
    select(all_of(names(input_population)), brain_cancer)
  
  # Report summary
  n_cases <- sum(result$brain_cancer > 0)
  message(sprintf("%s: Assigned %d cases (10-year prevalence)", 
                  "Brain Cancer", n_cases))
  
  return(result)
}

# -----------------------------------------------------------------------------
# Example Usage ---------------------------------------------------------------
# -----------------------------------------------------------------------------

if (FALSE) {
  # Example population
  example_pop <- tibble(
    PersonID = 1:10000,
    age = sample(18:90, 10000, replace = TRUE),
    sex = sample(c("Male", "Female"), 10000, replace = TRUE),
    HSCT = sample(c("Belfast HSCT", "Northern HSCT", "South Eastern HSCT",
                    "Southern HSCT", "Western HSCT"), 10000, replace = TRUE),
    year = 2022
  )
  
  # Populate with Brain Cancer (10-year prevalence)
  pop_with_cancer <- populate_brain_cancer_prevalence(example_pop, period = "10-year")
  
  # Summary
  table(pop_with_cancer$brain_cancer > 0)
  
  # By age and sex
  pop_with_cancer %>%
    mutate(age_group = case_when(
      age < 55 ~ "0-54",
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 ~ "75+"
    )) %>%
    group_by(age_group, sex) %>%
    summarise(
      n = n(),
      cases = sum(brain_cancer > 0),
      prevalence = cases / n * 100
    )
}

