# =============================================================================
# Populate Lung Cancer Prevalence
# =============================================================================
# Function to assign Lung Cancer prevalence to a synthetic population
# Based on age × HSCT × sex distribution from NI Cancer Registry (2022)
# Uses 3D IPF to combine marginal distributions
# =============================================================================

library(tidyverse)

#' Populate Lung Cancer prevalence in synthetic population
#' 
#' @param input_population Data frame with columns: PersonID, age, sex, HSCT, year
#' @param period "10-year" or "25-year" prevalence (default: "10-year")
#' @param cancer_prevalence_data Optional: preloaded prevalence data (age × HSCT × sex distribution)
#' @return input_population with added column: lung_cancer (year of diagnosis or 0)
#' @export
populate_lung_cancer_prevalence <- function(input_population, 
                                    period = "10-year",
                                    cancer_prevalence_data = NULL) {
  # input_population <- base_population
  # Validate input
  required_cols <- c("id", "age", "sex", "HSCT", "year")
  missing_cols <- setdiff(required_cols, names(input_population))
  if (length(missing_cols) > 0) {
    stop(sprintf("Input population missing required columns: %s", 
                 paste(missing_cols, collapse = ", ")))
  }
  
  # Load prevalence data if not provided
  if (is.null(cancer_prevalence_data)) {
    if (period == "10-year") {
      if (!file.exists("outputs/cancer_population_3d_10yr.rds")) {
        stop("Cancer prevalence data file not found. Run populate_all_site_cancers_3d.R first.")
      }
      cancer_data <- readRDS("outputs/cancer_population_3d_10yr.rds")
      
    } else if (period == "25-year") {
      if (!file.exists("outputs/cancer_population_3d_25yr.rds")) {
        stop("Cancer prevalence data file not found. Run populate_all_site_cancers_3d.R first.")
      }
      cancer_data <- readRDS("outputs/cancer_population_3d_25yr.rds")
    } else {
      stop("period must be \"10-year\" or \"25-year\"")
    }
    
    cancer_data <- cancer_data %>% 
      mutate(sex = paste0(sex,'s')) %>% 
      mutate(HSCT = case_when(
        HSCT == 'Belfast HSCT' ~ 'BHSCT',
        HSCT == 'Northern HSCT' ~ 'NHSCT',
        HSCT == 'South Eastern HSCT' ~ 'SEHSCT',
        HSCT == 'Southern HSCT' ~ 'SHSCT',
        HSCT == 'Western HSCT' ~ 'WHSCT',
        TRUE ~ HSCT
      ))
    
    cancer_prevalence_data <- cancer_data %>%
      filter(cancer_site == "Lung Cancer") %>%
      select(age, HSCT, sex, prevalence)
    sum(cancer_prevalence_data$prevalence)
  }
  
  # Check that data exists
  if (nrow(cancer_prevalence_data) == 0) {
    stop("No prevalence data found for Lung Cancer")
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
    ) #%>%
   # filter(!is.na(age_group))
  
  # Calculate prevalence probability for each person
  pop_with_prob <- pop_clean %>%
    select(-any_of('prevalence')) %>% 
    left_join(
      cancer_prevalence_data, #%>%
        # rename(age_group = age, hsct_std = HSCT, sex_std = sex),
      by = c(age_group = 'age', "HSCT", "sex")
    ) %>%
    replace_na(list(prevalence= 0))
  
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
  
  # Assign diagnosis year (sample from past 10 or 25 years)
  current_year <- max(input_population$year, na.rm = TRUE)
  lookback_years <- ifelse(period == "10-year", 10, 25)
  
  result <- pop_with_prob_calc %>%
    mutate(
      lung_cancer = if_else(
        has_cancer,
        # sample(seq((current_year) - lookback_years, current_year), 
        #        size = n(), replace = TRUE),
        rep(min(input_population$year, na.rm = TRUE), n()),
        0L
      )
    ) %>%
    select(all_of(names(input_population)), lung_cancer)
  
  # Report summary
  n_cases <- sum(result$lung_cancer > 0)
  message(sprintf("%s: Assigned %d cases (%s prevalence)", 
                  "Lung Cancer", n_cases, period))
  
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
  
  # Populate with Lung Cancer (10-year prevalence)
  pop_with_cancer <- populate_lung_cancer_prevalence(example_pop, period = "10-year")
  
  # Summary
  table(pop_with_cancer$lung_cancer > 0)
  
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
      cases = sum(lung_cancer > 0),
      prevalence = cases / n * 100
    )
}



populate_lung_cancer_prevalence <- function(input_population) {
  


      cancer_data <- readRDS("outputs/cancer_population_3d_10yr.rds")

    cancer_data <- cancer_data %>% 
      mutate(sex = paste0(sex,'s')) %>% 
      mutate(HSCT = case_when(
        HSCT == 'Belfast HSCT' ~ 'BHSCT',
        HSCT == 'Northern HSCT' ~ 'NHSCT',
        HSCT == 'South Eastern HSCT' ~ 'SEHSCT',
        HSCT == 'Southern HSCT' ~ 'SHSCT',
        HSCT == 'Western HSCT' ~ 'WHSCT',
        TRUE ~ HSCT
      ))
    
    cancer_prevalence_data <- cancer_data %>%
      filter(cancer_site == "Lung Cancer") %>%
      select(age, HSCT, sex, prevalence)
    sum(cancer_prevalence_data$prevalence)
  
  

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
    ) #%>%
  # filter(!is.na(age_group))
  
  # Calculate prevalence probability for each person
  pop_with_prob <- pop_clean %>%
    select(-any_of('prevalence')) %>% 
    left_join(
      cancer_prevalence_data, #%>%
      # rename(age_group = age, hsct_std = HSCT, sex_std = sex),
      by = c(age_group = 'age', "HSCT", "sex")
    ) %>%
    replace_na(list(prevalence= 0))
  
  # Calculate probability for each person (prevalence / group_size)
  # NO ROUNDING - use fractional prevalence for probabilistic assignment
  
  current_year <- max(input_population$year, na.rm = TRUE)
  
  result <- pop_with_prob_calc <- pop_with_prob %>%
    group_by(age_group, HSCT, sex) %>%
    mutate(
      group_size = n(),
      # Probability = expected cases / population in this stratum
      cancer_prob = pmin(prevalence / (group_size * model_specification$population$scale_down_factor), 1.0),  # Cap at 100%
      # Random draw for each person
      random_draw = runif(n()),
      # Assign cancer probabilistically
      lung_cancer = current_year*(random_draw < cancer_prob)
    ) %>%
    ungroup()
  
  # Assign diagnosis year (sample from past 10 or 25 years)
  
  result <- result %>%
    select(all_of(names(input_population)), lung_cancer)
  
  # Report summary
  n_cases <- sum(result$lung_cancer > 0)
  message(sprintf("%s: Assigned %d cases (%s prevalence)", 
                  "Lung Cancer", n_cases, period))
  
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
  
  # Populate with Lung Cancer (10-year prevalence)
  pop_with_cancer <- populate_lung_cancer_prevalence(example_pop, period = "10-year")
  
  # Summary
  table(pop_with_cancer$lung_cancer > 0)
  
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
      cases = sum(lung_cancer > 0),
      prevalence = cases / n * 100
    )
}