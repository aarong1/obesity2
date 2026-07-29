#' =============================================================================
#' BMI INTERVENTION ENGINE
#' =============================================================================
#' 
#' Functions to simulate obesity interventions on a synthetic population.
#' Each function modifies BMI distributions while maintaining data integrity
#' and rank stability.
#' 
#' Population structure requirements:
#' - bmi: categorical ("normal", "overweight", "obese")
#' - bmi_percentile: numeric [0, 1] representing population distribution
#' 
#' @author Population Health Modeling Team
#' @date 2026-01-04
#' =============================================================================

library(dplyr)
library(rlang)

#' Reduce BMI for Individuals Above Target Percentile
#'
#' Shifts BMI percentiles downward for individuals above a threshold, simulating
#' interventions that target high-BMI populations. Maintains rank stability and
#' updates BMI categories when individuals cross thresholds.
#'
#' @param current_population Data frame with columns: bmi, bmi_percentile
#' @param target_percentile Numeric [0, 100]. Individuals above this are affected
#' @param reduction_amount Numeric [0, 1]. How much to reduce percentile (e.g., 0.1 = 10 percentile points)
#' @param affected_group String. dplyr filter expression (e.g., "age >= 40 & sex == 'Female'")
#' @param bmi_thresholds Named vector with percentile thresholds for BMI categories
#' 
#' @return Modified population data frame with updated bmi and bmi_percentile
#' 
#' @examples
#' # Reduce everyone above 75th percentile by 10 points
#' pop <- reduce_to_percentile(pop, target_percentile = 75, reduction_amount = 0.10)
#' 
#' # Target older adults
#' pop <- reduce_to_percentile(pop, target_percentile = 80, reduction_amount = 0.15,
#'                              affected_group = "age >= 50")
#'
#' @export
#' 
reduce_to_percentile <- function(current_population, 
                                 target_percentile = 0.9,
                                 subset = FALSE
) {
  if(subset==TRUE){
  current_population <- current_population %>%
    mutate(
      bmi = case_when(
        bmi_percentile >= target_percentile & bmi=='obese' & target == TRUE ~ "overweight",
        bmi_percentile >= target_percentile & bmi=='overweight' & target == TRUE ~ "normal",
        TRUE ~ bmi  # Keep original if something unexpected
      )
    )
  } else{
    current_population <- current_population %>%
      mutate(
        bmi = case_when(
          bmi_percentile >= target_percentile & bmi=='obese' ~ "overweight",
          bmi_percentile >= target_percentile & bmi=='overweight' ~ "normal",
          TRUE ~ bmi  # Keep original if something unexpected
        )
      )
  }
}


current_population %>% 
  mutate(target = ifelse(diabetes_status == 'undiagnosed_diabetes',T,F )) %>%
  reduce_to_percentile(0.89,subset=T) %>%
  reduce_to_percentile(0.79) %>%
  
  count(bmi)

reduce_to_percentile <- function(current_population, 
                                  target_percentile = 75,
                                  reduction_amount = 0.10,
                                  affected_group = NULL,
                                  bmi_thresholds = c(normal = 0.50, overweight = 0.75, obese = 1.0)) {
  
  # Validate inputs
  if (!all(c("bmi", "bmi_percentile") %in% names(current_population))) {
    stop("Population must contain 'bmi' and 'bmi_percentile' columns")
  }
  
  if (target_percentile < 0 || target_percentile > 100) {
    stop("target_percentile must be between 0 and 100")
  }
  
  if (reduction_amount < 0 || reduction_amount > 1) {
    stop("reduction_amount must be between 0 and 1")
  }
  
  # Convert percentile to [0,1] scale
  target_pct <- target_percentile / 100
  
  # Store original for tracking
  n_original <- nrow(current_population)
  
  # Apply group filter if specified
  if (!is.null(affected_group)) {
    current_population <- current_population %>%
      mutate(.intervention_eligible = eval(parse(text = affected_group)))
  } else {
    current_population <- current_population %>%
      mutate(.intervention_eligible = TRUE)
  }
  
  # Identify individuals above target percentile in eligible group
  current_population <- current_population %>%
    mutate(
      .above_threshold = .intervention_eligible & (bmi_percentile > target_pct),
      .original_percentile = bmi_percentile
    )
  
  # Apply reduction
  current_population <- current_population %>%
    mutate(
      bmi_percentile = if_else(
        .above_threshold,
        pmax(0, bmi_percentile - reduction_amount), # Don't go below 0
        bmi_percentile
      )
    )
  
  # Update BMI categories based on new percentiles
  current_population <- current_population %>%
    mutate(
      bmi = case_when(
        bmi_percentile <= bmi_thresholds["normal"] ~ "normal",
        bmi_percentile <= bmi_thresholds["overweight"] ~ "overweight",
        bmi_percentile <= bmi_thresholds["obese"] ~ "obese",
        TRUE ~ bmi  # Keep original if something unexpected
      )
    )
  
  # Report intervention effects
  n_affected <- sum(current_population$.above_threshold)
  n_category_changes <- sum(current_population$bmi != 
                            case_when(
                              current_population$.original_percentile <= bmi_thresholds["normal"] ~ "normal",
                              current_population$.original_percentile <= bmi_thresholds["overweight"] ~ "overweight",
                              TRUE ~ "obese"
                            ))
  
  message(sprintf("Intervention applied to %d individuals (%.1f%%)", 
                  n_affected, 100 * n_affected / n_original))
  message(sprintf("BMI category changed for %d individuals (%.1f%%)", 
                  n_category_changes, 100 * n_category_changes / n_original))
  
  # Clean up temporary columns
  current_population %>%
    select(-starts_with("."))
}


#' Move Obese Individuals to Overweight Category
#'
#' Simulates weight loss interventions by randomly selecting a proportion of
#' obese individuals and reclassifying them as overweight. Adjusts BMI percentiles
#' to match the new category.
#'
#' @param current_population Data frame with columns: bmi, bmi_percentile
#' @param proportion Numeric [0, 1]. Proportion of obese individuals to move
#' @param affected_group String. dplyr filter expression for targeting
#' @param new_percentile_range Numeric vector length 2. Percentile range for reclassified individuals
#' @param seed Integer. Random seed for reproducibility
#' 
#' @return Modified population data frame
#' 
#' @examples
#' # Move 20% of obese individuals to overweight
#' pop <- reduce_obese_to_overweight(pop, proportion = 0.20)
#' 
#' # Target women aged 50+
#' pop <- reduce_obese_to_overweight(pop, proportion = 0.30, 
#'                                    affected_group = "sex == 'Female' & age >= 50")
#'
#' @export
reduce_obese_to_overweight <- function(current_population,
                                        proportion = 0.20,
                                        affected_group = NULL,
                                        new_percentile_range = c(0.55, 0.70),
                                        seed = NULL) {
  
  # Validate inputs
  if (!all(c("bmi", "bmi_percentile") %in% names(current_population))) {
    stop("Population must contain 'bmi' and 'bmi_percentile' columns")
  }
  
  if (proportion < 0 || proportion > 1) {
    stop("proportion must be between 0 and 1")
  }
  
  if (length(new_percentile_range) != 2 || any(new_percentile_range < 0) || any(new_percentile_range > 1)) {
    stop("new_percentile_range must be a vector of length 2 with values between 0 and 1")
  }
  
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  n_original <- nrow(current_population)
  
  # Identify eligible obese individuals
  if (!is.null(affected_group)) {
    current_population <- current_population %>%
      mutate(.eligible_obese = bmi == "obese" & eval(parse(text = affected_group)))
  } else {
    current_population <- current_population %>%
      mutate(.eligible_obese = bmi == "obese")
  }
  
  n_eligible <- sum(current_population$.eligible_obese)
  
  if (n_eligible == 0) {
    warning("No eligible obese individuals found. No intervention applied.")
    return(current_population %>% select(-starts_with(".")))
  }
  
  # Randomly select proportion of eligible individuals
  current_population <- current_population %>%
    mutate(
      .random_draw = runif(n()),
      .selected = .eligible_obese & (.random_draw <= proportion)
    )
  
  n_selected <- sum(current_population$.selected)
  
  # Assign new percentiles within overweight range
  # Maintain some variation - distribute uniformly within range
  selected_indices <- which(current_population$.selected)
  new_percentiles <- runif(n_selected, 
                          min = new_percentile_range[1], 
                          max = new_percentile_range[2])
  
  # Update BMI category and percentile for selected individuals
  current_population <- current_population %>%
    mutate(
      bmi = if_else(.selected, "overweight", bmi),
      bmi_percentile = if_else(.selected, 
                              new_percentiles[cumsum(.selected)[.selected]],
                              bmi_percentile)
    )
  
  # Report results
  message(sprintf("Moved %d obese individuals to overweight (%.1f%% of %d eligible)", 
                  n_selected, 100 * proportion, n_eligible))
  message(sprintf("Overall population: %.1f%% affected", 
                  100 * n_selected / n_original))
  
  # Clean up
  current_population %>%
    select(-starts_with("."))
}


#' Apply Differential Reduction Rates to Overweight and Obese
#'
#' Implements a two-tier intervention strategy with different reduction rates
#' for overweight and obese individuals. Creates cascading effects where obese
#' can move to overweight and overweight can move to normal.
#'
#' @param current_population Data frame with columns: bmi, bmi_percentile
#' @param overweight_reduction Numeric [0, 1]. Proportion of overweight to move to normal
#' @param obese_reduction Numeric [0, 1]. Proportion of obese to move to overweight
#' @param affected_group String. dplyr filter expression for targeting
#' @param maintain_gradual Boolean. If TRUE, shift percentiles gradually rather than categorical jump
#' @param seed Integer. Random seed for reproducibility
#' 
#' @return Modified population data frame
#' 
#' @examples
#' # Standard intervention: 15% overweight→normal, 25% obese→overweight
#' pop <- reduce_overweight_obese(pop, overweight_reduction = 0.15, obese_reduction = 0.25)
#' 
#' # Intensive intervention for deprived areas
#' pop <- reduce_overweight_obese(pop, overweight_reduction = 0.30, obese_reduction = 0.40,
#'                                 affected_group = "deprivation == 'Most deprived'")
#'
#' @export
reduce_overweight_obese <- function(current_population,
                                     overweight_reduction = 0.15,
                                     obese_reduction = 0.25,
                                     affected_group = NULL,
                                     maintain_gradual = TRUE,
                                     seed = NULL) {
  
  # Validate inputs
  if (!all(c("bmi", "bmi_percentile") %in% names(current_population))) {
    stop("Population must contain 'bmi' and 'bmi_percentile' columns")
  }
  
  if (overweight_reduction < 0 || overweight_reduction > 1) {
    stop("overweight_reduction must be between 0 and 1")
  }
  
  if (obese_reduction < 0 || obese_reduction > 1) {
    stop("obese_reduction must be between 0 and 1")
  }
  
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  n_original <- nrow(current_population)
  
  # Identify eligible individuals by BMI category
  if (!is.null(affected_group)) {
    current_population <- current_population %>%
      mutate(
        .eligible = eval(parse(text = affected_group)),
        .eligible_overweight = bmi == "overweight" & .eligible,
        .eligible_obese = bmi == "obese" & .eligible
      )
  } else {
    current_population <- current_population %>%
      mutate(
        .eligible_overweight = bmi == "overweight",
        .eligible_obese = bmi == "obese"
      )
  }
  
  n_overweight <- sum(current_population$.eligible_overweight)
  n_obese <- sum(current_population$.eligible_obese)
  
  # Random selection for both groups
  current_population <- current_population %>%
    mutate(
      .random_draw = runif(n()),
      .overweight_selected = .eligible_overweight & (.random_draw <= overweight_reduction),
      .obese_selected = .eligible_obese & (.random_draw <= obese_reduction)
    )
  
  n_overweight_moved <- sum(current_population$.overweight_selected)
  n_obese_moved <- sum(current_population$.obese_selected)
  
  if (maintain_gradual) {
    # Gradual approach: shift percentiles by a fixed amount
    # Overweight→Normal: reduce percentile to bring into normal range
    # Obese→Overweight: reduce percentile to bring into overweight range
    
    current_population <- current_population %>%
      mutate(
        bmi_percentile = case_when(
          .overweight_selected ~ pmax(0, bmi_percentile - 0.15),  # Shift down to normal range
          .obese_selected ~ pmax(0.50, bmi_percentile - 0.20),    # Shift down to overweight range
          TRUE ~ bmi_percentile
        )
      )
    
    # Update categories based on new percentiles
    current_population <- current_population %>%
      mutate(
        bmi = case_when(
          bmi_percentile <= 0.50 ~ "normal",
          bmi_percentile <= 0.75 ~ "overweight",
          bmi_percentile > 0.75 ~ "obese",
          TRUE ~ bmi
        )
      )
    
  } else {
    # Categorical approach: assign to middle of target range
    
    current_population <- current_population %>%
      mutate(
        bmi = case_when(
          .overweight_selected ~ "normal",
          .obese_selected ~ "overweight",
          TRUE ~ bmi
        ),
        bmi_percentile = case_when(
          .overweight_selected ~ runif(n(), 0.25, 0.45),  # Middle of normal range
          .obese_selected ~ runif(n(), 0.55, 0.70),       # Middle of overweight range
          TRUE ~ bmi_percentile
        )
      )
  }
  
  # Report results
  message("=== Intervention Results ===")
  message(sprintf("Overweight→Normal: %d individuals (%.1f%% of %d eligible)", 
                  n_overweight_moved, 100 * overweight_reduction, n_overweight))
  message(sprintf("Obese→Overweight: %d individuals (%.1f%% of %d eligible)", 
                  n_obese_moved, 100 * obese_reduction, n_obese))
  message(sprintf("Total affected: %d (%.1f%% of population)", 
                  n_overweight_moved + n_obese_moved,
                  100 * (n_overweight_moved + n_obese_moved) / n_original))
  
  # Distribution summary
  final_dist <- current_population %>%
    count(bmi) %>%
    mutate(pct = 100 * n / n_original)
  
  message("\n=== Final BMI Distribution ===")
  for (i in seq_len(nrow(final_dist))) {
    message(sprintf("%s: %d (%.1f%%)", 
                   final_dist$bmi[i], final_dist$n[i], final_dist$pct[i]))
  }
  
  # Clean up
  current_population %>%
    select(-starts_with("."))
}


#' Validate Intervention Results
#'
#' Checks data integrity after intervention and compares pre/post distributions.
#'
#' @param pre_intervention Original population data frame
#' @param post_intervention Modified population data frame
#' 
#' @return List with validation results and summary statistics
#' 
#' @export
validate_intervention <- function(pre_intervention, post_intervention) {
  
  # Check population size unchanged
  size_check <- nrow(pre_intervention) == nrow(post_intervention)
  
  # Check percentile bounds
  percentile_check <- all(post_intervention$bmi_percentile >= 0 & 
                         post_intervention$bmi_percentile <= 1)
  
  # Check BMI-percentile consistency
  consistency_check <- post_intervention %>%
    mutate(
      expected_bmi = case_when(
        bmi_percentile <= 0.50 ~ "normal",
        bmi_percentile <= 0.75 ~ "overweight",
        TRUE ~ "obese"
      ),
      consistent = bmi == expected_bmi
    ) %>%
    pull(consistent) %>%
    all()
  
  # Distribution comparison
  pre_dist <- pre_intervention %>% count(bmi) %>% mutate(prop = n / sum(n))
  post_dist <- post_intervention %>% count(bmi) %>% mutate(prop = n / sum(n))
  
  dist_comparison <- full_join(
    pre_dist %>% rename(pre_n = n, pre_prop = prop),
    post_dist %>% rename(post_n = n, post_prop = prop),
    by = "bmi"
  ) %>%
    mutate(
      change_n = post_n - pre_n,
      change_pct = 100 * (post_prop - pre_prop)
    )
  
  results <- list(
    all_valid = size_check & percentile_check & consistency_check,
    size_check = size_check,
    percentile_check = percentile_check,
    consistency_check = consistency_check,
    distribution_comparison = dist_comparison
  )
  
  # Print summary
  message("=== Validation Results ===")
  message(sprintf("Population size maintained: %s", ifelse(size_check, "✓", "✗")))
  message(sprintf("Percentiles in valid range: %s", ifelse(percentile_check, "✓", "✗")))
  message(sprintf("BMI-percentile consistency: %s", ifelse(consistency_check, "✓", "✗")))
  message("\n=== Distribution Changes ===")
  print(dist_comparison)
  
  invisible(results)
}

