# General Health Application Function
#
# Applies general_health categories (good_health / not_good_health) to a synthetic
# population using IPFP-estimated joint probabilities from general_health.R.
#
# The function:
# 1. Takes a population with general_health_percentile values
# 2. Maps demographic variables to match the health stratified prevalence format
# 3. Uses percentile-based assignment to assign health status while preserving rank stability
# 4. Returns the population with general_health column added
#
# Dependencies:
# - risk_joint_estimation/general_health.R (for health_stratified_prevalence)
# - Column `general_health_percentile` must exist; generated with runif() if absent
#
# Expected overall distribution (2023/24 NIHS):
# - good_health:     68.7%
# - not_good_health: 31.3%

apply_general_health_indicator_parameter_rank_stability <- function(current_population,
                                                          health_stratified_prevalence) {

  current_population <- current_population %>%
    select(-any_of(c('health', 'general_health')))

  # Generate independent uniform percentile if not already present
  if (!"general_health_percentile" %in% names(current_population)) {
    current_population <- current_population %>%
      mutate(general_health_percentile = runif(n = n()))
  }

  # Process health stratified prevalence to create cumulative probabilities
  health_stratified_prevalence <- health_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(health), .by_group = TRUE) %>%   # not_good_health first, then good_health
    mutate(
      prob_cond = count / sum(count),   # conditional probability within stratum
      prob_cum  = cumsum(prob_cond)     # CDF of health given stratum
    ) %>%
    mutate(list_probs = list(prob_cum)) %>%
    ungroup()

  # Cut age bands to match health data (16-34 combined; 75+ as last group)
  current_population <- current_population %>%
    as_tibble() %>%
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),
      right  = FALSE,
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75+")
    ))

  # Recode deprivation to match health data format
  current_population <- current_population %>%
    mutate(deprivation = mdm_quintile_soa_name)

  # Extract HSCT name to match health data format
  current_population <- current_population %>%
    mutate(hsct = HSCT)

  # Extract geography to match health data format
  current_population <- current_population %>%
    mutate(geo = Urban_mixed_rural_status)

  # Join with health stratified prevalence using percentile rank threshold
  current_population <- current_population %>%
    left_join(health_stratified_prevalence,
              relationship = 'many-to-one',
              multiple     = 'first',
              by = join_by('sex',
                           age10 == age,
                           'deprivation',
                           hsct,
                           'geo',
                           general_health_percentile < prob_cum))

  # Clean up temporary columns
  current_population <- current_population %>%
    select(-c(list_probs, count, prob_cum, prob_cond))

  # Check for missing assignments
  missing_health <- sum(is.na(current_population$health))
  if (missing_health > 0) {
    warning(paste("Warning:", missing_health,
                  "individuals could not be assigned a general health category.",
                  "Check for missing combinations in health_stratified_prevalence."))
  }

  return(current_population)
}
