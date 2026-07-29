# Loneliness Application Function
#
# Applies loneliness categories (lonely / not_lonely) to a synthetic population
# using IPFP-estimated joint probabilities from loneliness.R.
#
# The function:
# 1. Takes a population with loneliness_percentile values
# 2. Maps demographic variables to match the loneliness stratified prevalence format
# 3. Uses percentile-based assignment to assign loneliness status while preserving rank stability
# 4. Returns the population with loneliness column added
#
# Dependencies:
# - risk_joint_estimation/loneliness.R (for loneliness_stratified_prevalence)
# - Column `loneliness_percentile` must exist; generated with runif() if absent
#
# Expected overall distribution (2023/24 NIHS):
# - lonely:     24.6%
# - not_lonely: 75.4%

apply_loneliness_indicator_parameter_rank_stability <- function(current_population,
                                                      loneliness_stratified_prevalence) {

  current_population <- current_population %>%
    select(-any_of(c('loneliness')))

  # Generate independent uniform percentile if not already present
  if (!"loneliness_percentile" %in% names(current_population)) {
    current_population <- current_population %>%
      mutate(loneliness_percentile = runif(n = n()))
  }

  # Process loneliness stratified prevalence to create cumulative probabilities
  loneliness_stratified_prevalence <- loneliness_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(loneliness), .by_group = TRUE) %>%   # not_lonely first, then lonely
    mutate(
      prob_cond = count / sum(count),   # conditional probability within stratum
      prob_cum  = cumsum(prob_cond)     # CDF of loneliness given stratum
    ) %>%
    mutate(list_probs = list(prob_cum)) %>%
    ungroup()

  # Cut age bands to match loneliness data (16-34 combined; 75+ as last group)
  current_population <- current_population %>%
    as_tibble() %>%
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),
      right  = FALSE,
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75+")
    ))

  # Recode deprivation to match loneliness data format
  current_population <- current_population %>%
    mutate(deprivation = mdm_quintile_soa_name)

  # Extract HSCT name to match loneliness data format
  current_population <- current_population %>%
    mutate(hsct = HSCT)

  # Extract geography to match loneliness data format
  current_population <- current_population %>%
    mutate(geo = Urban_mixed_rural_status)

  # Join with loneliness stratified prevalence using percentile rank threshold
  current_population <- current_population %>%
    left_join(loneliness_stratified_prevalence,
              relationship = 'many-to-one',
              multiple     = 'first',
              by = join_by('sex',
                           age10 == age,
                           'deprivation',
                           hsct,
                           'geo',
                           loneliness_percentile < prob_cum))

  # Clean up temporary columns
  current_population <- current_population %>%
    select(-c(list_probs, count, prob_cum, prob_cond))

  # Check for missing assignments
  missing_loneliness <- sum(is.na(current_population$loneliness))
  if (missing_loneliness > 0) {
    warning(paste("Warning:", missing_loneliness,
                  "individuals could not be assigned a loneliness category.",
                  "Check for missing combinations in loneliness_stratified_prevalence."))
  }

  return(current_population)
}
