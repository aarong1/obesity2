# Limiting Long-Term Disability Application Function
#
# Applies limiting disability categories (limiting / not_limiting) to a synthetic
# population using IPFP-estimated joint probabilities from limiting_long_term_disability.R.
#
# The function:
# 1. Takes a population with limiting_percentile values
# 2. Maps demographic variables to match the limiting stratified prevalence format
# 3. Uses percentile-based assignment to assign limiting disability while preserving rank stability
# 4. Returns the population with limiting column added
#
# Dependencies:
# - risk_joint_estimation/limiting_long_term_disability.R (for limiting_stratified_prevalence)
# - Column `limiting_percentile` must exist; generated with runif() if absent
#
# Expected overall distribution (2022/23-2023/24 NIHS):
# - limiting:     34.0%
# - not_limiting: 66.0%

apply_long_term_limiting_indicator_parameter_rank_stability <- function(current_population,
                                                              limiting_stratified_prevalence) {

  current_population <- current_population %>%
    select(-any_of(c('limiting')))

  # Generate independent uniform percentile if not already present
  if (!"limiting_percentile" %in% names(current_population)) {
    current_population <- current_population %>%
      mutate(limiting_percentile = runif(n = n()))
  }

  # Process limiting stratified prevalence to create cumulative probabilities
  limiting_stratified_prevalence <- limiting_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(limiting), .by_group = TRUE) %>%   # not_limiting first, then limiting
    mutate(
      prob_cond = count / sum(count),   # conditional probability within stratum
      prob_cum  = cumsum(prob_cond)     # CDF of limiting given stratum
    ) %>%
    mutate(list_probs = list(prob_cum)) %>%
    ungroup()

  # Cut age bands to match limiting data (16-34 combined; 75+ as last group)
  current_population <- current_population %>%
    as_tibble() %>%
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),
      right  = FALSE,
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75+")
    ))

  # Recode deprivation to match limiting data format
  current_population <- current_population %>%
    mutate(deprivation = mdm_quintile_soa_name)

  # Extract HSCT name to match limiting data format
  current_population <- current_population %>%
    mutate(hsct = HSCT)

  # Extract geography to match limiting data format
  current_population <- current_population %>%
    mutate(geo = Urban_mixed_rural_status)

  # Join with limiting stratified prevalence using percentile rank threshold
  current_population <- current_population %>%
    left_join(limiting_stratified_prevalence,
              relationship = 'many-to-one',
              multiple     = 'first',
              by = join_by('sex',
                           age10 == age,
                           'deprivation',
                           hsct,
                           'geo',
                           limiting_percentile < prob_cum))

  # Clean up temporary columns
  current_population <- current_population %>%
    select(-c(list_probs, prob_cum, count, prob_cond))

  # Check for missing assignments
  missing_limiting <- sum(is.na(current_population$limiting))
  if (missing_limiting > 0) {
    warning(paste("Warning:", missing_limiting,
                  "individuals could not be assigned a limiting disability category.",
                  "Check for missing combinations in limiting_stratified_prevalence."))
  }

  return(current_population)
}
