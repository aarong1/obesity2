# Long-Term Disability Application Function
#
# Applies long-term disability categories (disabled / not_disabled) to a synthetic
# population using IPFP-estimated joint probabilities from long_term_disability.R.
#
# The function:
# 1. Takes a population with disability_percentile values
# 2. Maps demographic variables to match the disability stratified prevalence format
# 3. Uses percentile-based assignment to assign disability status while preserving rank stability
# 4. Returns the population with disability column added
#
# Dependencies:
# - risk_joint_estimation/long_term_disability.R (for disability_stratified_prevalence)
# - Column `disability_percentile` must exist; generated with runif() if absent
#
# Expected overall distribution (2022/23-2023/24 NIHS):
# - disabled:     45.7%
# - not_disabled: 54.3%

apply_long_term_indicator_parameter_rank_stability <- function(current_population,
                                                     disability_stratified_prevalence) {

  current_population <- current_population %>%
    select(-any_of(c('disability')))

  # Generate independent uniform percentile if not already present
  if (!"disability_percentile" %in% names(current_population)) {
    current_population <- current_population %>%
      mutate(disability_percentile = runif(n = n()))
  }

  # Process disability stratified prevalence to create cumulative probabilities
  disability_stratified_prevalence <- disability_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(disability), .by_group = TRUE) %>%   # not_disabled first, then disabled
    mutate(
      prob_cond = count / sum(count),   # conditional probability within stratum
      prob_cum  = cumsum(prob_cond)     # CDF of disability given stratum
    ) %>%
    mutate(list_probs = list(prob_cum)) %>%
    ungroup()

  # Cut age bands to match disability data (16-34 combined; 75+ as last group)
  current_population <- current_population %>%
    as_tibble() %>%
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),
      right  = FALSE,
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75+")
    ))

  # Recode deprivation to match disability data format
  current_population <- current_population %>%
    mutate(deprivation = mdm_quintile_soa_name)

  # Extract HSCT name to match disability data format
  current_population <- current_population %>%
    mutate(hsct = HSCT)

  # Extract geography to match disability data format
  current_population <- current_population %>%
    mutate(geo = Urban_mixed_rural_status)

  # Join with disability stratified prevalence using percentile rank threshold
  current_population <- current_population %>%
    left_join(disability_stratified_prevalence,
              relationship = 'many-to-one',
              multiple     = 'first',
              by = join_by('sex',
                           age10 == age,
                           'deprivation',
                           hsct,
                           'geo',
                           disability_percentile < prob_cum))

  # Clean up temporary columns
  current_population <- current_population %>%
    select(-c(list_probs, prob_cum, count, prob_cond))

  # Check for missing assignments
  missing_disability <- sum(is.na(current_population$disability))
  if (missing_disability > 0) {
    warning(paste("Warning:", missing_disability,
                  "individuals could not be assigned a disability category.",
                  "Check for missing combinations in disability_stratified_prevalence."))
  }

  return(current_population)
}
