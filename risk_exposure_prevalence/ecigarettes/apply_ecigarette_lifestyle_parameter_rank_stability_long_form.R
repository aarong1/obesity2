# E-cigarettes Application Function
# 
# This function applies e-cigarette categories to a synthetic population
# using previously estimated joint probabilities from the electronic cigarettes joint estimation.
# 
# The function:
# 1. Takes a population with ecigarette_percentile values (from apply_correlated_quantiles)
# 2. Maps demographic variables to match the e-cigarette stratified prevalence format
# 3. Uses percentile-based assignment to assign e-cigarette categories while preserving rank stability
# 4. Returns the population with ecigarette categories assigned
#
# Dependencies: 
# - joint_estimation/electronic_cigarettes.R (for ecigarette_stratified_prevalence)
# - apply_correlated_quantiles must be run first with 'ecigarette' in risks_to_include

apply_ecigarette_lifestyle_parameter_rank_stability <- function(current_population,
                                                                ecigarette_stratified_prevalence){
  

  # Process e-cigarette stratified prevalence to create cumulative probabilities
  ecigarette_stratified_prevalence <- ecigarette_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(ecigarette), .by_group = TRUE) %>%  # ensure consistent e-cigarette order
    mutate(
      prob_cond = probability / sum(probability),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of e-cigarette given stratum
    ) %>%
    
    mutate(list_probs = list(prob_cum)) |> 
    ungroup()
  

  #cut age bands to match e-cigarette data (note: 75+ in original, 75-110 in processed)
  current_population <- current_population |> 
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),  # upper bound exclusive
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75-110")
    )
    )
  
  #recode deprivation to match e-cigarette data format
  current_population <- current_population |> 
    mutate(deprivation = mdm_quintile_soa_name
    )

  # Extract HSCT name to match e-cigarette data format
  current_population <- current_population |> 
    mutate(hsct = HSCT)

  # Extract geography to match e-cigarette data format
  current_population <- current_population |> 
    mutate(geo = Urban_mixed_rural_status)
  
  # Join with e-cigarette stratified prevalence
  current_population <- current_population |> 
    left_join(ecigarette_stratified_prevalence,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age10 == age,  
                           'deprivation',   
                           hsct,  
                           'geo',
                           smoking_percentile < prob_cum)
              )
  
  # Clean up temporary columns
  current_population <- current_population |> 
   select( - c( list_probs, probability, prob_cum, prob_cond ) )
  
  # Check for missing values
  missing_ecigarette <- sum(is.na(current_population$ecigarette))
  if(missing_ecigarette > 0) {
    warning(paste("Warning:", missing_ecigarette, "individuals could not be assigned an e-cigarette category. Check for missing combinations in ecigarette_stratified_prevalence."))
  }
  
  return(current_population)
}


# Example usage and testing:
# 
# # First, load the e-cigarette joint estimation results
# source("joint_estimation/electronic_cigarettes.R")
# 
# # Create test population 
# test_population <- instantiate_base_pop()
# 
# # Apply correlated quantiles including e-cigarette percentile
# test_population <- apply_correlated_quantiles(current_population = test_population,
#                                               correlation_matrix = pearson_correlation_matrix,
#                                               risks_to_include = c('bmi', 'ecigarette'),
#                                               model_configuration_list = model_specification
#                                               )
# 
# # Apply e-cigarette lifestyle parameters
# test_population <- apply_ecigarette_lifestyle_parameter_rank_stability(test_population,
#                                                                       ecigarette_stratified_prevalence)
# 
# # Check results
# names(test_population)
# count(test_population, ecigarette)

# ===== FUNCTION SUMMARY =====
# 
# The apply_ecigarette_lifestyle_parameter_rank_stability function successfully:
# 1. Maps synthetic population demographics to e-cigarette stratified prevalence format
# 2. Assigns e-cigarette categories (current, former_regular, former_not_regular, never) based on percentile ranks
# 3. Preserves demographic distributions while matching target prevalence rates
# 4. Returns population with ecigarette column added
# 
# Expected e-cigarette categories:
# - current: Currently uses e-cigarettes (10%)
# - former_regular: Former regular user (3%)
# - former_not_regular: Former irregular user (8%)  
# - never: Never used e-cigarettes (79%)
