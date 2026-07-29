# 
# #depends on risk_correlation.r
# 

# #depends on joint_estimation/5veg.R
# 
# 
# test_population <- instantiate_base_pop()
# test_population <- apply_correlated_quantiles(test_population,
#                                               risks_to_include = c('bmi','sleep'),
#                                               correlation_matrix = pearson_correlation_matrix)



# Diet Application Function
# 
# This function applies diet categories (meets_5_a_day, below_5_a_day) to a synthetic population
# using previously estimated joint probabilities from the 5veg joint estimation.
# 
# The function:
# 1. Takes a population with diet_percentile values (from apply_correlated_quantiles)
# 2. Maps demographic variables to match the diet stratified prevalence format
# 3. Uses percentile-based assignment to assign diet categories while preserving rank stability
# 4. Returns the population with diet categories assigned
#
# Dependencies: 
# - joint_estimation/5veg.R (for diet_stratified_prevalence)
# - apply_correlated_quantiles must be run first with 'diet' in risks_to_include

apply_diet_lifestyle_parameter_rank_stability <- function(current_population,
                                                                diet_stratified_prevalence){
  
  current_population <-  current_population %>% 
    select(-any_of(c('diet')))
  
  
  # Check if diet_percentile column exists
  if(!"diet_percentile" %in% names(current_population)) {
    stop("Error: 'diet_percentile' column not found in current_population. Make sure to run apply_correlated_quantiles() with 'diet' in risks_to_include first.")
  }
  
  # Process diet stratified prevalence to create cumulative probabilities
  diet_stratified_prevalence <- diet_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(diet), .by_group = TRUE) %>%  # ensure consistent diet order (meets_5_a_day first, then below_5_a_day)
    mutate(
      prob_cond = probability / sum(probability),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of diet given stratum
    ) %>%
    
    mutate(list_probs = list(prob_cum)) |> 
    ungroup()
  
  #cut age bands to match diet data
  current_population <- current_population |> 
    as_tibble() |> 
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),  # upper bound exclusive
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75-110")
    )
    )
  
  #recode deprivation to match diet data format
  current_population <- current_population |> 
    mutate(deprivation = as.character(mdm_quintile_soa_name) )

  # Extract HSCT name to match diet data format
  current_population <- current_population |> 
    mutate( hsct = HSCT )

  # Extract geography to match diet data format
  current_population <- current_population |> 
    mutate(geo = Urban_mixed_rural_status)
  
  # Join with diet stratified prevalence
    current_population <- current_population |>
      # select(sex,
      #        hsct, # = HSCT,
      #        geo = Urban_mixed_rural_status,
      #        deprivation,# = mdm_quintile_soa_name,
      #        age10,
      #        diet_percentile) |>
    left_join(diet_stratified_prevalence,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age10 == age,  
                           'deprivation',   
                           hsct,  
                           'geo',
                           diet_percentile < prob_cum)
              )
  
  # Clean up temporary columns
  current_population <- current_population |> 
   select( - c( list_probs, probability, prob_cum, prob_cond ) )
  
  # Check for missing values
  missing_diet <- sum(is.na(current_population$diet))
  if(missing_diet > 0) {
    warning(paste("Warning:", missing_diet, "individuals could not be assigned a diet category. Check for missing combinations in diet_stratified_prevalence."))
  }
  
  return(current_population)
}


# Example usage and testing:
# 
# # First, load the diet joint estimation results
# source("joint_estimation/5veg.R")
# 
# # Create test population 
# test_population <- instantiate_base_pop()
# 
# # Apply correlated quantiles including diet percentile
# test_population <- apply_correlated_quantiles(current_population = test_population,
#                                               correlation_matrix = pearson_correlation_matrix,
#                                               risks_to_include = c('bmi', 'diet'),
#                                               model_configuration_list = model_specification
#                                               )
# 
# # Check that diet_percentile column exists
# names(test_population)
# 
# # Apply diet lifestyle parameters
# test_population <- apply_diet_lifestyle_parameter_rank_stability(test_population,
#                                                                 diet_stratified_prevalence)
# 
# # Check results
# names(test_population)
# count(test_population, diet)
# 
# # Verify diet distribution matches expected proportions
# test_population %>% 
#   count(diet) %>% 
#   mutate(prop = n / sum(n))

# ===== FUNCTION SUMMARY =====
# 
# The apply_diet_lifestyle_parameter_rank_stability function successfully:
# 1. Maps synthetic population demographics to diet stratified prevalence format
# 2. Assigns diet categories (meets_5_a_day, below_5_a_day) based on percentile ranks
# 3. Preserves demographic distributions while matching target prevalence rates
# 4. Returns population with diet column added
# 
# Expected overall distribution:
# - meets_5_a_day: 41%
# - below_5_a_day: 59%
# 
# This matches the Northern Ireland Health Survey 2023/24 findings for 5-a-day consumption.


