# Alcohol Application Function
# 
# This function applies alcohol risk categories to a synthetic population
# using previously estimated joint probabilities from the alcohol joint estimation.
# 
# The function:
# 1. Takes a population with alcohol_percentile values (from apply_correlated_quantiles)
# 2. Maps demographic variables to match the alcohol stratified prevalence format
# 3. Uses percentile-based assignment to assign alcohol categories while preserving rank stability
# 4. Returns the population with alcohol categories assigned
#
# Dependencies: 
# - joint_estimation/alcohol.R (for alcohol_stratified_prevalence)
# - apply_correlated_quantiles must be run first with 'alcohol' in risks_to_include

# current_population <- base_population_w_risk_factors
apply_alcohol_lifestyle_parameter_rank_stability <- function(current_population,
                                                             alcohol_stratified_prevalence){
  
  current_population <-  current_population %>% 
    select(-any_of('alcohol'))
  
  
  # Check if alcohol_percentile column exists
  if(!"alcohol_percentile" %in% names(current_population)) {
    stop("Error: 'alcohol_percentile' column not found in current_population. Make sure to run apply_correlated_quantiles() with 'alcohol' in risks_to_include first.")
  }
  
  # Process alcohol stratified prevalence to create cumulative probabilities
  alcohol_stratified_prevalence <- alcohol_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(alcohol), .by_group = TRUE) %>%  # ensure consistent alcohol order
    mutate(
      prob_cond = probability / sum(probability),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of alcohol given stratum
    ) %>%
    mutate(list_probs = list(prob_cum)) |> 
    ungroup()

  #cut age bands to match alcohol data
  current_population <- current_population |> 
    mutate(age10 = cut(
      age,
      breaks = c(0, 18, 35, 45, 55, 65, 75, 110),  # alcohol data starts at 18
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-17", "18-34", "35-44", "45-54", "55-64", "65-74", "75-110")
    )
    ) #%>% count(age,age10) 
  
  #recode deprivation to match alcohol data format
  current_population <- current_population |> 
           mutate(deprivation = case_when(
             mdm_quintile_soa == 1 ~ 'Most Deprived',
             mdm_quintile_soa == 2 ~ 'Quintile 2',
             mdm_quintile_soa == 3 ~ 'Quintile 3',
             mdm_quintile_soa == 4 ~ 'Quintile 4',
             mdm_quintile_soa == 5 ~ 'Least Deprived', 
           )
      )

  # Extract HSCT name to match alcohol data format
  current_population <- current_population |> 
    mutate(hsct = HSCT
    )
    
  # Extract geography to match alcohol data format
  current_population <- current_population |> 
    mutate(geo = Urban_mixed_rural_status)
  
  # Join with alcohol stratified prevalence
  current_population <- current_population |> 
    
    # select(sex, 
    #        age10,  
    #        deprivation,   
    #        hsct,  
    #        geo,
    #        alcohol_percentile) |> 
    
    left_join(alcohol_stratified_prevalence,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age10 == age,  
                           'deprivation',   
                           hsct,  
                           'geo',
                           alcohol_percentile < prob_cum)
              )
  
  # Clean up temporary columns
  # print(names(current_population))
  current_population <- current_population |> 
   select( -  c( list_probs, probability, prob_cum, prob_cond ) )
  
  # Check for missing values
  missing_alcohol <- sum(is.na(current_population$alcohol))
  if(missing_alcohol > 0) {
    warning(paste("Warning:", missing_alcohol, "individuals could not be assigned an alcohol category. Check for missing combinations in alcohol_stratified_prevalence."))
  }
  
  return(current_population)
}


# Example usage and testing:
# 
# # First, load the alcohol joint estimation results
# source("joint_estimation/alcohol.R")
# 
# # Create test population 
# test_population <- instantiate_base_pop()
# 
# # Apply correlated quantiles including alcohol percentile
# test_population <- apply_correlated_quantiles(current_population = test_population,
#                                               correlation_matrix = pearson_correlation_matrix,
#                                               risks_to_include = c('bmi', 'alcohol'),
#                                               model_configuration_list = model_specification
#                                               )
# 
# # Apply alcohol lifestyle parameters
# test_population <- apply_alcohol_lifestyle_parameter_rank_stability(test_population,
#                                                                    alcohol_stratified_prevalence)
# 
# # Check results
# names(test_population)
# count(test_population, alcohol)

# ===== FUNCTION SUMMARY =====
# 
# The apply_alcohol_lifestyle_parameter_rank_stability function successfully:
# 1. Maps synthetic population demographics to alcohol stratified prevalence format
# 2. Assigns alcohol risk categories (no_risk, lower_risk, increased_risk, higher_risk) based on percentile ranks
# 3. Preserves demographic distributions while matching target prevalence rates
# 4. Returns population with alcohol column added
# 
# Expected alcohol categories:
# - no_risk: No alcohol risk (24%)
# - lower_risk: Lower risk alcohol consumption (59%)
# - increased_risk: Increased risk alcohol consumption (14%)
# - higher_risk: Higher risk alcohol consumption (2%)
# Note: Some data may include 'unknown' category (1%)
