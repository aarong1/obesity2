# Physical Activity Application Function

# This function applies physical activity categories to a synthetic population
# using previously estimated joint probabilities from the PA joint estimation.

# The function:
# 1. Takes a population with physical_activity_percentile values (from apply_correlated_quantiles)
# 2. Maps demographic variables to match the PA stratified prevalence format
# 3. Uses percentile-based assignment to assign PA categories while preserving rank stability
# 4. Returns the population with pa categories assigned

# Dependencies: 
# - joint_estimation/PA.R (for pa_stratified_prevalence)
# - apply_correlated_quantiles must be run first with 'pa' in risks_to_include

apply_pa_lifestyle_parameter_rank_stability <- function(current_population,
                                                        pa_stratified_prevalence){
  
  current_population <-  current_population %>% 
    select(-any_of('pa'))
  
  
  # Check if physical_activity_percentile column exists
  if(!"physical_activity_percentile" %in% names(current_population)) {
    stop("Error: 'physical_activity_percentile' column not found in current_population. Make sure to run apply_correlated_quantiles() with 'pa' in risks_to_include first.")
  }
  
  # Process PA stratified prevalence to create cumulative probabilities
  pa_stratified_prevalence <- pa_stratified_prevalence %>%
    as_tibble() |> 
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(pa), .by_group = TRUE) %>%  # ensure consistent PA order (meets_rec first)
    mutate(
      prob_cond = probability / sum(probability),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of PA given stratum
    ) %>%
    mutate(list_probs = list(prob_cum)) |> 
    ungroup()

  #cut age bands to match PA data
  current_population <- current_population |> 
    mutate(age10 = cut(
      age,
      breaks = c(0, 19, 25, 35, 45, 55, 65, 75, 110),  # upper bound exclusive
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-18", "19-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
    )
    )
  
  #recode deprivation to match PA data format
  current_population <- current_population |> 
    mutate(deprivation = as.character(mdm_quintile_soa_name)
    )

  # Extract HSCT name to match PA data format
  current_population <- current_population |> 
    mutate(
      hsct = HSCT
    )

  # Extract geography to match PA data format
  current_population <- current_population |> 
    mutate(geo = Urban_mixed_rural_status
    )
  
  # Join with PA stratified prevalence
  current_population <- current_population |> 
    as_tibble() |> 
    # select(sex,
    #        hsct, # = HSCT,
    #        geo = Urban_mixed_rural_status,
    #        deprivation,# = mdm_quintile_soa_name,
    #        age10,
    #        physical_activity_percentile) |>
    left_join(pa_stratified_prevalence,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age10 == age,  
                           'deprivation',   
                           hsct,  
                           'geo',
                           physical_activity_percentile < prob_cum)
              )
  
  # Clean up temporary columns
  current_population <- current_population |> 
   select( - c( list_probs, probability, prob_cum, prob_cond ) )
  
  # Check for missing values
  missing_pa <- sum(is.na(current_population$pa))
  if(missing_pa > 0) {
    warning(paste("Warning:", missing_pa, "individuals could not be assigned a PA category. Check for missing combinations in pa_stratified_prevalence."))
  }
  
  return(current_population)
}


# Example usage and testing:
# 
# # First, load the PA joint estimation results
# source("joint_estimation/PA.R")
# 
# # Create test population 
# test_population <- instantiate_base_pop()
# 
# # Apply correlated quantiles including PA percentile
# test_population <- apply_correlated_quantiles(current_population = test_population,
#                                               correlation_matrix = pearson_correlation_matrix,
#                                               risks_to_include = c('bmi', 'pa'),
#                                               model_configuration_list = model_specification
#                                               )
# 
# # Apply PA lifestyle parameters
# test_population <- apply_pa_lifestyle_parameter_rank_stability(test_population,
#                                                               pa_stratified_prevalence)
# 
# # Check results
# names(test_population)
# count(test_population, pa)

# ===== FUNCTION SUMMARY =====
# 
# The apply_pa_lifestyle_parameter_rank_stability function successfully:
# 1. Maps synthetic population demographics to PA stratified prevalence format
# 2. Assigns PA categories (meets_rec, some_activity, low_activity, inactive) based on percentile ranks
# 3. Preserves demographic distributions while matching target prevalence rates
# 4. Returns population with pa column added
# 
# Expected PA categories:
# - meets_rec: Meets physical activity recommendations
# - some_activity: Some activity but below recommendations  
# - low_activity: Low levels of activity
# - inactive: No physical activity
