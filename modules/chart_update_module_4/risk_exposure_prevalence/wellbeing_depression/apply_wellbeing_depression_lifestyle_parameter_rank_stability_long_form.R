# Wellbeing/Depression Application Function
# 
# This function applies wellbeing categories to a synthetic population
# using previously estimated joint probabilities from the wellbeing joint estimation.
# 
# The function:
# 1. Takes a population with depression_percentile values (from apply_correlated_quantiles)
# 2. Maps demographic variables to match the wellbeing stratified prevalence format
# 3. Uses percentile-based assignment to assign wellbeing categories while preserving rank stability
# 4. Returns the population with wellbeing categories assigned
#
# Dependencies: 
# - joint_estimation/wellbeing.R (for wellbeing_results_df)
# - apply_correlated_quantiles must be run first with 'wellbeing' in risks_to_include
# current_population = base_population_w_correlated_percentiles 
apply_wellbeing_depression_lifestyle_parameter_rank_stability <- function(current_population,
                                                                          wellbeing_results_df){
  
  # Check if depression_percentile column exists
  if(!"depression_percentile" %in% names(current_population)) {
    stop("Error: 'depression_percentile' column not found in current_population. Make sure to run apply_correlated_quantiles() with 'wellbeing' in risks_to_include first.")
  }
  
  # Rename probability column to match other functions and create stratified prevalence
  wellbeing_stratified_prevalence <- wellbeing_results_df %>%
    rename(prob = probability) 
  
  # Process wellbeing stratified prevalence to create cumulative probabilities
  wellbeing_stratified_prevalence <- wellbeing_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(wellbeing), .by_group = TRUE) %>%  # ensure consistent wellbeing order
    mutate(
      prob_cond = prob / sum(prob),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of wellbeing given stratum
    ) %>%
    
    mutate(list_probs = list(prob_cum)) |> 
    ungroup()
  

  #cut age bands to match wellbeing data (note different age groups)
  current_population <- current_population |> 
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),  # wellbeing has different age grouping
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75-110")
    )
    )

  #recode deprivation to match wellbeing data format
  current_population <- current_population |> 
    mutate(deprivation = mdm_quintile_soa_name #case_when(
      # mdm_quintile_soa == 1 ~ 'most_deprived',
    #   mdm_quintile_soa == 2 ~ 'quintile_2',
    #   mdm_quintile_soa == 3 ~ 'quintile_3',
    #   mdm_quintile_soa == 4 ~ 'quintile_4',
    #   mdm_quintile_soa == 5 ~ 'least_deprived', 
    # )
  )

  # Extract HSCT name to match wellbeing data format
  current_population <- current_population |> 
    mutate(hsct = HSCT #case_when(
    #   HSCT == 'BHSCT' ~ "belfast",
    #   HSCT == 'NHSCT' ~ "northern", 
    #   HSCT == 'SEHSCT' ~ "south_eastern",
    #   HSCT == 'SHSCT' ~ "southern",
    #   HSCT == 'WHSCT' ~ "western"#,
      #TRUE ~ str_remove(HSCT, '\\s[^\\s]*$')
    # )
  )

  # Extract geography to match wellbeing data format
  current_population <- current_population |> 
    mutate(geo = Urban_mixed_rural_status # case_when(
      
      # Urban_mixed_rural_status == 'Mixed'~'mixed',
      # Urban_mixed_rural_status == 'Urban'~'urban',
      # Urban_mixed_rural_status == 'Rural'~'rural' )
    )
  
  # Join with wellbeing stratified prevalence
  current_population <- current_population |> 
    as_tibble() |> 
    # select(sex,
    #        hsct, # = HSCT,
    #        geo = Urban_mixed_rural_status,
    #        deprivation,# = mdm_quintile_soa_name,
    #        age10,
    #        depression_percentile) |>
    left_join(wellbeing_stratified_prevalence,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age10 == age,  
                           'deprivation',   
                           hsct,  
                           'geo',
                           depression_percentile < prob_cum)
              )
  
  # Clean up temporary columns
  current_population <- current_population |> 
   select( - c( list_probs, percentage,prob, prob_cum, prob_cond ) )
  
  # Check for missing values
  missing_wellbeing <- sum(is.na(current_population$wellbeing))
  if(missing_wellbeing > 0) {
    warning(paste("Warning:", missing_wellbeing, "individuals could not be assigned a wellbeing category. Check for missing combinations in wellbeing_stratified_prevalence."))
  }
  
  return(current_population)
}


# current_population |> 
# apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) |> count(wellbeing,age_risk) |> print(n=100)


# Example usage and testing:
# 
# # First, load the wellbeing joint estimation results
# source("joint_estimation/wellbeing.R")
# 
# # Create test population 
# test_population <- instantiate_base_pop()
# 
# # Apply correlated quantiles including wellbeing percentile
# test_population <- apply_correlated_quantiles(current_population = test_population,
#                                               correlation_matrix = pearson_correlation_matrix,
#                                               risks_to_include = c('bmi', 'wellbeing'),
#                                               model_configuration_list = model_specification
#                                               )
# 
# # Apply wellbeing lifestyle parameters
# test_population <- apply_wellbeing_depression_lifestyle_parameter_rank_stability(test_population,
#                                                                                 wellbeing_results_df)
# 
# # Check results
# names(test_population)
# count(test_population, wellbeing)

# ===== FUNCTION SUMMARY =====
# 
# The apply_wellbeing_depression_lifestyle_parameter_rank_stability function successfully:
# 1. Maps synthetic population demographics to wellbeing stratified prevalence format
# 2. Assigns wellbeing categories (good_wellbeing, moderate_wellbeing, poor_wellbeing) based on percentile ranks
# 3. Preserves demographic distributions while matching target prevalence rates
# 4. Returns population with wellbeing column added
# 
# Expected wellbeing categories based on GHQ12 scores:
# - good_wellbeing: GHQ12 score 0 (52%)
# - moderate_wellbeing: GHQ12 score 1-3 (28.5%)
# - poor_wellbeing: GHQ12 score 4+ (19.5%) - indicates possible psychiatric disorder
