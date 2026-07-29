# cholesterol_joint_results_df
# 
# current_population <- base_population_w_correlated_percentiles

apply_cholesterol_physiological_parameter_rank_stability <- function(current_population,chol_perc){ #cholesterol_joint_results_df
  
  cholesterol_stratified_prevalence_df <- chol_perc |> #cholesterol_joint_results_df
    group_by(sex, age, deprivation, ) %>%
    arrange(desc(cholesterol_status), .by_group = TRUE) %>%  # ensure consistent BMI order if needed
    mutate(
      probability = percentage/100,
      prob_cond = percentage / sum(percentage),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
    ) |> 
    mutate(list_probs = list(prob_cum)) |> 
    ungroup() 
  
  # current_population <- base_population_w_modifiable_risk_factors
  
  current_population <- current_population |> 
    mutate(age_risk = cut(age, breaks = c( -Inf, 16, 34, 44, 54, 64, 74, Inf),
                          labels = c('0-15',"16-34", "35-44", "45-54", "55-64", "65-74", "75-110")
    )) |> 
    left_join(cholesterol_stratified_prevalence_df,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age_risk == age,  
                           mdm_quintile_soa_name == deprivation,   
                           cholesterol_percentile<prob_cum)) #|> View()
  
  # current_population |> filter(is.na(cholesterol_status)) |> select(age_risk,mdm_quintile_soa_name,deprivation,sex,cholesterol_status)

  # count(current_population,cholesterol_status)
        
  # cholesterol_status     n
  # 1 raised_cholesterol 74114
  # 2 normal_cholesterol 74313
  # 3    hdl/cholesterol   566
  
  
  #fill in childrens cholesterol
  current_population <- current_population |> 
    replace_na(list(cholesterol_status = 'normal_cholesterol'))
  
  current_population <- current_population |> 
    select( -  c( list_probs, prob_cum, prob_cond ) ) #percentage
  
}



apply_granular_cholesterol_measure_posthoc_overlay <- function(current_population,special_cholesterol){
  
  
  current_population <- current_population %>% 
    left_join(special_cholesterol,
              relationship = 'many-to-one',
              multiple = 'first',
              by = join_by('sex', 
                           cholesterol_status == broad_cholesterol_status,
                           age_risk == age,  
                           cholesterol_percentile<prob_cum)) %>% 
    mutate(d=cholesterol_percentile) %>% 
    mutate(cholesterol_status_granular = coalesce(cholesterol_status.y,cholesterol_status)) %>% 
    select(-c('prob_cond', 'prob_cum', 'list_probs','d', probability, cholesterol_status.y))
  # filter(!cholesterol_status %in% c('total/hdl','low_hdl') %in% (cholesterol_status != cholesterol_status.y))
  
  return(current_population)
  
}





# # cholesterol Application Function
# # 
# # This function applies cholesterol categories to a synthetic population
# # using previously estimated joint probabilities from the cholesterol joint estimation.
# # 
# # The function:
# # 1. Takes a population with cholesterol_percentile values (from apply_correlated_quantiles)
# # 2. Maps demographic variables to match the cholesterol stratified prevalence format
# # 3. Uses percentile-based assignment to assign cholesterol categories while preserving rank stability
# # 4. Returns the population with cholesterol categories assigned
# #
# # Dependencies: 
# # - joint_estimation/cholesterol.R (for cholesterol_results_df)
# # - apply_correlated_quantiles must be run first with 'cholesterol' in risks_to_include
# 
# apply_cholesterol_lifestyle_parameter_rank_stability <- function(current_population,
#                                                              cholesterol_results_df){
#   
#   # Check if cholesterol_percentile column exists
#   if(!"cholesterol_percentile" %in% names(current_population)) {
#     stop("Error: 'cholesterol_percentile' column not found in current_population. Make sure to run apply_correlated_quantiles() with 'cholesterol' in risks_to_include first.")
#   }
#   
#   # Rename probability column to match other functions
#   cholesterol_stratified_prevalence <- cholesterol_results_df %>%
#     rename(prob = probability)
#   
#   # Process cholesterol stratified prevalence to create cumulative probabilities
#   cholesterol_stratified_prevalence <- cholesterol_stratified_prevalence %>%
#     group_by(sex, age, deprivation, hsct, geo) %>%
#     arrange(desc(cholesterol), .by_group = TRUE) %>%  # ensure consistent cholesterol order
#     mutate(
#       prob_cond = prob / sum(prob),       # exact conditional probability
#       prob_cum = cumsum(prob_cond)        # CDF of cholesterol given stratum
#     ) %>%
#     
#     mutate(list_probs = list(prob_cum)) |> 
#     ungroup()
#   
# 
#   #cut age bands to match cholesterol data
#   current_population <- current_population |> 
#     mutate(age10 = cut(
#       age,
#       breaks = c(0, 16, 25, 35, 45, 55, 65, 75, 110),  # upper bound exclusive
#       right = FALSE,  # left-closed, right-open: [a, b)
#       labels = c("0-15", "16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-110")
#     )
#     )
#   
#   #recode deprivation to match cholesterol data format
#   current_population <- current_population |> 
#     mutate(deprivation = case_when(
#       mdm_quintile == 1 ~ 'Q1_most_deprived',
#       mdm_quintile == 2 ~ 'Q2',
#       mdm_quintile == 3 ~ 'Q3',
#       mdm_quintile == 4 ~ 'Q4',
#       mdm_quintile == 5 ~ 'Q5_least_deprived', 
#     )
#     )
# 
#   # Extract HSCT name to match cholesterol data format
#   current_population <- current_population |> 
#     mutate(hsct = case_when(
#       str_detect(HSCT, "Belfast") ~ "Belfast",
#       str_detect(HSCT, "Northern") ~ "Northern", 
#       str_detect(HSCT, "South") ~ "South_Eastern",
#       str_detect(HSCT, "Southern") ~ "Southern",
#       str_detect(HSCT, "Western") ~ "Western",
#       TRUE ~ str_remove(HSCT, '\\s[^\\s]*$')
#     ))
# 
#   # Extract geography to match cholesterol data format
#   current_population <- current_population |> 
#     mutate(geo = case_when(
#       str_detect(Urban, "Urban") ~ "Urban",
#       str_detect(Urban, "Rural") ~ "Rural",
#       TRUE ~ str_remove(Urban, '\\s(.*)')
#     ))
#   
#   # Join with cholesterol stratified prevalence
#   current_population <- current_population |> 
#     left_join(cholesterol_stratified_prevalence,
#               relationship = 'many-to-one',
#               multiple = 'first',
#               
#               by = join_by('sex', 
#                            age10 == age,  
#                            'deprivation',   
#                            hsct,  
#                            'geo',
#                            cholesterol_percentile < prob_cum)
#               )
#   
#   # Clean up temporary columns
#   current_population <- current_population |> 
#    select( - c( list_probs, prob, prob_cum, prob_cond ) )
#   
#   # Check for missing values
#   missing_cholesterol <- sum(is.na(current_population$cholesterol))
#   if(missing_cholesterol > 0) {
#     warning(paste("Warning:", missing_cholesterol, "individuals could not be assigned a cholesterol category. Check for missing combinations in cholesterol_stratified_prevalence."))
#   }
#   
#   return(current_population)
# }
# 
# 
# # Example usage and testing:
# # 
# # # First, load the cholesterol joint estimation results
# # source("joint_estimation/cholesterol.R")
# # 
# # # Create test population 
# # test_population <- instantiate_base_pop()
# # 
# # # Apply correlated quantiles including cholesterol percentile
# # test_population <- apply_correlated_quantiles(current_population = test_population,
# #                                               correlation_matrix = pearson_correlation_matrix,
# #                                               risks_to_include = c('bmi', 'cholesterol'),
# #                                               model_configuration_list = model_specification
# #                                               )
# # 
# # # Apply cholesterol lifestyle parameters
# # test_population <- apply_cholesterol_lifestyle_parameter_rank_stability(test_population,
# #                                                                    cholesterol_results_df)
# # 
# # # Check results
# # names(test_population)
# # count(test_population, cholesterol)
# 
# # ===== FUNCTION SUMMARY =====
# # 
# # The apply_cholesterol_lifestyle_parameter_rank_stability function successfully:
# # 1. Maps synthetic population demographics to cholesterol stratified prevalence format
# # 2. Assigns cholesterol categories (current_smoker, former_regular, former_irregular, never_smoked) based on percentile ranks
# # 3. Preserves demographic distributions while matching target prevalence rates
# # 4. Returns population with cholesterol column added
# # 
# # Expected cholesterol categories:
# # - current_smoker: Currently smokes regularly
# # - former_regular: Former regular smoker  
# # - former_irregular: Former irregular smoker
# # - never_smoked: Never smoked
