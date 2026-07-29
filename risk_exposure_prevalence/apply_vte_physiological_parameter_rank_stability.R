# base_population_w_correlated_percentiles <- current_population

# current_population <- base_population_w_risk_factors

vte_prevalence <- tribble(
  ~percentage, ~sex, ~age,
  0.2,  'Males', '30-50',
  0.6,  'Females','30-50', 
  1.3,  'Females',  '50-70',
  1.9,  'Males',    '50-70', 
  4.3,  'Females',  '70-100',
  4.3,  'Males',    '70-100', 
  8.1,  'Females', '100-110',
  9.3,   'Males',   '100-110'
) |> 
  mutate(
    prob_cum = percentage / 100,
   # prob_cond = probability / sum(probability),       # exact conditional probability
    # prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
  )

apply_vte_physiological_parameter_rank_stability <- function(current_population,vte_prevalence){
  
  current_population <- current_population |> 
    select(-any_of(c('vte_status')))
  
  vte_stratified_prevalence_df <- vte_prevalence |> 
    mutate(vte_status = 'vte')
  # group_by(sex, age_risk) %>%
  # # arrange(desc(smoking), .by_group = TRUE) %>%  # ensure consistent BMI order if needed
  # mutate(
  #   prob_cond = probability / sum(probability),       # exact conditional probability
  #   prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
  # ) |> 
  # mutate(list_probs = list(prob_cum)) |> 
  # ungroup() 
  
  current_population <- current_population |> 
    mutate( age_risk = cut(
      age,
      breaks = c(-Inf, 30, 50, 70, Inf),  # wellbeing has different age grouping
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-30", "30-50", "50-70", "100-110")
    )
    )
  
  current_population <- current_population |> 
    group_by(age_risk, sex) |> 
    mutate(vte_percentile = rank(vte_risk)/max(rank(vte_risk))) |> 
    ungroup()
  #select(vte_risk,vte_percentile) 
  
  # current_population |>
  # ggplot(aes(vte_risk, vte_percentile)) +
  #   geom_point()
  
  # select(sex,
  #        HSCT,
  #        Urban_mixed_rural_status,
  #        mdm_quintile_soa_name,
  #        age_risk,
  #        vtepercentile) |>
  # mutate(deprivation = mdm_quintile_soa_name,
  #        # case_when(
  #        #   mdm_quintile_soa_name ==  'Most Deprived'~'most_deprived',
  #        #   mdm_quintile_soa_name ==  'Quintile 2'~'quintile_2',
  #        #   mdm_quintile_soa_name ==  'Quintile 3'~'quintile_3',
  #        #   mdm_quintile_soa_name ==  'Quintile 4'~'quintile_4',
  #        #   mdm_quintile_soa_name ==  'Least Deprived'~'least_deprived'
  #        #   
  #        # ),
  #        
  #        hsct = HSCT, #case_when(
  #        # HSCT == 'BHSCT'~'belfast',
  #        # HSCT == 'NHSCT'~'northern'     ,
  #        # HSCT == 'SEHSCT'~'south_eastern' ,
  #        # HSCT == 'SHSCT'~'southern'     ,
  #        # HSCT == 'WHSCT'~'western'  ),
  #        
  #        geo = Urban_mixed_rural_status#case_when(
  #        
  #        # Urban_mixed_rural_status == 'Mixed'~'mixed',
  #        # Urban_mixed_rural_status == 'Urban'~'urban',     
  #        # Urban_mixed_rural_status == 'Rural'~'rural' )
  # ) |> 
  
  
  current_population <- current_population |> 
    left_join(vte_stratified_prevalence_df,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age_risk==age,  
                           vte_percentile<prob_cum)) #|> View()
  
  # current_population |> select(vte_percentile,probability) |> filter(!is.na(probability))
  # count(current_population,vte_status)
  
  # fill in children vte as zero
  current_population <- current_population |> 
    replace_na(list(vte_status = 'no_vte')) 
  
  current_population <- current_population |> 
    select( -  c(   prob_cum, percentage ) ) #percentage,prob_cond
  
}


# count(current_population,vte_status)




# current_population |> 
#   apply_vtelifestyle_parameter_rank_stability(vteresults_df) |> 
#   count(smoking,age_risk)










# # Smoking Application Function
# # 
# # This function applies smoking categories to a synthetic population
# # using previously estimated joint probabilities from the smoking joint estimation.
# # 
# # The function:
# # 1. Takes a population with vtepercentile values (from apply_correlated_quantiles)
# # 2. Maps demographic variables to match the smoking stratified prevalence format
# # 3. Uses percentile-based assignment to assign smoking categories while preserving rank stability
# # 4. Returns the population with smoking categories assigned
# #
# # Dependencies: 
# # - joint_estimation/smoking.R (for vteresults_df)
# # - apply_correlated_quantiles must be run first with 'smoking' in risks_to_include
# 
# apply_vtelifestyle_parameter_rank_stability <- function(current_population,
#                                                              vteresults_df){
#   
#   # Check if vtepercentile column exists
#   if(!"vtepercentile" %in% names(current_population)) {
#     stop("Error: 'vtepercentile' column not found in current_population. Make sure to run apply_correlated_quantiles() with 'smoking' in risks_to_include first.")
#   }
#   
#   # Rename probability column to match other functions
#   vtestratified_prevalence <- vteresults_df %>%
#     rename(prob = probability)
#   
#   # Process smoking stratified prevalence to create cumulative probabilities
#   vtestratified_prevalence <- vtestratified_prevalence %>%
#     group_by(sex, age, deprivation, hsct, geo) %>%
#     arrange(desc(smoking), .by_group = TRUE) %>%  # ensure consistent smoking order
#     mutate(
#       prob_cond = prob / sum(prob),       # exact conditional probability
#       prob_cum = cumsum(prob_cond)        # CDF of smoking given stratum
#     ) %>%
#     
#     mutate(list_probs = list(prob_cum)) |> 
#     ungroup()
#   
# 
#   #cut age bands to match smoking data
#   current_population <- current_population |> 
#     mutate(age10 = cut(
#       age,
#       breaks = c(0, 16, 25, 35, 45, 55, 65, 75, 110),  # upper bound exclusive
#       right = FALSE,  # left-closed, right-open: [a, b)
#       labels = c("0-15", "16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-110")
#     )
#     )
#   
#   #recode deprivation to match smoking data format
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
#   # Extract HSCT name to match smoking data format
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
#   # Extract geography to match smoking data format
#   current_population <- current_population |> 
#     mutate(geo = case_when(
#       str_detect(Urban, "Urban") ~ "Urban",
#       str_detect(Urban, "Rural") ~ "Rural",
#       TRUE ~ str_remove(Urban, '\\s(.*)')
#     ))
#   
#   # Join with smoking stratified prevalence
#   current_population <- current_population |> 
#     left_join(vtestratified_prevalence,
#               relationship = 'many-to-one',
#               multiple = 'first',
#               
#               by = join_by('sex', 
#                            age10 == age,  
#                            'deprivation',   
#                            hsct,  
#                            'geo',
#                            vtepercentile < prob_cum)
#               )
#   
#   # Clean up temporary columns
#   current_population <- current_population |> 
#    select( - c( list_probs, prob, prob_cum, prob_cond ) )
#   
#   # Check for missing values
#   missing_smoking <- sum(is.na(current_population$smoking))
#   if(missing_smoking > 0) {
#     warning(paste("Warning:", missing_smoking, "individuals could not be assigned a smoking category. Check for missing combinations in vtestratified_prevalence."))
#   }
#   
#   return(current_population)
# }
# 
# 
# # Example usage and testing:
# # 
# # # First, load the smoking joint estimation results
# # source("joint_estimation/smoking.R")
# # 
# # # Create test population 
# # test_population <- instantiate_base_pop()
# # 
# # # Apply correlated quantiles including smoking percentile
# # test_population <- apply_correlated_quantiles(current_population = test_population,
# #                                               correlation_matrix = pearson_correlation_matrix,
# #                                               risks_to_include = c('bmi', 'smoking'),
# #                                               model_configuration_list = model_specification
# #                                               )
# # 
# # # Apply smoking lifestyle parameters
# # test_population <- apply_vtelifestyle_parameter_rank_stability(test_population,
# #                                                                    vteresults_df)
# # 
# # # Check results
# # names(test_population)
# # count(test_population, smoking)
# 
# # ===== FUNCTION SUMMARY =====
# # 
# # The apply_vtelifestyle_parameter_rank_stability function successfully:
# # 1. Maps synthetic population demographics to smoking stratified prevalence format
# # 2. Assigns smoking categories (current_smoker, former_regular, former_irregular, never_smoked) based on percentile ranks
# # 3. Preserves demographic distributions while matching target prevalence rates
# # 4. Returns population with smoking column added
# # 
# # Expected smoking categories:
# # - current_smoker: Currently smokes regularly
# # - former_regular: Former regular smoker  
# # - former_irregular: Former irregular smoker
# # - never_smoked: Never smoked
