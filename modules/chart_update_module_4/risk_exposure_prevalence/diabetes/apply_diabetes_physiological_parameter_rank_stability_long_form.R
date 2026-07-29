# current_population <- base_population_w_correlated_percentiles

apply_diabetes_physiological_parameter_rank_stability <- function(current_population,
                                                                  diabetes_joint_estimation_results_df){
  
  current_population <- current_population |> 
    select(-any_of(c('diabetes_status')))
  
  diabetes_stratified_prevalence_df <- diabetes_joint_estimation_results_df |> 
        mutate(diabetes_status = factor(diabetes_status, ordered=T,levels=c( "no_diabetes", "diagnosed_diabetes", "undiagnosed_diabetes"))) %>% 
    group_by(sex, age, deprivation) %>%
    arrange((diabetes_status), .by_group = TRUE) %>%  # ensure consistent BMI order if needed
    mutate(
      prob_cond = probability / sum(probability),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
    ) |> 
    mutate(list_probs = list(prob_cum)) |> 
    ungroup() 
  
  current_population <- current_population |> 
    #   # select(sex,
    #   #        HSCT,
    #   #        Urban_mixed_rural_status,
    #   #        mdm_quintile_soa_name,
    #   #        age_risk,
    #   #        smoking_percentile) |> 
    
    mutate(deprivation_quintile =
             case_when(
               mdm_quintile_soa_name == 'Most Deprived'~'most_deprived',
               mdm_quintile_soa_name == 'Quintile 2'~'quintile_2',
               mdm_quintile_soa_name == 'Quintile 3'~'quintile_3',
               mdm_quintile_soa_name == 'Quintile 4'~'quintile_4',
               mdm_quintile_soa_name == 'Least Deprived'~'least_deprived'
             ),
           
           # hsct = case_when(
           #   HSCT == 'BHSCT'~'belfast',
           #   HSCT == 'NHSCT'~'northern'     ,
           #   HSCT == 'SEHSCT'~'south_eastern' ,
           #   HSCT == 'SHSCT'~'southern'     ,
           #   HSCT == 'WHSCT'~'western'  ),
           
           # geography = case_when(
           #   Urban_mixed_rural_status == 'Mixed'~'mixed',
           #   Urban_mixed_rural_status == 'Urban'~'urban',     
           #   Urban_mixed_rural_status == 'Rural'~'rural' )
    ) |>

    left_join(diabetes_stratified_prevalence_df,
              relationship = 'many-to-one',
              multiple = 'first',
              by = join_by('sex', 
                           age_risk == age,  
                           mdm_quintile_soa_name == deprivation,  
                           # HSCT == hsct,  
                           # Urban_mixed_rural_status== geography,
                           diabetes_percentile<prob_cum)) #|> View()
  
  current_population <- current_population |> 
    select( -  c( list_probs, probability, prob_cum, prob_cond ) ) #percentage
  
  #fill in childrens diabetes
  current_population <- current_population |> 
    replace_na(list(diabetes_status = 'no_diabetes'))
  
  return(current_population)
  
}


