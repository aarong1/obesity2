# bmi/apply_bmi_lifestyle_parameter_3State_rank_stability_long_form.R
# #depends on risk_correlation.r

# #depends on joint_estimation/adult_obesity.R
# 
# 
# test_population <- instantiate_base_pop()
# test_population <- apply_correlated_quantiles(test_population,
#                                               risks_to_include = c('bmi','sleep'),
#                                               correlation_matrix = pearson_correlation_matrix)
# 
# head(test_population[,c('sex','age','mdm_quintile_soa','HSCT','Urban_mixed_rural_status','bmi_percentile')])

# head(
#   select(
#   test_population,
#   all_of(c('sex',   'age',   'deprivation',    'hsct',   'geo',    'bmi_percentile')
#          )
#   )
# )
# 
# 
# # c(sex,   age,   deprivation,    hsct,   geo,   bmi)
# head(bmi_stratified_prevalence)
# 
# 
# count(bmi_stratified_prevalence,sex)
# count(test_population,sex)
# 
# 
# count(bmi_stratified_prevalence,age)
# count(test_population,age10)
# 
# #cut age bands
# test_population <- test_population |> 
#   mutate(age10 = cut(
#     age,
#     breaks = c(0, 16, 25, 35, 45, 55, 65, 75, 110),  # upper bound exclusive
#     right = FALSE,  # left-closed, right-open: [a, b)
#     labels = c("0-15", "16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-110")
#   )
#   )
# 
# count(bmi_stratified_prevalence,deprivation)
# count(test_population,mdm_quintile_soa)
# count(test_population,deprivation)
# #recode deprivation
# 
# test_population <- test_population |> 
# mutate(deprivation = case_when(
# mdm_quintile_soa == 1 ~ 'Most deprived',
# mdm_quintile_soa == 2 ~ 'Quintile 2',
# mdm_quintile_soa == 3 ~ 'Quintile 3',
# mdm_quintile_soa == 4 ~ 'Quintile 4',
# mdm_quintile_soa == 5 ~ 'Least deprived', 
# )
# )
# 
# count(bmi_stratified_prevalence,hsct)
# count(test_population,HSCT)
# #paste hsct
# test_population <- test_population |> 
#   mutate(hsct = str_remove(string = HSCT, 
#                           pattern = '\\s(.*)'
#   ))
# 
# count(test_population,Urban_mixed_rural_status)
# count(bmi_stratified_prevalence,geo)
# count(test_population,geo)
# # geography - Urban_mixed_rural_status rural
# 
# 
# test_population <- test_population |> 
# mutate(geo = str_remove(string = Urban_mixed_rural_status, 
#                      pattern = '\\s(.*)'
# ))
# 
# 
# # P(bmi \mid X) = \frac{P(bmi, X)}{\sum_{bmi’} P(bmi’, X)}
# # P(bmi | X) = P(bmi, X / SUM_bmi’ * P(bmi’, X)
# 
# # Where X = (sex, age, deprivation, hsct, geo)
# # bmi = a particular bmi
# # bmi' = set of all bmi
# 
# bmi_stratified_prevalence <- bmi_stratified_prevalence %>%
#   group_by(sex, age, deprivation, hsct, geo) %>%
#   arrange(desc(bmi), .by_group = TRUE) %>%  # ensure consistent BMI order if needed
#   mutate(
#     prob_cond = prob / sum(prob),       # exact conditional probability
#     prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
#   ) %>%
# 
#   mutate(list_probs = list(prob_cum)) |> 
#   ungroup()
#  
#  
# 
# bmi_stratified_prevalence |> 
#   arrange(across(-c( prob, bmi)) ) |> 
#   arrange(across(-c(prob,bmi)) ) |> 
#   tail(9)
# 
# head(test_population ,3)
# 
# names(test_population)
# names(bmi_stratified_prevalence)
# 
# test_population|> 
#   select(sex, trust,
#          geo,
#          deprivation,
#          age10 ,
#          hsct,
#          bmi_percentile) |> 
#   
#   left_join(bmi_stratified_prevalence,
#             relationship = 'many-to-one',
#             multiple = 'first',
#             
#             by = join_by('sex', 
#                          age10 == age,  
#                          'deprivation',   
#                          hsct,  
#                          'geo',
#                          bmi_percentile<prob_cum)) |> 
#   view()


apply_bmi_lifestyle_parameter_3State_rank_stability <- function(current_population,
                                                               bmi_stratified_prevalence){
  
  bmi_stratified_prevalence <- bmi_stratified_prevalence %>%
    group_by(sex, age, deprivation, hsct, geo) %>%
    arrange(desc(bmi), .by_group = TRUE) %>%  # ensure consistent BMI order if needed
    mutate(
      prob_cond = prob / sum(prob),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
    ) %>%
    
    mutate(list_probs = list(prob_cum)) |> 
    ungroup()

  
  #cut age bands
  current_population <- current_population |> 
    as_tibble() |> 
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 25, 35, 45, 55, 65, 75, 110),  # upper bound exclusive
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-15", "16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-110")
    )
    )
  
  #recode deprivation
  
  current_population <- current_population |> 
    mutate(deprivation = case_when(
      mdm_quintile_soa == 1 ~ 'Most deprived',
      mdm_quintile_soa == 2 ~ 'Quintile 2',
      mdm_quintile_soa == 3 ~ 'Quintile 3',
      mdm_quintile_soa == 4 ~ 'Quintile 4',
      mdm_quintile_soa == 5 ~ 'Least deprived', 
    )
    )

  #paste hsct
  # current_population <- current_population |> 
  #   as_tibble() |> 
  #   mutate(hsct = str_remove(string = HSCT, 
  #                            pattern = '\\s[^\\s]*$'
  #   )
  # )

  current_population <- current_population |> 
    mutate(geo = str_remove(string = Urban_mixed_rural_status, 
                            pattern = '\\s(.*)'
    )
  )
  
  current_population <- current_population |> 
    
    
    mutate(  hsct = case_when(
      HSCT == 'BHSCT'~'Belfast',
      HSCT == 'NHSCT'~'Northern'     ,
      HSCT == 'SEHSCT'~'South Eastern' ,
      HSCT == 'SHSCT'~'Southern'     ,
      HSCT == 'WHSCT'~'Western'  )
    ) |> 
    # select(sex,
    #        hsct,
    #        geo=Urban_mixed_rural_status,
    #        deprivation,
    #        age10,
    #        bmi_percentile) |>
    
    left_join(bmi_stratified_prevalence,
              relationship = 'many-to-one',
              multiple = 'first',
              na_matches = 'never',
              by = join_by('sex', 
                           age10 == age,  
                           'deprivation',   
                           hsct,  
                           'geo',
                           bmi_percentile < prob_cum))
  
  
  
  current_population <- current_population |> 
   select( - c( list_probs, prob,prob_cum, prob_cond ) )
  
  }


# test_population <- instantiate_base_pop()
# 
# test_population <- apply_correlated_quantiles(current_population = test_population,
#                                               correlation_matrix = pearson_correlation_matrix,
#                                               risks_to_include = 'bmi',
#                                               model_configuration_list = model_specification
#                                               )
# names(test_population)
# 
# apply_bmi_lifestyle_parameter_3State_rank_stability(test_population,
#                                                     bmi_stratified_prevalence) |> names()


