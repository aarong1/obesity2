# risk_prevalence_operator/apply_child_bmi_lifestyle_parameter_3State_rank_stability

#depends on risk_correlation.r
#depends on risk_estimation_/child_obesity.R

# plot_child_bmi_joint_results
# test_population <- instantiate_base_pop(test_specification)
# test_population <- test_population |>
#   apply_correlated_quantiles(
#     risks_to_include = c('bmi','sleep'),
#     correlation_matrix = pearson_correlation_matrix)
# 
# test_population <- test_population |> 
#   apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence)

# test_population$year = test_population$year+1

################################################
############## checking logic ##################
################################################
# head(test_population[,c('sex','age','bmi_percentile')])
# c("sex", "age", "deprivation", "hsct",  "geo", "bmi", "prob", "year")
# 
# head(
#   select(
#     test_population,
#     all_of(c('sex',   'age',   'bmi_percentile')
#     )
#   )
# )
# 
# # c(sex,   age,   deprivation,    hsct,   geo,   bmi)
# head(child_bmi_stratified_prevalence)
# 
# count(child_bmi_stratified_prevalence,sex)
# count(test_population,sex)
# 
# count(child_bmi_stratified_prevalence,age)
# count(test_population,ageKid)
# 
# 
# ###################################################
# # Make test population header consistent for joins
# ###################################################
# 
# #cut age bands
# test_population <- test_population |> 
#   mutate(ageKid = cut(
#     age,
#     
#     breaks = c(0, 2, 11, 16),  # upper bound exclusive
#     right = FALSE,  # left-closed, right-open: [a, b)
#     labels = c("0-1", "2-10", "11-15")
#   )
#   )
# 
# 
# 
# 
# child_bmi_stratified_prevalence_summarised <- 
#   
#   child_bmi_stratified_prevalence %>%
#   group_by(sex, age) %>%
#   arrange((bmi), .by_group = TRUE) %>%  # ensure consistent BMI order if needed
#   mutate(
#     prob_cond = prob / sum(prob),       # exact conditional probability
#     prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
#   ) %>%
#     group_by(sex, age) %>%
#   summarise(list_probs = list(prob_cum),
#             prob_cum,
#             bmi
#             ) |> 
#   ungroup() |> 
#   select(sex, age, child_bmi = bmi ,prob_cum,list_probs)
# 
# ###############################
# child_bmi_stratified_prevalence_summarised |> 
#   arrange(across(-c( prob)) ) |> 
#   head(9)
# 
# head(test_population, 3)
# 
# names(test_population)
# names(child_bmi_stratified_prevalence_summarised)
# ###############################
# 
# test_population <- test_population |> 
#   select(sex,
#          ageKid,
#          bmi_percentile) |> 
#   
#   left_join(child_bmi_stratified_prevalence_summarised,
#             relationship = 'many-to-one',
#             multiple = 'first',
#             na_matches = 'never',
#             by = join_by('sex', 
#                          ageKid == age,  
#                          
#                          bmi_percentile<prob_cum)) #|> view()
# 

apply_child_bmi_lifestyle_parameter_3State_rank_stability <-function(current_population,
                                                                     child_bmi_stratified_prevalence){
  


  #cut age bands
  current_population <- current_population |> 
    mutate(ageKid = cut(
      age,
      breaks = c(0, 2, 11, 16),  # upper bound exclusive
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-1", "2-10", "11-15")
    )
    )
  
  
  child_bmi_stratified_prevalence_summarised <- 
    
    child_bmi_stratified_prevalence %>%
    group_by(sex, age) %>%
    arrange((bmi), .by_group = TRUE) %>%  # ensure consistent BMI order if needed
    mutate(
      prob_cond = prob / sum(prob),       # exact conditional probability
      prob_cum = cumsum(prob_cond)        # CDF of BMI given stratum
    ) %>%
    group_by(sex, age) %>%
    mutate(list_probs = list(prob_cum),
              prob_cum,
              bmi
    ) |> 
    ungroup() |> 
    select(sex, age, child_bmi = bmi ,prob_cum) #,list_probs
  
  ###############################
  
  current_population <- current_population |> 
    # select(sex,
    #        ageKid,
    #        bmi_percentile) |> 
    
    left_join(child_bmi_stratified_prevalence_summarised,
              relationship = 'many-to-one',
              multiple = 'first',
              na_matches = 'never',
              by = join_by('sex', 
                           ageKid == age,  
                           bmi_percentile<prob_cum)) #|> view()
  
  
  current_population |> select(-prob_cum)
}


# test_population <- apply_bmi_lifestyle_parameter_3State_rank_stability(test_population,
#                                                     bmi_stratified_prevalence) |> 
#                     combine_child_adult_bmi()




combine_child_adult_bmi <- function(current_population){
  current_population <- current_population |> 
    mutate(bmi = coalesce(bmi,child_bmi)) |> 
    select(-child_bmi)
}
