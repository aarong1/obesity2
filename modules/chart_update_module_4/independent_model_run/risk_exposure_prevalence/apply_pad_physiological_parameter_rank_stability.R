# base_population_w_correlated_percentiles <- current_population

# current_population <- base_population_w_risk_factors

# pad_prevalence <- read_csv('./data/data_pipeline/pad_prevalence.csv')

pad_prevalence <- read.csv(textConnection(object = "Sex,AgeGroup,Year,PAD_cases,Population,Prevalence_percent,PR_2018_vs_2000,PR_CI_lower,PR_CI_upper
Overall,40-59,2000,5558,1476000,0.4,0.82,0.79,0.85
Overall,40-59,2010,5473,1529000,0.4,,,
Overall,40-59,2018,4941,1562000,0.3,,,
Overall,60-79,2000,20481,842000,2.4,1.15,1.13,1.17
Overall,60-79,2010,29564,1048000,3.0,,,
Overall,60-79,2018,33979,1201000,3.0,,,
Overall,80-110,2000,7115,208000,3.4,1.89,1.84,1.94
Overall,80-110,2010,13419,227000,5.9,,,
Overall,80-110,2018,16850,256000,6.4,,,
Overall,All ages,2000,33154,2526000,1.3,1.28,1.26,1.3
Overall,All ages,2010,48456,2803000,1.7,,,
Overall,All ages,2018,55770,3019000,1.6,,,
Females,40-59,2000,2212,730000,0.3,0.8,0.76,0.85
Females,40-59,2010,2257,758000,0.3,,,
Females,40-59,2018,1934,777000,0.2,,,
Females,60-79,2000,9046,454000,2.0,1.14,1.11,1.17
Females,60-79,2010,12288,544000,2.5,,,
Females,60-79,2018,13754,619000,2.3,,,
Females,80-110,2000,4072,140000,2.9,1.96,1.89,2.03
Females,80-110,2010,7596,146000,5.2,,,
Females,80-110,2018,8924,156000,5.7,,,
Females,All ages,2000,15330,1324000,1.2,1.32,1.3,1.35
Females,All ages,2010,22141,1448000,1.6,,,
Females,All ages,2018,24612,1552000,1.5,,,
Males,40-59,2000,3346,746000,0.5,0.83,0.79,0.87
Males,40-59,2010,3216,771000,0.4,,,
Males,40-59,2018,3008,785000,0.4,,,
Males,60-79,2000,11435,388000,3.0,1.16,1.14,1.19
Males,60-79,2010,17276,504000,3.6,,,
Males,60-79,2018,20225,582000,3.4,,,
Males,80-110,2000,3043,69000,4.4,1.79,1.72,1.87
Males,80-110,2010,5823,81000,7.2,,,
Males,80-110,2018,7926,100000,7.9,,,
Males,All ages,2000,17824,1203000,1.5,1.25,1.22,1.27
Males,All ages,2010,26315,1356000,1.9,,,
Males,All ages,2018,31159,1466000,1.8,,,"))

pad_prevalence <- pad_prevalence |> 
  filter(AgeGroup != 'All ages',
         Sex != 'Overall',
         Year == 2018) |> 
  select(sex = Sex, age = AgeGroup, prob_cum = Prevalence_percent) |> 
  mutate(prob_cum = prob_cum/100)

apply_pad_physiological_parameter_rank_stability <- function(current_population,pad_prevalence){
  
  pad_stratified_prevalence_df <- pad_prevalence |> 
    mutate(pad_status = 'pad')
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
      breaks = c(-Inf,  40, 60, 80, Inf),  # wellbeing has different age grouping
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-39", "40-59", "60-79", "80-110")
    )
    )
  
  current_population <- current_population |> 
    group_by(age_risk, sex) |>
    mutate(pad_percentile = rank(ties.method = 'random',pad_risk)/max(rank(pad_risk))) |> 
    ungroup()
  #select(pad_risk,pad_percentile) 
  
  # current_population |>
  # ggplot(aes(pad_risk, pad_percentile)) +
  #   geom_point()
  
  # select(sex,
  #        HSCT,
  #        Urban_mixed_rural_status,
  #        mdm_quintile_soa_name,
  #        age_risk,
  #        padpercentile) |>
  # mutate(deprivation = mdm_quintile_soa_name,
  #        # case_when(
  #        #   mdm_quintile_soa_name ==  'Most Deprived'~'most_deprived',
  #        #   mdm_quintile_soa_name ==  'Quintile 2'~'quintile_2',
  #        #   mdm_quintile_soa_name ==  'Quintile 3'~'quintile_3',
  #        #   mdm_quintile_soa_name ==  'Quintile 4'~'quintile_4',
  #        #   mdm_quintile_soa_name ==  'Least Deprived'~'least_deprived'
  #        #   
  #        # ),
  
  #        hsct = HSCT, #case_when(
  #        # HSCT == 'BHSCT'~'belfast',
  #        # HSCT == 'NHSCT'~'northern'     ,
  #        # HSCT == 'SEHSCT'~'south_eastern' ,
  #        # HSCT == 'SHSCT'~'southern'     ,
  #        # HSCT == 'WHSCT'~'western'  ),
  
  #        geo = Urban_mixed_rural_status#case_when(
  
  #        # Urban_mixed_rural_status == 'Mixed'~'mixed',
  #        # Urban_mixed_rural_status == 'Urban'~'urban',     
  #        # Urban_mixed_rural_status == 'Rural'~'rural' )
  # ) |> 
  
  current_population <-
current_population |> 
    left_join(pad_stratified_prevalence_df,
              relationship = 'many-to-one',
              multiple = 'first',
              
              by = join_by('sex', 
                           age_risk == age,  
                           pad_percentile<prob_cum)) #|> #|> View()
  
  # current_population |> select(pad_percentile,probability) |> filter(!is.na(probability))
  
  # count( pad_status)
  
  # fill in children pad as zero
  current_population <- current_population |> 
    replace_na(list(pad_status = 'no_pad')) 
  
  
  current_population <- current_population |> 
    select( -  c(   prob_cum ) ) #percentage
  
}




# current_population <- current_population |> select(!c(starts_with('pad_status'),starts_with('prob_cum'))) 
#   
# current_population |> select(c(age_risk,pad_percentile, pad_status,prob_cum)) 
# 
# ggplot(current_population, aes(prob_cum, pad_percentile)) +
#   geom_point()
# 
# current_population |> #select(-c(pad_status.x,pad_status.y)) -> current_population
#   # apply_pad_lifestyle_parameter_rank_stability(padresults_df) |>
#   count(pad_status,age_risk)
# 
# 








# # Smoking Application Function
# # 
# # This function applies smoking categories to a synthetic population
# # using previously estimated joint probabilities from the smoking joint estimation.
# # 
# # The function:
# # 1. Takes a population with padpercentile values (from apply_correlated_quantiles)
# # 2. Maps demographic variables to match the smoking stratified prevalence format
# # 3. Uses percentile-based assignment to assign smoking categories while preserving rank stability
# # 4. Returns the population with smoking categories assigned
# #
# # Dependencies: 
# # - joint_estimation/smoking.R (for padresults_df)
# # - apply_correlated_quantiles must be run first with 'smoking' in risks_to_include
# 
# apply_padlifestyle_parameter_rank_stability <- function(current_population,
#                                                              padresults_df){
#   
#   # Check if padpercentile column exists
#   if(!"padpercentile" %in% names(current_population)) {
#     stop("Error: 'padpercentile' column not found in current_population. Make sure to run apply_correlated_quantiles() with 'smoking' in risks_to_include first.")
#   }
#   
#   # Rename probability column to match other functions
#   padstratified_prevalence <- padresults_df %>%
#     rename(prob = probability)
#   
#   # Process smoking stratified prevalence to create cumulative probabilities
#   padstratified_prevalence <- padstratified_prevalence %>%
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
#     left_join(padstratified_prevalence,
#               relationship = 'many-to-one',
#               multiple = 'first',
#               
#               by = join_by('sex', 
#                            age10 == age,  
#                            'deprivation',   
#                            hsct,  
#                            'geo',
#                            padpercentile < prob_cum)
#               )
#   
#   # Clean up temporary columns
#   current_population <- current_population |> 
#    select( - c( list_probs, prob, prob_cum, prob_cond ) )
#   
#   # Check for missing values
#   missing_smoking <- sum(is.na(current_population$smoking))
#   if(missing_smoking > 0) {
#     warning(paste("Warning:", missing_smoking, "individuals could not be assigned a smoking category. Check for missing combinations in padstratified_prevalence."))
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
# # test_population <- apply_padlifestyle_parameter_rank_stability(test_population,
# #                                                                    padresults_df)
# # 
# # # Check results
# # names(test_population)
# # count(test_population, smoking)
# 
# # ===== FUNCTION SUMMARY =====
# # 
# # The apply_padlifestyle_parameter_rank_stability function successfully:
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
