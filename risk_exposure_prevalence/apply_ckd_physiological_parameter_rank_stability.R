# base_population_w_correlated_percentiles <- current_population

# current_population <- base_population_w_risk_factors

## STAGE 3a - 5

#https://data.cvdprevent.nhs.uk/data-explorer?indicator=8&area=1&period=17&level=1
ckd_prevalence <- tibble::tribble(
  ~Condition,        ~Code, ~`All.18+`, ~`All.18-39`, ~`All.40-59`, ~`All.60-79`,  ~`All.80+`, ~`Males.18+`, ~`Males.18-39`, ~`Males.40-59`, ~`Males.60-79`,  ~`Males.80-110`, ~`Females.18+`, ~`Females.18-39`, ~`Females.40-59`, ~`Females.60-79`,  ~`Females.80-110`,
  "Chronic kidney disease (CKD)", "CVDP001CKD",     "3.98",   "0.08",   "0.89",   "7.84", "30.42",      "3.48",   "0.08",   "0.83",   "7.45", "29.34",         "4.48",   "0.07",   "0.96",   "8.21", "31.20"
)

ckd_prevalence <- ckd_prevalence |> 
  pivot_longer(cols = -c(1:2), 
               names_sep = '\\.',
               names_to = c('sex','age_risk'), 
               values_to = 'prevalence') |> 
  filter(sex != 'All',
         age_risk != '18+')

ckd_prevalence <- ckd_prevalence |> 
  mutate(probability = as.numeric(prevalence) /100) |> 
  select(-c(Condition,Code)) |> mutate(prob_cum = probability) 

# add_df <- tribble(
# ~sex, ~age_risk, ~prevalence, ~probability, ~prob_cum,
# 'Males',   '0-18'  ,  0,            0,   1,
# 'Females',   '0-18',    0,            0,   1
# )
# 
# ckd_prevalence <- rbind(
#   add_df,
#   ckd_prevalence
# )

stroe_unit_tests <- function(){
current_population <- past_populations %>% filter(year == min(year))

current_population %>%
  # apply_ckd_risk_wo_risk_factors() %>% 
  apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>% 
  count(age_risk,sex,ckd_status) %>%
  # filter(age>65) %>% View

  add_count(age_risk,
            sex,wt = n) %>%
  filter(ckd_status=='ckd') %>%
  mutate(n/nn*100)

current_population %>%
  apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>%
  count(age_risk,sex,ckd_status) %>%
  add_count(age_risk,
            sex,wt = n) %>%
  filter(ckd_status=='ckd') %>%
  mutate(n/nn*100)
}

  
# current_population %>% 
#   apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>%
#   count(age_risk,sex,ckd_status) %>% 
#   add_count(age_risk,
#             sex,wt = n) %>% 
#   filter(ckd_status=='ckd') %>% 
#   mutate(n/nn*100)

# past_populations %>% 
#   filter(year == min(year)) %>% 
#   add_count(age, ckd_status) %>% 
#   count(age, ckd_status, n,wt = ckd_risk ) %>% 
#   mutate(nn/n)

apply_ckd_physiological_parameter_rank_stability <- function(current_population,ckd_prevalence){
  
  current_population <- current_population |> 
    select(-any_of(c('ckd_status')))
  
  ckd_stratified_prevalence_df <- ckd_prevalence |> 
    mutate(ckd_status = 'ckd')
  
  # ckd_stratified_prevalence_df <- rbind(ckd_stratified_prevalence_df,
  #     ckd_prevalence |> 
  #   mutate(ckd_status = 'ckd') %>% 
  #   mutate(prob_cum=1.1)
  # )  
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
      breaks = c(-Inf, 18, 40, 60, 80, Inf),  # wellbeing has different age grouping
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-18", "18-39", "40-59", "60-79", "80-110")
    )
    )
  
  current_population <- current_population |> 
    group_by(age_risk, sex) |>
    mutate(ckd_percentile = rank(ties.method = 'random', ckd_risk)/max(rank(ties.method = 'random',ckd_risk))
                                ) |> 
    ungroup()
    #select(ckd_risk,ckd_percentile) 
  
  # current_population |> 
  # ggplot(aes(ckd_risk, ckd_percentile)) +
  #   geom_point()
    
      # select(sex,
      #        HSCT,
      #        Urban_mixed_rural_status,
      #        mdm_quintile_soa_name,
      #        age_risk,
      #        ckdpercentile) |>
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
    left_join(ckd_stratified_prevalence_df,
              relationship = 'many-to-one',
              multiple = 'first',
              by = join_by('sex', 
                           age_risk,  
                           ckd_percentile<prob_cum)) #%>% count(ckd_status)
  
  current_population <- current_population |>
    replace_na(list(ckd_status = 'no_ckd'))
  
  # current_population |> select(ckd_percentile,probability) |> filter(!is.na(probability))
  # count(current_population,ckd_status)
  
  # fill in children ckd as zero
  
  current_population <- current_population |> 
    select( -  c(  prevalence, probability, prob_cum ) ) #percentage,prob_cond
  
}



# 