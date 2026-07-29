library(fst)

source('./main/main_configuration.R') # model_specification list
initial_time_zero_population = read.fst('./main/initial_time_zero_population.fst')

print('Applying established prevalence to initial time zero population from beofre population start')
print('These established instances are labelled start_year - 1')

# Example lists
trusts <- c("BHSCT", "NHSCT","SHSCT", "WHSCT","SEHSCT")
morbidities <- c(
  'non_diabetic_hyperglycaemia',
  'copd',
  'asthma',
  'depression',
  'atrial_fibrillation',
  'lung_cancer',
  'chronic_kidney_disease',
  'chd',
  'dementia',
  'hypertension',
  'stroke',
  'diabetes',
  'heart_failure'
)

# proportions are v. similar
(our_hsct_pop <- count(initial_time_zero_population,HSCT,name='pop_hsct') |> 
    mutate(prop_hsct = pop_hsct/sum(pop_hsct) ) |> 
    mutate(pop_hsct = pop_hsct  * model_specification$population$scale_down_factor ) |>
    mutate(pop_ni = sum(pop_hsct) )
)

# data.frame(HSCT = c('Belfast',	'Northern',	'South Eastern',	'Southern',	'Western'),
#            Count = c(363390, 479266, 367927, 390976, 301616) ) |> 
#       add_count(wt=Count) |> 
#       mutate(Count/n)|> 
#   mutate(HSCT = paste(HSCT,'HSCT'))

(prevalence_hsct_new <- prevalence_hsct |> 
    left_join(our_hsct_pop) |> 
    mutate(.after = prob, our_prob = Count/pop_hsct) |> 
    mutate(our_count = our_prob *  pop_hsct) |> 
    mutate(count_w_qof_prob = prob *  pop_hsct ) |> 
    mutate(our_expected_states = Count/model_specification$population$scale_down_factor ) |>
    mutate(qof_expected_states = count_w_qof_prob/model_specification$population$scale_down_factor ) |> 
    # replacing prob from original data file !!!!!!!!!
    mutate(prob=our_prob)
)

filter(prevalence_hsct_new, Disease == 'Stroke & TIA',Year==2023) |>
  select( - c(Year, Disease,Interpolated,Per1k,pop_ni))

do_not_run_moved_to_loop <- function(){
  
  multi_run_population_w_established_prevalence  <- data.frame()
  
  initial_time_zero_population$asthma = 0
  initial_time_zero_population$copd = 0
  initial_time_zero_population$depression = 0
  initial_time_zero_population$non_diabetic_hyperglycaemia = 0
  
  # initial_time_zero_population %>% 
  #   summarise(across(morbidities, ~ sum(.x!=0) * model_specification$population$scale_down_factor ))
  # multi_run_population_w_established_prevalence %>% 
  #   summarise(across(morbidities, ~ sum(.x!=0) * model_specification$population$scale_down_factor ))
  
  prevalence_hsct_new <- prevalence_hsct_new %>% 
    # group_by(Disease,) %>% 
    arrange(Disease,HSCT,Year) %>% 
    fill(prob,.direction = 'down')# %>% 
  # filter(Disease=='Depression')
  
  for(run in 1:3){#(model_specification$model$number_of_runs*2)
    
    print(paste('run',run))
    
    population_w_established_prevalence <- reduce2(
      .x = rep(trusts,length(morbidities)),
      .y = rep(morbidities,each = length(trusts)),
      .init = initial_time_zero_population,
      .f = function(pop, trust, morbidity) {
        
        assign_year_minus_one_prevalence(
          input_population = pop,
          trust = trust,
          morbidity = morbidity,
          #year = 2017,
          prevalence_df = prevalence_hsct_new,
          configuration = model_specification
        )
      }
    )
    
    population_w_established_prevalence %>% 
      summarise(across(morbidities, ~ sum(.x!=0) * model_specification$population$scale_down_factor ))
    
    print('Collecting last run from establishing prevalence')
    
    multi_run_population_w_established_prevalence <- 
      rbind(
        multi_run_population_w_established_prevalence,
        mutate(population_w_established_prevalence, 
               run = {{run}})
        
      )
  }
  
  
  show_survivor_prevalence_by_discrete_states(multi_run_population_w_established_prevalence,
                                              stroke,
                                              year,
                                              HSCT,
                                              stroke)  |>
    mutate(mean_counted_states_scaled_to_population = mean_counted_states * model_specification$population$scale_down_factor) |>
    select(-c(6:8)) |>
    filter(stroke!=0)
  
  initial_time_zero_population <- multi_run_population_w_established_prevalence
  
}

initial_time_zero_population$asthma_recovered = NA
initial_time_zero_population$copd_recovered = NA
initial_time_zero_population$depression_recovered = NA
initial_time_zero_population$non_diabetic_hyperglycaemia_recovered = NA
do_not_run_moved_to_loop <- function(){
  # initial_time_zero_population$year = initial_time_zero_population$year - 1
  
  
  initial_time_zero_population <- initial_time_zero_population |> 
    declare_absolute_incident_morbidity(morbidity = "stroke") |> 
    declare_absolute_incident_morbidity(morbidity = "chd") |> 
    declare_absolute_incident_morbidity(morbidity = "diabetes") |> 
    declare_absolute_incident_morbidity(morbidity = "dementia") |> 
    declare_absolute_incident_morbidity(morbidity = "heart_failure") |> 
    declare_absolute_incident_morbidity(morbidity = "atrial_fibrillation") |> 
    declare_absolute_incident_morbidity(morbidity = "hypertension") |> 
    declare_absolute_incident_morbidity(morbidity = "chronic_kidney_disease") |> 
    declare_absolute_incident_morbidity(morbidity = "asthma") |> 
    declare_absolute_incident_morbidity(morbidity = "copd") |> 
    declare_absolute_incident_morbidity(morbidity = "depression") |> 
    declare_absolute_incident_morbidity(morbidity = "non_diabetic_hyperglycaemia") |>
    declare_absolute_incident_morbidity(morbidity = "lung_cancer") 
}


show_incidence <- function(current_population, morbidity_year_risk, morbidity, ...) {
  
  current_population |> 
    group_by(...) |> 
    summarise(summed_risk = sum({{morbidity_year_risk}}), 
              counted_states = sum({{morbidity}} == max(year)), # !=0
              .groups = "drop") |> 
    mutate( tot_ppl=n())
  
}


print('Initialise deaths dataframe')

###################################################################

dead_population <- data.frame()
initial_time_zero_population$death <- NA
initial_time_zero_population$death_reason <- NA
initial_time_zero_population$qmortality_risk <- NA
initial_time_zero_population$qx <- NA


###################################################################

# print('Apply and Partition deaths')
# 
# initial_time_zero_population <- initial_time_zero_population %>% 
#   apply_age_sex_death()  
# #uses data.table - now converts back to data frame in function
# 
# current_population_who_died <- initial_time_zero_population |> 
#   filter( !is.na(death) & !is.null(death) & !death==0 )
# 
# dead_population <- rbind(dead_population, current_population_who_died)
# 
# current_population_alive <- initial_time_zero_population |> 
#   filter(is.na(death)| is.null(death)| death==0)
# 
# initial_time_zero_population <- current_population_alive


# stroke_outcome_incidence = data.frame()
# diabetes_outcome_incidence = data.frame()
# chd_outcome_incidence = data.frame()
# af_outcome_incidence = data.frame()
# hypertension_outcome_incidence = data.frame()
# ckd_outcome_incidence = data.frame()
# lungcancer_outcome_incidence = data.frame()
# dementia_outcome_incidence = data.frame()
# heart_failure_outcome_incidence = data.frame()
# 
# stroke_outcome_prevalence = data.frame()
# diabetes_outcome_prevalence = data.frame()
# chd_outcome_prevalence = data.frame()
# af_outcome_prevalence = data.frame()
# hypertension_outcome_prevalence = data.frame()
# ckd_outcome_prevalence = data.frame()
# lungcancer_outcome_prevalence = data.frame()
# dementia_outcome_prevalence = data.frame()
# heart_failure_outcome_prevalence = data.frame()




apply_crude_intervention_multiplier <- function( current_population,
                                                 intervention = model_specification$intervention,
                                                 morbidity = stroke
                                                 
) {
  
  if(is.null(intervention) ) {
    
    return(current_population)
    
  }
  
  yr = max(current_population$year)
  
  intervention_multiplier <- intervention$impact_scalar[yr]
  
  input <- intervention
  morbidity = input$target_morbidity
  
  new_col_sym   <- sym(paste0(morbidity, "_intervened_risk"))
  risk_col_sym  <- sym(paste0(morbidity, "_year_risk"))
  
  
  # 2) shortcuts for your two filter‐lists
  demo <- intervention$demographics
  risk <- intervention$risk
  
  current_population <- 
    
    # 3) pipe through your two mutates and return the result
    current_population %>%
    mutate(
      intervention_target =
        age                  >= demo$age[1] &
        age                  <= demo$age[2] &
        sex               %in% demo$sex &
        HSCT              %in% demo$HSCT &
        # mdm_quintile_soa %in% c(demo$mdm_quintile_soa)
        townsend_score    >= demo$townsend_score[1] &
        townsend_score    <= demo$townsend_score[2] &
        as.integer(mdm_decile) %in% demo$mdm_decile &
        broad_ethnicity   %in% demo$broad_ethnicity &
        between(.data$qrisk_score,
                risk$qrisk[1],
                risk$qrisk[2]) &
        between(.data$bmi_percentile,
                risk$bmi_percentile[1],
                risk$bmi_percentile[2]) &
        .data$bmi %in% risk$bmi
    ) %>%
    mutate(
      # dynamically named new column
      !!new_col_sym := (!!risk_col_sym) *
        intervention_target *
        intervention_multiplier
    )
  
  return(current_population)
  
  # finally adjust your risk (e.g. stroke_risk, fracture_risk, etc.)
  #  mutate(adjusted_risk = stroke_risk * multiplier)
  
  #   intervention <- c(1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)[time]
}