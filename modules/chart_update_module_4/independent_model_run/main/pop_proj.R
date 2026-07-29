library(tidyverse)
library(data.table)
library(readxl)
library(readODS)


#################### Version Control Comments #######################

########################    WHATS CHANGED   #########################  

# A major re release of this module was determined because of:

#   1. The new data file from 2011 census of population counts by SYA and gender.
#   2. Finally partitioning and holding death and death reason
#   3. Calculating prevalence as well as incidence, whether these are
#        calculated together is yet to be determined

#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
###################### SYNTHETIC POPULATION #########################


load_dependencies <- function() {
  
  #source('./main/1_5_temp_synthetic_population.R')
  # source('./synthetic_population/1_6_temp_synthetic_population.R')
  # source('./synthetic_population/1_7_temp_synthetic_population.R')
  # source('./synthetic_population/1_8_temp_synthetic_population.R')
  source('./synthetic_population/1_9_temp_synthetic_population.R')
  
  
  # source('./prevalence_operators/apply_ethnicity_lifestyle_parameter.R')
  
  ############## Baseline Population ####################

  ############## Death and Deceased #####################
  
  #source('./prevalence_operators/apply_cvd_death.R')
  #source('./prevalence_operators/apply_other_death.R')
  source('./risk_incidence_operators/apply_age_sex_death.R')
  source('./risk_incidence_operators/qmortality.R')
  
  ################# Utility ################# 
  source('./risk_prevalence_operators/transform_probability.R')
  

  
}


load_dependencies()


###############################################
############ Model Specification ###############
###############################################

population_specification <- list(
  name = 'populaltion_projection_specification',
  population = list(scale_down_factor = 1.9e6/1e5
  ),
  
  model = list(
    start_year = 2016,
    duration = 10,
    number_of_runs = 8),
  
  meta = list(
    random_seed = {invisible(set.seed(22));22},
    
    time_configuration_defined = Sys.time(),
    
    last_time_configuration_run_in_main_def = NULL),
  
  risk = list(
    alcohol = 'two_state_transition_probability',
    smoking = 'two_state_transition_probability',
    bmi = 'three_state_rank_stability',
    diet = 'two_state_transition_probability',
    activity = 'two_state_transition_probability',
    
    cholesterol = 'two_state_transition_probability',
    atrial_fibrillation = 'two_state_transition_probability',
    hypertension = 'two_state_transition_probability',
    diabetes = 'two_state_transition_probability',
    peripheral_arterial_disease = 'two_state_transition_probability'
    
    # pollution = correlation_linear,
    # sleep = correlation_linear
    
  ),
  
  intervention = list(
    
    impact_scalar = list('2020' = 1),
    
    target_geography = list(
      trust = list(),
      lgd = list(),
      urban = list()
    ),
    
    target_demographics = list(
      age = list(),
      sex = list(),
      townsend_score = list(),
      mdm_decile = list()
    ),
    
    target_risk = list(
      alcohol = list(),
      smoking = list(),
      bmi = list(),
      diet = list(),
      activity = list()
    ),
    
    target_column_attributes = list(
      column = list('attribute')
      
    ),
    
    target_morbidity = 'stroke' )
  
)


### # model_specification$target = input
### input$demographics$age  = c(15,110)
### input$demographics$sex = c('Males','Females')
### input$demographics$townsend_score = range(townsend$townsend_TDS)
### input$demographics$mdm_decile = 1:10
### input$demographics$broad_ethnicity = c("minority", "white")
### input$demographics$HSCT = count(initial_time_zero_population,HSCT) |> pull(HSCT)
### input$risk$bmi = c('normal','overweight','obese')
### input$risk$bmi_percentile = c( 0.6, 1 )
### input$risk$qrisk = c( 0.25, 1 )



model_specification <- population_specification

############ Population Actualization ################
###############################################

base_population <- instantiate_base_pop( model_specification)   


initial_time_zero_population <- ungroup(base_population)

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
# #uses data.table - not converts back to data frame in function
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





# Model run ----
past_populations <- data.frame()#initial_time_zero_population)
initial_time_zero_population$intervention = 'non-intervention'

for(run in 1:(model_specification$model$number_of_runs)) {
  
  cat(paste('################################### \n run : ', run, ' \n###################################### \n'))

  current_population <- initial_time_zero_population |> 
    mutate(run = {{run}})
  
  current_population <- current_population |> mutate(bern_trial = runif(n()))
  
  for (time in 1:model_specification$model$duration){
    
    cat(paste('###################################### \n Time, t : ', time, '\n Run, r:', run,'\n###################################### \n'))
    
    print('Adding the current population to the past populations data structure')
    #current_population <- current_population |> select(-bern_trial)
    past_populations <- rbind(past_populations, current_population)
    
    current_population <- current_population |>
      mutate(age = age + 1) |> 
      mutate(
        age20 = cut(age,include.lowest = T,
                    breaks = seq(0,120,20),
                    labels = c('0-20',
                               '20-40',
                               '40-60',
                               '60-80',
                               '80-100',
                               '100-120')
        )
      )
    
    current_population <- current_population |>
      mutate(year = year + 1)
    
##########################################
    print('Apply and Partition Deaths')
##########################################
    
    current_population <- current_population %>% 
      apply_age_sex_death(apply_death = T) #|> 
      #apply_qmortality_mortality(apply_death = T)
    #uses data.table - not converts back to data frame in function
    
    current_population_who_died <- current_population |> 
      filter( !is.na(death) & !is.null(death) & !death==0 )
    
    dead_population <- rbind(dead_population, current_population_who_died)
    
    current_population_alive <- current_population |> 
      filter(is.na(death)| is.null(death)| death==0)
    
    current_population <- current_population_alive
    
    ##########################################
    print('Apply Births')
    ##########################################
    
    print(names(current_population))
    current_population <- current_population %>% 
      select(-bern_trial) |> 
      add_basic_yearly_births() |> 
      rowwise() |> 
      mutate(bern_trial = runif(n=1)) |> 
      ungroup()


    }
   
    
  }
  
