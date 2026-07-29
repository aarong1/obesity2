

apply_diabetes_physiological_parameter <- function(current_population,diabetes_prevalence) {
  
  if(!is.data.frame(current_population)){
    stop('must pass a data frame or tibble!')
  }
  
  if(!any( c("area_code", "area_name", "age", "sex") %in% names(current_population))){
    stop('must have basic spatial demogrphic attributes added and named correctly
         (area_code, area_name, age, sex)')
  }
  
  if(dim(current_population)[1]<1000){
    stop('length of data frame must be greater than 1000 ')
  }
  
  cat('log: info: starting hypertension parameter function call\n')
  
  current_year = max(current_population$year)
  
  prevalence <- diabetes_prevalence %>% 
    transmute(
      age_risk = age,
      sex,
      prob = prevalence/100
    ) 
  
  current_population <- current_population %>% 
    mutate(age10 = cut(
      age,
      breaks = c(0, 16, 35, 45, 55, 65, 75, 110),  # wellbeing has different age grouping
      right = FALSE,  # left-closed, right-open: [a, b)
      labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75-110")
    )) |>
    left_join(prevalence) |> 
    replace_na(list(prob = 0)) #%>% view
  
  current_population <- current_population %>% 
    rowwise() %>% 
    mutate(binom_trial=rbinom(size = 1,
                              prob=prob,
                              n=1)) %>% 
    ungroup() %>% 
    mutate(diabetes = ifelse(binom_trial==1,current_year,NA)
           # mutate(hypertension = ifelse(binom_trial==1,'hypertension',NA),
           
    )  %>% 
    select(-c( binom_trial,
               
               prob))
  
  cat('log: info: ending diabetes parameter function call\n')
  return (current_population)
}

###########################################
############ TEST ############
###########################################

# test_population <- instantiate_base_pop(scale_down_factor=100) 
# apply_hypertension_lifestyle_parameter(test_population)



