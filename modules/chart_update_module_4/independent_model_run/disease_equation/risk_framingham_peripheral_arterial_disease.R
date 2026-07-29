## Intermittent Claudication (IC) Disease Risk Predictor
# Logistic regression based on provided coefficients

risk_ic_disease <- function(
    sex,                 # 0 = female, 1 = male
    age,                  # years
    bp_cat = 'Normal',               # one of: 'Normal', 'High Normal', 'Stage1 Hypertension', 'Stage2+ Hypertension'
    diabetes = 0,             # 0 = no, 1 = yes
    cigarettes_per_day = 0,    # integer or numeric
    cholesterol = 170,          # mg/dL Serum cholesterol 
    chd = 0           # 0 = no history of CHD, 1 = yes
) {
  # Input validation
  if (!sex %in% c('Males', 'Females')) stop("`male` must be 0 or 1")
  if (age < 0 || age > 120) stop("`age` out of plausible range")
  valid_bp <- c('Normal','High Normal','Stage1 Hypertension','Stage2+ Hypertension')
  if (!bp_cat %in% valid_bp) stop("`bp_cat` must be one of: ", paste(valid_bp, collapse=", "))
  if (!diabetes %in% c(0,1)) stop("`diabetes` must be 0 or 1")
  if (cigarettes_per_day < 0) stop("`cigarettes_per_day` cannot be negative")
  if (cholesterol < 0) stop("`cholesterol` cannot be negative")
  if (!chd %in% c(0,1)) stop("`chd` must be 0 or 1")
  
  # Coefficients
  
  intercept <- -8.9152
  beta_male <- 0.5033
  beta_age  <- 0.0372
  
  # Blood pressure categories
  
  bp_coefs <- c(
    'Normal'                 = 0.0000,
    'High Normal'            = 0.2621,
    'Stage1 Hypertension'    = 0.4067,
    'Stage2+ Hypertension'   = 0.7977
  )
  beta_diabetes    <- 0.9503
  beta_cigarettes  <- 0.0314
  beta_cholesterol <- 0.0048
  beta_chd         <- 0.9939
  
  # Linear predictor
  lp <- intercept +
    beta_male * (sex  == 'Males') +
    beta_age * age +
    bp_coefs[bp_cat] +
    beta_diabetes * diabetes +
    beta_cigarettes * cigarettes_per_day +
    beta_cholesterol * cholesterol +
    beta_chd * chd
  
  # Risk probability
  risk <- exp(lp) / (1 + exp(lp))
  return(risk)
}

# ratio_asymp_to_symp <- 2.5  # based on epidemiology
# p_PAD_estimate <- p_IC * (1 + ratio_asymp_to_symp)





# Example usage:
# risk_ic_disease(
#   male = 1,
#   age = 65,
#   bp_cat = 'Stage2+ Hypertension',
#   diabetes = 1,
#   cigarettes_per_day = 10,
#   cholesterol = 200,
#   chd = 1
# )
#> 0.xxx





apply_pad_risk_wo_risk_factors <- function(input_population){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 85, 85, age),
      sex) 
  
  
  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(pad_risk =
             ifelse(age < 30, 0,
                    risk_ic_disease(
                      age = age, 
                      sex = sex
                    )
                )
    )
  
  input_population <- input_population |> 
    select(-any_of('pad_risk'))
  
  input_population <- left_join(input_population,postp[c('pad_risk','id')], by ='id')
  # initial_time_zero_population$stroke_risk <- postp$stroke_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  
  return(input_population)
}


# test_population |> 
#   apply_pad_risk_wo_risk_factors() |> 
#   pull(pad_risk)
