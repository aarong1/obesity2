# Logistic regression risk function for morbidity
# Coefficients based on provided table
# Gender: binary (0,1), Age: years, BMI: kg/m^2, Drinks: per week, Triglycerides: mg/dL
# Returns risk probability

#Optimal ----
# Fasting triglyceride	below 1.7mmol/L	below 150mg/dL
# Non-fasting triglyceride	below 2.0mmol/L	below 177mg/dL

risk_framingham_hepatic_steatosis <- function(
    age,             # in years
    sex,         # 0 = reference (e.g. Male), 1 = other (e.g. Female)
    bmi = 25,             # Body Mass Index (kg/m^2)
    drinks_per_week = 1, # average drinks per week
    triglycerides =  170  # triglycerides level (mg/dL)
) {
  # Check inputs
  # if (!sex %in% c('Males','Females')) stop("`sex` must be 0 or 1")
  if (age < 0 || age > 150) stop("`age` out of plausible range")
  # if (bmi <= 0 || bmi > 100) stop("`bmi` out of plausible range")
  if (drinks_per_week < 0) stop("`drinks_per_week` cannot be negative")
  if (triglycerides < 0) stop("`triglycerides` cannot be negative")
  
  # Coefficients
  intercept      <- -7.7109
  beta_sex    <- -1.103
  beta_age       <-  0.0374
  beta_bmi       <-  0.1643
  beta_drinks    <- -0.1108
  beta_triglycer <-  0.00519
  
  # Linear predictor
  lp <- intercept +
    beta_sex    * (sex == 'Females') +
    beta_age       * age +
    beta_bmi       * bmi +
    beta_drinks    * drinks_per_week +
    beta_triglycer * triglycerides
  
  # Risk probability via logistic transform
  risk <- exp(lp) / (1 + exp(lp))
  return(risk)
}

# Example usage:
# risk_morbidity(gender = 1, age = 33, bmi = 35, drinks_per_week = 8, triglycerides = 187)
#> 0.3447788


apply_liver_disease_risk_wo_risk_factors <- function(input_population){

postp1 <- 
  input_population %>% 
  filter(year == max(year,na.rm = TRUE))

postp <- postp1 %>%
  mutate(
    age = ifelse(age > 75, 75, age),
    sex = ifelse(sex == 'Females', 1,0),
    bmi = case_when(
      bmi == "normal"     ~ 20,
      bmi == "overweight" ~ 28,
      bmi == "obese"       ~ 38,
      TRUE ~ NA_real_),
    drinks_per_week = case_when(
      alcohol == 'no_risk'  ~ 1,
      alcohol == 'lower_risk'  ~ 5,
      alcohol == 'higher_risk'  ~ 20,
    ),
    triglycerides = case_when(
      cholesterol_status == 'raised_cholesterol' ~ 180,
      cholesterol_status == 'hdl/cholesterol' ~ 200,
      T~ 110
    )
  )


postp <- postp %>% 
  rowwise() %>% 
  # mutate(list(print(c(age,sex) )))%>% 
  mutate(liver_risk =
           ifelse(age < 30, 0,
                  risk_framingham_hepatic_steatosis(
                    age = age, 
                    sex = sex#,
                    #bmi = bmi
                  ))
         
  )

input_population <- input_population |> select(-any_of('liver_risk'))

input_population <- left_join(input_population,postp[c('liver_risk','id')], by ='id')
# initial_time_zero_population$stroke_risk <- postp$stroke_risk
# initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]

return(input_population)
}

# test_population |>
#   apply_liver_disease_risk_wo_risk_factors() |>
#   pull(liver_risk) |> 
#   hist()

