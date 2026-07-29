# risk_framingham_atrial_fibrillation
# https://www.framinghamheartstudy.org/fhs-risk-functions/atrial-fibrillation-10-year-risk/


atrial_fibrillation_risk <- function(
        age = NULL,
        sex = NULL,
        
        bmi = 25,
        Systolic_blood_pressure = 160,
        Prevalent_heart_failure = 0,
        
        #not used 
        Treatment_for_hypertension = 0,
        PR_interval_ms = 120,
        Significant_murmur = 0#,

        # Male_sex_age2, calculated
        # Age_x_significant_murmur,  calculated
        # Age_x_prevalent_heart_failure , calculated
        
        ){
  
coef_Age <- 	0.15052
coef_Age2	 <- -0.00038
coef_Male_sex <- 	1.99406
coef_Body_mass_index <- 	0.01930
coef_Systolic_blood_pressure <- 	0.00615

coef_Treatment_for_hypertension <- 	0.42410
coef_PR_interval <- 	0.07065
coef_Significant_murmur <- 	3.79586

coef_Prevalent_heart_failure <- 	9.42833
coef_Male_sex_x_age2	 <- -0.00028
coef_Age_x_significant_murmur	 <- -0.04238
coef_Age_x_prevalent_heart_failure	 <- -0.12307

baseline_hazard <- 0.96337 

# age = 70;
# sex = 'Females'
# sex = 'Males'

(risk <-   coef_Age * (age - 60.9022)
         + coef_Age2 * (age^2 - 3806.9)
         + coef_Male_sex * ( (sex == 'Males') - 0.4464)
         + coef_Body_mass_index * (bmi - 26.2861)
         + coef_Systolic_blood_pressure * ( Systolic_blood_pressure - 136.1674)

         + coef_Treatment_for_hypertension * (Treatment_for_hypertension - 0.2413)
        #+ coef_PR_interval * (PR_interval/10 - 16.3901)
        #+ coef_Significant_murmur * (Significant_murmur - 0.0281)  # valvular heart disease

         #+ coef_Prevalent_heart_failure * (Prevalent_heart_failure - 0.0087)
         + coef_Male_sex_x_age2 * ( (sex=='Males') * age^2 - 1654.66)
         #+ coef_Age_x_significant_murmur * age * (Significant_murmur -1.8961)
         #+ coef_Age_x_prevalent_heart_failure * (age * Prevalent_heart_failure - 0.61)
         )


  # Calculate 10-year risk
 ( af_risk <- 1 - (baseline_hazard ^ exp(risk)))
  
  return(af_risk)
  
}

# S0(10) = 0.96337 (10 year baseline survival)
# Betas are given for 1 unit increase for continuous variables and for the condition present in dichotomous variables.

apply_af_risk_wo_risk_factors <- function(input_population,intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 90, 90, age),
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 28,
        bmi == "obese"       ~ 35,
        TRUE ~ NA_real_),
      Treatment_for_hypertension = case_when(
          hypertension_status %in% c("hypertensive_untreated",
                                     "hypertensive_uncontrolled",
                                     "hypertensive_controlled"
                                     )   ~ 1,
          TRUE ~ 120),
      Systolic_blood_pressure   = case_when(hypertension_status %in%
                                              c( 'hypertensive_uncontrolled',
                                                 'hypertensive_untreated') ~ 150,
                                                    T~ 110),
      Prevalent_heart_failure  =(heart_failure != 0),
      sex) 
    
  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(af_risk =
             0.9 * ifelse(age < 35, 0,  #should really be 45
                    atrial_fibrillation_risk(
                      age = age, 
                      sex = sex,
                      bmi = bmi,
                      Treatment_for_hypertension = Treatment_for_hypertension,
                      Systolic_blood_pressure = Systolic_blood_pressure,
                      Prevalent_heart_failure = Prevalent_heart_failure
                    )
             )
    )
  
  input_population <- input_population |> 
    select(-any_of('af_risk'))
  
  input_population <- left_join(input_population,
                                postp[c('af_risk','id')], 
                                by ='id')
  
  input_population <- ungroup(input_population)
  return(input_population)
}


  
# atrial_fibrillation_risk(age = 40, sex = 'Males',bmi = 40)

# age = 70;
# sex = 'Females'

