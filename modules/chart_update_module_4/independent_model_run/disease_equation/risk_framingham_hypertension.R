
# weibull regression equation

#1 – exp[ – exp(	ln(4) – [22.94954 + ∑ Xß]/0.87692)]

# scale = 0.87692
# shape = 1/scale
# Intercept = 22.94954

risk_framingham_hypertension <- function(
                                      age = NULL,
                                      sex = NULL,
                                      SBP =  120,
                                      DBP = 70,
                                      Smoking = 0,
                                      Parental_Hypertension = 0,
                                      bmi = 24.11
                                      ){

coef_age <- 	-0.15641
coef_sex <- 	-0.20293
coef_SBP <- 	-0.05933
coef_DBP <-   -0.12847

# A normal blood pressure reading is typically 120/80 mmHg, 
# with the top number being systolic and the bottom number being diastolic. 

coef_Smoking <- -0.19073
coef_Parental_Hypertension <- -0.16612

# parental_hypertension is ordinal 
# zero, one or both, 0, 1, 2 parents

coef_BMI <- 	-0.03388
coef_age_x_DBP_interaction <- 	0.00162


risk <- age * coef_age +
        (sex == 'Females' )  * coef_sex +
        SBP  * coef_SBP +
        DBP  * coef_DBP +
        Smoking * coef_Smoking +
        Parental_Hypertension * coef_Parental_Hypertension +
        bmi * coef_BMI +
        age * DBP * coef_age_x_DBP_interaction
        

#print(paste('risk: ', risk + 22.94954))
      
(  
  p = 1 - exp( - exp(	(log( 4 ) - ( 22.94954 + risk ))/0.87692 ) ) 
  )

#the log4 is the number of years

# also 
# has 1 years log1 
# and 2 years log2


return(p)

}

apply_hypertension_risk_wo_risk_factors <- function(input_population,intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 75, 75, age),
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 28,
        bmi == "obese"       ~ 35,
        TRUE ~ NA_real_),
      sex) 
  
  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(hypertension_risk =
             ifelse(age < 20, 0,
                    risk_framingham_hypertension(
                      age = age, 
                      sex = sex#,
                      #bmi = bmi, 
                    )
             )
    )
  
  input_population <- input_population |> select(-any_of('hypertension_risk'))

  input_population <- left_join(input_population,postp[c('hypertension_risk','id')], by ='id')
  input_population <- ungroup(input_population)
  return(input_population)
}

# Diabetes mellitus was defined as fasting glucose ≥7.0 mmol/L (126 mg/dL) or use of antihyperglycemic therapy.
# Hypertension, defined as systolic/diastolic blood pressure of ≥140/90 mm Hg

#risk_framingham_hypertension(  56,  'Males' )

# p = 1 - exp( - exp(	log( 10 ) - ( 22.94954 + -30:40 )/ 1.87692 ) )
# plot(-30:40, p)


