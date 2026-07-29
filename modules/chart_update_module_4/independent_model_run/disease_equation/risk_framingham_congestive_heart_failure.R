# Function to calculate 4-year Heart Failure (HF) risk
#https://www.framinghamheartstudy.org/fhs-risk-functions/congestive-heart-failure/
#pooled logistic regression
#https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.4780091214#:~:text=Observations%20over%20multiple%20intervals%20are,dependent%20covariate%20Cox%20regression%20analysis.
#has a more complete calculator but with less readily availabel clinical calculators



heart_failure_risk <- function(
age = NULL,
sex = NULL,
LVH_b = 0,
Heart_rate_10_bpm = 75,
Systolic_blood_pressure_20_mm_Hg = 120,
CHD_b = 0,
Valve_disease_b = 0,
Diabetes_b = 0,
BMI_kg_m2 = 25,
Valve_disease_and_diabetes_b = 0) {
  
  # Coefficients for HF risk
  if (sex == "Males") {

Intercept	<- 	-9.2087		#+5
coef_Age_10_y <- 	0.0412 ##
coef_LVH_b	<- 0.9026
coef_Heart_rate_10_bpm	<- 0.0166
coef_Systolic_blood_pressure_20_mm_Hg <- 	0.00804	
coef_CHD_b <- 	1.6079
coef_Valve_disease_b <- 	0.9714
coef_Diabetes_b <- 0.2244

  } else if(sex == 'Females'){

Intercept <- 		-10.7988		#+5
coef_Age_10_y <- 	0.0503  #
coef_LVH_b <- 	1.3402
coef_Heart_rate_10_bpm <- 	0.0105	
coef_Systolic_blood_pressure_20_mm_Hg  <- 	0.00337	
coef_CHD_b <- 	1.5549
coef_Valve_disease_b <- 	1.3929
coef_Diabetes_b <- 	1.3857

coef_BMI_kg_m2 <- 	0.0578
coef_Valve_disease_and_diabetes_b <- 	-0.9860	

  }
  
  
  # Risk Calculation
risk <- coef_Age_10_y * age + #/10
        coef_LVH_b * LVH_b +
        coef_Heart_rate_10_bpm * Heart_rate_10_bpm + #/10
        coef_Systolic_blood_pressure_20_mm_Hg * Systolic_blood_pressure_20_mm_Hg/20  + #/20 
        coef_CHD_b * CHD_b +
        coef_Valve_disease_b * Valve_disease_b +
        coef_Diabetes_b * Diabetes_b +
          Intercept
  
  if(sex=='Females'){
    risk <- risk +
       coef_BMI_kg_m2 * BMI_kg_m2  +
       coef_Valve_disease_and_diabetes_b * (Diabetes_b & Valve_disease_b)
  }
      
  # print(risk)
  
  # Calculate 4-year risk
  hf_risk <- 1/(1+exp(-risk))
  
  #print(hf_risk)
  
  return(hf_risk )  # Return decimal probability
}

apply_hf_risk_wo_risk_factors <- function(input_population,intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 95, 95, age),
      sex = sex,
      bmi = case_when(
        bmi == "normal"     ~ 20,
        bmi == "overweight" ~ 27,
        bmi == "obese"       ~ 40,
        TRUE ~ NA_real_),
      ) 
  
  postp <- postp %>% 
    rowwise() %>% 
   # mutate(list(print(c(age,sex) )))%>% 
    mutate(hf_risk =
             ifelse(age < 35, 0,
                    heart_failure_risk(
                      age = age, 
                      sex = sex,
                      BMI_kg_m2 = bmi
                    )
             )
    ) |> 
###########################################################
  mutate(hf_risk = hf_risk * 2)
###########################################################
  
  input_population <- input_population |> select(-any_of('hf_risk'))
  
  input_population <- left_join(input_population,postp[c('hf_risk','id')], by ='id')
  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  input_population <- ungroup(input_population)
  return(input_population)
}

# Example usage

# heart_failure_risk(age = 80, 
#                    sex = "Males",
#                    Heart_rate_10_bpm = 70,
#                    LVH_b = 0,
#                    Systolic_blood_pressure_20_mm_Hg = 110,
#                    CHD_b = 0,
#                    Valve_disease_b = 0,
#                    Diabetes_b = 0,
#                    BMI_kg_m2 = 25,
#                    Valve_disease_and_diabetes_b=1)



