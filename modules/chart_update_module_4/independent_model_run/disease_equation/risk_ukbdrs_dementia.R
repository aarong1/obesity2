# UK Biobank Dementia Risk Score (UKBDRS)
# https://mentalhealth.bmj.com/content/26/1/e300719

###### calculates the risk of developing dementia over the next 14 years

# age
# parental_history

# education
# deprivation
##### townsend deprivation
# diabetes
# depression
# stroke
# hypertension
##### Greater than 140/90 mm Hg Systolic/Diastolic
########################################################################  

######  Blood pressure	Systolic (top number) mm Hg	Diastolic (bottom number) mm Hg
######  Low	Less than 90	Less than 60
######  Optimal	Less than 120	Less than 80
######  Normal	120–129	80–84
######  Normal to high	130–139	85–89
######  High	Greater than 140	Greater than 90

########################################################################

# high_cholesterol

##### hypercholesterolemia/ Dyslipidaemia
##### LDL > 130mg/dL

# sex
##### is male sex??

# lives_alone


#################
# 14 year risk 
#################

age =60;
sex = 'Males';
townsend = 0;
diabetes = 0;
hypertension = 0;
high_cholesterol = 0;
parental_history = 0;
education_years = 14;
depression = 0;
stroke = 0;
lives_alone = 0;
baseline_survival = 0.9916195

risk_ukbdrs_dementia <- function(age =NULL,
                                    sex = NULL,
                                    townsend = NULL,
                                    diabetes = 0,
                                    hypertension = 0,
                                    high_cholesterol = 0,
                                    parental_history = 0,
                                    education_years = 14,
                                    depression = 0,
                                    stroke = 0,
                                    lives_alone = 0,
                                    baseline_survival = 0.9916195) {
  
  # Beta coefficients for the predictors (from Table 1 in the paper)
  beta_coefficients <- c(
    age = 0.178,
    sex = 0.169,
    diabetes = 0.536,
    townsend = 0.228, #townsend = 0.228,  
    ###### changed manually ###########
    #https://mentalhealth.bmj.com/content/ebmental/suppl/2023/07/19/bmjment-2023-300719.DC1/bmjment-2023-300719supp001_data_supplement.pdf
    parental_history = 0.431,
    education = -0.041,
    depression = 0.556,
    stroke = 0.655,
    hypertension = 0.159,
    high_cholesterol = 0.104,
    lives_alone = 0.141
  )
  
  # Linear predictor calculation
  linear_predictor <- beta_coefficients[["age"]] * (age-60) + 
    beta_coefficients[["sex"]] * (sex == 'Males') + #male
    beta_coefficients[["diabetes"]] * diabetes +
    beta_coefficients[["parental_history"]] * parental_history +
    beta_coefficients[["high_cholesterol"]] * high_cholesterol +
    beta_coefficients[["hypertension"]] * hypertension +
    beta_coefficients[["education"]] * (education_years - 14) +
    beta_coefficients[["townsend"]] * townsend +
    beta_coefficients[["depression"]] * depression +
    beta_coefficients[["stroke"]] * stroke +
    beta_coefficients[["lives_alone"]] * lives_alone
  
  # Calculate 14-year risk using baseline survival
  risk_probability <- 1 - baseline_survival^exp(linear_predictor)
  
  return(risk_probability)
}

apply_dementia_ukbdrs_14yr_risk_wo_risk_factors <- function(input_population,intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 85, 85, age),
      sex,
      townsend_score) 
  
  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(dementia_risk =
             0.65 * ifelse(age < 45, 0,
                    risk_ukbdrs_dementia(
                         age = age, 
                         sex = sex,
                         townsend = (townsend_quintile %in% c(1,2) ) , # changed to quantile
                       )
             )
    )

  input_population <- input_population |> select(-any_of('dementia_risk'))

  input_population <- left_join(input_population,postp[c('dementia_risk','id')], by ='id')
  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  input_population <- ungroup(input_population)

  return(input_population)

}


apply_dementia_ukbdrs_risk <- function(input_population,intervention=1){

  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE)) %>% 
    pivot_longer(cols = - any_of(base_population_demographic_column_names)
    ) %>% 
    mutate(category = str_extract(string = name,pattern = 'cholesterol|overweight|bp|smoking|diabetic|atrial_fibrillation')) %>% 
    group_by(id,category) %>% 
    arrange(desc(value)) %>% 
    slice_head() %>%
    ungroup() %>% 
    pivot_wider(id_cols = -value,names_from = category,values_from = name) 
  
  dim(input_population)
  # 2117   31
  dim( postp1)
  # 2111   24
  
  postp <- postp1 %>%
    mutate(
      
      # sex = case_when(
      # sex == 'Women'~1,
      # sex == 'Men'),
      
      bp=case_when(bp == 'high_bp'~150,
                   bp == 'normal_bp'~100),
      cholesterol=case_when(
        cholesterol == 'normal_cholesterol'~3.5,
        cholesterol == 'high_cholesterol'~10),
      type2 = case_when(
        diabetic == 'non_diabetic'~0,
        diabetic == 'diabetic'~1),
      bmi=case_when(overweight=='overweight'~35,
                    overweight=='not_overweight'~20),
      smoking=case_when(
        smoking=='smoking_currently'~4,
        smoking=='smoking_used'~2,
        smoking=='smoking_never'~1),
      atrial_fibrillation=case_when(
        atrial_fibrillation=='atrial_fibrillation'~1*intervention) # change back to `intervention`
    ) 
  
  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(dementia_risk =
             case_when(age < 40 ~ 0,
                       age > 95 ~ 0.5,
                       T ~ risk_ukbdrs_dementia(
                         age = age, 
                         sex = sex,
                         #bmi = bmi, 
                         #smoke_cat = smoking, 
                         #cholesterol_ratio = cholesterol, 
                         #sbp = bp, 
                         #b_type2 = type2, 
                         townsend = townsend_score, 
                         #b_AF = atrial_fibrillation 
                         )
                       ),
           valid= case_when(age < 30 ~ 'shouldnt',
                            age > 85 ~ 'shoudlnt',
                            T ~ 'should')
           
    )
  
  input_population <- left_join(input_population,postp[c('dementia_risk','id')], by ='id')
  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  
  return(input_population)
}



# Example usage with dummy data
(example_risk <- risk_ukbdrs_dementia(
  age = 90,
  parental_history = 0,  # 1 if yes, 0 if no
  education_years = 14,  # Number of years in education
  townsend = 0,       # Townsend deprivation score
  diabetes = 0,          # 1 if yes, 0 if no
  depression = 0,        # 1 if yes, 0 if no
  stroke = 0,            # 1 if yes, 0 if no
  hypertension = 0,      # 1 if yes, 0 if no
  high_cholesterol = ,  # 1 if yes, 0 if no
  sex = 0,               # 1 for male, 0 for female
  lives_alone = 0        # 1 if lives alone, 0 otherwise
))
#print(paste("14-year dementia risk:", round(example_risk, 6)*100, "%"))

