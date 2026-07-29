# risk_qdiabetes_diabetes

age = 35;sex = 'Males';bmi =15;smoking = 2;cholesterol_ratio = 2;bp = 100;type2 = 1;townsend_score = 3; atrial_fibrillation =0

risk_qdiabetes_diabetes <- function(  
    age = NULL,
    sex = NULL,
    town = NULL,
    b_corticosteroids=0,
    b_cvd=0,
    b_treatedhyp=0,
    bmi = 25,
    ethrisk=1,
    fh_diab=0,
    smoke_cat = 1,
    surv = 10
    ) {
  if (sex == 'Males') {
    
    # QDiabetes-2013 Type 2 Diabetes risk score for males
      # Survivor probabilities array (pre-calculated)
    
    
      survivor <- c(
        # 0,
        0.998213708400726,
        0.996353209018707,
        0.994382798671722,
        0.992213606834412,
        0.989733397960663,
        0.987064540386200,
        0.984254062175751,
        0.981255292892456,
        0.977990627288818,
        0.974455237388611,
        0.970843732357025,
        0.967315018177032,
        0.963437378406525,
        0.959633111953735,
        0.955690681934357
      )

      # Ethnicity risk factors
      Iethrisk <- c(
        0,                   # white or not stated
        1.2366090720913343,  # indian
        1.4716746107789032,  # pakisani  
        1.8073235649498174,  # bangladeshi
        1.2056055595936399,  # other_asian
        0.6032369975938766,  # caribbean
        0.9095436207452737,  # african
        0.9137604632927513,  # chinese
        0.7123719045990779   # other
      )
      
      # Smoking categories
      Ismoke <- c(
        0,                     # non smoker
        0.16182385823959777,   # ex-smoker
        0.1902020385619117,    # light smoker
        0.3210636179312467,    # modeerate smoker
        0.41400013017974946    # heavy smoker
      )
      
      # Fractional polynomial transforms
      dage <- age / 10
      age_1 <- log(dage)
      age_2 <- dage^3
      dbmi <- bmi / 10
      bmi_1 <- dbmi^2
      bmi_2 <- dbmi^3
      
      # Centering continuous variables
      age_1 <- age_1 - 1.496771812438965
      age_2 <- age_2 - 89.1495590209961
      bmi_1 <- bmi_1 - 6.832604885101318
      bmi_2 <- bmi_2 - 17.859918594360352
      town <- town - -0.132148191332817
      
      # Initializing the risk score
      a <- 0
      
      # Adding ethnicity and smoking category contributions
      a <- a + Iethrisk[ethrisk + 1]
      a <- a + Ismoke[smoke_cat + 1]
      
      # Adding contributions from continuous variables
      a <- a + age_1 * 4.420559832337168
      a <- a + age_2 * -0.004113223829939419
      a <- a + bmi_1 * 1.1169895991721528
      a <- a + bmi_2 * -0.17935295302512691
      a <- a + town * 0.029153081590382265
      
      # Adding contributions from boolean risk factors
      a <- a + b_corticosteroids * 0.20598119799056924
      a <- a + b_cvd * 0.3914728454990503
      a <- a + b_treatedhyp * 0.5010787979849035
      a <- a + fh_diab * 0.8385800403428994
      
      # Adding interaction terms
      a <- a + age_1 * bmi_1 * 0.50510312537680635
      a <- a + age_1 * bmi_2 * -0.1375233635462656
      a <- a + age_1 * fh_diab * -1.1463560542602569
      a <- a + age_2 * bmi_1 * -0.00158006864527727
      a <- a + age_2 * bmi_2 * 0.00033940900578240623
      a <- a + age_2 * fh_diab * 0.001852416035398126
      
  } else if (sex == 'Females') {
    
    # Survivor probabilities array (pre-calculated)
    
    survivor <- c(
      #0,
      0.998714804649353,
      0.997435748577118,
      0.996052920818329,
      0.994562506675720,
      0.992949724197388,
      0.991141080856323,
      0.989293158054352,
      0.987293541431427,
      0.985133886337280,
      0.982810735702515,
      0.980465650558472,
      0.978020071983337,
      0.975493073463440,
      0.972945988178253,
      0.970350146293640
    )
    
    # Ethnicity risk factors
    Iethrisk <- c(
      #0,
      0,
      1.2672136244963337,
      1.4277605208830098,
      1.8624060798103199,
      1.2379988338989651,
      0.4709034172907678,
      0.34764009017031605,
      1.1587283467731935,
      0.7335499325010315
    )
    
    # Smoking categories
    Ismoke <- c(
      0,
      0.10125370249475051,
      0.19155205643806134,
      0.3091894136143334,
      0.4646730392693821
    )
    
    # Fractional polynomial transforms
    dage <- age / 10
    age_1 <- sqrt(dage)
    age_2 <- dage^3
    dbmi <- bmi / 10
    bmi_1 <- dbmi
    bmi_2 <- dbmi^3
    
    # Centering continuous variables
    age_1 <- age_1 - 2.135220289230347
    age_2 <- age_2 - 94.76679992675781
    bmi_1 <- bmi_1 - 2.549620866775513
    bmi_2 <- bmi_2 - 16.573980331420898
    town <- town - (-0.224075347185135)
    
    # Initializing the risk score
    a <- 0
    
    # Adding ethnicity and smoking category contributions
    a <- a + Iethrisk[ethrisk + 1]
    a <- a + Ismoke[smoke_cat + 1]
    
    # Adding contributions from continuous variables
    a <- a + age_1 * 4.384833121298967
    a <- a + age_2 * -0.004976396440654115
    a <- a + bmi_1 * 3.375333632606433
    a <- a + bmi_2 * -0.06316284886673183
    a <- a + town * 0.0432726992998636
    
    # Adding contributions from boolean risk factors
    a <- a + b_corticosteroids * 0.2681990966241487
    a <- a + b_cvd * 0.3596176830984253
    a <- a + b_treatedhyp * 0.5314598436974726
    a <- a + fh_diab * 0.7315358845837641
    
    # Adding interaction terms
    a <- a + age_1 * bmi_1 * 1.303783287399799
    a <- a + age_1 * bmi_2 * -0.07082937177690461
    a <- a + age_1 * fh_diab * -0.7968266815834252
    a <- a + age_2 * bmi_1 * -0.006772532376127855
    a <- a + age_2 * bmi_2 * 0.00023749807286661167
    a <- a + age_2 * fh_diab * 0.0017048228889394394

  }
  
  # Calculate the diabetes risk score
  score <- (1 - (survivor[surv]^exp(a)))
  
  return(score)
  
}

apply_diabetes_risk_wo_risk_factors <- function(input_population,intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 85, 85, age),
      sex,
      bmi = case_when(
        bmi == "normal"     ~ 20,
        bmi == "overweight" ~ 27,
        bmi == "obese"       ~ 40,
        TRUE ~ NA_real_),
      townsend_score) 
  
  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(diabetes_risk =
             ifelse(age < 20, 0,
                    risk_qdiabetes_diabetes(
                      age = age, 
                      sex = sex,
                      bmi = bmi,
                      town = townsend_score
                    )
                )
    )
  input_population <- input_population |> select(-any_of('diabetes_risk'))
  
  input_population <- left_join(input_population,postp[c('diabetes_risk','id')], by ='id')
  # initial_time_zero_population$stroke_risk <- postp$stroke_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  input_population <- ungroup(input_population)
  return(input_population)
}


apply_diabetes_risk <- function(input_population,intervention=1){
  
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
        atrial_fibrillation=='atrial_fibrillation'~(1*intervention)) # change back to `intervention`
    ) 
  
  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(diabetes_risk =
             case_when(age < 30 ~ 0,
                       age > 85 ~ 0.5,
                       T ~ risk_qdiabetes_diabetes(
                         age = age, 
                         sex = sex,
                         bmi = bmi, 
                         smoke_cat = smoking, 
                         #cholesterol_ratio = cholesterol, 
                         #sbp = bp, 
                         #b_type2 = type2, 
                         town = townsend_score, 
                         #b_AF = atrial_fibrillation 
                         )),
           valid= case_when(age < 30 ~ 'shouldnt',
                            age > 85 ~ 'shoudlnt',
                            T ~ 'should')
           ) |> 
##########################################################################################
    mutate(diabetes_risk = diabetes_risk * 0.4)
##########################################################################################  
  input_population <- left_join(input_population,postp[c('diabetes_risk','id')], by ='id')
  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  
  return(input_population)
}

###########################################
############ TEST ############
###########################################

#  x <- instantiate_base_pop(scale_down_factor = model_specification$population$scale_down_factor )    %>%
#    #this applies Age, Sex, Deprivation
#    apply_bmi_lifestyle_parameter() %>%
#    apply_smoking_lifestyle_parameter() %>%
#    apply_cholesterol_physiological_parameter() %>%
#    apply_hypertension_physiological_parameter() %>%
#    apply_diabetes_physiological_parameter() %>%
#    apply_atrial_fibrillation_physiological_parameter()# %>%
#    # apply_cvd_risk() %>%
#    # mutate(year_risk = transform_10y_probability_to_1y(risk))
# # 
# apply_diabetes_risk(x) %>%
#   pull(diabetes_risk) %>%
#   sum()

###########################################
############ TEST ############
###########################################

# age = 55
# sex = 'male'
# b_AF = 1
# # b_CCF = 0
# # b_chd = 1
# # b_ra = 0
# # b_renal = 0
# # b_treatedhyp = 1
# # b_type1 = 0
# b_type2 = 1
# # b_valvular = 0
# bmi = 25
# # ethrisk = 2
# # fh_cvd = 0
# cholesterol_ratio = 4.5
# sbp = 120
# smoke_cat = 1
# surv = 10
# town = 1
# 
# Example usage:
risk_qdiabetes_diabetes(
  age = 85,
  sex = 'Females',
  #b_AF = 0,
  # b_CCF = 0,
  # b_chd = 1,
  # b_ra = 0,
  # b_renal = 0,
  # b_treatedhyp = 1,
  # b_type1 = 0,
  #b_type2 = 1,
  # b_valvular = 0,
  bmi = 25,
  # ethrisk = 2,
  # fh_cvd = 0,
  #cholesterol_ratio = 2.5,
  #sbp = 120,
  smoke_cat = 2,
  surv = 10,
  town = 4
)
