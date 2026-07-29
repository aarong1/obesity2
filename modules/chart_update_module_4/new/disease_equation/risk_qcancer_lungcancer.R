# risk_qcancer_lung.R

# Female

# (25,84)

risk_qcancer_lungcancer_female <- function(
    age = NULL,
    #sex = NULL,
    b_asthma = 0,
    b_bloodcancer = 0,
    b_breastcancer = 0,
    b_cervicalcancer = 0,
    b_copd = 0,
    b_oralcancer = 0,
    b_ovariancancer = 0,
    b_renalcancer = 0,
    b_uterinecancer = 0,
    bmi = 25,
    ethrisk = 1,
    fh_lungcancer = 0,
    smoke_cat = 1,
    surv = 5,
    #town_1 = 1,
    #town_2 = 1
    town=1
  ){

  survivor = c(
    # 0,
    0.999944269657135,
    0.999894261360168,
    0.999844491481781,
    0.999790668487549,
    0.999729692935944
  )
  
  # /* The conditional arrays */
    
   Iethrisk = c(
      # 0,
      0,
      -0.6162779850671894500000000,
      -1.0042328006097481000000000,
      0.1620831542679997700000000,
      -0.3769892042563389700000000,
      -0.6617252595619067500000000,
      -0.5927929598415709900000000,
      0.1493736725915963700000000,
      -0.4822778759169992200000000
   )
   
   Ismoke = c(
    #0,
    0.6112754576086113500000000,
    1.7358454024106114000000000,
    1.8830939134170273000000000,
    2.3993666780330876000000000
   )

  # /* Applying the fractional polynomial transforms */
  #   /* (which includes scaling)                      */

  dage = age
  dage = dage/10
  age_1 = log(dage)
  age_2 = dage**3
  dbmi = bmi
  dbmi = dbmi/10
  bmi_1 = dbmi**(-2)
  bmi_2 = dbmi**(-2) * log(dbmi)

  # /* Centring the continuous variables */

  age_1 = age_1 - 1.501477479934692
  age_2 = age_2 - 90.417015075683594
  bmi_1 = bmi_1 - 0.151186034083366
  bmi_2 = bmi_2 - 0.142813667654991
  town = town - -0.383295059204102

    
  
  # /* Start of Sum */
  a = 0
    
  
  # /* The conditional sums */
    
  a = a + Iethrisk[ethrisk]
  a = a + Ismoke[smoke_cat]
  
    
  
  # /* Sum from continuous values */

  a = a + age_1 * 5.4496774345837338000000000
  a = a + age_2 * -0.0012617980386000023000000
  a = a + bmi_1 * 4.0768085177264339000000000
  a = a + bmi_2 * -4.3337378643456770000000000

  # /* Sum from boolean values */

  a = a + b_asthma * 0.2819835885385259500000000
  a = a + b_bloodcancer * 0.6589602612666110000000000
  a = a + b_breastcancer * 0.4266513323267903100000000
  a = a + b_cervicalcancer * 0.4554423599948057600000000
  a = a + b_copd * 0.6763831712402721400000000
  a = a + b_oralcancer * 1.0405267066762911000000000
  a = a + b_ovariancancer * 0.4931619422684360200000000
  a = a + b_renalcancer * 0.5537055222785811600000000
  a = a + b_uterinecancer * 0.4238756438929613400000000
  a = a + fh_lungcancer * 0.2787977559153541100000000
  a = a + town * 0.0406920461830567460000000
  #a = a + town_1 * 0.5135882742488984100000000
  #a = a + town_2 * -0.5710205355898726500000000

  # /* Sum from interaction terms */

  a = a + age_1 * (smoke_cat==1) * 2.4893996121290236000000000
  a = a + age_1 * (smoke_cat==2) * 2.4559411049193383000000000
  a = a + age_1 * (smoke_cat==3) * 2.4320914848139008000000000
  a = a + age_1 * (smoke_cat==4) * 1.7511080930265210000000000
  a = a + age_2 * (smoke_cat==1) * -0.0027979410978276986000000
  a = a + age_2 * (smoke_cat==2) * -0.0033489058782917372000000
  a = a + age_2 * (smoke_cat==3) * -0.0028170483278367863000000
  a = a + age_2 * (smoke_cat==4) * -0.0022072853540985198000000
  
  # print(a)
  
  # /* Calculate the score itself */
  score =  (1 - survivor[surv] ^ exp(a) ) ####made changes ----
  #print(score)
  return(score)

}

# Male

risk_qcancer_lungcancer_male <- function(
      age = NULL,
      alcohol_cat6 = 1,
      b_asbestos = 0,
      b_asthma = 0,
      b_bloodcancer = 0,
      b_lung = 0,
      b_copd = 0,
      b_oesgastric = 0,
      b_oralcancer = 0,
      b_renalcancer = 0,
      bmi = 25,
      ethrisk = 1,
      fh_lungcancer = 0,
      smoke_cat = 1,
      surv = 5,
      #town_1 = 0,
      #town_2 = 0
      town=1

  ){

   survivor = c(
    #0,
    0.999953985214233,
    0.999909281730652,
    0.999866127967834,
    0.999819517135620,
    0.999768972396851
  )
  
  # /* The conditional arrays */
    
    Ialcohol = c(
      # 0,
      -0.0976386639663687080000000,
      -0.0879312989839871610000000,
      -0.0278835296992912510000000,
      0.1241299911930168200000000,
      0.2226691104900235800000000
    )
  
   Iethrisk = c(
    # 0,
    0,
    -1.0331236428796484000000000,
    -0.9043527044321475500000000,
    -0.1690393665316634300000000,
    -1.0082250664668821000000000,
    -0.5477502603485426800000000,
    -0.8004555068511054500000000,
    -0.4442850728257539100000000,
    -0.6746227407692440000000000
  )
  
   Ismoke = c(
    #0,
    0.8643511232175890000000000,
    1.7806232376275741000000000,
    1.9642842211163529000000000,
    2.3267148810073270000000000
   )
  
  # /* Applying the fractional polynomial transforms */
  #   /* (which includes scaling)                      */
    
  dage = age
  dage = dage/10
  age_1 = dage
  age_2 = dage*log(dage)
  dbmi = bmi
  dbmi = dbmi/10
  bmi_1 = dbmi ** (-1)
  bmi_2 = ( dbmi**(-1))*log(dbmi)

  # /* Centring the continuous variables */

  age_1 = age_1 - 4.428966999053955
  age_2 = age_2 - 6.591039657592773
  bmi_1 = bmi_1 - 0.380103737115860
  bmi_2 = bmi_2 - 0.367678552865982
  town = town - -0.264977723360062
  
  # /* Start of Sum */
     a = 0
       

  # /* The conditional sums */  

  a = a + Ialcohol[alcohol_cat6]
  a = a + Iethrisk[ethrisk]
  a = a + Ismoke[smoke_cat]
    
  
  # /* Sum from continuous values */
    
  
  a = a + age_1 * 6.4301757663471468000000000  
  a = a + age_2 * -1.9010396695468927000000000  
  a = a + bmi_1 * 1.9425789668620277000000000  
  a = a + bmi_2 * -6.8930979029871811000000000 
  a = a + town * 0.0285745703610741780000000
    
  # /* Sum from boolean values */

    a = a + b_asbestos * 0.6146015094622907500000000
  a = a + b_asthma * 0.1671886251831129500000000
  a = a + b_bloodcancer * 0.6466215627434739300000000
  a = a + b_lung * 0.2532948211137247600000000
  a = a + b_copd * 0.6504005248723407900000000
  a = a + b_oesgastric * 0.5814732073975124000000000
  a = a + b_oralcancer * 1.0504664246702722000000000
  a = a + b_renalcancer * 0.4056966715141066000000000
  a = a + fh_lungcancer * 0.2498954970166089300000000
  #a = a + town_1 * 0.5208833802139940500000000
  #a = a + town_2 * -0.5700541873531250700000000

    
  
  # /* Sum from interaction terms */

  a = a + age_1 * (smoke_cat==1) * 1.6228654481140201000000000
  a = a + age_1 * (smoke_cat==2) * 1.9122750065681651000000000
  a = a + age_1 * (smoke_cat==3) * 1.7352421908685334000000000
  a = a + age_1 * (smoke_cat==4) * 1.7644086878535417000000000
  a = a + age_2 * (smoke_cat==1) * -0.6178538590751476700000000
  a = a + age_2 * (smoke_cat==2) * -0.7424750785474034700000000
  a = a + age_2 * (smoke_cat==3) * -0.6638692019922705100000000
  a = a + age_2 * (smoke_cat==4) * -0.7032683725299220300000000
  
    # print(a)
    
  # /* Calculate the score itself */
  score = 1 - (survivor[surv] ^ exp(a) )
  
  return(score)
  
}


risk_qcancer_lungcancer <- function(age = NULL,
                                       sex = NULL,
                                       bmi = 25,
                                       ethrisk = 1,
                                       smoke_cat = 1,
                                       #town_1 = 0,
                                       #town_2 = 1, #use this - has better matching scaling parameter
                                       town = 1, #took townsend scaling parameter and parameter weight
                                       b_asthma = 0,
                                       b_copd = 0,
                                       b_lung = 0,
                                       b_breastcancer = 0,
                                       b_cervicalcancer = 0,
                                       b_oralcancer = 0,
                                       b_ovariancancer = 0,
                                       b_renalcancer = 0,
                                       b_uterinecancer = 0,
                                       fh_lungcancer = 0,
                                       alcohol_cat6 = 1,
                                       b_asbestos = 0,
                                       b_bloodcancer = 0,
                                       b_oesgastric = 0,
                                       surv = 5
                                       ){
  
  if(sex=='Males'){
    # print(1)
    risk_qcancer_lungcancer_male(  age = age,
                                   alcohol_cat6 = alcohol_cat6,
                                   b_asbestos = b_asbestos,
                                   b_asthma = b_asthma,
                                   b_bloodcancer = b_bloodcancer,
                                   b_lung = b_lung,
                                   b_copd = b_copd,
                                   b_oesgastric = b_oesgastric,
                                   b_oralcancer = b_oralcancer,
                                   b_renalcancer = b_renalcancer,
                                   bmi = bmi,
                                   ethrisk = ethrisk,
                                   fh_lungcancer = fh_lungcancer,
                                   smoke_cat = smoke_cat,
                                   #town_1 = 0,
                                   #town_2 = 0
                                   town=town,
                                   surv = surv
                                   
                                 )
  } else if(sex =='Females'){
    
    # print(2)
    
    risk_qcancer_lungcancer_female(
      age = age,
      b_asthma = b_asthma,
      b_bloodcancer = b_bloodcancer,
      b_breastcancer = b_breastcancer,
      b_cervicalcancer = b_cervicalcancer,
      b_copd = b_copd,
      b_oralcancer = b_oralcancer,
      b_ovariancancer = b_ovariancancer,
      b_renalcancer = b_renalcancer,
      b_uterinecancer = b_uterinecancer,
      bmi = bmi,
      ethrisk = ethrisk,
      fh_lungcancer = fh_lungcancer,
      smoke_cat = smoke_cat,
      surv = surv,
      #town_1 = 1,
      #town_2 = 1
      town=town
    )
  }
}

apply_lung_cancer_risk_wo_risk_factors <- apply_lungcancer_risk_wo_risk_factors <- function(input_population, intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 90, 90, age),
      sex,
      # Encode BMI categories
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 30,
        bmi == "obese"      ~ 38,
        TRUE                ~ NA_real_),
      custom_townsend_score_dz,
      # b_asthma == asthma != 0
      # b_copd == copd != 0
      ethrisk,
      smoke_cat = case_when(
        smoking == 'current_smoker' ~ 4,
        smoking == 'former' ~ 2,
        smoking == 'never_smoked' ~1,
        TRUE ~ 1
      ),
      alcohol_cat6 = case_when(
        alcohol == 'no_risk'  ~ 1,
        alcohol == 'lower_risk' ~ 3,
        alcohol == 'higher_risk'  ~ 5,
        TRUE                           ~ 1),
      
      b_bloodcancer = blood_cancer != 0,
      b_breastcancer = female_breast_cancer != 0,
      b_cervicalcancer = cervical_cancer != 0,
      b_oralcancer = oral_cancer != 0,
      b_ovariancancer = ovarian_cancer != 0,
      b_renalcancer = renal_cancer != 0,
      b_uterinecancer = uterine_cancer != 0,
      b_oesgastric = osteogastric_cancer != 0
      ) 
  
  if( 'asthma' %in% names(postp) ){
    postp <- postp %>% 
      mutate(
      b_asthma = asthma != 0
      )
  }else{    
    postp <- postp %>% 
      mutate(
        b_asthma = 0 
      )
  }
  
  if( 'copd' %in% names(postp) ){
    postp <- postp %>% 
      mutate(
        b_copd = copd != 0
      )
  }else{    
    postp <- postp %>% 
    mutate(
      b_copd = 0 
    )
  }

  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(lungcancer_risk =
             ifelse(age < 25, 0, # changed this after sppg results
##############################################################################################
                    # * 10  
                      risk_qcancer_lungcancer(  #### made changes ####
##############################################################################################
                      age = age, 
                      sex = sex,
                      bmi = bmi,
                      town = custom_townsend_score_dz,
                      b_asthma = b_asthma,
                      b_copd = b_copd,
                      ethrisk = ethrisk,
                      smoke_cat = smoke_cat,
                      alcohol_cat6 = alcohol_cat6
                    )) * 1.6
           
    )
  input_population <- input_population |> select(-any_of('lungcancer_risk'))
  
  input_population <- left_join(input_population,postp[c('lungcancer_risk','id')], by ='id')
  # initial_time_zero_population$stroke_risk <- postp$stroke_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  
  input_population <- ungroup(input_population)
  
  return(input_population)
}

apply_lungcancer_risk <- function(y, intervention = 1 ) {
  
  postp1 <- 
    y %>% 
    filter(year == max(year,na.rm = TRUE)) %>% 
    pivot_longer(cols = - any_of(base_population_demographic_column_names)
    ) %>% 
    mutate(category = str_extract(string = name,pattern = 'cholesterol|overweight|bp|smoking|diabetic|atrial_fibrillation')) %>% 
    group_by(id,category) %>% 
    arrange(desc(value)) %>% 
    slice_head() %>%
    ungroup() %>% 
    pivot_wider(id_cols = -value,names_from = category,values_from = name) 
  
  dim(y)
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
    mutate(lung_cancer_risk =
             case_when(age < 30 ~ 0,
                       age > 85 ~ 0.5,
                       T ~ rk_qcancer_lungcancer_gendered(
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
           valid = case_when(age < 30 ~ 'shouldnt',
                            age > 85 ~ 'shoudlnt',
                            T ~ 'should')
           
    ) |> 
    
###########################################################
  mutate(lung_cancer_risk = lung_cancer_risk)
###########################################################

  y <- left_join(y,postp[c('lung_cancer_risk','id')], by ='id')
  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  
  return(y)
}


age = NULL;
alcohol_cat6 = 1;
b_asbestos = 0;
b_asthma = 0;
b_bloodcancer = 0;
b_lung = 0;
b_copd = 0;
b_oesgastric = 0;
b_oralcancer = 0;
b_renalcancer = 0;
bmi = 25;
ethrisk = 1;
fh_lungcancer = 0;
smoke_cat = 1;
surv = 5;
town=1



risk_qcancer_lungcancer_male( age = 90,
                              alcohol_cat6 = 1,
                              b_asbestos = 0,
                              b_asthma = 0,
                              b_bloodcancer = 0,
                              b_lung = 0,
                              b_copd = 0,
                              b_oesgastric = 0,
                              b_oralcancer = 0,
                              b_renalcancer = 0,
                              bmi = 35,
                              ethrisk = 1,
                              fh_lungcancer = 0,
                              smoke_cat = 1,
                              surv = 5,
                              #town_1 = 0,
                              #town_2 = 0
                              town=1)

# 
 risk_qcancer_lungcancer_male(90)
# 
 risk_qcancer_lungcancer_female(90)

 