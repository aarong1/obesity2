# risk_qstroke_stroke

age = 35;sex = 'Males';bmi =15;smoking = 2;cholesterol_ratio = 2;bp = 100;type2 = 1;townsend_score = 3; atrial_fibrillation =0

 
risk_qstroke_stroke <- function(  
  age = NULL,
  sex = NULL,
  b_AF = 0,
  # b_CCF = 0,
  # b_chd = 1,
  # b_ra = 0,
  # b_renal = 0,
  # b_treatedhyp = 0,
  # b_type1 = 0,
  b_type2 = 0,
  # b_valvular = 0,
  bmi = 25,
  # ethrisk = 2,
  # fh_cvd = 0,
  cholesterol_ratio = 4.5,
  sbp = 120,
  smoke_cat = 1,
  surv = 10,
  town = 1.5) {
  if (sex == 'Males') {
    # Survivor array
    survivor <- c(
      #0,
      0.999487161636353,
      0.998943150043488,
      0.998373985290527,
      0.997744083404541,
      0.997065007686615,
      0.996341764926910,
      0.995574831962585,
      0.994802832603455,
      0.993988394737244,
      0.993128657341003,
      0.992210268974304,
      0.991193234920502,
      0.990198373794556,
      0.989128828048706,
      0.988554954528809
    )
    
    
# white or not stated
# indian
# pakisani  
# bangladeshi
# other_asian
# caribbean
# african
# chinese
# other
    
    
    # Ethnicity coefficients
    Iethrisk <- c(
      0,
      0,
      -0.07643684764582899,
      0.057135765550468801,
      0.075016850934192747,
      -0.025509341829454024,
      -0.019675589013744801,
      -0.17478874930906932,
      -0.40990101010865926,
      -0.30782656439075029
    )
    
# white or not stated
# indian
# pakisani  
# bangladeshi
# other_asian
# caribbean
# african
# chinese
# other
    
    # Smoking coefficients
    Ismoke <- c(
      0,
      0.1552773552459725,
      0.52962269557138697,
      0.5329542987864534,
      0.72496487357647421
    )
    
    # Apply fractional polynomial transformations
    dage <- age / 10
    age_1 <- dage ^ (-1)
    age_2 <- dage ^ (-1) * log(dage)
    
    dbmi <- bmi / 10
    bmi_1 <- dbmi ^ (-2)
    bmi_2 <- dbmi ^ (-2) * log(dbmi)
    
    # Center the continuous variables
    age_1 <- age_1 - 0.223224431276321
    age_2 <- age_2 - 0.334742367267609
    bmi_1 <- bmi_1 - 0.144959196448326
    bmi_2 <- bmi_2 - 0.139980062842369
    cholesterol_ratio <- cholesterol_ratio - 4.374470233917236
    sbp <- sbp - 131.99687194824219
    town <- town - (-0.014665771275759)
    
    # Initialize sum
    a <- 0
    
    # Add ethnicity and smoking contributions
    # a <- a + Iethrisk[ethrisk]
    a <- a + Ismoke[smoke_cat]
    
    # Add continuous variable contributions
    a <- a + age_1 * -3.967393626176547
    a <- a + age_2 * -39.03474202119515
    a <- a + bmi_1 * 5.1085638322885796
    a <- a + bmi_2 * -10.340887461957003
    a <- a + cholesterol_ratio * 0.07402541806167742
    a <- a + sbp * 0.014126612350531642
    a <- a + town * 0.04102010300548117
    
    # Add boolean variable contributions
    a <- a + b_AF * 0.46889565652525278
    # a <- a + b_CCF * 0.88498265959825995
    # a <- a + b_chd * 0.94054699037801903
    # a <- a + b_ra * 0.21653833226301319
    # a <- a + b_renal * 0.33268307252482043
    # a <- a + b_treatedhyp * 0.61163034043473719
    # a <- a + b_type1 * 1.2863652184197687
    a <- a + b_type2 * 0.6758136449121166
    # a <- a + b_valvular * 0.87676952423597576
    # a <- a + fh_cvd * 0.25551606683528055
    
    # Add interaction terms
    if (smoke_cat == 1)
      a <- a + age_1 * 0.1319310717787916
    if (smoke_cat == 2)
      a <- a + age_1 * -3.7777100480315955
    if (smoke_cat == 3)
      a <- a + age_1 * -2.0141772764248631
    if (smoke_cat == 4)
      a <- a + age_1 * -3.2046066813681535
    if (smoke_cat == 1)
      a <- a + age_2 * 1.5734752401338576
    if (smoke_cat == 2)
      a <- a + age_2 * 10.100456626340586
    if (smoke_cat == 3)
      a <- a + age_2 * 5.9354809751853459
    if (smoke_cat == 4)
      a <- a + age_2 * 9.2403842314780817
    
    # Final stroke risk score calculation
    score <-  (1 - (survivor[surv] ^ exp(a)))
    
  } else if (sex == 'Females') {
    # Survivor probabilities array (pre-calculated)
    survivor <- c(
      #0,
      0.999592542648315,
      0.999157011508942,
      0.998699188232422,
      0.998193085193634,
      0.997646689414978,
      0.997065961360931,
      0.996467173099518,
      0.995843231678009,
      0.995173335075378,
      0.994471669197083,
      0.993704795837402,
      0.992924809455872,
      0.992103338241577,
      0.991260945796967,
      0.990782141685486
    )
    
    # Ethnicity risk factors
    Iethrisk <- c(
      0,
      0,
      -0.030539492957658849,
      0.442761118284029310,
      0.267379586291984220,
      -0.259830836446871270,
      -0.048801241913734243,
      -0.278855311273075720,
      -0.676714357232730430,
      -0.161153500919018560
    )
    
    # Smoking categories
    Ismoke <- c(
      0,
      0.149830557525171190,
      0.451918349357311070,
      0.614633548269955530,
      0.813178512259268070
    )
    
    # Fractional polynomial transforms
    dage <- age / 10
    age_1 <- dage ^ 2
    age_2 <- dage ^ 3
    dbmi <- bmi / 10
    bmi_1 <- dbmi ^ (-2)
    bmi_2 <- dbmi ^ (-2) * log(dbmi)
    
    # Centering continuous variables
    age_1 <- age_1 - 20.710655212402344
    age_2 <- age_2 - 94.252044677734375
    bmi_1 <- bmi_1 - 0.152111470699310
    bmi_2 <- bmi_2 - 0.143223732709885
    cholesterol_ratio <- cholesterol_ratio - 3.597237586975098
    sbp <- sbp - 127.181053161621090
    town <- town - (-0.092155806720257)
    
    # Initializing the risk score
    a <- 0
    
    # Adding ethnicity and smoking category contributions
    # a <- a + Iethrisk[ethrisk + 1]
    a <- a + Ismoke[smoke_cat ]
    
    # Adding contributions from continuous variables
    a <- a + age_1 * 0.16973205182974782
    a <- a + age_2 * -0.009348980513922285
    a <- a + bmi_1 * 2.3158227325733081
    a <- a + bmi_2 * -8.39273889850246
    a <- a + cholesterol_ratio * 0.07638183067951346
    a <- a + sbp * 0.01101065488190087
    a <- a + town * 0.05692825383001629
    
    # Adding contributions from boolean risk factors
    a <- a + b_AF * 1.1236185329326394
    # a <- a + b_CCF * 1.0018266666317022
    # a <- a + b_chd * 1.1384605450143492
    # a <- a + b_ra * 0.28950194486119213
    # a <- a + b_renal * 0.38404332504969602
    # a <- a + b_treatedhyp * 0.600058993635894
    # a <- a + b_type1 * 1.2931653635533871
    a <- a + b_type2 * 0.7743227044341612
    # a <- a + b_valvular * 0.8823685364057472
    # a <- a + fh_cvd * 0.28518074726880477
    
    # Adding interaction terms for age and other variables
    a <- a + age_1 * (smoke_cat == 1) * -0.0021738924907283397
    a <- a + age_1 * (smoke_cat == 2) * 0.009295835469160798
    a <- a + age_1 * (smoke_cat == 3) * -0.02384452033906373
    a <- a + age_1 * (smoke_cat == 4) * -0.04420811685047747
    a <- a + age_1 * b_AF * -0.03787466607632756
    # a <- a + age_1 * b_CCF * -0.07033389327503668
    # a <- a + age_1 * b_chd * -0.1242795995222882
    # a <- a + age_1 * b_treatedhyp * -0.05475338244979077
    # a <- a + age_1 * b_type1 * -0.01043258488081836
    a <- a + age_1 * b_type2 * -0.05435272440634235
    # a <- a + age_1 * b_valvular * -0.06812947862499538
    a <- a + age_1 * bmi_1 * 0.036646806960760316
    a <- a + age_1 * bmi_2 * 0.8374396689614353
    # a <- a + age_1 * fh_cvd * -0.02095889478428734
    a <- a + age_1 * sbp * -0.00005125668412590844
    a <- a + age_1 * town * -0.0016537747988553985
    a <- a + age_2 * (smoke_cat == 1) * 0.00004179828040074173
    a <- a + age_2 * (smoke_cat == 2) * -0.001512722384301164
    a <- a + age_2 * (smoke_cat == 3) * 0.0018092337569956974
    a <- a + age_2 * (smoke_cat == 4) * 0.003759135804003676
    a <- a + age_2 * b_AF * 0.002630964121327297
    # a <- a + age_2 * b_CCF * 0.00522377183204137
    # a <- a + age_2 * b_chd * 0.01057659215923173
    # a <- a + age_2 * b_treatedhyp * 0.004588680134055191
    # a <- a + age_2 * b_type1 * -0.00318706892108062
    a <- a + age_2 * b_type2 * 0.0044700226930346355
    # a <- a + age_2 * b_valvular * 0.005243337024419571
    a <- a + age_2 * bmi_1 * -0.008295332779445844
    a <- a + age_2 * bmi_2 * -0.06428026854117822
    # a <- a + age_2 * fh_cvd * 0.0014106757779590851
    a <- a + age_2 * sbp * -0.00001793661589096183
    a <- a + age_2 * town * 0.000003751890332394288
    
    # Calculate the stroke risk score
    score <-  (1 - (survivor[surv] ^ exp(a)))
    
  }
  
  return(score)
}

apply_stroke_risk <- function(y,intervention=1){

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
      atrial_fibrillation=='atrial_fibrillation'~1*intervention) # change back to `intervention`
) 

postp <- postp %>% 
  rowwise() %>% 
  # mutate(list(print(c(age,sex) )))%>% 
  mutate(stroke_risk =
           case_when(age < 30 ~ 0,
                     age > 85 ~ 0.5,
                     T ~ risk_qstroke_stroke(
                           age = age, 
                           sex = sex,
                           bmi = bmi, 
                           smoke_cat = smoking, 
                           cholesterol_ratio = cholesterol, 
                           sbp = bp, 
                           b_type2 = type2, 
                           town = townsend_score, 
                           b_AF = atrial_fibrillation )),
                     valid= case_when(age < 30 ~ 'shouldnt',
                                      age > 85 ~ 'shoudlnt',
                                      T ~ 'should')

  )

y <- y |> select(-any_of('stroke_risk'))
y <- left_join(y,postp[c('stroke_risk','id')], by ='id')

# initial_time_zero_population$stroke_risk <- postp$stroke_risk
# initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]

return(y)
}

###########################################
############ TEST ############
###########################################

# x <- instantiate_base_pop(scale_down_factor = model_specification$population$scale_down_factor )    %>%
#   #this applies Age, Sex, Deprivation
#   apply_bmi_lifestyle_parameter() %>%
#   apply_smoking_lifestyle_parameter() %>%
#   apply_cholesterol_physiological_parameter() %>%
#   apply_hypertension_physiological_parameter() %>%
#   apply_diabetes_physiological_parameter() %>%
#   apply_atrial_fibrillation_physiological_parameter() %>%
#   apply_cvd_risk() %>%
#   mutate(year_risk = transform_10y_probability_to_1y(risk))
# 
# apply_stroke_risk(x) %>%
#   pull(stroke_risk) %>%
#   sum()

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
risk_qstroke_stroke(
  age = 5,
  sex = 'Females',
  b_AF = 0,
  # b_CCF = 0,
  # b_chd = 1,
  # b_ra = 0,
  # b_renal = 0,
  # b_treatedhyp = 1,
  # b_type1 = 0,
  b_type2 = 1,
  # b_valvular = 0,
  bmi = 25,
  # ethrisk = 2,
  # fh_cvd = 0,
  cholesterol_ratio = 2.5,
  sbp = 120,
  smoke_cat = 2,
  surv = 10,
  town = 4
)
