
# postp$risk %>% range(na.rm = T)
# postp$risk

# "The obesity paradox"

# # Encode risk covariates 
# 
# #gender
# list( women = 1, men = 0 )
# 
# # smoking
# list( smoking_currently = 4, smoking_used = 2, smoking_ever = 1 )
# 
# # cholesterol_HDL_ratio
# list( high_cholesterol = 8.5, low_cholesterol = 4.5 )

# # BMI
# bmi = list( overweight = 27.5,non_overweight = 22.5 )

# dbmi = bmi
# dbmi=dbmi/10
# bmi_1 = dbmi^(-2)
# bmi_2 = (dbmi^(-2))*log(dbmi)

# # systolic_blood_pressure
# list( high_bp = 150,normal = 105 )

# #type 2
# list( diabetic = 1, non_diabetic = 0 )

# # age
# "Age of patients must be between 25 and 84."
# #Applying the fractional polynomial transforms 
#         #(which includes scaling)                      

#         dage = age
#         dage=dage/10
#         age_1 = dage^(-2)
#         age_2 = dage^(3)



############################################################

############################################################


#############-----------------------------------------
# TEST 
#############-----------------------------------------
#qrisk(age = 40, sex = 'Males', bmi=30,smoking = 1,cholesterol_ratio = 5, bp = 100,type2 = 1,townsend_score = 5, atrial_fibrillation =1)

#age = 35;sex = 1;bmi =15;smoking = 2;cholesterol_ratio = 5;bp = 100;type2 = 1;townsend_score = 3; atrial_fibrillation =0

#plot(-5:10,(1 - (0.988876402378082)^exp(-5:10)))


## from qstroke
# atrial_fibrillation = 0,
# # b_CCF = 0,
# # b_chd = 1,
# # b_ra = 0,
# # b_renal = 0,
# # b_treatedhyp = 0,
# # b_type1 = 0,
# type2 = 0,
# # b_valvular = 0,
# bmi = 25,
# # ethrisk = 2,
# # fh_cvd = 0,
# cholesterol_ratio = 4.5,
# sbp = 120,
# smoke_cat = 1,
# surv = 10 #,

qrisk <- function(age = NULL, 
                  sex = NULL, 
                  townsend_score = NULL,
                  bmi = 25, 
                  smoke_cat = 1,
                  cholesterol_ratio = 4.5,
                  bp = 120, #systolic
                  type2 = 0,
                  atrial_fibrillation = 0) {
  
  if(sex =='Females'){
    
    
    dage = age
    dage = dage/10
    age_1 = dage^(-2)
    age_2 = dage
    
    dbmi = bmi
    dbmi=dbmi/10
    bmi_1 = dbmi^(-2)
    bmi_2 = (dbmi^(-2))*log(dbmi)
    
    Iethrisk <- c(0,                              # white or not stated
                  0.2804031433299542519499425,    # indian
                  0.5629899414207539800000000,    # pakisani  
                  0.2959000085111651600000000,    # bangladeshi
                  0.0727853798779825450000000,    # other_asian
                  -0.1707213550885731700000000,   # caribbean
                  -0.3937104331487497100000000,   # african
                  -0.3263249528353027200000000,   # chinese
                  -0.1712705688324178400000000)   # other
    
    Ismoke <- c( 0,                             # non smoker
                 0.1338683378654626200000000,   # ex-smoker
                 0.5620085801243853700000000,   # light smoker
                 0.6674959337750254700000000,   # modeerate smoker
                 0.8494817764483084700000000)   # heavy smoker
    
    ethrisk = 1
    std_systolic_blood_pressure   = 10
    atrial_fibrillation    = atrial_fibrillation
    
    # heart_attack_relative    = 0
    # atypical_antipsy     = 0
    # regular_steroid_tablets      = 0
    # erectile_disfunction    = 0
    # migraine    = 0
    # rheumatoid_arthritis    = 0
    # chronic_kidney_disease   = 0
    # severe_mental_illness     = 0
    # systemic_lupus_erythematosis  = 0
    # blood_pressure_treatment      = 0
    # type1 = 0
    
    surv <- 10
    survivor <- c(0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0,
                  0.988876402378082,
                  0,
                  0,
                  0,
                  0,
                  0)
    
    # print('##########')
    age_1 = age_1 - 0.053274843841791
    #print(age_1)
    age_2 = age_2 - 4.332503318786621
    #print(age_2)
    bmi_1 = bmi_1 - 0.154946178197861
    #print(bmi_1)
    bmi_2 = bmi_2 - 0.144462317228317
    #print(bmi_2)
    cholesterol_ratio = cholesterol_ratio - 3.476326465606690
    #print(cholesterol_ratio)
    bp = bp - 123.130012512207030
    #print(bp)
    std_systolic_blood_pressure = std_systolic_blood_pressure - 9.002537727355957
    #print(std_systolic_blood_pressure)
    townsend_score = townsend_score - 0.392308831214905
    #print(townsend_score)
    
    # Start of Sum 
    a=0
    
    #The conditional sums 
    
    a =a+ Iethrisk[ethrisk]
    a =a+ Ismoke[smoking]
    
    # Sum from continuous values 
    #print('##########')
    
    a =a+ age_1 * -8.138810924772618800000000
    #print(a)
    a =a+ age_2 * 0.7973337668969909800000000
    #print(a)
    a =a+ bmi_1 * 0.2923609227546005200000000
    #print(a)
    a =a+ bmi_2 * -4.1513300213837665000000000
    #print(a)
    a =a+ cholesterol_ratio * 0.1533803582080255400000000
    #print(a)
    a =a+ bp * 0.0131314884071034240000000
    #print(a)
    a =a+ std_systolic_blood_pressure * 0.0078894541014586095000000
    #print(a)
    a =a+ townsend_score* 0.0772237905885901080000000
    #print(a)
    
    # Sum from boolean values 
    
    a =a+ atrial_fibrillation * 1.5923354969269663000000000
    #print(a)
    # a =a+ atypical_antipsy * 0.2523764207011555700000000
    # a =a+ regular_steroid_tablets * 0.5952072530460185100000000
    # a =a+ migraine * 0.3012672608703450000000000
    # a =a+ rheumatoid_arthritis * 0.2136480343518194200000000
    # a =a+ chronic_kidney_disease * 0.6519456949384583300000000
    # a =a+ severe_mental_illness * 0.1255530805882017800000000
    # a =a+ systemic_lupus_erythematosis * 0.7588093865426769300000000
    # a =a+ blood_pressure_treatment * 0.5093159368342300400000000
    # a =a+ type1 * 1.7267977510537347000000000
    a =a+ type2 * 1.0688773244615468000000000
    #print(a)
    # a =a+ heart_attack_relative * 0.4544531902089621300000000
    
    ################# Sum from interaction terms #################
    
    a =a+ age_1 * (smoking==2) * -4.7057161785851891000000000
    a =a+ age_1 * (smoking==3) * -2.7430383403573337000000000
    a =a+ age_1 * (smoking==4) * -0.8660808882939218200000000
    a =a+ age_1 * (smoking==5) * 0.9024156236971064800000000
    a =a+ age_1 * atrial_fibrillation * 19.9380348895465610000000000
    
    #print(a)
    # a =a+ age_1 * regular_steroid_tablets * -0.9840804523593628100000000
    # a =a+ age_1 * migraine * 1.7634979587872999000000000
    # a =a+ age_1 * chronic_kidney_disease * -3.5874047731694114000000000
    # a =a+ age_1 * systemic_lupus_erythematosis * 19.6903037386382920000000000
    # a =a+ age_1 * blood_pressure_treatment * 11.8728097339218120000000000
    # a =a+ age_1 * type1 * -1.2444332714320747000000000
    
    a =a+ age_1 * type2 * 6.8652342000009599000000000
    a =a+ age_1 * bmi_1 * 23.8026234121417420000000000
    a =a+ age_1 * bmi_2 * -71.1849476920870070000000000
    
    #print(a)
    # a =a+ age_1 * heart_attack_relative * 0.9946780794043512700000000
    
    a =a+ age_1 * bp * 0.0341318423386154850000000
    a =a+ age_1 * townsend_score * -1.0301180802035639000000000
    
    a =a+ age_2 * (smoking==2) * -0.0755892446431930260000000
    a =a+ age_2 * (smoking==3) * -0.1195119287486707400000000
    a =a+ age_2 * (smoking==4) * -0.1036630639757192300000000
    a =a+ age_2 * (smoking==5) * -0.1399185359171838900000000
    a =a+ age_2 * atrial_fibrillation * -0.0761826510111625050000000
    # a =a+ age_2 * regular_steroid_tablets * -0.1200536494674247200000000
    # a =a+ age_2 * migraine * -0.0655869178986998590000000
    # a =a+ age_2 * chronic_kidney_disease * -0.2268887308644250700000000
    # a =a+ age_2 * systemic_lupus_erythematosis * 0.0773479496790162730000000
    # a =a+ age_2 * blood_pressure_treatment * 0.0009685782358817443600000
    # a =a+ age_2 * type1 * -0.2872406462448894900000000
    a =a+ age_2 * type2 * -0.0971122525906954890000000
    a =a+ age_2 * bmi_1 * 0.5236995893366442900000000
    a =a+ age_2 * bmi_2 * 0.0457441901223237590000000
    # a =a+ age_2 * heart_attack_relative * -0.0768850516984230380000000
    a =a+ age_2 * bp * -0.0015082501423272358000000
    a =a+ age_2 * townsend_score * -0.0315934146749623290000000
    #print(a)
    
    
    ## males ----
  }else if(sex=='Males'){
    # Calculate the score itself 
    #print('##########')
    
    surv = 10
    survivor = c(
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0.977268040180206,
      0,
      0,
      0,
      0,
      0
    )
    
    # The conditional arrays 
    
    Iethrisk = c(
      0, # white or not stated
      0.2771924876030827900000000,# indian
      0.4744636071493126800000000,# pakisani  
      0.5296172991968937100000000,# bangladeshi
      0.0351001591862990170000000,# other_asian
      -0.3580789966932791900000000, # caribbean
      -0.4005648523216514000000000, # african
      -0.4152279288983017300000000, # chinese
      -0.2632134813474996700000000# other
    )
    
    Ismoke = c(
      0,
      0.1912822286338898300000000,
      0.5524158819264555200000000,
      0.6383505302750607200000000,
      0.7898381988185801900000000
    )
    
    ethrisk = 1
    std_systolic_blood_pressure   = 10
    atrial_fibrillation    = atrial_fibrillation
    
    # Applying the fractional polynomial transforms 
    #   (which includes scaling)                      
    
    dage = age
    dage=dage/10
    age_1 = dage**(-1)
    age_2 = dage**3
    dbmi = bmi
    dbmi = dbmi/10
    bmi_2 = (dbmi**-2)*log(dbmi)
    bmi_1 = (dbmi**-2)
    
    # Centring the continuous variables 
    
    age_1 = age_1 - 0.234766781330109
    age_2 = age_2 - 77.284080505371094
    bmi_1 = bmi_1 - 0.149176135659218
    bmi_2 = bmi_2 - 0.141913309693336
    cholesterol_ratio = cholesterol_ratio - 4.300998687744141
    bp = bp - 128.571578979492190
    # sbps5 = sbps5 - 8.756621360778809
    townsend_score = townsend_score - 0.526304900646210
    
    # Start of Sum 
    a=0
    
    # The conditional sums 
    
    a = a+ Iethrisk[ethrisk]
    a = a + Ismoke[smoke_cat]
    
    # Sum from continuous values 
    
    a = a + age_1 * -17.8397816660055750000000000
    a = a + age_2 * 0.0022964880605765492000000
    a = a + bmi_1 * 2.4562776660536358000000000
    a = a + bmi_2 * -8.3011122314711354000000000
    a = a + cholesterol_ratio * 0.1734019685632711100000000
    a = a + bp * 0.0129101265425533050000000
    # a = a + sbps5 * 0.0102519142912904560000000
    a = a + townsend_score * 0.0332682012772872950000000
    
    # Sum from boolean values 
    
    a = a + atrial_fibrillation * 0.8820923692805465700000000
    #a = a + b_atypicalantipsy * 0.1304687985517351300000000
    #a = a + b_corticosteroids * 0.4548539975044554300000000
    #a = a + b_impotence2 * 0.2225185908670538300000000
    #a = a + b_migraine * 0.2558417807415991300000000
    #a = a + b_ra * 0.2097065801395656700000000
    #a = a + b_renal * 0.7185326128827438400000000
    #a = a + b_semi * 0.1213303988204716400000000
    #a = a + b_sle * 0.4401572174457522000000000
    #a = a + b_treatedhyp * 0.5165987108269547400000000
    # a = a + b_type1 * 1.2343425521675175000000000
    a = a + type2 * 0.8594207143093222100000000
    #a = a + fh_cvd * 0.5405546900939015600000000
    
    # Sum from interaction terms
    
    a = a + age_1 * (smoke_cat==1) * -0.2101113393351634600000000
    a = a + age_1 * (smoke_cat==2) * 0.7526867644750319100000000
    a = a + age_1 * (smoke_cat==3) * 0.9931588755640579100000000
    a = a + age_1 * (smoke_cat==4) * 2.1331163414389076000000000
    a = a + age_1 * atrial_fibrillation * 3.4896675530623207000000000
    #a = a + age_1 * b_corticosteroids * 1.1708133653489108000000000
    #a = a + age_1 * b_impotence2 * -1.5064009857454310000000000
    #a = a + age_1 * b_migraine * 2.3491159871402441000000000
    # a = a + age_1 * b_renal * -0.5065671632722369400000000
    #a = a + age_1* b_treatedhyp * 6.5114581098532671000000000
    # a = a + age_1 * b_type1 * 5.3379864878006531000000000
    a = a + age_1 * type2 * 3.6461817406221311000000000
    a = a + age_1 * bmi_1 * 31.0049529560338860000000000
    a = a + age_1 * bmi_2 * -111.2915718439164300000000000
    #a = a + age_1 * fh_cvd * 2.7808628508531887000000000
    a = a + age_1 * bp * 0.0188585244698658530000000
    a = a + age_1 * townsend_score * -0.1007554870063731000000000
    a = a + age_2 * (smoke_cat==1) * -0.0004985487027532612100000
    a = a + age_2 * (smoke_cat==2) * -0.0007987563331738541400000
    a = a + age_2 * (smoke_cat==3) * -0.0008370618426625129600000
    a = a + age_2 * (smoke_cat==4) * -0.0007840031915563728900000
    a = a + age_2 * atrial_fibrillation * -0.0003499560834063604900000
    # a = a + age_2 * b_corticosteroids * -0.0002496045095297166000000
    #a = a + age_2 * b_impotence2 * -0.0011058218441227373000000
    #a = a + age_2 * b_migraine * 0.0001989644604147863100000
    #a = a + age_2 * b_renal * -0.0018325930166498813000000
    #a = a + age_2* b_treatedhyp * 0.0006383805310416501300000
    #a = a + age_2 * b_type1 * 0.0006409780808752897000000
    a = a + age_2 * type2 * -0.0002469569558886831500000
    a = a + age_2 * bmi_1 * 0.0050380102356322029000000
    a = a + age_2 * bmi_2 * -0.0130744830025243190000000
    #a = a + age_2 * fh_cvd * -0.0002479180990739603700000
    a = a + age_2 * bp * -0.0000127187419158845700000
    a = a + age_2 * townsend_score * -0.0000932996423232728880000
    
  }
  #print(a)
  score = (1 - (survivor[surv])^exp(a))
  #print('##########')
  
  return(score)
}

library(data.table)

## ---- Shared constants -------------------------------------------------

# Survivor functions at 10 years
qrisk_survivor_female <- 0.988876402378082
qrisk_survivor_male   <- 0.977268040180206

# Ethnicity & smoking lookups
qrisk_Iethrisk_f <- c(
  0,                              # white or not stated
  0.28040314332995425,            # indian
  0.56298994142075398,            # pakistani
  0.29590000851116516,            # bangladeshi
  0.072785379877982545,           # other_asian
  -0.17072135508857317,            # caribbean
  -0.39371043314874971,            # african
  -0.32632495283530272,            # chinese
  -0.17127056883241784             # other
)

qrisk_Ismoke_f <- c(
  0,                  # non smoker
  0.13386833786546262,# ex-smoker
  0.56200858012438537,# light
  0.66749593377502547,# moderate
  0.84948177644830847 # heavy
)

qrisk_Iethrisk_m <- c(
  0,                  # white or not stated
  0.27719248760308279,# indian
  0.47446360714931268,# pakistani
  0.52961729919689371,# bangladeshi
  0.03510015918629902,# other_asian
  -0.35807899669327919,# caribbean
  -0.4005648523216514, # african
  -0.41522792889830173,# chinese
  -0.26321348134749967 # other
)

qrisk_Ismoke_m <- c(
  0,
  0.19128222863388983,
  0.55241588192645552,
  0.63835053027506072,
  0.78983819881858019
)

## ---- Female vectorised -----------------------------------------------

qrisk_female_vec <- function(
    age,
    townsend_score,
    bmi = 25,
    smoke_cat = 1L,             # 0–4 or 1–5 depending on your coding
    cholesterol_ratio = 4.5,
    bp = 120,
    type2 = 0,
    atrial_fibrillation = 0,
    ethrisk = 1L                # ethnicity index, default 1
) {
  # coerce to vectors
  age   <- as.numeric(age)
  bmi   <- as.numeric(bmi)
  town  <- as.numeric(townsend_score)
  chol  <- as.numeric(cholesterol_ratio)
  bp    <- as.numeric(bp)
  type2 <- as.numeric(type2)
  af    <- as.numeric(atrial_fibrillation)
  smk   <- as.integer(smoke_cat)
  eth   <- as.integer(ethrisk)
  
  # fractional polynomials
  dage <- age / 10
  age_1 <- dage^(-2)
  age_2 <- dage
  
  dbmi <- bmi / 10
  bmi_1 <- dbmi^(-2)
  bmi_2 <- (dbmi^(-2)) * log(dbmi)
  
  # centring
  age_1 <- age_1 - 0.053274843841791
  age_2 <- age_2 - 4.332503318786621
  bmi_1 <- bmi_1 - 0.154946178197861
  bmi_2 <- bmi_2 - 0.144462317228317
  chol  <- chol  - 3.476326465606690
  bp_c  <- bp    - 123.13001251220703
  std_sbp <- 10 - 9.002537727355957     # you had std_systolic_blood_pressure = 10 then centred
  town  <- town  - 0.392308831214905
  
  # base linear predictor
  a <- numeric(length(age))
  
  # categorical terms
  a <- a + qrisk_Iethrisk_f[eth]
  a <- a + qrisk_Ismoke_f[smk]
  
  # continuous terms
  a <- a + age_1 * -8.138810924772619
  a <- a + age_2 *  0.797333766896991
  a <- a + bmi_1 *  0.2923609227546005
  a <- a + bmi_2 * -4.1513300213837665
  a <- a + chol  *  0.15338035820802554
  a <- a + bp_c  *  0.013131488407103424
  a <- a + std_sbp * 0.0078894541014586095
  a <- a + town  *  0.07722379058859011
  
  # binary terms
  a <- a + af    * 1.5923354969269663
  a <- a + type2 * 1.0688773244615468
  
  # interaction terms with smoking / AF / type2 / BMI / bp / town
  # NOTE: smk==k returns logical vector; converted via * to numeric
  a <- a + age_1 * (smk == 2L) * -4.705716178585189
  a <- a + age_1 * (smk == 3L) * -2.7430383403573337
  a <- a + age_1 * (smk == 4L) * -0.8660808882939218
  a <- a + age_1 * (smk == 5L) *  0.9024156236971065
  a <- a + age_1 * af          * 19.93803488954656
  
  a <- a + age_1 * type2 *  6.86523420000096
  a <- a + age_1 * bmi_1 * 23.802623412141742
  a <- a + age_1 * bmi_2 * -71.18494769208701
  
  a <- a + age_1 * bp_c  *  0.034131842338615485
  a <- a + age_1 * town  * -1.0301180802035639
  
  a <- a + age_2 * (smk == 2L) * -0.07558924464319303
  a <- a + age_2 * (smk == 3L) * -0.11951192874867074
  a <- a + age_2 * (smk == 4L) * -0.10366306397571923
  a <- a + age_2 * (smk == 5L) * -0.1399185359171839
  a <- a + age_2 * af          * -0.07618265101116251
  
  a <- a + age_2 * type2 * -0.09711225259069549
  a <- a + age_2 * bmi_1 *  0.5236995893366443
  a <- a + age_2 * bmi_2 *  0.04574419012232376
  a <- a + age_2 * bp_c  * -0.0015082501423272358
  a <- a + age_2 * town  * -0.03159341467496233
  
  # convert linear predictor to 10y risk
  score <- 1 - (qrisk_survivor_female ^ exp(a))
  score
}

## ---- Male vectorised --------------------------------------------------

qrisk_male_vec <- function(
    age,
    townsend_score,
    bmi = 25,
    smoke_cat = 1L,
    cholesterol_ratio = 4.5,
    bp = 120,
    type2 = 0,
    atrial_fibrillation = 0,
    ethrisk = 1L
) {
  
  # with(x[1,],{
  # print(age)
  age   <- as.numeric(age)
  bmi   <- as.numeric(bmi)
  town  <- as.numeric(townsend_score)
  chol  <- as.numeric(cholesterol_ratio)
  bp    <- as.numeric(bp)
  type2 <- as.numeric(type2)
  af    <- as.numeric(atrial_fibrillation)
  smk   <- as.integer(smoke_cat)
  eth   <- as.integer(ethrisk)
  
  # print(age)
  # print(bmi)
  # print(town)
  # print(chol)
  # print(bp)
  # print(type2)
  # print(af)
  # print(smk)
  # print(eth)
  
  # fractional polynomials
  dage  <- age / 10
  age_1 <- dage^(-1)
  age_2 <- dage^3
  
  dbmi  <- bmi / 10
  bmi_2 <- (dbmi^(-2)) * log(dbmi)
  bmi_1 <- (dbmi^(-2))
  
  # centring
  age_1 <- age_1 - 0.234766781330109
  age_2 <- age_2 - 77.284080505371094
  bmi_1 <- bmi_1 - 0.149176135659218
  bmi_2 <- bmi_2 - 0.141913309693336
  chol  <- chol  - 4.300998687744141
  bp_c  <- bp    - 128.57157897949219
  town  <- town  - 0.52630490064621
  
  a <- numeric(length(age))
  
  # categorical
  a <- a + qrisk_Iethrisk_m[eth]
  a <- a + qrisk_Ismoke_m[smk]
  
  # continuous
  a <- a + age_1 * -17.839781666005575
  a <- a + age_2 *  0.0022964880605765492
  # print(a)
  # print('---')
  a <- a + bmi_1 *  2.456277666053636
  # print(a)
  a <- a + bmi_2 * -8.301112231471135
  a <- a + chol  *  0.1734019685632711
  a <- a + bp_c  *  0.012910126542553305
  a <- a + town  *  0.033268201277287295
  # print(a)
  # binary
  a <- a + af    * 0.8820923692805466
  a <- a + type2 * 0.8594207143093222
  # print(a)
  # interactions
  a <- a + age_1 * (smk == 1L) * -0.21011133933516346
  a <- a + age_1 * (smk == 2L) *  0.7526867644750319
  a <- a + age_1 * (smk == 3L) *  0.9931588755640579
  a <- a + age_1 * (smk == 4L) *  2.1331163414389076
  a <- a + age_1 * af          *  3.4896675530623207
  a <- a + age_1 * type2       *  3.646181740622131
  a <- a + age_1 * bmi_1       * 31.004952956033886
  a <- a + age_1 * bmi_2       * -111.29157184391643
  a <- a + age_1 * bp_c        *  0.018858524469865853
  a <- a + age_1 * town        * -0.1007554870063731
  # print(a)
  a <- a + age_2 * (smk == 1L) * -0.0004985487027532612
  a <- a + age_2 * (smk == 2L) * -0.0007987563331738541
  a <- a + age_2 * (smk == 3L) * -0.000837061842662513
  a <- a + age_2 * (smk == 4L) * -0.0007840031915563729
  a <- a + age_2 * af          * -0.0003499560834063605
  a <- a + age_2 * type2       * -0.00024695695588868315
  a <- a + age_2 * bmi_1       *  0.005038010235632203
  a <- a + age_2 * bmi_2       * -0.013074483002524319
  a <- a + age_2 * bp_c        * -0.00001271874191588457
  a <- a + age_2 * town        * -0.00009329964232327289
  
  score <- 1 - (qrisk_survivor_male ^ exp(a))
  score
  # a
  # })
}
# setDT(x)
# x <-  x[, qrisk_score := 
#           # qrisk(
#           qrisk_vec(
#             age              = age_capped,
#             sex              = sex,
#             townsend_score   = custom_townsend_score_dz,
#             bmi              = bmi_num
#             # you can pass smoke_cat, cholesterol_ratio, bp, type2, af later
#           )
# ]

qrisk_vec <- function(
    age,
    sex, 
    ...
    # townsend_score = 0,
    # bmi = 25,
    # smoke_cat = 1L,
    # cholesterol_ratio = 4.5,
    # bp = 120,
    # type2 = 0,
    # atrial_fibrillation = 0,
    # ethrisk = 1L
) {
  # print()
  args <- list(...)
  # print(args)
  if(!'townsend_score' %in% names(args)){townsend_score = rep(0,length(age))}else{townsend_score = args$townsend_score}
  if(!'bmi' %in% names(args)){bmi = rep(25,length(age))}else{bmi = args$bmi}
  if(!'smoke_cat' %in% names(args)){smoke_cat = rep(1L,length(age))}else{smoke_cat = args$smoke_cat}
  if(!'cholesterol_ratio' %in% names(args)){cholesterol_ratio = rep(4.5,length(age))}else{cholesterol_ratio = args$cholesterol_ratio}
  if(!'bp' %in% names(args)){bp = rep(120,length(age))}else{bp = args$bp}
  if(!'type2' %in% names(args)){type2 = rep(0,length(age))}else{type2 = args$type2}
  if(!'atrial_fibrillation' %in% names(args)){atrial_fibrillation = rep(0,length(age))}else{atrial_fibrillation = args$atrial_fibrillation}
  if(!'ethrisk' %in% names(args)){ethrisk = rep(1L,length(age))}else{ethrisk = args$ethrisk}
  # print(sex)
  sex <- as.character(sex)
  out <- numeric(length(age))
  # print(cholesterol_ratio)
  # print(out)
  
  idx_f <- sex == "Females"
  idx_m <- sex == "Males"
  
  if (T) {
    out[idx_f] <- qrisk_female_vec(
      age   = age[idx_f],
      townsend_score = townsend_score[idx_f],
      bmi   = bmi[idx_f],
      smoke_cat = smoke_cat[idx_f],
      cholesterol_ratio = cholesterol_ratio[idx_f],
      bp = bp[idx_f],
      type2 = type2[idx_f],
      atrial_fibrillation = atrial_fibrillation[idx_f],
      ethrisk = ethrisk[idx_f]
    )
    # print(age[idx_m])
    # print(
    #   qrisk_male_vec(
    #     age   = age[idx_m],
    #     townsend_score = townsend_score[idx_m],
    #     bmi   = bmi[idx_m],
    #     smoke_cat = smoke_cat[idx_m],
    #     cholesterol_ratio = cholesterol_ratio[idx_m],
    #     bp = bp[idx_m],
    #     type2 = type2[idx_m],
    #     atrial_fibrillation = atrial_fibrillation[idx_m],
    #     ethrisk = ethrisk[idx_m]
    #   )
    # )
  }
  
  if (T) {
    out[idx_m] <- qrisk_male_vec(
      age   = age[idx_m],
      townsend_score = townsend_score[idx_m],
      bmi   = bmi[idx_m],
      smoke_cat = smoke_cat[idx_m],
      cholesterol_ratio = cholesterol_ratio[idx_m],
      bp = bp[idx_m],
      type2 = type2[idx_m],
      atrial_fibrillation = atrial_fibrillation[idx_m],
      ethrisk = ethrisk[idx_m]
    )
    
    # print(idx_m)
    # print(age[idx_m])
    # print(townsend_score[idx_m])
    # print(bmi[idx_m])
    # print(smoke_cat[idx_m])
    # print(cholesterol_ratio[idx_m])
    # print(bp[idx_m])
    # print(type2[idx_m])
    # print(atrial_fibrillation[idx_m])
    # print(ethrisk[idx_m])
    # print(
    # qrisk_male_vec(
    #   age   = age[idx_m],
    #   townsend_score = townsend_score[idx_m],
    #   bmi   = bmi[idx_m],
    #   smoke_cat = smoke_cat[idx_m],
    #   cholesterol_ratio = cholesterol_ratio[idx_m],
    #   bp = bp[idx_m],
    #   type2 = type2[idx_m],
    #   atrial_fibrillation = atrial_fibrillation[idx_m],
    #   ethrisk = ethrisk[idx_m]
    #   )
    # )
  }
  
  out
}

# initial_time_zero_population

# pop <- as.data.table(initial_time_zero_population)
# 
# # latest year
# max_year <- pop[, max(year, na.rm = TRUE)]
# 
# # work only on that subset
# postp <- pop[year == max_year]

# recode BMI to numeric once, by reference
# postp[
#   , bmi_num := fifelse(
#     bmi == "normal",     20,
#     fifelse(
#       bmi == "overweight", 27,
#       fifelse(bmi == "obese", 35, NA_real_)
#     )
#   )
# ]
# count(postp,bmi_num)
# # age cap
# postp[, age_capped := pmin(age, 84)]
# 
# 
# # compute qrisk_score vectorised (no rowwise)
# x <- postp[4:80,]
# x <-  x[, qrisk_score := 
#           # qrisk(
#           qrisk_vec(
#             age              = age_capped,
#             sex              = sex,
#             townsend_score   = custom_townsend_score_dz,
#             bmi              = bmi_num
#             # you can pass smoke_cat, cholesterol_ratio, bp, type2, af later
#           )
# ]
# 
# count(x,qrisk_score)
# # qrisk_score      n
# # <num>  <int>
# #   1: 0.000105051      1
# # 2:          NA 190345
# hist(postp$qrisk_year_risk)
# qrisk_vec(60,'Females',-2,24)
# 0.0008123621

apply_cvd_risk_wo_risk_factors_dt <- function(input_population) {
  # make a copy so we don't mutate the original by reference
  pop <- as.data.table(input_population)
  
  # latest year
  max_year <- pop[, max(year, na.rm = TRUE)]
  
  # work only on that subset
  postp <- pop[year == max_year]
  
  # recode BMI to numeric once, by reference
  postp[
    , bmi_num := fifelse(
      bmi == "normal",     20,
      fifelse(
        bmi == "overweight", 27,
        fifelse(bmi == "obese", 35, NA_real_)
      )
    )
  ]
  
  # age cap
  postp[, age_capped := pmin(age, 84)]
  
  # compute qrisk_score vectorised (no rowwise)
  postp[
    # qrisk_score := ifelse(
    # age_capped < 25,
    # 0,
    # qrisk(
    ,  qrisk_score := qrisk_vec(
      age              = age,#age_capped,
      sex              = sex#,
      # townsend_score   = townsend_score#,
      # bmi              = bmi_num
      # you can pass smoke_cat, cholesterol_ratio, bp, type2, af later
    )
    #)#, by = .I
  ]
  
  # drop any existing qrisk_score in the main table
  # if ("qrisk_score" %in% names(pop)) {
  #   pop[, qrisk_score := NULL]
  # }
  
  # join scores back by id, by reference (fast)
  # pop[postp[, .(id, qrisk_score)], on = "id", qrisk_score := i.qrisk_score]
  
  # return as data.table or data.frame as you prefer
  # pop[]
}


apply_cvd_risk_wo_risk_factors <- function(input_population){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year, na.rm = TRUE)) 
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 84, 84, age),
      bmi = case_when(
        bmi == "normal"     ~ 20,
        bmi == "overweight" ~ 27,
        bmi == "obese"       ~ 35,
        TRUE ~ NA_real_ ),
      sex,
      townsend_score) %>% 
    rowwise() %>% 
    mutate(qrisk_score = ifelse(age<25, 0,
                                qrisk(
                                  age = age, 
                                  sex = sex,
                                  bmi = bmi,
                                  townsend_score = townsend_score)))
  
  
  #input_population <- current_population
  input_population <- input_population |> 
    select(-any_of('qrisk_score'))
  
  #names(input_population)
  input_population <- left_join(input_population,
                                postp[c('qrisk_score','id')],
                                by='id')
  
  return(input_population)
}

apply_cvd_risk <- function(input_population, intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year, na.rm = TRUE)) %>% 
    pivot_longer(cols = -all_of(base_population_demographic_column_names)) %>% 
    mutate(category = str_extract(string = name,pattern = 'cholesterol|overweight|bp|smoking|diabetic|atrial_fibrillation')) %>% 
    group_by(id,category) %>% 
    arrange(desc(value)) %>% 
    slice_head() %>% 
    ungroup() %>% 
    pivot_wider(id_cols = -value,names_from = category,values_from = name) 
  
  postp <- postp1 %>%
    mutate(
      # sex = case_when(
      # sex == 'Females'~1,
      # sex == 'Males'~2),
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
    ) %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(qrisk_score =
             case_when(age < 30 ~ 0,
                       age > 85 ~ 0.5,
                       T~ qrisk(age, 
                                sex,
                                bmi, 
                                smoking, 
                                cholesterol, 
                                bp, 
                                type2, 
                                townsend_score, 
                                atrial_fibrillation )
             )
    )
  
  #input_population <- current_population
  input_population <- input_population |> select(-any_of('qrisk_score'))
  
  #names(input_population)
  input_population <- left_join(input_population,postp[c('qrisk_score','id')],by='id')
  
  #print(input_population)
  return(input_population)
}

###########################################
############ TEST ############
###########################################

not_actually_a_function_just_dont_want_contents_run <- function(){
  # all babies with age 0 = were NAN
  # risk went haywire under 25 yo
  
  full_test_population  %>% 
    #group_by(age) %>% 
    #summarise(m=mean(risk,na.rm = T)) %>% View
    ggplot() +  
    geom_boxplot(aes(group=age,age,risk))
  
  full_test_population %>% 
    mutate(townsend_quintile=as.character(townsend_quintile)) %>% 
    ggplot() +  
    geom_boxplot(aes(colour = townsend_quintile, group = townsend_quintile, townsend_quintile,risk),alpha=0.1)
  
  full_test_population  %>% 
    ggplot() +  
    geom_boxplot(aes(group=!is.na(atrial_fibrillation),
                     !is.na(atrial_fibrillation),
                     risk))
  
  #input_population <- apply_cvd_risk(x)
  
  #input_population[is.na(input_population$risk),]
}


