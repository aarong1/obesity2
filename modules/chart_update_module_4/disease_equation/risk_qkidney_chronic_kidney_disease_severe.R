# b_renalstones,
# b_sle,
# are in females but not males 

risk_qkidney_nephrology3 <- function(
    age = NULL, 
    sex = NULL,
    town = NULL, 
    ethrisk = 1,
    bmi = 25,
    smoke_cat = 1,
    sbp = 120,
    b_type1 = 0,
    b_type2 = 0,
    b_CCF = 0,
    b_cvd = 0,
    b_nsaid = 0,
    b_pvd = 0,
    b_ra = 0,
    b_renalstones = 0,
    b_sle = 0,
    b_treatedhyp = 0,
    fh_kidney = 0,
    surv = 5
) {
  # === Validation ===
  #if (age < 35 || age > 74) stop("age must be in range 35–74")
  #if (bmi < 20 || bmi > 40) stop("bmi must be in range 20–40")
  if (ethrisk < 1 || ethrisk > 9) stop("ethrisk must be in range 1–9")
  if (sbp < 70 || sbp > 210) stop("sbp must be in range 70–210")
  if (smoke_cat < 0 || smoke_cat > 4) stop("smoke_cat must be in range 0–4")
  if (surv < 1 || surv > 5) stop("surv must be in range 1–5")
  if (town < -7 || town > 11) stop("town must be in range -7 to 11")

  bool_vars <- c(b_CCF, b_cvd, b_nsaid, b_pvd, b_ra,  b_renalstones,  b_sle, b_treatedhyp, b_type1,  b_type2,  fh_kidney)
    if (any(!bool_vars %in% c(0, 1))) stop("All binary inputs must be 0 or 1")

  if( sex == 'Males' ){

    #print('Males')
    
  # === Survivor function ===

  survivor <- c(
    #0, # index 0 unused
    0.999482631683350,
    0.998838543891907,
    0.997995197772980,
    0.997083306312561,
    0.996157050132751
  )
  
  # === Ethnic risk and smoking risk coefficients ===
  Iethrisk <- c(
    #0,
    0,
    0.15035918550299343,
    0.69438661039458394,
    0.30248247025481079,
    0.36428668253906854,
    -0.18826226115306105,
    0.1607974560923533,
    0.30664536765496675,
    0.28517059933876532
  )
  
  Ismoke <- c(
    #0,
    0.12144250824323455,
    0.13824104373542898,
    0.21372142763159313,
    0.2196760294878585
  )
  
  # === Fractional polynomial transforms ===
  dage <- age / 10
  age_1 <- dage^3
  age_2 <- dage^3 * log(dage)
  
  dbmi <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  
  dsbp <- sbp / 100
  sbp_1 <- dsbp^-2
  sbp_2 <- dsbp^(-0.5)
  
  # === Centering ===
  age_1 <- age_1 - 129.786895751953120
  age_2 <- age_2 - 210.509735107421870
  bmi_1 <- bmi_1 - 0.138382270932198
  bmi_2 <- bmi_2 - 0.136841759085655
  sbp_1 <- sbp_1 - 0.551519691944122
  sbp_2 <- sbp_2 - 0.861767768859863
  town <- town - (-0.442225903272629)
  
  # === Score calculation ===
  a <- 0
  a <- a + Iethrisk[ethrisk]
  a <- a + Ismoke[smoke_cat]
  
  a <- a + age_1 * 0.037256630686502298
  a <- a + age_2 * -0.01245955304365007
  a <- a + bmi_1 * 4.0321363736131879
  a <- a + bmi_2 * -16.794176222687863
  a <- a + sbp_1 * 5.0163247808020985
  a <- a + sbp_2 * -14.826930357207585
  a <- a + town * 0.019610306205662658
  
  a <- a + b_CCF * 1.043229043958031
  a <- a + b_cvd * 0.33350593399060524
  a <- a + b_nsaid * 0.25360714718534833
  a <- a + b_pvd * 0.38386319456206508
  a <- a + b_ra * 0.39068602062787761
  a <- a + b_treatedhyp * 1.0232261464655623
  a <- a + b_type1 * 2.5075410232894906
  a <- a + b_type2 * 1.8034937790520313
  a <- a + fh_kidney * 1.2749066751148557
  
  a <- a + age_1 * b_treatedhyp * -0.025984988624096955
  a <- a + age_1 * b_type1 * -0.042677433497271655
  a <- a + age_1 * b_type2 * -0.023362196238814727
  a <- a + age_2 * b_treatedhyp * 0.010925507938753076
  a <- a + age_2 * b_type1 * 0.017219619219557569
  a <- a + age_2 * b_type2 * 0.008755242493996061
  

}

  if(sex == 'Females'){

 #print('Females')
    
    
  # Survivor values
  survivor <- c(
    #0, # index 0 unused
    0.999198436737061,
    0.998197913169861,
    0.996961772441864,
    0.995605170726776,
    0.994150519371033
  )
  
  # Coefficients
  Iethrisk <- c(
    #0,
    0,
    0.029765038060628347,
    0.43554843085104178,
    0.40234031212628713,
    0.15106823540633343,
    -0.72760140581918697,
    -0.57215843791497834,
    0.12020744076079062,
    0.20333004324212706
  )
  
  Ismoke <- c(
    #0,
    0.17294627305165622,
    0.26507811091453698,
    0.23829368943383136,
    0.35755517500759798
  )
  
  # Fractional polynomial transformations
  dage <- age / 10
  age_1 <- dage^3
  age_2 <- dage^3 * log(dage)
  dbmi <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  dsbp <- sbp / 100
  sbp_1 <- dsbp^-2
  sbp_2 <- dsbp^-2 * log(dsbp)
  
  # Centering
  age_1 <- age_1 - 134.03318786621094
  age_2 <- age_2 - 218.83543395996094
  bmi_1 <- bmi_1 - 0.143204286694527
  bmi_2 <- bmi_2 - 0.139157548546791
  sbp_1 <- sbp_1 - 0.583845973014832
  sbp_2 <- sbp_2 - 0.157089039683342
  town  <- town - (-0.607831299304962)
  
  # Sum of components
  a <- 0
  a <- a + Iethrisk[ethrisk ]
  a <- a + Ismoke[smoke_cat ]
  
  a <- a + age_1 * 0.048437152989423138
  a <- a + age_2 * -0.017694770113347951
  a <- a + bmi_1 * 1.1105635237480727
  a <- a + bmi_2 * -12.369096623225834
  a <- a + sbp_1 * -2.1086300793705433
  a <- a + sbp_2 * -5.6186919081620603
  a <- a + town * 0.029299062364208574
  
  a <- a + b_CCF * 0.82128751430694003
  a <- a + b_cvd * 0.31548609286143398
  a <- a + b_nsaid * 0.26321887432936897
  a <- a + b_pvd * 0.30101131937372472
  a <- a + b_ra * 0.48337252576273465
  a <- a + b_renalstones * 0.23967246465183087
  a <- a + b_sle * 0.87563423552898745
  a <- a + b_treatedhyp * 0.91247212744883732
  a <- a + b_type1 * 2.1047508138768976
  a <- a + b_type2 * 1.5036808227335385
  a <- a + fh_kidney * 0.75663356404049442
  
  a <- a + age_1 * b_treatedhyp * -0.016023794994524821
  a <- a + age_1 * b_type1 * -0.031522747736686001
  a <- a + age_1 * b_type2 * -0.028039334453367589
  a <- a + age_2 * b_treatedhyp * 0.0064950068136830665
  a <- a + age_2 * b_type1 * 0.012362180195767172
  a <- a + age_2 * b_type2 * 0.011291679202256905
  
  }
  
  # Final score
  (
    score <- (1 - survivor[surv]^exp(a))
    )
  
  return(score)

}


apply_ckd_risk_wo_risk_factors <- function(input_population, intervention=1){
  
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
    mutate(ckd_risk =
             ifelse(age < 30, 0,
                    risk_qkidney_nephrology3(
                      age = age, 
                      sex = sex,
                      bmi = bmi,
                      town = townsend_score
                    )
            ) * 1.4
    )
  input_population <- input_population |> select(-any_of('ckd_risk'))
  
  input_population <- left_join(input_population,
                                postp[c('ckd_risk','id')], 
                                by ='id')
  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  input_population <- ungroup(input_population)
  
  return(input_population)
}

#risk_qkidney_nephrology3( age = 35:40, sex = 'Males', town = 1:6 )
