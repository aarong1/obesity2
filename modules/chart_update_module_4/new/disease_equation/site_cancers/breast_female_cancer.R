breastcancer_female <- function(age,
                                age_1_fh_breastcancer = 0,
                                age_2_fh_breastcancer = 0,
                                alcohol_cat6,
                                b_benignbreast = 0,
                                b_bloodcancer = 0,
                                b_cop = 0,
                                b_hrt_oest = 0,
                                b_lungcancer = 0,
                                b_manicschiz = 0,
                                b_ovariancancer = 0,
                                bmi,
                                ethrisk,
                                fh_breastcancer = 0,
                                surv,
                                # town_1,
                                # town_2,
                                town) {
  
  # baseline survival: index 1 = 0yr, 2 = 1yr, …, 6 = 5yr
  survivors <- c(
    
    0.998234629631042,
    0.996467649936676,
    0.994715332984924,
    0.992784678936005,
    0.990664660930634
  )
  
  # categorical effects
  Ialcohol <- c(
    # 0.0,
    0.052608749231567525,
    0.10409227333225146,
    0.18991705189460917,
    0.27151481129254795,
    0.22557544831456663
  )
  
  
  Iethrisk <- c(
    # 0.0,
    -0.31642004172228422,
    -0.33867490010482365,
    -0.94208356242121705,
    -0.24713464422456044,
    -0.19607484240207898,
    -0.29264162372426122,
    -0.31945779001361019,
    -0.18693349501071405
  )
  
  # fractional–polynomial transforms
  dage  <- age / 10
  age1  <- dage^(-1)
  age2  <- dage^(-1) * log(dage)
  dbmi  <- bmi  / 10
  bmi1  <- dbmi^(-2)
  bmi2  <- dbmi^(-2) * log(dbmi)
  
  town_1 <- town/4
  town_2 <- (town/4)^2
  #see back calculator notes for townsend transformation in PHM technical documentation
  
  # centring
  age1 <- age1 - 0.223646596074104
  age2 <- age2 - 0.334952861070633
  bmi1 <- bmi1 - 0.151265263557434
  bmi2 <- bmi2 - 0.142848879098892
  
  # build linear predictor
  a <- 0
  a <- a + Ialcohol[alcohol_cat6 ]
  a <- a + Iethrisk[ethrisk ]
  a <- a + age1 * -19.081587156614287
  a <- a + age2 *  18.509756179218396
  a <- a + bmi1 *  -1.4752669093749973
  a <- a + bmi2 *   3.0544989463359729
  
  a <- a + age_1_fh_breastcancer *  5.2222964960331106
  a <- a + age_2_fh_breastcancer * -8.002089077472986
  a <- a + b_benignbreast        *  0.41367643341384747
  a <- a + b_bloodcancer         *  0.44994536748262803
  a <- a + b_cop                 *  0.12610786134996657
  a <- a + b_hrt_oest            *  0.16913593645922093
  a <- a + b_lungcancer          *  0.62046068634592022
  a <- a + b_manicschiz          *  0.15117702688059084
  a <- a + b_ovariancancer       *  0.35123379560422596
  a <- a + fh_breastcancer       *  0.65834446669129887
  a <- a + town_1                *  0.00000075629653384331456
  a <- a + town_2                * -0.086746493896535493
  
  # pick the right baseline-survival (surv in 1:5 → index surv+1)
  S <- survivors[surv]
  
  # 100*(1 – S^exp(a))
  score <- (1 - S^exp(a))
  return(score)
}


# town = townsend /4
# town * 0.000000756296533 + town2 * (−0.086746493896)

apply_breast_cancer_risk_wo_risk_factors <- function(input_population, intervention = 1){
  
  # input_population = initial_time_zero_population
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 90, 90, age),
      sex,
      # Encode BMI categories
      bmi = case_when(
        bmi == "normal"     ~ 22,
        bmi == "overweight" ~ 30,
        bmi == "obese"      ~ 38,
        TRUE                ~ NA_real_),
      custom_townsend_score_dz,
      ethrisk,
      alcohol_cat6 = case_when(
        alcohol == 'no_risk'  ~ 1,
        alcohol == 'lower_risk' ~ 3,
        alcohol == 'higher_risk'  ~ 5,
        TRUE                           ~ 1),
  b_bloodcancer = blood_cancer!=0,
  b_lungcancer = lung_cancer!=0,
  b_ovariancancer = ovarian_cancer!=0
    )

  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(breast_cancer_risk =
             ifelse(age < 25, 0, # changed this after sppg results
                    ##############################################################################################
                    # * 10  
                    breastcancer_female(  #### made changes ####
                                                  ##############################################################################################
                                                  age = age, 
                                                  # sex = sex,
                                                  bmi = bmi,
                                                  town = custom_townsend_score_dz,
                                                  ethrisk = ethrisk,
                                                  alcohol_cat6 = alcohol_cat6
                    ))*1.77*1.9
    )
  
  input_population <- input_population |> select(-any_of('breast_cancer_risk'))
  
  input_population <- left_join(input_population,postp[c('breast_cancer_risk','id')], by ='id')

  input_population <- ungroup(input_population)
  
  return(input_population)
}

