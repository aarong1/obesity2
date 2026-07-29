
# Oesophageal-gastric
risk_oesteogastric_female <- function(
    age,
    bmi = 25,
    smoke_cat = 1,
    town = 0,
    alcohol_cat6 = 1,
    b_barratts = 0,
    b_bloodcancer = 0,
    b_breastcancer = 0,
    b_lungcancer = 0,
    b_oralcancer = 0,
    b_peptic = 0,
    b_type2 = 0,
    surv = 5
) {
  #--- baseline survivors at 1–5 years
  survivors <- c(
    #0
    0.999951303005219,
    0.999912202358246,
    0.999867975711823,
    0.999822735786438,
    0.999769926071167
  )
  
  #--- categorical effects 
  Ialcohol <- c(
    #0
    -0.11189368983791335,
    -0.09686591768081258,
    -0.020493347098261897,
    0.69179172272655054,
    0.69149113794917483
  )
  
  Ismoke <- c(
    #0
    0.082454477615318322,
    0.57786728901095497,
    0.71788998349242594,
    0.87748067752842873
  )
  
  #--- fractional‐polynomial transforms
  dage  <- age / 10
  age_1 <- dage^2
  age_2 <- dage^2 * log(dage)
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  
  #--- centre continuous variables
  age_1 <- age_1 - 20.148687362670898
  age_2 <- age_2 - 30.254657745361328
  bmi_1 <- bmi_1 - 0.151121616363525
  bmi_2 <- bmi_2 - 0.142785012722015
  town  <- town  - 0.161748945713043
  
  #--- linear predictor
  a <- 0
  
  # conditional sums
  a <- a + Ialcohol[alcohol_cat6]
  a <- a + Ismoke[smoke_cat ]
  
  # continuous
  a <- a + age_1 *  0.45914965886458331
  a <- a + age_2 * -0.16675870039332663
  a <- a + bmi_1  *  6.9375480360437578
  a <- a + bmi_2  * -13.15886212755024
  a <- a + town   *  0.029423061779428595
  
  # boolean
  a <- a + b_barratts     * 1.3419977053300407
  a <- a + b_bloodcancer  * 0.76256297790980909
  a <- a + b_breastcancer * 0.26785164693269614
  a <- a + b_lungcancer   * 0.82354622245478371
  a <- a + b_oralcancer   * 1.3464504475029004
  a <- a + b_peptic       * 0.25513098061052886
  a <- a + b_type2        * 0.2889222416353735
  
  # absolute risk (%) = 100 * (1 − S0^exp(a))
  S0    <- survivors[surv]
  score <- (1 - S0^exp(a))
  return(score)
}



## Male ----


risk_oesteogastric_male <- function(
    age,                 # integer (years)
    town,                 # Townsend score (already centred on UK 2001 distribution)
    bmi = 25,                 # kg/m^2
    alcohol_cat6 = 1,        # 0–5  (same ordering as original model) 
    b_barratts = 0,          # 0 / 1 
    b_oralcancer = 0,        # 0 / 1 
    b_pancreascancer = 0,    # 0 / 1 
    b_peptic = 0,            # 0 / 1
    b_type2 = 0,             # 0 / 1
    smoke_cat = 1,           # 0–4 
    surv = 5                # 1–5  (years)
) {
  ## --- 1. Baseline survivor function (years 1-5) ---
  S0 <- c(
    #0
    0.999897956848145,
    0.999810755252838,
    0.999715626239777,
    0.999619841575623,
    0.999517440795898
  )
  
  ## --- 2. Categorical log-hazard increments ---
  # NB: in the C/SQL code categories start at 0 ⇒ add +1 for R indices
  Ialcohol <- c(
    #0
    -0.060408814366972742,
    -0.120795324725350890,
    -0.064776788859829793,
    0.212448549327400200,
    0.485713933786969370
  )
  Ismoke <- c(
    #0
    0.223046313338126740,
    0.546911891270208340,
    0.614301239736905760,
    0.647849200703754400
  )
  
  ## --- 3. Fractional-polynomial transformations ---
  dage  <- age / 10
  age_1 <- dage^2
  age_2 <- dage^2 * log(dage)
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  
  ## --- 4. Centre continuous variables ---
  age_1 <- age_1 - 19.620740890502930
  age_2 <- age_2 - 29.201423645019531
  bmi_1 <- bmi_1 - 0.144450336694717
  bmi_2 <- bmi_2 - 0.139742657542229
  town  <- town  - 0.259125590324402
  
  ## --- 5. Assemble linear predictor ---
  a <- 0
  
  # categorical (remember +1 index shift)
  a <- a + Ialcohol[alcohol_cat6]
  a <- a + Ismoke[smoke_cat]
  
  # continuous terms
  a <- a + age_1 *  0.55179048337676251
  a <- a + age_2 * -0.20781400681781595
  a <- a + bmi_1 *  8.44084961131547650
  a <- a + bmi_2 * -20.690908334247094
  a <- a + town  *  0.027485527684047967
  
  # binary indicators
  a <- a + b_barratts      * 1.3981857737696197
  a <- a + b_oralcancer    * 0.9751074059450815
  a <- a + b_pancreascancer* 1.4262126421694010
  a <- a + b_peptic        * 0.22446304526922312
  a <- a + b_type2         * 0.16378180506856244
  
  ## --- 6. Convert to absolute risk (%) ---
  risk <- (1 - S0[surv] ^ exp(a))
  return(risk)
}


## Gendered wrapper for oesophageal/gastric cancer risk, apply function, and simplified wo_risk_factors()

# Dispatch based on sex, call raw functions and convert % to probability
risk_qcancer_oesteogastric <- function(
    age,
    sex,
    town,
    bmi = 25,
    smoke_cat = 1,
    alcohol_cat6 =1,
    b_barratts = 0,
    b_bloodcancer = 0,
    b_breastcancer = 0,
    b_lungcancer = 0,
    b_oralcancer = 0,
    b_peptic = 0,
    b_type2 = 0,
    surv = 5
) {
  if (sex == 'Females') {
    # female raw returns percentage
    risk <- risk_oesteogastric_female(
      age = age,
      alcohol_cat6 = alcohol_cat6,
      b_barratts = b_barratts,
      b_bloodcancer = b_bloodcancer,
      b_breastcancer = b_breastcancer,
      b_lungcancer = b_lungcancer,
      b_oralcancer = b_oralcancer,
      b_peptic = b_peptic,
      b_type2 = b_type2,
      bmi = bmi,
      smoke_cat = smoke_cat,
      surv = surv,
      town = town
    )
  } else if(sex == 'Males'){
    # males
    risk <- risk_oesteogastric_male(
      age = age,
      alcohol_cat6 = alcohol_cat6,
      b_barratts = b_barratts,
      b_oralcancer = b_oralcancer,
      b_pancreascancer = b_lungcancer,
      b_peptic = b_peptic,
      b_type2 = b_type2,
      bmi = bmi,
      smoke_cat = smoke_cat,
      surv = surv,
      town = town
    )
  }
  # convert percentage to probability
  return(risk)
}

# Apply oesgastric cancer risk across population

# Apply function to an input population dataframe
apply_osteogastric_cancer_risk_wo_risk_factors <- function(input_population, intervention = 1) {
  postp1 <- input_population %>%
    filter(year == max(year, na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(age = pmin(age, 90),
           b_type2 = case_when(
             diabetes_status == 'diagnosed_diabetes' ~ 1,
             diabetes_status == 'undiagnosed_diabetes' ~ 1,
             diabetes_status == 'no_diabetes' ~0,
             TRUE ~ 0
           ),
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
           bmi = case_when(
             bmi == "normal"     ~ 22.5,
             bmi == "overweight" ~ 28,
             bmi == "obese"       ~ 35,
             TRUE ~ NA_real_)
           # other covariates (b_braincancer, etc.) are assumed present
    ) %>%
    rowwise() %>%
    mutate(
      osteogastric_cancer_risk = ifelse(
        age < 25, 0,
        risk_qcancer_oesteogastric(
          age              = age,
          sex              = sex,
          town = custom_townsend_score_dz,
          bmi              = bmi,
          alcohol_cat6 =alcohol_cat6,
          smoke_cat = smoke_cat,
          b_type2 = b_type2
        )
      ) * 0.6717391
    ) %>%
    ungroup()
  
  input_population <- input_population %>%
    select(-any_of('osteogastric_cancer_risk')) %>%
    left_join(select(postp, id, osteogastric_cancer_risk),
              by = 'id')
  
  return(input_population)
}


# test_population |>
#   apply_osteogastric_cancer_risk_wo_risk_factors() |> 
#   ggplot( ) +
#   geom_point(aes( age, osteogastric_risk, col = bmi ) ) +
#   geom_smooth(aes( age, osteogastric_risk, col = bmi ) ) +
#   facet_wrap(~sex)
# 
# test_population |>
#   apply_osteogastric_cancer_risk_wo_risk_factors() |> 
#   ggplot() +
#   geom_point(aes( age, osteogastric_risk, col = as.character(townsend_quintile)   ) ) +
#   geom_smooth(aes( age, osteogastric_risk, col = as.character(townsend_quintile), group=as.character(townsend_quintile)  ) ) +
#   facet_wrap(~sex)

