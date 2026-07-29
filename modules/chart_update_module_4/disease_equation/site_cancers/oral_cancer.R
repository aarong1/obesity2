
risk_oralcancer_female<- function(
    age,              # numeric age in years (25-84 in the original validation)
    town,             # Townsend score (can be negative)
    alcohol_cat6 = 1,     # factor 0-5  (0 = none, 5 = heaviest)
    smoke_cat = 1,        # 0-4  (0 = never; 4 = heavy)
    b_bloodcancer = 0,    # 0 / 1
    b_ovariancancer = 0,  # 0 / 1
    surv = 5             # 1-5  (risk horizon in years)
) {
  ## --- 1. Baseline survivor function (years 1–5) ---
  S0 <- c(
    # 0
    0.999955236911774,
    0.999913334846497,
    0.999870777130127,
    0.999827504158020,
    0.999773323535919
  )
  
  ## --- 2. Log-hazard increments for categorical variables ---
  # add +1 when you index because categories in the C code start at 0
  Ialcohol <- c(
    # 0.000000000000000000,   # cat 0
    0.027999573991790434,   # 1
    0.161952333485795570,   # 2
    0.469045561295809990,   # 3
    1.050113709562972200,   # 4
    1.477564061249553500    # 5
  )
  Ismoke <- c(
    # 0.000000000000000000,   # cat 0
    0.202771610564268650,   # 1
    0.751311169011947390,   # 2
    0.922381368685718290,   # 3
    1.258014320621153800    # 4
  )
  
  ## --- 3. Fractional-polynomial transforms ---
  dage  <- age / 10
  age_1 <- dage^(-2)
  
  ## --- 4. Centre continuous covariates ---
  age_1 <- age_1 - 0.049628291279078
  town  <- town  - 0.161764934659004
  
  ## --- 5. Create linear predictor ---
  a <- 0
  a <- a + Ialcohol[alcohol_cat6 ]   
  a <- a + Ismoke[smoke_cat     ]
  
  # continuous terms
  a <- a + age_1 * (-31.59790890203471)
  a <- a + town  *   0.027322975582607253
  
  # binary indicators
  a <- a + b_bloodcancer  * 1.5136324928800686
  a <- a + b_ovariancancer* 1.4208557298354756
  
  ## --- 6. Convert to absolute risk (%) and return ---
  risk <- (1 - S0[surv] ^ exp(a))
  return(risk)
}



## Male ----
# 


risk_oralcancer_male <- function(
    age,           # numeric (years) – should be 25-84 under the original validation
    town,           # numeric (can be negative)
    bmi = 25,           # numeric (kg/m^2)
    alcohol_cat6 = 1,  # integer 0–5
    smoke_cat = 1,     # integer 0–4
    b_bloodcancer = 0, # 0 / 1
    b_colorectal = 0,  # 0 / 1
    b_lungcancer = 0,  # 0 / 1
    surv = 5          # 1–5  (risk horizon in years)
) {
  ## --- 1. Baseline survivor function (years 1–5) ---
  S0 <- c(
    #0
    0.999936580657959,
    0.999877035617828,
    0.999810338020325,
    0.999735474586487,
    0.999666094779968
  )
  
  ## --- 2. Log-hazard increments for categorical covariates ---
  Ialcohol <- c(
    # 0.000000000000000000,   # cat 0
    -0.11815137221117873,    # 1
    0.017369446321814367,   # 2
    0.30919815160108927,    # 3
    0.95266877468309552,    # 4
    1.31018963772298110     # 5
  )
  Ismoke <- c(
    # 0.000000000000000000,   # cat 0
    0.141313630107231140,   # 1
    0.837623266183970320,   # 2
    0.854011447928299640,   # 3
    1.083175089535542300    # 4
  )
  
  ## --- 3. Fractional-polynomial transforms ---
  dage  <- age / 10
  age_1 <- dage^(-0.5)
  age_2 <- dage^(-0.5) * log(dage)
  
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^(-2)
  bmi_2 <- dbmi^(-1)
  
  ## --- 4. Centre continuous covariates ---
  age_1 <- age_1 - 0.475125968456268
  age_2 <- age_2 - 0.707154035568237
  bmi_1 <- bmi_1 - 0.144458159804344
  bmi_2 <- bmi_2 - 0.380076527595520
  town  <- town  - 0.258994281291962
  
  ## --- 5. Linear predictor ---
  a <- 0
  a <- a + Ialcohol[alcohol_cat6 ]   # +1 for R’s 1-based indexing
  a <- a + Ismoke[smoke_cat ]
  
  # continuous terms
  a <- a + age_1 * ( -5.3604756931843474 )
  a <- a + age_2 * ( 18.4654946257000350 )
  a <- a + bmi_1 * ( 16.7531872033111960 )
  a <- a + bmi_2 * ( -10.3137864786102560 )
  a <- a + town  * ( 0.046676794726002648 )
  
  # binary indicators
  a <- a + b_bloodcancer * 0.85005392253691792
  a <- a + b_colorectal  * 0.48087862364839068
  a <- a + b_lungcancer  * 1.05451225353212390
  
  ## --- 6. Convert to absolute risk (%) ---
  risk <- (1 - S0[surv] ^ exp(a))
  return(risk)
}

## Gendered wrapper for oral cancer risk, apply function, and simplified wo_risk_factors()

# Dispatch based on sex, call raw functions and return probability
risk_qcancer_oralcancer <- function(
    age,
    sex,
    town,
    bmi = 25,
    alcohol_cat6 = 1,
    smoke_cat = 1,
    b_colorectal = 0, # map if needed
    b_lungcancer = 0,
    b_bloodcancer = 0,
    b_ovariancancer = 0,
    surv = 5
) {
  if (sex == 'Females') {
    # female raw returns probability directly
    risk_female <- risk_oralcancer_female(
      age = age,
      town = town,
      alcohol_cat6 = alcohol_cat6,
      smoke_cat = smoke_cat,
      b_bloodcancer = b_bloodcancer,
      b_ovariancancer = b_ovariancancer,
      surv = surv
    )
    return(risk_female)
    
  } else if(sex == 'Males'){
    # male raw returns probability directly
    risk_male <- risk_oralcancer_male(
      age = age,
      town = town,
      bmi = bmi,
      alcohol_cat6 = alcohol_cat6,
      smoke_cat = smoke_cat,
      b_bloodcancer = b_bloodcancer,
      b_colorectal = b_ovariancancer, # map if needed
      b_lungcancer = b_lungcancer,
      surv = surv
    )
    return(risk_male)
  }
}

# Apply oral cancer risk across a populations dataframe
apply_oral_cancer_risk_wo_risk_factors <- function(input_population, intervention = 5) {
postp1 <- input_population %>%
  filter(year == max(year, na.rm=TRUE))

postp <- postp1 %>%
  mutate(
    age = pmin(age, 90),
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
    # covariates: alcohol_cat6, b_bloodcancer, b_ovariancancer, smoke_cat, town
  )

postp <- postp %>%
  rowwise() %>%
  mutate(
    oral_cancer_risk = ifelse(
      age < 25, 0,
      risk_qcancer_oralcancer(
        age = age,
        sex = sex,
        town = custom_townsend_score_dz ,
        bmi = bmi,
        alcohol_cat6 = alcohol_cat6,
        smoke_cat = smoke_cat
      )
    ) * 1
  ) %>%
  ungroup()

input_population %>%
  select(-any_of('oral_cancer_risk')) %>%
  left_join(postp %>% select(id, oral_cancer_risk), by='id')
}

# test_population |>
#   apply_oralcancer_risk_wo_risk_factors() |>
#   ggplot( ) +
#   geom_point(aes( age, oralcancer_risk, col = bmi ) ) +
#   geom_smooth(aes( age, oralcancer_risk, col = bmi ) ) +
#   facet_wrap(~sex)
# 
# test_population |>
#   apply_oralcancer_risk_wo_risk_factors() |>
#   ggplot() +
#   geom_point(aes( age, oralcancer_risk, col = as.character(townsend_quintile)   ) ) +
#   geom_smooth(aes( age, oralcancer_risk, col = as.character(townsend_quintile), group=as.character(townsend_quintile)  ) ) +
#   facet_wrap(~sex)
