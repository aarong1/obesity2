
risk_pancreatic_cancer_female <- function(
    age,            # numeric: 25–84 y
    town,            # Townsend score (continuous)
    bmi = 20,            # numeric: kg/m²
    smoke_cat = 1,      # integer 0–4
    b_breastcancer = 0, # 0 / 1
    b_chronicpan = 0,   # 0 / 1 (chronic pancreatitis)
    b_renalcancer = 0,  # 0 / 1
    b_type2 = 0,        # 0 / 1 (type-2 diabetes)
    surv = 5           # integer 1–5 (risk horizon, years)
) {
  ## 1. Baseline survivor-function values (years 1 to 5)
  S0 <- c(
    0.999964594841003,
    0.999925315380096,
    0.999884843826294,
    0.999844610691071,
    0.999795138835907
  )
  
  ## 2. Smoking category coefficients
  Ismoke <- c(
    #0.000000000000000000,
    0.025375758413601845,
    0.569920687829630010,
    0.639056878397405660,
    0.704289450581061290
  )
  
  ## 3. Fractional-polynomial transform (with scaling)
  dage  <- age / 10
  age_1 <- dage^(-0.5)
  
  ## 4. Centre continuous variables
  age_1 <- age_1 - 0.471977025270462
  bmi   <- bmi   - 25.729867935180664
  town  <- town  - 0.161796689033508
  
  ## 5. Build the linear predictor
  a <- 0
  a <- a + Ismoke[smoke_cat]     # +1 because R indices start at 1
  a <- a + age_1 * (-23.260189976963467)
  a <- a + bmi   *  0.009781510953804978
  a <- a + town  *  0.011402050541305815
  
  a <- a + b_breastcancer * 0.3192435657707442
  a <- a + b_chronicpan   * 1.2927221506222435
  a <- a + b_renalcancer  * 0.6789970707278265
  a <- a + b_type2        * 0.4107439191649736
  
  ## 6. Absolute risk (%) at the chosen horizon
  risk <-  (1 - S0[surv] ^ exp(a))
  return(risk)
}

## Male ----

risk_pancreatic_cancer_male <- function(
    age,            # numeric, 25–84
    bmi = 25,            # numeric, kg/m²
    smoke_cat = 1,      # 0–4  (never → heavy)
    b_bloodcancer =0,  # 0 / 1
    b_chronicpan =0,   # 0 / 1
    b_type2 =0,        # 0 / 1 (type-2 diabetes)
    surv = 5           # 1–5  (risk horizon, yrs)
) {
  ## Baseline survivor at 1-5 yr
  S0 <- c(
    #0
    0.999952435493469,
    0.999906480312347,
    0.999861896038055,
    0.999808788299561,
    0.999760389328003
  )
  
  ## Smoking coefficients (same ordering as categories 0-4)
  Ismoke <- c(
    #0.000000000000000000,
    0.083678878341541807,
    0.444894932058506720,
    0.672901202436161800,
    0.663625971422573510
  )
  
  ## Fractional-polynomial transforms
  dage  <- age / 10
  age_1 <- dage^(-0.5)
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  
  ## Centre continuous terms
  age_1 <- age_1 - 0.475096940994263
  bmi_1 <- bmi_1 - 0.144456043839455
  bmi_2 <- bmi_2 - 0.139745324850082
  
  ## Linear predictor
  a <- 0
  a <- a + Ismoke[smoke_cat ]
  a <- a + age_1 * (-22.388503771890694)
  a <- a + bmi_1 *  4.2251656934838371
  a <- a + bmi_2 * (-11.281643872666246)
  
  a <- a + b_bloodcancer * 0.5369701587873449
  a <- a + b_chronicpan   * 1.6922917636081924
  a <- a + b_type2        * 0.6132344324406092
  
  ## Absolute risk (%) at chosen horizon
  risk <- (1 - S0[surv] ^ exp(a))
  return(risk)
}


## Gendered wrapper for pancreatic cancer risk, apply function, and simplified wo_risk_factors()

# Dispatch based on sex, call raw functions and return probability
risk_qcancer_pancreatic_cancer<- function(
    age,
    sex,
    town,
    bmi = 25,
    smoke_cat = 1,
    b_breastcancer = 0,
    b_chronicpan = 0,
    b_renalcancer = 0,
    b_type2 = 0,
    surv = 5
) {
  if (sex == 'Females') {
    # female raw returns percentage
    risk <- risk_pancreatic_cancer_female(
      age = age,
      bmi = bmi,
      town = town,
      b_type2 = b_type2,
      smoke_cat = smoke_cat,
      b_breastcancer = b_breastcancer,
      b_chronicpan = b_chronicpan,
      b_renalcancer = b_renalcancer,
      surv = surv
    )
  } else {
    # male raw returns percentage
    risk <- risk_pancreatic_cancer_male(
      age = age,
      bmi = bmi,
      smoke_cat = smoke_cat,
      b_type2 = b_type2,
      b_bloodcancer = b_breastcancer,  # map if needed
      b_chronicpan = b_chronicpan,
      surv = surv
    )
  }
  # convert percentage to probability
  return(risk)
}

# Apply pancreatic cancer risk across a population dataframe
apply_pancreatic_cancer_risk_wo_risk_factors <- function(input_population, intervention = 5) {
  postp1 <- input_population %>%
    filter(year == max(year, na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = pmin(age, 90),
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 28,
        bmi == "obese"       ~ 35,
        TRUE ~ NA_real_),
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
      custom_townsend_score_dz
      # expect covariates: b_breastcancer, b_chronicpan, b_renalcancer,
      # b_type2, bmi, smoke_cat, town
    ) %>%
    rowwise( ) %>%
    mutate(
      pancreatic_cancer_risk = ifelse(
        age < 25, 0,
        risk_qcancer_pancreatic_cancer(
          age = age,
          sex = sex,
          town = custom_townsend_score_dz,
          bmi = bmi,
          b_type2 = b_type2,
          smoke_cat = smoke_cat
        )
      ) * 1.6
    ) %>%
    ungroup()
  
  input_population %>%
    select(-any_of('pancreatic_cancer_risk')) %>%
    left_join(postp %>% 
                select(id, pancreatic_cancer_risk),
              by = 'id')
}

# Simplified function: only age, sex, surv, town (if needed)

# teset_population <- instantiate_base_pop(test_specification)
# 
# test_population |>
#   apply_pancreatic_cancer_risk_wo_risk_factors() |>
#   ggplot( ) +
#   geom_point(aes( age, pancreatic_cancer_risk, col = bmi ) ) +
#   geom_smooth(aes( age, pancreatic_cancer_risk, col = bmi ) ) +
#   facet_wrap(~sex)
# 
# test_population |>
#   apply_pancreatic_cancer_risk_wo_risk_factors() |>
#   ggplot() +
#   geom_point(aes( age, pancreatic_cancer_risk, col = as.character(townsend_quintile)   ) ) +
#   geom_smooth(aes( age, pancreatic_cancer_risk, col = as.character(townsend_quintile), group=as.character(townsend_quintile)  ) ) +
#   facet_wrap(~sex)
