
# Prostate cancer ----
## Male ----

risk_prostate_cancer_male<- function(
    age,                         # numeric   25–84
    sex,
    town,                        # Townsend score (centred later)
    ethrisk,                     # 1–9      (ONS categories)
    bmi = 20,                         # numeric kg/m²
    smoke_cat = 1,                   # 0–4      (never → heavy)
    b_type1 = 0,                     # 0 / 1   (type-1 diabetes)
    b_type2  ,                     # 0 / 1   (type-2 diabetes)
    age_1_fh_prostatecancer = 0.1,     # 0 / 1
    age_2_fh_prostatecancer = 0.1,     # 0 / 1
    b_manicschiz = 0.01,                # 0 / 1
    fh_prostatecancer = 0.1,           # 0 / 1
    surv = 5                        # 1–5 year risk horizon
) {
  
  if (sex == 'Females') {
    return(0)
  } else{
  
  ## Baseline survivor function (1…5 years)
  S0 <- c(
    0.999701976776123,
    0.999389469623566,
    0.999051749706268,
    0.998661398887634,
    0.998229324817657
  )
  
  ## Ethnicity and smoking category coefficients
  Iethrisk <- c(
    #0.000000000000000000,
    -0.508766992354229950,
    -0.866982197008514530,
    -1.236432569385744800,
    -0.778053671995835860,
    1.044086983146447900,
    0.684437419411313860,
    -0.698503085196340970,
    0.393088307774554890
  )
  
  Ismoke <- c(
    # 0.000000000000000000,
    0.015883779217294801,
    -0.249514465571867850,
    -0.304576145481981920,
    -0.237987860679658740
  )
  
  ## Interaction (age × smoking) coefficients
  age1_int <- c(
    # 0.000000000000000000,
    -2.053305941779285200,
    -2.890546310165653200,
    -9.369836980243906800,
    -8.858450699853746000
  )
  
  age2_int <- c(
    # 0.000000000000000000,
    -9.637377048311796200,
    -3.431074594677306200,
    -21.162570460923313000,
    -24.646378113213263000
  )
  
  ## Fractional-polynomial transforms (with centring)
  dage  <- age / 10
  age_1 <- dage^(-0.5)                 - 0.475571960210800
  age_2 <- dage^(-0.5) * log(dage)     - 0.706925392150879
  
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^-1                     - 0.380129784345627
  bmi_2 <- dbmi^(-0.5)                 - 0.616546630859375
  
  town_c <- town - 0.261956512928009   # centred Townsend
  
  ## Linear predictor
  a <- 0
  a <- a + Iethrisk[ethrisk]                    # ethnicity main effect
  a <- a + Ismoke[smoke_cat]                # smoking main effect
  
  a <- a + age_1 * (-13.099612581816686)
  a <- a + age_2 *   55.276108637451607
  a <- a + bmi_1 * (-14.297877101339337)
  a <- a + bmi_2 *   18.293733971731786
  a <- a + town_c *  (-0.0284342617816042)
  
  a <- a + age_1_fh_prostatecancer *  2.7874621493140661 
  # age_1_fh_prostatecancer = age_1 * fh_prostatecancer
  a <- a + age_2_fh_prostatecancer * (-29.048495836727859)
  # age_2_fh_prostatecancer = age_2 * fh_prostatecancer
  # these are interaction terms NOT inputs as 
  # suggested by the function definition
  a <- a + b_manicschiz            * (-0.44046374073333017)
  a <- a + b_type1                 * (-0.55834661211309444)
  a <- a + b_type2                 * (-0.11085159340787583)
  a <- a + fh_prostatecancer       *   2.0344915550609244
  
  ## Interactions age×smoking
  a <- a + age_1 * age1_int[smoke_cat]
  a <- a + age_2 * age2_int[smoke_cat]
  
  ## Absolute risk (%)
  risk <-  (1 - S0[surv] ^ exp(a))
  return(risk)
  }
}

# Apply prostate cancer risk across a population dataframe
apply_prostate_cancer_risk_wo_risk_factors <- function(input_population, intervention = 5) {
  postp1 <- input_population %>%
    filter(year == max(year, na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(age = pmin(age, 90),
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
           custom_townsend_score_dz,
           ethrisk) %>%
    rowwise() %>%
    mutate(
      prostate_cancer_risk = ifelse(
        age < 25, 0,
        risk_prostate_cancer_male(
          sex = sex,
          age = age,
          bmi = bmi,
          town = custom_townsend_score_dz,
          ethrisk = ethrisk,
          smoke_cat = smoke_cat,
          b_type2 = b_type2
        )
      ) * 1.7675
    ) %>%
    ungroup()
  
  input_population %>%
    select(-any_of('prostate_cancer_risk')) %>%
    left_join(postp %>% 
              select(id, prostate_cancer_risk),
              by = 'id')
}

# y <- risk_prostate_cancer_male(
#   age = 50, 
#   sex = 'Males', 
#   town = 0,
#   bmi = 20:35)

# bmi = 20:40
# dbmi  <- bmi / 10
# bmi_1 <- dbmi^-1- 0.380129784345627
# bmi_2 <- dbmi^(-0.5)- 0.616546630859375

# ## Linear predictor
# a <- 0

# a <- a + bmi_1 * (-14.297877101339337)
# a <- a + bmi_2 *   18.293733971731786
# plot(1 - 0.999 ^ exp(a))

# test_population |>
#   apply_prostate_cancer_risk_wo_risk_factors() |>
#   ggplot( ) +
#   geom_point(aes( age, prostate_cancer_risk, col = bmi ) ) +
#   geom_smooth(aes( age, prostate_cancer_risk, col = bmi ) ) +
#   facet_wrap(~sex)

# test_population |>
#   apply_prostate_cancer_risk_wo_risk_factors() |>
#   ggplot() +
#   geom_point(aes( age, prostate_cancer_risk, col = as.character(townsend_quintile)   ) ) +
#   geom_smooth(aes( age, prostate_cancer_risk, col = as.character(townsend_quintile), group=as.character(townsend_quintile)  ) ) +
#   facet_wrap(~sex)


