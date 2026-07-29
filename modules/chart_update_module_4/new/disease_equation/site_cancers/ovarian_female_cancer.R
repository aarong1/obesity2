
#Ovarian cancer ----
## Female ----
# 


risk_ovariancancer_female <- function(
    age,                      # numeric (years 25–84)
    sex,
    bmi,                      # numeric (kg/m²)
    age_1_fh_ovariancancer = 0,   # 0 / 1
    age_2_fh_ovariancancer = 0,   # 0 / 1
    b_breastcancer = 0,           # 0 / 1
    b_cervicalcancer = 0,         # 0 / 1
    b_cop = 0,                    # 0 / 1
    fh_ovariancancer = 0,         # 0 / 1
    surv = 5                      # integer 1–5 (risk horizon)
) {
  
  if(sex == 'Males'){
    return(0)
  }else if(sex == 'Females'){
  # 1. Baseline survivor function values for years 1–5
  S0 <- c(
    #0
    0.999722599983215,
    0.999479830265045,
    0.999221026897430,
    0.998951792716980,
    0.998684525489807
  )
  
  # 2. Fractional-polynomial transforms (with scaling by 10 where needed)
  dage  <- age / 10
  age_1 <- dage                        # x¹
  age_2 <- dage * log(dage)            # x·log(x)
  
  # 3. Centre continuous variables
  age_1 <- age_1 - 4.487829208374023
  age_2 <- age_2 - 6.737888336181641
  bmi   <- bmi   - 25.727840423583984
  
  # 4. Build linear predictor
  a <- 0
  
  #   4a. Continuous terms
  a <- a + age_1 *  3.6496242385839661
  a <- a + age_2 * -1.1991731219303103
  a <- a + bmi   *  0.011304598769420525
  
  #   4b. Binary indicators
  a <- a + age_1_fh_ovariancancer * (-4.292666853656549)
  a <- a + age_2_fh_ovariancancer *   1.688204933293370
  a <- a + b_breastcancer         *   0.485469510959992
  a <- a + b_cervicalcancer       *   0.473056942589241
  a <- a + b_cop                  *  (-0.430066820865163)
  a <- a + fh_ovariancancer       *   1.336359483469376
  
  # 5. Absolute risk (%) over the chosen horizon
  risk <-  (1 - S0[surv] ^ exp(a))
  return(risk)
  }
}


# Apply ovarian cancer risk across a population dataframe
apply_ovarian_cancer_risk_wo_risk_factors <- function(input_population, intervention = 1) {
postp1 <- input_population %>%
  filter(year == max(year, na.rm=TRUE))

postp <- postp1 %>%
  mutate(
    age = pmin(age, 90),
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
    ovarian_cancer_risk = ifelse(
      age < 20, 0,
      risk_ovariancancer_female(
        age = age,
        sex = sex,
        bmi = bmi
        # alcohol_cat6 = alcohol_cat6,
        # smoke_cat = smoke_cat,
      )
    ) * 0.6
  ) %>%
  ungroup()

input_population %>%
  select(-any_of('ovarian_cancer_risk')) %>%
  left_join(postp %>% select(id, ovarian_cancer_risk), by='id')
}

# test_population |>
#   apply_ovariancancer_risk_wo_risk_factors() |>
#   ggplot( ) +
#   geom_point(aes( age, ovariancancer_risk, col = bmi ) ) +
#   geom_smooth(aes( age, ovariancancer_risk, col = bmi ) ) +
#   facet_wrap(~sex)
# 
# test_population |>
#   apply_ovariancancer_risk_wo_risk_factors() |>
#   ggplot() +
#   geom_point(aes( age, ovariancancer_risk, col = as.character(townsend_quintile)   ) ) +
#   geom_smooth(aes( age, ovariancancer_risk, col = as.character(townsend_quintile), group=as.character(townsend_quintile)  ) ) +
#   facet_wrap(~sex)

