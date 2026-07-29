
risk_renalcancer_female <- function(
    age,
    town,
    bmi = 25, 
    smoke_cat = 1, 
    b_type2 = 0, 
    b_bloodcancer = 0,
    b_braincancer = 0, 
    b_cervicalcancer = 0, 
    b_colorectal = 0,
    b_ovariancancer = 0, 
    b_uterinecancer = 0,
    surv = 5
) {
  
  ## Baseline survivor function (index 0-5 in C → 1-6 in R)
  survivor <- c(
    # 0.000000000000000,  # dummy slot so that survivor[surv + 1] = C’s survivor[surv]
    0.999912858009338,
    0.999821424484253,
    0.999732971191406,
    0.999641656875610,
    0.999542832374573
  )
  
  ## Smoking category coefficients
  Ismoke <- c(
    # 0.0000000000000000000000000,
    0.2424368501539901600000000,
    0.5544456610978699700000000,
    0.8003428243906539600000000,
    0.8526176873713247100000000
  )
  
  ## Fractional-polynomial age transforms
  dage   <- age / 10
  age_1  <- dage
  age_2  <- dage^2
  
  dtown <- town/5
  town_1 <- dtown
  town_2 <- dtown^2
  
  ## Centre continuous variables
  age_1 <- age_1 - 4.487745761871338
  age_2 <- age_2 - 20.139862060546875
  bmi   <- bmi   - 25.724693298339844
  
  ## Linear predictor
  a <- 0
  a <- a + Ismoke[smoke_cat]                    # +1 because R is 1-based
  
  a <- a + age_1 *  1.9824578275162619
  a <- a + age_2 * -0.10496076209659227
  a <- a + bmi   *  0.010336870596784856
  
  a <- a + b_bloodcancer  * 0.48950495525763654
  a <- a + b_braincancer  * 2.3206648663049099
  a <- a + b_cervicalcancer * 0.94192979452946313
  a <- a + b_colorectal   * 0.36193845390480039
  a <- a + b_ovariancancer * 0.96187511010839699
  a <- a + b_type2        * 0.29590425304821777
  a <- a + b_uterinecancer * 0.75290270813016125
  a <- a + town_1         * 0.13132370105130087
  a <- a + town_2         * -0.40271804429320668
  
  ## Absolute 5-year risk (same as C: 1 – survivor^exp(lp))
  risk <- 1 - survivor[surv] ^ exp(a)
  return(risk)
}

risk_renalcancer_female(age = 84,town = -7)

## Male ----


risk_renalcancer_male <- function(
    age,
    town,
    bmi = 25,
    smoke_cat = 1,
    b_colorectal = 0,
    b_lungcancer = 0,
    b_prostatecancer = 0,
    b_type2 = 0,
    surv = 5
)
{
  survivor = c(
    # 0,
    0.999815762042999,
    0.999644875526428,
    0.999471843242645,
    0.999277472496033,
    0.999066352844238
  )
  
  # The conditional arrays 
  
  Ismoke = c(
    # 0,
    0.2112239987625918200000000,
    0.4980213954417773700000000,
    0.7237512139320682000000000,
    0.8124579211392302100000000
  )
  
  # Applying the fractional polynomial transforms 
  # (which includes scaling)                      
  
  dage = age
  dage=dage/10
  age_1 = dage
  age_2 = dage ^ 3
  
  # Centring the continuous variables 
  
  age_1 = age_1 - 4.426476955413818
  age_2 = age_2 - 86.731056213378906
  bmi = bmi - 26.308208465576172
  town = town - 0.260041594505310
  
  # Start of Sum 
  a=0
  
  # The conditional sums 
  
  a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values 
  
  a = a + age_1 * 1.6395376233886698000000000
  a = a + age_2 * -0.0069787249855328588000000
  a = a + bmi * 0.0191266080190585430000000
  a = a + town * -0.0039642613172689916000000
  
  # Sum from boolean values 
  
  a = a + b_colorectal * 0.2250243172974877300000000
  a = a + b_lungcancer * 0.5785135435280358600000000
  a = a + b_prostatecancer * 0.3753145930635795000000000
  a = a + b_type2 * 0.1898117535793944500000000
  
  # Sum from interaction terms 
  
  
  # Calculate the score itself 
  score = (1 - survivor[surv] ^ exp(a) )
  return(score)
}

risk_renalcancer_male(age = 84,town = -5)


## Gendered wrapper for renal cancer risk, apply function, and simplified wo_risk_factors()

# Dispatch based on sex, call raw functions and return probability
risk_qcancer_renal_cancer<- function(
    age,
    sex,
    town,
    bmi = 25,
    smoke_cat = 1,
    b_bloodcancer = 0,
    b_braincancer = 0,
    b_cervicalcancer = 0,
    b_colorectal = 0,
    b_ovariancancer = 0,
    b_type2 = 0,
    b_uterinecancer = 0,
    surv = 5
) {
  if (tolower(sex) %in% c('female','females')) {
    # female raw returns probability directly
    return(
      risk_renalcancer_female(
        age = age,
        b_bloodcancer = b_bloodcancer,
        b_braincancer = b_braincancer,
        b_cervicalcancer = b_cervicalcancer,
        b_colorectal = b_colorectal,
        b_ovariancancer = b_ovariancancer,
        b_type2 = b_type2,
        b_uterinecancer = b_uterinecancer,
        bmi = bmi,
        smoke_cat = smoke_cat,
        surv = surv,
        town = town
      )
    )
  } else {
    # male raw returns probability directly
    return(
      risk_renalcancer_male(
        age = age,
        b_colorectal = b_colorectal,
        b_lungcancer = b_ovariancancer, # map if needed
        b_prostatecancer = b_uterinecancer,
        b_type2 = b_type2,
        bmi = bmi,
        smoke_cat = smoke_cat,
        surv = surv,
        town = town # male uses single town
      )
    )
  }
}

# Apply renal cancer risk across a population dataframe
apply_renal_cancer_risk_wo_risk_factors <- function(input_population, intervention = 5) {
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
      custom_townsend_score_dz,
      b_type2 = case_when(
        diabetes_status == 'diagnosed_diabetes' ~ 1,
        diabetes_status == 'undiagnosed_diabetes' ~ 1,
        diabetes_status == 'no_diabetes' ~0,
        TRUE ~ 0
      ),
      smoke_cat = case_when(
        smoking == 'current_smoker' ~ 4,
        smoking == 'former' ~ 2,
        smoking == 'never_smoked' ~ 1,
        TRUE ~ 1
      ),
      b_bloodcancer = blood_cancer != 0,
      b_braincancer = brain_cancer != 0,
      b_cervicalcancer = cervical_cancer != 0,
      # b_colorectal = colorectal_cancer != 0,
      b_ovariancancer = ovarian_cancer != 0,
      b_uterinecancer = uterine_cancer != 0
    ) %>%
    rowwise() %>%
    mutate(
      renal_cancer_risk = ifelse(
        age < 25, 0,
        risk_qcancer_renal_cancer(
          age = age,
          sex = sex,
          bmi = bmi,
          town = custom_townsend_score_dz,
          smoke_cat = smoke_cat,
          b_type2 = b_type2#,
          # b_bloodcancer = b_bloodcancer,
          # b_braincancer = b_braincancer,
          # b_cervicalcancer = b_cervicalcancer,
          # b_colorectal = b_colorectal,
          # b_ovariancancer = b_ovariancancer,
          # b_uterinecancer = b_uterinecancer,
        )
      ) * 0.2359707
    ) %>%
    ungroup()
  
  input_population %>%
    select(-any_of('renal_cancer_risk')) %>%
    left_join(postp %>% select(id, renal_cancer_risk), by = 'id')
}

# 
# test_population |>
#   apply_renal_cancer_risk_wo_risk_factors() |> #pull(renal_cancer_risk)
#   ggplot( ) +
#   geom_point(aes( age, renal_cancer_risk, col = bmi ) ) +
#   geom_smooth(aes( age, renal_cancer_risk, col = bmi ) ) +
#   facet_wrap(~sex)
# 
# test_population |>
#   apply_renal_cancer_risk_wo_risk_factors() |>
#   ggplot() +
#   geom_point(aes( age, renal_cancer_risk, col = as.character(townsend_quintile)   ) ) +
#   geom_smooth(aes( age, renal_cancer_risk, col = as.character(townsend_quintile), group=as.character(townsend_quintile)  ) ) +
#   facet_wrap(~sex)
