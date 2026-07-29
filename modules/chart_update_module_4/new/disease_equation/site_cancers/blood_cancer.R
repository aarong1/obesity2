
# Blood cancer ----
## ## Female ----

risk_bloodcancer_female <- function(
    age,
    bmi,
    smoke_cat  = 1,
    b_braincancer = 0,
    b_ovariancancer = 0,
    b_type1 = 0,
    fh_bloodcancer = 0,
    surv= 5)
{
  survivor = c(
    #0,
    0.999728083610535,
    0.999484181404114,
    0.999221444129944,
    0.998965024948120,
    0.998681783676147
  )
  
  # The conditional arrays 
  
  Ismoke = c(
    #0,
    0.0067585720587060227000000,
    0.1786060334216023600000000,
    0.2000236983380095400000000,
    0.2824295828543657000000000
  )
  
  # Applying the fractional polynomial transforms 
  # (which includes scaling)                      
  
  dage = age
  dage=dage/10
  age_1 = dage** (2)
  age_2 = dage** (3)
  
  # Centring the continuous variables 
  
  age_1 = age_1 - 20.135219573974609
  age_2 = age_2 - 90.351325988769531
  bmi = bmi - 25.724174499511719
  
  # Start of Sum 
  a=0
  
  # The conditional sums 
  
  a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values 
  
  a = a + age_1 * 0.1837588586730924700000000
  a = a + age_2 * -0.0147172575575694360000000
  a = a + bmi * 0.0162413827296514320000000
  
  # Sum from boolean values 
  
  a = a + b_braincancer * 1.4166705727938707000000000
  a = a + b_ovariancancer * 0.4611088666746058200000000
  a = a + b_type1 * 0.4133397797532169000000000
  a = a + fh_bloodcancer * 1.4057490757127600000000000
  
  # Sum from interaction terms 
  
  
  # Calculate the score itself 
  score = (1 - survivor[surv] ^ exp(a) )
  return(score)
}

## Male ----
# 

risk_bloodcancer_male <- function(age,
                             bmi = 25,
                             smoke_cat = 1,
                             b_renalcancer = 0,
                             b_type1 = 0,
                             fh_bloodcancer = 0,
                             surv = 5) {
  # 0–5 year baseline survival
  survivors <- c(
    #0.0,
    0.999643683433533,
    0.999320745468140,
    0.998986303806305,
    0.998629689216614,
    0.998259723186493
  )
  
  # smoking effect for categories 0–4
  Ismoke <- c(
    #0.0,
    0.021413685386272918,
    0.09966847079797983,
    0.15990839062050988,
    0.21000270935119911
  )
  
  # fractional‐polynomial transforms
  dage  <- age / 10
  age1  <- dage^2
  age2  <- dage^3
  
  # centring
  age1 <- age1 - 19.604045867919922
  age2 <- age2 - 86.799774169921875
  bmi  <- bmi  - 26.309041976928711
  
  # build linear predictor
  a <- 0
  a <- a + Ismoke[smoke_cat]
  a <- a + age1 * 0.18774091714058408
  a <- a + age2 * -0.014745653676834981
  a <- a + bmi  * 0.0080906902408843691
  a <- a + b_renalcancer   * 0.37709744176579374
  a <- a + b_type1         * 0.47358388019697939
  a <- a + fh_bloodcancer  * 1.3664654694337226
  
  # pick baseline survival for chosen horizon (1…5 yrs)
  S <- survivors[surv]
  
  # risk = 100 * (1 − S^exp(a))
  score <- (1 - S^exp(a))
  return(score)
  
}

## Gendered wrapper for blood cancer risk, and an apply function

# Dispatch to male/female raw functions without internal validation
risk_qcancer_bloodcancer <- function(
    age,
    sex,
    b_braincancer = 0,
    b_ovariancancer = 0,
    b_type1      = 0,
    bmi           = 25,
    fh_bloodcancer = 0,
    smoke_cat     = 1,
    surv          = 5
) {
  if (sex == 'Females') {
    risk_bloodcancer_female(
      age = age,
      b_braincancer    = b_braincancer,
      b_ovariancancer  = b_ovariancancer,
      b_type1          = b_type1,
      bmi              = bmi,
      fh_bloodcancer   = fh_bloodcancer,
      smoke_cat        = smoke_cat,
      surv             = surv
    )
  } else if( sex == 'Males'){
    risk_bloodcancer_male(
      age              = age,
      b_renalcancer    = b_braincancer,
      b_type1          = b_type1,
      bmi              = bmi,
      fh_bloodcancer   = fh_bloodcancer,
      smoke_cat        = smoke_cat,
      surv             = surv
    )
  }
}

# Apply function to an input population dataframe
apply_blood_cancer_risk_wo_risk_factors <- function(input_population, intervention = 1) {
  postp1 <- input_population %>%
    filter(year == max(year, na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(age = pmin(age, 85),
           bmi = case_when(
             bmi == "normal"     ~ 22.5,
             bmi == "overweight" ~ 28,
             bmi == "obese"       ~ 35,
             TRUE ~ NA_real_),
           b_type1 = case_when(
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
           b_renalcancer = renal_cancer!=0,,
           b_ovariancancer = ovarian_cancer!=0
           
           # other covariates (b_braincancer, etc.) are assumed present
    ) %>%
    rowwise() %>%
    mutate(
      blood_cancer_risk = ifelse(
        age < 25, 0,
         risk_qcancer_bloodcancer(
          age              = age,
          sex              = sex,
          bmi              = bmi,
          b_type1 = b_type1,
          smoke_cat = smoke_cat

        )
      ) * 0.7
    ) %>%
    ungroup()
  
  input_population <- input_population %>%
    select(-any_of('blood_cancer_risk')) %>%
    left_join(postp %>% select(id, blood_cancer_risk), by = 'id')
  
  return(input_population)
}


# test_population |> 
#   apply_bloodcancer_risk_wo_risk_factors()
  