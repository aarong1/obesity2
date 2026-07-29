
# Uterine cancer -----
## Female -----


risk_uterine_cancer_female <- function(
    age,
    sex,
    bmi = 25,
    smoke_cat = 1,
    b_type2 = 0,
    b_breastcancer = 0,
    b_colorectal = 0,
    b_endometrial = 0,
    b_manicschiz = 0,
    b_pos = 0,
    surv = 5
)
{
  
  if (sex == 'Males'){
    return(0)
  }
  
  survivor = c(
    # 0,
    0.999801218509674,
    0.999620676040649,
    0.999434530735016,
    0.999217212200165,
    0.998992919921875
  )
  
  # The conditional arrays 
  
  Ismoke = c(
    # 0,
    -0.2020713821985857000000000,
    -0.1817584707326591600000000,
    -0.3021556609298930400000000,
    -0.4191963756060861400000000
  )
  
  # Applying the fractional polynomial transforms 
  # (which includes scaling)                      
  
  dage = age
  dage = dage/10
  age_1 = dage ^ .5
  age_2 = dage
  dbmi = bmi
  dbmi = dbmi/10
  bmi_1 = dbmi ^ 2
  
  # Centring the continuous variables 
  
  age_1 = age_1 - 2.118349790573120
  age_2 = age_2 - 4.487406253814697
  bmi_1 = bmi_1 - 6.617829799652100
  
  # Start of Sum 
  a=0
  
  # The conditional sums 
  
  a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values 
  
  a = a + age_1 * 23.1774636105576340000000000
  a = a + age_2 * -4.4743743749881331000000000
  a = a + bmi_1 * 0.1463194102262433400000000
  
  # Sum from boolean values 
  
  a = a + b_breastcancer * 0.9128754137084009700000000
  a = a + b_colorectal * 0.4465582900206094300000000
  a = a + b_endometrial * 0.8550002865110695200000000
  a = a + b_manicschiz * 0.4385483915097909700000000
  a = a + b_pos * 0.6853665306190324100000000
  a = a + b_type2 * 0.2969025294844695500000000
  
  # Sum from interaction terms 
  
  # Calculate the score itself 
  score = (1 - survivor[surv] ^ exp(a) )
  return(score)
}


# Apply uterine cancer risk across a population dataframe
apply_uterian_cancer_risk_wo_risk_factors <- function(input_population, intervention = 5) {
  postp1 <- input_population %>%
    filter(year == max(year, na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = pmin(age, 84),
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 28,
        bmi == "obese"       ~ 35,
        TRUE ~ 20 ),
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
      b_breastcancer = female_breast_cancer != 0,
      b_colorectal = colorectal_cancer != 0,
      
      
      # expect covariates: b_breastcancer, b_colorectal, b_endometrial,
      # b_manicschiz, b_pos, b_type2, bmi, smoke_cat
    ) %>%
    rowwise( ) %>%
    mutate(
      uterine_cancer_risk = ifelse(
        # males and age < 25 ineligible
        age < 25, 0,
        risk_uterine_cancer_female(
          age = age,
          sex = sex,
          bmi = bmi,
          smoke_cat = smoke_cat,
          b_type2 = b_type2,
        )
      ) #*61.5
    ) %>%
    ungroup()
  
  input_population %>%
    select(-any_of('uterine_cancer_risk')) %>%
    left_join(
      postp %>% select(id, uterine_cancer_risk),
      by = 'id'
    )
}



# test_population |>
#   apply_uterian_cancer_risk_wo_risk_factors() |> #pull(uterine_cancer_risk)
#   ggplot( ) +
#   geom_point(aes( age, uterine_cancer_risk, col = bmi ) ) +
#   #geom_smooth(aes( age, uterine_cancer_risk, col = bmi ) ) +
#   facet_wrap(~sex)



