## Gendered wrapper for colorectal cancer risk, apply function, and simplified wo_risk_factors()

# Wrapper to dispatch based on sex
risk_qcancer_colorectalcancer_gendered <- function(
    age,
    age_1_fh_gicancer,
    age_2_fh_gicancer,
    alcohol_cat6,
    b_breastcancer,
    b_cervicalcancer,
    b_colitis,
    b_ovariancancer,
    b_polyp,
    b_type2,
    b_uterinecancer,
    ethrisk,
    fh_gicancer,
    smoke_cat,
    surv=1,
    town,
    sex
) {
  if (tolower(sex) %in% c('female', 'females')) {
    # Female-specific raw function
    colorectal_female(
      age = age,
      age_1_fh_gicancer = age_1_fh_gicancer,
      age_2_fh_gicancer = age_2_fh_gicancer,
      alcohol_cat6 = alcohol_cat6,
      b_breastcancer = b_breastcancer,
      b_cervicalcancer = b_cervicalcancer,
      b_colitis = b_colitis,
      b_ovariancancer = b_ovariancancer,
      b_polyp = b_polyp,
      b_type2 = b_type2,
      b_uterinecancer = b_uterinecancer,
      ethrisk = ethrisk,
      fh_gicancer = fh_gicancer,
      smoke_cat = smoke_cat,
      surv = surv
    )
  } else {
    # Male-specific raw function
    colorectal_male(
      age = age,
      age_1_fh_gicancer = age_1_fh_gicancer,
      age_2_fh_gicancer = age_2_fh_gicancer,
      alcohol_cat6 = alcohol_cat6,
      b_bloodcancer = b_breastcancer, # map appropriately
      b_colitis = b_colitis,
      b_lungcancer = b_ovariancancer,
      b_oralcancer = b_polyp,
      b_polyp = b_polyp,
      b_type2 = b_type2,
      bmi = 25,                   # default BMI
      ethrisk = ethrisk,
      fh_gicancer = fh_gicancer,
      smoke_cat = smoke_cat,
      surv = surv,
      town = town
    )
  }
}

# Apply colorectal cancer risk across a population dataframe
apply_colorectalcancer_risk_wo_risk_factors <- function(input_population) {
  postp1 <- input_population %>%
    filter(year == max(year, na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = pmin(age, 90)
      # expect covariates: age_1_fh_gicancer, age_2_fh_gicancer,
      # alcohol_cat6, b_breastcancer, b_cervicalcancer, b_colitis,
      # b_ovariancancer, b_polyp, b_type2, b_uterinecancer,
      # ethrisk, fh_gicancer, smoke_cat, town
    ) %>%
    rowwise() %>%
    mutate(
      colorectalcancer_risk = ifelse(
        age < 25, 0,
        100 * risk_qcancer_colorectalcancer_gendered(
          age = age,
          age_1_fh_gicancer = age_1_fh_gicancer,
          age_2_fh_gicancer = age_2_fh_gicancer,
          alcohol_cat6 = alcohol_cat6,
          b_breastcancer = b_breastcancer,
          b_cervicalcancer = b_cervicalcancer,
          b_colitis = b_colitis,
          b_ovariancancer = b_ovariancancer,
          b_polyp = b_polyp,
          b_type2 = b_type2,
          b_uterinecancer = b_uterinecancer,
          ethrisk = ethrisk,
          fh_gicancer = fh_gicancer,
          smoke_cat = smoke_cat,
          surv = 5,
          town = town,
          sex = sex
        )
      )
    ) %>%
    ungroup()
  
  input_population %>%
    select(-any_of('colorectalcancer_risk')) %>%
    left_join(postp %>% select(id, colorectalcancer_risk), by = 'id')
}

# Simplified function call: wo_risk_factors for colorectal cancer
# Uses only age, sex, and town (if applicable), other parameters defaulted

risk_qcancer_colorectalcancer_wo_risk_factors <- function(
    age,
    sex,
    town = 1,
    surv = 5
) {
  # Default values for all other covariates
  risk_qcancer_colorectalcancer_gendered(
    age = age,
    age_1_fh_gicancer = 0,
    age_2_fh_gicancer = 0,
    alcohol_cat6 = 1,
    b_breastcancer = 0,
    b_cervicalcancer = 0,
    b_colitis = 0,
    b_ovariancancer = 0,
    b_polyp = 0,
    b_type2 = 0,
    b_uterinecancer = 0,
    ethrisk = 1,
    fh_gicancer = 0,
    smoke_cat = 1,
    surv = surv,
    town = town,
    sex = sex
  )
}