
# Neck-of-femur (NOF) fracture risk  for females — returns probability (0–1)

# NICE approved for england and wales, and scotland 
# Osteoporosis


default_nof_female <- function(
    age = NULL,
    bmi = NULL,
    ethrisk = NULL,
    
    smoke_cat = 1,
    alcohol_cat6 = 1,
    
    b_antidepressant = 0,
    b_anycancer = 0,
    b_asthmacopd = 0,
    b_corticosteroids = 0,
    b_cvd = 0,
    b_dementia = 0,
    b_endocrine = 0,
    b_epilepsy2 = 0,
    b_falls = 0,
    b_fracture4 = 0,
    b_hrt_oest = 0,
    b_liver = 0,
    b_parkinsons = 0,
    b_ra_sle = 0,
    b_renal = 0,
    b_type1 = 0,
    b_type2 = 0,
    fh_osteoporosis = 0,
    surv = NULL,
    intervention_effect = NULL,
    diminish_feature = NULL
) {
  # Enforce mandatory
  enforce_args("age", age)
  enforce_args("bmi", bmi)
  enforce_args("ethrisk", ethrisk)
  
  # Only one intervention
  if (!is.null(intervention_effect) && !is.null(diminish_feature)) stop("Only one of intervention_effect or diminish_feature may be provided.")
  
  # Validate as before
  if (!is.null(intervention_effect) && (intervention_effect < 0 || intervention_effect > 1)) stop("intervention_effect must be between 0 and 1.")
  
  coeffs_bin_f <- c(
    b_antidepressant=0.29613702281361015,
    b_anycancer=0.11413504536129365,
    b_asthmacopd=0.14586468140838021,
    b_corticosteroids=0.17801655206966349,
    b_cvd=0.13867259004313143,
    b_dementia=0.55933926151498436,
    b_endocrine=0.19889323459236768,
    b_epilepsy2=0.45447051866494581,
    b_falls=0.26827166495754934,
    b_fracture4=0.44859061295690350,
    b_hrt_oest=-0.25853153132813594,
    b_liver=0.47643941255732225,
    b_parkinsons=0.54448153618029238,
    b_ra_sle=0.30841080245869545,
    b_renal=0.24878174406741482,
    b_type1=1.33501555881261340,
    b_type2=0.41285144813251984,
    fh_osteoporosis=0.38379497558604947
  )
  if (!is.null(diminish_feature)) {
    fname <- names(diminish_feature)
    if (!(fname %in% names(coeffs_bin_f))) stop("Invalid feature to diminish.")
    fval <- diminish_feature[[1]]
    a <- 0  # will subtract below
  }
  
  # Baseline survivors
  survs <- c(0.999798893928528,
             0.999581158161163,
             0.999346554279327,
             0.999083101749420,
             0.998790085315704,
             0.998455047607422,
             0.998097419738770,
             0.997704565525055,
             0.997259438037872,
             0.996739983558655)
  if (is.null(surv)) surv <- length(survs)
  
  # Categorical arrays
  Ialcohol <- c(0,-0.12617582013302897,-0.13732433479084133,-0.11336168348939307,0.40178870335718131,0.50065721318809830)
  Iethrisk <- c(0,0,-0.52102821686313605,-0.41779421048371701,-0.79512369940410910,
                -0.65881047977255047,-1.57962255477994940,-1.07775807356383240,
                -0.85217856317616292,-0.57150351840750313)
  Ismoke   <- c(0,0.029418868029262912,0.326375642832078430,0.349643727870208980,0.515517691469181690)
  
  # Transforms
  dage <- age/10; age_1 <- dage^2-25.815217971801758; age_2 <- dage^3-131.16371154785156
  dbmi <- bmi/10; bmi_1 <- dbmi^-2-0.146003782749176
  
  # Linear predictor
  a <- Ialcohol[alcohol_cat6+1] + Iethrisk[ethrisk+1] + Ismoke[smoke_cat+1]
  a <- a + age_1*0.26005507923658827 + age_2*(-0.017124380501438056) + bmi_1*5.2078461133313851
  for (nm in names(coeffs_bin_f)) a <- a + get(nm)*coeffs_bin_f[nm]
  
  # Apply diminish
  if (!is.null(diminish_feature)) a <- a - coeffs_bin_f[[fname]] * get(fname) * fval
  
  prob <- 1 - survs[surv]^(exp(a))
  if (!is.null(intervention_effect)) prob <- prob*(1-intervention_effect)
  return(prob)
}

# Neck-of-femur (NOF) fracture risk (4-year) for males — returns probability (0–1)
default_nof_male <- function(
    age = NULL,
    bmi = NULL,
    ethrisk = NULL,
    smoke_cat = 1,
    alcohol_cat6 = 1,
    b_antidepressant = 0,
    b_anycancer = 0,
    b_asthmacopd = 0,
    b_carehome = 0,
    b_corticosteroids = 0,
    b_cvd = 0,
    b_dementia = 0,
    b_epilepsy2 = 0,
    b_falls = 0,
    b_fracture4 = 0,
    b_liver = 0,
    b_parkinsons = 0,
    b_ra_sle = 0,
    b_renal = 0,
    b_type1 = 0,
    b_type2 = 0,
    fh_osteoporosis = 0,
    surv = NULL,
    intervention_effect = NULL,
    diminish_feature = NULL
) {
  # Enforce mandatory inputs
  enforce_args("age", age)
  enforce_args("bmi", bmi)
  enforce_args("ethrisk", ethrisk)
  
  # Only one intervention allowed
  if (!is.null(intervention_effect) && !is.null(diminish_feature)) {
    stop("Only one of 'intervention_effect' or 'diminish_feature' may be provided.", call. = FALSE)
  }
  
  # Validate intervention_effect
  if (!is.null(intervention_effect)) {
    if (!is.numeric(intervention_effect) || length(intervention_effect) != 1 ||
        intervention_effect < 0 || intervention_effect > 1) {
      stop("'intervention_effect' must be a single numeric between 0 and 1.", call. = FALSE)
    }
  }
  
  # Binary/family-history coefficients for diminishing
  coeffs_bin_m <- c(
    b_antidepressant = 0.42677868800990187,
    b_anycancer      = 0.23284997516454178,
    b_asthmacopd     = 0.23480155549787779,
    b_carehome       = 0.18790031027255144,
    b_corticosteroids= 0.17401085680145045,
    b_cvd            = 0.23769025952003547,
    b_dementia       = 0.84640833781655367,
    b_epilepsy2      = 0.70235950031403205,
    b_falls          = 0.34610487349139629,
    b_fracture4      = 0.64051042798776270,
    b_liver          = 0.83550379108632500,
    b_parkinsons     = 0.98806866468877430,
    b_ra_sle         = 0.42013414173764280,
    b_renal          = 0.57189278878097272,
    b_type1          = 1.36692825190257760,
    b_type2          = 0.30570436623928593,
    fh_osteoporosis   = 0.27337605103109514
  )
  
  # Validate diminish_feature
  if (!is.null(diminish_feature)) {
    if (!is.numeric(diminish_feature) || length(diminish_feature) != 1 ||
        diminish_feature < 0 || diminish_feature > 1) {
      stop("'diminish_feature' must be a single numeric between 0 and 1.", call. = FALSE)
    }
    feat_name <- names(diminish_feature)
    if (is.null(feat_name) || !(feat_name %in% names(coeffs_bin_m))) {
      stop("'diminish_feature' name must match one binary/family-history feature.", call. = FALSE)
    }
  }
  
  # Baseline survivors at years 1–10
  survivors <- c(
    0.999893844127655,
    0.999774158000946,
    0.999640882015228,
    0.999497473239899,
    0.999327778816223,
    0.999136328697205,
    0.998924136161804,
    0.998683452606201,
    0.998405933380127,
    0.998105704784393
  )
  if (is.null(surv)) surv <- length(survivors)
  
  # Categorical coefficient arrays
  Ialcohol <- c(
    0,
    -0.19892778799156768,
    -0.26135127858736523,
    -0.21652612292615428,
    0.07716824745737189,
    0.48658895922459683
  )
  Iethrisk <- c(
    0,
    0,
    -0.51977072137266467,
    -0.42736228902907902,
    -1.36167851019038390,
    -0.89793032737509737,
    -0.92011786226761050,
    -0.58502497198020909,
    -1.26258324405887420,
    -0.39347225411628184
  )
  Ismoke <- c(
    0,
    0.017210771932229665,
    0.43340145804840513,
    0.47063054551016315,
    0.63590786208283900
  )
  
  # Fractional-polynomial transforms & centering
  dage  <- age / 10
  age_1 <- dage^3 - 113.99311828613281
  age_2 <- dage^3 * log(dage) - 179.96238708496094
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^-2 - 0.14116498827934300
  
  # Build linear predictor
  a <- 0
  a <- a + Ialcohol[alcohol_cat6 + 1]
  a <- a + Iethrisk[ethrisk + 1]
  a <- a + Ismoke[smoke_cat + 1]
  a <- a + age_1 *  0.041982559539881142
  a <- a + age_2 * -0.015251995233972332
  a <- a + bmi_1 *  5.716754271006956300
  
  # Add binary/fh terms
  for (nm in names(coeffs_bin_m)) {
    a <- a + get(nm) * coeffs_bin_m[nm]
  }
  
  # Apply diminish_feature if provided
  if (!is.null(diminish_feature)) {
    fval <- diminish_feature[[1]]
    fname <- names(diminish_feature)
    a <- a - coeffs_bin_m[fname] * get(fname) * fval
  }
  
  # Compute probability (0–1)
  prob <- 1 - survivors[surv]^(exp(a))
  
  # Apply intervention_effect (if any)
  if (!is.null(intervention_effect)) {
    prob <- prob * (1 - intervention_effect)
  }
  
  return(prob)
}



calculate_qfracture_nof <- function(current_population,
                                sex = c("Males", "Females"),
                                age,
                                bmi,
                                ethrisk,
                                smoke_cat = 1,
                                alcohol_cat6 = 1,
                                b_antidepressant = 0,
                                b_anycancer = 0,
                                b_asthmacopd = 0,
                                b_corticosteroids = 0,
                                b_cvd = 0,
                                b_dementia = 0,
                                b_endocrine = 0,
                                b_epilepsy2 = 0,
                                b_falls = 0,
                                b_fracture4 = 0,
                                b_hrt_oest = 0,
                                b_carehome = 0,
                                b_liver = 0,
                                b_parkinsons = 0,
                                b_ra_sle = 0,
                                b_renal = 0,
                                b_type1 = 0,
                                b_type2 = 0,
                                fh_osteoporosis = 0,
                                surv = 4,
                                intervention_effect = NULL,
                                diminish_feature = NULL) {
    # Normalize gender input

    if (sex == 'Females') {
      default_nof_female(
        age = age,
        bmi = bmi,
        ethrisk = ethrisk,
        smoke_cat = smoke_cat,
        alcohol_cat6 = alcohol_cat6,
        b_antidepressant = b_antidepressant,
        b_anycancer = b_anycancer,
        b_asthmacopd = b_asthmacopd,
        b_corticosteroids = b_corticosteroids,
        b_cvd = b_cvd,
        b_dementia = b_dementia,
        b_endocrine = b_endocrine,
        b_epilepsy2 = b_epilepsy2,
        b_falls = b_falls,
        b_fracture4 = b_fracture4,
        b_hrt_oest = b_hrt_oest,
        b_liver = b_liver,
        b_parkinsons = b_parkinsons,
        b_ra_sle = b_ra_sle,
        b_renal = b_renal,
        b_type1 = b_type1,
        b_type2 = b_type2,
        fh_osteoporosis = fh_osteoporosis,
        surv = surv,
        intervention_effect = intervention_effect,
        diminish_feature = diminish_feature
      )
    } else if (sex == "Males") {
      default_nof_male(
        age = age,
        bmi = bmi,
        ethrisk = ethrisk,
        smoke_cat = smoke_cat,
        alcohol_cat6 = alcohol_cat6,
        b_antidepressant = b_antidepressant,
        b_anycancer = b_anycancer,
        b_asthmacopd = b_asthmacopd,
        b_carehome = b_carehome,
        b_corticosteroids = b_corticosteroids,
        b_cvd = b_cvd,
        b_dementia = b_dementia,
        b_epilepsy2 = b_epilepsy2,
        b_falls = b_falls,
        b_fracture4 = b_fracture4,
        b_liver = b_liver,
        b_parkinsons = b_parkinsons,
        b_ra_sle = b_ra_sle,
        b_renal = b_renal,
        b_type1 = b_type1,
        b_type2 = b_type2,
        fh_osteoporosis = fh_osteoporosis,
        surv = surv,
        intervention_effect = intervention_effect,
        diminish_feature = diminish_feature
      )
    } else {
      stop("`sex` must be 'Males' or 'Females'.")
    }
  
  
  
  
  }

# Top-level wrappers selecting by 'sex'
# (as defined previously)

# -----------------------------------------------------------------------------
# Apply functions: compute risk without other risk factors
# -----------------------------------------------------------------------------



# Wrapper to apply 4-year neck-of-femur (NOF) risk
apply_nof_risk_wo_risk_factors <- function(
    input_population,
    intervention_effect = NULL,
    diminish_feature = NULL
) {

  postp <- input_population %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    mutate(
      age = pmin(age, 85),
      ethrisk = recode(
        ethnicity,
        white           = 1,
        `irish traveller` = 1,
        roma            = 1,
        indian          = 2,
        filipino        = 5,
        arab            = 5,
        `other asian`   = 5,
        pakistani       = 3,
        chinese         = 8,
        `black african` = 7,
        `black other`   = 6,
        mixed           = 9,
        other           = 9
      ) ,
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 28,
        bmi == "obese"       ~ 35,
        TRUE ~ NA_real_
      )) |>
    rowwise() |> 
    mutate(
      nof_risk = ifelse(
        age < 30, 0,
        calculate_qfracture_nof(
          sex   = sex,
          age   = age,
          bmi   = bmi,
          ethrisk = ethrisk,
          surv  = 10,
          intervention_effect = intervention_effect,
          diminish_feature   = diminish_feature
        )
      )
    ) %>%
    ungroup()
  
  input_population %>%
    select(-any_of('nof_risk')) %>%
    left_join(
      postp %>% select(id, nof_risk),
      by = "id"
    )
}


test_population |> 
  apply_nof_risk_wo_risk_factors() |> #names()
  ggplot() + 
  geom_point(aes(age, nof_risk))
