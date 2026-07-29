
# Hip, wrist, shoulder or spine fracture	0.6%
# Hip fracture	0.1% (neck of femur)

# Hip fracture (also known as 'fractured neck of femur')
# Distal radius fracture (which is a type of wrist fracture)
# Vertebral fracture (which is a type of fracture of the spine)
# Proximal humerus fracture (which is a fracture of the shoulder)

# NICE approved for england and wales, and scotland 
# osteoporosis fracture 

# QFracture-2016 R implementation with interventions

# Helper to enforce mandatory fields and default 'surv'
enforce_args <- function(name, value) {
  if (is.null(value)) stop(sprintf("'%s' is mandatory and must be provided.", name), call. = FALSE)
}

# Fracture risk (4-year) for females - returns probability (0–1)
risk_fracture4_female <- function(
    age,
    bmi = 25,
    ethrisk = 1,
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
    b_hrt_oest = 0,
    b_liver = 0,
    b_malabsorption = 0,
    b_parkinsons = 0,
    b_ra_sle = 0,
    b_renal = 0,
    b_type1 = 0,
    b_type2 = 0,
    fh_osteoporosis = 0,
    surv = 10,
    intervention_effect = NULL,           # numeric between 0–1, reduces final prob
    diminish_feature = NULL               # named numeric (one) for binary/fh feature
) {
  # Enforce mandatory inputs
  
  #enforce_args("age", age)
  #enforce_args("bmi", bmi)
  #enforce_args("ethrisk", ethrisk)
  
  # Only one intervention allowed
  if (!is.null(intervention_effect) && !is.null(diminish_feature)) {
    stop("Only one of 'intervention_effect' or 'diminish_feature' may be provided.", call. = FALSE)
  }
  
  # Validate intervention_effect
  if (!is.null(intervention_effect)) {
    if (!is.numeric(intervention_effect) || length(intervention_effect) != 1 || intervention_effect < 0 || intervention_effect > 1) {
      stop("'intervention_effect' must be a single numeric between 0 and 1.", call. = FALSE)
    }
  }
  
  # Validate diminish_feature
  coeffs_bin <- c(
    b_antidepressant = 0.29358915788812839,
    b_anycancer      = 0.11755227331477931,
    b_asthmacopd     = 0.19971937533528999,
    b_corticosteroids= 0.21870200942467749,
    b_cvd            = 0.14196437255030517,
    b_dementia       = 0.46973872630851521,
    b_endocrine      = 0.11053940282175923,
    b_epilepsy2      = 0.40244600986040330,
    b_falls          = 0.33223216263035837,
    b_hrt_oest       = -0.20278644563582324,
    b_liver          = 0.48311615769615862,
    b_malabsorption  = 0.16874778018355746,
    b_parkinsons     = 0.47422393580391814,
    b_ra_sle         = 0.22670593274719042,
    b_renal          = 0.25086487237940064,
    b_type1          = 0.78328871609322936,
    b_type2          = 0.23638696578140608,
    fh_osteoporosis   = 0.38379497558604947
  )
  if (!is.null(diminish_feature)) {
    if (!is.numeric(diminish_feature) || length(diminish_feature) != 1 || diminish_feature < 0 || diminish_feature > 1) {
      stop("'diminish_feature' must be a single numeric between 0 and 1.", call. = FALSE)
    }
    feat_name <- names(diminish_feature)
    if (is.null(feat_name) || !(feat_name %in% names(coeffs_bin))) {
      stop("'diminish_feature' name must match one binary/family-history feature.", call. = FALSE)
    }
  }
  
  # Baseline survivors at years 1–10
  survivors <- c(
    0.998549282550812,
    0.996993362903595,
    0.995339095592499,
    0.993514478206635,
    0.991504669189453,
    0.989245474338531,
    0.986799359321594,
    0.984268248081207,
    0.981499433517456,
    0.978452086448669
  )
  if (is.null(surv)) surv <- length(survivors)
  
  # Coefficient arrays
  Ialcohol <- c(#0, 
                -0.016119659842711558, 
                0.018142191954688299,
                0.087039813091311105, 
                0.48508766816483712, 
                0.4521470045723863)
  Iethrisk <- c(#0, 
                0, 
                -0.42566069216366254, 
                -0.55432091195021416,
                -0.91826010978069306, 
                -0.68193606531483042,
                -1.4668483404988077,
                -0.9101238114228446, 
                -0.6421783317544739, 
                -0.5036829432634511)
  Ismoke   <- c(#0, 
                0.05573569343056117, 
                0.16338956617013528,
                0.1540488338696587, 
                0.23297715917579045)
  
  # Transforms & centering
  dage   <- age/10
  age_1  <- dage^2 - 25.463895797729492
  age_2  <- dage^3 - 128.49531555175781
  dbmi   <- bmi/10
  bmi_1  <- dbmi^-1 - 0.382189363241196
  
  # Linear predictor
  a <- 0
  a <- a + Ialcohol[alcohol_cat6]
  a <- a + Iethrisk[ethrisk]
  a <- a + Ismoke[smoke_cat]
  a <- a + age_1 *  0.14882306172165083
  a <- a + age_2 * -0.0095516624764288762
  a <- a + bmi_1 *  2.818029138982781
  
  # Add binary/fh terms
  a <- a + b_antidepressant  * coeffs_bin["b_antidepressant"]
  a <- a + b_anycancer       * coeffs_bin["b_anycancer"]
  a <- a + b_asthmacopd      * coeffs_bin["b_asthmacopd"]
  a <- a + b_corticosteroids * coeffs_bin["b_corticosteroids"]
  a <- a + b_cvd             * coeffs_bin["b_cvd"]
  a <- a + b_dementia        * coeffs_bin["b_dementia"]
  a <- a + b_endocrine       * coeffs_bin["b_endocrine"]
  a <- a + b_epilepsy2       * coeffs_bin["b_epilepsy2"]
  a <- a + b_falls           * coeffs_bin["b_falls"]
  a <- a + b_hrt_oest        * coeffs_bin["b_hrt_oest"]
  a <- a + b_liver           * coeffs_bin["b_liver"]
  a <- a + b_malabsorption   * coeffs_bin["b_malabsorption"]
  a <- a + b_parkinsons      * coeffs_bin["b_parkinsons"]
  a <- a + b_ra_sle          * coeffs_bin["b_ra_sle"]
  a <- a + b_renal           * coeffs_bin["b_renal"]
  a <- a + b_type1           * coeffs_bin["b_type1"]
  a <- a + b_type2           * coeffs_bin["b_type2"]
  a <- a + fh_osteoporosis    * coeffs_bin["fh_osteoporosis"]
  
  # Apply diminish_feature (if any)
  if (!is.null(diminish_feature)) {
    f <- diminish_feature[[1]]
    val <- get(names(diminish_feature))
    coef <- coeffs_bin[names(diminish_feature)]
    a <- a - coef * val * f
  }
  
  # Compute base probability
  prob <- 1 - survivors[surv]^(exp(a))
  
  # Apply intervention_effect (if any)
  if (!is.null(intervention_effect)) {
    prob <- prob * (1 - intervention_effect)
  }
  
  return(prob)
  
}
  
  # Analogous: Fracture risk (10-year) for males
# returns probability (0–1) with optional interventions

risk_fracture4_male <- function(
    age,
    bmi = 25,
    ethrisk = 1,
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
    b_liver = 0,
    b_malabsorption = 0,
    b_parkinsons = 0,
    b_ra_sle = 0,
    b_renal = 0,
    b_type1 = 0,
    b_type2 = 0,
    fh_osteoporosis = 0,
    surv = 10,
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
    b_antidepressant = 0.41850098025644566,
    b_anycancer      = 0.29238496678496179,
    b_asthmacopd     = 0.26582431501520032,
    b_carehome       = 0.094672730961445892,
    b_corticosteroids= 0.26015928939547550,
    b_cvd            = 0.21554209768209007,
    b_dementia       = 0.62530663904759942,
    b_epilepsy2      = 0.66957080643014266,
    b_falls          = 0.44771413426265022,
    b_liver          = 0.79709360810249363,
    b_malabsorption  = 0.25089210354989244,
    b_parkinsons     = 0.82542763402188735,
    b_ra_sle         = 0.38465858327350932,
    b_renal          = 0.44796728800624863,
    b_type1          = 1.01216891450673050,
    b_type2          = 0.24392544324902793,
    fh_osteoporosis   = 0.98987175124665938
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
    0.999435424804688,
    0.998826444149017,
    0.998191893100739,
    0.997505903244019,
    0.996722519397736,
    0.995886385440826,
    0.994963765144348,
    0.993958413600922,
    0.992862939834595,
    0.991695225238800
  )
  if (is.null(surv)) surv <- length(survivors)
  
  # Categorical coefficient arrays
  Ialcohol <- c(#0,
                -0.11296487134120664,
                -0.10199331473836099,
                -0.00871721778380552,
                0.23783672918943868,
                0.59006748283242694)
  Iethrisk <- c(#0,
                0,
                -0.24977593616949206,
                -0.33887234093637719,
                -1.17806523127562170,
                -0.56375361284174452,
                -0.88026742214030562,
                -0.62208390680560210,
                -0.87002407600843878,
                -0.35869346039908434)
  
  Ismoke   <- c(#0,
                0.03746225471129239,
                0.25366546242832472,
                0.27285416899049170,
                0.34982597804377003)
  
  # Fractional-polynomial transforms & centering
  dage   <- age / 10
  age_1  <- sqrt(dage)        - 2.201908826828003
  age_2  <- dage               - 4.848402023315430
  dbmi   <- bmi   / 10
  bmi_1  <- dbmi^-1            - 0.375702142715454
  bmi_2  <- dbmi^-0.5          - 0.612945437431335
  
  # Build linear predictor
  a <- 0
  a <- a + Ialcohol[alcohol_cat6]
  a <- a + Iethrisk[ethrisk]
  a <- a + Ismoke[smoke_cat]
  
  a <- a + age_1 * -8.59621133957421790
  a <- a + age_2 *  2.35345855675999570
  a <- a + bmi_1 * 18.35347536390157700
  a <- a + bmi_2 * -19.09052734677333200
  
  # Add binary/fh terms
  a <- a + b_antidepressant * coeffs_bin_m["b_antidepressant"]
  a <- a + b_anycancer      * coeffs_bin_m["b_anycancer"]
  a <- a + b_asthmacopd     * coeffs_bin_m["b_asthmacopd"]
  a <- a + b_carehome       * coeffs_bin_m["b_carehome"]
  a <- a + b_corticosteroids* coeffs_bin_m["b_corticosteroids"]
  a <- a + b_cvd            * coeffs_bin_m["b_cvd"]
  a <- a + b_dementia       * coeffs_bin_m["b_dementia"]
  a <- a + b_epilepsy2      * coeffs_bin_m["b_epilepsy2"]
  a <- a + b_falls          * coeffs_bin_m["b_falls"]
  a <- a + b_liver          * coeffs_bin_m["b_liver"]
  a <- a + b_malabsorption  * coeffs_bin_m["b_malabsorption"]
  a <- a + b_parkinsons     * coeffs_bin_m["b_parkinsons"]
  a <- a + b_ra_sle         * coeffs_bin_m["b_ra_sle"]
  a <- a + b_renal          * coeffs_bin_m["b_renal"]
  a <- a + b_type1          * coeffs_bin_m["b_type1"]
  a <- a + b_type2          * coeffs_bin_m["b_type2"]
  a <- a + fh_osteoporosis  * coeffs_bin_m["fh_osteoporosis"]
  
  # Apply diminish_feature if provided
  if (!is.null(diminish_feature)) {
    fval <- diminish_feature[[1]]
    fname <- names(diminish_feature)
    a <- a - coeffs_bin_m[fname] * get(fname) * fval
  }
  
  # Compute base probability
  prob <- 1 - survivors[surv]^(exp(a))
  
  # Apply intervention effect if provided
  if (!is.null(intervention_effect)) {
    prob <- prob * (intervention_effect)
    
  }
  
  return(prob)
}






# Top-level wrappers selecting by 'sex'
# (as defined previously)


# Top-level dispatcher for QFracture-2016
# type = "fracture4" or "nof"; sex = "female" or "male"

calculate_qfracture_fracture <- function(
    sex,
    age,
    ethrisk,
    bmi               = 25,
    smoke_cat         = 1,
    alcohol_cat6      = 1,
    b_antidepressant  = 0,
    b_anycancer       = 0,
    b_asthmacopd      = 0,
    b_carehome        = 0,
    b_corticosteroids = 0,
    b_cvd             = 0,
    b_dementia        = 0,
    b_endocrine       = 0,
    b_epilepsy2       = 0,
    b_falls           = 0,
    b_fracture4       = 0,
    b_hrt_oest        = 0,
    b_liver           = 0,
    b_malabsorption   = 0,
    b_parkinsons      = 0,
    b_ra_sle          = 0,
    b_renal           = 0,
    b_type1           = 0,
    b_type2           = 0,
    fh_osteoporosis   = 0,
    surv              = 10,
    intervention_effect = NULL,
    diminish_feature   = NULL
      ) {
  #sex  <- match.arg(sex)
  
  if(sex=='Males'){
    
      risk <- risk_fracture4_male(
                       age = age, 
                       ethrisk = ethrisk,
                       bmi = bmi,
                       smoke_cat         = smoke_cat,
                       alcohol_cat6      = alcohol_cat6,
                       b_antidepressant  = b_antidepressant,
                       b_anycancer       = b_anycancer,
                       b_asthmacopd      = b_asthmacopd,
                       b_carehome        = b_carehome,
                       b_corticosteroids = b_corticosteroids,
                       b_cvd             = b_cvd,
                       b_dementia        = b_dementia,
                       b_epilepsy2       = b_epilepsy2,
                       b_falls           = b_falls,
                       b_liver           = b_liver,
                       b_malabsorption   = b_malabsorption,
                       b_parkinsons      = b_parkinsons,
                       b_ra_sle          = b_ra_sle,
                       b_renal           = b_renal,
                       b_type1           = b_type1,
                       b_type2           = b_type2,
                       fh_osteoporosis   = fh_osteoporosis,
                       surv              = surv,
                       intervention_effect = intervention_effect,
                       diminish_feature   = diminish_feature)
    
  } else if(sex == 'Females'){
  
    risk <- risk_fracture4_female(
      age = age,
      ethrisk = ethrisk,
      bmi = bmi,
      smoke_cat         = smoke_cat,
      alcohol_cat6      = alcohol_cat6,
      b_antidepressant  = b_antidepressant,
      b_anycancer       = b_anycancer,
      b_asthmacopd      = b_asthmacopd,
      b_corticosteroids = b_corticosteroids,
      b_cvd             = b_cvd,
      b_dementia        = b_dementia,
      b_endocrine       = b_endocrine,
      b_epilepsy2       = b_epilepsy2,
      b_falls           = b_falls,
      b_hrt_oest        = b_hrt_oest,
      b_liver           = b_liver,
      b_malabsorption   = b_malabsorption,
      b_parkinsons      = b_parkinsons,
      b_ra_sle          = b_ra_sle,
      b_renal           = b_renal,
      b_type1           = b_type1,
      b_type2           = b_type2,
      fh_osteoporosis   = fh_osteoporosis,
      surv              = surv,
      intervention_effect = intervention_effect,
      diminish_feature   = diminish_feature)
  }
  return(unname(risk))
 }




calculate_qfracture_fracture( sex="Females",
                    age=65, bmi=22, ethrisk=3)

risk_fracture4_male(age = 65,bmi = 22,ethrisk = 3)


# -----------------------------------------------------------------------------
# Apply functions: compute risk without other risk factors
# -----------------------------------------------------------------------------

# Wrapper to apply 10-year overall fracture risk
apply_fracture4_risk_wo_risk_factors <- function(
    input_population,
    intervention_effect = NULL,
    diminish_feature = NULL
) {
  library(dplyr)
  
  # Subset to most recent year
  postp <- input_population %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    mutate(
      # Cap age at 85
      age = pmin(age, 85),
      # Encode ethnicity
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
      ),
      
      # Encode BMI categories
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 28,
        bmi == "obese"       ~ 35,
        TRUE                           ~ NA_real_
      )
      ) |> 
    rowwise() |> 
    mutate(
      
      # Compute 4-year fracture risk
      fracture4_risk = ifelse(
        age < 30, 0,
        calculate_qfracture_fracture(
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
  
  # Merge risk back into full population
  input_population %>%
    select(-any_of('fracture4_risk')) %>%
    left_join(
      postp %>% select(id, fracture4_risk),
      by = "id"
    )
}

