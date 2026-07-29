neph5_female <- function(
    age, 
    b_CCF, 
    b_cvd,
    b_pvd, 
    b_ra,
    b_renalstones, 
    b_sle, 
    b_treatedhyp,
    b_type1,
    b_type2, 
    bmi, 
    ethrisk, 
    fh_kidney, 
    sbp, 
    smoke_cat, 
    surv, 
    town
) {
  # --- Input validation ---
  if (age < 35 || age > 74) stop("age must be in range 35–74")
  if (bmi < 20 || bmi > 40) stop("bmi must be in range 20–40")
  if (ethrisk < 1 || ethrisk > 9) stop("ethrisk must be in range 1–9")
  if (sbp < 70 || sbp > 210) stop("sbp must be in range 70–210")
  if (smoke_cat < 0 || smoke_cat > 4) stop("smoke_cat must be in range 0–4")
  if (surv < 1 || surv > 5) stop("surv must be in range 1–5")
  if (town < -7 || town > 11) stop("town must be in range -7 to 11")
  
  bool_vars <- c(
    b_CCF, b_cvd, b_pvd, b_ra, b_renalstones, b_sle,
    b_treatedhyp, b_type1, b_type2, fh_kidney
  )
  if (any(!bool_vars %in% c(0, 1))) stop("Binary inputs must be 0 or 1")
  
  # --- Survivor function ---
  survivor <- c(
    #0, # index 0 unused
    0.999950349330902,
    0.999883890151978,
    0.999821722507477,
    0.999749183654785,
    0.999661386013031
  )
  
  # --- Conditional coefficients ---
  Iethrisk <- c(
    #0,
    0,
    0.1467681155300852,
    0.9069329917093114,
    -0.07694190442920201,
    1.1224305908740781,
    0.01187060840019042,
    0.4913159784965747,
    1.2513498001807077,
    0.30496319628043134
  )
  Ismoke <- c(
    #0,
    0.198216367886209,
    0.36968071150422488,
    0.080565277448668593,
    0.35524199977378795
  )
  
  # --- Fractional polynomial transforms ---
  dage <- age / 10
  age_1 <- dage^3
  age_2 <- dage^3 * log(dage)
  
  dbmi <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  
  dsbp <- sbp / 100
  sbp_1 <- dsbp^-2
  sbp_2 <- dsbp^-2 * log(dsbp)
  
  # --- Centering continuous variables ---
  age_1 <- age_1 - 135.12515258789063
  age_2 <- age_2 - 220.98374938964844
  bmi_1 <- bmi_1 - 0.142878264188766
  bmi_2 <- bmi_2 - 0.139003574848175
  sbp_1 <- sbp_1 - 0.582698464393616
  sbp_2 <- sbp_2 - 0.157353490591049
  town <- town - (-0.601529955863953)
  
  # --- Score calculation ---
  a <- 0
  a <- a + Iethrisk[ethrisk ]
  a <- a + Ismoke[smoke_cat ]
  
  a <- a + age_1 * 0.004490521168052976
  a <- a + age_2 * 0.0013741026779469333
  a <- a + bmi_1 * 6.2772064718078511
  a <- a + bmi_2 * -16.232549679883757
  a <- a + sbp_1 * -3.4926352766269355
  a <- a + sbp_2 * -9.8319594804682797
  a <- a + town * 0.041878983520445286
  
  a <- a + b_CCF * 1.4931034261471803
  a <- a + b_cvd * 0.29654093856294234
  a <- a + b_pvd * 0.53110422843231431
  a <- a + b_ra * 0.41828460737179313
  a <- a + b_renalstones * 0.72632375995464005
  a <- a + b_sle * 1.5446429177437644
  a <- a + b_treatedhyp * 1.5689061804732225
  a <- a + b_type1 * 3.1035727563953652
  a <- a + b_type2 * 1.5424275118825657
  a <- a + fh_kidney * 1.8581736287905835
  
  a <- a + age_1 * b_treatedhyp * -0.096281005282166501
  a <- a + age_1 * b_type1 * 0.12868711759824525
  a <- a + age_1 * b_type2 * 0.066989225879683131
  a <- a + age_2 * b_treatedhyp * 0.042311116765129199
  a <- a + age_2 * b_type1 * -0.063026228673904991
  a <- a + age_2 * b_type2 * -0.031798673621628207
  
  # --- Final score ---
  score <- 100 * (1 - survivor[surv + 1]^exp(a))
  return(score)
}


neph5_male <- function(
    age, b_CCF, b_cvd, b_pvd, b_ra, b_treatedhyp, b_type1, b_type2,
    bmi, ethrisk, fh_kidney, sbp, smoke_cat, surv, town
) {
  # === Validation ===
  if (age < 35 || age > 74) stop("age must be in range 35–74")
  if (bmi < 20 || bmi > 40) stop("bmi must be in range 20–40")
  if (ethrisk < 1 || ethrisk > 9) stop("ethrisk must be in range 1–9")
  if (sbp < 70 || sbp > 210) stop("sbp must be in range 70–210")
  if (smoke_cat < 0 || smoke_cat > 4) stop("smoke_cat must be in range 0–4")
  if (surv < 1 || surv > 5) stop("surv must be in range 1–5")
  if (town < -7 || town > 11) stop("town must be in range -7 to 11")
  
  bool_vars <- c(b_CCF, b_cvd, b_pvd, b_ra, b_treatedhyp, b_type1, b_type2, fh_kidney)
  if (any(!bool_vars %in% c(0, 1))) stop("Binary inputs must be 0 or 1")
  
  # === Survivor function ===
  survivor <- c(
    #0, # unused
    0.999939739704132,
    0.999862551689148,
    0.999781429767609,
    0.999686837196350,
    0.999580204486847
  )
  
  # === Conditional coefficients ===
  Iethrisk <- c(
    #0,
    0,
    0.31184222812344486,
    0.60462149916052599,
    0.19973519893633007,
    0.8719077058136131,
    0.4472807032846115,
    0.62399958321141891,
    -39.877231123123522,
    -0.040364685758766145
  )
  
  Ismoke <- c(
    #0,
    0.14484330477938123,
    0.16049380503890687,
    0.28799690115266502,
    0.085745140766654696
  )
  
  # === Fractional polynomial transforms ===
  dage <- age / 10
  age_1 <- dage^3
  age_2 <- dage^3 * log(dage)
  
  dbmi <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  
  dsbp <- sbp / 100
  sbp_1 <- dsbp^-2
  sbp_2 <- dsbp^(-0.5)
  
  # === Centering continuous variables ===
  age_1 <- age_1 - 130.458740234375
  age_2 <- age_2 - 211.82398986816406
  bmi_1 <- bmi_1 - 0.138219580054283
  bmi_2 <- bmi_2 - 0.136762171983719
  sbp_1 <- sbp_1 - 0.551189959049225
  sbp_2 <- sbp_2 - 0.861638963222504
  town  <- town - (-0.442067831754684)
  
  # === Score calculation ===
  a <- 0
  a <- a + Iethrisk[ethrisk ]
  a <- a + Ismoke[smoke_cat ]
  
  a <- a + age_1 * 0.018400660157609917
  a <- a + age_2 * -0.004876830484284986
  a <- a + bmi_1 * 10.309964222721741
  a <- a + bmi_2 * -23.359410989290129
  a <- a + sbp_1 * 6.4536856404376355
  a <- a + sbp_2 * -19.111575326392522
  a <- a + town * 0.019142999918577226
  
  a <- a + b_CCF * 1.392138004563414
  a <- a + b_cvd * 0.29555302083861063
  a <- a + b_pvd * 0.68439468752190213
  a <- a + b_ra * 0.42706586802221386
  a <- a + b_treatedhyp * 1.9121501989990664
  a <- a + b_type1 * 2.4260612036077056
  a <- a + b_type2 * 1.0249996940958706
  a <- a + fh_kidney * 2.2703957707797713
  
  a <- a + age_1 * b_treatedhyp * -0.081428414946102035
  a <- a + age_1 * b_type1 * 0.0058630274302377793
  a <- a + age_1 * b_type2 * 0.040933124718983353
  a <- a + age_2 * b_treatedhyp * 0.034952218728896894
  a <- a + age_2 * b_type1 * -0.0053628404874913266
  a <- a + age_2 * b_type2 * -0.019539436357429343
  
  # === Final score ===
  score <- 100 * (1 - survivor[surv + 1]^exp(a))
  return(score)
}
