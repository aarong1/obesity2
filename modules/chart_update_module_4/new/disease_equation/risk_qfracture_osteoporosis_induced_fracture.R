
fracture4_male <- function(age, 
                           alcohol_cat6,
                           smoke_cat,
                           bmi, 
                           b_antidepressant = 0, 
                           b_anycancer = 0, 
                           b_asthmacopd = 0,
                           b_carehome = 0, 
                           b_corticosteroids = 0,
                           b_cvd = 0,
                           b_dementia = 0,
                           b_epilepsy = 02,
                           b_falls = 0, 
                           b_fracture4 = 0, 
                           b_liver = 0, 
                           b_malabsorption = 0, 
                           b_parkinsons = 0,
                           b_ra_sle = 0, 
                           b_renal = 0, 
                           b_type1 = 0, 
                           b_type2 = 0, 
                           
                           ethrisk=0, 
                           fh_osteoporosis = 0
                           ) {
  
  # Survivor probabilities
  survivor <- c(
    0,
    0.999644935131073,
    0.999273777008057,
    0.998869180679321,
    0.998421549797058,
    0.997928738594055,
    0.997390747070313,
    0.996753513813019,
    0.996096074581146,
    0.995346963405609,
    0.994551837444305,
    0.993598461151123,
    0.992581725120544,
    0.991482257843018,
    0.990263521671295,
    0.989037752151489,
    0.987761437892914,
    0.986425399780273,
    0.984853565692902
  )
  
  # Conditional arrays
  Ialcohol <- c(0, 
                -0.0753424993511384, 
                0.00356409201605206, 
                0.11071809294679587, 
                0.27727727298188781, 
                0.76293841342804958)
  
  Iethrisk <- c(0, 
                0,
                -0.25782479851902956, 
                -0.27396916018626188, 
                -1.2488100943578264, 
                -0.44781369031222829,
                -0.95698337178329307, 
                -0.6454670770263975, 
                -0.24416687132687531, 
                -0.55856718797289318)
  
  Ismoke <- c(0, 
              -0.000803951352001642, 
              0.1560272763218023,
              0.25117409813223207, 
              0.27967401140088227)
  
  # Fractional polynomial transforms
  dage <- age / 10
  dbmi <- bmi / 10
  age_1 <- dage^0.5 - 2.213409662246704
  age_2 <- dage - 4.899182319641113
  bmi_1 <- dbmi^(-1) - 0.376987010240555
  bmi_2 <- dbmi^(-0.5) - 0.613992691040039
  
  # Start of sum
  a <- 0
  
  # Add conditional sums
  a <- a + Ialcohol[alcohol_cat6 ]
  a <- a + Iethrisk[ethrisk]
  a <- a + Ismoke[smoke_cat]
  
  # Add continuous variable contributions
  a <- a + age_1 * -9.0010590056070825
  a <- a + age_2 * 2.4013416577413533
  a <- a + bmi_1 * 18.178986548463467
  a <- a + bmi_2 * -18.91647404660355
  
  # Add boolean variable contributions
  bool_coefficients <- c(
    b_antidepressant = 0.46871937557887416,
    b_anycancer = 0.45075005338651963,
    b_asthmacopd = 0.28866933110119714,
    b_carehome = 0.46240175997411309,
    b_corticosteroids = 0.29590704827022962,
    b_cvd = 0.2342575101174369,
    b_dementia = 0.64101075890791592,
    b_epilepsy2 = 0.78213945924202077,
    b_falls = 0.54278016879014757,
    b_fracture4 = 0.30376483170944424,
    b_liver = 0.94929834714932115,
    b_malabsorption = 0.21980433977230238,
    b_parkinsons = 0.89713150428493182,
    b_ra_sle = 0.44031912127988931,
    b_renal = 0.45650294178223877,
    b_type1 = 0.8447272010743575,
    b_type2 = 0.22193850259057335,
    fh_osteoporosis = 1.6999403855072708
  )
  
  inputs <- c(b_antidepressant, 
              b_anycancer,
              b_asthmacopd,
              b_carehome, 
              b_corticosteroids,
              b_cvd, 
              b_dementia, 
              b_epilepsy2,
              b_falls, 
              b_fracture4,
              b_liver, 
              b_malabsorption,
              b_parkinsons, 
              b_ra_sle, 
              b_renal, 
              b_type1, 
              b_type2,
              fh_osteoporosis)
  
  a <- a + sum(inputs * unlist(bool_coefficients))
  
  # Calculate the score
  score <- 100 * (1 - survivor[surv]^(exp(a)))
  
  return(score)
}





# fracture4_male(
#   age = 50, alcohol_cat6 = 1, b_antidepressant = 1, b_anycancer = 0, 
#   b_asthmacopd = 0, b_carehome = 0, b_corticosteroids = 0, b_cvd = 0, 
#   b_dementia = 0, b_epilepsy2 = 0, b_falls = 0, b_fracture4 = 0, 
#   b_liver = 0, b_malabsorption = 0, b_parkinsons = 0, b_ra_sle = 0, 
#   b_renal = 0, b_type1 = 0, b_type2 = 0, bmi = 25, ethrisk = 1, 
#   fh_osteoporosis = 0, smoke_cat = 0, surv = 10
# )