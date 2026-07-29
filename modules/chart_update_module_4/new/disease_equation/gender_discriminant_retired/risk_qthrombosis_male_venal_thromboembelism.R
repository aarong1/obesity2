# VTE risk calculator for males
vte_male <- function(age, b_CCF, b_admit, b_antipsychotic, b_anycancer, b_copd, b_ibd, b_renal, 
                     b_varicosevein, bmi, smoke_cat, surv, town) {
  
  # Survivor array (pre-calculated)
  survivor <- c(0, 0.9993539, 0.9986778, 0.9978991, 0.9970629, 0.9961508)
  
  # Coefficients for smoking categories
  Ismoke <- c(0, 0.07322798, 0.2090383, 0.2352668, 0.2891967)
  
  # Fractional polynomial transforms
  dage <- age / 10
  age_1 <- dage^3
  age_2 <- dage^3 * log(dage)
  
  dbmi <- bmi / 10
  bmi_1 <- dbmi^(-2)
  bmi_2 <- bmi_1 * log(dbmi)
  
  # Centering continuous variables
  age_1 <- age_1 - 98.67565
  age_2 <- age_2 - 151.0342
  bmi_1 <- bmi_1 - 0.1413167
  bmi_2 <- bmi_2 - 0.1382609
  town <- town - 0.2174438
  
  # Initializing the risk score
  a <- 0
  
  # Adding smoking category
  a <- a + Ismoke[smoke_cat]
  
  # Adding continuous variable contributions
  a <- a + age_1 * 0.03387864
  a <- a + age_2 * -0.01343558
  a <- a + bmi_1 * 8.014656
  a <- a + bmi_2 * -26.85445
  a <- a + town * 0.01182779
  
  # Adding binary risk factors
  a <- a + b_CCF * 0.2945135
  a <- a + b_admit * 0.3732582
  a <- a + b_antipsychotic * 0.3992543
  a <- a + b_anycancer * 0.6337245
  a <- a + b_copd * 0.346318
  a <- a + b_ibd * 0.4879618
  a <- a + b_renal * 0.4871457
  a <- a + b_varicosevein * 0.4131507
  
  # Calculating the final score
  score <- 100 * (1 - (survivor[surv + 1]^exp(a)))
  
  return(score)
}

# Example usage of the function:
# vte_male(age = 50, b_CCF = 1, b_admit = 1, b_antipsychotic = 0, b_anycancer = 0, b_copd = 1, 
#          b_ibd = 0, b_renal = 0, b_varicosevein = 0, bmi = 25, smoke_cat = 2, surv = 3, town = 0)
