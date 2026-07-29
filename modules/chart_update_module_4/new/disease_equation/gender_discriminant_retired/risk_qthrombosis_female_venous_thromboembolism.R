vte_female_risk <- function(age, b_CCF, b_admit, b_antipsychotic, b_anycancer, b_cop, b_copd, b_hrt, 
                            b_ibd, b_renal, b_tamoxifen, b_varicosevein, bmi, smoke_cat, surv, town) {
  # Survivor function (5-year survival rates at different time points)
  survivor <- c(0, 0.999402642250061, 0.998779714107513, 0.998076260089874, 0.997314155101776, 0.996479928493500)
  
  # Coefficients for smoking categories
  # non
  # ex
  # light <10
  # moderate <19
  # heavey >20
  
  Ismoke <- c(0, 0.0899056072614921, 0.2096026499560841, 0.2698567860827918, 0.37779267161809493)
  
  # Apply fractional polynomial transformations
  dage <- age / 10
  age_2 <- log(dage)
  age_1 <- dage^(-0.5)
  dbmi <- bmi / 10
  bmi_1 <- dbmi^(-2)
  bmi_2 <- dbmi^(-2) * log(dbmi)
  
  # Center the continuous variables
  age_1 <- age_1 - 0.461668938398361
  age_2 <- age_2 - 1.545814394950867
  bmi_1 <- bmi_1 - 0.146233677864075
  bmi_2 <- bmi_2 - 0.140570744872093
  town <- town - 0.081886291503906
  
  # Start summing the contributions from the continuous and categorical variables
  a <- 0
  
  # Add contributions from smoking category
  a <- a + Ismoke[smoke_cat]  # smoke_cat is 0-indexed in C but 1-indexed in R
  
  # Add contributions from continuous variables
  a <- a + age_1 * 44.38304638346105
  a <- a + age_2 * 12.430963361971429
  a <- a + bmi_1 * 4.2938468556841043
  a <- a + bmi_2 * -22.686465809497374
  a <- a + town * 0.024325695810313554
  
  # Add contributions from boolean variables
  a <- a + b_CCF * 0.32035852745471716
  a <- a + b_admit * 0.36482704170626978
  a <- a + b_antipsychotic * 0.5419744307906361
  a <- a + b_anycancer * 0.50735512080321943
  a <- a + b_cop * 0.26517273102741074
  a <- a + b_copd * 0.39731720602755477
  a <- a + b_hrt * 0.07287794278307834
  a <- a + b_ibd * 0.4023036851423945
  a <- a + b_renal * 0.4367724008370839
  a <- a + b_tamoxifen * 0.36732897841362733
  a <- a + b_varicosevein * 0.390719459302283
  
  # Calculate the final VTE risk score using the survivor function
  score <- 100.0 * (1 - survivor[surv + 1]^exp(a))  # surv is 0-indexed in C
  
  return(score)
}

# Example usage
vte_female_risk(
  age = 50,
  b_CCF = 0, b_admit = 1, b_antipsychotic = 0, b_anycancer = 0, b_cop = 0, 
  b_copd = 0, b_hrt = 1, b_ibd = 0, b_renal = 0, b_tamoxifen = 0, 
  b_varicosevein = 1, bmi = 28, smoke_cat = 1, surv = 3, town = 0.5
)
