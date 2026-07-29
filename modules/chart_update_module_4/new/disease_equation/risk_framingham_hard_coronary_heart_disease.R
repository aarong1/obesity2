calculate_hard_chd_risk <- function(Age, 
                                    Total_cholesterol,
                                    hdl_cholesterol,
                                    sbp, 
                                    treatment_htn = 0,
                                    smoker = 0,
                                    gender = "male") {

  if (gender =='women'){
L = 31.764001 * ln(Age) +
  22.465206 * ln(Total_cholesterol) +
  (-1.187731) * ln(HDL_cholesterol) +
  2.552905 * ln(Systolic_BP) +
  0.420251 * Treated_for_blood_pressure +
  13.07543 * Smoker + 
  (-5.060998) * ln(Age) * ln(Total_cholesterol) +
  (-2.996945) * ln(Age) * Smoker - 
  146.5933061
  
baseline_hazard <- 0.98767
  
} else { #gender =='male'

L = 52.00961 * ln(Age) +
  20.014077 * ln(Total_cholesterol) +
  (-0.905964) * ln(HDL_cholesterol) +
  1.305784 * ln(Systolic_BP) +
  0.241549 * Treated_for_blood_pressure +
  12.096316 * Smoker +
  (-4.605038) * ln(Age) * ln(Total_cholesterol) +
  (-2.84367) * ln(Age) * Smoker +
  (-2.93323) * ln(Age) * ln(Age) -
  172.300168

baseline_hazard <- 0.9402

}

P = 1 - baseline_hazard^(exp(L))

}
# Function to calculate 10-year risk for Hard CHD based on the Framingham Risk Score
calculate_hard_chd_risk <- function(age, total_cholesterol, hdl_cholesterol, sbp, 
                                    treatment_htn = 0, smoker = 0, gender = "male") {
  
  # Log-transform input variables
  log_age <- log(age)
  log_total_cholesterol <- log(total_cholesterol)
  log_hdl_cholesterol <- log(hdl_cholesterol)
  log_sbp <- log(sbp)
  
  # Coefficients for men
  if (gender == "male") {
    coeff_age <- 52.009610
    coeff_total_chol <- 20.014077
    coeff_hdl <- -0.905964
    coeff_sbp <- 1.305784
    coeff_treatment <- 0.241549
    coeff_smoker <- 12.096316
    coeff_age_total_chol <- -4.605038
    coeff_age_smoker <- -2.843670
    coeff_age_squared <- -2.933230
    baseline_survival <- 0.940200
  } else {
    # Coefficients for women
    coeff_age <- 31.764001
    coeff_total_chol <- 22.465206
    coeff_hdl <- -1.187731
    coeff_sbp <- 2.552905
    coeff_treatment <- 0.420251
    coeff_smoker <- 13.075430
    coeff_age_total_chol <- -5.060998
    coeff_age_smoker <- -2.996945
    # Women have no age-squared term, so set it to 0
    coeff_age_squared <- 0
    baseline_survival <- 0.987670
  }
  
  # Risk Score Calculation
  risk_score <- (coeff_age * log_age) +
                (coeff_total_chol * log_total_cholesterol) +
                (coeff_hdl * log_hdl_cholesterol) +
                (coeff_sbp * log_sbp) +
                (coeff_treatment * treatment_htn) +
                (coeff_smoker * smoker) +
                (coeff_age_total_chol * log_age * log_total_cholesterol) +
                (coeff_age_smoker * log_age * smoker) +
                (coeff_age_squared * log_age^2)
  
  # 10-year risk
  risk <- 1 - (baseline_survival ^ exp(risk_score))
  
  # Return the risk percentage
  return(risk * 100)
}

# Example usage:
calculate_hard_chd_risk(age = 55, total_cholesterol = 240, hdl_cholesterol = 50, 
                        sbp = 140, treatment_htn = 1, smoker = 1, gender = "male")
