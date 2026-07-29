# risk_qthrombosis_venal_thromboembelism.R
 age = 25
  sex = 'Males'
  town = 2
  
  bmi = 25
  smoke_cat = 1  #1-5
  
  b_CCF = 0 #congestive cardiac failure 
  b_admit = 0
  b_anycancer = 0
  b_varicosevein = 0
  b_copd = 0
  b_ibd = 0
  b_renal = 0
  
  b_antipsychotic = 0
  #women only
  b_tamoxifen = 0
  b_hrt = 0
  b_cop = 0
  
  surv = 5   #1-5
# VTE risk calculator 
risk_qthrombosis_venal_thromboembelism <- function(
  age = NULL,
  sex = NULL,
  town = NULL,
  
  bmi = 25,
  smoke_cat = 1,  #1-5
  
  b_CCF = 0, #congestive cardiac failure 
  b_admit = 0,
  b_anycancer = 0,
  b_varicosevein = 0,
  b_copd = 0,
  b_ibd = 0,
  b_renal = 0,
  
  b_antipsychotic = 0,
  #women only
  b_tamoxifen = 0,
  b_hrt = 0,
  b_cop = 0,
  
  surv = 5   #1-5
  
  ) {
  if (sex == 'Males') {
    # Survivor array (pre-calculated)
    survivor <- c( 0.9993539, 
                   0.9986778, 
                   0.9978991, 
                   0.9970629, 
                   0.9961508)
    # 1, 2, 3, 4, 5
    
    # Coefficients for smoking categories
    
    Ismoke <- c(#0, 
                0.07322798, 
                0.2090383, 
                0.2352668, 
                0.2891967)
    
    #never smoked, used to, light, medium, heavy
    
    # Fractional polynomial transforms
    dage <- age / 10
    age_1 <- dage ^ 3
    age_2 <- dage ^ 3 * log(dage)
    
    dbmi <- bmi / 10
    bmi_1 <- dbmi ^ (-2)
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
    score <-  (1 - (survivor[surv] ^ exp(a)))
    
  }
  else if (sex == 'Females') {
    # Survivor function (5-year survival rates at different time points)
    survivor <- c(
      0.999402642250061,
      0.998779714107513,
      0.998076260089874,
      0.997314155101776,
      0.996479928493500
    )
    
    # Coefficients for smoking categories
    # non
    # ex
    # light <10
    # moderate <19
    # heavey >20
    
    Ismoke <- c(
      #0,
      0.0899056072614921,
      0.2096026499560841,
      0.2698567860827918,
      0.37779267161809493
    )
    
    # Apply fractional polynomial transformations
    dage <- age / 10
    age_2 <- log(dage)
    age_1 <- dage ^ (-0.5)
    dbmi <- bmi / 10
    bmi_1 <- dbmi ^ (-2)
    bmi_2 <- dbmi ^ (-2) * log(dbmi)
    
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
    score <-  (1 - survivor[surv] ^ exp(a))  # surv is 0-indexed in C
    
  }
  
  return(score)
}

# Example usage of the function:
risk_qthrombosis_venal_thromboembelism(
  age = 40,
  sex = 'Males',
  town = 5

)



apply_vte_risk_wo_risk_factors <- function(input_population){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 90, 90, age),
      sex) 
  

  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(vte_risk =
             ifelse(age < 25, 0,
                    risk_qthrombosis_venal_thromboembelism(
                      age = age, 
                      sex = sex,
                      town = townsend_score
                    )
             )
    )
  
  input_population <- input_population |> 
    select(-any_of('vte_risk'))
  
  input_population <- left_join(input_population,postp[c('vte_risk','id')], by ='id')
  
  # initial_time_zero_population$stroke_risk <- postp$stroke_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  
  return(input_population)
}

# test_population |>
#   apply_vte_risk_wo_risk_factors() |> 
#   pull(vte_risk) |> 
#   hist()#count(age < 25, vte_risk ==0)
