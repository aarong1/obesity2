library(data.table)

apply_cervical_cancer_risk_wo_risk_factors <- function(input_population) {
  
  # 1. Define incidence rates using the fread "visual table" approach
  # This replaces the bulky tribble call
cervical_age_sex_incidence <- fread("
age, sex, per100k
0-29, Females, 1.5 
30-34, Females, 19.400000000000002 
35-39, Females, 18.6 
40-44, Females, 18.1 
45-49, Females, 15.600000000000001 
50-54, Females, 10.8 
55-59, Females, 13.9 
60-64, Females, 9.600000000000001 
65-69, Females, 6.4 
70-74, Females, 7.5 
75-79, Females, 6.9 
80-84, Females, 4.800000000000001 
85-110, Females, 7.800000000000001 
  ")
  
  # Calculate annual risk decimal
cervical_age_sex_incidence[, cervical_cancer_year_risk := as.numeric(per100k) / 100000]
cervical_age_sex_incidence[, per100k := NULL]
  
  if( 'cervical_cancer_year_risk' %in% names(input_population)){
    input_population[, cervical_cancer_year_risk := NULL] 
  }
  
  # 2. Ensure input is a data.table (in-place)
  if (!is.data.table(input_population)) setDT(input_population)
  
  # 3. Create join key and perform update-on-join
  # Using the 'labels' argument in cut to match the table keys exactly
  input_population[, age_join := as.character(cut(age,
                                                  breaks = c(-Inf, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                                                  labels = c("0-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                             "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                                             "85-89", "90-110")))]
  
  # Update-on-join (assigns the new column without a full copy)
  input_population[
    cervical_age_sex_incidence, 
    on = .(age_join=age, sex), 
    cervical_cancer_year_risk := i.cervical_cancer_year_risk
  ]
  
  input_population[,age_join := NULL]
  
  input_population[is.na(cervical_cancer_year_risk), cervical_cancer_year_risk := 0]
  
  return(input_population)
}

# initial_time_zero_population = read.fst('./main/initial_time_zero_population10down.fst')

# input_population <-  initial_time_zero_population #%>%
# initial_time_zero_population %>%
#   apply_cervical_cancer_risk_wo_risk_factors()  %>%
#     count(cervical_cancer_year_risk)
