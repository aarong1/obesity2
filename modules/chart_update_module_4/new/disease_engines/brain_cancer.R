library(data.table)

apply_brain_cancer_risk_wo_risk_factors <- function(input_population) {
  
  # 1. Define incidence rates using the fread "visual table" approach
  # This replaces the bulky tribble call
  brain_age_sex_incidence <- fread("
    age_join,   sex,      per100k
    0-29,     Males,    3.4
    0-29,     Females,  2.1
    30-34,    Males,    5.2
    30-34,    Females,  1.9
    35-39,    Males,    5.8
    35-39,    Females,  3.7
    40-44,    Males,    4.1
    40-44,    Females,  1.6
    45-49,    Males,    8.2
    45-49,    Females,  3.8
    50-54,    Males,    9.0
    50-54,    Females,  3.0
    55-59,    Males,    11.8
    55-59,    Females,  11.8
    60-64,    Males,    14.7
    60-64,    Females,  10.7
    65-69,    Males,    27.5
    65-69,    Females,  16.2
    70-74,    Males,    27.8
    70-74,    Females,  18.7
    75-79,    Males,    48.1
    75-79,    Females,  22.9
    80-84,    Males,    41.5
    80-84,    Females,  21.8
    85-89,    Males,    30.6
    85-89,    Females,  29.1
    90-110,      Males,    29.9
    90-110,      Females,  10.3
  ")
  
  # Calculate annual risk decimal
  brain_age_sex_incidence[, brain_cancer_year_risk := per100k / 100000]
  brain_age_sex_incidence[, per100k := NULL]
  
  # 2. Ensure input is a data.table (in-place)
  if (!is.data.table(input_population)) setDT(input_population)
  
  if( 'brain_cancer_year_risk' %in% names(input_population)){
    input_population[, brain_cancer_year_risk := NULL] 
  }
  
  
  # 3. Create join key and perform update-on-join
  # Using the 'labels' argument in cut to match the table keys exactly
  input_population[, age_join := as.character(cut(age,
                                     breaks = c(-Inf, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                                     labels = c("0-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                                                "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", 
                                                "85-89", "90-110")))]
  
  # Update-on-join (assigns the new column without a full copy)
  input_population[
    brain_age_sex_incidence, 
    on = .(age_join, sex), 
    brain_cancer_year_risk := i.brain_cancer_year_risk
  ]
  
  input_population[,age_join := NULL]
  
  input_population[is.na(brain_cancer_year_risk), brain_cancer_year_risk := 0]
  
  return(input_population)
}

# initial_time_zero_population = read.fst('./main/initial_time_zero_population10down.fst')

# input_population <-  initial_time_zero_population #%>%
# initial_time_zero_population %>% 
#   apply_brain_cancer_risk_wo_risk_factors()  %>% 
#     count(brain_cancer_year_risk)
