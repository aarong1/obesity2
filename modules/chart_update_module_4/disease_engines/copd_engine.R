# COPD 
# source('./new_files/copd.R')
copd_incidence_per100k <- 
  tribble(
    ~'age_group', ~'Males', ~'Females',
    '0-4', 1.2, 0.0,
    '5-9', 2.6, 3.4,
    '10-14', 2.1, 1.6,
    '15-19', 0.0, 0.0,
    '20-24', 1.8, 1.3,
    '25-29', 3.4, 3.4,
    '30-34', 7.9, 9.8,
    '35-39', 25.0, 28.2,
    '40-44', 52.0, 60.4,
    '45-49', 106.0, 121.2,
    '50-54', 205.2, 174.5,
    '55-59', 298.0, 235.4,
    '60-64', 491.9, 326.9,
    '65-69', 437.5, 216.8,
    '70-74', 334.8, 228.0,
    '75-79', 263.4, 0.0,
    '80-84', 0.0, 0.0,
    '85-110', 0.0, 0.0)

copd_incidence_per100k <- copd_incidence_per100k |> 
  pivot_longer(cols = 2:3, names_to = 'sex') |> 
  mutate(copd_prob = value/100e3)
# smokes
# SAPM
4.01

# GBD
# 10 pack years 
# every demographic
3.56

# ambient pm25
# 5micro-grams per metrer-cubed
1.06

# 10 years since quitting
# %
38 
( 3.56 - 1 ) * 0.38 + 1
# 1.9728

x <- initial_time_zero_population |> 
  slice_sample(prop = 0.1)

copd_theoretical_minimum <- x |> 
  mutate(age_group=
           cut(age, 
               include.lowest = T,
               right = F,
               breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,110),
               labels = c(
                 "0-4",    "5-9",    "10-14",  "15-19" ,
                 "20-24",  "25-29",  "30-34",  "35-39" ,
                 "40-44",  "45-49",  "50-54",  "55-59" ,
                 "60-64",  "65-69",  "70-74",  "75-79" ,
                 "80-84",  "85-110"
               )
           )
  ) |> 
  mutate(RR_smoke = case_when(
    smoking == 'current_smoker' ~ 3.5,
    smoking == 'former_regular' ~ 1.9,
    smoking == 'former_irregular' ~ 1.9,
    smoking == 'never_smoked' ~ 1,
    TRUE ~ 1
    ) 
    ) |> 
  group_by(age_group,sex) |> 
  summarise(AF = 1-n()/sum(RR_smoke)) |>
  # mutate( (1-AF) ) |>
  left_join(
  copd_incidence_per100k[c('age_group','sex','copd_prob')]) |>
  mutate(copd_prob_min = copd_prob * (1-AF) ) 

# current_population <- base_population_w_risk_factors



apply_copd_risk <- function(current_population,intervention = 1){
  
  current_population <- current_population %>% 
    mutate(age_group=
             cut(age, 
                 include.lowest = T,
                 right = F,
                 breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,110),
                 labels = c(
                   "0-4",    "5-9",    "10-14",  "15-19" ,
                   "20-24",  "25-29",  "30-34",  "35-39" ,
                   "40-44",  "45-49",  "50-54",  "55-59" ,
                   "60-64",  "65-69",  "70-74",  "75-79" ,
                   "80-84",  "85-110"
                 )
             )
    ) |> #select(age, age_group)
    mutate(RR_smoke = case_when(
      smoking == 'current_smoker' ~ 3.5,
      smoking == 'former_regular' ~ 1.9,
      smoking == 'former' ~ 1.9,
      
      smoking == 'former_irregular' ~ 1.9,
      smoking == 'never_smoked' ~ 1,
      TRUE ~ 1
    ) )  %>% 
    left_join(copd_theoretical_minimum[c('age_group','sex','copd_prob_min')]) %>% 
    mutate(copd_year_risk = copd_prob_min * RR_smoke) #|> 
  
  current_population <- current_population %>% select(-c(copd_prob_min,RR_smoke))
}

# base_population_w_risk_factors %>% 
#   apply_copd_risk() %>% pull(copd_year_risk)
# 
# current_population$copd_year_risk
# current_population$copd_prob_min

# current_population <- base_population_w_risk_factors

library(data.table)

apply_copd_risk_dt <- function(current_population, intervention = 1) {
  # make sure it's a data.table
  current_population <- as.data.table(current_population)
  copd_theoretical_minimum <- as.data.table(copd_theoretical_minimum)
  
  # age group
  current_population[, age_group :=
                       cut(
                         age,
                         include.lowest = TRUE,
                         right = FALSE,
                         breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,110),
                         labels = c(
                           "0-4",    "5-9",    "10-14",  "15-19",
                           "20-24",  "25-29",  "30-34",  "35-39",
                           "40-44",  "45-49",  "50-54",  "55-59",
                           "60-64",  "65-69",  "70-74",  "75-79",
                           "80-84",  "85-110"
                         )
                       )
  ]
  
  # RR_smoke (case_when equivalent)
  current_population[, RR_smoke :=
                       fcase(
                         smoking == "current_smoker",             3.5,
                         smoking %in% c("former_regular",
                                        "former_irregular",
                                        "former"),      1.9,
                         smoking == "never_smoked",               1,
                         default = 1
                       )
  ]
  
  # inner_join on age_group + sex and bring in copd_prob_min
  current_population <- current_population[
    copd_theoretical_minimum[, .(age_group, sex, copd_prob_min)],
    on = .(age_group, sex),
    nomatch = 0L
  ]
  
  # yearly risk
  current_population[, copd_year_risk := copd_prob_min * RR_smoke]
  
  # drop temporary columns
  current_population[, c("copd_prob_min", "RR_smoke") := NULL]
  
  current_population
}


# base_population_w_risk_factors %>%
#   apply_copd_risk_dt() %>% 
#   pull(copd_year_risk)
# 
# current_population$copd_year_risk
# current_population$copd_prob_min
