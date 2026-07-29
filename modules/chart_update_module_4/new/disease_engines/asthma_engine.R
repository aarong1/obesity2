asthma_incidence <- 
  tribble(
    ~'age', ~'per100k',
    '0-5', 929.0,
    '6-10', 561.0,
    '11-15', 356.0,
    '16-20', 170.0,
    '21-30', 150.0,
    '31-40', 180.0,
    '41-50', 201.0,
    '51-60', 204.0,
    '61-70', 231.0,
    '71-80', 194.0,
    '81-110', 111.0
  ) %>% 
   mutate(asthma_prob = per100k/100000, age_group = age) %>% 
   select(age_group, asthma_prob)

# asthma 
# bmi per 5kg/m2 males and females 
# Obese/ Overweight	
# Males/ Females	
# 1-4 yo /5-9yo /10-14yo / 15-19 yo  
1.3
# https://pmc.ncbi.nlm.nih.gov/articles/PMC1899288/#sec6


# GBD Realtive Risks ----
# Asthma	
# 5 'kg/m2'
# Males
# 1.409
# "(1.29 to 1.545)"	
# 
# Asthma	
# 5 'kg/m2'
# Females
# "1.402"
# (1.275 to 1.532)"

   
# and 
# smoking 
# active 10cig a day
1.73

# ex 10years since quit
 50
# %
 ( 1.73 - 1 ) * 0.5 + 1

# https://www.sciencedirect.com/science/article/pii/S2590332224004871?utm_source=chatgpt.com
#  Adult asthma risk RR ≈ 1.071 (≈ +7.1%)  ￼
# Childhood asthma risk RR ≈ 1.214 (≈ +21.4%)  ￼

# initial_time_zero_population <- initial_time_zero_population |> 
#   slice_sample(prop = 0.1) 

 x <- initial_time_zero_population |> 
   slice_sample(prop = 0.1) |> 
    mutate(age_group =
              cut(age, 
                  breaks = c(-Inf,5,10,15,20,30,40,50,60,70,80,110),
                  labels = c(
                     '0-5', '6-10', '11-15', '16-20', '21-30', '31-40', 
                     '41-50', '51-60', '61-70', '71-80', '81-110'
                  )
              )
    ) |> #select(age, age_group)
    mutate(RR_smoke = case_when(
       smoking == 'current_smoker' ~ 1.73,
       smoking == 'former_regular' ~ 1.365,
       smoking == 'former' ~ 1.365,
       
       smoking == 'former_irregular' ~ 1.365,
       smoking == 'never_smoked' ~ 1,
       TRUE ~ 1
    ) ,
    RR_bmi = case_when(
       bmi == 'obese' ~ 1.792,
       bmi == 'overweight' ~ 1.38,
       bmi == 'normal' ~ 1,
       TRUE ~ 1
    )
    )
 
asthma_theoretical_minimum <- x  |> 
   group_by(age_group,sex) |> 
   summarise(AF = 1-n()/sum(RR_smoke*RR_bmi)) |>
   # mutate( (1-AF) ) |>
   left_join(
      asthma_incidence
   ) |>
   mutate(asthma_prob_min = asthma_prob * (1-AF) ) #|> 

# current_population <- base_population_w_risk_factors
# base_population_w_risk_factors %>% 
#    apply_asthma_risk() %>% 
#    pull(asthma_year_risk) 

apply_asthma_risk <- function(current_population,intervention = 1){
   
current_population <- current_population %>% 
   mutate(age_group =
             cut(age, 
                 breaks = c(-Inf,5,10,15,20,30,40,50,60,70,80,110),
                 labels = c(
                    '0-5', '6-10', '11-15', '16-20', '21-30', '31-40', 
                    '41-50', '51-60', '61-70', '71-80', '81-110'
                 )
             )
   ) |> #select(age, age_group)
   mutate(RR_smoke = case_when(
      smoking == 'current_smoker' ~ 1.73,
      smoking == 'former_regular' ~ 1.365,
      smoking == 'former' ~ 1.365,
      smoking == 'former_irregular' ~ 1.365,
      smoking == 'never_smoked' ~ 1,
      TRUE ~ 1
   ) ,
   RR_bmi = case_when(
      bmi == 'obese' ~ 1.792,
      bmi == 'overweight' ~ 1.38,
      smoking == 'normal' ~ 1,
      TRUE ~ 1
   )
   ) %>% 
   left_join(asthma_theoretical_minimum[c('age_group','sex','asthma_prob_min')]) %>% 
   mutate(asthma_year_risk = asthma_prob_min * RR_smoke*RR_bmi) #|> 

current_population <- current_population %>% select(-c(asthma_prob_min,RR_smoke,RR_bmi))
   
}



library(data.table)

apply_asthma_risk_dt <- function(current_population, intervention = 1) {
   # Work on a data.table
   current_population <- as.data.table(current_population)
   asthma_tm <- as.data.table(asthma_theoretical_minimum)[
      , .(age_group, sex, asthma_prob_min)
   ]
   
   # Age groups
   current_population[, age_group := cut(
      age,
      breaks = c(-Inf, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, 110),
      labels = c(
         "0-5", "6-10", "11-15", "16-20", "21-30", "31-40",
         "41-50", "51-60", "61-70", "71-80", "81-110"
      )
   )]
   
   # Smoking RR
   current_population[, RR_smoke := fcase(
      smoking == "current_smoker",    1.73,
      smoking == "former_regular",    1.365,
      smoking == "former_irregular",  1.365,
      smoking == "never_smoked",      1,
      default = 1
   )]
   
   # BMI RR  (fixed: bmi == "normal", not smoking == "normal")
   current_population[, RR_bmi := fcase(
      bmi == "obese",       1.792,
      bmi == "overweight",  1.38,
      bmi == "normal",      1,
      default = 1
   )]
   
   # Join on age_group + sex to get asthma_prob_min
   current_population[
      asthma_tm,
      on = .(age_group, sex),
      asthma_prob_min := i.asthma_prob_min
   ]
   
   # Final risk
   current_population[
      , asthma_year_risk := asthma_prob_min * RR_smoke * RR_bmi
   ]           
   
   # Drop intermediate columns
   current_population[
      , c("asthma_prob_min", "RR_smoke", "RR_bmi") := NULL
   ]
   
   return(current_population)
}



