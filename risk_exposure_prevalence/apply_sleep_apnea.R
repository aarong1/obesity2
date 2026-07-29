
# https://publications.ersnet.org/content/erj/47/4/1162
#The effect of sex and age on the comorbidity burden of OSA: an observational analysis from a large nationwide US health claims database
sleep_prevalence <- 
tibble::tribble(
  ~sex, ~age, ~count, ~percentage, ~Diagnosed.with.OSA, ~Matched.random.sample,
 "Males",  "30–39",   "137109", 8.04,   "137109 (8.04)",    "274218 (8.04)",
 "Males",  "40–49",  "237421", 13.93,  "237421 (13.93)",   "474842 (13.93)",
 "Males",  "50–59",  "276659", 16.23,  "276659 (16.23)",   "553318 (16.23)",
 "Males",  "60–69",   "141296", 8.29,   "141296 (8.29)",    "282592 (8.29)",
 "Males",  "70–79",    "46400", 2.72,    "46400 (2.72)",     "92800 (2.72)",
 "Males",   "80-110",    "16556", 0.97,    "16436 (0.96)",     "32992 (0.97)",
"Females",  "30–39",   "135896", 7.97,   "135896 (7.97)",    "271792 (7.97)",
"Females",  "40–49",  "232535", 13.64,  "232535 (13.64)",   "465070 (13.64)",
"Females",  "50–59",  "283513", 16.63,  "283513 (16.63)",   "567026 (16.63)",
"Females",  "60–69",   "126532", 7.42,   "126532 (7.42)",    "253064 (7.42)",
"Females",  "70–79",    "46682", 2.74,    "46682 (2.74)",     "93364 (2.74)",
"Females",    "80-110",    "24306", 1.43,    "23938 (1.40)",     "48244 (1.42)"
  )   


total = 1704905 

Men_count = 	855441 
men_pc = 50.18

Women_count =	849464 
women_pc = 49.82

sleep_prevalence <- sleep_prevalence |> 
  mutate(probability = percentage / 100) |> 
  select(probability, sex, age) |> 
  mutate(sleep = 'sleep_apnea')
  

apply_sleep_lifestyle_parameter_rank_stability <- function(current_population, lookup_dz_raster_cell){
  
  current_population <- current_population |> 
    select(-any_of(c('sleep')))

  current_population <- current_population |> 
    mutate(age_risk = cut(age,c(-Inf,30,40,50,60,70,80,110,Inf),
                          labels = c("0-30",
                          "0–29",
                          "30–39",
                          "40–49",
                          "50–59",
                          "60–69",
                          "70–79",
                          "80-110")))

  current_population <- current_population |> 
    left_join(sleep_prevalence,
              relationship = 'many-to-one',
              multiple = 'first',
              by = join_by('sex', 
                           age_risk==age,  
                           sleep_percentile<probability)) #|> View()
  
  
  # fill in the rest
  
  current_population <- current_population |> 
    replace_na(list(sleep = 'no_sleep_apnea'))
  
  current_population <- current_population |> 
    select(-any_of(c('probability','percentage')))
    
  
}


# current_population$sleep_percentile
# 
# count(current_population,sleep)

