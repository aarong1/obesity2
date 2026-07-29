
hypothyroid_incidence <- tribble(
  ~sex,~age_group,~flynn,~flynn_l,~flynn_u,~pred,~pred_l,~pred_u,
  "Males", "10-19" ,0.09, 0.03, 0.19, 0.47, 0.43, 0.52,
  "Males", "20-29" ,0.24, 0.14, 0.38, 0.91, 0.85, 0.98,
  "Males", "30-39" ,0.43, 0.30, 0.60, 0.94, 0.87, 1.01,
  "Males", "40-49" ,0.64, 0.48, 0.84, 1.09, 1.02, 1.17,
  "Males", "50-59" ,1.19, 0.95, 1.48, 1.50, 1.42, 1.59,
  "Males", "60-69" ,1.78, 1.46, 2.14, 1.96, 1.87, 2.07,
  "Males", "70-79" ,2.69, 2.20, 3.25, 1.97, 1.87, 2.07,
  "Females", "10-19" ,0.35, 0.22, 0.53, 1.01, 0.95, 1.09,
  "Females", "20-29" ,1.83, 1.53, 2.19, 1.95, 1.86, 2.06,
  "Females", "30-39" ,3.39, 3.01, 3.82, 2.03, 1.93, 2.14,
  "Females", "40-49" ,6.07, 5.55, 6.62, 2.34, 2.23, 2.46,
  "Females", "50-59" ,7.78, 7.14, 8.46, 2.35, 2.24, 2.47,
  "Females", "60-69" ,9.06, 8.37, 9.79, 2.56, 2.44, 2.69,
  "Females", "70-79" ,8.84, 8.08, 9.65, 2.87, 2.74, 3.01
)

hypothyroid_incidence <- rbind(
  hypothyroid_incidence,
  hypothyroid_incidence %>% 
    filter(age_group == "70-79") %>%
    mutate(age_group = '80-89'),
  hypothyroid_incidence %>% 
    filter(age_group == "70-79") %>%
    mutate(age_group = '90-110')
) %>% 
  select(sex ,    age_group ,hypothyroidism_year_risk = flynn) %>% 
  mutate(hypothyroidism_year_risk = hypothyroidism_year_risk/1000)



apply_hypothyroid_risk <- function(current_population){
  
  year = max(current_population$year)
  
  current_population <- current_population %>% 
    # filter(year == max(year,na.rm = TRUE)) %>% 
    select(-any_of('hypothyroidism_year_risk'))
  
  current_population <- current_population %>% 
    mutate(age_group = cut(age,breaks = c(-Inf,10,20,30,40,50,60,70,80,90, Inf),
                           labels =c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-110")
    )) 
  
  current_population <- current_population %>%
    left_join(hypothyroid_incidence,by = c( age_group = 'age_group','sex'))
  
  # current_population <- current_population %>% 
  #   mutate(
  #     hypothyroidism = rbinom(n = nrow(current_population),
  #                             size = 1,
  #                             prob = hypothyroidism_year_risk)*year
  #   ) 
  
  current_population <- current_population %>% 
    replace_na(list(hypothyroidism_year_risk = 0)) 
  

  current_population
}





