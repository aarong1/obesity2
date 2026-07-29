# Age (years)	Incidence rate (95% CI) per 10 000 person-years 
# 1994	2004	2014 
# Overall 
# <20	—	0.21 (0.11, 0.41)	0.08 (0.03, 0.25) 
# ≥20 and <30	0.79 (0.45, 1.39)	1.09 (0.83, 1.44)	0.81 (0.57, 1.16) 
# ≥30 and <40	2.47 (1.85, 3.30)	2.18 (1.82, 2.61)	2.09 (1.70, 2.57) 
# ≥40 and <50	3.82 (3.02, 4.83)	3.73 (3.29, 4.22)	3.48 (3.00, 4.03) 
# ≥50 and <60	6.60 (5.41, 8.04)	6.98 (6.30, 7.75)	5.95 (5.30, 6.67) 
# ≥60 and <70	7.35 (5.99, 9.00)	10.44 (9.55, 11.42)	7.41 (6.62, 8.29) 
# ≥70 and <80	10.27 (8.47, 12.45)	12.08 (10.86, 13.44)	10.27 (9.15, 11.53) 
# ≥80 and <90	7.39 (5.36, 10.20)	11.79 (10.30, 13.52)	6.97 (5.76, 8.44) 
# ≥90	10.81 (5.62, 20.77)	7.13 (4.55, 11.17)	3.41 (1.94, 6.01) 

#2014 

# Rheumatoid arthritis is getting less frequent—results of a nationwide population-based cohort study
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5850292/#sup1
  
rheumatoid_arthritis_incidence <- tibble::tribble(
  ~sex,  ~age_group, ~rheumatoid_arthritis_year_risk,
'Females', "0-19", 0.11,
'Females', "20-29", 1.26,
'Females', "30-39", 3.34,
'Females', "40-49", 4.99,
'Females', "50-59", 8.32,
'Females', "60-69", 8.96,
'Females', "70-79", 12.20,
'Females', "80-89", 8.50,
'Females', "90-110", 3.24,
'Males', "0-19", 0.05,
'Males', "20-29", 0.40,
'Males', "30-39", 0.84,
'Males', "40-49", 1.99,
'Males', "50-59", 3.63,
'Males', "60-69", 5.82,
'Males', "70-79", 8.11,
'Males', "80-89", 4.81,
'Males', "90-110", 3.82) %>% 
  mutate(rheumatoid_arthritis_year_risk = rheumatoid_arthritis_year_risk/10000)

# current_population = initial_time_zero_population

apply_rheumatoid_arthritis_risk <- function(current_population, rheumatoid_arthritis_incidence){
  
  year = min(current_population$year)
  
  current_population <- current_population %>% 
    select(-any_of('rheumatoid_arthritis_year_risk'))
  
 
  current_population <- current_population %>% 
    mutate(age_group = cut(age,breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
                           labels =c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-110")
    )) %>% #pull(age_group) %>% unique()
    left_join(rheumatoid_arthritis_incidence)
   
   # current_population <- current_population %>% 
   #   mutate(
   #     rheumatoid_arthritis = rbinom(n = nrow(current_population),
   #                             size = 1,
   #                             prob = rheumatoid_arthritis_year_risk)*year
   #   ) 
   current_population <- current_population %>% 
     replace_na(list(rheumatoid_arthritis_year_risk = 0)) 
   
}


