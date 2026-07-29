# per 100000 population per year 
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4687424/
# Age‐specific incidence, risk factors and outcome of acute abdominal aortic aneurysms in a defined population

aaa_incidence <- tribble(
  ~age_aaa, ~Females, ~Males,
  '0-44', 0, 0,
  '45–54', 0, 0,
  '55–64', 8, 3,
  '65–74', 55, 11,
  '75–84', 112, 31,
  '85-110', 298, 82
) %>% 
  pivot_longer(-1,names_to = 'sex', values_to ='per100k') %>% 
  mutate(aaa_year_risk = per100k/100000) 

apply_aaa_risk <- function(current_population, aaa_incidence){
  
  year = max(current_population$year)
  current_population <- current_population %>% 
    filter(year == min(year)) %>% 
    mutate(age_aaa = cut(age,breaks = c(-Inf, 44,54,64,74,84, Inf),
                                 labels =c('0-44','45–54','55–64','65–74','75–84','85-110')
    )) %>%
    select(-any_of('aaa_year_risk'))
  
  current_population %>% left_join(aaa_incidence)
  
  current_population %>% select(-age_aaa)
  
  current_population <- current_population %>% 
    mutate(
      aaa = rbinom(n = nrow(current_population),
                              size = 1,
                              prob = aaa_year_risk)*year
    ) 
  current_population <- current_population %>% 
    replace_na(list(aaa_year_risk = 0)) 
}
