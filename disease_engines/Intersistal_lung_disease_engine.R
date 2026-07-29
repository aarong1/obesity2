ild_incidence <- tribble(
  ~ILD, ~age_group, ~per100k,
  'CTD-ILDs', "0-17", 0, 
  'CTD-ILDs', "18-54", 1.11,
  'CTD-ILDs', "55-59", 7.09,
  'CTD-ILDs', "60-64", 12.70,
  'CTD-ILDs', "65-69", 19.45,
  'CTD-ILDs', "70-74", 30.58,
  'CTD-ILDs', "75-79", 50.66,
  'CTD-ILDs', "80-84", 68.12,
  'CTD-ILDs', "85-110", 82.71,
  'Hypersensitivity pneumonitis', "0-18", 1.15, 
  'Hypersensitivity pneumonitis', "18-54", 1.15, 
  'Hypersensitivity pneumonitis', "55-59", 4.13, 
  'Hypersensitivity pneumonitis', "60-64", 4.81, 
  'Hypersensitivity pneumonitis', "65-69", 5.55, 
  'Hypersensitivity pneumonitis', "70-74", 7.80, 
  'Hypersensitivity pneumonitis', "75-79", 9.29, 
  'Hypersensitivity pneumonitis', "80-84", 10.51, 
  'Hypersensitivity pneumonitis', "85-110", 10.66, 
  "IPF-CS", "0-18", 0, 
  "IPF-CS", "18-54", 1.33,
  "IPF-CS", "55-59", 10.25,
  "IPF-CS", "60-64", 19.91,
  "IPF-CS", "65-69", 34.56,
  "IPF-CS", "70-74", 66.60,
  "IPF-CS", "75-79", 125.70,
  "IPF-CS", "80-84", 212.91,
  "IPF-CS", "85-110", 334.20
) %>% filter(ILD == 'CTD-ILDs') %>% 
  mutate(ild_year_risk = per100k/100000) %>% 
  select(age_group, ild_year_risk)


apply_ILD_risk <- function(input_population){
  
  input_population <- input_population %>% 
    mutate(age_join  = cut(age,
                           breaks = c( -Inf, 17, 54, 59, 64, 69, 74, 79, 84, Inf),
                           labels = c("0-17", "18-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-110")
  )) %>% 
    select(-any_of('ild_year_risk')) %>%
    left_join(ild_incidence, by = c( 'age_join' = 'age_group'))
  
  
  input_population <- input_population %>% 
    replace_na(list(ild_year_risk = 0)) 
}

