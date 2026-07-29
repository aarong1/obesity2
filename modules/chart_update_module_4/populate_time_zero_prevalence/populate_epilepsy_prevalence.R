
epilepsy_prevalence_age <- tibble::tribble(
  ~country,	~source,	~age,	~denominator,	~cases,	~ per1k, ~pop2019, ~prevalent_cases,
  "Northern Ireland", "GOLD",   "0-4",     83929,    149,    1.78,     120369,    214,
  "Northern Ireland", "GOLD",   "5-9",     98404,    483,    4.91,     128546,    631,
  "Northern Ireland", "GOLD", "10-14",     94485,    592,    6.27,     124127,    778,
  "Northern Ireland", "GOLD", "15-19",     94421,    885,    9.37,     112397,   1053,
  "Northern Ireland", "GOLD", "20-24",     92942,   1192,   12.83,     115359,   1480,
  "Northern Ireland", "GOLD", "25-29",    104936,   1370,   13.06,     122325,   1597,
  "Northern Ireland", "GOLD", "30-34",    111178,   1548,   13.92,     126761,   1765,
  "Northern Ireland", "GOLD", "35-39",    107491,   1544,   14.36,     124583,   1790,
  "Northern Ireland", "GOLD", "40-44",    107360,   1595,   14.86,     116254,   1727,
  "Northern Ireland", "GOLD", "45-49",    113939,   1611,   14.14,     125780,   1778,
  "Northern Ireland", "GOLD", "50-54",    115192,   1672,   14.51,     131984,   1916,
  "Northern Ireland", "GOLD", "55-59",    103510,   1515,   14.64,     124654,   1824,
  "Northern Ireland", "GOLD", "60-64",     86094,   1325,   15.39,     105804,   1628,
  "Northern Ireland", "GOLD", "65-69",     76165,   1227,   16.11,      89873,   1448,
  "Northern Ireland", "GOLD", "70-74",     64155,   1005,   15.67,      81399,   1275,
  "Northern Ireland", "GOLD", "75-79",     48097,    732,   15.22,      61874,    942,
  "Northern Ireland", "GOLD", "80-84",     34899,    495,   14.18,      42839,    608,
  "Northern Ireland", "GOLD", "85-89",     21823,    289,   13.24,      25005,    331,
  "Northern Ireland", "GOLD", "90-110",     11375,    154,   13.54,      13734,    186,
  
)

epilepsy_prevalence <- epilepsy_prevalence_age %>%
  summarise( denominator = sum(denominator),
             cases = sum(cases)) %>% 
  mutate(prob = cases/denominator)

epilepsy_prevalence_age <- epilepsy_prevalence_age%>% 
  mutate(prob = per1k/1000) %>% 
  select(prob, age)

populate_epilepsy_prevalence <- function(input_population){
  if(!'epilepsy_year_risk'%in%names(input_population)){
    stop('epilepsy_year_risk column must be present')}
  
  year = max(input_population$year)
  
  input_population$epilepsy_percentile <- 
    rank(input_population$epilepsy_year_risk,ties.method = 'random')/
    max(rank(input_population$epilepsy_year_risk,ties.method = 'random'))
  
  input_population <- input_population %>% 
    mutate(epilepsy = ifelse(epilepsy_percentile < epilepsy_prevalence$prob, year, 0))
  
  input_population <- ungroup(input_population)
  
  return(input_population)
  
}

# x <- initial_time_zero_population %>% 
#   populate_epilepsy_prevalence(epilepsy_prevalence)
# 
# count(x,epilepsy)
