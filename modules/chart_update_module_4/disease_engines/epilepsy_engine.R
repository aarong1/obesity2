epilepsy_incidence_age <- tibble::tribble(
  ~country, ~source, ~age, ~`person_years`, ~incident_cases, ~`per100k`,      ~`95ci`, ~population, ~pop_cases,
  "N Ireland",  "GOLD",      "0-4",     62518,    43L,     68.78,  "49.78-92.65",   120369,    83L,
  "N Ireland",  "GOLD",      "5-9",     95406,    48L,     50.31,  "37.10-66.71",   128546,    65L,
  "N Ireland",  "GOLD",    "10-14",     92486,    34L,     36.76,  "25.46-51.37",   124127,    46L,
  "N Ireland",  "GOLD",    "15-19",     92368,    65L,     70.37,  "54.31-89.69",   112397,    79L,
  "N Ireland",  "GOLD",    "20-24",     87942,    39L,     44.35,  "31.54-60.62",   115359,    51L,
  "N Ireland",  "GOLD",    "25-29",     98505,    49L,     49.74,  "36.80-65.76",   122325,    61L,
  "N Ireland",  "GOLD",    "30-34",   105046,    27L,       25.7,  "16.94-37.40",   126761,    33L,
  "N Ireland",  "GOLD",    "35-39",   103063,    36L,     34.93,  "24.46-48.36",  124583,   44L,
  "N Ireland",  "GOLD",    "40-44",   104216,    44L,     42.22,  "30.68-56.68",  116254,   49L,
  "N Ireland",  "GOLD",    "45-49",   111387,    32L,     28.73,  "19.65-40.56",  125780,   36L,
  "N Ireland",  "GOLD",    "50-54",   113016,    33L,       29.2,  "20.10-41.01",   131984,    39L,
  "N Ireland",  "GOLD",    "55-59",   101765,    36L,     35.38,  "24.78-48.97",  124654,   44L,
  "N Ireland",  "GOLD",    "60-64",     84643,    48L,     56.71,  "41.81-75.19",   105804,    60L,
  "N Ireland",  "GOLD",    "65-69",     75009,    35L,     46.66,  "32.50-64.89",   89873,    42L,
  "N Ireland",  "GOLD",    "70-74",     63220,    24L,     37.96,  "24.32-56.49",   81399,    31L,
  "N Ireland",  "GOLD",    "75-79",     47366,    40L,     84.45, "60.33-115.00",   61874,    52L,
  "N Ireland",  "GOLD",    "80-84",     34184,    28L,     81.91, "54.43-118.38",   42839,    35L,
  "N Ireland",  "GOLD",    "85-89",     21133,    18L,     85.17, "50.48-134.61",   25005,    21L,
  "N Ireland",  "GOLD",    "90-94",     9741,     5L,     51.33, "16.67-119.79",    13734,    7L,
) %>% 
  mutate(epilepsy_year_risk = per100k/100000) %>% 
  select(c('age','epilepsy_year_risk'))


apply_epilepsy_risk <- function(input_population, intervention = 1){
  # input_population = current_population
  year = max(input_population$year)
  
  input_population <- input_population |> 
    select(-any_of('epilepsy_year_risk'))
  
  input_population <- input_population |> 
    mutate(age_group =
           cut(age, 
               breaks = c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,Inf),
               labels = c( "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                           "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-110"
                 
               )
           )
  )

  input_population <- left_join(input_population,
                                epilepsy_incidence_age, 
                                by = c('age_group' = 'age') )
  
  # input_population <- input_population %>% 
  #   mutate(
  #     epilepsy = rbinom(n = nrow(input_population),
  #                           size = 1,
  #                           prob = epilepsy_year_risk)*year
  #   )
  
  # input_population <- input_population %>% 
  #   mutate(
  #     epilepsy = (runif(n())<epilepsy_year_risk)*year
  #   )
  
  input_population <- input_population %>% 
    replace_na(list(epilepsy_year_risk = 0)) 

  input_population <- ungroup(input_population)
  
  return(input_population)
}

# rbinom(n = nrow(input_population),
#        size = 1,
#        prob = input_population$epilepsy_year_risk)*year
# 
# current_population %>% apply_epilepsy_risk() %>% count(is.na(epilepsy_year_risk))
