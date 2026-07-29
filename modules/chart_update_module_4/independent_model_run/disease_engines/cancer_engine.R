

all_cancer_incidence <- tibble::tribble(
  ~age_group, ~Total.number.of.cases, ~Average.number.of.cases.per.year, ~`Age-specific.incidence.rate.per.100,000.persons`, ~Total.number.of.male.cases, ~Average.number.of.male.cases.per.year, ~`per100k.Males`, ~Total.number.of.female.cases, ~Average.number.of.female.cases.per.year, ~`per100k.Females`,
           "0-4",                    129,                                26,                                               21.6,                          66,                                     13,                                             21.5,                            63,                                       13,                                               21.8,
           "5-9",                     70,                                14,                                               10.9,                          37,                                      7,                                             11.3,                            33,                                        7,                                               10.6,
         "10-14",                     68,                                14,                                               11.1,                          44,                                      9,                                             13.9,                            24,                                        5,                                                  8,
         "15-19",                    108,                                22,                                                 19,                          47,                                      9,                                             16.1,                            61,                                       12,                                               22.2,
         "20-24",                    205,                                41,                                               35.9,                         102,                                     20,                                             34.5,                           103,                                       21,                                               37.5,
         "25-29",                    400,                                80,                                               66.2,                         174,                                     35,                                             57.5,                           226,                                       45,                                               74.9,
         "30-34",                    747,                               149,                                              118.7,                         264,                                     53,                                             85.1,                           483,                                       97,                                              151.2,
         "35-39",                   1118,                               224,                                              179.9,                         387,                                     77,                                            128.1,                           731,                                      146,                                                229,
         "40-44",                   1517,                               303,                                                257,                         553,                                    111,                                            193.3,                           964,                                      193,                                              316.8,
         "45-49",                   2527,                               505,                                              403.2,                         968,                                    194,                                            316.2,                          1559,                                      312,                                              486.3,
         "50-54",                   4090,                               818,                                              622.1,                        1695,                                    339,                                            526.4,                          2395,                                      479,                                                714,
         "55-59",                   5590,                              1118,                                              898.6,                        2760,                                    552,                                            902.2,                          2830,                                      566,                                              895.1,
         "60-64",                   7160,                              1432,                                             1347.6,                        3924,                                    785,                                           1498.9,                          3236,                                      647,                                             1200.7,
         "65-69",                   8784,                              1757,                                             1940.2,                        5088,                                   1018,                                           2282.5,                          3696,                                      739,                                             1608.1,
         "70-74",                  10471,                              2094,                                               2573,                        6197,                                   1239,                                           3199.5,                          4274,                                      855,                                               2004,
         "75-79",                  10120,                              2024,                                             3266.5,                        5843,                                   1169,                                           4107.4,                          4277,                                      855,                                             2552.5,
         "80-84",                   8100,                              1620,                                             3816.4,                        4528,                                    906,                                           4987.3,                          3572,                                      714,                                             2941.2,
         "85-89",                   5253,                              1051,                                               4183,                        2721,                                    544,                                           5751.5,                          2532,                                      506,                                             3234.9,
         "90-110",                   2646,                               529,                                             3906.1,                        1126,                                    225,                                           5657.4,                          1520,                                      304,                                             3177.4
  )


all_cancer_incidence <- all_cancer_incidence %>% 
  select(age_group, per100k.Males, per100k.Females) %>% 
  pivot_longer(-1, names_sep = '\\.',names_to=c(NA, 'sex'), values_to = 'per100k') %>% 
  mutate(cancer_year_risk = per100k/100000) %>% 
  select(-per100k)
  
  
  apply_cancer_risk <- function(input_population){
    
    input_population <- input_population %>% 
      select(-any_of('cancer_year_risk'))
      
    input_population <- input_population %>% 
      mutate( age_group = cut(age, breaks = c(-Inf, 4,9,14,19,24,29,34,39,
                                              44,49,54,59,64,69,74,79,
                                              84,89, Inf),
                              
            labels = c("0-4",    "5-9",    "10-14",  "15-19",  "20-24",  "25-29",  "30-34",  "35-39", 
               "40-44",  "45-49",  "50-54",  "55-59",  "60-64",  "65-69",  "70-74",  "75-79", 
               "80-84",  "85-89",  "90-110")
            )
      )
    
    input_population <- input_population %>% 
      left_join(all_cancer_incidence,by = c('age_group', 'sex'))
    
    return(input_population)
    
  }
  
  # current_population %>% 
  #   apply_cancer_risk() %>% 
  #   select(cancer_year_risk) %>% 
  #   pull(cancer_year_risk) %>% 
  #   hist()





