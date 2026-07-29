# https://pmc.ncbi.nlm.nih.gov/articles/PMC10382342/
# Trends in incidence of recorded diagnosis of osteoporosis, osteopenia, and fragility fractures in people aged 50 years and above: retrospective cohort 
# study using UK primary care data

  #per10k
  osteoporosis_incidence <- tibble::tribble(
    ~`age`, ~`Males_per10k`,~`Males–per10k-(95%)`, ~`Females_per10k`,~`Females–per10k-(95%)`, ~`Men.–.Adjusted*.IRR.(95%CI)`, ~`Women.–.Adjusted*.IRR.(95%CI)`,
        # "All ages",                   "15.28 (15.06–15.51)",                     "79.82 (79.32–80.31)",                     "1 (Ref.)",               "4.92 (4.84–5.00)",
           "50–54",                      "4.50",  "(4.24–4.77)",                     "22.99",  "(22.38–23.61)",                     "1 (Ref.)",                       "1 (Ref.)",
           "55–59",                      "6.64",  "(6.30–6.98)",                     "42.44",  "(41.58–43.32)",             "1.49 (1.38–1.61)",               "1.85 (1.79–1.91)",
           "60–64",                    "10.10",  "(9.66–10.55)",                     "63.50",  "(62.38–64.63)",             "2.26 (2.10–2.43)",               "2.77 (2.68–2.86)",
           "65–69",                   "14.68",  "(14.11–15.27)",                     "87.27",  "(85.86–88.70)",             "3.26 (3.03–3.50)",               "3.81 (3.69–3.93)",
           "70–74",                   "21.19",  "(20.42–21.98)",                   "113.32", "(111.56–115.11)",             "4.75 (4.43–5.09)",               "4.96 (4.81–5.12)",
           "75–79",                   "30.84",  "(29.77–31.94)",                  "143.20",  "(141.01–145.41)",             "6.93 (6.47–7.42)",               "6.28 (6.09–6.48)",
           "80–84",                   "40.84",  "(39.33–42.40)",                  "153.20",  "(150.61–155.83)",             "9.14 (8.51–9.80)",               "6.73 (6.52–6.95)",
           "85–89",                   "49.51",  "(47.15–51.97)",                  "153.39",  "(150.11–156.72)",          "11.02 (10.21–11.90)",               "6.75 (6.52–6.99)",
           "90–110",                   "50.47",  "(46.66–54.52)",                  "118.62",  "(114.81–122.53)",          "11.10 (10.07–12.24)",               "5.22 (5.00–5.44)"
    ) %>% 
    pivot_longer(cols = c(2,4),names_sep = '_',names_to = c('sex',NA),values_to = 'osteoporosis_year_risk') %>% 
    select(age, sex, osteoporosis_year_risk) %>% 
    mutate(osteoporosis_year_risk = as.numeric(osteoporosis_year_risk)/10000) 

  apply_osteoporosis_risk <- function(input_population){
    
    year = max(input_population$year)
    
    input_population <- input_population %>% 
      select(-any_of('osteoporosis_year_risk'))
    
    input_population <- input_population %>% 
      mutate(age_group = cut(age,breaks = c(-Inf,50,55,60,65,70,75,80,85,90, Inf),
                         labels =c("0-49","50–54","55–59","60–64","65–69","70–74","75–79","80–84","85–89","90-110")
      )) %>% 
      left_join(osteoporosis_incidence,by = c(age_group = 'age',sex='sex'))
    
    # input_population <- input_population %>% 
    #   mutate(
    #     osteoporosis = rbinom(n = nrow(input_population),
    #                                            size = 1,
    #                                            prob = osteoporosis_year_risk)*year
    #   ) 
    
    input_population <- input_population %>% 
      replace_na(list(osteoporosis_year_risk = 0)) 
    
    input_population
  }
  
  
  