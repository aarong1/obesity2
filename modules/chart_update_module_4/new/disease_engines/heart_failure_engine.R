# For every five-point increase in BMI, the risk of heart failure rose by 32 percent in the study. 

# 1.32 per 5 kgm-2

HF <- tribble(				
~age, ~sex,	~n, ~'Total PY',	~'Incident',	~per1000,	~CI,	~HFrEF,	~HFpEF,	~Unknown,
'18-45',  'Males',	3030042,	9083853.9,	3118,	0.34,	"0.33-0.36",	17.8,	10.7,	71.5,
'18-45',  'Females',	3121844,	8666833.9,	2164,	0.25,	"0.24-0.26",	13.4,	8.3,	78.3,
'45-64',  'Males',	1994544,	6778722.1,	19405,	2.86,	"2.82-2.90",	17.6,	9.9,	72.6,
'45-64',  'Females',	1916814,	6592247.7,	11021,	1.67,	"1.64-1.70",	13.0,	13.5,	73.6,
'65-74',  'Males',	749922,	2233188.5,	22029,	9.86,	"9.74-10.00",	15.5,	7.6,	76.9,
'65-74',  'Females',	803145,	2429971.2,	15884,	6.54,	"6.44-6.64",	11.0,	12.1,	76.9,
'75-84',  'Males',	418537,	1142713.4,	27861,	24.38,	"24.10-24.67",	12.5,	6.3,	81.2,
'75-84',  'Females',	520555,	1443004.6,	27663,	19.17,	"18.95-19.40",	8.0,	9.6,	82.5,
'85-110',  'Males',	146703,	339066.6,	18722,	55.22,	"54.43-56.01",	7.4,	3.8,	88.8,
'85-110',  'Females',	259333,	620985.1,	27923,	44.97,	"44.44-45.50",	4.2,	4.6,	91.2
)


apply_heart_failure_risk_factors <- function(input_population) {
    # Accept Data.Table
    dt <- as.data.table(input_population)

    # Prepare incidence table
    # Age groups in chd_incidence_per100k: '30-54', '55-64', '65-74', '75-84', '85-110'
    # We assume 0 incidence for age < 30

    # Create mapping for age groups
    dt[, age_group_inc := cut(age, 
                              breaks = c(-Inf, 18, 44, 64, 74, 84, 110),
                              labels = c("0-18", "45-64", "65-74", "65-74", "75-84", "85-110"),
                              right = TRUE)]
    
    # Join incidence
    dt[HF, on = .(age_group_inc = age, sex), heart_failure_year_risk := per1000 / 1000
]
    # dt[, heart_failure_year_risk := per1000 / 1000]
    
    # Fill NA with 0 (for age 0-29 etc)
    dt[is.na(heart_failure_year_risk), heart_failure_year_risk := 0]
    
    # Cleanup
    dt[, age_group_inc := NULL]
    
    return(dt)
}

x <- initial_time_zero_population %>% 
  select(-heart_failure_year_risk) %>% 
  apply_heart_failure_wo_risk_factors()


HF <- tribble(				
  ~age, ~sex,	~n, ~'Total PY',	~'Incident',	~per1000,	~CI,	~HFrEF,	~HFpEF,	~Unknown,
  '18-45',  'Males',	3030042,	9083853.9,	3118,	0.34,	"0.33-0.36",	17.8,	10.7,	71.5,
  '18-45',  'Females',	3121844,	8666833.9,	2164,	0.25,	"0.24-0.26",	13.4,	8.3,	78.3,
  '45-64',  'Males',	1994544,	6778722.1,	19405,	2.86,	"2.82-2.90",	17.6,	9.9,	72.6,
  '45-64',  'Females',	1916814,	6592247.7,	11021,	1.67,	"1.64-1.70",	13.0,	13.5,	73.6,
  '65-74',  'Males',	749922,	2233188.5,	22029,	9.86,	"9.74-10.00",	15.5,	7.6,	76.9,
  '65-74',  'Females',	803145,	2429971.2,	15884,	6.54,	"6.44-6.64",	11.0,	12.1,	76.9,
  '75-84',  'Males',	418537,	1142713.4,	27861,	24.38,	"24.10-24.67",	12.5,	6.3,	81.2,
  '75-84',  'Females',	520555,	1443004.6,	27663,	19.17,	"18.95-19.40",	8.0,	9.6,	82.5,
  '85-110',  'Males',	146703,	339066.6,	18722,	55.22,	"54.43-56.01",	7.4,	3.8,	88.8,
  '85-110',  'Females',	259333,	620985.1,	27923,	44.97,	"44.44-45.50",	4.2,	4.6,	91.2
)

initial_time_zero_population
# 1.32 per 5 kgm-2



initial_time_zero_population$pm25g
pollution_rr <- 1.09

heart_failure_theoretical_minimum <-
  initial_time_zero_population  |> 
  group_by(age_match,sex) |> 
  summarise(AF = 1-n()/sum(pollution_rr^( (pm25g)/10))) |> #-min(pm25g)
  left_join(
    lung_cancer_data,
    by= c('age_match' = 'age', 'sex')
  ) %>% 
  mutate(min_prob = per100k/100000 * (1-AF) ) 

# mutate( (1-AF) ) |>
# left_join(
#   lung_cancer_incidence
# ) |>
# mutate(lung_cancer_prob_min = lung_cancer_prob * (1-AF) ) |> 

# View() 


apply_lung_cancer_risk_w_pollution <- function(input_population){
  
  input_population <- input_population %>% 
    mutate(age_match = cut( age,
                            breaks = c(-Inf, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                            labels = c("0-39", "40-44", "45-49", "50-54",
                                       "55-59", "60-64", "65-69", "70-74", "75-79",
                                       "80-84", "85-89", "90-110"))) %>% 
    mutate(RR_pm25 = pollution_rr^( (pm25g)/10)) %>% 
    left_join(lung_cancer_theoretical_minimum[c('age_match', 'sex', 'min_prob')],
              by = c('age_match','sex')) %>% 
    mutate(lung_cancer_year_risk = min_prob * RR_pm25) 
  
  input_population <- input_population %>% 
    select(-c(min_prob, RR_pm25, age_match))
  
}

initial_time_zero_population %>% 
  mutate(pm25g = 0.5*pm25g) %>% 
  apply_lung_cancer_risk_w_pollution() %>% 
  count(wt = lung_cancer_year_risk)




