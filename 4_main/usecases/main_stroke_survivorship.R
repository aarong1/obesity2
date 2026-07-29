# Association of physical activity level and all-cause mortality among stroke survivors: evidence from NHANES 2007–2018
#https://pmc.ncbi.nlm.nih.gov/articles/PMC12041438/#sec19


library(fst)
library(tidyverse)
library(readxl)

pop <- read.fst('./main/initial_time_zero_population10down.fst')

ppp <- read_xlsx(path =  'data/ni/ni_ppp_machine_readable.xlsx', sheet = 'Mortality_assumptions')

names(ppp)[-c(1,2)] <- str_extract( pattern = '[0-9]*', string = names(ppp)[-c(1,2)])

names(ppp)[1] <- 'sex'
names(ppp)[2] <- 'age'

all_cause_morbidity_rr_df <- data.frame(
  rr = c(1, 2),
  sle = c( 'normal','stroke')
)

all_cause_morbidity_stroke_rr_df <- data.frame(
  rr = c(1, 2),
  sle = c( 'normal','stroke')
)

all_cause_morbidity_risk_stroke_survivors_rr_df <- tribble(
  ~var, ~val, ~`some_activity`, ~`meets_rec`,
  'Age', '≤65', 0.58, 0.35,
  'Age', '>65', 0.50, 0.35,
  'Sex', 'Males', 0.58, 0.45,
  'Sex', 'Females', 0.53, 0.41,
  'Race', 'white', 0.62, 0.41,
  'Race', 'minority', 0.42, 0.49,
  'Ratio of family income to poverty', '≤1.3', 0.61, 0.54,
  'Ratio of family income to poverty', '1.3–3.5', 0.37, 0.51,
  'Ratio of family income to poverty', '>3.5', 0.38, 0.37,
  'Smoking', 'never_smoked', 0.67, 0.45,
  'Smoking', 'former', 0.54, 0.48,
  'Smoking', 'current_smoker', 0.31, 0.21,
  'Alcohol', 'no_risk', 0.61, 0.24,
  'Alcohol', 'increased_risk', 0.51, 0.69,
  'Alcohol', 'higher_risk', 0.36, 0.38,
  'BMI', 'normal', 0.29, 0.26, #'≤25',30
  'BMI', 'overweight', 0.88, 0.47, #'25–30
  'BMI', 'obese', 0.86, 0.61, #'>30',30
  'Education', 'less_high_school', 0.72, 0.45,
  'Education', 'high_school', 0.61, 0.69,
  'Education', 'college_or_higher', 0.37, 0.31,
  'Diabetes', 'TRUE', 0.77, 0.59,
  'Diabetes', 'FALSE', 0.45, 0.33,
  'Hypertension', 'TRUE', 0.57, 0.44,
  'Hypertension', 'FALSE', 0.85, 0.87,
  'Dyslipidemia', 'TRUE', 0.63, 0.53,
  'Dyslipidemia', 'FALSE', 0.45, 0.26
) %>% 
  filter(var %in% c('Age',
                    'Sex',
                    'Race',
'Smoking',
'Alcohol',
'BMI',
'Diabetes',
'Hypertension',
'Dyslipidemia')) %>% 
  pivot_longer(cols = c('some_activity', 'meets_rec') ) %>% 
  mutate(var_join=paste0(var,'_join'))

# pop %>% 
#   calc_stroke_prob %>% 
#   populate_stroke 
con <- dbConnect(duckdb::duckdb(), dbdir = 'past_populations_db/past_populations.duckdb', read_only = F)
latest_tbl <- sort(decreasing = T,dbListTables(con))[1]

# x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' USING SAMPLE 60 PERCENT (bernoulli);'))  # Set cache size to 2MB
x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' ;'))  # Set cache size to 2MB

# past_populations_20260116_015236

past_populations <- dbFetch(x)
dbClearResult(x)
dbDisconnect(con, shutdown=TRUE)
setDT(past_populations)

pop <- past_populations %>% 
  filter(year==min(year)) %>% 
  slice_sample(n=1000)
         
paf_df <- pop %>%   
  mutate(
    rr_stroke = ifelse((stroke != 0), 2, 1),
    'Age_join',
    'Sex_join',
    'Race_join',
    'Smoking_join',
    'Alcohol_join',
    'BMI_join',
    'Diabetes_join',
    'Hypertension_join',
    'Dyslipidemia_join'
  

paf_df <- pop %>% 
  count(age,sex,bmi) %>% 
  mutate(n = n*10) %>% 
  filter(!is.na(bmi)) %>% 
  pivot_wider(id_cols = c(age,sex),
              names_from = bmi,
              values_from = n) %>% 
  # left_join(rr_df) %>% 
  mutate(denom = obese * 1.20 + overweight * 1.07 + normal * 1  ) %>% 
  mutate(paf = (1 - (obese + normal + overweight)/ denom))


paf_min_df <- ppp %>% 
  pivot_longer(-c(1,2), names_to = 'year', values_to = 'q') %>% 
  mutate( q = q/100000 ) %>% 
  filter(age != 'Birth') %>% 
  mutate(age = as.numeric(age)) %>% 
  left_join( paf_df[c('age','sex','paf')]) %>% 
  replace_na(list(paf=0)) %>% 
  mutate( q_adj = q * (1-paf)) 

pop_d = data.frame()
pop_a = data.frame()

pop <- read.fst('./main/initial_time_zero_population10down.fst')
pop1 <- pop

for( k in 1:20 ){
  
  message(k)
  
  for(j in c('intervention','non-intervention')){
    
    message(j)
    pop1 <- pop
    
    for( i in 2023:2050){
      
      message(i)
      
      pop2 <- pop1 %>% 
        
        select(id, age, sex, year, bmi) %>% 
        mutate(year = i) %>%
        mutate(intervene = j) %>%
        mutate(age = age+1) %>% 
        
        mutate(year = as.character(year)) %>% 
        left_join(paf_min_df[c('age','sex','year','q_adj')],
                  relationship = "many-to-one" ) %>% # %>% #count(is.na(q_adj)) # "many-to-one"
        left_join(rr_df) %>% 
        mutate(rr = ifelse(j=='intervention' & i>=2026 & i<=2030 , 0.5 * (rr-1) + 1 , rr)) %>%
        mutate(q_modelled = q_adj * rr ) %>% 
        mutate(bern_trial = runif(n=n())) %>% 
        mutate(death = (bern_trial<q_modelled)) 
      
      # print('add births')
      # pop2 <- pop2 %>% 
      #   mutate(year = as.numeric(year)) %>% 
      #   mutate(id = as.character(id ))%>% 
      #   asfr_births( fertility = fertility)
      
      pop_dead <- pop2 %>%
        filter(death == T ) %>% 
        filter(age>30) %>% 
        count(year,intervene, run=k)
      
      pop1 <- pop2 %>% 
        filter(death == F ) 
      
      pop_alive <- pop1 %>% 
        filter(age>30) %>% 
        count(year,intervene,run = k)
      
      pop_d <- rbind( pop_d, pop_dead ) 
      pop_a <- rbind( pop_a, pop_alive ) 
      
    }
  }
}

ggplot(pop_a) +
  geom_point(aes(year, n, colour = intervene)) +
  geom_line(aes(year, n, colour = intervene))

library(echarts4r)

pop_a %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene) %>% 
  e_charts( year) %>% 
  e_line(n)

ggplot(pop_d) +
  geom_point(aes(year, n, colour = intervene))+
  # geom_line(aes(year, n, group = intervene)) +
  geom_rect(aes(xmin = '2026', xmax = '2030', ymin = 1401, ymax = 2600), fill = 'green', alpha = 0.005) +
  ylim(c(1400, 2600))

pop_a %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene) %>% 
  e_charts( year) %>% 
  e_line(n)

pop_d %>% 
  pivot_wider(names_from=intervene,values_from = n) %>% 
  mutate()

pop_a %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  ggplot() +
  geom_point(aes(year, n, colour = intervene)) +
  geom_line(aes(year, n, colour = intervene))

pop_d %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot() +
  geom_rect(aes(xmin = 2025, xmax = 2030, ymin = 1401, ymax = 2600),
            colour = 'lightgrey', 
            fill = 'green', alpha = 0.005) +
  ylim(c(1400, 2600))+
  geom_point(aes(year, n, colour = intervene))+
  geom_line(aes(year, n, group = intervene, colour = intervene)) +
  # scale_x_continuous(
  #   breaks = seq(2023, 2065, by = 5),
  #   labels = scales::label_number()
  # )+
  theme_minimal(base_family = "Graphik")

natural_var <- pop_d %>% 
  group_by(year,intervene) %>%
  summarise(nn=n(), sd =sd(n), n = mean(n)) %>% 
  mutate( error = sd / sqrt(nn) ) %>% 
  ungroup() %>% 
  summarise(error = mean(error)) %>% 
  pull(error)
# ungroup() %>% 
# ggplot() + 
# geom_point(aes(year, error)) 

x <- pop_d %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  pivot_wider(names_from=intervene,values_from = n) %>% 
  mutate(delta = intervention - `non-intervention`) %>% 
  ungroup() %>% 
  mutate(cum_delta = cumsum(delta))

x %>% 
  ggplot() +
  geom_col(aes(year, delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'none',
    delta - natural_var * 1.96 < 0 ~ 'down',
    delta - natural_var * 1.96 > 0 ~ 'up'
  )))  +
  scale_fill_manual(values = c("up" = "salmon", "down" = "lightgreen",'none' = 'black'))+
  geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  geom_smooth(method = 'loess',data  = x %>% filter(year>2030), aes(y =delta, x = year)) +
  theme_minimal() +
  labs(
    fill = 'title_text',
    x = "Year",
    y = "Delta"
  ) 
geom_bar(aes(year, delta )) +
  geom_line(aes(year, delta )) #+ 
# geom_rect(aes(xmin = '2026', xmax = '2030', ymin = 01, ymax = 2600), fill = 'green', alpha = 0.005) +
# ylim(c(1400, 2600))


x %>% 
  ggplot() +
  geom_col(aes(year, cum_delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'none',
    delta - natural_var * 1.96 < 0 ~ 'down',
    delta - natural_var * 1.96 > 0 ~ 'up'
  )))  +
  scale_fill_manual(values = c("up" = "salmon", "down" = "lightgreen",'none' = 'black'))+
  geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  geom_smooth(method = 'loess',data  = x %>% filter(year>2030), aes(y =delta, x = year)) +
  theme_minimal() +
  labs(
    fill = 'title_text',
    x = "Year",
    y = "Delta"
  ) 
geom_bar(aes(year, delta )) +
  geom_line(aes(year, delta )) + 
  
  
  apply_age_sex_death<- function(current_population, apply_death = F){
    
    year1 <- max(current_population$year)
    
    current_population <- select(current_population, - any_of( 'qx') )
    
    current_population <- as.data.table(current_population)[ as.data.table(lifetables), on = .( age, sex), nomatch = 0 ]
    
    current_population$bern_trial <- runif(n=length(current_population$qx))
    
    current_population = current_population[, `:=` (death = year1 * (bern_trial<qx))] 
    current_population[ , death_reason := ifelse(year1==death, 'age_sex_std', NA)]
    
    current_population <- as.data.frame(current_population) 
  }

library(readxl)
official <- read_excel("data/ni/ni_ppp_machine_readable.xlsx", 
                       sheet = "Population")

official %>% 
  pivot_longer(-c(Sex,Age), names_to = 'year', values_to = 'pop') %>% 
  count(Age, year, wt = pop) %>% 
  # filter(Age>30) %>% 
  filter(year<2055) %>% 
  count(year, wt= n) %>% 
  ggplot() +
  geom_point(aes(year, n))

