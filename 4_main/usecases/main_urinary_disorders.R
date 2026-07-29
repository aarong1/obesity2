library(fst)
library(tidyverse)
library(readxl)

pop <- read.fst('./main/initial_time_zero_population10down.fst')

ppp <- read_xlsx(path =  'data/ni/ni_ppp_machine_readable.xlsx', sheet = 'Mortality_assumptions')

names(ppp)[-c(1,2)] <- str_extract( pattern = '[0-9]*', string = names(ppp)[-c(1,2)])

names(ppp)[1] <- 'sex'
names(ppp)[2] <- 'age'

rr_df <- data.frame(
  rr = c(1, 1.85),
  sle = c( 'normal','sle')
)

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

