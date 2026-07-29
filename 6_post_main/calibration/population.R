
##### Deaths ####

deaths_by_year <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                               sheet = "5.1", range = 'A118:D141') 

names(deaths_by_year) <- c('year', 'deaths','Males','Females')

past_populations %>% 
  count(year, death) %>% 
  filter(death!=0) %>% 
  ggplot()+
  geom_line(aes(death, n*20), color = 'black',lwd = 1) +
  geom_line(aes(year, deaths), 
            data = deaths_by_year, 
            color = 'red', 
            lwd = 1) 

#### Birthe ####

births_by_year <- read_excel('./data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx',
                             sheet = "Table 3.1", range = 'A118:D141') 

names(births_by_year) <- c('year', 'births','Males','Females')

past_populations %>% 
  filter(age==0) %>% 
  count(year) %>% 
  ggplot() +
  geom_point(aes(year, n*20), color = 'black',lwd = 1) +
  geom_line(aes(year, n*20), color = 'black',lwd = 1) +
  geom_line(aes(year, births), 
            data = births_by_year, 
            color = 'red', 
            lwd = 1) 


##### Population ####

population_by_year <- read_excel('./data/registrar_general_annual_reports/Section 2 - Population_Tables_2023.xlsx',
                             sheet = "Table 3.1", range = 'A24:B47')

names(population_by_year) <- c('year','pop','Males','Females')

population_by_year <- population_by_year %>% 
  mutate(pop = as.numeric(pop) * 1000,
         Males = as.numeric(Males) * 1000,
         Females = as.numeric(Females) * 1000)

pop_proj_by_year <- read_excel('./data/2022_ppp_proj_age_sex.xlsx',sheet = 'Flat File') %>% 
  filter(Sex == 'All Persons') %>% 
  group_by(year =`Mid-Year`) %>% 
  summarise(population = sum(NPP))

modelled_pop <- past_populations %>% 
  filter(death==0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>% 
  mutate(pop = n*model_specification$population$scale_down_factor) 


ggplot()+
  geom_line(aes(year, pop), data = modelled_pop, color = 'black', lwd = 1) +
  geom_line(aes(year, pop), data = population_by_year, color = 'blue', lwd = 1) +
  geom_line(aes(year, population), data = pop_proj_by_year, color = 'red', lwd = 1)
