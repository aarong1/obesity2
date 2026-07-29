
library(fst)
# age_synthetic_population.R
lifetables_male <- read_excel("data/lifetables.xlsx", 
                              sheet = '2017-2019', #'2020-2022', #'2017-2019', #'2020-2022', #
                              range = "A6:F107") %>% 
  mutate(sex='Males') 

# extend MALE lifetables past 100 ----
lifetables_male_over100 <- lifetables_male[rep(101,15), ] |> 
  mutate(age=101:(100+n()))

lifetables_male <- rbind(lifetables_male, lifetables_male_over100)

lifetables_female <- read_excel("data/lifetables.xlsx", 
                                sheet = "2017-2019", #'2020-2022', #"2017-2019",
                                range = "H6:M107") %>% 
  mutate(sex='Females')


# extend FEMALE lifetables past 100 ----
lifetables_female_over100 <- lifetables_female[rep(101,15), ] |> 
  mutate(age=101:(100+n()))

lifetables_female <- rbind(lifetables_female, lifetables_female_over100)

lifetables <- rbind(lifetables_female,lifetables_male)

lifetables <- lifetables[c('age','sex','mx','qx')]


#lifetables$mx <- lifetables$mx*1.2
lifetables$age <- lifetables$age #-7

lifetables <- rbind(lifetables,
list(116, 'Males', 1, 1),
list(116, 'Females', 1, 1)
) 
 
pop <- read.fst('./synthetic_population/pop.fst')

origin_pop <- pop

pop_df <- tibble()
for( j in 1){ #20
  
    year_pop <- origin_pop
    year_pop$run = j
    #year_pop$age <- year_pop$age -1
  for( i in 2021:2055){ #2050
  
    print(i)
    print(j)
    
    #year_pop$age <- year_pop$age 
    
year_pop <- year_pop |> 
  left_join(lifetables) |> 
  mutate(sample = runif(n=n())) |> 
  mutate(dead = qx > sample) |> #View()
  filter(dead == FALSE)

dead_pop <- year_pop |> 
  left_join(lifetables) |> 
  mutate(sample = runif(n=n())) |> 
  mutate(dead = qx > sample) |> #View()
  filter(dead == TRUE) |> 
  count()

  print(dead_pop)
  
  # print()

yr_agg_pop <- year_pop |> 
    count(age, sex,year,run, mdm_quintile_soa, HSCT) |> 
  mutate(n = n*10)

  pop_df <- rbind(pop_df,
                  yr_agg_pop)
  
  year_pop$year <- year_pop$year + 1 
  year_pop$age <- year_pop$age + 1
  
  }
}

x <- write.fst(pop_df, 'pop_df.fst')

pp <- read_excel("data/SNPP18_SYA_Age_Bands.xlsx", 
                 sheet = "Tabular Single Year of Age",
                 skip = 15)


ppp <- read_excel("data/2022_ppp_proj_age_sex.xlsx", 
                  sheet = "Flat File") |> 
  filter(str_starts(pattern = 'All',
                    Sex,
                    negate = T),
         str_starts(pattern = 'Northern ',
                    Area,
                    negate = F) ) |> 
  mutate(Age= as.integer(Age)) |>
  filter(Age>=60,Age<=75) |> 
  filter(Age%%5==0) |>
  
  count(year = `Mid-Year`, Age, wt = NPP, name ='n') 


pp <- pp |> 
  filter(str_starts(pattern = 'All',
                     Gender,
                    negate = T),
         str_starts(pattern = 'Northern ',
                     Area_Name,
                    negate = F) ) |> 
  pivot_longer(-c(1:4),names_to ='year', values_to = 'pop') 
  
pp <- pp |> 
  filter(Age>=60,Age<=75) |> 
  filter(Age%%5==0) |>
  
  count(year, Age, wt = pop, name ='n') 

plot_yearly_populations <- save_yearly_populations |> 
  filter(age>=60,age<=79) |> 
  filter(age%%5==0) |>
  # filter(age%/%5==13) |>
  
  count(year, age) |> 
  mutate(n = n*10)

pop_df |> 
  filter(age>=60,age<=79) |> 
  # filter(age%%5==0) |>
  filter(age%/%5==15) |>
  count(year, age ,run,wt =n) |>  # =age%/%5
  
  #group_by(year,age) |> 
  #summarise(n=mean(n)) |> 
  
ggplot() +
  geom_line(aes(year,n,color= as.character(age) )) +
  #geom_line(aes(year,mean(n)),color='green')+
  geom_line(data = plot_yearly_populations, lty = 2, aes( color = as.character(age), x = as.integer(year), y = n))+
    geom_line(data = pp, lty = 3, aes( color = as.character(Age), x = as.integer(year), y = n))+
    geom_line(data = ppp, lty = 4, aes( color = as.character(Age), x = as.integer(year), y = n)) +
    theme_minimal()

saveRDS(pop_df, file = 'pop.rds')

#mortality
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/methodologies/nationalpopulationprojectionsmortalityassumptions2022based#method-for-setting-the-mortality-assumptions
#fertility
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/methodologies/nationalpopulationprojectionsfertilityassumptions2022based
# migration
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/methodologies/nationalpopulationprojectionsmigrationassumptions2022based

# pop <- instantiate_base_pop()

current_population <- pop |> 
  ungroup() #|> 
  #sample_frac(size = 0.01)

source('./births_module/births_by_fertility_projections.R')
source('./deaths_module/apply_age_sex_death.R')
current_population <- read.fst('./synthetic_population/pop.fst')

save_yearly_populations <- data.frame(origin_pop) #current_population

current_population <- origin_pop 
for (i in 1:25){# 25
  
  print(i)

current_population <- current_population|> 
  asfr_births(fertility) |> 
  
  apply_age_sex_death(apply_death = T) 

print(count(current_population, death))
  
  current_population <- current_population|> 
  filter(death == 0)

current_population$year <- current_population$year + 1
current_population$age <- current_population$age + 1

save_yearly_populations <- bind_rows(save_yearly_populations,current_population)

}

write.fst(save_yearly_populations, './births_module/2045_yearly_start_populations.fst')

save_yearly_populations |> #count(death)
  group_by(year) |> 
  summarise(pension = sum(age>=67),
         working = sum(age>=18 & age<65) 
         ) |>
  # filter()
  
  group_by(year) |> 
  summarise(ratio = pension/working) |> 
  ggplot() +
  geom_line(aes(year,ratio))


