library(tidyverse)
library(readxl)
library(readr)
library(fst)

# Projections for population estimates take estimate of fertility into account
# 
# The long-term low-fertility variant is calculated as principal minus 0.2; 
# the high-fertility variant is calculated as the principal plus 0.1 in the short-term, 
# rising to the principal plus 0.2 in the longer term.
# #https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/methodologies/nationalpopulationprojectionsfertilityassumptions2022based
# 
####################### Total fertility rate #####################################
##################### also the same: Total period fertility rate ############################

# The total fertility rate is the average number of live children that a group of women would bear if they experienced 
# the age-specific fertility rates of the calendar year throughout their childbearing lifespan.
# 
# 
# The single most important factor in population growth is the total fertility rate (TFR).
# If, on average, women give birth to 2.1 children and these children survive to the age of 15,
# any given woman will have replaced herself and her partner upon death. 
# A TFR of 2.1 is known as the replacement rate
# 
# 
########################  Age specific fertility rate ###########################
# Age-specific fertility rate refers to the number of births to females 
# in a particular age category in a particular year compared 
# to the number of females in that age category.
# 
# unit: per 100k
# 
############################# General fertility rate ############################
# The number of live births in a year per 1,000 women 
# aged 15 to 44 years. Measure of current fertility levels. 

#projection

pop_proj <- read_excel("data/2022_ppp_proj_age_sex.xlsx", 
                       sheet = "Tabular Single Year of Age", 
                       skip = 2)
names(pop_proj) <- pop_proj[1,]

pop_proj <- pop_proj[-1,]

pop_proj <- pop_proj |> 
  filter(Sex == 'All Persons', 
         Age ==0) |> 
  pivot_longer(-(1:3),names_to = 'Year',values_to='Count')|> 
  mutate(Year = as.numeric(Year))

# historical
rg_births <- read_excel("data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx", 
                        sheet = "Table 3.1", skip = 3) |> 
  rename(c(all_live_births = `All live births`)) |> 
  filter(Year != 'Contents') |> 
  mutate(Year = as.numeric(Year))


#fertility historic 
ni_sya_fertility <- read_excel("data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx", 
                            sheet = "Table 3.14", skip = 3)

ni_sya_fertility <- ni_sya_fertility |> 
  
  pivot_longer(-1,,names_to = 'Year',values_to='ni_fertility_rate_per1k' ) |> 
  rename(c(mother_age = 'Age of mother')) |> 
  filter(!is.na(mother_age)) |> 
  mutate(mother_age = stringr::str_extract(mother_age, "\\d+"))  |> 
  mutate(across(everything(), as.numeric))

ni_sya_fertility |> 
  filter(mother_age%%5==0) |> 
ggplot()+
  geom_line(aes(Year, ni_fertility_rate_per1k  , group=mother_age, col=as.factor(mother_age)))+
  geom_smooth(method = 'loess',aes(Year, ni_fertility_rate_per1k  , group=mother_age, col=as.factor(mother_age)))

# NI bulk projected 
NI_projected_fertility_rate <- read_csv("data/NI_projected_fertility_rate.csv", 
                                        skip = 6) |> 
  rename(c(NI_crude = 'Northern Ireland'))


# UK age specific projected
UK_ASFR_projected <- read_csv("data/UK_ASFR_projected.csv", 
                                        skip = 6) 

UK_ASFR_projected <- UK_ASFR_projected |> 
  pivot_longer(-1,names_to = 'age',values_to='uk_fertility_rate') |> 
  separate(age,sep = ' to ', into = c('age_lower','age_higher')) |> 
  mutate(across(everything(), as.numeric))


past_populations <- read.fst( './past_populations/past_populations_sppg_undefined.fst',)


child_bearing_population <- past_populations |> 
  filter(sex=='Females', 15 <= age, age <= 44) |> 
  count(year,run,age, sex) |> 
  group_by(year, age, sex) |> 
  summarise(pop = mean(n) * 475) #model_specification$population$scale_down_factor )



child_bearing_population <- child_bearing_population |> 
  left_join(UK_ASFR_projected,
            join_by(year == Year,between(age, 
                           age_lower, 
                           age_higher)))
  
child_bearing_population <- select(child_bearing_population ,-c(age_lower, age_higher)) |> 
  left_join(select(NI_projected_fertility_rate,Year,NI_crude),
            by = c(year = 'Year'))


child_bearing_population <- child_bearing_population |> 
  left_join(ni_sya_fertility,
            by = c(year = 'Year', age = 'mother_age')
            )


child_bearing_population <- child_bearing_population |> 
   mutate(uk_pred_births = uk_fertility_rate * pop/1000,
          NI_crude_pred_births = NI_crude/30 * pop,
          ni_sya_pred_births = ni_fertility_rate_per1k * pop/1000
          )

total_pred_births <- child_bearing_population |> 
  group_by(year) |> 
  summarise(uk_pred_births_total = sum(uk_pred_births),
            NI_crude_pred_births_total = sum(NI_crude_pred_births),
            ni_sya_pred_births_total = sum(ni_sya_pred_births,na.rm = F)
            )
  
ggplot() +
  geom_point(data = pop_proj,
            mapping = aes(Year, Count), col='purple') +

  geom_point(data = filter(rg_births,Year>1990),
            mapping = aes(Year, all_live_births),col='blue') +

  geom_point(data = total_pred_births,
             mapping = aes(year, uk_pred_births_total),col='green') +

  geom_point(data = total_pred_births,
             mapping = aes(year, NI_crude_pred_births_total),col='orange') +

  geom_point(data = total_pred_births,
             mapping = aes(year, ni_sya_pred_births_total),col='red',size=3) + ylim(c(0,NA))


library(tidyverse)
library(mgcv)

# suppose your data is in df with columns: Year, fertility_rate, mother_age
# Fit GAM for each age group

ni_sya_fertility <- ni_sya_fertility |> 
  filter(!is.na(mother_age))

fits <- ni_sya_fertility |> 
  # filter(mother_age%%5==0) %>%
  group_by(mother_age) %>%
  group_map(~ gam(ni_fertility_rate_per1k ~ s(Year, k = 25), data = .x))

# Create future years
future_years <- data.frame(Year = 2024:2060)

# Predict forward conservatively
proj <- ni_sya_fertility |> 
  # filter(mother_age%%5==0) %>%
  group_by(mother_age) %>%
  group_map(.keep = T, .f = ~ {
    fit <- gam(log(ni_fertility_rate_per1k) ~ Year, data = .x)
    pred <- predict(fit, newdata = future_years, se.fit = TRUE)
    tibble(
      mother_age = first(.x$mother_age),
      Year = future_years$Year,
      fit = pmax(pred$fit, 0),  # no negatives
      lower = pmax(pred$fit - 1.96 * pred$se.fit, 0),
      upper = pred$fit + 1.96 * pred$se.fit
    )
  }) %>% bind_rows()

proj <- ni_sya_fertility |>  
  filter(Year==2023) |> 
  select(mother_age, ni_fertility_rate_per1k) %>% 
  left_join(proj,.) |> 
  rowwise() |>
  mutate(fit1 = sum(exp(fit), ni_fertility_rate_per1k,ni_fertility_rate_per1k)/3) |> 
  ungroup()



# Plot observed + projections
ggplot( ni_sya_fertility ,
          # filter(mother_age%%5==0), 
        aes(Year, ni_fertility_rate_per1k, colour = factor(mother_age))) +
  geom_line() +
    geom_line(data = proj, aes(Year, 
                               #exp(fit),
                               fit1,
                               colour = factor(mother_age)), linetype = "dashed") +
  # geom_ribbon(data = proj,
  #             aes(x = Year, y= exp(fit), ymin = exp(lower), ymax = exp(upper), fill = factor(mother_age)),
  #             alpha = 0.2, colour = NA) +
  theme_minimal()


