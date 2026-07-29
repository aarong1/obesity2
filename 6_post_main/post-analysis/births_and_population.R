

past_populations %>% 
  filter(age==0) %>% 
  pull(id) %>% 
  as.character()# count(year,run) %>% group_by(year) %>% summarise(mean(n))

past_populations %>% 
  filter(age==0) %>% 
  # select(mothers_age) %>% 
  count(year,run,mothers_age) %>% 
  right_join(expand.grid (run = unique(.$run),mothers_age = unique(.$mothers_age),year = unique(.$year))) %>%
  replace_na(list(n=0)) %>%
  group_by(year,mothers_age) %>% 
  summarise(n = mean(n)*model_specification$population$scale_down_factor)

past_populations %>% 
  filter(age==0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(mean(n)*model_specification$population$scale_down_factor)

# Live births 

# Table 3.3 Live births by Mother's Age, 1974 to 2023 [Notes 1 and 2]

births_timeseries_mothersAge <- read_excel("data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx", 
                                       sheet = "Table 3.3", 
                                       range = 'A4:AO54')
actual_births <- births_timeseries_mothersAge %>% 
  select(year = Year, births = `All Ages`) %>% 
  tail(n=15)

modelled_births <- past_populations %>% 
  filter(age==0) %>% 
  count(year,name='births') %>% 
  mutate(births =births *model_specification$population$scale_down_factor)

ggplot() +
  geom_line(data = modelled_births,aes(year,births)) +
  geom_line(data = actual_births,aes(year,births),color='green')


# Table 3.8a Live births by Health and Social Care Trust, sex and month of registration, 2023 [Notes 1 and 2]
# Table 3.8b Live births by Local Government District, sex and month of registration, 2023 [Notes 1 and 2]
# data below and sex not significant

# Table 3.16 Live births, stillbirths and maternities rates (per 1,000 women), by sex of child and age of mother, 2023 [Notes 1, 2, 7 and 9]
# dont want to have to find denominator sex available elsewhere (leans males)
# we are not going to take the age of mother for determination for outcome of pregnancy 
# or sex of child.


# Fertility Rates

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



#Table 3.14 Age-specific fertility rates (single years) 1985 to 2023 [Notes 1, 2 and 9]
ASFR_SYA <- read_excel("data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx", 
                                           sheet = "Table 3.14", 
                                           range = 'A4:AN35')
#Table 3.17 Cumulative Fertility by female birth cohort and selected age, Northern Ireland, 2023  [Notes 1, 2 and 12]

#Table 3.15a	Births, general fertility rates and TPFRs by Health and Social Care Trust, 2013 to 2023

#Table 3.15b	Births, general fertility rates and TPFRs by Local Government District, 2013 to 2023

births_LGD <- read_excel("data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx", 
                       sheet = "Table 3.14", 
                       range = 'A4:AB16')
