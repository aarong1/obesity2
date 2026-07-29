library(readxl)



past_populations %>% 
  count(death_reason,year,run) %>% 
  group_by(death_reason,year) %>%
  summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  filter(!is.na(death_reason)) %>%
  ggplot(aes(x=year,y=n,colour=death_reason)) +
  geom_line()+
  facet_grid(~death_reason)


modelled_deaths <- past_populations %>% 
  count(year,run,dead = !is.na(death_reason)) %>% 
  filter(dead==TRUE) %>%
  group_by(year) %>%
  summarise(deaths=mean(n)*model_specification$population$scale_down_factor) %>% 
  mutate(year= as.character(year))

actual_deaths <- deaths_timeseries_sex_cause_all %>% 
  summarise(across(-c(1,2), sum, na.rm=TRUE)) %>% 
  pivot_longer(everything(),names_to = 'year',values_to = 'deaths') 
  

  ggplot() +
  geom_point(data = actual_deaths, aes(x=year,y=deaths)) +
    geom_point(data = modelled_deaths, aes(x=year,y=deaths),color='blue')
    

# all deaths by cause and sex

deaths_timeseries_sex_cause_all <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                         sheet = "Table 6.1", 
                                         range = "A4:M38")

deaths_timeseries_sex_cause_male <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                              sheet = "Table 6.1", 
                                              range = "A40:M74")

deaths_timeseries_sex_cause_female <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                              sheet = "Table 6.1", 
                                              range = "A76:M110")

# all deaths by cause and age and sex

deaths_age_sex_cause_all <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                       sheet = "Table 6.2", 
                                       range = "A4:P62")

deaths_age_sex_cause_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                       sheet = "Table 6.2", 
                                       range = "A64:P122")

deaths_age_sex_cause_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                         sheet = "Table 6.2", 
                                         range = "A124:P182")


# all deaths by cause and age and sex


deaths_sex_age_timeseries_all <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                       sheet = "5.3", 
                                       range = "A4:N15")

deaths_sex_age_timeseries_males <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                         sheet = "5.3", 
                                         range = "A17:N27")

deaths_sex_age_timeseries_females <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                           sheet = "5.3", 
                                           range = "A30:N31")



# deaths by geography
# by trust

deaths_sex_age_trust_all <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                         sheet = "5.4a", 
                                         range = "A4:V10")

deaths_sex_age_trust_males <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                           sheet = "5.4a", 
                                           range = "A12:V18")

deaths_sex_age_trust_females <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                         sheet = "5.4a", 
                                         range = "A20:V26")


# by lgd
deaths_sex_age_lgd_all <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                   sheet = "5.4b", 
                                   range = "A4:V16")

deaths_sex_age_lgd_males <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                     sheet = "5.4b", 
                                     range = "A18:V30")

deaths_sex_age_lgd_females <- read_excel("data/registrar_general_annual_reports/Section 5 - Death_Tables_2023.xlsx", 
                                     sheet = "5.4b", 
                                     range = "A32:V44")


# suicides
ni_suicides_age_sex <- read_ods("data/registrar_general_annual_reports/Suicide_Tables_Review_ODS.ods"   , sheet = 'Table2', range = 'A5:P13')

names(ni_suicides_age_sex)[2] <- 'sex'
names(ni_suicides_age_sex)[4] <- '0-19'
names(ni_suicides_age_sex)[16] <- '75-110'

ni_suicides_age_sex <- ni_suicides_age_sex |> fill(Year)

ni_suicides_age_sex <- ni_suicides_age_sex |> 
  pivot_longer(cols = -c(1:2), 
               names_to = 'age_group',
               values_to = 'Count' ) |> 
  filter(age_group!='Total')

ni_suicides_age_sex <- ni_suicides_age_sex |> 
  filter(sex != 'All') |> 
  mutate(sex = paste0(sex,'s'))
