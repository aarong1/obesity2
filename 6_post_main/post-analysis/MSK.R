library(tidyverse)

(
  osteoarthritis_incidence_df <- past_populations %>%
    group_by(id,run) %>% 
    arrange(year) %>% 
    fill(death,death_reason) %>% 
    ungroup() %>% 
    filter(is.na(death_reason)) %>% 
  filter(osteoarthritis == year) %>% 
  filter(year != min(year)) %>% 
  count(year,run,osteoarthritis) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor))

(
  rheumatoid_arthritis_incidence_df <- past_populations %>%
  group_by(id,run) %>% 
  arrange(year) %>% 
  fill(death,death_reason) %>% 
  ungroup() %>% 
  filter(is.na(death_reason)) %>% 
  filter(rheumatoid_arthritis == year) %>% 
  filter(year != min(year)) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor) 
)

past_populations %>%
  count(year,stroke)
past_populations %>%
  count(year, wt=stroke_year_risk)

past_populations %>%
  count(year,run,osteoarthritis) %>% 
  group_by(year,osteoarthritis) %>% 
  summarise(mean(n))

past_populations %>%
  count(year, run,wt = osteoarthritis_year_risk) %>% 
  group_by(year) %>% 
  summarise(mean(n))

past_populations %>%
  count(year,rheumatoid_arthritis)
past_populations %>%
  count(year, wt = rheumatoid_arthritis_year_risk)

osteoporosis_incidence_df <- past_populations %>%
  
  group_by(id,run) %>% 
  arrange(year) %>% 
  fill(death,death_reason) %>% 
  ungroup() %>% 
  filter(is.na(death_reason)) %>% 
  
  filter(year != min(year)) %>% 
  filter(osteoporosis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor) 



rheumatoid_arthritis_risk_sum_df <- past_populations %>%
  # filter(rheumatoid_arthritis == year) %>%
  count(year,run,wt = rheumatoid_arthritis_year_risk) %>% 
  filter(year != min(year)) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor)

osteoporosis_risk_sum_df <- past_populations %>%
  # filter(osteoporosis == year) %>% 
  count(year,run,wt = osteoporosis_year_risk) %>% 
  filter(year != min(year)) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor)

osteoarthritis_risk_sum_df <- past_populations %>%
  # filter(osteoarthritis == year) %>% 
  count(year,run,wt = osteoarthritis_year_risk) %>% 
  filter(year != min(year)) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor)

ggplot() +
  geom_point(data = rheumatoid_arthritis_incidence_df, aes(year,n))+
  geom_point(data = rheumatoid_arthritis_risk_sum_df, aes(year,n),colour='blue')+
ylim(c(0,NA))

ggplot() +
  geom_point(data = osteoarthritis_incidence_df, aes(year,n))+
  geom_point(data = osteoarthritis_risk_sum_df, aes(year,n),colour='blue')+
ylim(c(0,NA))

ggplot() +
  geom_point(data = osteoporosis_incidence_df, aes(year,n))+
  geom_point(data = osteoporosis_risk_sum_df, aes(year,n),colour='blue')+
  ylim(c(0,NA))

####-----------

# initial_time_zero_population %>%
#   apply_osteoarthritis_prevalence() %>%
#   ungroup() %>%
#   count(osteoarthritis)

# initial_time_zero_population %>%
#   apply_osteoarthritis_risk(osteoarthritis_incidence) %>%
#   transmute( sex, age_group = cut(age,breaks = c(-Inf,20,30,40,50,60,70,80,90, Inf),
#                                     labels =c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-110")
#   ),
#   osteoarthritis_year_risk,
#             rank(osteoarthritis_year_risk),
#             max(frank(osteoarthritis_year_risk))) %>%
#   mutate(osteoarthritis_percentile = frank(osteoarthritis_year_risk)/max(frank(osteoarthritis_year_risk))) %>%
#   left_join(osteoarthritis_prevalence,relationship = 'many-to-one',by=c('age_group', sex = 'sex')) %>%
#   mutate(osteoarthritis = ifelse(runif(n()) < osteoarthritis_prevalence_prob/0.5 * osteoarthritis_percentile ,2023,0)) %>% count(osteoarthritis)

# initial_time_zero_population %>% count(osteoarthritis_percentile)

past_populations %>%
  filter(osteoarthritis !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor) %>% 
  ggplot()+
  geom_point(aes(year,n))

past_populations %>%
  # filter(osteoarthritis !=0) %>% 
  count(year,rheumatoid_arthritis,run) %>% 
  group_by(year, rheumatoid_arthritis) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

past_populations %>%
  # filter(osteoarthritis !=0) %>% 
  count(year,osteoporosis,run) %>% 
  group_by(year, osteoporosis) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

past_populations %>%
  # filter(osteoarthritis !=0) %>% 
  count(year,osteoarthritis,run) %>% 
  group_by(year, osteoarthritis) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

past_populations %>%
  filter(rheumatoid_arthritis !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n=n*model_specification$population$scale_down_factor) %>% 
  full_join(actual_rheumatoid_arthritis_perv) %>% 
  ggplot()+
  geom_point(aes(year,Count),col='blue') +
  geom_point(aes(year,n))

past_populations %>% #count(osteoporosis)
  filter(osteoporosis !=0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor) %>% 
  # mutate(prev=cumsum(n)) %>% 
  full_join(actual_osteoporosis_perv) %>% 
  ggplot()+
  geom_point(aes(year,Count),col='blue') +
  geom_point(aes(year,n))

past_populations %>% #count(osteoporosis)
  # filter(epilepsy !=0) %>% 
  count(year,epilepsy,run) %>% 
  group_by(year,epilepsy) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

actual_osteoporosis_perv <- prevalence %>% 
  filter(Disease == 'Osteoporosis') %>% 
  filter(!is.na(Count)) %>% 
  mutate(year = as.numeric(str_extract(string = Year, group=0, pattern = '[0-9]*')))
  
actual_rheumatoid_arthritis_perv <- prevalence %>% 
    filter(Disease == 'Rheumatoid Arthritis') %>% 
    filter(!is.na(Count)) %>% 
    mutate(year = as.numeric(str_extract(string = Year, group=0, pattern = '[0-9]*')))


