# risk
# morbidity

#RISK FACTORS

past_populations %>%
  count(smoking,year,run) %>%
  group_by( year,smoking) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
ggplot()+geom_point(aes(x=year,y=n,color=smoking))

past_populations %>%
  count(alcohol,year,run) %>%
  group_by(alcohol,year) %>% 
    summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_point(aes(x=year,y=n,color=alcohol))

past_populations %>%
  count(diet,year,run) %>%
  group_by(diet,year) %>% 
    summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_point(aes(x=year,y=n,color=diet))

past_populations %>%
  count(pa,year,run) %>%
  group_by(pa,year) %>% 
    summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_point(aes(x=year,y=n,color=pa))

past_populations %>%
  count(cholesterol_status,year,run) %>%
  group_by(cholesterol_status,year) %>% 
    summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_point(aes(x=year,y=n,color=cholesterol_status))

past_populations %>%
  count(hypertension_status,year,run) %>%
  group_by(hypertension_status,year) %>% 
    summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_point(aes(x=year,y=n,color=hypertension_status))

past_populations %>%
  count(diabetes_status,year,run) %>%
  group_by(diabetes_status,year) %>% 
    summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_point(aes(x=year,y=n,color=diabetes_status))


#MORBIDITTY


past_populations %>% #count(death_reason)
  filter( !is.na( death_reason )) %>%
  filter(stroke != 0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_line(aes(x=year,y=n))

past_populations %>%
  filter(chd !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(diabetes !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(dementia !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(heart_failure !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(atrial_fibrillation !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypertension !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(chronic_kidney_disease !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(lung_cancer !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(colorectal_cancer !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(osteoarthritis !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(epilepsy !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(rheumatoid_arthritis !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(copd !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(asthma !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(non_diabetic_hyperglycaemia !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(osteoporosis !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypothyroidism !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(pad !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(low_birth_weight !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

