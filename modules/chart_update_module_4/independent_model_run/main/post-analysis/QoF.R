
#Incidence ----
past_populations %>%
  filter(stroke == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) 

past_populations %>%
  filter(chd == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(diabetes == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(dementia == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(heart_failure == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(atrial_fibrillation == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypertension == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(chronic_kidney_disease == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(cancer == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(osteoarthritis == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(epilepsy == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)


past_populations %>%
  filter(copd == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(asthma == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(non_diabetic_hyperglycaemia == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypothyroidism == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(pad == year) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)


#Prevalence ----



past_populations %>%
  filter(stroke != 0) %>% 
  group_by(id,run) %>%
  fill(death,death_reason) %>%
  filter(is.na(death_reason)) %>% 
  ungroup() %>% 
  # filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) 

past_populations %>%
  filter(chd != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(diabetes != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(dementia != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(heart_failure != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(atrial_fibrillation != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypertension != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(chronic_kidney_disease != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(cancer != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(osteoarthritis != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(epilepsy != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)


past_populations %>%
  filter(copd != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(asthma != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(non_diabetic_hyperglycaemia != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypothyroidism != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(pad != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)



#Deaths ----


past_populations %>%
  # fill(death,death_reason) %>%
  filter(death_reason == 'stroke')
  ungroup() %>% 
  # filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) 

past_populations %>%
  filter(chd != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(diabetes != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(dementia != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(heart_failure != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(atrial_fibrillation != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypertension != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(chronic_kidney_disease != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(cancer != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(osteoarthritis != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(epilepsy != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)


past_populations %>%
  filter(copd != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(asthma != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(non_diabetic_hyperglycaemia != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypothyroidism != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(pad != 0) %>% 
  filter(year != min(year)) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

