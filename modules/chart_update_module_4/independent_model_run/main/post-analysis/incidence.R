# Incidence 
# Cumulative Incidence

#INCIDENCE 

past_populations %>%
  filter(stroke == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) 

past_populations %>%
  filter(chd == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(diabetes == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(dementia == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(heart_failure == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(atrial_fibrillation == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypertension == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(chronic_kidney_disease == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(lung_cancer == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(colorectal_cancer == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(osteoarthritis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(epilepsy == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(rheumatoid_arthritis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  ggplot()+geom_point(aes(x=year,y=n))



past_populations %>%
  filter(osteoporosis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  ggplot()+geom_point(aes(x=year,y=n))

past_populations %>%
  filter(osteoarthritis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  ggplot()+geom_point(aes(x=year,y=n))

past_populations %>%
  filter(copd == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(asthma == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(non_diabetic_hyperglycaemia == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)


past_populations %>%
  filter(hypothyroidism == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(pad == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(low_birth_weight == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)


# CUMULATIVE INCIDENCE 

past_populations %>%
  filter(stroke == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) %>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(chd == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(diabetes == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(dementia == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(heart_failure == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(atrial_fibrillation == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(hypertension == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(chronic_kidney_disease == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(lung_cancer == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(colorectal_cancer == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(osteoarthritis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(epilepsy == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(rheumatoid_arthritis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(copd == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(asthma == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(non_diabetic_hyperglycaemia == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)%>% 
  mutate(n = cumsum(n))

past_populations %>%
  filter(osteoporosis == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypothyroidism == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(pad == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(low_birth_weight == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)





# INCIDENCE by sum of risk

past_populations %>%
  filter(stroke == year) %>% 
  count(year,run,wt = stroke_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(chd == year) %>% 
  count(year,run,wt = chd_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(diabetes == year) %>% 
  count(year,run,wt = diabetes_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(dementia == year) %>% 
  count(year,run,wt = dementia_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(heart_failure == year) %>% 
  count(year,run,wt = heart_failure_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(atrial_fibrillation == year) %>% 
  count(year,run,wt = atrial_fibrillation_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(hypertension == year) %>% 
  count(year,run, wt = hypertension_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(chronic_kidney_disease == year) %>% 
  count(year,run,wt = chronic_kidney_disease_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(lung_cancer == year) %>% 
  count(year,run,wt = lung_cancer_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(colorectal_cancer == year) %>% 
  count(year,run,wt = colorectal_cancer_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(osteoarthritis == year) %>% 
  count(year,run,wt = osteoarthritis_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) 
  

past_populations %>%
  filter(epilepsy == year) %>% 
  count(year,run,wt = epilepsy_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(rheumatoid_arthritis == year) %>% 
  count(year,run,wt = rheumatoid_arthritis_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(copd == year) %>% 
  count(year,run,wt = copd_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor) 
  

past_populations %>%
  filter(asthma == year) %>% 
  count(year,run,wt = asthma_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(non_diabetic_hyperglycaemia == year) %>% 
  count(year,run,wt = non_diabetic_hyperglycaemia_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)
  

past_populations %>%
  filter(osteoporosis == year) %>% 
  count(year,run,wt = osteoporosis_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(hypothyroidism == year) %>% 
  count(year,run,wt = hypothyroidism_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(pad == year) %>% 
  count(year,run,wt = pad_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)

past_populations %>%
  filter(low_birth_weight == year) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)*model_specification$population$scale_down_factor)



