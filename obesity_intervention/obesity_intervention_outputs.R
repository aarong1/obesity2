past_populations <- past_populations |> 
  mutate(intervention = ifelse(
    run < ( model_specification$model$number_of_runs/2 + 1 ),
         'non-intervention',
         'intervene'
    ))


#Stroke ----
past_populations |> 
  filter(year != 2018) |> 
  group_by(year,run,intervention) |> 
  summarise(inc_stroke = sum(stroke == year)) |> 
  group_by(year,intervention,.drop =T) |> 
  summarise(inc_stroke = mean(inc_stroke) * 1900  ) |> 
  ggplot(aes(x = year, y = inc_stroke, color = as.character(intervention))) +
  geom_line() 


#Diabetes ----
past_populations |> 
  filter(year != 2018) |> 
  group_by(year,run,intervention) |> 
  summarise(inc_diabetes = sum(diabetes == year,na.rm = T)) |> 
  group_by(year,intervention,.drop =T) |> 
  summarise(inc_diabetes = mean(inc_diabetes) * 1900  ) |> 
  
  ggplot(aes(x = year, y = inc_diabetes, color = as.character(intervention))) +
  geom_line() 

#Heart Failure ---
past_populations |> 
  filter(year != 2018) |> 
  group_by(year,run,intervention) |> 
  summarise(inc_heart_failure = sum(heart_failure == year,na.rm = T)) |> 
  group_by(year,intervention,.drop =T) |> 
  summarise(inc_heart_failure = mean(inc_heart_failure) * 1900  ) |> 
  
  ggplot(aes(x = year, y = inc_heart_failure, color = as.character(intervention))) +
  geom_line() 

# %>%
#Chronic kidney disease ---
  past_populations |> 
  filter(year != 2018) |> 
  group_by(year,run,intervention) |> 
  summarise(inc_ckd = sum(chronic_kidney_disease == year,na.rm = T)) |> 
  group_by(year,intervention,.drop =T) |> 
  summarise(inc_ckd = mean(inc_ckd) * 1900  ) |> 
  
  ggplot(aes(x = year, y = inc_ckd, color = as.character(intervention))) +
  geom_line() 
  
  