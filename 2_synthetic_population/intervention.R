# intervention



intervention_df <- data.frame(
  year=
c(2015,
2016,
2017,
2018,
2019,
2020,
2021,
2022,
2023,
2024,
2025),
power =
c( 1.0000000,
   1.0000000,
   0.4823529,
   0.4823529,
   0.4823529,
   0.4823529,
   0.4941176,
   0.5952941,
   0.6964706,
   0.7976471,
   0.8988235)
)

intervention_configuration = list(
  flag = T,
  target = list( 
    age = 40:50,
    sex = 'Males',
    ethnicity = NULL,
    deprivation = c(-5,-1)#,
  # diabetes = T,
  # high_cholesterol = T,
  # high_blood_pressure = T,
  # smoking = T,
  # diet = T,
  # activity = T,
  # 
  )
)

target_default = list( 
  age = 40:100,
  sex = c('Males','Females'),
  ethnicity = c("white",
                "arab",
                "mixed",
                "indian",
                "black african",
                "other asian",
                "chinese", 
                "roma", 
                "other", 
                "irish traveller",
                "pakistani",
                "filipino"),       
  townsend = c(-4.925793,  9.191205))

intervention_configuration$target
  
# initial_time_zero_population  %>% 
#   filter(year == max(year,na.rm = TRUE)) %>% 
#   pivot_longer(cols = - any_of(base_population_demographic_column_names)) %>% 
#   mutate(category = str_extract(string = name,pattern = 'cholesterol|overweight|bp|smoking|diabetic|atrial_fibrillation')) %>% 
#   group_by(id,category) %>% 
#   arrange(desc(value)) %>% 
#   slice_head() %>%
#   ungroup() %>% 
#   pivot_wider(id_cols = -value,names_from = category,values_from = name) 

initial_time_zero_population |> 
  mutate(intervened = 
           age %in% intervention_configuration$target$age
         &
           sex %in% intervention_configuration$target$sex
         &
           ethnicity %in% intervention_configuration$target$ethnicity
         &
           (
             townsend_score > min(intervention_configuration$target$ethnicity) 
            &
              townsend_score < max(intervention_configuration$target$ethnicity)
           )
         )

