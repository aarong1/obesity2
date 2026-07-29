library(mgcv) 


#live births 
# past_population <- read.csv('./past_populations/past_populations_sppg_undefined.csv')

# current_population <- past_populations |> 
#   filter(min(year) == year )

# source('./births_module/births.R')
ni_sya_fertility <- read_excel("../data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx", 
                               sheet = "Table 3.14", skip = 3)




ni_sya_fertility <- ni_sya_fertility |> 
  
  pivot_longer(-1,,names_to = 'Year',values_to='ni_fertility_rate_per1k' ) |> 
  rename(c(mother_age = 'Age of mother')) |> 
  filter(!is.na(mother_age)) |> 
  mutate(mother_age = stringr::str_extract(mother_age, "\\d+"))  |> 
  mutate(across(everything(), as.numeric))

# pop <- read.fst('./synthetic_population/pop.fst')

# 
# current_population <- pop |>
#   filter(min(year) == year )
# 
# # pregnancy lottery -----
# 
# current_population


ni_sya_fertility <- ni_sya_fertility |> 
  filter(!is.na(mother_age))

fits <- ni_sya_fertility |> 
  # filter(mother_age%%5==0) %>%
  group_by(mother_age) %>%
  group_map(~ gam(ni_fertility_rate_per1k ~ s(Year, k = 25), data = .x))

# Create future years
future_years <- data.frame(Year = 2024:2060)


uture_years <- data.frame(Year = 2024:2060)

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

fertility_2023 <-
  ni_sya_fertility |>
  filter(Year %in% 2020:2023) |>
  mutate(sex = 'Females',asfr = ni_fertility_rate_per1k/1000) |>
  select(age=mother_age, year = Year, asfr, sex)

  fertility <- proj |>
    mutate(sex = 'Females',asfr = fit1/1000) |>
  select(age=mother_age, year = Year, asfr, sex)

  fertility <- rbind(fertility,
                     fertility_2023)
# 
# # current_population <- select(current_population, -asfr)
# 
# current_population <- current_population |> 
#       left_join(#relationship = 'many-to-one',
#         fertility
#       )
# 
# childbearing_population <- current_population |>
#   filter(!is.na(asfr))
# 
# mothers <- childbearing_population |> 
#   rowwise() |> 
#   mutate(bern_trial = runif(1)) |> 
#   mutate(birth = bern_trial<asfr) |> 
#   filter(birth) #|> 
#   # select(asfr,pregnant,birth) |>
#     # view()
# 
# newborn_population <- mothers |> 
#   mutate(
#   #dob=year,
#   mothers_age = age,
#   mothers_id = id,
#   age = 0,
#   id = as.numeric(paste0(year, id, '111',row_number()))
#   )
# 
# current_population <- bind_rows(
#   current_population,
# newborn_population) |> 
#   select(-c(birth, asfr))



asfr_births <- function(current_population,fertility){
  
  current_population <- current_population |> 
    left_join(#relationship = 'many-to-one',
      fertility
    )
  
  childbearing_population <- current_population |>
    filter(!is.na(asfr))
  
  mothers <- childbearing_population |> 
    rowwise() |> 
    mutate(bern_trial = runif(1)) |> 
    mutate(birth = bern_trial<asfr) |> 
    filter(birth) #|> 
  # select(asfr,pregnant,birth) |>
  # view()
  
  newborn_population <- mothers |> 
    mutate(
      #dob=year,
      mothers_age = age,
      mothers_id = id,
      age = 0,
      id = as.numeric(paste0(year, id, '111',row_number()))
    )
  
  current_population <- bind_rows(
    current_population,
    newborn_population) |> 
    select(-c(birth, asfr))
  
  current_population
}

