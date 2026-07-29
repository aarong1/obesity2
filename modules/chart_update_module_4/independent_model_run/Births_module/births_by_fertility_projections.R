#live births 
# past_population <- read.csv('./past_populations/past_populations_sppg_undefined.csv')

# current_population <- past_populations |> 
#   filter(min(year) == year )

# source('./births_module/births.R')
ni_sya_fertility <- read_excel("data/registrar_general_annual_reports/Section 3 - Births_Tables_2023.xlsx", 
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
# 
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

