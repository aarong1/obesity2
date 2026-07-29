make_sle_age_group <- function(age){
  cut(
    age,
    breaks = c(-Inf,18,40,55,70,85, Inf),
    labels = c("<18","18-39","40-54","55-69","70-84",">=85"),
    right = FALSE
  )
}

sle_incidence_per100k <-  tibble::tribble(
  ~`Year`, ~"18-39_Females",~"40-54_Females",~"55-69_Females",~"70-84_Females",~">=85_Females", ~"Total_Females", ~"18-39_Males",~"40-54_Males",~"55-69_Males",~"70-84_Males",~">=85_Males", ~"Total_Males",
  "2017", 9.10, 6.68, 7.28, 7.88, 4.63, 7.72, 1.44, 2.54, 3.09, 2.03, 2.78, 2.22, 
  "2018", 6.14, 9.43, 5.23, 9.88, 3.27, 7.20, 0.76, 0.49, 3.80, 2.07, 0.00, 1.53, 
  "2019", 5.59, 8.57, 8.63, 4.58, 1.68, 6.72, 0.59, 1.55, 1.79, 1.57, 2.91, 1.31
)

sle_incidence_rates <-  sle_incidence_per100k %>% 
  summarise(across(where(is.numeric), ~ mean(.x) / 100000) )

library(dplyr)
library(tidyr)
library(stringr)

sle_incidence_lookup <- sle_incidence_per100k %>%
  pivot_longer(
    -Year,
    names_to = "age_sex",
    values_to = "rate_per_100k"
  ) %>%
  filter(!str_detect(age_sex, "^Total")) %>%   # drop total cols
  separate(age_sex, into = c("age_group","sex"), sep = "_") %>%
  mutate(
    sex = recode(sex,
                 "Females" = "Females",
                 "Males"   = "Males"),
    sle_year_risk = rate_per_100k / 100000
  ) %>%
  group_by(sex, age_group) %>%
  summarise(sle_year_risk = mean(sle_year_risk), .groups="drop")


apply_sle_risk <- function(current_population){
  
  current_population %>%
    select(-any_of("sle_year_risk")) %>%
    mutate(
      age_group = make_sle_age_group(age)
    ) %>%
    left_join(
      sle_incidence_lookup,
      by = c("sex","age_group")
    ) %>%
    replace_na(list(sle_year_risk = 0))
}