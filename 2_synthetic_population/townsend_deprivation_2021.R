# townsend_deprivation_2021.R 

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

# HSCT
# tenure <- read_csv("data/ni-census21-household-health_trust+hh_tenure_agg4-9fdb960a.csv")
# rooms_per_person <- read_csv("data/ni-census21-household-health_trust+hh_persons_per_room-22301dcb.csv")
# car <- read_csv("data/ni-census21-household-health_trust+hh_car_van_ind-cdaf497b.csv")
# economic_activity <- read_csv("data/ni-census21-people-health_trust+economic_activity_agg4-85aa051a.csv")



# DATA ZONE
tenure <- read_csv("data/deprivation_dz/ni-census21-household-dz21+hh_tenure_agg4-cb920b3e.csv")
rooms_per_person <- read_csv("data/deprivation_dz/ni-census21-household-dz21+hh_persons_per_room-a44dc9c7.csv")
car <- read_csv("data/deprivation_dz/ni-census21-household-dz21+hh_car_van_ind-1768b967.csv")
economic_activity <- read_csv("data/deprivation_dz/ni-census21-people-dz21+economic_activity_agg4-129f1742.csv")


# ---- 1) ECONOMIC: unemployment rate among economically active ----
econ <- economic_activity %>%
  # keep just what we need
  select(
    area_code  = 1,
    area_label = 2,
    act_code    = `Economic Activity - 4 Categories Code`,
    act_label   = `Economic Activity - 4 Categories Label`,
    n           = Count
  ) %>%
  # drop "No code required" rows from any totals
  filter(act_label != "No code required") %>%
  mutate(
    employed   = if_else(act_code == 1, n, 0),
    unemployed = if_else(act_code == 2, n, 0),
    inactive   = if_else(act_code == 3, n, 0)
  ) %>%
  group_by(area_code, area_label) %>%
  summarise(
    employed   = sum(employed),
    unemployed = sum(unemployed),
    inactive   = sum(inactive),
    .groups = "drop"
  ) %>%
  mutate(
    econ_active = employed + unemployed,
    comp_unemp_prop = if_else(econ_active > 0, unemployed / econ_active, NA_real_) # component 1
  )

# ---- 2) CAR: % households with NO car/van ----
car <- car %>%
  select(
    area_code  = 1,
    area_label = 2,
    car_code    = `Car or Van Availability - 2 Categories Code`,
    car_label   = `Car or Van Availability - 2 Categories Label`,
    n           = Count
  ) %>%
  mutate(
    no_car = if_else(str_detect(car_label, "^No cars or vans available$"), n, 0)
  ) %>%
  group_by(area_code, area_label) %>%
  summarise(
    hh_total = sum(n),
    hh_no_car = sum(no_car),
    comp_no_car_prop = if_else(hh_total > 0, hh_no_car / hh_total, NA_real_), # component 2
    .groups = "drop"
  )

# ---- 3) TENURE: % households NOT owner-occupied ----
tenure <- tenure %>%
  select(
    area_code  = 1,
    area_label = 2,
    ten_code    = `Tenure - 4 Categories Code`,
    ten_label   = `Tenure - 4 Categories Label`,
    n           = Count
  ) %>%
  mutate(
    is_owner = ten_code == 1
  ) %>%
  group_by(area_code, area_label) %>%
  summarise(
    hh_total = sum(n),
    hh_owner = sum(n[is_owner]),
    comp_non_owner_prop = if_else(hh_total > 0, (hh_total - hh_owner) / hh_total, NA_real_), # component 3
    .groups = "drop"
  )

# ---- 4) ROOMS: % households overcrowded (>1 person per room) ----
rooms <- rooms_per_person %>%
  select(
    area_code  = 1,
    area_label = 2,
    room_code   = `Rooms (Persons per Room) Code`,
    room_label  = `Rooms (Persons per Room) Label`,
    n           = Count
  ) %>%
  mutate(
    is_overcrowded = room_code >= 3  # code 3: >1.0 to 1.5; code 4: >1.5 persons/room
  ) %>%
  group_by(area_code, area_label) %>%
  summarise(
    hh_total = sum(n),
    hh_over  = sum(n[is_overcrowded]),
    comp_overcrowd_prop = if_else(hh_total > 0, hh_over / hh_total, NA_real_), # component 4
    .groups = "drop"
  )

# ---- 5) Assemble components ----
townsend_components <- econ %>%
  select(area_code, area_label, comp_unemp_prop) %>%
  left_join(car %>% select(area_code, comp_no_car_prop), by = "area_code") %>%
  left_join(tenure %>% select(area_code, comp_non_owner_prop), by = "area_code") %>%
  left_join(rooms %>% select(area_code, comp_overcrowd_prop), by = "area_code") %>%
  relocate(area_label, .after = area_code)

townsend_components <- townsend_components |> 
  mutate(
    comp_unemp_prop = comp_unemp_prop * 100,
    comp_no_car_prop = comp_no_car_prop * 100,
    comp_non_owner_prop = comp_non_owner_prop * 100,
    comp_overcrowd_prop = comp_overcrowd_prop * 100
  )
    
  

# ---- 5.5) log transform overcrowding and unemployed ----
townsend_components <- townsend_components %>%
  mutate(log_comp_unemp_prop = log(comp_unemp_prop + 1), 
         log_comp_overcrowd_prop = log(comp_overcrowd_prop + 1)
         )

# ---- 6) Z-score each component across Trusts and sum ----
zscore <- function(x) as.numeric(scale(x))  # returns centered+scaled vector

townsend_index <- townsend_components %>%
  mutate(
    z_unemp       = zscore(log_comp_unemp_prop),
    z_no_car      = zscore(comp_no_car_prop),
    z_non_owner   = zscore(comp_non_owner_prop),
    z_overcrowd   = zscore(log_comp_overcrowd_prop),
    townsend_sum  = z_unemp + z_no_car + z_non_owner + z_overcrowd
  ) %>%
  arrange(desc(townsend_sum)) %>%
  mutate(
    townsend_rank = row_number() # 1 = most deprived
  )

# ---- 7) Nice printout of components & final index ----
townsend_index %>%
  transmute(
    `area_code`  = area_code,
    `area_label`       = area_label,
    `Unemployed (prop)`   = log_comp_unemp_prop,
    `No car (prop)`       = comp_no_car_prop,
    `Non-owner (prop)`    = comp_non_owner_prop,
    `Overcrowded (prop)`  = log_comp_overcrowd_prop,
    `z_unemp`        = z_unemp,
    `z_no_car`       = z_no_car,
    `z_non_owner`    = z_non_owner,
    `z_overcrowd`    = z_overcrowd,
    `Townsend score` = townsend_sum,
    `Rank (1=most deprived)` = townsend_rank
  )

write_fst(townsend_index, './synthetic_population/data_zone_townsend_deprivation.fst')
