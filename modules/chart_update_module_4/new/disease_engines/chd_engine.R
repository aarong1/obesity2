#https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002602

past_populations <- past_populations %>% 
  apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell)

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell)

chd_incidence_per100k <- 
  tribble(
    ~'age', ~'Males', ~'Females',
    '30-54', 88.1, 21.2,
    '55-64', 317.0, 90.3,  
    '65-74', 533.0, 237.0,  
    '75-84', 1017.0, 597.0,  
    '85-110', 1987.0, 1395.0, 
  )

bmi_chd_rr <- 
  tribble(
    ~age, ~RR, ~CI,
"0-19",  "1", "(1.259 to 3.683)"  ,
"20-24",  "2.274", "(1.259 to 3.683)"  ,
"25-29",  "2.274", "(1.259 to 3.683)"  ,
"30-34",  "2.018", "(1.3 to 3.099)"  ,
"35-39",  "1.724", "(1.533 to 1.93)"  ,
"40-44",  "1.599", "(1.418 to 1.784)"  ,
"45-49",  "1.567", "(1.458 to 1.68)"  ,
"50-54",  "1.52", "(1.417 to 1.631)"  ,
"55-59",  "1.466", "(1.372 to 1.557)"  ,
"60-64",  "1.414", "(1.325 to 1.504)"  ,
"65-69",  "1.364", "(1.287 to 1.448)"  ,
"70-74",  "1.319", "(1.242 to 1.4)"  ,
"75-79",  "1.274", "(1.187 to 1.365)"  ,
"80-84",  "1.17", "(1.091 to 1.252)"  ,
"85-89",  "1.17", "(1.091 to 1.252)"  ,
"90-94",  "1.17", "(1.091 to 1.252)"  ,
"95-110",  "1.17", "(1.091 to 1.252)"
)

pm25_chd_rr <- 1.41

# our convention for labelling CHD years is the
# lowest year represents prevalence at our start date
# 0 represents no CHD
# any year there after is incident CHD at that year

past_populations %>%
  filter(year == min(year) + 1) %>% 
  count(chd)

# chd     n
# 1    0 56993
# 2 2023  2236
# 3 2024   147




model_specification$population$scale_down_factor
#10
model_specification$model$number_of_runs
#4

library(data.table)

  inc_dt <- as.data.table(chd_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Function 1: Apply risk based on age and gender alone
apply_chd_risk_base <- function(input_population) {
  # Accept Data.Table
  dt <- as.data.table(input_population)
  
  # Prepare incidence table
  # Age groups in chd_incidence_per100k: '30-54', '55-64', '65-74', '75-84', '85-110'
  # We assume 0 incidence for age < 30

  
  # Create mapping for age groups
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 29, 54, 64, 74, 84, 110),
                            labels = c("0-29", "30-54", "55-64", "65-74", "75-84", "85-110"),
                            right = TRUE)]
  
  # Join incidence
  dt[inc_dt, on = .(age_group_inc = age, sex), chd_year_risk := i.incidence / 100000]
  
  # Fill NA with 0 (for age 0-29 etc)
  dt[is.na(chd_year_risk), chd_year_risk := 0]
  
  # Cleanup
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_chd_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Filter for population at risk (exclude prevalent CHD cases) if 'chd' column exists
  # Prevalent cases (chd != 0) cannot become incident cases, so exclude from PAF calculation
  if ("chd" %in% names(dt)) {
    dt <- dt[chd == 0]
  }
  
  # 1. Map Age to BMI RR groups
  # Groups: "0-19", "20-24", "25-29", "30-34", "35-39", ... "95-110"
  # Breaks for: 0-19 (0-19), 20-24 (20-24)...
  # Common pattern: 5 year groups starting at 20.
  breaks_bmi <- c(0, 20, seq(25, 95, by = 5), 111)
  labels_bmi <- c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                  "75-79", "80-84", "85-89", "90-94", "95-110")
  
  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
  
  # 2. Get BMI RR Base from table
  rr_bmi_dt <- as.data.table(bmi_chd_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  
  # Join RR base
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
  
  # 3. Calculate Individual BMI RR
  # Map BMI category to value: normal->22, overweight->30, obese->37
  # Formula: RR_base ^ ((bmi_val - 22)/5)

  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 30,
    bmi == "obese", 37,
    default = 20
  )]
  
  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1] # Handle missing RRs or age groups
  
  # 4. Calculate PM2.5 RR
  # RR = 1.41 ^ (pm25g / 10)
  dt[, RR_pm25_indiv := pm25_chd_rr^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  # 5. Combined RR
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
  
  # 6. Calculate AF per Incidence Age Group and Sex
  # Must use incidence age groups to match incidence table logic
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 29, 54, 64, 74, 84, 110),
                            labels = c("0-29", "30-54", "55-64", "65-74", "75-84", "85-110"),
                            right = TRUE)]
  
  # AF = 1 - N / sum(RR_combined)
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  # 7. Apply AF to Incidence to get Min Probability
  inc_dt <- as.data.table(chd_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Join AF and Incidence
  # Note: 0-29 will not be in inc_dt, so they are dropped or have no risk
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, chd_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, chd_prob_min)])
}

chd_theoretical_min_table <- calculate_chd_theoretical_min(current_population)

# Function 3: Apply Risk using Factors and Theoretical Minimum
apply_chd_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. Recalculate RRs (Logic duplicated from calc function - could be helper)
  # Map Age to BMI RR groups
  breaks_bmi <- c(0, 20, seq(25, 95, by = 5), 111)
  labels_bmi <- c("0-19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", 
                  "75-79", "80-84", "85-89", "90-94", "95-110")
  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
  
  # 2. Get BMI RR Base and PM2.5 RR
  rr_bmi_dt <- as.data.table(bmi_chd_rr)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
  
  dt[, bmi_val := fcase(
    bmi == "normal", 20,
    bmi == "overweight", 30,
    bmi == "obese", 37,
    default = 20
  )]

  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  dt[, RR_pm25_indiv := pm25_chd_rr^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv]
  
  # 3. Join Minimum Risk
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 29, 54, 64, 74, 84, 110),
                            labels = c("0-29", "30-54", "55-64", "65-74", "75-84", "85-110"),
                            right = TRUE)]
  print(names(min_dt))
    print(names(dt))

  dt[min_dt, on = .(age_group_inc, sex), chd_prob_min := i.chd_prob_min]
  dt[is.na(chd_prob_min), chd_prob_min := 0]
  
  # 4. Calculate Final Risk
  dt[, chd_year_risk := chd_prob_min * RR_combined]
  
  # Cleanup
  dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
         "RR_pm25_indiv", "RR_combined", "age_group_inc", "chd_prob_min") := NULL]
  
  return(dt)
}

# p %>%
#   filter(year==min(year)) %>% 
#   select(-chd_year_risk) %>% 
#     apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) #%>% 
  # apply_chd_risk_base()


# theoretical_min_table <- calculate_chd_theoretical_min(p)
# 
#  p <- apply_chd_risk_factors(p, theoretical_min_table)
#  
#  p %>% 
#   select(-chd_year_risk) %>% 
#  mutate(bmi=fifelse(bmi=='normal','normal',
#          fifelse(bmi=='overweight','normal',
#          fifelse(bmi=='obese','normal',NA)))) %>%
#          apply_chd_risk_factors(theoretical_min_table) %>% 
#  pull(chd_year_risk) %>% 
#  sum()
# 
# p[,c("chd_year_risk"                        ,
# "age_group_bmi"                        ,
# "RR_bmi_base"                          ,
# "bmi_val"                              ,
# "RR_bmi_indiv"                         ,
# "RR_pm25_indiv"                        ,
# "RR_combined"                          ,
# "age_group_inc"   ,                     
# "chd_prob_min"    )]
