# https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1002602

library(tibble)
library(dplyr)
library(data.table)


# ============================================================================
# STROKE RISK ENGINE
# ============================================================================

# Data Definitions ----

# Stroke Incidence Data (per 100,000)
# stroke_incidence_per100k <- tribble(
#   ~age, ~Males, ~Females,
#   '0-44', 110.0, 110.0,
#   '45-54', 890.0, 790.0, 
#   '55-64', 2690.0, 1960.0,
#   '65-74', 6400.0, 4390.0, 
#   '75-110', 14890.0, 12430.0
# )


stroke_incidence_per100k <- 
  tribble(
    ~age, ~Males, ~Females,
    '0-44', 7.0, 6.0,
    '45-64', 114.0, 69.0,
    '65-74', 393.0, 275.0,
    '75-110', 794.0, 879.0,
  )

# Risk Factor Relative Risks for Stroke ----

# BMI - Relative Risk by Age Group
rr_stroke_bmi <- tribble(
  ~age, ~RR,
  "0-24", 1.0,
  "25-29", 2.472,
  "30-34", 2.472,
  "35-39", 2.235,
  "40-44", 1.979,
  "45-49", 1.826,
  "50-54", 1.733,
  "55-59", 1.635,
  "60-64", 1.543,
  "65-69", 1.455,
  "70-74", 1.38,
  "75-79", 1.304,
  "80-84", 1.228,
  "85-89", 1.068,
  "90-94", 1.068,
  "95-110", 1.068
)

# Smoking - Relative Risk
rr_stroke_smoking <- tibble::tribble(
             ~HR, ~smoking,
             "1.00", "never_smoked",
             "1.13",  "former",
             "1.34",  "current_smoker"
  )

# Alcohol - Relative Risk
rr_stroke_alcohol <- tibble::tribble(
  ~alcohol, ~HR,
    "no_risk", 1, #                "Never", 1,
                      #            "Former", 1.27,
    "lower_risk", 0.86, #       "Current Low", 0.86,
    "increased_risk", 0.98, # "Current Moderate", 0.98,
    "higher_risk", 0.91 #      "Current High", 0.91
  )

# Physical Activity - Relative Risk
rr_stroke_physical_activity <- tibble::tribble(
  ~activity, ~RR,
  "meets_rec", 1,
  "low_activity", 1.06,
  "some_activity", 1.06,
  "inactive", 1.12
)

# Hypertension - Relative Risk
rr_stroke_hypertension <- tibble::tribble(
  ~hypertension, ~RR,
  # "No", 1,
  # "Yes", 2.35
  'hypertensive_controlled', 1,
  'hypertensive_uncontrolled', 2.35,
  'hypertensive_untreated', 2.35,
  'normotensive_untreated', 1
)

# Diabetes - Relative Risk
rr_stroke_diabetes <- tibble::tribble(
  ~diabetes, ~RR,
  "no_diabetes", 1,
  "undiagnosed_diabetes", 1.52,
  "diagnosed_diabetes", 1.52
  
)

# Cholesterol - Relative Risk
# rr_stroke_cholesterol <- tibble::tribble(
#   ~cholesterol_level, ~RR,
#   "≤3.2", 1,
#   "3.2–4.0", 1.07,
#   ">4.0", 1.14
# )

rr_stroke_cholesterol <- tibble::tribble(
  ~cholesterol_level, ~RR,
  "normal_cholesterol", 1,
  "raised_cholesterol", 1.07,
  "hdl/cholesterol", 1.14
)

# Depression - Relative Risk
# rr_stroke_depression <- tibble::tribble(
#   ~depression, ~RR,
#   "No", 1,
#   "Yes", 1.08
# )

# PM2.5 - Relative Risk per 10 μg/m³
rr_stroke_pm25 <- 1.13

# Convert incidence to long format for easy merging
inc_dt <- as.data.table(stroke_incidence_per100k) %>%
  melt(id.vars = "age", variable.name = "sex", value.name = "incidence")

# Functions ----

# Function 1: Apply risk based on age and sex alone
apply_stroke_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Incidence groups: '0-44', '45-54', '55-64', '65-74', '75-110'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 65, 75, 110),
                            labels = c("0-44", "45-64", "65-74", "75-110"),
                            right = FALSE)]
  
  inc_dt_temp <- as.data.table(stroke_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
  dt[inc_dt_temp, on = .(age_group_inc = age, sex), stroke_year_risk := i.incidence / 100000]
  dt[is.na(stroke_year_risk), stroke_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Function 2: Calculate PAF and Theoretical Minimum
calculate_stroke_theoretical_min <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Exclude prevalent cases
  if ("stroke" %in% names(dt)) {
    dt <- dt[stroke == 0]
  }
  
  # 1. BMI RR - Map Age to BMI RR groups
  breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
  labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", 
                  "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")

  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]

  rr_bmi_dt <- as.data.table(rr_stroke_bmi)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]

  dt[, bmi_val := fcase(
    bmi == "normal", 22,
    bmi == "overweight", 30,
    bmi == "obese", 37,
    default = 22
  )]

  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]

  # 2. PM2.5 RR
  dt[, RR_pm25_indiv := rr_stroke_pm25^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  # 3. Smoking RR
  rr_smoking_dt <- as.data.table(rr_stroke_smoking)[, .(smoking, RR_smoking = as.numeric(HR))]
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR_smoking]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 4. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_stroke_alcohol)[, .(alcohol, RR_alcohol = as.numeric(HR))]
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR_alcohol]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 5. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_stroke_physical_activity)
  dt[rr_pa_dt, on = .(pa = activity), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 6. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_stroke_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status=hypertension), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 7. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_stroke_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status = diabetes), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # 8. Cholesterol RR
  rr_cholesterol_dt <- as.data.table(rr_stroke_cholesterol)
  dt[rr_cholesterol_dt, on = .(cholesterol_status = cholesterol_level), RR_cholesterol_indiv := i.RR]
  dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
  
  # 9. Depression RR
  # rr_depression_dt <- as.data.table(rr_stroke_depression)
  # dt[rr_depression_dt, on = .(depression), RR_depression_indiv := i.RR]
  # dt[is.na(RR_depression_indiv), RR_depression_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * 
                      RR_alcohol_indiv * RR_pa_indiv * RR_hypertension_indiv * 
                      RR_diabetes_indiv * RR_cholesterol_indiv 
     # * RR_depression_indiv
     ]
  
  # PAF grouping
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45,65, 75, 110),
                            labels = c("0-44", "45-64", "65-74", "75-110"),
                            right = FALSE)]
                            
  paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age_group_inc, sex)]
  
  inc_dt <- as.data.table(stroke_incidence_per100k) %>%
    melt(id.vars = "age", variable.name = "sex", value.name = "incidence")
    
  min_dt <- merge(as.data.table(inc_dt), paf_dt, by.x = c("age", "sex"), by.y = c("age_group_inc", "sex"))
  
  min_dt[, stroke_prob_min := (incidence / 100000) * (1 - AF)]
  
  return(min_dt[, .(age_group_inc = age, sex, stroke_prob_min)])
}


# Function 3: Apply Risk Factors
apply_stroke_risk_factors <- function(input_population, theoretical_min_table) {
  dt <- as.data.table(input_population)
  min_dt <- as.data.table(theoretical_min_table)
  
  # 1. BMI RR
  breaks_bmi <- c(0, 25, seq(30, 95, by = 5), 111)
  labels_bmi <- c("0-24", "25-29", "30-34", "35-39", "40-44", 
                  "45-49", "50-54", "55-59", "60-64", "65-69", 
                  "70-74", "75-79", "80-84", "85-89", "90-94", "95-110")
  
  dt[, age_group_bmi := cut(age, breaks = breaks_bmi, labels = labels_bmi, right = FALSE)]
  
  rr_bmi_dt <- as.data.table(rr_stroke_bmi)[, .(age_group_bmi = age, RR_base = as.numeric(RR))]
  dt[rr_bmi_dt, on = .(age_group_bmi), RR_bmi_base := i.RR_base]
  
  dt[, bmi_val := fcase(
    bmi == "normal", 22,
    bmi == "overweight", 30,
    bmi == "obese", 37,
    default = 22
  )]
  
  dt[, RR_bmi_indiv := RR_bmi_base^((bmi_val - 20) / 5)]
  dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
  
  # 2. PM2.5 RR
  dt[, RR_pm25_indiv := rr_stroke_pm25^(pm25g / 10)]
  dt[is.na(RR_pm25_indiv), RR_pm25_indiv := 1]
  
  # 3. Smoking RR
  rr_smoking_dt <- as.data.table(rr_stroke_smoking)[, .(smoking, RR_smoking = as.numeric(HR))]
  dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR_smoking]
  dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
  
  # 4. Alcohol RR
  rr_alcohol_dt <- as.data.table(rr_stroke_alcohol)[, .(alcohol, RR_alcohol = as.numeric(HR))]
  dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR_alcohol]
  dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
  
  # 5. Physical Activity RR
  rr_pa_dt <- as.data.table(rr_stroke_physical_activity)
  dt[rr_pa_dt, on = .(pa = activity), RR_pa_indiv := i.RR]
  dt[is.na(RR_pa_indiv), RR_pa_indiv := 1]
  
  # 6. Hypertension RR
  rr_hypertension_dt <- as.data.table(rr_stroke_hypertension)
  dt[rr_hypertension_dt, on = .(hypertension_status = hypertension), RR_hypertension_indiv := i.RR]
  dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
  
  # 7. Diabetes RR
  rr_diabetes_dt <- as.data.table(rr_stroke_diabetes)
  dt[rr_diabetes_dt, on = .(diabetes_status = diabetes), RR_diabetes_indiv := i.RR]
  dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
  
  # 8. Cholesterol RR
  rr_cholesterol_dt <- as.data.table(rr_stroke_cholesterol)
  dt[rr_cholesterol_dt, on = .(cholesterol_status = cholesterol_level), RR_cholesterol_indiv := i.RR]
  dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
  
  # 9. Depression RR
  # rr_depression_dt <- as.data.table(rr_stroke_depression)
  # dt[rr_depression_dt, on = .(depression), RR_depression_indiv := i.RR]
  # dt[is.na(RR_depression_indiv), RR_depression_indiv := 1]
  
  # Combine all RRs
  dt[, RR_combined := RR_bmi_indiv * RR_pm25_indiv * RR_smoking_indiv * 
                      RR_alcohol_indiv * RR_pa_indiv * RR_hypertension_indiv * 
                      RR_diabetes_indiv * RR_cholesterol_indiv 
     #* RR_depression_indiv
     ]
  
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 45, 55, 65, 75, 110),
                            labels = c("0-44", "45-54", "55-64", "65-74", "75-110"),
                            right = FALSE)]
                            
  dt[min_dt, on = .(age_group_inc, sex), stroke_prob_min := i.stroke_prob_min]
  dt[is.na(stroke_prob_min), stroke_prob_min := 0]
  
  dt[, stroke_year_risk := stroke_prob_min * RR_combined]
  
  # Clean up temporary columns
  dt[, c("age_group_bmi", "RR_bmi_base", "bmi_val", "RR_bmi_indiv", 
         "RR_pm25_indiv", "RR_smoking_indiv", "RR_alcohol_indiv", "RR_pa_indiv",
         "RR_hypertension_indiv", "RR_diabetes_indiv", "RR_cholesterol_indiv", 
         "RR_combined", "age_group_inc", "stroke_prob_min") := NULL]
         
  return(dt)
}


store_unit_tests <- function(){
 
  x <- past_populations %>% 
    filter(year ==min(year))
  
  t = calculate_stroke_theoretical_min(x)
  stroke_theoretical_min_table <- calculate_stroke_theoretical_min(current_population)
  y <- apply_stroke_risk_factors(x,theoretical_min_table = t)
  
  y <- y %>% mutate(age1=cut(age, 
                             breaks = c(-Inf, 45, 55, 65, 75, 110),
                             labels = c("0-44", "45-54", "55-64", "65-74", "75-110"),
                             right = FALSE))
  
  y %>% 
    group_by(age1, sex) %>% 
    summarise(n=n(), wt=sum(stroke_year_risk)) %>% 
    mutate(stroke_year_risk_per100k = wt/n*100000)
  
  input_population = past_populations %>% 
    filter(year ==min(year))
  
}
