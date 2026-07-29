# Type 1 Diabetes Incidence Engine
# Source: Based on population incidence data

# ============================================================================
# TYPE 1 DIABETES RISK ENGINE
# ============================================================================

library(tibble)
library(dplyr)
library(data.table)

# Data Definitions ----

# Type 1 Diabetes Incidence Data (per 100,000 per year)
# Note: Type 1 diabetes is primarily autoimmune and not strongly associated 
# with modifiable risk factors, so age-based risk is appropriate
type1_diabetes_incidence_per100k <- tribble(
  ~age, ~Males, ~Females,
  "0-14", 31.69, 31.69,     
  "15-24", 26.89, 26.89,    
  "25-34", 29.88, 29.88,    
  "35-44", 31.20, 31.20,    
  "45-54", 23.96, 23.96,    
  "55-64", 29.86, 29.86,    
  "65-74", 41.23, 41.23,    
  "75-110", 54.15, 54.15    
)

# Functions ----

# Function: Apply risk based on age and sex alone
apply_type1_diabetes_risk_engine_age_sex <- function(input_population) {
  dt <- as.data.table(input_population)
  
  # Convert incidence to long format for easy merging
  inc_dt <- as.data.table(type1_diabetes_incidence_per100k)
  inc_dt <- melt(inc_dt, id.vars = "age", variable.name = "sex", value.name = "incidence")
  
  # Age groups: '0-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-110'
  dt[, age_group_inc := cut(age, 
                            breaks = c(-Inf, 15, 25, 35, 45, 55, 65, 75, Inf),
                            labels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-110"),
                            right = FALSE)]
  
  dt[inc_dt, on = .(age_group_inc = age, sex), type1_diabetes_year_risk := i.incidence / 100000]
  dt[is.na(type1_diabetes_year_risk), type1_diabetes_year_risk := 0]
  
  dt[, age_group_inc := NULL]
  
  return(dt)
}

# Example usage stored for testing
store_unit_tests <- function(){
  
  x <- past_populations %>% 
    filter(year == min(year))
  
  # Test age/sex only
  y <- apply_type1_diabetes_risk_engine_age_sex(x)
  
  y <- y %>% 
    mutate(age1 = cut(age, 
                      breaks = c(-Inf, 15, 25, 35, 45, 55, 65, 75, Inf),
                      labels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-110"),
                      right = FALSE))
  
  y %>% 
    group_by(age1, sex) %>% 
    summarise(n = n(), wt = sum(type1_diabetes_year_risk, na.rm = TRUE)) %>% 
    mutate(type1_diabetes_year_risk_per100k = wt/n*100000)
  
  input_population <- past_populations %>% 
    filter(year == min(year))
  
}

# Original reference data for documentation
# tibble::tribble(
#    ~Age.group, ~Incident.cases, ~`Population.(Census.2016)`, ~`Incident.rate.per.100000.population/year`,          ~`95%CI`,
#    "0–14*",           "319",                 "1006552",                                     "31.69*",   "28.21 to 35.17",
#    "15–24*",           "155",                   "576452",                                     "26.89*",   "22.66 to 31.12",
#    "25–34*",           "197",                   "659410",                                     "29.88*",   "25.70 to 34.05",
#    "35–44*",           "233",                   "746881",                                     "31.20*",   "27.19 to 35.20",
#    "45–54*",           "150",                   "626045",                                     "23.96*",   "20.13 to 27.79",
#    "55–64*",           "152",                   "508958",                                     "29.86*",   "25.12 to 34.61",
#    "65–74*",           "154",                   "373508",                                     "41.23*",   "34.72 to 47.74",
#    "75+*",           "143",                   "264059",                                     "54.15*",   "45.28 to 63.03"
# )