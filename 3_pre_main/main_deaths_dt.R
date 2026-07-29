library(data.table)

message('started pre main deaths dt')


# time_one_population <- read.fst('./3_pre_main/intermediate_populations/time_one_population.fst')
# 
# time_one_population <- time_one_population %>%
#   mutate(age_band_death =
#            cut(age, include.lowest = T,
#                breaks = c(-Inf, 0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
#                labels = c('0', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39' ,'40-44' ,'45-49' ,'50-54' ,'55-59' ,'60-64', '65-69' ,'70-74', '75-79' ,'80-84', '85-89' ,'90+')
#            )) %>%
#   left_join(ages_df)
# 
# pop <- time_one_population %>% 
#   count(sex,age_band_death10) %>% 
#   mutate(n=n*model_specification$population$scale_down_factor) %>% 
#   pull(n)
# 
# other_deaths_df <- fatality %>% 
#   filter(std == 'other') %>%
#   ungroup() %>% 
#   mutate(disease = pop) %>% 
#   mutate(other_fatality = deaths/disease) %>% 
#   select(age_band_death10,sex,other_fatality)
# 
# fatality_wide <- fatality %>% 
#   pivot_wider(id_cols = c(sex, age_band_death10), names_glue = '{std}_fatality',names_from = std, values_from = fatality)

#------------------------------------------------------------------------------
# JOIN LOOKUPS
#------------------------------------------------------------------------------

apply_deaths_modelled_deaths <- function(input_population, other_deaths_df, fatality_wide){
  
setDT(fatality_wide)
setDT(input_population)
setDT(other_deaths_df)

setkey(fatality_wide, sex, age_band_death10)
setkey(input_population, sex, age_band_death10)

input_population1 <- copy( input_population)
input_population1[,age_band_death10 := cut(age, include.lowest = T,
                       breaks = c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, 89, Inf),
                       labels = c('1-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+'),
                       right = TRUE)
]

nrow(input_population1)
input_population1[
  fatality_wide,
  (setdiff(names(fatality_wide), c("sex", "age_band_death10"))) :=
    mget(paste0("i.", setdiff(names(fatality_wide), c("sex", "age_band_death10")))),
  on = .(sex, age_band_death10)
]

nrow(input_population1)
count(input_population1,is.na(stroke_fatality))
count(input_population1,is.na(other_fatality))

input_population1[
  other_deaths_df,
  other_fatality := i.other_fatality,
  on = .(sex, age_band_death10)
]






#------------------------------------------------------------------------------
# DISEASE DEATH PROBABILITIES
#------------------------------------------------------------------------------

input_population1[,  `:=`(
"chd_deaths" = (chd!=0)*chd_fatality,
"stroke_deaths"= (stroke!=0)*stroke_fatality,
"heart_failure_deaths"= (heart_failure!=0)*heart_failure_fatality,
"diabetes_deaths"= (diabetes!=0)*diabetes_fatality,
"chronic_kidney_disease_deaths"= (chronic_kidney_disease!=0)*chronic_kidney_disease_fatality,
"dementia_deaths"= (dementia!=0)*dementia_fatality,
"asthma_deaths"= (asthma!=0)*asthma_fatality,
"copd_deaths"= (copd!=0)*copd_fatality,
"lung_cancer_deaths"= (lung_cancer!=0)*lung_cancer_fatality,
"colorectal_cancer_deaths"= (colorectal_cancer!=0)*colorectal_cancer_fatality,
"prostate_cancer_deaths"= (prostate_cancer!=0)*prostate_cancer_fatality,
"female_breast_cancer_deaths"= (female_breast_cancer!=0)*female_breast_cancer_fatality,
"oral_cancer_deaths"= (oral_cancer!=0)*oral_cancer_fatality,
"pancreatic_cancer_deaths"= (pancreatic_cancer!=0)*pancreatic_cancer_fatality,
"uterine_cancer_deaths"= (uterine_cancer!=0)*uterine_cancer_fatality,
"ovarian_cancer_deaths"= (ovarian_cancer!=0)*ovarian_cancer_fatality,
"kidney_cancer_deaths"= (kidney_cancer!=0)*kidney_cancer_fatality)
]

death_cols <- grep("_deaths$", names(input_population1), value = TRUE)

#------------------------------------------------------------------------------
# PROBABILITY SUMS
#------------------------------------------------------------------------------

input_population1[,
  modelled_prob_sum := rowSums(.SD, na.rm = TRUE),
  .SDcols = death_cols
]

input_population1[,
  deaths_prob_sum := modelled_prob_sum + other_fatality
]

input_population1[,
  surv := max(0, 1 - modelled_prob_sum - other_fatality)
]

input_population1[,
  other := fifelse(
    modelled_prob_sum == 0,
    other_fatality,
    pmax(
      0,
      pmin(
        other_fatality,
        1 - modelled_prob_sum - surv
      )
    )
  )
]

#------------------------------------------------------------------------------
# FAST SAMPLING
#------------------------------------------------------------------------------

sample_cols <- c(
  death_cols,
  "other_fatality",
  "surv"
)

sample_names <- c(
  gsub("_deaths$", "", death_cols),
  "other",
  "survive"
)

prob_mat <- as.matrix(input_population1[, ..sample_cols])

prob_mat <- prob_mat / rowSums(prob_mat, na.rm = TRUE)

prob_mat[!is.finite(prob_mat)] <- 0

sample_index <- apply(
  prob_mat,
  1,
  function(p) sample.int(length(p), 1, prob = p)
)

death_samples <- sample_names[sample_index]

input_population[, death_reason := death_samples]

input_population[death_reason != "survive", death := min(year)]

input_population

}

#------------------------------------------------------------------------------
# VALIDATION
#------------------------------------------------------------------------------
store_unit_test <- function(){
expt <- time_one_population[
  ,
  .N,
  by = death_reason
][
  ,
  Freq := N * model_specification$population$scale_down_factor
]
# 
validation <- setDT(fatality)[
  ,
  .(actual = sum(deaths)),
  by = std
]
# 
validation <- rbind(
  validation,
  data.table(
    std = "survive",
    actual = nrow(time_one_population) * model_specification$population$scale_down_factor
  )
)

}

message('done pre main deaths dt')


# 
# validation <- validation[
#   expt,
#   on = .(std = death_reason)
# ]
# 
# validation[
#   ,
#   err := (actual - Freq) / actual
# ]
# 
# setorder(validation, -err)


#------------------------------------------------------------------------------
# RISK WEIGHTING
#------------------------------------------------------------------------------

# time_one_population[
#   ,
#   all_cause_rr_obese := fifelse(
#     bmi == "obese", 1.20,
#     fifelse(bmi == "overweight", 1.07, 1)
#   )
# ]
# 
# time_one_population[
#   ,
#   all_cause_rr_diabetes := fifelse(
#     diabetes_status != "no_diabetes",
#     1.7,
#     1
#   )
# ]
# 
# time_one_population[
#   ,
#   all_cause_rr_smoking := fifelse(
#     smoking == "current_smoker",
#     3.7,
#     1
#   )
# ]
# 
# time_one_population[
#   ,
#   all_cause_rr_active := fifelse(
#     pa != "meets_recommendations",
#     1.86,
#     1
#   )
# ]
# 
# time_one_population[
#   ,
#   all_cause_rr_alcohol := fifelse(
#     alcohol %in% c("increased_risk", "higher_risk"),
#     1.36,
#     1
#   )
# ]
# 
# time_one_population[
#   all_cause_paf,
#   paf_all_cause := i.paf_all_cause,
#   on = .(sex, age_band_death10)
# ]

# time_one_population[
#   ,
#   tm_mortality_prob := mortality_prob * (1 - paf_all_cause)
# ]

# time_one_population[
#   ,
#   risk_weight_mortality_prob :=
#     tm_mortality_prob *
#     all_cause_rr_obese *
#     all_cause_rr_diabetes *
#     all_cause_rr_smoking *
#     all_cause_rr_active *
#     all_cause_rr_alcohol
# ]

# time_one_population[
#   ,
#   risk_weight_survival_prob :=
#     fifelse(
#       is.na(1 - pmin(0.99, risk_weight_mortality_prob)),
#       survival_prob,
#       1 - pmin(0.99, risk_weight_mortality_prob)
#     )
# ]

#------------------------------------------------------------------------------
# FINAL SURVIVAL / OTHER
#------------------------------------------------------------------------------