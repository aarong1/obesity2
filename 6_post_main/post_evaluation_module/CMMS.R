library(fst)
library(tidyverse)


compute_cmms <- function(df) {
  df %>% 
    mutate(
      cmms =
        # 1.52 * (lung_cancer != 0) +      # proxy for cancer
       1.52 * (lung_cancer !=0 |          prostate_cancer !=0 |     female_breast_cancer !=0 |
        kidney_cancer !=0 |         oesophageal_cancer !=0 |  stomach_cancer !=0 |       osteogastric_cancer !=0 | 
        oral_cancer !=0 |          pancreatic_cancer !=0  |  uterine_cancer !=0 |       
        blood_cancer !=0 |         ovarian_cancer !=0 |      colorectal_cancer !=0 )+
        1.31 * (chd != 0) +
        0.28 * (atrial_fibrillation != 0) +
        0.72 * (chronic_kidney_disease != 0) +
        0.54 * (diabetes != 0) +
        0.54 * (stroke != 0) +
        0.97 * (heart_failure != 0) +
        0.34 * (alcohol == "increased_risk"  & !is.na(alcohol)) +
        0.12 * (copd != 0) +               # COPD
        0.15 * (hypothyroidism != 0) +                     # thyroid 
        0.34 * (asthma !=0) +                                    # asthma (assumed 0/1)
        # --- Imputed conditions ---
        0.38 * (depression_percentile > 0.80) +
        0.12 * (diet_percentile < 0.1 & age > 10) +        # migraine proxy (low health)
        0.22 * (wellbeing == 'poor_wellbeing' & !is.na(wellbeing)) +                               # anxiety proxy (assumed 0/1 or score)
        0.32 * (physical_activity_percentile < 0.10) +     # painful conditions proxy
        0.36 * (depression_percentile > 0.99) +            # schizophrenia proxy (deprivation)
        0.12 * (smoking_percentile < 0.15 & age < 50 & age >10) +    # migraine alt proxy
        0.36 * (diet_percentile < 0.20 & age < 40 )         # IBD proxy
    )
}

library(data.table)

compute_cmms_dt <- function(df) {
  # ensure data.table by reference, avoid copies
  setDT(df)
  
  x <- df[, cmms :=
       # 1.52 * (lung_cancer != 0) +      # proxy for cancer
       1.52 * (lung_cancer !=0 |          prostate_cancer !=0 |     female_breast_cancer !=0 |
                 kidney_cancer !=0 |         oesophageal_cancer !=0 |  stomach_cancer !=0 |       osteogastric_cancer !=0 | 
                 oral_cancer !=0 |          pancreatic_cancer !=0  |  uterine_cancer !=0 |       
                 blood_cancer !=0 |         ovarian_cancer !=0 |      colorectal_cancer !=0 )+
       1.31 * (chd != 0) +
       0.28 * (atrial_fibrillation != 0) +
       0.72 * (chronic_kidney_disease != 0) +
       0.54 * (diabetes != 0) +
       0.54 * (stroke != 0) +
       0.97 * (heart_failure != 0) +
       0.34 * (alcohol == "increased_risk"  & !is.na(alcohol)) +
       0.12 * (copd != 0) +               # COPD
       0.15 * (hypothyroidism != 0) +                     # thyroid 
       0.34 * (asthma !=0) +                                    # asthma (assumed 0/1)
       # --- Imputed conditions ---
       0.38 * (depression_percentile > 0.80) +
       0.12 * (diet_percentile < 0.1 & age > 10) +        # migraine proxy (low health)
       0.22 * (wellbeing == 'poor_wellbeing' & !is.na(wellbeing)) +                               # anxiety proxy (assumed 0/1 or score)
       0.32 * (physical_activity_percentile < 0.10) +     # painful conditions proxy
       0.36 * (depression_percentile > 0.99) +            # schizophrenia proxy (deprivation)
       0.12 * (smoking_percentile < 0.15 & age < 50 & age >10) +    # migraine alt proxy
       0.36 * (diet_percentile < 0.20 & age < 40 ) ,        # IBD proxy
     
  ]
  x
}

  # past_populations <- read.fst('./past_populations/past_populations_04_01_2026.fst')

past_populations <- past_populations %>% 
  compute_cmms_dt()

past_populations %>% #select(cmms) %>%# sum() %>% 
  group_by(year, run, intervention ) %>%
  summarise(cmms = mean(cmms,na.rm=T)) %>% 
  group_by(year, intervention ) %>%
  summarise(cmms = mean(cmms)) %>% 
  mutate(year = as.character(year)) %>%
  group_by(intervention) %>% 
  e_charts(year) %>% 
  e_tooltip(trigger='axis') %>% 
  e_line(cmms)

past_populations %>% 
  # filter(year!=min(year)) %>%
  group_by(year, age10, run, intervention ) %>%
  summarise(cmms = mean(cmms,na.rm=T),n=n()) %>% 
  ungroup() %>% 
  complete(year,  age10, nesting(run,intervention), fill = list(cmms = 0,n=0)) %>% #View()
  group_by(year, intervention, age10 ) %>%
  summarise(cmms = mean(cmms),n=sum(n)) %>% 
  mutate(year = as.character(year)) %>%
  # pivot_wider(id_cols = c(year,age10),names_from = intervention,values_from= cmms, values_fill = 0L) %>%
  # dcast(formula = age10 + year ~ intervention, value.var = 'cmms', fill = 0L) %>%
  group_by(age10) %>%
  mutate(year=as.character(year)) %>%
  # e_charts(year) %>% 
  # e_tooltip() %>% 
  # e_title(subtext = 'Intervention is solid, baseline is dashed') %>% 
  # e_line(intervention) %>% 
  # e_line(`non-intervention`, lineStyle = list(type = 'dashed')) 
  ggplot()+
  geom_point(aes(year,cmms,color=age10,group = age10))+  #,group = interaction(age10)
  geom_line(aes(year,cmms,color=age10, lty=intervention, group = interaction(intervention,age10)))

past_populations %>%  
  slice_sample(n=1000) %>% 
  # filter(year == min(year)) %>% 
  compute_cmms() %>% #select(cmms) %>%# sum() %>% 
  group_by(year, run, id, intervention ) %>%
  summarise(cmms = mean(cmms),custom_townsend_score_dz = mean(custom_townsend_score_dz)) %>% 
  group_by(year, id, intervention ) %>%
  summarise(cmms = mean(cmms),custom_townsend_score_dz = mean(custom_townsend_score_dz)) %>% 
  ggplot() +
  geom_point(aes(custom_townsend_score_dz,cmms,group=year,color=as.character(year) ), alpha=0.1) +
  geom_smooth(se = F,method = 'lm', aes(custom_townsend_score_dz,cmms,group=year,color=as.character(year)  ))


past_populations %>% 
  compute_cmms() %>% #select(cmms) %>%# sum() %>% 
  group_by(year, run, intervention, mdm_quintile_soa_name) %>%
  summarise(cmms = mean(cmms)) %>% 
  group_by(year, run, intervention, mdm_quintile_soa_name ) %>%
  summarise(cmms = mean(cmms)) %>% 
  ggplot() +
  geom_point(aes(mdm_quintile_soa_name,cmms,group=year,color=as.character(year) ), alpha=0.1) +
  geom_smooth(se = F,method = 'lm', aes(mdm_quintile_soa_name,cmms,group=year,color=as.character(year)  ))

past_populations %>% 
  compute_cmms() %>% #select(cmms) %>%# sum() %>% 
  group_by(year, run, intervention, mdm_quintile_soa_name) %>%
  summarise(cmms = mean(cmms)) %>% 
  group_by(year, run, intervention, mdm_quintile_soa_name ) %>%
  summarise(cmms = mean(cmms)) %>% 
  ggplot() +
  geom_point(aes(year,cmms,group=mdm_quintile_soa_name,color=as.character(mdm_quintile_soa_name) ), alpha=0.1) +
  geom_smooth(se = F,method = 'lm', aes(year,cmms,group=mdm_quintile_soa_name,color=as.character(mdm_quintile_soa_name)  ))

## -------------------------------
## Generic helper
## -------------------------------

cms20_score <- function(data, weights, na_as_zero = TRUE) {
  vars <- names(weights)
  
  # check all required condition columns exist
  missing <- setdiff(vars, names(data))
  if (length(missing) > 0) {
    stop(
      "Data is missing these condition columns required for CMS-20: ",
      paste(missing, collapse = ", ")
    )
  }
  
  # build design matrix of 0/1 indicators in the same order as `weights`
  X <- lapply(data[vars], function(x) {
    if (na_as_zero) x[is.na(x)] <- 0
    as.numeric(x > 0)   # treat logical / 0-1 / counts as present > 0
  })
  X <- as.data.frame(X)[, vars, drop = FALSE]
  
  # linear predictor: sum_j log(RR_j) * I(condition_j)
  lp <- as.numeric(as.matrix(X) %*% weights)
  lp
}

## -------------------------------
## 20-condition weights
## log(RR/HR) from 20-condition models
## -------------------------------

## 1) Consultations (NB count model, 20-condition column)
cms20_weights_consultations <- c(
  atrial_fibrillation        = log(1.52),
  diabetes                   = log(1.40),
  painful_condition          = log(1.35),
  copd                       = log(1.31),
  cancer                     = log(1.27),
  constipation               = log(1.27),
  connective_tissue_disorder = log(1.27),
  psychosis_bipolar          = log(1.25),
  epilepsy                   = log(1.25),
  anxiety_depression         = log(1.24),
  ibs                        = log(1.20),  # irritable bowel syndrome
  heart_failure              = log(1.18),
  asthma                     = log(1.16),
  dementia                   = log(1.14),
  chd                        = log(1.13),  # coronary heart disease
  stroke_tia                 = log(1.12),
  hearing_loss               = log(1.11),
  alcohol_problems           = log(1.09),
  ckd                        = log(1.08),  # chronic kidney disease
  hypertension               = log(1.07)
)

## 2) Emergency hospital admission (Cox model, 20-condition column)
cms20_weights_emerg_adm <- c(
  epilepsy                   = log(2.17),
  alcohol_problems           = log(2.14),
  cancer                     = log(1.87),
  copd                       = log(1.84),
  psychosis_bipolar          = log(1.73),
  painful_condition          = log(1.69),
  dementia                   = log(1.53),
  atrial_fibrillation        = log(1.51),
  anxiety_depression         = log(1.49),
  diabetes                   = log(1.45),
  stroke_tia                 = log(1.42),
  chd                        = log(1.40),
  constipation               = log(1.31),
  ckd                        = log(1.25),
  asthma                     = log(1.24),
  heart_failure              = log(1.23),
  connective_tissue_disorder = log(1.20),
  ibs                        = log(1.11),
  hypertension               = log(1.08),
  hearing_loss               = log(1.07)
)

## 3) Mortality (Cox model, 20-condition column)
cms20_weights_mortality <- c(
  cancer                     = log(3.60),
  dementia                   = log(2.59),
  copd                       = log(2.36),
  alcohol_problems           = log(2.25),
  epilepsy                   = log(2.13),
  painful_condition          = log(1.62),
  constipation               = log(1.62),
  heart_failure              = log(1.55),
  psychosis_bipolar          = log(1.41),
  anxiety_depression         = log(1.41),
  diabetes                   = log(1.41),
  atrial_fibrillation        = log(1.38),
  stroke_tia                 = log(1.36),
  ckd                        = log(1.31),
  chd                        = log(1.09),
  connective_tissue_disorder = log(0.99),
  hypertension               = log(0.93),
  ibs                        = log(0.88),
  hearing_loss               = log(0.87),
  asthma                     = log(0.84)
)