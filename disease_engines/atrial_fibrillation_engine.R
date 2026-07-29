library(tibble)
library(dplyr)
library(data.table)

# Temporal Trends in Incidence, Prevalence, and Mortality of Atrial Fibrillation in Primary Care
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5524079/
# Original literature data (commented out - using derived data files instead):

# af_incidence <- tibble::tribble(
#   ~sex, ~age_band, ~`overall`, ~`1998-2001`, ~`2002-06`, ~`2007-10`,
# "All", "all ages", "1.26 (1.25–1.27)", "1.11 (1.09–1.13)", "1.33 (1.31–1.34)", "1.33 (1.31–1.35)",
# "All",   "0 to55",  "0.13 (0.13–0.13)",  "0.12 (0.11–0.13)",  "0.14 (0.13–0.15)",  "0.13 (0.13–0.14)",
# "All",   "55 to 64",  "1.16 (1.13–1.19)",  "1.06 (1.00–1.12)",  "1.20 (1.16–1.24)",  "1.22 (1.17–1.26)",
# "All",   "65 to 74",  "3.24 (3.19–3.30)",  "3.02 (2.92–3.13)",  "3.42 (3.34–3.50)",  "3.26 (3.18–3.35)",
# "All",   "75 to 84",  "6.42 (6.33–6.52)",  "5.72 (5.55–5.89)",  "6.84 (6.71–6.99)",  "6.66 (6.51–6.82)",
# "All",   "85 to 110 ",  "7.65 (7.48–7.81)",  "6.27 (5.98–6.58)",  "8.05 (7.80–8.31)",  "8.73 (8.46–9.01)",
# "Males",   "all ages",  "1.33 (1.32–1.35)",  "1.17 (1.14–1.20)",  "1.39 (1.37–1.42)",  "1.43 (1.41–1.46)",
# "Males",   "<55",  "0.19 (0.18–0.19)",  "0.17 (0.15–0.18)",  "0.20 (0.19–0.21)",  "0.19 (0.18–0.20)",
# "Males",   "55 to 64",  "1.55 (1.51–1.60)",  "1.43 (1.34–1.53)",  "1.60 (1.53–1.67)",  "1.62 (1.55–1.70)",
# "Males",   "65 to 74",  "3.97 (3.89–4.06)",  "3.78 (3.62–3.96)",  "4.06 (3.94–4.20)",  "4.05 (3.91–4.20)",
# "Males",   "75 to 84",  "7.12 (6.98–7.28)",  "6.34 (6.06–6.64)",  "7.54 (7.32–7.78)",  "7.45 (7.20–7.70)",
# "Males",   "85 to 110",  "8.24 (7.93–8.56)",  "6.65 (6.08–7.26)",  "8.69 (8.21–9.19)", "9.57 (9.06–10.09)",
# "Females", "all ages",  "1.18 (1.16–1.19)",  "1.05 (1.02–1.08)",  "1.26 (1.24–1.28)",  "1.22 (1.20–1.25)",
# "Females",  "<55",  "0.07 (0.07–0.07)",  "0.06 (0.06–0.07)",  "0.07 (0.07–0.08)",  "0.07 (0.06–0.08)",
# "Females",  "55 to 64",  "0.76 (0.73–0.80)",  "0.68 (0.62–0.74)",  "0.80 (0.75–0.85)",  "0.81 (0.76–0.87)",
# "Females",  "65 to 74",  "2.58 (2.51–2.64)",  "2.35 (2.22–2.48)",  "2.82 (2.72–2.93)",  "2.53 (2.42–2.64)",
# "Females",  "75 to 84",  "5.93 (5.81–6.04)",  "5.30 (5.09–5.52)",  "6.35 (6.18–6.53)",  "6.08 (5.89–6.27)",
# "Females",  "85 to 110",  "7.37 (7.18–7.57)",  "6.11 (5.77–6.47)",  "7.77 (7.48–8.07)",  "8.33 (8.01–8.66)"

# ============================================================================
# ATRIAL FIBRILLATION (AF) RISK ENGINE
# ============================================================================
# Literature sources:
# 1. Temporal Trends in Incidence, Prevalence, and Mortality of Atrial Fibrillation
#    https://pmc.ncbi.nlm.nih.gov/articles/PMC5524079/
# 2. Impact of lifestyle risk factors on atrial fibrillation
#    https://www.sciencedirect.com/science/article/pii/S2772487524001090
# 
# Risk factors: COPD (RR 2.24), Hypertension (RR 1.50), OSA (RR 1.40), 
# Obesity (RR 1.39), Smoking (RR 1.33), Diabetes (RR 1.28), Alcohol (RR 1.08),
# Hypercholesterolemia (RR 1.02)
# ============================================================================

# Load AF incidence data from derived CSV files (age-specific rates)
male_af_inc <- read.csv('./disease_engines/derived_male_af.csv')
female_af_inc <- read.csv('./disease_engines/derived_female_af.csv')

af_inc <- rbind(
  male_af_inc %>% mutate(sex = 'Males'), 
  female_af_inc %>% mutate(sex = 'Females')
)

# Create af_incidence_per100k from the loaded data
# The 'mode' column contains probability, convert to per 100,000
af_incidence_per100k <- af_inc %>%
  select(age, sex, mode) %>%
  mutate(incidence = mode * 100000) %>%
  select(age, sex, incidence)

# Simplified incidence (commented out - using derived data files instead):
# af_incidence_per100k <- tribble(
#     ~age, ~Males, ~Females,
#     '0-54', 19, 7,
#     '55-64', 155, 76,
#     '65-74', 397, 258,
#     '75-84', 712, 593,
#     '85-110', 824, 737
# )


# BMI/Obesity - Relative Risk (per 5 kg/m² increase from reference BMI of 20)
# Base RR of 1.39 for obesity (BMI ≥30) extrapolated across BMI range
rr_af_bmi <- 1.07  # Per 5-unit increase

# Smoking - Relative Risk
rr_af_smoking <- tibble::tribble(
  ~smoking, ~RR,
  "never_smoked", 1.0,
  "former", 1.16,
  "current_smoker", 1.33
)

# Alcohol - Relative Risk
rr_af_alcohol <- tibble::tribble(
  ~alcohol, ~RR,
  "never", 1.0,
  "within_guidelines", 1.0,
  "above_guidelines", 1.08
)

# Hypertension - Relative Risk
rr_af_hypertension <- tibble::tribble(
  ~hypertension_status, ~RR,
  "controlled", 1.0,
  "uncontrolled", 1.50
)

# Diabetes - Relative Risk
rr_af_diabetes <- tibble::tribble(
  ~diabetes_status, ~RR,
  "no", 1.0,
  "yes", 1.28
)

# Cholesterol - Relative Risk
rr_af_cholesterol <- tibble::tribble(
  ~cholesterol_status, ~RR,
  "normal", 1.0,
  "raised", 1.01,
  "high", 1.02
)

#https://www.sciencedirect.com/science/article/pii/S2772487524001090#fig1
# hronic Obstructive Pulmonary Disease	2.24 (1.50–3.35)	Xue [3]
# Hypertension	1.50 (1.42–1.58)	Aune [4]
# Obstructive sleep apnea	1.40 (1.16–1.68)	Ng [5]
# Obesity	1.39 (1.30–1.49)	Wu [6]
# Current smoking	1.33 (1.14–1.56)	Aune [7]
# Diabetes mellitus	1.28 (1.22–1.35)	Aune [8]
# Alcohol per 1 drink/day	1.08 (1.06–1.10)	Larsson [9]
# Triglycerides	1.02 (0.90–1.17)	Guan [10]


# COPD - Relative Risk (not currently modeled - no COPD data in population)
# rr_af_copd <- tibble::tribble(
#   ~copd, ~RR,
#   "No", 1.0,
#   "Yes", 2.24
# )

# Obstructive Sleep Apnoea - Relative Risk (not currently modeled)
# rr_af_osa <- tibble::tribble(
#   ~osa, ~RR,
#   "No", 1.0,
#   "Yes", 1.40
# )

# ============================================================================
# FUNCTION 1: Apply AF risk based on age and sex alone
# ============================================================================
apply_af_risk_engine_age_sex <- function(input_population) {
    dt <- as.data.table(input_population)
    
    inc_dt <- as.data.table(af_incidence_per100k)
    
    # Join by exact age and sex
    dt[inc_dt, on = .(age, sex), af_year_risk := i.incidence / 100000]
    dt[is.na(af_year_risk), af_year_risk := 0]
    
    return(dt)
}

# ============================================================================
# FUNCTION 2: Calculate PAF and theoretical minimum AF risk
# ============================================================================
calculate_af_theoretical_min <- function(input_population) {
    dt <- as.data.table(input_population)
    
    # Exclude prevalent cases
    if ("atrial_fibrillation" %in% names(dt)) {
        dt <- dt[atrial_fibrillation == 0]
    }
    
    # 1. BMI RR (per 5 kg/m² from reference of 20)
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 28,
        bmi == "obese", 35,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := rr_af_bmi^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Smoking RR
    rr_smoking_dt <- as.data.table(rr_af_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 3. Alcohol RR
    rr_alcohol_dt <- as.data.table(rr_af_alcohol)
    dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
    dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
    
    # 4. Hypertension RR
    rr_hypertension_dt <- as.data.table(rr_af_hypertension)
    dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
    dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    
    # 5. Diabetes RR
    rr_diabetes_dt <- as.data.table(rr_af_diabetes)
    dt[rr_diabetes_dt, on = .(diabetes_status), RR_diabetes_indiv := i.RR]
    dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
    
    # 6. Cholesterol RR
    rr_cholesterol_dt <- as.data.table(rr_af_cholesterol)
    dt[rr_cholesterol_dt, on = .(cholesterol_status), RR_cholesterol_indiv := i.RR]
    dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
    
    # 7. COPD RR (commented out)
    # rr_copd_dt <- as.data.table(rr_af_copd)
    # dt[rr_copd_dt, on = .(copd), RR_copd_indiv := i.RR]
    # dt[is.na(RR_copd_indiv), RR_copd_indiv := 1]
    
    # 8. OSA RR (commented out)
    # rr_osa_dt <- as.data.table(rr_af_osa)
    # dt[rr_osa_dt, on = .(osa), RR_osa_indiv := i.RR]
    # dt[is.na(RR_osa_indiv), RR_osa_indiv := 1]
    
    # Combined RR for each individual
    dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_alcohol_indiv * 
                        RR_hypertension_indiv * RR_diabetes_indiv * 
                        RR_cholesterol_indiv] # * RR_copd_indiv * RR_osa_indiv
    
    # Calculate PAF by age-sex group
    paf_dt <- dt[, .(AF = 1 - .N / sum(RR_combined, na.rm = TRUE)), by = .(age, sex)]
    
    # Merge incidence data with PAF
    inc_dt <- as.data.table(af_incidence_per100k)
    
    min_dt <- merge(inc_dt, paf_dt, by = c("age", "sex"))
    
    # Calculate theoretical minimum risk
    min_dt[, af_prob_min := (incidence / 100000) * (1 - AF)]
    
    return(min_dt[, .(age, sex, af_prob_min)])
}

# ============================================================================
# FUNCTION 3: Apply individual risk factors to calculate personalized AF risk
# ============================================================================
apply_af_risk_factors <- function(input_population, theoretical_min_table) {
    dt <- as.data.table(input_population)
    min_dt <- as.data.table(theoretical_min_table)
    
    # 1. BMI RR (per 5 kg/m² from reference of 20)
    dt[, bmi_val := fcase(
        bmi == "normal", 20,
        bmi == "overweight", 28,
        bmi == "obese", 35,
        default = 20
    )]
    
    dt[, RR_bmi_indiv := rr_af_bmi^((bmi_val - 20) / 5)]
    dt[is.na(RR_bmi_indiv), RR_bmi_indiv := 1]
    
    # 2. Smoking RR
    rr_smoking_dt <- as.data.table(rr_af_smoking)
    dt[rr_smoking_dt, on = .(smoking), RR_smoking_indiv := i.RR]
    dt[is.na(RR_smoking_indiv), RR_smoking_indiv := 1]
    
    # 3. Alcohol RR
    rr_alcohol_dt <- as.data.table(rr_af_alcohol)
    dt[rr_alcohol_dt, on = .(alcohol), RR_alcohol_indiv := i.RR]
    dt[is.na(RR_alcohol_indiv), RR_alcohol_indiv := 1]
    
    # 4. Hypertension RR
    rr_hypertension_dt <- as.data.table(rr_af_hypertension)
    dt[rr_hypertension_dt, on = .(hypertension_status), RR_hypertension_indiv := i.RR]
    dt[is.na(RR_hypertension_indiv), RR_hypertension_indiv := 1]
    
    # 5. Diabetes RR
    rr_diabetes_dt <- as.data.table(rr_af_diabetes)
    dt[rr_diabetes_dt, on = .(diabetes_status), RR_diabetes_indiv := i.RR]
    dt[is.na(RR_diabetes_indiv), RR_diabetes_indiv := 1]
    
    # 6. Cholesterol RR
    rr_cholesterol_dt <- as.data.table(rr_af_cholesterol)
    dt[rr_cholesterol_dt, on = .(cholesterol_status), RR_cholesterol_indiv := i.RR]
    dt[is.na(RR_cholesterol_indiv), RR_cholesterol_indiv := 1]
    
    # 7. COPD RR (commented out)
    # rr_copd_dt <- as.data.table(rr_af_copd)
    # dt[rr_copd_dt, on = .(copd), RR_copd_indiv := i.RR]
    # dt[is.na(RR_copd_indiv), RR_copd_indiv := 1]
    
    # 8. OSA RR (commented out)
    # rr_osa_dt <- as.data.table(rr_af_osa)
    # dt[rr_osa_dt, on = .(osa), RR_osa_indiv := i.RR]
    # dt[is.na(RR_osa_indiv), RR_osa_indiv := 1]
    
    # Combined RR for each individual
    dt[, RR_combined := RR_bmi_indiv * RR_smoking_indiv * RR_alcohol_indiv * 
                        RR_hypertension_indiv * RR_diabetes_indiv * 
                        RR_cholesterol_indiv] # * RR_copd_indiv * RR_osa_indiv
    
    # Apply theoretical minimum (exact age match)
    dt[min_dt, on = .(age, sex), af_prob_min := i.af_prob_min]
    dt[is.na(af_prob_min), af_prob_min := 0]
    
    # Final individualized risk
    dt[, af_year_risk := af_prob_min * RR_combined]
    
    # Cleanup temporary columns
    dt[, c("bmi_val", "RR_bmi_indiv", "RR_smoking_indiv", "RR_alcohol_indiv",
           "RR_hypertension_indiv", "RR_diabetes_indiv", "RR_cholesterol_indiv",
           "RR_combined", "af_prob_min") := NULL]
    
    return(dt)
}

# ============================================================================
# TEST CODE: Verify PAF calculation recovers incidence
# ============================================================================
# library(data.table)
# 
# # Create test population
# test_pop <- data.table(
#     age = sample(55:90, 10000, replace = TRUE),
#     sex = sample(c("Males", "Females"), 10000, replace = TRUE),
#     bmi = sample(c("normal", "overweight", "obese"), 10000, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
#     smoking = sample(c("never", "former", "current"), 10000, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
#     alcohol = sample(c("never", "within_guidelines", "above_guidelines"), 10000, replace = TRUE, prob = c(0.2, 0.7, 0.1)),
#     hypertension_status = sample(c("controlled", "uncontrolled"), 10000, replace = TRUE, prob = c(0.7, 0.3)),
#     diabetes_status = sample(c("no", "yes"), 10000, replace = TRUE, prob = c(0.9, 0.1)),
#     cholesterol_status = sample(c("normal", "raised", "high"), 10000, replace = TRUE, prob = c(0.6, 0.3, 0.1))
# )
# 
# # Calculate theoretical minimum
# min_table <- calculate_af_theoretical_min(test_pop)
# 
# # Apply risk factors
# test_pop <- apply_af_risk_factors(test_pop, min_table)
# 
# # Verify: mean risk by age/sex group should approximate incidence
# test_pop[, age_group := cut(age, 
#                             breaks = c(0, 55, 65, 75, 85, 111),
#                             labels = c("0-54", "55-64", "65-74", "75-84", "85-110"),
#                             right = FALSE)]
# 
# result <- test_pop[, .(af_year_risk_per100k = mean(af_year_risk, na.rm = TRUE) * 100000), 
#                    by = .(age_group, sex)]
# print(result[order(sex, age_group)])
