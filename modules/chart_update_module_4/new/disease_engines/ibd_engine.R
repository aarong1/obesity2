# https://pmc.ncbi.nlm.nih.gov/articles/instance/10592313/bin/NIHMS1919955-supplement-1.pdf

library(dplyr)
library(tidyr)
library(tibble)

# ------------------------------------------------------------
# Common helper: map integer age -> the Table 1 age bands
# (<10, 10-19, 20-29, 30-39, 40-49, 50-59, 60-64, 65-79, 80+)
# ------------------------------------------------------------
make_ibd_age_group <- function(age){
  cut(
    age,
    breaks = c(-Inf, 10, 20, 30, 40, 50, 60, 65, 80, Inf),
    labels = c("<10","10-19","20-29","30-39","40-49","50-59","60-64","65-79","80+"),
    right = FALSE
  )
}

# ============================================================
# 1) Irritable Bowel Disease (IBD) — incidence -> annual risk
#    Using "High probability incidence" column from your table
#    Risk per year ≈ rate_per_100k / 100000
# ============================================================
ibd_incidence <- tribble(
  ~sex,      ~age_group, ~ibd_year_risk,
  "Females", "<10",      4.12,
  "Males",   "<10",      3.96,
  "Females", "10-19",    14.87,
  "Males",   "10-19",    20.56,
  "Females", "20-29",    17.02,
  "Males",   "20-29",    19.32,
  "Females", "30-39",    13.45,
  "Males",   "30-39",    13.92,
  "Females", "40-49",    10.61,
  "Males",   "40-49",    10.96,
  "Females", "50-59",    10.08,
  "Males",   "50-59",    9.47,
  "Females", "60-64",    9.18,
  "Males",   "60-64",    11.36,
  "Females", "65-79",    8.79,
  "Males",   "65-79",    7.77,
  "Females", "80+",      4.93,
  "Males",   "80+",      5.16
) %>%
  mutate(ibd_year_risk = ibd_year_risk / 100000) # per 100k PY -> probability per year

apply_ibd_risk <- function(input_population){
  input_population %>%
    select(-any_of("ibd_year_risk")) %>%
    mutate(age_group = make_ibd_age_group(age)) %>%
    left_join(ibd_incidence, by = c("sex","age_group")) %>%
    replace_na(list(ibd_year_risk = 0))
}

# ============================================================
# 2) Crohn's disease — incidence -> annual risk
# ============================================================
crohns_incidence <- tribble(
  ~sex,      ~age_group, ~crohns_year_risk,
  "Females", "<10",      2.28,
  "Males",   "<10",      2.43,
  "Females", "10-19",    7.95,
  "Males",   "10-19",    11.49,
  "Females", "20-29",    6.89,
  "Males",   "20-29",    7.00,
  "Females", "30-39",    4.76,
  "Males",   "30-39",    4.49,
  "Females", "40-49",    3.54,
  "Males",   "40-49",    3.45,
  "Females", "50-59",    3.25,
  "Males",   "50-59",    2.80,
  "Females", "60-64",    2.73,
  "Males",   "60-64",    2.81,
  "Females", "65-79",    2.35,
  "Males",   "65-79",    1.70,
  "Females", "80+",      1.06,
  "Males",   "80+",      1.53
) %>%
  mutate(crohns_year_risk = crohns_year_risk / 100000)

apply_crohns_risk <- function(current_population){
  current_population %>%
    select(-any_of("crohns_year_risk")) %>%
    mutate(age_group = make_ibd_age_group(age)) %>%
    left_join(crohns_incidence, by = c("sex","age_group")) %>%
    replace_na(list(crohns_year_risk = 0))
}

# ============================================================
# 3) Ulcerative colitis — incidence -> annual risk
# ============================================================
ulcerative_colitis_incidence <- tribble(
  ~sex,      ~age_group, ~ulcerative_colitis_year_risk,
  "Females", "<10",      1.77,
  "Males",   "<10",      1.18,
  "Females", "10-19",    6.15,
  "Males",   "10-19",    8.17,
  "Females", "20-29",    9.12,
  "Males",   "20-29",    11.80,
  "Females", "30-39",    8.19,
  "Males",   "30-39",    8.98,
  "Females", "40-49",    6.64,
  "Males",   "40-49",    7.12,
  "Females", "50-59",    6.30,
  "Males",   "50-59",    6.15,
  "Females", "60-64",    6.02,
  "Males",   "60-64",    8.42,
  "Females", "65-79",    5.56,
  "Males",   "65-79",    5.52,
  "Females", "80+",      3.46,
  "Males",   "80+",      3.61
) %>%
  mutate(ulcerative_colitis_year_risk = ulcerative_colitis_year_risk / 100000)

apply_ulcerative_colitis_risk <- function(current_population){
  current_population %>%
    select(-any_of("ulcerative_colitis_year_risk")) %>%
    mutate(age_group = make_ibd_age_group(age)) %>%
    left_join(ulcerative_colitis_incidence, by = c("sex","age_group")) %>%
    replace_na(list(ulcerative_colitis_year_risk = 0))
}

# ------------------------------------------------------------
# Optional: quick usage
# current_population <- apply_ibd_risk(current_population)
# current_population <- apply_crohns_risk(current_population)
# current_population <- apply_ulcerative_colitis_risk(current_population)
# ------------------------------------------------------------