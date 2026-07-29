
library(tidyverse)
library(data.table)
library(echarts4r)
library(fst)
library(readODS)

source('deaths_module/apply_age_sex_death.R')
source('births_module/births_by_fertility_projections.R')

source('./2_synthetic_population/risk_correlation.R')
source('./1_2_utils/transform_probability.R')
source('./1_2_utils/apply_chd_risk.R') 
source('./1_2_utils/disease_prevalence.R')

source('./1_2_utils/prevalence_qof.R')

select <- dplyr::select

source('./1_2_utils/main_utils.R')
source('./2_synthetic_population/1_10_temp_synthetic_population.R')

source("~/Documents/SIB/PHM/PHModel/2_synthetic_population/risk_correlation.R", echo = TRUE)

source('./3_pre_main/pre_main_1.R')
source('./3_pre_main/pre_main_1_theoretical_minimum.R')
source('./3_pre_main/pre_main_2.R')

source('./3_pre_main/pre_main_3.R')
source('./3_pre_main/pre_main_35.R')
source('./3_pre_main/pre_main_35_deaths.R')
source('./3_pre_main/pre_main_36.R')

source('./4_main/main_2_4.R')
source('./4_main/main_2_5.R')
source('./4_main/main_2_6.R')

source('./main/post_main.R')
source('./main/main_2_5.R')
source('./main/main_2_6.R')
