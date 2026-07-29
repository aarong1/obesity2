library(fst)
library(echarts4r)
library(data.table)
library(tidyverse)
library(DBI)

source('./6_post_main/post_evaluation_module/MULTIMORBIDITY.R')
source('./6_post_main/post_evaluation_module/CMMS.R')

source('./6_post_main/post_evaluation_module/COST.R')
source('./6_post_main/post_evaluation_module/DALYS-YLD.R')
source('./6_post_main/post_evaluation_module/DALYS-YLL.R')
source('./6_post_main/post_evaluation_module/QALYs.R')

source('./6_post_main/post_evaluation_module/PYLL.R')
source('./6_post_main/post_evaluation_module/YWLL.R')

source('./6_post_main/post_evaluation_module/DFLE.R')
source('./6_post_main/post_evaluation_module/HLE.R')

source('./6_post_main/post_evaluation_module/DISEASE.R')

source('./6_post_main/post_evaluation_module/post_evaluation_functions.R')

source('./6_post_main/post_evaluation_module/age_sex_utility_weights.R')
source('./6_post_main/post_evaluation_module/qol_age_sex_equation.R')


library(foreach)
library(doParallel)
library(fst)
registerDoParallel(4L)
threads_fst(5)
data.table::setDTthreads(5) #this is so that don't use all the processors


add_eq5d

add_multimorbidity_fn
compute_cmms_dt
calculate_costs_fn

daly_yld_fn
calculate_daly_yll

qaly_yld_fn

sick_days_fn
bed_days_fn


# past_populations <- read.fst(path = './past_populations/past_populations_obesity_interventions_10_01_2026.fst')
#open connection

con <- dbConnect(duckdb::duckdb(), dbdir = 'past_populations_db/past_populations.duckdb', read_only = F)
latest_tbl <- sort(decreasing = T,dbListTables(con))[1]

# x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' USING SAMPLE 60 PERCENT (bernoulli);'))  # Set cache size to 2MB
x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' ;'))  # Set cache size to 2MB

# past_populations_20260116_015236

past_populations <- dbFetch(x)
dbClearResult(x)
dbDisconnect(con, shutdown=TRUE)
setDT(past_populations)

