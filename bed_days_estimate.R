# The HIS provides information on admitted patient care delivered by Health and Social Care
# Hospitals in Northern Ireland. It is a patient-level administrative data source and each record
# relates to an individual consultant episode. During a single hospital admission, a patient may
# be transferred from the care of one consultant to another, generating an additional consultant
# episode. Episode-based data is therefore not equivalent to admission-based data, as each
# admission may be made up of one or more consultant episodes.
## https://www.health-ni.gov.uk/sites/default/files/publications/health/hs-episode-based-activity-additional-info-explanatory-notes.pdf -----

placeholder_AF_obesity_CVD <- 0.1
placeholder_AF_obesity_Cancer <- 0.1
placeholder_AF_obesity_resp <- 0.1

dw_resp= 0.35
dw_cvd = 0.55
dw_cancer = 0.65

BMI_CVD_DALYS_frac = 0.13
BMI_cancer_DALYS_frac = 0.1
BMI_resp_DALYS_frac = 0.1

AF <- data.frame(
  AF = c(placeholder_AF_obesity_CVD,
  placeholder_AF_obesity_Cancer,
  placeholder_AF_obesity_resp),
  short = c('cvd', 'cancer', 'resp')
)

library(readxl)

episodes <- read_excel("data/hospital_activity_ni/22:23/hs-episode-based-activity-stats-volume-2-22-23.xls", 
                      sheet = "Diagnosis Summary", skip = 2)
# View(episode)


stats <- episodes[-1,
c("Title",
  "Finished
Episodes",
  'Emergency',
  "Admissions",
  
  "Mean Episode Duration",
  "Median Episode Duration",
  "Bed
Days")] |> 
  rename(
    Finished_Episodes = "Finished\nEpisodes",
    Mean_Episode_Duration = "Mean Episode Duration",
    Bed_Days = "Bed\nDays"
  ) |>
  filter(
  Title %in% c(
      "Diabetes mellitus",
      "Diseases of the circulatory system (I00-I99)",
      "Neoplasms (C00-D48)"
      )
) |> 
  mutate(
    LOS_estimate = as.numeric(Finished_Episodes)/as.numeric(Admissions) * Mean_Episode_Duration)

stats2223 <- stats

# If we know incidence for 2025 
# then we can scale to our estimates for 2035
## and apply the multiplier 
# to bed days
# episodes
# and admissions
stats2223$short <- c('cvd', 'cvd', 'cancer')

x <- stats2223 |> 
  left_join(AF) |> 
  mutate(across(c('Finished_Episodes','Emergency','Admissions','Bed_Days'),
                ~ as.numeric(.x) *  AF)) |> 
  mutate(total_admisisons = sum(Admissions)) |>
  mutate(total_episodes = sum(Finished_Episodes)) |>
  rowwise() |> 
  
  mutate(w_mean_duration = Mean_Episode_Duration*Finished_Episodes/total_episodes) |> 
  mutate(W_LOS_estimate = LOS_estimate*Admissions/total_admisisons) |> 
  ungroup()

obesity_stats <- x |> 
  summarise(
    total_bed_days = sum(Bed_Days),
    avg_w_mean_duration = sum(w_mean_duration),
    avg_W_LOS_estimate = sum(W_LOS_estimate),
    total_admissions = sum(Admissions),
    total_episodes = sum(Finished_Episodes)
  )

# number of sicknesses 
# sum of sickness days

# Deaths

deaths <- read_excel("data/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                                sheet = "Table 6.1", 
                     range='A4:M34',
                     skip = 3)

deaths <- deaths[,c("Cause",
          "ICD Code",
         "2023")] |> 
  filter(
    Cause %in% c(
      "Diabetes mellitus (E10-E14)",
      "IX. DISEASES OF THE CIRCULATORY SYSTEM (I00-I99)",
      "II. NEOPLASMS (C00-D48)"
    )
  ) |> 
  mutate(short=c('cancer','cvd')) |> 
  left_join(AF) |> 
  mutate(deaths_obesity = as.numeric(`2023`) * AF)

deaths_due_obesity <- deaths$deaths_obesity

#DALYS  = YLL + YLD 
# HLE for males is approximately 60.3 years and for females it is 61.4 years
# DONT USE HLE - double count
# YLL
# YLD

ni_pop_frac_uk = 0.3


yld_uk <- read_excel("data/ghe2021_yld_bycountry_2021.xlsx", 
                  sheet = "All ages", skip = 6) |> 
  select(Sex, super_cause=5,cause = 6, yld = `United Kingdom`) |> 
  filter(Sex == 'Persons',
         super_cause == 'Malignant neoplasms'|
           super_cause == 'Diabetes mellitus'|
           super_cause == 'Cardiovascular diseases'|
           super_cause == 'Respiratory diseases'
  ) |> 
  mutate(short = case_when(
    super_cause == 'Malignant neoplasms' ~ 'cancer',
    super_cause == 'Cardiovascular diseases' ~ 'cvd',
    super_cause == 'Respiratory diseases' ~ 'resp',
    super_cause == 'Diabetes mellitus' ~ 'cvd'
  ))


yld_ni <- yld_uk |> 
  mutate(yld_ni = as.numeric(yld) * ni_pop_frac_uk) |> 
  left_join(AF) |>
  mutate(yld_obesity = yld_ni * AF*1000)
  



######


daly <- read_excel("data/ghe2021_daly_bycountry_2021.xlsx", 
                  sheet = "All ages", skip = 6)

daly_uk <- daly |> 
select(Sex, super_cause=5,cause = 6, dalys = `United Kingdom`) |> 
  filter(Sex == 'Persons',
  super_cause == 'Malignant neoplasms'|
  super_cause == 'Diabetes mellitus'|
  super_cause == 'Cardiovascular diseases'|
  super_cause == 'Respiratory diseases'
  ) |> 
  mutate(short = case_when(
    super_cause == 'Malignant neoplasms' ~ 'cancer',
    super_cause == 'Cardiovascular diseases' ~ 'cvd',
    super_cause == 'Respiratory diseases' ~ 'resp',
    super_cause == 'Diabetes mellitus' ~ 'cvd'
  ))



dalys_ni <- daly_uk |> 
  mutate(dalys_ni = as.numeric(dalys) * ni_pop_frac_uk) |> 
  left_join(AF) |>
  mutate(dalys_obesity = dalys_ni * AF*1000)
  

# see GHE

tribble(
~'Risk', ~'disease', ~'Death_perc', ~'death_raw', ~'DALYS_perc',
'BMI', "CVD", 11.3 ,460, 13.0,
'BMI', "CHD", 14.6 ,280, 17.5,
'BMI', "Stroke", 5.3  ,60,  7.2
)

# BMI_CVD_DALYS_frac = 0.13
# BMI_cancer_DALYS_frac = 0.1
# BMI_resp_DALYS_frac = 0.1


## Prevalence ----
# see tables

prevalence <- read_excel("data/DisPrevHsct_nisra_2324.xlsx", 
                         sheet = "Unpivoted")

prevalence <- prevalence |>
  pivot_wider(names_from = `Statistic Label`,
              values_from = VALUE) |> 
  filter(`Health and Social Care Trust` == 'Northern Ireland') |> 
  filter(`Financial Year` == "2023/24") |> 
  mutate(prev = `Number of patients on the register`/1000*`Raw disease prevalence per 1,000 patients`) |> 
  mutate(no=`Number of patients on the register`) |> 
  select(Disease, prev, no)

prevalence$short <- c('cvd','resp', 'cancer','cvd','resp',NA,NA,'cvd','cvd',NA,NA,NA,NA,'cvd','cvd','cvd','cvd')

prevalence <- prevalence |> 
  count(short, wt = prev, name = 'prevalence') |> 
  left_join(AF) |> 
  mutate(prev_obesity = prevalence * AF)

## Utility Weights ----
# see tables

# disability weights


dw_resp = 0.3
dw_cvd = 0.5
dw_cancer = 0.65

# YLL

#Incidence

Incidence <- 
  tibble(
    'CVD' = 20355,
    'Cancer' = 13905,
    resp = 20000)

#Prevalence
prevalence$prev_obesity[1] * dw_cancer
prevalence$prev_obesity[2] * dw_cvd
prevalence$prev_obesity[3] * dw_resp

# Costs

costs_2020_cvd <- tibble::tribble(
    ~millions_pounds,~year,~type, ~super_type,
     "136.9","2020","Inpatient Care", 'NHS Costs',
     "25.3", "2020","Outpatient Care", 'NHS Costs',
     "11.4", "2020","Accident & Emergency", 'NHS Costs',
     "36.3", "2020","Primary Care", 'NHS Costs',
     "67.5", "2020","Medications", 'NHS Costs',
      "8.7", "2020","Medical Devices", 'NHS Costs',
    "286.2", "2020","Total NHS Costs","Other Costs",
    "125.1", "2020","Long Term Care","Other Costs",
     "77.8", "2020","Losses from Morbidity","Other Costs",
    "115.2", "2020","Losses from Mortality","Other Costs",
    "153.6", "2020","Informal Care","Other Costs",
    "471.7", "2020", "Total Other Costs","Other Costs"
  )

costs_2020_cvd <- costs_2020_cvd |> 
  mutate(obesity_costs = as.numeric(millions_pounds) * placeholder_AF_obesity_CVD)

prescriptions <- 
  tibble::tribble(
                                                  ~Prescriptions,  ~`2023`, ~`prop_2023`, 
                                "Positive inotropic drugs (2.1)",       66,      66/10409*100, 
                                               "Diuretics (2.2)",      870,     870/10409*100, 
                                   "Anti-arrhythmic drugs (2.3)",       24,      24/10409*100, 
                      "Beta-adrenoreceptor blocking drugs (2.4)",     1544,    1544/10409*100, 
                "Antihypertensive and heart failure drugs (2.5)",     2137,    2137/10409*100, 
    "Nitrates, calcium blockers & other antianginal drugs (2.6)",     1551,    1551/10409*100, 
                            "Anticoagulants and protamine (2.8)",      564,     564/10409*100, 
                                      "Antiplatelet drugs (2.9)",     1077,    1077/10409*100, 
               "Anti-fibrinolytic drugs and haemostatics (2.11)",       21,      21/10409*100, 
                                 "Lipid-regulating drugs (2.12)",     2552,    2552/10409*100, 
                            "other BNF sections (small volumes)",        4,       4/10409*100, 
       "All prescriptions for disease of the circulatory system",    10409,   10409/10409
  )

prescriptions <- prescriptions |> 
  select(Prescriptions, `2023`,prop_2023) |> 
  mutate(obesity_prescriptions = `2023` * placeholder_AF_obesity_CVD) 





working_days_lost = 308601.83
working_days_lost_per_staff_yr = 13.809

staff_years = working_days_lost/working_days_lost_per_staff_yr
#22347.88

lost_cost =  43.988e6

avg_spells_per_staff_year = 0.651
avg_spells_per_staff_year_short_term = 0.485
avg_spells_per_staff_year_long_term = avg_spells_per_staff_year- avg_spells_per_staff_year_short_term
# 0.166

resp_perc_spells = 5.517
resp_perc_days = 11.664

cvd_perc_spells = 4.387
cvd_perc_days = 1.973

cancer_perc_spells = 3.621
cancer_perc_days = 1.1

# short_term
# average_duration

# long_term
# average_duration

Cancers_perc_spells_long_term = 4.263
Cancers_perc_days_long_term = 3.269

resp_perc_spells_long_term = 3.562
resp_perc_days_long_term = 4.458

cvd_perc_spells_long_term = 4.913         
cvd_perc_days_long_term = 3.972

obesity_days_lost <- working_days_lost * resp_perc_days/100 * placeholder_AF_obesity_CVD +
  working_days_lost * cvd_perc_days/100 * placeholder_AF_obesity_Cancer +
  working_days_lost * cancer_perc_days/100 * placeholder_AF_obesity_resp 

obesity_spells <- avg_spells_per_staff_year * staff_years * resp_perc_spells/100 * placeholder_AF_obesity_CVD +
  avg_spells_per_staff_year * staff_years * cvd_perc_spells/100 * placeholder_AF_obesity_Cancer +
  avg_spells_per_staff_year * staff_years * cancer_perc_spells/100 * placeholder_AF_obesity_resp 

cost_per_day = lost_cost/working_days_lost

obesity_cost <- obesity_days_lost * cost_per_day


metric_card_html <- function( top ='top', 
                         change = 'change', 
                         text ='text',
                         change_icon =  '',
                         color = 'red',
                         opacity='opacity-75'){
  
  change_class = if(change_icon=='negative'){  "fa-arrow-down me-1"
  }else if(change_icon=='negative'){ "fa-arrow-up me-1"
  }else{''}
  
  color_class = case_when(color == '#8F00FF' ~ 'theme-purple',
                          color == 'teal' ~ 'theme-teal',
                          color == 'steelblue' ~ 'theme-teal',
                          
                          
                          color == '#dc3545' ~ 'theme-red', 
                          color == 'mediumseagreen' ~ 'theme-green'
  )
  
  
  div(class = paste("grid-item grid-item--small",opacity),
      div(class = "grid-item-content",
          div(class = "metric-card",
              #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
              div(class = "metric-value", style = paste("color:",color), top),
              div(class = "metric-label", text),
              div(class = paste("metric-change", change_icon),
                  tags$i(class = change_class, change)
              )
          )
      )
  )
}


metric_card <- function( top ='top', 
                         change = 'change', 
                         text ='text',
                         change_icon =  '',
                         color = 'red',
                         opacity='opacity-75'){
  
change_class = if(change_icon=='negative'){  "fa-arrow-down me-1"
}else if(change_icon=='negative'){ "fa-arrow-up me-1"
}else{''}

color_class = case_when(color == '#8F00FF' ~ 'theme-purple',
                        color == 'teal' ~ 'theme-teal',
                        color == 'steelblue' ~ 'theme-teal',
                        
                        
                          color == '#dc3545' ~ 'theme-red', 
                        color == 'mediumseagreen' ~ 'theme-green'
)

    
div(class = paste("grid-item grid-item--small",opacity),
    div(class = "grid-item-content",
        div(class = "metric-card",
            #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
            div(class = "metric-value", style = paste("color:",color), format(top,big.mark = ',',digits=3)),
            div(class = "metric-label", text),
            div(class = paste("metric-change", change_icon),
                tags$i(class = change_class, change)
            )
        )
    )
)
}


metric_card_total_bed_days <- metric_card(obesity_stats$total_bed_days,'', 'Total Bed Days')
metric_card_avg_w_mean_duration <-  metric_card(obesity_stats$avg_w_mean_duration , 'days', 'Average Episode')
metric_card_avg_W_LOS_estimate <- metric_card(obesity_stats$avg_W_LOS_estimate ,'', 'Obesity weighted LOS', 'Estimate')
metric_card_total_admissions <- metric_card(obesity_stats$total_admissions , '','Total Admissions')
metric_card_total_episodes <- metric_card(obesity_stats$total_episodes, '', 'Total Consultant Episodes')

# Deaths
metric_card_cancer_deaths_obesity <- metric_card(deaths$deaths_obesity[1], 'Cancers', 'Deaths',color='teal')
metric_card_cvd_deaths_obeisty <- metric_card(deaths$deaths_obesity[2], 'CVD',  'Deaths',color='teal',opacity=0.3)
metric_card_total_deaths_obesity <- metric_card(deaths$deaths_obesity[2],  '', 'Total Deaths', 'Due to Obesity',color='teal')
metric_card_resp_deaths_obesity <- metric_card('', 'NA',  'resp deaths are only slightly above background fatality figures',color='teal')

metric_card_YLL_cancer <-  metric_card(prevalence$prev_obesity[1] * dw_cancer,'', 'YLL Cancer')
metric_card_YLL_cvd <-  metric_card(prevalence$prev_obesity[2] * dw_cvd,'', 'YLL CVD')
metric_card_YLL_resp <-  metric_card(prevalence$prev_obesity[3] * dw_resp,'', 'YLL Respiratory')

metric_card_YLL_total <- metric_card(
    prevalence$prev_obesity[1] * dw_cancer + prevalence$prev_obesity[2] * dw_cvd + prevalence$prev_obesity[3] * dw_resp,
      'due to obesity',
      'YLL Total'
    )

# YLD
yld_ni <- yld_ni |> 
  group_by(short) |> 
  summarise(yld_ni = sum(yld_ni),yld_obesity = sum(yld_obesity))

# DALYS
dalys_ni <- dalys_ni |> 
  group_by(short) |> 
  summarise(dalys_ni = sum(dalys_ni),dalys_obesity = sum(dalys_obesity))


metic_card_daly_total_obesity <- metric_card(sum(dalys_ni$dalys_obesity),'','DALYs', 'due to obesity')

metic_card_daly_resp_obesity <- metric_card(dalys_ni$dalys_obesity[3],'','DALYs resp', 'due to obesity')
metic_card_daly_cancer_obesity <- metric_card(dalys_ni$dalys_obesity[1],'','DALYs cancer', 'due to obesity')
metic_card_daly_cvd_obesity <- metric_card(dalys_ni$dalys_obesity[2],'','DALYs cvd', 'due to obesity')

metic_card_yld_total_obesity <- metric_card(sum(yld_ni$prev_obesity),'','YLDs', 'due to obesity')

metic_card_yld_resp_obesity <- metric_card(yld_ni$prev_obesity[3],'','YLDs resp', 'due to obesity')
metic_card_yld_cancer_obesity <- metric_card(yld_ni$prev_obesity[1],'','YLDs cancer', 'due to obesity')
metic_card_yld_cvd_obesity <- metric_card(yld_ni$prev_obesity[2],'','YLDs cvd', 'due to obesity')

# Prevalence
metic_card_prev_total_obesity <- metric_card(prevalence$prev_obesity[1]+
                                                prevalence$prev_obesity[2]+
                                                prevalence$prev_obesity[3],
                                             '','Prevalence Total')

metic_card_prev_cancer_obesity <- metric_card(prevalence$prev_obesity[1],'','Prevalence cancer')
metic_card_prev_cvd_obesity <- metric_card(prevalence$prev_obesity[2],'','Prevalence cvd')
metic_card_prev_resp_obesity <- metric_card(prevalence$prev_obesity[3],'','Prevalence resp')

#Incidence
metic_card_inc_total_obesity <- metric_card(Incidence$resp * AF$AF[3]+
                                         Incidence$CVD * AF$AF[1]+
                                           Incidence$Cancer * AF$AF[2],
                                       '','Incidence Total')
metic_card_inc_cancer_obesity <- metric_card(Incidence$CVD * AF$AF[1] ,'','Incidence cancer')
metic_card_inc_cvd_obesity <- metric_card(Incidence$Cancer * AF$AF[2] ,'','Incidence cvd')
metic_card_inc_resp_obesity <- metric_card(Incidence$resp * AF$AF[3] ,'','Incidnce resp')

#Prescriptions
prescriptiosn_table <- reactable(
  prescriptions,
  defaultPageSize = 12,
  bordered = TRUE,
  highlight = TRUE,
  striped = TRUE,
  columns = list(
    #`2023` = colDef(format = colFormat(separators = TRUE)),
    prop_2023 = colDef(show = F,format = colFormat(percent = TRUE, digits = 1)),
    obesity_prescriptions = colDef(format = colFormat(separators = TRUE, digits = 1))
  )
)

#Cost
metric_card_costs_total_obesity <- metric_card('£',(costs_2020_cvd$obesity_costs[7]+
               costs_2020_cvd$obesity_costs[7]),
            'Millions',
            'Total Cost Obesity')

metric_card_nhs_obesity <- metric_card(costs_2020_cvd$obesity_costs[7],'CVD','NHS')
metric_card_society_obesity <-metric_card(costs_2020_cvd$obesity_costs[12],'CVD','Society')

#NHS
metric_card_inpatient_obesity <- metric_card(costs_2020_cvd$obesity_costs[1],'CVD','Inpatient')
metric_card_outpatient_obesity <- metric_card(costs_2020_cvd$obesity_costs[2],'CVD','Outpatient')
metric_card_AE_obesity <- metric_card(costs_2020_cvd$obesity_costs[3],'CVD','A & E')
metric_card_primary_care_obesity <- metric_card(costs_2020_cvd$obesity_costs[4],'CVD','Primary Care')
metric_card_med_obesity <- metric_card(costs_2020_cvd$obesity_costs[5],'CVD','Med')
metric_card_medical_device_obesity <- metric_card(costs_2020_cvd$obesity_costs[6],'CVD','Medical devices')

#Other
metric_card_long_term_obesity <- metric_card(costs_2020_cvd$obesity_costs[8],'CVD','Long Term Care','Due to obesity')
metric_card_morbidity_obesity <- metric_card(costs_2020_cvd$obesity_costs[9],'CVD','Losses Morbidity','Due to obesity')
metric_card_mortality_obesity <- metric_card(costs_2020_cvd$obesity_costs[10],'CVD','Losses Mortality','Due to obesity')
metric_card_informal_care_obesity <- metric_card(costs_2020_cvd$obesity_costs[11],'CVD','Informal Care','Due to obesity')

#Lost Labour
metric_card_obesity_days_lost_obesity <- metric_card(obesity_days_lost, 'Obesity', 'NICS Days Lost','Due to obesity')
metric_card_obesity_spells_obesity <- metric_card(obesity_spells, 'Obesity', 'NICS Obesity Spells','Long and short term')
metric_card_obesity_cost_obesity <- metric_card(obesity_cost, 'Obesity', 'NICS Obesity Cost','£')

metric_card_obesity_days_lost_population <- metric_card(obesity_days_lost * 32,'Obesity','Total Days Lost','Due to obesity',color='teal')
metric_card_obesity_spells_population <- metric_card(obesity_spells * 32,'Obesity','Population Spells Off' ,'Long and short term',color='teal')
metric_card_obesity_cost_population <- metric_card(obesity_cost * 32,'Obesity','Population Cost','£',color='teal')


