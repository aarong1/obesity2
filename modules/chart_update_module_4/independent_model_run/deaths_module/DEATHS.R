library(data.table)
# stroke
# chd
# diabetes
# hypothyroidism
# asthma
# copd
# ndh
# kidney_disease
# cancer
# dementia
# heart_failure
# lung 
# prostate
# breast
# bowel

disease_lookups <- tribble(
  ~std, ~disease,
  'atrial_fibrillation', 'Atrial fibrillation and flutter',
  'COPD','Emphysema',
  'COPD', 'Other chronic obstructive pulmonary disease',
  #'Other interstitial pulmonary diseases',
  'chronic_kidney_disease', 'Chronic kidney disease',
  'dementia','Vascular dementia',
  'dementia','Unspecified dementia',
  'CHD', 'Angina pectoris',
  'CHD', 'Acute myocardial infarction',
  'CHD', 'Other acute ischaemic heart diseases',
  'stroke', 'Cerebrovascular disease (I60-I69)',
  'diabetes', 'Diabetes mellitus (E10-E14)',
  'asthma', 'Asthma (J45)',
  # 'Chronic liver disease (K70 K73-K74)',
  'cancer', 'Malignant neoplasms (C00-C97)',
  # 'Acute myocardial infarction (I21)' ,
  'heart_failure', 'Heart failure'
)

cancer_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                                sheet = "Table 6.5", range = "A4:M194")%>%
  mutate(sex ='Males')

cancer_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                                sheet = "Table 6.5", range = "O4:AA213") %>%
  mutate(sex ='Females')

cancer_deaths <- rbind(cancer_males, cancer_females) %>% 
  filter(Year == 2023)

deaths_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                           sheet = "Table 6.2", range = 'A64:P122')%>%
  mutate(sex ='Males')

deaths_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                             sheet = "Table 6.2", range = 'A124:P182') %>%
  mutate(sex ='Females')

names(deaths_males) <- names(deaths_females)

deaths <- rbind(deaths_males, deaths_females) 

deaths <- deaths %>% select(ICD10 = `ICD-10 Codes`,
                  Cause = `Cause of Death (ICD Code)`,
                  everything())

# deaths %>% 
#   filter(Cause == 'Cerebrovascular disease (I60-I69)')

# deaths %>% 
#   filter(Cause == 'Diabetes mellitus (E10-E14)')

# 'stroke', 'Cerebrovascular disease (I60-I69)'
# 'diabetes', 'Diabetes mellitus (E10-E14)'
# 'asthma', 'Asthma (J45)'
# # 'Chronic liver disease (K70 K73-K74)'
# 'cancer', 'Malignant neoplasms (C00-C97)'
# # 'Acute myocardial infarction (I21)' 

# Persons - 'A4:Y845'
# Males - 'AA4:AY845'
# Females - 'BA4:BY845'

first <- deaths %>% 
  filter(Cause %in% disease_lookups$disease) %>% 
  left_join(disease_lookups, by = c('Cause' = 'disease')) %>% 
  mutate(across(c('All Ages',   '0', '1-4', '5-9', '10-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '85-89','90+'),~ .x*19)) %>% 
  group_by(std) %>%
  summarise(deaths = sum(`All Ages`))  

detailed_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                             sheet = "Table 6.4a", range = 'AA4:AY845')

detailed_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                             sheet = "Table 6.4a", range = 'BA4:BY845')

detailed <- rbind(detailed_males, detailed_females)

#' 'Atrial fibrillation and flutter'
#' 'COPD','Emphysema'
#' 'COPD', 'Other chronic obstructive pulmonary disease'
#' #'Other interstitial pulmonary diseases'
#' 'kidney_disease', 'Chronic kidney disease'
#' 'dementia','Vascular dementia'
#' 'dementia','Unspecified dementia'
#' 'CHD', 'Angina pectoris'
#' 'CHD', 'Acute myocardial infarction'
#' 'CHD', 'Other acute ischaemic heart diseases'

second <- detailed %>% 
  filter(Description %in% disease_lookups$disease) %>% 
  left_join(disease_lookups, by = c('Description' = 'disease')) %>% 
  group_by(std) %>%
  summarise(deaths = sum(`All Ages`))  

d <- rbind(first, 
      second)

# past_populations <- read.fst('past_populations/past_populations_qof_nil_mh_09_12_2025_1904.fst')
past_populations <- read.fst('past_populations/past_populations_12_12_2025_1315.fst')
# setDT(pp)

setDT(past_populations)

range(past_populations$year)
past_populations[year == min(past_populations$year),]
# asthma  <- past_populations[year == 2024,
#        .( cases = (.N)*model_specification$population$scale_down_factor),                     # .N = count in each group
#         by = asthma
# ][order(asthma)         # ensure same order as dplyr's default
# ][, csum := cumsum(cases) - first(cases)]
# 
# stroke <- past_populations[year == 2024,
#         .N,                     # .N = count in each group
#         by = stroke
# ][order(stroke)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]
# 
# cancer <- past_populations[year == 2024,
#                            .N,                     # .N = count in each group
#                            by = lung_cancer
# ][order(lung_cancer)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]
# 
# diabetes <- past_populations[year == 2024,
#                            .N,                     # .N = count in each group
#                            by = diabetes
# ][order(diabetes)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]
# 
# chd <- past_populations[year == 2024,
#                              .N,                     # .N = count in each group
#                              by = chd
# ][order(chd)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]
# 
# copd <- past_populations[year == 2024,
#                              .N,                     # .N = count in each group
#                              by = copd
# ][order(copd)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]
# 
# dementia <- past_populations[year == 2024,
#                              .N,                     # .N = count in each group
#                              by = dementia
# ][order(diabetes)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]
# 
# heart_failure <- past_populations[year == 2024,
#                          .N,                     # .N = count in each group
#                          by = heart_failure
# ][order(heart_failure)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]
#
# chronic_kidney_disease <- past_populations[year == 2024,
#                              .N,                     # .N = count in each group
#                              by = chronic_kidney_disease
# ][order(chronic_kidney_disease)         # ensure same order as dplyr's default
# ][, csum := cumsum(N) - first(N)]


past_populations %>% 
  # filter(year == 2025) %>%
  count(year, asthma) %>% 
  mutate(csum = cumsum(n) - first(n))

model_specification$population$scale_down_factor
print(model_specification$model$start_year)

################## phases ###################
  # - chronic - every year - prevalence only
  # = acute -> event based - split evenly between incidence and prevalence
  # - progressive - treat as prevalence only 

#prevalence
#STROKE
stroke <- past_populations[year == 2024,
               .N,
               by = .(run,stroke)
               ][order(run)
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(stroke)
  ][, csum := cumsum(cases) - first(cases)]

stroke[stroke==2024,cases]
stroke[stroke==2023,csum]

d[d$std=='stroke','deaths']/2 / stroke[stroke==2024,cases]
d[d$std=='stroke','deaths']/2 / stroke[stroke==2023,csum]

#CHD

chd <- past_populations[year == 2024,
                           .N,                    
                           by = .(run,chd)
][order(run)
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(chd)][, csum := cumsum(cases) - first(cases)]

chd[chd==2024,cases]
chd[chd==2023,csum]

d[d$std=='CHD','deaths']/2 / chd[chd==2024,cases]
d[d$std=='CHD','deaths']/2 / chd[chd==2023,csum]

#DIABETES

# diabetes[diabetes !=0,sum(N)]
diabetes <- past_populations[year == 2024,
                       .N,                    
                       by = .(run,diabetes)
][order(run)        
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(diabetes)][, csum := cumsum(cases) - first(cases)]

diabetes[max(diabetes)==diabetes,csum]

d[d$std=='diabetes','deaths']/ diabetes[diabetes==2024,csum]
#0.004650208

# asthma
# asthma[asthma !=0,sum(N)]
asthma <- past_populations[year == 2024,
                             .N,                    
                             by = .(run,asthma)
][order(run,asthma)        
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(asthma)
][, csum := cumsum(cases) - first(cases)
]

asthma[max(asthma)==asthma,csum]

d[d$std=='asthma','deaths']/ asthma[asthma==2024,csum]

# cancer
# cancer[cancer !=0,sum(N)]
cancer <- past_populations[year == 2024,
                           .N,                    
                           by = .(run,lung_cancer)
][order(run,lung_cancer)        
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(lung_cancer)][, csum := cumsum(cases) - first(cases)
]

cancer[max(lung_cancer,na.rm = T)==lung_cancer,csum]

d[d$std=='cancer','deaths']/ cancer[lung_cancer==2024,csum]

# copd
# copd[copd !=0,sum(N)]
copd <- past_populations[year == 2024,
                           .N,                    
                           by = .(run,copd)
][order(run,copd)        
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(copd)][, csum := cumsum(cases) - first(cases)]

prevalence_hsct %>% filter(Year==2023) %>%
  count(Disease,wt = Count)

copd[max(copd)==copd,csum]

d[d$std=='COPD','deaths']/ copd[copd==2024,csum]

# dementia
# dementia[dementia !=0,sum(N)]
dementia <- past_populations[year == 2024,
                         .N,                    
                         by = .(run,dementia)
][order(run,dementia)        
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(dementia)][, csum := cumsum(cases) - first(cases)]

dementia[max(dementia)==dementia,csum]

d[d$std=='dementia','deaths']/ dementia[dementia==2024,csum]

# heart_failure
# heart_failure[heart_failure !=0,sum(N)]
heart_failure <- past_populations[year == 2024,
                             .N,                    
                             by = .(run,heart_failure)
][order(run,heart_failure)        
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(heart_failure)][, csum := cumsum(cases) - first(cases)]

heart_failure[max(heart_failure)==heart_failure,csum]

d[d$std=='heart_failure','deaths']/ heart_failure[heart_failure==2024,csum]

# chronic_kidney_disease
# chronic_kidney_disease[chronic_kidney_disease !=0,sum(N)]
chronic_kidney_disease <- past_populations[year == 2024,
                             .N,                    
                             by = .(run,chronic_kidney_disease)
][order(run,chronic_kidney_disease)        
][,  .(cases=mean(N)*model_specification$population$scale_down_factor), by = .(chronic_kidney_disease)][, csum := cumsum(cases) - first(cases)]

chronic_kidney_disease[chronic_kidney_disease==2024,csum]

d[d$std=='chronic_kidney_disease','deaths']/ chronic_kidney_disease[chronic_kidney_disease==2024,csum]

#' =============================================================================
#' CREATE CASE FATALITY RATE LOOKUP DATAFRAME
#' =============================================================================
#' Calculate case fatality rates for all morbidities by dividing deaths by cases
#' This creates a lookup table that can be used in apply_case_death function

case_fatality_rate_df <- tribble(
  ~morbidity, ~case_fatality_rate, ~deaths, ~cases, ~notes,
  
  # STROKE - using incidence (acute event, split deaths between prevalence & incidence)
  'stroke', 
  as.numeric(d[d$std=='stroke','deaths'] / stroke[stroke!=0,sum(cases)]),
  as.numeric(d[d$std=='stroke','deaths']),
  stroke[stroke!=0,sum(cases)],
  'Acute event - deaths split 50/50 between incidence and prevalence',
  
  # CHD - using incidence (acute event, split deaths between prevalence & incidence)
  'chd',
  as.numeric(d[d$std=='CHD','deaths'] / chd[chd!=0,sum(cases)]),
  as.numeric(d[d$std=='CHD','deaths']),
  chd[chd!=0,sum(cases)],
  'Acute event - deaths split 50/50 between incidence and prevalence',
  
  # DIABETES - using prevalence (chronic condition)
  'diabetes',
  as.numeric(d[d$std=='diabetes','deaths'] / diabetes[diabetes!=0,sum(cases)]),
  as.numeric(d[d$std=='diabetes','deaths']),
  diabetes[diabetes!=0,sum(cases)],
  'Chronic condition - deaths attributed to prevalence',
  
  # ASTHMA - using prevalence (chronic condition)
  'asthma',
  as.numeric(d[d$std=='asthma','deaths'] / asthma[asthma!=0, sum(cases)]),
  as.numeric(d[d$std=='asthma','deaths']),
  asthma[asthma!=0, sum(cases)],
  'Chronic condition - deaths attributed to prevalence',
  
  # COPD - using prevalence (progressive condition)
  'copd',
  as.numeric(d[d$std=='COPD','deaths'] / copd[copd!=0, sum(cases)]),
  as.numeric(d[d$std=='COPD','deaths']),
  copd[copd!=0, sum(cases)],
  'Progressive condition - deaths attributed to prevalence',
  
  # DEMENTIA - using prevalence (progressive condition)
  'dementia',
  as.numeric( d[d$std=='dementia','deaths'] / dementia[dementia!=0, sum(cases)] ),
  as.numeric(d[d$std=='dementia','deaths']),
  dementia[dementia!=0, sum(cases)],
  'Progressive condition - deaths attributed to prevalence',
  
  # HEART FAILURE - using prevalence (progressive condition)
  'heart_failure',
  as.numeric(d[d$std=='heart_failure','deaths'] / heart_failure[heart_failure!=0, sum(cases)]),
  as.numeric(d[d$std=='heart_failure','deaths']),
  heart_failure[heart_failure!=0, sum(cases)],
  'Progressive condition - deaths attributed to prevalence',
  
  # CHRONIC KIDNEY DISEASE - using prevalence (progressive condition)
  'chronic_kidney_disease',
  as.numeric(d[d$std=='chronic_kidney_disease','deaths'] / chronic_kidney_disease[chronic_kidney_disease!=0, sum(cases)]),
  as.numeric(d[d$std=='chronic_kidney_disease','deaths']),
  chronic_kidney_disease[chronic_kidney_disease!=0, sum(cases)],
  'Progressive condition - deaths attributed to prevalence',
  
  # CANCER - using prevalence
  'cancer',
  as.numeric(d[d$std=='cancer','deaths'] / cancer[lung_cancer!=0, sum(cases)]),
  as.numeric(d[d$std=='cancer','deaths']),
  cancer[lung_cancer!=0, sum(cases)],
  'Malignant neoplasms - deaths attributed to prevalence'#,
  # 'lung_cancer', lung_case_fatality,NA, NA, NA,
  # 'colorectal_cancer', colorectal_case_fatality,NA, NA, NA,
  # 'oral_cancer', oral_case_fatality,NA, NA, NA,
  # 'pancreatic_cancer', pancreatic_case_fatality,NA, NA, NA,
  # 'uterine_cancer', uterine_case_fatality,NA, NA, NA,
  # 'blood_cancer', blood_case_fatality,NA, NA, NA,
  # 'ovarian_cancer', ovarian_case_fatality,NA, NA, NA,
  # 'osteogastric_cancer', osteogastric_case_fatality,NA, NA, NA,
  # 'prostate_cancer', prostate_case_fatality,NA, NA, NA,
  # 'female_breast_cancer', breast_case_fatality,NA, NA, NA,
  # 'renal_cancer', renal_case_fatality, NA, NA, NA
)

write.fst (case_fatality_rate_df,'./deaths_module/case_fatality_rate_df.fst')


# Display the case fatality rate lookup table
print("=== CASE FATALITY RATE LOOKUP TABLE ===")
print(case_fatality_rate_df %>% 
        select(morbidity, case_fatality_rate, deaths, cases) %>%
        mutate(case_fatality_rate = round(case_fatality_rate, 6)))

pp <- slice_sample(.data = past_populations %>% filter(run==1, year==min(year)), n=5000)

setDT(pp)

pp %>% mutate(diabetes_percentile = rank(diabetes_risk,ties.method = 'random')/max(rank(diabetes_year_risk,ties.method = 'random'))) %>%
  ggplot()+
  geom_point(aes(diabetes_year_risk,diabetes_percentile))

pp %>% mutate(diabetes_deaths_percentile = 107.5221* rank(get('diabetes_risk'),ties.method = 'random')/max(rank(get('diabetes_risk'),ties.method = 'random'))) %>%
  mutate(diabetes_death = ifelse(runif(n=n())>diabetes_percentile,1,0)) %>% 
  count(diabetes_death)

pp <- pp[ get('diabetes')!=0, 
          `:=`( r = frank(get('diabetes_year_risk'))/max(frank(get('diabetes_year_risk')) ),
                n = .N,
                diabetes_deaths_percentile = rank(get('diabetes_year_risk'), ties.method = 'random')/max(rank(pp[,'diabetes_year_risk'],ties.method = 'random')))]
  hist(pp$diabetes_deaths_percentile)
  
p <- 0.004650208

pp[
  133* diabetes_deaths_percentile < runif(.N),
  # diabetes_deaths_percentile<p,
  `:=`(
    death        = year,       # or max(year), depending on your model
    death_reason = "diabetes"
  )
];count(pp,death_reason )

# .5 /.003750208 = 133

past_populations <- read.fst('past_populations/past_populations_12_12_2025_1315.fst')
# setDT(pp)

setDT(past_populations)

x <- past_populations[year == 2024,]

pp <- x
# pp[ copd!=0, 
#            .N,.(run,copd)
#     ][,
#       .(h = mean(N, na.rm = T)),.(copd)]

p <- 0.019750208
morbidity = 'copd'
pp1 <- pp[ copd !=0, 
          `:=`( #r = frank(get('copd_year_risk'))/max(frank(get('copd_year_risk')) ),
                #n = .N,
                # deaths_percentile = 4.5*rank(get('copd_year_risk'), ties.method = 'random')/max(rank(pp[,'copd_year_risk'],ties.method = 'random')),
                deaths_percentile = frank(get('copd_year_risk'))/max(frank(get('copd_year_risk')))
          )
          ]

pp1 %>% ggplot() + geom_point(aes(deaths_percentile, copd_year_risk))
# geom_bar(aes(deaths_percentile))

sample(x = pp1[copd!=0,id], size = round(p*sum(pp1$copd!=0)), replace = F, prob=pp1[copd!=0,copd_year_risk]) 

pp1[
  (.5/p) * deaths_percentile < runif(.N), 
  ][,
    .(n = .N), .(copd)
  ]

# %>% count(death,copd,death_reason )

pp2 <- pp1[
  (.5/p)*   deaths_percentile < runif(.N),
  # diabetes_deaths_percentile < p,
  `:=`(
    death        = year,       # or max(year), depending on your model
    death_reason = "copd"
  )
];
count(pp2,death,copd,death_reason ) %>% mutate(csum = cumsum(n)-first(n)) %>% mutate(death / csum)

# .5 /.003750208 = 133

#' =============================================================================
#' APPLY CASE FATALITY TO POPULATION
#' =============================================================================
#' Function to apply disease-specific case fatality rates to a population
#' Looks up the case fatality rate from case_fatality_rate_df

#' @param input_population data.table with population data
#' @param morbidity character string of the morbidity column name
#' @param case_fatality_lookup_df dataframe containing case fatality rates
#' @return modified input_population with death assignments

apply_case_death <- function(input_population, morbidity = 'diabetes') {

  setDT(input_population)
  
  # Lookup the case fatality rate for this morbidity
  p <- case_fatality_rate_df %>% 
    filter(morbidity == {{morbidity}}) %>% 
    pull(case_fatality_rate)
  
  # Check if morbidity exists in lookup table
  if(length(p) == 0) {
    stop(paste0("Morbidity '", morbidity, "' not found in case_fatality_rate_df"))
  }
  
  # Create the year risk variable name
  morbidity_year_risk <- paste0(morbidity, '_year_risk')
  
  # Calculate death percentile for individuals with the morbidity
  input_population[get(morbidity) != 0, 
                   `:=`(death_percentile = frank(get(morbidity_year_risk)) / 
                          max(frank(get(morbidity_year_risk))))]
  
  
  # Apply case fatality rate - assign deaths to those with morbidity
  # Higher year_risk = higher percentile = more likely to die
  input_population[
    # (runif(.N) < p) & (get(morbidity) != 0), 
    
    (.5/p) * death_percentile < runif(.N),
    
    `:=`(
      death        = max(year),     
      death_reason = morbidity
    )
  ]
  
  cat(paste0("\n=== Applied case fatality for: ", morbidity, " ===\n"))
  cat(paste0("Case fatality rate: ", round(p, 6), "\n"))
  cat(paste0("Deaths assigned: ", sum(input_population$death_reason == morbidity, na.rm = TRUE), "\n"))
  cat(paste0("Cases prevalent: ", sum(input_population$'diabetes' !=0, na.rm = TRUE), "\n"))
  
  input_population$death_percentile <- NULL
  
  return(input_population)
}

past_populations %>% 
  filter(year == min(year),run==1) %>% 
  setDT() %>% 
  apply_case_death(morbidity = 'diabetes') %>% 
  count(death_reason, death, age20, diabetes)

# Example usage with the updated function

# Example usage with the updated function
past_populations %>% filter(year == min(year)+1) %>% setDT() %>% 
  apply_case_death(morbidity = 'diabetes') %>% 
  count(death_reason, death, diabetes)

# You can now easily apply case fatality for any morbidity:
# apply_case_death(input_population, morbidity = 'stroke')
# apply_case_death(input_population, morbidity = 'chd')
# apply_case_death(input_population, morbidity = 'copd')
# apply_case_death(input_population, morbidity = 'dementia')
# etc.


# asthma[,sum(cases)]
# copd[,sum(cases)]
# chronic_kidney_disease[,sum(cases)]
# cancer[,sum(cases)]
# dementia[,sum(cases)]
# heart_failure[,sum(cases)]
# lung_cancer[,sum(cases)]
# colorectal_cancer[,sum(cases)]
# cancer[,sum(cases)]


# prostate
# breast
# bowel

past_populations <- read.fst('past_populations/past_populations_12_12_2025_1315.fst')
# setDT(pp)

setDT(past_populations)

pp <- past_populations[year == 2024,]

pp <- pp %>% 
  # setDT() %>%
  # filter(year == min(year)) %>%
  apply_case_death( morbidity = 'lung_cancer') %>% 
  apply_case_death( morbidity = 'stroke') %>% 
  apply_case_death( morbidity = 'chd') %>% 
  apply_case_death( morbidity = 'diabetes') %>% 
  apply_case_death( morbidity = 'asthma') %>% 
  apply_case_death( morbidity = 'copd') %>% 
  apply_case_death( morbidity = 'chronic_kidney_disease') %>% 
  apply_case_death( morbidity = 'dementia') %>% 
  apply_case_death( morbidity = 'heart_failure') 
  # apply_case_death(morbidity = 'hypothyroidism') %>% 
  # apply_case_death(morbidity = 'ndh') %>% 
  # apply_case_death(morbidity = 'cancer') %>% 

death <- pp %>% 
  count(death_reason,run, death)

all_death_pp_df <- 
  expand_grid(death_reason = setdiff(unique(death[,death_reason]),NA),
              run = unique(death$run),
              death = setdiff(unique( death$death),0) )

left_join(
  all_death_pp_df,
  death,
  by = c('death_reason','run','death')
) %>%
  mutate(n = ifelse(is.na(n),0,n)) -> all_death_pp_df

# case_fatality_rate_df

all_death_pp_df %>% 
  group_by(death_reason) %>%
  summarise(mean(n)*model_specification$population$scale_down_factor) %>%
  mutate(death_reason =case_when(
         death_reason == 'lung_cancer' ~ 'cancer',
         death_reason == 'copd' ~'COPD',
         death_reason == 'chd' ~ 'CHD',
         TRUE ~ death_reason
          )) %>% 
  left_join(d,by = c(death_reason = 'std'))



            
            
            
            
            
            