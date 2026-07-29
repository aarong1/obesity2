costs <- tibble::tribble(
  ~Disease, ~`Annual.Cost.per.Patient.(£)`, ~`Total.Annual.Cost.(£bn)`,                         ~Key.Notes,
  "Stroke",                "25,000–45,000",                       "26",          "High unpaid care burden",
  "CHD",                       "~7,857",                     "12.7",           "~50% productivity loss",
  "Diabetes",                       "~2,500",                       "14", "Majority cost from complications",
  "Hypothyroidism",                          "~50",                     "0.09",              "Low cost if treated",
  "Asthma",                       "~3,039",                      "3.0",                 "Mainly NHS costs",
  "COPD",                       "~1,909",                      "1.9",            "Hospital-driven costs",
  "NDH",                      "Minimal",                      "N/A",      "Costs arise from prevention",
  "CKD (dialysis)",                      "~34,000",                      "7.0",             "Dialysis very costly",
  "Dementia",                      "~32,000",                       "42",            "Carer burden dominant",
  "Heart Failure",                       "~2,000",                      "2.0",        "Hospitalisations dominate",
  "Lung Cancer",                       "~9,070",                      "2.4",           "High productivity loss",
  "Prostate Cancer",                       "~1,584",                      "0.8",                "Mostly healthcare",
  "Breast Cancer",                       "~1,076",                      "1.5",     "Long-term survivorship costs",
  "Bowel Cancer",                       "~2,756",                      "1.6",    "Mix of care & mortality costs"
) %>%  
  mutate(cost = c(
    35000, 7857, 2500, 50, 3039, 1909, 0, 34000/3, 32000, 2000, 9070, 1584, 1076, 2756
  ),
  disease = c(
    'stroke', 'chd', 'diabetes', 'hypothyroidism', 'asthma', 'copd', 'non_diabetic_hyperglycaemia', 'chronic_kidney_disease', 'dementia', 'heart_failure', 'lung_cancer',  'prostate_cancer', 'female_breast_cancer', 'colorectal_cancer'
  )
  )

library(readr)
nhs_cost <- read_csv("6_post_main/post_evaluation_module/nhs_cost_estimates.csv")
# View(nhs_cost_estimates)

nhs_cost$pp <- 
  c( mean(c(1000,1800)),        
     mean(c(2500,6000)),        
     mean(c(900,1400)),          
     mean(c(1200,2200)),        
     mean(c(1200,2500)),        
     mean(c(6000,12000)),       
     mean(c(10000,18000)),      
     mean(c(8000,14000)),       
     mean(c(6000,10000)),       
     mean(c(10000,20000)),     
     mean(c(5000,9000)),       
     mean(c(8000,15000)),      
     mean(c(12000,25000)),     
     mean(c(8000,16000)),      
     mean(c(12000,30000))     
  )

nhs_cost$morb <- c(
  'atrial_fibrillation',
  'rheumatoid_arthritis',
  'osteoarthritis',
  'epilepsy',
  'osteoporosis',
  'kidney_cancer',
  'oesophageal_cancer',
  'stomach_cancer',
  'oral_cancer',
  'pancreatic_cancer',
  'uterine_cancer',
  'ovarian_cancer',
  'Multiple myeloma',
  'Lymphoma',
  'Leukaemia'
)



costs
nhs_cost <- nhs_cost %>% 
  select(Condition,
         `Estimated Total NHS Cost per Year`,
         `Estimated Mean Cost per Patient per Year`,
         Notes,
         pp,
         morb
  ) 


names(nhs_cost) <- names(costs)

costs <- rbind(costs, nhs_cost)

# pp <- read.fst('past_populations/past_populations_sppg_asthma_copd_depression_ndh_03_12_2025_1741.fst')

# unname(morb_cols) [!unname(morb_cols) %in% costs$disease]
# 'kidney_cancer'
# 'osteogastric_cancer'
# 'oral_cancer'
# 'pancreatic_cancer'
# 'uterine_cancer'
# 'ovarian_cancer'
# # blood_cancer

# [1] "pad"                    "cancer"                 "depression"            
# [4] "osteogastric_cancer"    "blood_multiple_myeloma" "blood_lymphoma"        
# [7] "blood_leukaemia"        "blood_cancer"           "hypertension"     

# 'https://www.nice.org.uk/Media/Default/standards-and-indicators/qof%20indicator%20key%20documents/NM33%20cost%20statement.pdf'
# 335/2/1.892*1.035^8, 'https://bjgp.org/content/64/627/e641'

costs <- costs %>% 
rbind(
  tribble(
    ~`Disease`                 ,  ~`Annual.Cost.per.Patient.(£)`,
    ~`Total.Annual.Cost.(£bn)` ,   ~`Key.Notes ` ,
    ~`cost`                    ,    ~`disease`,  
'Peripheral Arterial Disease', NA,NA,NA,23.98018, 'pad',
'Hypertension',NA,NA,NA, 116.578, 'hypertension' ,
'Blood', NA, NA, NA,16000, 'blood_cancer',
'Osteogastric', NA,NA, NA, 14000, 'osteogastric_cancer'
) %>% setnames( names(costs))
) %>% 
  filter(! disease %in% c('Multiple myeloma', 'Lymphoma', 'Leukaemia'))

# 
# pp <- past_populations
# 
# isCost <- pp %>% 
#   select(
#     c(
#       stroke,               chd,                   diabetes,             hypothyroidism,      
#       asthma,               copd,                  non_diabetic_hyperglycaemia,                  chronic_kidney_disease,      
#       dementia,             heart_failure,         lung_cancer,          prostate_cancer,     
#       female_breast_cancer,         colorectal_cancer,          atrial_fibrillation,  rheumatoid_arthritis,
#       osteoarthritis,       epilepsy,              osteoporosis,         kidney_cancer,        
#       #oesophageal_cancer,   stomach_cancer,    
#       osteogastric_cancer,
#       oral_cancer,          pancreatic_cancer,   
#       uterine_cancer,       ovarian_cancer,        blood_cancer#`Multiple myeloma`,     Lymphoma,            
#       # Leukaemia 
#     ) ) %>% as.matrix() %>% {.!=0}
# 
# setDT(costs)
# 
# nrow(isCost); ncol(isCost)
# nrow(pp); ncol(pp)
# 
# cost_plain <- matrix(
#   nrow = nrow(pp), ncol = ncol(isCost), byrow=F,
#   c(rep(costs[disease=='stroke',cost],nrow(pp)),
#     rep(costs[disease=='chd',cost],nrow(pp)),
#     rep(costs[disease=='diabetes',cost],nrow(pp)),
#     rep(costs[disease=='hypothyroidism',cost],nrow(pp)),
#     rep(costs[disease=='asthma',cost],nrow(pp)),
#     rep(costs[disease=='copd',cost],nrow(pp)),
#     rep(costs[disease=='non_diabetic_hyperglycaemia',cost],nrow(pp)),
#     rep(costs[disease=='chronic_kidney_disease',cost],nrow(pp)),
#     rep(costs[disease=='dementia',cost],nrow(pp)),
#     rep(costs[disease=='heart_failure',cost],nrow(pp)),
#     rep(costs[disease=='lung_cancer',cost],nrow(pp)),
#     rep(costs[disease=='prostate_cancer',cost],nrow(pp)),
#     rep(costs[disease=='female_breast_cancer',cost],nrow(pp)),
#     rep(costs[disease=='colorectal_cancer',cost],nrow(pp)),
#     rep(costs[disease=='atrial_fibrillation',cost],nrow(pp)),
#     rep(costs[disease=='rheumatoid_arthritis',cost],nrow(pp)),
#     rep(costs[disease=='osteoarthritis',cost],nrow(pp)),
#     rep(costs[disease=='epilepsy',cost],nrow(pp)),
#     rep(costs[disease=='osteoporosis',cost],nrow(pp)),
#     rep(costs[disease=='kidney_cancer',cost],nrow(pp)),
#     rep(costs[disease=='oesophageal_cancer',cost],nrow(pp)),
#     
#     # rep(costs[disease=='oesophageal_cancer',cost],nrow(pp)),
#     # rep(costs[disease=='stomach_cancer',cost],nrow(pp)),
#     rep(costs[disease=='oral_cancer',cost],nrow(pp)),
#     rep(costs[disease=='pancreatic_cancer',cost],nrow(pp)),
#     rep(costs[disease=='uterine_cancer',cost],nrow(pp)),
#     rep(costs[disease=='ovarian_cancer',cost],nrow(pp)),
#     rep(costs[disease=='blood_cancer',cost],nrow(pp))
#     
#   )
# )
# 
# costFilter <- cost_plain * isCost 
# disease_total_costs <- rowSums(costFilter)
# people_total_costs <- colSums(costFilter)
# 
# pp_costs <- cbind(pp[,.(run,year)],costFilter)
# 
# pp_costs_long <- melt(
#   pp_costs,
#   id.vars = c('run','year'),
#   variable.name = 'disease',
#   value.name = 'cost'
# )
# 
# pp_costs_long[, .(total_cost = sum(cost, na.rm =T)), by=.(run,year,disease)
#               ]
# 
# pp_costs_long2 <- pp_costs_long[,.(total_cost = mean(total_cost, na.rm =T)*model_specification$population$scale_down_factor) ,
#                 by=.(year,disease)]
# 
# #pp_costs_long2[year==2025 & disease=='stroke', sum(total_cost, na.rm =T)]
# 
# pp_costs_long2[,cum_cost := cumsum(total_cost),by=.(disease)]
# pp_costs_long2 %>% 
#   arrange((year)) %>% 
#   mutate(cum_cost=cumsum(total_cost)) %>% View()
# 
# pp_costs_long2
#            ggplot() +
#            geom_point(aes(year,cum_cost,group=disease,color=disease),alpha=0.3) +
#   geom_line(aes(year,cum_cost,group=disease,color=disease),alpha=0.3)
# 
# # 548,991,844 #1 year
# # 2,195,967,376 #4 years
# 
# costFilter





calculate_costs_fn <- function(past_populations, group_vars= as.character(), year_cut_off=NULL){
  
  costs <- tibble::tribble(
    ~cost, ~disease,
    3500.00000,                      "stroke",
    7857.00000,                         "chd",
    2500.00000,                    "diabetes",
    50.00000,              "hypothyroidism",
    3039.00000,                      "asthma",
    1909.00000,                        "copd",
    0.00000, "non_diabetic_hyperglycaemia",
    3500,      "chronic_kidney_disease",
    3200.00000,                    "dementia",
    2000.00000 ,              "heart_failure",
    9070.00000 ,                "lung_cancer",
    1584.00000 ,            "prostate_cancer",
    1076.00000 ,       "female_breast_cancer",
    2756.00000 ,          "colorectal_cancer",
    1400.00000 ,        "atrial_fibrillation",
    4250.00000 ,       "rheumatoid_arthritis",
    1150.00000 ,             "osteoarthritis",
    1700.00000 ,                   "epilepsy",
    1850.00000 ,               "osteoporosis",
    9000.00000 ,               "kidney_cancer",
    1400.00000 ,         "oesophageal_cancer",
    1100.00000 ,             "stomach_cancer",
    8000.00000 ,                "oral_cancer",
    1500.00000 ,          "pancreatic_cancer",
    7000.00000 ,             "uterine_cancer",
    1150.00000 ,             "ovarian_cancer",
    23.98018 ,                        "pad",
    116.57800 ,               "hypertension",
    1600.00000 ,               "blood_cancer",
    1400.00000 ,        "osteogastric_cancer"
  )
  
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  isCost <- past_populations %>% 
    select(
      c(
        stroke,               chd,                   diabetes,             hypothyroidism,      
        asthma,               copd,                  non_diabetic_hyperglycaemia,                  chronic_kidney_disease,      
        dementia,             heart_failure,         lung_cancer,          prostate_cancer,     
        female_breast_cancer,         colorectal_cancer,          atrial_fibrillation,  rheumatoid_arthritis,
        osteoarthritis,       epilepsy,              osteoporosis,         kidney_cancer,        
        oesophageal_cancer,   stomach_cancer,        oral_cancer,          pancreatic_cancer,   
        uterine_cancer,       ovarian_cancer,        blood_cancer
      ) ) %>% 
    as.matrix() %>%
    {.!=0}

  setDT(costs)

  cost_plain <- matrix(
    nrow = nrow(past_populations), ncol = ncol(isCost), byrow=F,
    c(rep(costs[disease=='stroke',cost],nrow(past_populations)),
      rep(costs[disease=='chd',cost],nrow(past_populations)),
      rep(costs[disease=='diabetes',cost],nrow(past_populations)),
      rep(costs[disease=='hypothyroidism',cost],nrow(past_populations)),
      rep(costs[disease=='asthma',cost],nrow(past_populations)),
      rep(costs[disease=='copd',cost],nrow(past_populations)),
      rep(costs[disease=='non_diabetic_hyperglycaemia',cost],nrow(past_populations)),
      rep(costs[disease=='chronic_kidney_disease',cost],nrow(past_populations)),
      rep(costs[disease=='dementia',cost],nrow(past_populations)),
      rep(costs[disease=='heart_failure',cost],nrow(past_populations)),
      rep(costs[disease=='lung_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='prostate_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='female_breast_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='colorectal_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='atrial_fibrillation',cost],nrow(past_populations)),
      rep(costs[disease=='rheumatoid_arthritis',cost],nrow(past_populations)),
      rep(costs[disease=='osteoarthritis',cost],nrow(past_populations)),
      rep(costs[disease=='epilepsy',cost],nrow(past_populations)),
      rep(costs[disease=='osteoporosis',cost],nrow(past_populations)),
      rep(costs[disease=='kidney_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='oesophageal_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='stomach_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='oral_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='pancreatic_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='uterine_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='ovarian_cancer',cost],nrow(past_populations)),
      rep(costs[disease=='blood_cancer',cost],nrow(past_populations))
    )
  )
  
  costFilter <- cost_plain * isCost 

  pp_costs <- cbind(past_populations[,c('run',group_vars,'intervention','year'),with = FALSE ],costFilter)
  
  pp_costs_long <- melt(
    pp_costs,
    id.vars = c('run',group_vars,'intervention', 'year'),
    variable.name = 'disease',
    value.name = 'cost'
  )
  
  pp_costs_long <- pp_costs_long[, .(total_cost = sum(cost, na.rm =T)), 
                                 by=c('run', group_vars,'intervention', 'year','disease')]
  
  pp_costs_long2 <- pp_costs_long[,.(total_cost = mean(total_cost, na.rm =T)*model_specification$population$scale_down_factor) ,
                                  by=c('year',group_vars,'intervention','disease')]
  
  pp_costs_long2
  
}
 

