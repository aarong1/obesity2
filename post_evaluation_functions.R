# calculate_costs_fn
# sick_days_fn
# bed_days_fn
# qaly_yld_fn
# compute_cmms
# add_multimorbidity_fn

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
  ) %>% 
    mutate(cost = cost/10)
  
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


sick_days_fn <- function(past_populations, group_vars = as.character(), year_cut_off = NULL){
  morb_cols <- c('cvd'='pad', 'msk'='osteoporosis',     'cancer'='cancer',                               'msk'='osteoarthritis',             'msk'='rheumatoid_arthritis',       'other'='epilepsy',                  'other'='hypothyroidism','resp'='asthma',                     'resp'='copd',                       'other'='depression',                 'cvd'='non_diabetic_hyperglycaemia','cancer'='colorectal_cancer',          'cancer'='prostate_cancer',           'cancer'='female_breast_cancer',      'cancer'='kidney_cancer',                         'cancer'='oesophageal_cancer',                   'cancer'='stomach_cancer',                       'cancer'='osteogastric_cancer',                  'cancer'='oral_cancer',                          'cancer'='pancreatic_cancer',                    'cancer'='uterine_cancer',            'cancer'='blood_multiple_myeloma',               'cancer'='blood_lymphoma',                       'cancer'='blood_leukaemia',                      'cancer'='blood_cancer',                         'cancer'='ovarian_cancer',            'cancer'='lung_cancer','cvd'='stroke','cvd'='chd','cvd'='diabetes','other'='dementia','cvd'='heart_failure','cvd'='atrial_fibrillation','cvd'='hypertension','cvd'='chronic_kidney_disease')
  
  sick_days_matrix <- tibble::tribble(
    ~broad, ~sick_spells_per_case, ~days_lost_per_case, ~cost_per_case,
    "resp",         0.0025372668,        0.113786804,     16.219132,
    "cvd",         0.0007710649,        0.007355829,      1.048497,
    "cancer",         0.0031595995,        0.020359984,      2.902105,
    "msk",         0.0020023097,        0.071843454,     10.240542
  )
  
  setDT(sick_days_matrix)
  setDT(past_populations)
  
  
  past_populations <- past_populations[age<68&age<20,]
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  m <- melt(
    past_populations,
    id.vars = c("year", "run",'intervention',group_vars),
    measure.vars = unname(morb_cols),
    variable.name = "variable",
    value.name = "value"
  )
  
  m[data.table(variable = unname(morb_cols), 
               broad = names(morb_cols)), on = .( variable), broad := i.broad]
  
  # count non-zero per year/run/variable
  m <- m[, .(n = sum(value != 0,na.rm = T)), by = c('year',group_vars, 'run','intervention','broad')]
  
  # complete grid of year/run/variable
  # grid <- CJ(
  #   year     = unique(m$year),
  #   run      = unique(m$run),
  #   broad = unique(m$broad)
  # )
  
  # do.call(CJ, list(c(5, NA, 1), c(1, 3, 2)))  # same as above
  
  grid <-  do.call(CJ, 
                   lapply(X = c('year','run',group_vars, 'broad'), 
                          function(x){c(unique(m[[x]]))}) 
  )
  
  names(grid) <- c('year', 'run', group_vars, 'broad')
  
  grid <- grid[, intervention := ifelse((run>max(run)/2), 'intervention','non-intervention')]
  
  m2 <- m[grid, on = c('year', 'run', group_vars, 'intervention', 'broad'), nomatch=0L]
  m2[is.na(n), n := 0L]
  
  # mean across runs
  
  res_dt <- m2[, .(prevalence = mean(n)), by = c('year', group_vars, 'intervention', 'broad')]
  
  res_dt <- res_dt[, .(prevalence = sum(prevalence)*model_specification$population$scale_down_factor), by = c('year', group_vars, 'intervention', 'broad')]
  
  res_dt
  
  res_dt[sick_days_matrix, on  = .(broad), 
         `:=`(sick_spells_per_case = i.sick_spells_per_case,
              days_lost_per_case = i.days_lost_per_case,
              cost_per_case = i.cost_per_case)
  ] 
  
  res_dt[, `:=`(days_lost = days_lost_per_case*prevalence*model_specification$population$scale_down_factor, 
                cost = cost_per_case*prevalence*model_specification$population$scale_down_factor, 
                sick_spells = sick_spells_per_case*prevalence*model_specification$population$scale_down_factor)]
}



bed_days_fn <- function(past_populations,group_vars = as.character(), year_cut_off = NULL){
  
  #bed days
  bed_days_df <- tibble::tribble(
    ~broad, ~bed_days_per_case, ~admissions_per_case, ~emergency_admissions_per_case,
    "cancer",         0.6203923,          0.45678642,          0.05017094,
    "cvd",         0.2090028,          0.03813758,          0.04001619,
    "resp",         0.1116457,          0.01755706,          0.03485174,
    "msk",         0.2636094,          0.07721338,          0.04428415
  )
  morb_cols <- c('cvd'='pad', 'msk'='osteoporosis',     'cancer'='cancer',                               'msk'='osteoarthritis',             'msk'='rheumatoid_arthritis',       'other'='epilepsy',                  'other'='hypothyroidism','resp'='asthma',                     'resp'='copd',                       'other'='depression',                 'cvd'='non_diabetic_hyperglycaemia','cancer'='colorectal_cancer',          'cancer'='prostate_cancer',           'cancer'='female_breast_cancer',      'cancer'='kidney_cancer',                         'cancer'='oesophageal_cancer',                   'cancer'='stomach_cancer',                       'cancer'='osteogastric_cancer',                  'cancer'='oral_cancer',                          'cancer'='pancreatic_cancer',                    'cancer'='uterine_cancer',            'cancer'='blood_multiple_myeloma',               'cancer'='blood_lymphoma',                       'cancer'='blood_leukaemia',                      'cancer'='blood_cancer',                         'cancer'='ovarian_cancer',            'cancer'='lung_cancer','cvd'='stroke','cvd'='chd','cvd'='diabetes','other'='dementia','cvd'='heart_failure','cvd'='atrial_fibrillation','cvd'='hypertension','cvd'='chronic_kidney_disease')
  
  
  # sick_days_matrix <- tibble::tribble(
  #   ~broad, ~sick_spells_per_case, ~days_lost_per_case, ~cost_per_case,
  #   "resp",         0.0025372668,        0.113786804,     16.219132,
  #   "cvd",         0.0007710649,        0.007355829,      1.048497,
  #   "cancer",         0.0031595995,        0.020359984,      2.902105,
  #   "msk",         0.0020023097,        0.071843454,     10.240542
  # )
  
  setDT(bed_days_df)
  setDT(past_populations)
  
  
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  m <- melt(
    past_populations,
    id.vars = c("year", "run",'intervention',group_vars),
    measure.vars = unname(morb_cols),
    variable.name = "variable",
    value.name = "value"
  )
  
  m[data.table(variable = unname(morb_cols), 
               broad = names(morb_cols)), on = .( variable), broad := i.broad]
  
  # count non-zero per year/run/variable
  m <- m[, .(n = sum(value != 0,na.rm = T)), by = c('year',group_vars, 'run','intervention','broad')]
  
  # complete grid of year/run/variable
  # grid <- CJ(
  #   year     = unique(m$year),
  #   run      = unique(m$run),
  #   broad = unique(m$broad)
  # )
  
  # do.call(CJ, list(c(5, NA, 1), c(1, 3, 2)))  # same as above
  
  grid <-  do.call(CJ, 
                   lapply(X = c('year','run',group_vars, 'broad'), 
                          function(x){c(unique(m[[x]]))}) 
  )
  
  names(grid) <- c('year', 'run', group_vars, 'broad')
  
  grid <- grid[, intervention := ifelse((run>max(run)/2), 'intervention','non-intervention')]
  
  m2 <- m[grid, on = c('year', 'run', group_vars, 'intervention', 'broad'), nomatch=0L]
  m2[is.na(n), n := 0L]
  
  # mean across runs
  
  res_dt <- m2[, .(prevalence = mean(n)), by = c('year', group_vars, 'intervention', 'broad')]
  
  res_dt <- res_dt[, .(prevalence = sum(prevalence)*model_specification$population$scale_down_factor), by = c('year', group_vars, 'intervention', 'broad')]
  
  res_dt
  
  res_dt[bed_days_df, on  = .(broad), 
         `:=`(bed_days_per_case = i.bed_days_per_case,
              admissions_per_case = i.admissions_per_case,
              emergency_admissions_per_case = i.emergency_admissions_per_case)
  ] 
  
  res_dt[, `:=`(bed_days = bed_days_per_case*prevalence , 
                admissions = admissions_per_case*prevalence , 
                emergency_admissions = emergency_admissions_per_case*prevalence )]
  return(res_dt)
}


qaly_yld_fn <- function(past_populations, group_vars= as.character(), year_cut_off=NULL){
  
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  uws <- data.table(
    disease = c(
      "stroke","chd","diabetes","hypothyroidism","asthma","copd",
      "non_diabetic_hyperglycaemia","chronic_kidney_disease","dementia",
      "heart_failure","lung_cancer","prostate_cancer","female_breast_cancer",
      "colorectal_cancer",
      "atrial_fibrillation","rheumatoid_arthritis","osteoarthritis",
      "epilepsy","osteoporosis","kidney_cancer","oesophageal_cancer",
      "stomach_cancer","oral_cancer","pancreatic_cancer","uterine_cancer",
      "ovarian_cancer","blood_cancer"
    ),
    uw = c(
      0.118,0.124,0.049,0.051,0.043,0.150,
      0.000,0.073,0.477,
      0.201,0.451,0.100,0.200,
      0.288,
      0.035,0.230,0.165,
      0.263,0.040,0.300,0.420,
      0.380,0.320,0.540,0.240,
      0.360,0.310
    )
  )
  
  isCost <- past_populations %>% 
    select(
      c(
        stroke,               chd,                   diabetes,             hypothyroidism,      
        asthma,               copd,                  non_diabetic_hyperglycaemia,                  chronic_kidney_disease,      
        dementia,             heart_failure,         lung_cancer,          prostate_cancer,     
        female_breast_cancer,         colorectal_cancer,          atrial_fibrillation,  rheumatoid_arthritis,
        osteoarthritis,       epilepsy,              osteoporosis,         kidney_cancer,        
        oesophageal_cancer,   stomach_cancer,        oral_cancer,          pancreatic_cancer,   
        uterine_cancer,       ovarian_cancer,        blood_cancer )) %>% 
    as.matrix() %>% 
    {.!=0}
  
  setDT(uws)
  
  uw_plain <- matrix( 
    nrow = nrow(past_populations), ncol = ncol(isCost), byrow=F,
    c(rep(uws[disease=='stroke',uw],nrow(past_populations)),
      rep(uws[disease=='chd',uw],nrow(past_populations)),
      rep(uws[disease=='diabetes',uw],nrow(past_populations)),
      rep(uws[disease=='hypothyroidism',uw],nrow(past_populations)),
      rep(uws[disease=='asthma',uw],nrow(past_populations)),
      rep(uws[disease=='copd',uw],nrow(past_populations)),
      rep(uws[disease=='non_diabetic_hyperglycaemia',uw],nrow(past_populations)),
      rep(uws[disease=='chronic_kidney_disease',uw],nrow(past_populations)),
      rep(uws[disease=='dementia',uw],nrow(past_populations)),
      rep(uws[disease=='heart_failure',uw],nrow(past_populations)),
      rep(uws[disease=='lung_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='prostate_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='female_breast_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='colorectal_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='atrial_fibrillation',uw],nrow(past_populations)),
      rep(uws[disease=='rheumatoid_arthritis',uw],nrow(past_populations)),
      rep(uws[disease=='osteoarthritis',uw],nrow(past_populations)),
      rep(uws[disease=='epilepsy',uw],nrow(past_populations)),
      rep(uws[disease=='osteoporosis',uw],nrow(past_populations)),
      rep(uws[disease=='kidney_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='oesophageal_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='stomach_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='oral_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='pancreatic_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='uterine_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='ovarian_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='blood_cancer',uw],nrow(past_populations))
    )
  )
  
  uwFilter <- uw_plain * isCost 
  uwFilter[uwFilter==0]=1
  
  combined_uw <- 1-unlist(apply(simplify = F,(1-uwFilter), MARGIN = 1, function(x) prod(x,na.rm = T)))
  
  uw_all <- cbind(past_populations[,c('run', group_vars, 'intervention','year'),with = FALSE], uwFilter,combined_uw=unlist(combined_uw))
  
  uw_all_long <- melt( uw_all, id.vars = c('run',group_vars ,'intervention','year'), variable.name = 'disease', value.name = 'uw')
  
  #sum up all runs 
  uw_all_long <- uw_all_long[, .(total_uw = sum(uw)),  #,N=.N
                             by=c('run',group_vars, 'intervention','year','disease')]
  
  # mean over runs
  uw_all_long2 <- uw_all_long[,.(#N= mean(N)*model_specification$population$scale_down_factor,
    total_uw = mean(total_uw)*model_specification$population$scale_down_factor),
    by = c('year',group_vars,'intervention','disease')]
  
  uw_all_long2
  
}



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



add_multimorbidity_fn <- function(past_populations, group_vars= as.character(), cut_off_year=NULL){
  
  if(!is.null(cut_off_year)){
    past_populations <- past_populations[year == cut_off_year,]
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
  
  past_populations <- rowSums(isCost) %>% 
    cbind(past_populations, data.table(multimorbidity = .))
  
  past_populations
  
}





daly_yld_fn <- function(past_populations, group_vars= as.character(), year_cut_off=NULL){
  
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  dws <- data.table(
    disease = c( "stroke","chd","diabetes","hypothyroidism","asthma","copd", "non_diabetic_hyperglycaemia","chronic_kidney_disease","dementia", "heart_failure","lung_cancer","prostate_cancer","female_breast_cancer", "colorectal_cancer", "atrial_fibrillation","rheumatoid_arthritis","osteoarthritis", "epilepsy","osteoporosis","kidney_cancer","oesophageal_cancer", "stomach_cancer","oral_cancer","pancreatic_cancer","uterine_cancer", "ovarian_cancer","blood_cancer"
    ),
    dw = c( 0.118,0.124,0.049,0.051,0.043,0.150, 0.000,0.073,0.477, 0.201,0.451,0.100,0.200, 0.288, 0.035,0.230,0.165, 0.263,0.040,0.300,0.420, 0.380,0.320,0.540,0.240, 0.360,0.310
    )
  )
  
  isCost <- past_populations %>% 
    select(
      c(
        stroke,               chd,                   diabetes,             hypothyroidism,      
        asthma,               copd,                  non_diabetic_hyperglycaemia,                  chronic_kidney_disease,      
        dementia,             heart_failure,         lung_cancer,          prostate_cancer,     
        female_breast_cancer,         colorectal_cancer,          atrial_fibrillation,  rheumatoid_arthritis,
        osteoarthritis,       epilepsy,              osteoporosis,         kidney_cancer,        
        oesophageal_cancer,   stomach_cancer,        oral_cancer,          pancreatic_cancer,   
        uterine_cancer,       ovarian_cancer,        blood_cancer )) %>% 
    as.matrix() %>% 
    {.!=0}
  
  setDT(dws)
  
  dw_plain <- matrix( 
    nrow = nrow(past_populations), ncol = ncol(isCost), byrow=F,
    c(rep(dws[disease=='stroke',dw],nrow(past_populations)),
      rep(dws[disease=='chd',dw],nrow(past_populations)),
      rep(dws[disease=='diabetes',dw],nrow(past_populations)),
      rep(dws[disease=='hypothyroidism',dw],nrow(past_populations)),
      rep(dws[disease=='asthma',dw],nrow(past_populations)),
      rep(dws[disease=='copd',dw],nrow(past_populations)),
      rep(dws[disease=='non_diabetic_hyperglycaemia',dw],nrow(past_populations)),
      rep(dws[disease=='chronic_kidney_disease',dw],nrow(past_populations)),
      rep(dws[disease=='dementia',dw],nrow(past_populations)),
      rep(dws[disease=='heart_failure',dw],nrow(past_populations)),
      rep(dws[disease=='lung_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='prostate_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='female_breast_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='colorectal_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='atrial_fibrillation',dw],nrow(past_populations)),
      rep(dws[disease=='rheumatoid_arthritis',dw],nrow(past_populations)),
      rep(dws[disease=='osteoarthritis',dw],nrow(past_populations)),
      rep(dws[disease=='epilepsy',dw],nrow(past_populations)),
      rep(dws[disease=='osteoporosis',dw],nrow(past_populations)),
      rep(dws[disease=='kidney_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='oesophageal_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='stomach_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='oral_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='pancreatic_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='uterine_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='ovarian_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='blood_cancer',dw],nrow(past_populations))
    )
  )
  
  DWFilter <- dw_plain * isCost 
  combined_dw <- 1-unlist(apply(simplify = F,(1-DWFilter), MARGIN = 1, function(x) prod(x,na.rm = T)))
  
  dw_all <- cbind(  past_populations[,c('run', group_vars, 'intervention','year'), with = FALSE] ,
                    DWFilter,
                    combined_dw=unlist(combined_dw))
  
  print(1)
  print(names(dw_all))
  dw_all_long <- melt( dw_all, id.vars = c('run',group_vars,'intervention','year'), variable.name = 'disease', value.name = 'dw')
  print(2)
  dw_all_long <- dw_all_long[, .(total_dw = sum(dw)), by=c(group_vars,'intervention', 'run','year','disease')]
  
  dw_all_long2 <- dw_all_long[,.(total_dw = mean(total_dw)*model_specification$population$scale_down_factor), by = c(group_vars,'intervention','year','disease')]
  dw_all_long2
}





library(data.table)
library(fst)
library(readxl)

lifetables <- read_excel("modules/chart_update_module_4/data/lifetables.xlsx", 
                         sheet = "2020-2022", skip = 5)

ltnames <- c('age', 'mx', 'qx', 'lx', 'dx', 'ex' ,'sex')

lifetables <- rbind(
  lifetables[1:6] %>% 
    mutate(sex = 'Males') %>% 
    setnames(ltnames),
  
  lifetables[8:13] %>% 
    mutate(sex = 'Females') %>% 
    setnames(ltnames)
)

lifetables <- rbind(
  lifetables,
  data.frame(
    101:110,
    0.443038,
    0.362694,
    739.2,
    268.1,
    0,
    'males')%>% 
    setnames(ltnames),
  
  data.frame(
    101:110,
    0.496552,
    0.397790,
    1990.7,
    791.9,
    0,
    'females'
  ) %>% 
    setnames(ltnames)
)

setDT(lifetables)

calculate_daly_yll <- function(past_populations, group_vars= as.character(), year_cut_off = NULL){
  
  
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  pp <- setDT(past_populations)
  
  pp <- pp[death!=0,]
  
  setDT(lifetables)
  
  pp <- pp[
    lifetables[,.(age,sex,ex)],
    on = .(age, sex),
    ex := i.ex]
  
  deaths <- pp[,.(.N,yll=sum(ex,na.rm=T)),by=c('death_reason','run',group_vars,'intervention','year')]

  full_death_reasons <-  do.call(CJ, 
                                 lapply(X = c('year','run','death_reason',group_vars), 
                                        function(x){c(unique(deaths[[x]]))}) 
  )
  
  names(full_death_reasons) <- c('year', 'run','death_reason', group_vars)
  
  full_death_reasons[,intervention := ifelse(run>max(run)/2,'intervention','non-intervention')]
  
  full_death_reasons[deaths,on = c('year','run', group_vars, 'intervention','death_reason'), `:=` (yll = i.yll, N = i.N)] #, nomatch=0L
  
  full_death_reasons[is.na(yll), yll := 0]
  
  yll <- full_death_reasons[, .(yll = sum(yll,na.rm = T),N=sum(N)), by = c('run','intervention',group_vars,'death_reason','year')
  ][,.(yll = mean(yll,na.rm = T)), by = c('intervention',group_vars,'death_reason','year')]
  
  yll
  
}

