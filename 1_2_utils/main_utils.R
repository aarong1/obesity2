# functions ---

# read_joint_distribution_files
# read_theoretical_minimum_files
# show_survivor_prevalence_by_discrete_states
# show_survivor_prevalence_by_discrete_states_robust
# assign_year_minus_one_prevalence
# declare_absolute_incident_morbidity
# calculate_risk_of_morbidity
# calibration_plot
# plot_prevalence_by_group
# prep_fn
# load_dependencies
# apply_doh_disease_prevalence

select <- dplyr::select

read_theoretical_minimum_files <- function() {
  
  print('Reading Theoretical Minimum files')
  
  # read in theoretical minimums for each morbidity
cervical_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/cervical_theoretical_minimum.fst')
crc_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/crc_theoretical_minimum.fst')
breast_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/breast_theoretical_minimum.fst')
gallbladder_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/gallbladder_theoretical_minimum.fst')
kidney_cancer_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/kidney_cancer_theoretical_minimum.fst')
lung_cancer_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/lung_cancer_theoretical_minimum.fst')
oesophageal_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/oesophageal_theoretical_minimum.fst')
oral_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/oral_theoretical_minimum.fst')
ovarian_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/ovarian_theoretical_minimum.fst')
pancreatic_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/pancreatic_theoretical_minimum.fst')
uterine_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/uterine_theoretical_minimum.fst')
af_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/af_theoretical_minimum.fst')
chd_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/chd_theoretical_minimum.fst')
heart_failure_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/heart_failure_theoretical_minimum.fst')
stroke_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/stroke_theoretical_minimum.fst')
diabetes_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/diabetes_theoretical_minimum.fst')
dementia_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/dementia_theoretical_minimum.fst')
kidney_disease_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/kidney_disease_theoretical_minimum.fst')
aaa_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/aaa_theoretical_minimum.fst')
copd_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/copd_theoretical_minimum.fst')
asthma_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/asthma_theoretical_minimum.fst')
osteoarthritis_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/osteoarthritis_theoretical_minimum.fst')
rheumatoid_arthritis_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/rheumatoid_arthritis_theoretical_minimum.fst')
asthma_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/asthma_theoretical_minimum.fst')
osteoporotic_fracture_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/osteoporotic_fracture_theoretical_minimum.fst')
hip_fracture_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/hip_fracture_theoretical_minimum.fst')
rheumatoid_arthritis_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/rheumatoid_arthritis_theoretical_minimum.fst')
osteoarthritis_theoretical_minimum <<- read.fst( './3_pre_main/theoretical_minimum/osteoarthritis_theoretical_minimum.fst')
}


read_joint_distribution_files <- function(){
  
  print('Reading Joint Distribution files')
  
  bmi_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/bmi_stratified_prevalence.fst')
  child_bmi_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/child_bmi_stratified_prevalence.fst')
  smoking_results_df <<- read.fst( './3_pre_main/joint_distributions/smoking_results_df.fst')
  ecigarette_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/ecigarette_stratified_prevalence.fst')
  alcohol_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/alcohol_stratified_prevalence.fst')
  diet_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/diet_stratified_prevalence.fst')
  pa_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/pa_stratified_prevalence.fst')
  wellbeing_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/wellbeing_stratified_prevalence.fst')
  cholesterol_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/cholesterol_stratified_prevalence.fst')
  hypertension_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/hypertension_stratified_prevalence.fst')
  diabetes_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/diabetes_stratified_prevalence.fst')
  general_health_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/general_health_stratified_prevalence.fst')
  loneliness_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/loneliness_stratified_prevalence.fst')
  limiting_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/limiting_stratified_prevalence.fst')
  disability_stratified_prevalence <<- read.fst( './3_pre_main/joint_distributions/disability_stratified_prevalence.fst') 
  # special_cholesterol <<- read.fst( './3_pre_main/joint_distributions/special_cholesterol.fst') 
  special_cholesterol <<- qread('./3_pre_main/joint_distributions/special_cholesterol.fst')
  
  
}

show_survivor_prevalence_by_discrete_states <- function(input_population, morbidity, ...) {

  # this will show total number of survivors 
  # not the total number of strokes - of which ppl can have multiple
  
  input_population |> 
    group_by(run) |> 
    
    # must group by and sum by run
    # and then average by other facets

    group_by(...,.add = T) |> 
    summarise(counted_states = sum({{morbidity}} != 0), # !=0
              population = n(),
              .groups = "drop") |> 
    
    #   2.	For each year, compute
    # •	the mean of those counts
    # •	the standard deviation (sd) of those counts
    # •	the number of runs (n)
    # •	the standard error (se = sd/√n)
    # •	a 95 % CI via mean ± t₀.₀₇₅, (n − 1) × se
    
    group_by(...) |> 
    summarise(mean_counted_states = mean(counted_states), # !=0
              sd = sd(counted_states),
              se = sd/sqrt(model_specification$model$number_of_runs),
              # 95% t‐interval; df = runs - 1
              ci_lower    = mean_counted_states - qt(0.975, df = model_specification$model$number_of_runs - 1) * se,
              ci_upper    = mean_counted_states + qt(0.975, df = model_specification$model$number_of_runs - 1) * se,
              .groups = "drop")
  
}

###pos hoc analysis
show_survivor_prevalence_by_discrete_states_robust <- function(input_population, morbidity, ...) {
  
  # this will show total number of survivors 
  # not the total number of strokes - of which ppl can have multiple
  
  input_population |> 
    group_by(run) |> 
    
    # must group by and sum by run
    # and then average by other facets
    
    group_by(...,.add = T) |> 
    summarise(counted_states = sum({{morbidity}} != 0), # !=0
              .groups = "drop") |> 
    
    #   2.	For each year, compute
    # •	the mean of those counts
    # •	the standard deviation (sd) of those counts
    # •	the number of runs (n)
    # •	the standard error (se = sd/√n)
    # •	a 95 % CI via mean ± t₀.₀₇₅, (n − 1) × se
    
    group_by(...) |> 
    summarise(# mean_counted_states = mean(counted_states), # !=0
      sd = sd(counted_states),
      se = sd/sqrt(model_specification$model$number_of_runs),
      # 95% t‐interval; df = runs - 1
      ci_minus_lower    = qt(0.975, df = model_specification$model$number_of_runs - 1) * se,
      ci_plus_upper    =  + qt(0.975, df = model_specification$model$number_of_runs - 1) * se,
      mean_counted_states = mean(counted_states[abs(counted_states-mean(counted_states))<ci_minus_lower*2]),
      .groups = "drop")
  
}

# assign established prevalence ----
assign_year_minus_one_prevalence <- function(input_population,
                                             trust, 
                                             morbidity='cancer', 
                                             # year = 2023,
                                             prevalence_df = prevalence_hsct,
                                             configuration = model_specification
) {

  lookup <- c(
    'atrial_fibrillation' = 'Atrial Fibrillation',
    # 'lung_cancer' = 'Cancer',
    'cancer' = 'Cancer',
    'chronic_kidney_disease' = 'Chronic Kidney Disease',
    'chd' = 'Coronary Heart Disease',
    'dementia' = 'Dementia',
    'hypertension' = 'Hypertension',
    'stroke' = 'Stroke & TIA',
    'diabetes' = 'Diabetes Mellitus',
    'epilepsy' = 'Epilepsy',
    'pad' = 'Peripheral Arterial Disease',
    'rheumatoid_arthritis' = 'Rheumatoid Arthritis',
    'heart_failure' = 'Heart Failure',
    'non_diabetic_hyperglycaemia' = 'Non-Diabetic Hyperglycaemia',
    'depression'='Depression',
    'asthma' = 'Asthma',
    'copd' = 'Chronic Obstructive Pulmonary Disease')

  year <- min(input_population$year)-1
  # print(year)

  # print(morbidity)
  morbidity_as_in_prevalence_file <- (lookup[morbidity])
  # print(morbidity_as_in_prevalence_file)
  # print({{trust}})
  established_prevalence_year_label = min(input_population$year)#-1
  # print({{established_prevalence_year_label}})
  
  if(established_prevalence_year_label<2017){
    stop('Year too low - must be greater than 2017 to join with hsct prevalence')
  }
  if(!{{morbidity}} %in% names(lookup)){
    stop('Morbidity label not in list')
  }

  risk_col <- sym(paste0(morbidity, "_year_risk"))

  # print(risk_col)

  trust_rows <- input_population |> 
    filter(HSCT == {{trust}})
  
  (p <- prevalence_df |>
      filter(Disease == morbidity_as_in_prevalence_file) |>
      filter(Year == established_prevalence_year_label) |>
      filter(HSCT == trust )|>
      pull(prob)
  )
  
  # print(p)

  risk_vector <- trust_rows |> 
    pull(!!risk_col)
  
  n_cases = p*nrow(trust_rows)
  
  # print(n_cases)
  
  prob = n_cases-floor(n_cases)
  
  # print(prob)
  
  (stochastic_round_integer = sample(c(1,0),
                                     size = 1,
                                     prob = c(prob, 1-prob)
  ))
  
  
  print(risk_vector)
  print(floor(n_cases) + stochastic_round_integer)
  
  morbidity_prevalence_sampled_persons_ids <- sample(trust_rows$id,
                                                  size = (floor(n_cases) + stochastic_round_integer),
                                                  prob = risk_vector,
                                                  replace = F)
  
  print(morbidity_prevalence_sampled_persons_ids)
  
  input_population <- input_population |> 
    mutate(!!sym(morbidity) := ifelse(id %in% 
                            morbidity_prevalence_sampled_persons_ids,
                            established_prevalence_year_label ,
                           !!sym(morbidity))
    )

  return(input_population)
}

# prevalence_hsct |> 
#        filter(Disease == 'Atrial Fibrillation') |> 
#        filter(Year == 2020) |> 
#        filter(HSCT == 'BHSCT') |> 
#        pull(prob)

# x <- assign_year_minus_one_prevalence(
#   input_population = initial_time_zero_population ,
#   trust = 'BHSCT',
#   morbidity = 'asthma',
#   #year = 2017,
#   prevalence_df = prevalence_hsct_new,
#   configuration = model_specification
# )

# filter(x,copd!=0) %>% select(copd)

 #  { 
 #  x = assign_year_minus_one_prevalence(
 #    input_population = initial_time_zero_population ,
 #    trust = 'Belfast HSCT' ,
 #    morbidity =  'diabetes' ,
 #    year = 2017 ,
 #    prevalence_df = prevalence_hsct ,
 #    configuration = model_specification
 #  )
 
 
 #  count(initial_time_zero_population,diabetes)
 
 #  count(x,HSCT,diabetes)
 # }

# declare absolute morbidity ----
declare_absolute_incident_morbidity <- function(input_population = NULL,
                                                #transition_probability = NULL,
                                                morbidity = NULL){
  
  if( inherits(input_population, "rowwise_df")){
    input_population <- ungroup(input_population)
  }
  
  #sample(c(T,F),prob=c(transition_probability = 0.02,1-0.02),size=100, replace=T) 
  current_year = max(input_population$year)
  #morbidity = 'chronic_kidney_disease'
  suffers_col  <- sym(morbidity)    # turn “stroke”  →  symbol stroke
  risk_col <- sym(paste0(morbidity, "_year_risk"))
  history_col <- sym(paste0(morbidity, "_history"))
  recovered_col <- sym(paste0(morbidity, "_recovered"))
  
  #only stroke is recoverable and only then in the sense it is an acute onset.
  input_population <- input_population %>%
    mutate(bern_trial = runif(n())) |> 
    mutate(!!suffers_col :=
           # apply_morbidity
           # tyI
           # tyII
             case_when(
               #stroke can have multiple instances of acute episode - you'll only be a stroke survivor once !!
              ( morbidity == 'stroke') & (bern_trial < !!risk_col) ~ current_year,
               # if any other of the chronic diseases come back as recovered or has never suffered then calculate the risk
               # do not apply a new, or update an instance of disease if already suffering
               (morbidity != 'stroke') & (   (!!suffers_col==0) | is.na(!!suffers_col)) ~ current_year * (bern_trial < !!risk_col),
               #if sample comes back negative continue as is.
               T ~ !!suffers_col)
    )
}

declare_absolute_incident_morbidity <- function(input_population = NULL,
                                                #transition_probability = NULL,
                                                morbidity = NULL){
  
  if( inherits(input_population, "rowwise_df")){
    input_population <- ungroup(input_population)
  }
  
  #sample(c(T,F),prob=c(transition_probability = 0.02,1-0.02),size=100, replace=T) 
  current_year = max(input_population$year)
  #morbidity = 'chronic_kidney_disease'
  suffers_col  <- sym(morbidity)    # turn “stroke”  →  symbol stroke
  risk_col <- sym(paste0(morbidity, "_year_risk"))
  history_col <- sym(paste0(morbidity, "_history"))
  recovered_col <- sym(paste0(morbidity, "_recovered"))
  
#only stroke is recoverable and only then in the sense it is an acute onset.
input_population <- input_population %>%
  mutate(bern_trial = runif(n())) |> 
  mutate(!!suffers_col :=
           case_when(
             ( morbidity == 'non_diabetic_hyperglycaemia') & (bern_trial < !!risk_col) & (diabetes==0) ~ current_year,
             #stroke can have multiple instances of acute episode - you'll only be a stroke survivor once !!
             (morbidity == 'stroke') & (bern_trial < !!risk_col) ~ current_year,
             # if any other of the chronic diseases come back as recovered or has never suffered then calculate the risk
             # do not apply a new, or update an instance of disease if already suffering
             (!morbidity %in% c('stroke','non_diabetic_hyperglycaemia')) & ( (!!recovered_col == TRUE) | (!!suffers_col==0) | is.na(!!suffers_col)) ~ current_year * (bern_trial < !!risk_col),
             #if sample comes back negative continue as is.
             T ~ !!suffers_col)
  )
}

declare_absolute_incident_morbidity_alt <- function(input_population = NULL,
                                                #transition_probability = NULL,
                                                morbidity = NULL){
  
  if( inherits(input_population, "rowwise_df")){
    input_population <- ungroup(input_population)
  }
  
  #sample(c(T,F),prob=c(transition_probability = 0.02,1-0.02),size=100, replace=T) 
  current_year = max(input_population$year)
  #morbidity = 'chronic_kidney_disease'
  suffers_col  <- sym(morbidity)    # turn “stroke”  →  symbol stroke
  risk_col <- sym(paste0(morbidity, "_year_risk"))
  history_col <- sym(paste0(morbidity, "_history"))
  recovered_col <- sym(paste0(morbidity, "_recovered"))
  
  risk_col_name <- paste0(morbidity, "_year_risk")
  i <- as.data.table(input_population)

  eligible <- i[get(as.character(suffers_col)) == 0]
  if (nrow(eligible) == 0) {
    return(input_population)
  }

  # Expected incident case count from summed individual annual probabilities.
  x <- sum(eligible[[risk_col_name]], na.rm = TRUE)
  remainder <- x - floor(x)

  print(input_population %>%
          filter(!!suffers_col==0) %>%
          summarise(sum(!!risk_col)))

  print(paste(suffers_col, 'x: ', x, 'remainder:' , remainder, 'floor: ', floor(x) ))

  # Unbiased integer draw around x: floor with prob (1-r), ceiling with prob r.
  y <- sample(x = c(floor(x), floor(x) + 1), size = 1, prob = c(1 - remainder, remainder))
  y <- max(0, min(y, nrow(eligible)))

  weights <- eligible[[risk_col_name]]
  weights[is.na(weights) | weights < 0] <- 0
  if (sum(weights) <= 0 || y == 0) {
    ids <- integer(0)
  } else {
    ids <- eligible[sample.int(.N, size = y, replace = FALSE, prob = weights), id]
  }

  input_population <- input_population %>%
    mutate(!!suffers_col := ifelse(id %in% ids,
                            current_year,
                            !!suffers_col))
   
   return(input_population)
     
    
  #only stroke is recoverable and only then in the sense it is an acute onset.
  # input_population <- input_population %>%
  #   mutate(bern_trial = runif(n())) |>
  #   mutate(!!suffers_col :=
  #            case_when(
  #              ( morbidity == 'non_diabetic_hyperglycaemia') & (bern_trial < !!risk_col) & (diabetes==0) ~ current_year,
  #              #stroke can have multiple instances of acute episode - you'll only be a stroke survivor once !!
  #              (morbidity == 'stroke') & (bern_trial < !!risk_col) ~ current_year,
  #              # if any other of the chronic diseases come back as recovered or has never suffered then calculate the risk
  #              # do not apply a new, or update an instance of disease if already suffering
  #              (!morbidity %in% c('stroke','non_diabetic_hyperglycaemia')) & ( (!!recovered_col == TRUE) | (!!suffers_col==0) | is.na(!!suffers_col)) ~ current_year * (bern_trial < !!risk_col),
  #              #if sample comes back negative continue as is.
  #              T ~ !!suffers_col)
  #   )
   
}

# current_year = max(current_population$year)
# current_year = 2023
# current_population %>%
#   mutate(bern_trial = runif(n())) |>
#   rowwise() %>%
#   mutate(asthma =
#            case_when(
#              #stroke can have multiple instances of acute episode - you'll only be a stroke survivor once !!
#              #( morbidity == 'stroke') & (bern_trial < asthma_year_risk) ~ current_year,
#              # if any other of the chronic diseases come back as recovered or has never suffered then calculate the risk
#              # do not apply a new, or update an instance of disease if already suffering
#                 asthma==0  ~ current_year * (bern_trial < asthma_year_risk),
#              T ~ asthma)
#   ) %>%
#   count(asthma)
# 
# current_population %>%
#   declare_absolute_incident_morbidity(morbidity = "asthma") %>%
#   count(asthma)
# input_population=current_population
# count(input_population,rheumatoid_arthritis)

# calculate risk of morbidity ----
calculate_risk_of_morbidity <- function(input_population) {

  print('Calculating risk for serious morbidity' )
  print('Calculating morbidity risk to initial time zero population')
  
  #CVD
  initial_time_zero_population <- initial_time_zero_population %>% 
    #apply_cvd_risk() %>%.
    apply_cvd_risk_wo_risk_factors() %>% 
    mutate(qrisk_year_risk = transform_10y_probability_to_1y(qrisk_score)) 
  
  # initial_time_zero_population <- initial_time_zero_population %>%
  #   apply_cvd_risk_wo_risk_factors_dt()
  # # initial_time_zero_population$qrisk_score  = NULL
  # initial_time_zero_population <- initial_time_zero_population [
  #   , qrisk_year_risk := transform_10y_probability_to_1y(qrisk_score)
  # ]
  
  #graph_inspect_apply_risk(initial_time_zero_population, age, qrisk_year_risk, bmi, facet_formula = ~sex)
  
  #STROKE 1
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_stroke_risk_wo_risk_factors() %>% 
    mutate(stroke_year_risk = transform_10y_probability_to_1y(stroke_risk))# %>% 
  # declare_absolute_incident_morbidity(morbidity = 'stroke')
  ## function to convert risk of morbidity each year to the absolute declaration of state occupancy ----
  
  #CHD 2
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_chd_risk() %>%
    mutate(chd_year_risk = transform_10y_probability_to_1y(chd_risk)) 
  
  #DIABETES 3
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_diabetes_risk_wo_risk_factors() %>%
    mutate(diabetes_year_risk = transform_10y_probability_to_1y(diabetes_risk))
  
  #NDH 11
  initial_time_zero_population <- initial_time_zero_population %>%
    mutate(non_diabetic_hyperglycaemia_year_risk = diabetes_year_risk)
  
  #DEMENTIA 4 - Two implementations
  #  - UKBDRS
  #  - DRS
  
  initial_time_zero_population <- initial_time_zero_population %>%
    # apply_dementia_ukbdrs_14yr_risk_wo_risk_factors() %>%
    apply_dementia_drs_5yr_risk_wo_risk_factors() %>%
    # mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 14))
    mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 5))
  
  ##HEART FAILURE 5
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_hf_risk_wo_risk_factors() %>% 
    mutate(heart_failure_year_risk = transform_probability_to_1y(hf_risk, tot_years = 4))
  
  ##HYPERTENSION 6
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_hypertension_risk_wo_risk_factors() %>%
    mutate(hypertension_year_risk = transform_probability_to_1y(hypertension_risk, tot_years = 4))
  
  ##ATRIAL FIBRILLATION  7
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_af_risk_wo_risk_factors() %>%
    mutate(atrial_fibrillation_year_risk = transform_probability_to_1y(af_risk, tot_years = 10))
  
  #CHRONIC KIDNEY DISEASE 8
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_ckd_risk_wo_risk_factors() %>%
    mutate(chronic_kidney_disease_year_risk = transform_probability_to_1y(ckd_risk, tot_years = 5))
  
  ############  Respiratory ############ 
  
  # # asthma
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_asthma_risk()
  
  # # COPD
  initial_time_zero_population<- initial_time_zero_population %>%
    apply_copd_risk()
  
  ################# Other CVD ################# 
  
  ##PERIPHERAL ARTERIAL DISEASE
  initial_time_zero_population<- initial_time_zero_population %>%
    apply_pad_risk_wo_risk_factors() %>%
    mutate(pad_year_risk = transform_probability_to_1y(pad_risk, tot_years = 4))
  
  ##VENOUS THROMBOEMBELISM
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_vte_risk_wo_risk_factors() %>%
    mutate(vte_year_risk = transform_probability_to_1y(vte_risk, tot_years = 5))
  
  ################# Other  ################# 
  
  # # Depression
  initial_time_zero_population <- initial_time_zero_population %>%
    mutate(depression_year_risk = depression_percentile)
  
  # # EPILEPSY
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_epilepsy_risk()
  
  # ABDOMINABLE AORTIC ANEURYSM
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_aaa_risk()
  
  # INTERSISTAL LUNG DISEASE
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_ILD_risk()
  
  # T1DM
  
  # OSTEOPOROSIS
  # https://fingertips.phe.org.uk/static-reports/health-trends-in-england/England/musculoskeletal_health.html
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_osteoporosis_risk()
  
  # HYPOTHYROIDISM
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_hypothyroid_risk()
  
  #INFLAMMATORY BOWEL DISEASE
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_ibd_risk()
  
  #NAFLD
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_liver_disease_risk_wo_risk_factors() %>% 
    mutate(nafld_year_risk = transform_probability_to_1y(nafld_risk, tot_years = 10))
  
  #ARLD
  
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_arld_disease_risk_wo_risk_factors() %>% 
    mutate(arld_year_risk = transform_probability_to_1y(arld_risk, tot_years = 10))
  
  #SLE
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_sle_risk()
  
  # RHEUMATOID ARTHRITIS
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_rheumatoid_arthritis_risk(rheumatoid_arthritis_incidence)
  
  # OSTEOARTHRITIS
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_osteoarthritis_risk(osteoarthritis_incidence)
  
  
  
  # GLAUCOMA
  # CATARACTS
  # https://jamanetwork.com/journals/jamaophthalmology/fullarticle/261561
  
  ############### CANCERS ################# 
  
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_cancer_risk() 
  
  #LUNG CANCER 9
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_lungcancer_risk_wo_risk_factors() %>% 
    mutate(lung_cancer_year_risk = transform_probability_to_1y(lungcancer_risk, tot_years = 5))
  
  # # #COLORECTAL CANCER
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_colorectal_cancer_risk_wo_risk_factors() %>%
    mutate(colorectal_cancer_year_risk = transform_probability_to_1y(colorectal_cancer_risk, tot_years = 5))
  
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_brain_cancer_risk_wo_risk_factors() 
  
  initial_time_zero_population <- populate_brain_cancer_prevalence(initial_time_zero_population)
  
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_cervical_cancer_risk_wo_risk_factors()
  
  initial_time_zero_population <- populate_cervical_cancer_prevalence(initial_time_zero_population)
  
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_breast_cancer_risk_wo_risk_factors() %>%
    mutate(female_breast_cancer_year_risk = transform_probability_to_1y(breast_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_prostate_cancer_risk_wo_risk_factors() %>%
    mutate(prostate_cancer_year_risk = transform_probability_to_1y(prostate_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_pancreatic_cancer_risk_wo_risk_factors() %>%
    mutate(pancreatic_cancer_year_risk = transform_probability_to_1y(pancreatic_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_renal_cancer_risk_wo_risk_factors() %>%
    mutate(renal_cancer_year_risk = transform_probability_to_1y(renal_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_uterian_cancer_risk_wo_risk_factors() %>%
    mutate(uterine_cancer_year_risk = transform_probability_to_1y(uterine_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_ovarian_cancer_risk_wo_risk_factors() %>%
    mutate(ovarian_cancer_year_risk = transform_probability_to_1y(ovarian_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_blood_cancer_risk_wo_risk_factors() %>%
    mutate(blood_cancer_year_risk = transform_probability_to_1y(blood_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>%
    apply_osteogastric_cancer_risk_wo_risk_factors() %>%
    mutate(osteogastric_cancer_year_risk = transform_probability_to_1y(osteogastric_cancer_risk, tot_years = 5))
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_oral_cancer_risk_wo_risk_factors() %>%
    mutate(oral_cancer_year_risk = transform_probability_to_1y(oral_cancer_risk, tot_years = 5))
  
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_colorectal_cancer_risk_wo_risk_factors() %>%
    mutate(colorectal_year_risk = transform_probability_to_1y(colorectal_cancer_risk, tot_years = 5))
  
  initial_time_zero_population <- initial_time_zero_population %>% 
    apply_lung_cancer_risk_wo_risk_factors() %>%
    mutate(lung_cancer_year_risk = transform_probability_to_1y(lungcancer_risk, tot_years = 5))
  
  #FALLS 
  
  # source("./risk_correct_eq/risk_qfracture_hip_wrist_shoulder_spine.R")
  
  # # OESTEOPOROSIS FRACTURE OF THE HIP ( WRIST, SHOULDER, HIP, SPINE)
  # https://fingertips.phe.org.uk/static-reports/health-trends-in-england/England/musculoskeletal_health.html
  # initial_time_zero_population <- initial_time_zero_population %>% #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
  #   apply_fracture4_risk_wo_risk_factors() %>% 
  #   mutate(fracture4_year_risk = transform_probability_to_1y(fracture4_risk, tot_years = 10))
  
  # ggplot(initial_time_zero_population) +
  #   geom_point(aes(age,
  #                  fracture4_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
  #                  col=bmi)) +
  #   facet_grid(~sex)
  
  # # OESTEOPOROSIS FRACTURE OF THE NECK OF FEMUR
  # source("./risk_correct_eq/risk_qfracture_neck_of_femur.R")
  
  # initial_time_zero_population <- initial_time_zero_population %>% #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
  #   ungroup() %>% 
  #   apply_nof_risk_wo_risk_factors() %>% 
  #   mutate(nof_year_risk = transform_probability_to_1y(nof_risk, tot_years = 10))
  
  

}


##############################################################################################
###################################### Post-hoc Analysis #####################################
##############################################################################################

calibration_plot <- function(input_population,
                             model_specification = model_specification,
                             prevalence_hsct_new, 
                             morbidity, 
                             Disease,
                             save_plot=FALSE) {
  
  (qof_prevalence <- filter(prevalence_hsct_new,Disease=={{Disease}}))
  
  filter(prevalence_hsct_new,Year==2023) |> 
    select( - any_of(c(Year, Disease,Interpolated,Per1k,pop_ni)))
  
  (prevalence_by_trust <- show_survivor_prevalence_by_discrete_states(input_population = input_population,
                                                                             morbidity = {{morbidity}},
                                                                             year,
                                                                             HSCT)|> 
      mutate(mean_counted_states_scaled_to_population = mean_counted_states * model_specification$population$scale_down_factor) |> 
      mutate(ci_upper_counted_states_scaled_to_population = ci_upper * model_specification$population$scale_down_factor) |> 
      mutate(ci_lower_counted_states_scaled_to_population = ci_lower * model_specification$population$scale_down_factor)
  ) 
  
  plot_prevalence_by_trust <- prevalence_by_trust |> 
    ggplot() +
    expand_limits(y = 0) +
    
    geom_ribbon(mapping = aes(x = year,
                              y = mean_counted_states_scaled_to_population,
                              ymin = ci_lower_counted_states_scaled_to_population, 
                              ymax = ci_upper_counted_states_scaled_to_population,
                              color = HSCT,
                              fill = HSCT ),
                alpha=0.5) +
    scale_color_manual(values = pastel_colors) +
    scale_fill_manual(values = pastel_colors) +
    geom_line(aes(year,mean_counted_states_scaled_to_population,col=HSCT,group=HSCT)) +
    theme_minimal() +
    facet_wrap(~HSCT,scales = 'free_y') +
    ylab('Raw Prevalence') +
    
    geom_line(data = qof_prevalence, mapping = aes(as.numeric(Year), Count,col=HSCT,group=HSCT, linetype = "Calibrated"), show.legend = TRUE ) +
    scale_linetype_manual(name = "Data Source", values = c("Calibrated" = "dashed")) +
    labs(caption = rlang::as_label(rlang::enquo(morbidity)))+
    ggtitle(paste0(Disease,' Prevalence, estimated and calibrated'), subtitle = 'By Health and Social Care Trust')
  
  if ( save_plot ) {
    
    ggsave(
      create.dir = T,
      filename = paste0('./plot_figure/',rlang::as_label(rlang::enquo(morbidity)),'_prevalence_estimated_calibrated.png'),
      plot = plot_prevalence_by_trust,
      bg = 'white',
      width = 10,
      height = 5
    ) 
    }
  
  print(plot_prevalence_by_trust)
  return(prevalence_by_trust)
}

# calibration_plot(past_populations, prevalence_hsct_new, morbidity = chronic_kidney_disease, Disease = 'Chronic Kidney Disease')
graph_inspect_apply_risk <- function(initial_time_zero_population, age, risk, facet, facet_formula = NULL) {
  graph <- ggplot(initial_time_zero_population) +
    geom_point(aes({{age}}, {{risk}}, col = {{facet}}))
  
  if (!is.null(facet_formula)) {
    graph <- graph + facet_grid(facet_formula)
  }
  
  return(graph)
}; 

plot_prevalence_by_group <- function(input_population,
                                     morbidity,
                                     facet_var,
                                     scale_down_factor = model_specification$population$scale_down_factor,
                                     y_label = "Raw Prevalence",
                                     legend_title = NULL,
                                     title = NULL,
                                     subtitle = NULL,
                                     save_plot=FALSE) {
  
  require(dplyr)
  require(ggplot2)
  require(rlang)
  
  input_population <- input_population |> 
    mutate(
      age20 = cut(age,
                  include.lowest = T,
                  breaks = c(0 , 40 , 60 , 80 , 120 ),
                  labels = c('0-40',
                             '40-60',
                             '60-80',
                             '80+')
      )
    )
  
  facet_sym <- rlang::enquo(facet_var)
  facet_str <- rlang::as_name(facet_sym)
  
  print(rlang::as_label(rlang::enquo(morbidity)))
  
  prevalence_df <- show_survivor_prevalence_by_discrete_states(input_population = input_population, #past_populations,
                                                              morbidity = {{morbidity}},
                                                              #stroke,
                                                              year,
                                                              !!facet_sym)|> 
    mutate(mean_counted_states_scaled_to_population = mean_counted_states * model_specification$population$scale_down_factor) |> 
    mutate(ci_upper_counted_states_scaled_to_population = ci_upper * model_specification$population$scale_down_factor) |> 
    mutate(ci_lower_counted_states_scaled_to_population = ci_lower * model_specification$population$scale_down_factor)

  prevalence_df <- prevalence_df|>
    mutate(
      mean_counted_states_scaled_to_population = mean_counted_states * scale_down_factor,
      ci_upper_counted_states_scaled_to_population = ci_upper * scale_down_factor,
      ci_lower_counted_states_scaled_to_population = ci_lower * scale_down_factor
    )
  
  prevalence_plot <- ggplot(prevalence_df) +
    expand_limits(y = 0) +
    geom_ribbon(
      aes(
        x = year,
        y = mean_counted_states_scaled_to_population,
        ymin = ci_lower_counted_states_scaled_to_population,
        ymax = ci_upper_counted_states_scaled_to_population,
        color = as.character(!!facet_sym),
        fill = as.character(!!facet_sym)
      ),
      alpha = 0.5
    ) +
    geom_line(
      aes(
        x = year,
        y = mean_counted_states_scaled_to_population,
        color = as.character(!!facet_sym),
        group = as.character(!!facet_sym)
      )
    ) +
    scale_color_manual(values = pastel_colors, name = facet_str %||% legend_title) +
    scale_fill_manual(values = pastel_colors, name = facet_str %||% legend_title) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
    theme_minimal() +
    ylab(y_label) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    facet_wrap(facets = vars(!!facet_sym), scales = "fixed") +
    labs(caption = rlang::as_label(rlang::enquo(morbidity)))+
    ggtitle(
      label = title %||% paste("Prevalence"),
      subtitle = subtitle %||% rlang::as_label(rlang::enquo(morbidity))
    )
  
  if ( save_plot ) {
    
    print('Plot')
    
    ggsave(
      create.dir = T,
      filename = paste0('./plot_figure/',rlang::as_label(rlang::enquo(morbidity)),'_prevalence_by_',facet_str,'.png'),
      plot = prevalence_plot,
      bg = 'white',
      width = 10,
      height = 5
    ) }
  
  print(prevalence_plot)
  return(prevalence_df)
}

 #plot_prevalence_by_group(past_populations, chronic_kidney_disease, facet_var = !!sym('HSCT'))

#############################Return closure of cholesterol_ratio
##############Return closure of transition function

prep_fn <- function(generic_incidence_multiplier = 1,
                    generic_remission_multiplier = 1,
                    af_intervention_multiplier=1,
                    intervention = 1) {
  
  transition_lifestyle_physiological_parameters <- function(current_population,
                                                            .dummy,
                                                            transition_prob_incidence_multiplier = generic_incidence_multiplier,
                                                            transition_prob_remission_multipliers = generic_remission_multiplier,
                                                            af_intervention = af_intervention_multiplier,
                                                            intervention= 1){
    
    print(paste( 'Year : ' ,.dummy))
    #Increment year
    
    current_population <- current_population %>% 
      filter(year == max(year)) %>% 
      mutate(year = year+1)
    
    print('Transitioning lifestyle parameters')
    
    current_population <- current_population %>% 
      transition_smoking_lifestyle_parameter() %>% 
      transition_bmi_lifestyle_parameter(
        transition_prob_incidence_multiplier = 1-0.06*transition_prob_incidence_multiplier#,
        #transition_prob_remission_multiplier=1-0.06*transition_prob_incidence_multiplier,
      )  %>% 
      transition_cholesterol_physiological_parameter(
        #transition_prob_incidence_multiplier=1-0.06*transition_prob_incidence_multiplier
        #transition_prob_remission_multipliers=1+0.04*0
      ) %>% 
      transition_hypertension_physiological_parameter(
        #transition_prob_incidence_multiplier=1+0.04*0,
        #transition_prob_remission_multiplier=1+0.04*0
      ) %>%
      transition_diabetes_physiological_parameter() #%>% 
    # apply_cvd_risk() %>% #names() #intervention=af_intervention
    # mutate(year_cvd_risk = transform_10y_probability_to_1y(qrisk_score)) %>% 
    # apply_stroke_risk() %>% #intervention=af_intervention_multiplier
    # mutate(year_stroke_risk = transform_10y_probability_to_1y(stroke_risk)) 
    
    #current_population <- rbind(current_population,current_population)
    return(current_population)
    
  }
  
  return(transition_lifestyle_physiological_parameters)
  
}


apply_doh_disease_prevalence <- function(input_population,morbidity = 'osteoporosis'){
  
  morbidity_lookup = case_when(morbidity == 'osteoporosis' ~ 'Osteoporosis',
                               morbidity == 'rheumatoid_arthritis' ~ 'Rheumatoid Arthritis',
                               morbidity == 'hypothyroidism' ~ 'Hypothyroidism',
                               morbidity == 'epilepsy' ~ 'Epilepsy',
                               morbidity == 'pad' ~ 'Peripheral Arterial Disease'
  )
  
  prevalence_per_cap <- prevalence_per_cap %>% 
    pivot_longer(-1) %>% 
    set_names(c('disease','year','per_cap'))
  
  prevalence_per_cap %>% 
    group_by(disease) %>% 
    fill(per_cap,.direction = 'down') %>%
    filter(year == last(year)) %>% 
    filter(disease == morbidity_lookup) -> prevalence_lookup
  
  setDT(input_population) 
  
  col = paste0(morbidity,'_year_risk')
  
  
  # print(prevalence_lookup$per_cap)
  
  year = min(input_population$year)
  print(year)
  
  input_population[,morbidity_percentile := 
                     frank(ties.method = 'random',x = get(col))/
                     max(frank(ties.method = 'random',x = get(col)))]
  
  print(prevalence_lookup)
  
  input_population[,(morbidity) := year*(2*as.numeric(prevalence_lookup$per_cap)/1000*morbidity_percentile>runif(.N)) ]
  
  input_population
  # count(initial_time_zero_population,osteoporosis,nn=n()) %>% mutate(n/nn)
}
# initial_time_zero_population %>% apply_doh_disease_prevalence(morbidity = 'osteoporosis') %>% count(osteoporosis) #%>% pull(osteoporosis)*2023
# population_w_established_prevalencev%>% count(osteoporosis) 
# x1 %>% 
  # apply_doh_disease_prevalence('osteoporosis') %>% count(osteoporosis)
  # apply_doh_disease_prevalence('hypothyroidism') %>% 
  # apply_doh_disease_prevalence('pad') 


load_dependencies <- function() {
  
  initial_time_zero_population <<- read.fst('./main/initial_time_zero_population10down.fst')
  
  source('./synthetic_population/1_10_temp_synthetic_population.R')
  
  ############################# Baseline Population  ############################## 
  
  source('./risk_correlation.R')
  
  ############## Death and Deceased #####################
  
  source('./deaths_module/death_operators/apply_age_sex_death.R')
  source('./deaths_module/death_operators/qmortality.R')
  
  ################# Utility ################# 
  source('./transform_probability.R')
  
  ############################ RISK CALCULATIONS #########################
  
  ################## Qrisk3 - CBVD (Stroke/TIA) + CHD (MI/Angina) ######################
  source('./disease_equation/apply_qrisk_score.R') 
  source('./apply_chd_risk.R') 
  
  ################## 

  #CVD/Dementia/Fracture/LiverDisease
  source("./disease_equation/risk_qstroke_stroke_1_3.R")
  source("./disease_equation/risk_qdiabetes_diabetes.R")
  source("./disease_equation/risk_framingham_congestive_heart_failure.R")
  source("./disease_equation/risk_qkidney_chronic_kidney_disease_severe.R")
  source("./disease_equation/risk_framingham_hypertension.R")
  source("./disease_equation/risk_framingham_atrial_fibrillation.R")
  source("./disease_equation/risk_qthrombosis_venal_thromboembelism.R")
  source("./disease_equation/risk_framingham_peripheral_arterial_disease.R")
  
  source("./disease_equation/risk_ukbdrs_dementia.R")
  source("./disease_equation/risk_drs_dementia.R")
  
  source("./disease_equation/risk_qfracture_fracture.R")
  source('./disease_equation/risk_qfracture_neck_of_femur.R')
  
  source("./disease_equation/risk_framingham_liver_disease.R")
  
  #Cancer
  source('./disease_equation/risk_qcancer_lungcancer.R')
  source("./disease_equation/site_cancers/renal_cancer.R")
  source("./disease_equation/site_cancers/prostate_male_cancer.R")
  source("./disease_equation/site_cancers/pancreatic_cancer.R")
  source("./disease_equation/site_cancers/ovarian_female_cancer.R")
  source("./disease_equation/site_cancers/oral_cancer.R")
  source("./disease_equation/site_cancers/oesteogastric_cancer.R")
  source("./disease_equation/site_cancers/breast_female_cancer.R")
  source("./disease_equation/risk_qcancer_colorectal.R")
  source("./disease_equation/site_cancers/colorectal_cancer.R")
  source("./disease_equation/site_cancers/blood_cancer.R")
  source("./disease_equation/site_cancers/uterine_female_cancer.R")
  
  #Resp/epilepsy/MSK/AAA/LowBirthWeight
  source("disease_engines/cancer_engine.R")
  source("disease_engines/asthma_engine.R")
  source("disease_engines/copd_engine.R")
  source("disease_engines/epilepsy_engine.R")
  source("disease_engines/RA_engine.R")
  source("disease_engines/osteoporosis_engine.R")
  source("disease_engines/osteoarthritis_engine.R")
  
  source("disease_engines/aortic_aneurysm_engine.R")
  source("disease_engines/low_birth_weight_engine.R")
  source("disease_engines/Intersistal_lung_disease_engine.R")
  
  source("./disease_prevalence.R")
}

# load_dependencies()

# apply_doh_disease_prevalence <- function(input_population,morbidity = 'osteoporosis'){
#   
#  morbidity_lookup = case_when(morbidity == 'osteoporosis' ~ 'Osteoporosis',
#                               morbidity == 'rheumatoid_arthritis' ~ 'Rheumatoid Arthritis',
#                               morbidity == 'hypothyroidism' ~ 'Hypothyroidism',
#                               morbidity == 'epilepsy' ~ 'Epilepsy',
#                               morbidity == 'pad' ~ 'Peripheral Arterial Disease',
#                               morbidity == 'peripheral_arterial_disease' ~ 'Peripheral Arterial Disease'
#                               
#             )
#  
#   prevalence_per_cap <- prevalence_per_cap %>% 
#     pivot_longer(-1) %>% 
#     set_names(c('disease','year','per_cap'))
#   
#     prevalence_per_cap %>% 
#     group_by(disease) %>% 
#     fill(per_cap,.direction = 'down') %>%
#     filter(year == last(year)) %>% 
#     filter(disease == morbidity_lookup) -> prevalence_lookup
#     
#     setDT(input_population) 
#     
#     col = paste0(morbidity,'_year_risk')
#     
#     # print(prevalence_lookup$per_cap)
#     
#     input_population[,morbidity_percentile := 
#                                    frank(ties.method = 'random',x = get(col))/
#                                     max(frank(ties.method = 'random',x = get(col)))]
#     
#     input_population[,(morbidity) := 2*as.numeric(prevalence_lookup$per_cap)/1000*morbidity_percentile>runif(.N)]
#     
#       # count(initial_time_zero_population,osteoporosis,nn=n()) %>% mutate(n/nn)
# }

