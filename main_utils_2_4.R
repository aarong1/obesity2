
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
                                             morbidity='lung_cancer', 
                                             #year = 2017,
                                             prevalence_df = prevalence_hsct,
                                             configuration = model_specification
) {
  
  lookup <- c(
    'atrial_fibrillation' = 'Atrial Fibrillation',
    'lung_cancer' = 'Cancer',
    'chronic_kidney_disease' = 'Chronic Kidney Disease',
    'chd' = 'Coronary Heart Disease',
    'dementia' = 'Dementia',
    'hypertension' = 'Hypertension',
    'stroke' = 'Stroke & TIA',
    'diabetes' = 'Diabetes Mellitus',
    'heart_failure' = 'Heart Failure')
  
  print(morbidity)
  morbidity_as_in_prevalence_file <- (lookup[morbidity])
  print(morbidity_as_in_prevalence_file)
  print({{trust}})
  established_prevalence_year_label = min(input_population$year)#-1
  print({{established_prevalence_year_label}})
  
  
  
  if(established_prevalence_year_label<2017){
    stop('Year too low - must be greater than 2017 to join with hsct prevalence')
  }
  if(!{{morbidity}} %in% names(lookup)){
    stop('Morbidity label not in list')
  }
  
  risk_col <- sym(paste0(morbidity, "_year_risk"))
  
  print(risk_col)
  
  trust_rows <- input_population |> 
    filter(HSCT == {{trust}})
  
  (p <- prevalence_df |> 
      filter(Disease == {{morbidity_as_in_prevalence_file}}) |> 
      filter(Year == established_prevalence_year_label) |> 
      filter(HSCT == {{trust}}) |> 
      pull(prob)
  )
  
  print(p)
  
  risk_vector <- trust_rows |> 
    pull(!!risk_col)
  
  n_cases = p * nrow(trust_rows)
  
  print(n_cases)
  
  prob = n_cases - floor(n_cases)
  
  print('#########')
  print(prob)
  print('#########')
  
  
  (stochastic_round_integer = sample(c(1,0),
                                     size = 1,
                                     prob = c(prob, 1-prob)
  ))
  
  print(floor(n_cases) + stochastic_round_integer)
  
  morbidity_prevalence_sampled_persons_ids <- sample(trust_rows$id,
                                                     size = (floor(n_cases) + stochastic_round_integer),
                                                     prob = risk_vector,
                                                     replace = F)
  
  print(morbidity_prevalence_sampled_persons_ids)
  
  input_population <- input_population |> 
    mutate(!!sym(morbidity) := ifelse(id %in% 
                                        morbidity_prevalence_sampled_persons_ids,
                                      established_prevalence_year_label,
                                      !!sym(morbidity))
    )
  
  
  return(input_population)
}

#  { 
#  x = assign_year_minus_one_prevalence(
#    input_population = initial_time_zero_population ,
#    trust = 'Belfast HSCT' ,
#    morbidity =  'diabetes' ,
#    year = 2017 ,
#    prevalence_df = prevalence_hsct ,
#    configuration = model_specification
#  )
#   
# 
#  count(initial_time_zero_population,diabetes)
#  
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
             case_when(
               #stroke can have multiple instances of acute episode - you'll only be a stroke survivor once !!
               ( morbidity == 'stroke') & (bern_trial < !!risk_col) ~ current_year,
               # if any other of the chronic diseases come back as recovered or has never suffered then calculate the risk
               # do not apply a new, or update an instance of disease if already suffering
               (morbidity != 'stroke') & ( (!!recovered_col == TRUE) | (!!suffers_col==0) | is.na(!!suffers_col)) ~ current_year * (bern_trial < !!risk_col),
               #if sample comes back negative continue as is.
               T ~ !!suffers_col)
    )
  
  #How it works
  #	sym(x) turns the string x into a symbol that mutate() can use on the LHS or RHS.
  #	!!suffers_col := ... is the tidy-eval “unquote-bang” plus the := operator, which lets you create a column whose name is stored in suffers_col.
  #	On the RHS, !!risk_col unquotes the corresponding risk column.
  
  #return(input_population)
  
}

# calculate risk of morbidity ----
calculate_risk_of_morbidity <- function(input_population) {
  
  print('Calculating risk for serious morbidity' )
  
  
  
  #CVD
  input_population <- input_population %>% 
    apply_cvd_risk_wo_risk_factors() |> 
    mutate(qrisk_year_risk = transform_10y_probability_to_1y(qrisk_score)) |> nrow()
    
    #STROKE 1
    apply_stroke_risk_wo_risk_factors() |> 
    mutate(stroke_year_risk = transform_10y_probability_to_1y(stroke_risk)) |> 
    
    #CHD 2
    apply_chd_risk() |>
    mutate(chd_year_risk = transform_10y_probability_to_1y(chd_risk)) |> 
    
    #DIABETES 3
    apply_diabetes_risk_wo_risk_factors() |>
    mutate(diabetes_year_risk = transform_10y_probability_to_1y(diabetes_risk)) |> 
    
    #DEMENTIA 4
    # apply_dementia_risk_wo_risk_factors() |>
    apply_dementia_ukbdrs_14yr_risk_wo_risk_factors() |> 
    mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 14)) |> 
    #apply_dementia_drs_5yr_risk_wo_risk_factors() |>
    mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 5)) |> 
    
    
    ##HEARTFAILURE 5
    apply_hf_risk_wo_risk_factors() |> 
    mutate(heart_failure_year_risk = transform_probability_to_1y(hf_risk, tot_years = 4)) |> 
    
    ##HYPERTENSION 6
    apply_hypertension_risk_wo_risk_factors() |>
    mutate(hypertension_year_risk = transform_probability_to_1y(hypertension_risk, tot_years = 4)) |> 
    
    ##ATRIAL FIBRILLATION  7
    apply_af_risk_wo_risk_factors() |> 
    mutate(atrial_fibrillation_year_risk = transform_probability_to_1y(af_risk, tot_years = 10)) |> 
    
    #CHRONIC KIDNEY DISEASE 8
    apply_ckd_risk_wo_risk_factors() |>
    mutate(chronic_kidney_disease_year_risk = transform_probability_to_1y(ckd_risk, tot_years = 5)) |> 
    
    #LUNG CANCER 9
    apply_lungcancer_risk_wo_risk_factors() |> 
    mutate(lung_cancer_year_risk = transform_probability_to_1y(lungcancer_risk, tot_years = 5))
  
  #return(input_population)
  ## Left out PAD/IC, Liver, VTE and Qmortality and colorectal cancer
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
  
  filter(qof_prevalence,Year==2023) |> 
    select( - c(Year, Disease,Interpolated,Per1k,pop_ni))
  
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


load_dependencies <- function() {
  

  ############## Death and Deceased #####################

  # source('./incidence_operators/apply_age_sex_death.R')
  # source('./incidence_operators/qmortality.R')
  
  ################# Utility ################# 
  # source('./prevalence_operators/transform_probability.R')
  
  ############################ RISK CALCULATIONS #########################
  
  ################## Qrisk3 - CBVD (Stroke/TIA) + CHD (MI/Angina) ######################
  source('./risk_correct_eq/apply_qrisk_score.R') # an aggregate risk score that is later decomposed
  # source('./apply_chd_risk.R') # calculated from stroke and cvd risk
  
  ################## 
  
  # paste0('./risk_correct/', dir(pattern='.R','./risk_correct') ) %>%
  #   sapply(.,FUN=function(x){source(file=x,print.eval = F, echo =F)})
  
  ################## 
  
  source("./risk_correct_eq/risk_qstroke_stroke_1_2.R") # 1.2 includes ethnicity ----
  
  source("./risk_correct_eq/risk_ukbdrs_dementia.R")
  
  source("./risk_correct_eq/risk_qdiabetes_diabetes.R")
  source("./risk_correct_eq/risk_qkidney_chronic_kidney_disease_severe.R")
  
  source("./risk_correct_eq/risk_framingham_hypertension.R")
  source("./risk_correct_eq/risk_framingham_atrial_fibrillation.R")
  source("./risk_correct_eq/risk_framingham_congestive_heart_failure.R")
  
  source("./risk_correct_eq/risk_qfracture_fracture.R")
  source("./risk_correct_eq/risk_qthrombosis_venal_thromboembelism.R")
  
  source("./risk_correct_eq/risk_framingham_peripheral_arterial_disease.R")
  source("./risk_correct_eq/risk_framingham_liver_disease.R")
  
  source('./risk_correct_eq/risk_qcancer_lungcancer.R')
  source("./risk_correct_eq/site_cancers/renal_cancer.R")
  source("./risk_correct_eq/site_cancers/prostate_male_cancer.R")
  source("./risk_correct_eq/site_cancers/pancreatic_cancer.R")
  source("./risk_correct_eq/site_cancers/ovarian_female_cancer.R")
  source("./risk_correct_eq/site_cancers/oral_cancer.R")
  source("./risk_correct_eq/site_cancers/oesteogastric_cancer.R")
  
  # source("./risk_correct_eq/site_cancers/breast_female_cancer.R")
  
  source("./risk_correct_eq/site_cancers/colorectal_cancer.R")
  source("./risk_correct_eq/site_cancers/blood_cancer.R")
  source("./risk_correct_eq/site_cancers/uterine_female_cancer.R")
  
  
  
  # source("./disease_prevalence.R")
  
}

load_dependencies()



