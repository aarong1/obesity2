

stats2223_agg <- read.fst('./6_post_main/post_evaluation_module/preprocess/stats2223_agg.fst')

bed_days_estimator <- function( past_populations,stats2223_agg) {
  morb_cols <- c('cvd'='pad', 'msk'='osteoporosis',     'cancer'='cancer',                               'msk'='osteoarthritis',             'msk'='rheumatoid_arthritis',       'other'='epilepsy',                  'other'='hypothyroidism','resp'='asthma',                     'resp'='copd',                       'other'='depression',                 'cvd'='non_diabetic_hyperglycaemia','cancer'='colorectal_cancer',          'cancer'='prostate_cancer',           'cancer'='female_breast_cancer',      'cancer'='kidney_cancer',                         'cancer'='oesophageal_cancer',                   'cancer'='stomach_cancer',                       'cancer'='osteogastric_cancer',                  'cancer'='oral_cancer',                          'cancer'='pancreatic_cancer',                    'cancer'='uterine_cancer',            'cancer'='blood_multiple_myeloma',               'cancer'='blood_lymphoma',                       'cancer'='blood_leukaemia',                      'cancer'='blood_cancer',                         'cancer'='ovarian_cancer',            'cancer'='lung_cancer','cvd'='stroke','cvd'='chd','cvd'='diabetes','other'='dementia','cvd'='heart_failure','cvd'='atrial_fibrillation','cvd'='hypertension','cvd'='chronic_kidney_disease')
  
  x <- past_populations |>
    group_by(run,intervention,year) |> 
    summarise(
      across(
        all_of(unname(morb_cols)),
        \(x) sum(x, na.rm = TRUE)
      )
    )
  
  x <- pivot_longer(x, -c(run,intervention,year))
  
  x <- x |> 
    left_join(data.frame(class = names(morb_cols), name = morb_cols)) 
  
  x <- x |> 
    ungroup() |> 
    # filter(run==min(run) ) |>
    # filter(intervention == first(intervention)) |> 
    group_by(class,intervention,run, year) |> 
    summarise(value = mean(value,na.rm=T)) |> 
    group_by(class,intervention, year) |> 
    summarise(value = sum(value,na.rm=T)) 
  
  per_hosp <- x |> 
    ungroup() |> 
    filter(year ==min(year)) |> 
    filter(intervention == first(intervention)) |>
    left_join(stats2223_agg, by = c('class' = 'broad')) |> 
    mutate(bed_days_per_case = Bed_Days/value) |> 
    mutate(admissions_per_case = Admissions/value) |> 
    mutate(emergency_per_case = Emergency/value) |> 
    select(class, 
           bed_days_per_case,
           admissions_per_case,
           emergency_per_case)
  
  x |> 
      left_join(per_hosp) |> 
      # left_join(bed_days_df, by= c('class' = 'broad')) |> 
    mutate(bed_days = value * bed_days_per_case) |>
    mutate(admissions = value * admissions_per_case) |> 
    mutate(emergency_admissions = value * emergency_per_case) |> 
    
    mutate(year = as.character(year)) |> 
    group_by(intervention,year) |>
    summarise( bed_days = sum(bed_days, na.rm=T),
      admissions = sum(admissions, na.rm=T),
      emergency_admissions = sum(emergency_admissions, na.rm=T)) 
#     e_charts(year) |> 
#     e_tooltip() |> 
#     e_line(bed_days)
}

if(!interactive()){
  print('yes')
# unit_test <- function(){
bed_days_estimator(total_pop,stats2223_agg ) |> 
# bed_days_df() %>%
  group_by(year,intervention) %>%
  summarise(
    emergency_admissions = sum(emergency_admissions, na.rm =T),
    admissions = sum(admissions, na.rm =T),
    bed_days = sum(bed_days, na.rm =T)
  ) %>%
  mutate(year = as.character(year)) %>%
  group_by(intervention) %>%
  e_charts(year, emphasis = list(focus = "series")) %>%
  e_line(admissions) %>% #, name = "Emergency Admissions"
  # e_line(admissions, name = "Admissions ", y_index = 1) %>%
  # e_line(bed_days, name = "Bed days") %>%
  e_grid(top = '0%') |> 
  
  e_tooltip() %>%
  e_grid(containLabel = T) %>%
  e_theme('westeros')
}
# }