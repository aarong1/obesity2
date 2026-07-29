
################################################
############## Dementia Risk score #############
################################################
tbl <- tibble::tribble(
  ~var,      ~coeff,                ~lower_age_ci,  ~HR,            ~upper_age_ci,
  
  'Age, per year increase', "0.055",  '0.047 to 0.062', 1.06,  '1.05 to 1.06',
  "Age2, per unit increase", "−0.005", "−0.007 to −0.004",   0.995, "0.993 to 0.996",
  "Gender (female v male)",  "0.160",   "0.104 to 0.216",    1.17,   "1.11 to 1.24",
  "Calendar year, per year increase",  "0.074",   "0.063 to 0.084",    1.08,   "1.07 to 1.09",
  "BMI (kg/m2), per unit increase", "−0.050",  "−0.063 to-0.036",    0.95,   "0.94 to 0.96",
  "Current anti-hypertensive use (yes vs. no)", "−0.249", "−0.301 to −0.197",    0.78,   "0.74 to 0.82",
  "Systolic blood pressure (mmHg), per unit increase", "−0.006", "−0.008 to −0.005",   0.994, "0.992 to 0.995",
  "Lipid ratio (per unit increase)",  "0.042",  "−0.055 to 0.138",    1.04,   "0.95 to 1.15",
  
  "Smoking status:Never", 	"0",    NA , 		                1, NA,
  "Smoking status:Past", "−0.178", "−0.233 to −0.122", 0.84, "0.79 to 0.89",
  "Smoking status:Current", "−0.134", "−0.229 to −0.039", 0.88, "0.80 to 0.96",
  "Past", "−0.178", "−0.233 to −0.122", 0.84, "0.79 to 0.89",
  "Current", "−0.134", "−0.229 to −0.039", 0.88, "0.80 to 0.96",
  "History of alcohol problem (yes vs. no)",  "0.256",  "−0.009 to 0.521", 1.29, "0.99 to 1.68",
  "History of diabetes (yes vs. no)",  "0.183",   "0.102 to 0.264",  1.2, "1.11 to 1.30",
  "History of stroke or transient ischemic attack (yes vs. no)",  "0.242",   "0.177 to 0.306", 1.27, "1.19 to 1.36",
  "History of atrial fibrillation (yes vs. no)",  "0.057",  "−0.018 to 0.132", 1.06, "0.98 to 1.14",
  "Current depression/use of anti-depressants (yes vs. no)",  "0.400",   "0.335 to 0.465", 1.49, "1.40 to 1.59",
  "Current anxiety/use of anxiolytics (yes vs. no)",  "0.136",   "0.034 to 0.237", 1.15, "1.04 to 1.27",
  "Current NSAID use, excluding aspirin (yes vs. no)", "−0.157", "−0.237 to −0.078", 0.86, "0.79 to 0.93",
  "Current aspirin use (yes vs. no)",  "0.092",   "0.037 to 0.147",  1.1, "1.04 to 1.16"
)

#tbl$coeff
# Different from the UK Biobank dementia risk score
# This one uses the THIN primary care database
# it also more explicitly encodes BMI for obesity intervention functions
# This one also encodes 5 year risk

# REF
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4722622/

# See supplementary appendix for example implementation

# Implications
# We used routinely collected primary care data to derive a relatively simple 
# new risk algorithm, predicting a new GP recorded dementia diagnosis within 
# 5 years, which worked well in those aged 60–79 years, but not in older 
# age groups. This supports the previous suggestion that given the steep rise 
# in risk of dementia at 80 years, it would be reasonable to test for dementia 
# beyond this point on the basis of age alone [19]. It is likely that risk scores 
# using traditional risk factors will not perform well in this population, 
# and a different approach might be needed to identify a higher risk group aged 
# 80 or above using, for example, measures of frailty.

risk_drs_dementia <- function(age, 
                              sex, 
                              bmi = 25, 
                              hypertension = 0,
                              current_calendar_year = 2003,
                              deprivation_quintile = 1,
                              former_smoker = 0,
                              current_smoker = 0,
                              heavy_drinking = 0,
                              current_depression_and_or_use_of_antidepressants = 0,
                              current_aspirin_use = 0,
                              history_of_stroke_or_TIA = 0,
                              history_of_atrial_fibrillation = 0,
                              history_of_diabetes = 0,
                              current_nsaid_use_excluding_aspirin = 0,
                              current_anxiolytics = 0,
                              systolic_blood_pressure = 146.9
                              ) {
  
  # The risk (P) for an individual aged 60–79 years can be calculated using the following formula
  if(age<81){
    
  P <-  0.20921*(age - 65.608)+
    -0.00339*(age - 65.608)*(age - 65.608)+
    -0.0616*(bmi -27.501)+
    0.002508*(bmi - 27.501)*(bmi -27.501)+
    0.12854*(sex =='Females')+
    0.13199*(hypertension)+
    0.04477*(current_calendar_year -2003.719)+
    
    # deprivation quintile 1 is the reference quintile
    
    c(
    0.000000,   # 1 least deprived
    0.013371,   # 2 
    0.117904,   # 3 
    0.201776,   # 4 
    0.225529)[deprivation_quintile] +  # 5 most deprived
  
    -0.06792*(former_smoker)+
    -0.08657*(current_smoker)+
    0.443535*(heavy_drinking)+
    0.833612*(current_depression_and_or_use_of_antidepressants)+
    0.252833*(current_aspirin_use)+
    0.577207*(history_of_stroke_or_TIA)+
    0.220728*(history_of_atrial_fibrillation)+
    0.286701*(history_of_diabetes)
  
  
  # With a baseline hazard of 0.9969. The predicted 5 -year risk as a percentage is then calculated as follows:
  S = 0.9969
  
  }else if(age>80){


    
    P <-  0.055*(age - 65.608)+
      -0.005*(age - 65.608)*(age - 65.608)+
       0.160*(sex =='Females')+
       0.074*(current_calendar_year - 2003.719)+
      -0.050*(bmi -27.501)+
      -0.249*(hypertension)+
      -0.006*(systolic_blood_pressure)+
      
      # never smoker reference cat
      -0.178*(former_smoker)+
      -0.134*(current_smoker)+
       0.256*(heavy_drinking)+
      
      
       0.183*(history_of_diabetes)+
       0.242*(history_of_stroke_or_TIA)+
       0.057*(history_of_atrial_fibrillation)+
       0.400*(current_depression_and_or_use_of_antidepressants)+
     
       0.136*(current_anxiolytics)+
      -0.157*(current_nsaid_use_excluding_aspirin) +
       0.092*(current_aspirin_use)
      
    S = 0.9277
    
  }
  
  Prob5yr = (1-S^(exp(P)))
  
  return(Prob5yr)
  
}


risk_drs_dementia(age = 70,'Males')

risk_drs_dementia(age = 85,'Females')


apply_dementia_drs_5yr_risk_wo_risk_factors <- function(input_population,
                                                intervention=1){
  
  postp1 <- 
    input_population %>% 
    filter(year == max(year,na.rm = TRUE))
  
  postp <- postp1 %>%
    mutate(
      age = ifelse(age > 75, 75, age),
      sex,
      bmi = case_when(
        bmi == "normal"     ~ 22.5,
        bmi == "overweight" ~ 28,
        bmi == "obese"       ~ 35,
        TRUE ~ NA_real_),
      hypertension = case_when(hypertension_status %in% c( 'hypertensive_uncontrolled',
                                                           'hypertensive_untreated') ~ 1,
                               T~ 0),
      former_smoker = smoking == 'former',
      current_smoker = smoking == 'current',
      heavy_drinking = alcohol == 'higher_risk',
      # current_depression_and_or_use_of_antidepressants
      # current_aspirin_use
      history_of_stroke_or_TIA = (stroke!= 0),
      history_of_atrial_fibrillation = (af_status =='af'),
      history_of_diabetes = (diabetes_status != 'no_diabetes'),
      systolic_blood_pressure = case_when(hypertension_status %in% c( 'hypertensive_uncontrolled',
                                                   'hypertensive_untreated') ~ 150,
                       T~ 110),
      deprivation_quintile = mdm_quintile_soa #townsend_quintile*(-1)+6 #needs inverted back to pre-mdm comparison
      )

  postp <- postp %>% 
    rowwise() %>% 
    # mutate(list(print(c(age,sex) )))%>% 
    mutate(dementia_risk =
             ifelse(age < 60, 0,
                           risk_drs_dementia(
                             age = age, 
                             sex = sex,
                             bmi = bmi,
                             hypertension = hypertension,
                             current_calendar_year = year,
                             former_smoker = former_smoker,
                             current_smoker = current_smoker,
                             heavy_drinking = heavy_drinking,
                             history_of_stroke_or_TIA = history_of_stroke_or_TIA,
                             history_of_atrial_fibrillation = history_of_atrial_fibrillation,
                             history_of_diabetes = history_of_diabetes,
                             systolic_blood_pressure = systolic_blood_pressure,
                             deprivation_quintile = deprivation_quintile #townsend_quintile*(-1)+6 #needs inverted back to pre-mdm comparison
                             
                           )
             )
    )
  
  input_population <- input_population |> 
    select(-any_of('dementia_risk'))
  
  input_population <- left_join(input_population,
                                postp[c('dementia_risk','id')],
                                by ='id')
  
  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  
  input_population <- ungroup(input_population)
  
  return(input_population)
  
}


# test_population |> 
#   apply_dementia_drs_5yr_risk_wo_risk_factors() |> 
#   ggplot()+
#   geom_point(aes(age, dementia_risk, col=bmi))+
#   facet_wrap(~sex)
  
