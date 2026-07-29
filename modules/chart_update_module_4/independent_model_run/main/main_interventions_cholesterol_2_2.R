library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)

############################### SYNTHETIC POPULATION ##############################
#source('./main/1_5_temp_synthetic_population.R')
source('./synthetic_population/1_6_temp_synthetic_population.R')
#both load a instantiate_base_population function with a scale_down_factor_argument

#source('./prevalence_operators/apply_ethnicity_lifestyle_parameter.R')

############################# Baseline Population  ############################## 
source('./prevalence_operators/apply_bmi_lifestyle_parameter.R')
source('./prevalence_operators/apply_smoking_lifestyle_parameter.R')

# source('./prevalence_operators/apply_alcohol_lifestyle_parameter.R')
# source('./prevalence_operators/apply_diet_lifestyle_parameter.R')
# source('./prevalence_operators/apply_physical_activity_lifestyle_parameter.R')

# source('./prevalence_operators/apply_depression_lifestyle_parameter.R')

source('./prevalence_operators/apply_diabetes_physiological_parameter.R')
source('./prevalence_operators/apply_cholesterol_physiological_parameter.R')
source('./prevalence_operators/apply_hypertension_physiological_parameter.R')

source('./prevalence_operators/apply_atrial_fibrillation_physiological_parameter.R')
# source('./prevalence_operators/apply_peripherial_arterial_disease_physiological_parameter.R')
# source('./prevalence_operators/apply_copd_physiological_parameter.R')

############## Death and Deceased #####################

source('./prevalence_operators/apply_cvd_death.R')
source('./prevalence_operators/apply_other_death.R')

################# Utility ################# 

source('./prevalence_operators/apply_cvd_event_utils.R')

############ Incidence Operators for non-risk (non-functional/casual relationship type incidence) ###################### 

source('./incidence_operators/transition_bmi_lifestyle_parameter.R')
source('./incidence_operators/transition_smoking_lifestyle_parameter.R')
source('./incidence_operators/transition_diabetes_physiological_parameter.R')
source('./incidence_operators/transition_cholesterol_physiological_parameter.R')
source('./incidence_operators/transition_hypertension_physiological_parameter.R')

################ Death ###################### 

source('./incidence_operators/age_sex_death.R')

########################## RISK CALCULATIONS #########################

################## Qrisk3 - CBVD (Stroke/TIA) + CHD (MI/Angina) ######################
source('./prevalence_operators/apply_risk_calculations.R')

################## 
# paste0('./risk_correct/', dir(pattern='.R','./risk_correct') ) %>%
#   sapply(.,FUN=function(x){source(file=x,print.eval = F, echo =F)})
################## 

source("./risk_correct/risk_qstroke_stroke.R")
# source("./risk_correct/risk_qthrombosis_venal_thromboembelism.R")


#source("./risk_correct/risk_qdiabetes_type2.R")
#source("./risk_correct/risk_qCKD_chronic_kidney_disease.R")

# source("./risk_correct/risk_framingham_atrial_fibrillation.R")
source("./risk_correct/risk_framingham_congestive_heart_failure.R")
source("./risk_correct/risk_framingham_hard_coronary_heart_disease.R")
source("./risk_correct/risk_framingham_hypertension.R")
#source("./risk_correct/risk_ukb_calculate_UKBDRS.R")


################################# Model Building Pipeline ####################################

# instantiate_base_pop_ni(scale_down_factor = 100)        %>%
#   #instantiate_base_pop_belfast()        %>%
#   #instantiate_base_pop_urban()          %>%
#   #instantiate_base_pop_
#   apply_health_parameters()                             %>%
#   apply_intervention(config = "statins.yaml")           %>%
#   apply_stroke_mortality_prob()                         %>%
#   apply_coronary_mortality_prob()                       %>%
#   apply_other_cvd_mortality_prob()                      %>%
#   calculate_and_remove_deaths()                          %>%
#   apply_costing_and_economic_impact() #end of year one

############################# Model Specification ###############################################

model_specification <- list(
  population = list(
  scale_down_factor =800
  ),
  model = list(
    start_year = 2016,
  duration = 8,
  number_of_runs = 6)#,
  # intervention = list(
  #   af=list(
  #     dates = 1:(1+12),
  #     parameters = c(1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
  #   )
  #)
)

############################# Population Actualization ###############################################

x <- instantiate_base_pop(scale_down_factor = model_specification$population$scale_down_factor )    %>%
  #this applies Age, Sex, Deprivation
  apply_bmi_lifestyle_parameter() %>%
  apply_smoking_lifestyle_parameter() %>% 
  apply_cholesterol_physiological_parameter() %>% 
  apply_hypertension_physiological_parameter() %>% 
  apply_diabetes_physiological_parameter() %>% 
  apply_atrial_fibrillation_physiological_parameter() %>% 
  apply_cvd_risk() %>% 
  mutate(year_risk = transform_10y_probability_to_1y(risk)) #%>% 
  #apply_stroke_risk() %>% 
  #mutate(year_stroke_risk = transform_10y_probability_to_1y(stroke_risk))

############################# Return closure of transition function ###############################################

prep_fn <- function(multiplier_up,
                    multiplier_down,
                    af_intervention) {

transition_lifestyle_physiological_parameters <- function(x,
                                        .dummy,
                                        transition_prob_incidence_additive_modifier=0.2,
                                        #transition_prob_remission_additive_modifiers=0.2,
                                        af_intervention = 1
                                                          ){

  print(paste( 'Year : ' ,.dummy))
  y <- x %>% 
    filter(year == max(year)) %>% 
    mutate(year = year+1)
  
  y <- y %>% 
    transition_smoking_lifestyle_parameter() %>% 
    transition_bmi_lifestyle_parameter()  %>% 
    transition_cholesterol_physiological_parameter(
       transition_prob_incidence_additive_modifier=1-0.04*transition_prob_incidence_additive_modifier,
      #transition_prob_remission_additive_modifiers=1-0.04*0
      ) %>% 
    transition_hypertension_physiological_parameter(
      #transition_prob_incidence_additive_modifier=1-0.04*transition_prob_incidence_additive_modifier,
      #transition_prob_remission_additive_modifier=1-0.04*0
      ) %>%
    transition_diabetes_physiological_parameter() %>% 
    apply_cvd_risk(intervention=af_intervention) %>% 
    mutate(year_risk = transform_10y_probability_to_1y(risk)) #%>% 
    #apply_stroke_risk(intervention=af_intervention) #%>% 
    #mutate(year_stroke_risk = transform_10y_probability_to_1y(stroke_risk)) 
  
    # print(names(x))
    # print(names(y))
          
  y <- rbind(x,y)
  
  return(y)
  
}

return(transition_lifestyle_physiological_parameters)

}

#   transition_lifestyle_physiological_parameters <- prep_fn(multiplier_up=0,multiplier_down=0)
# y <- reduce(.x = c(list(x),1:model_specification$model$duration),
#        .f = transition_lifestyle_physiological_parameters
#   )

############################# Return closure of transition function ###############################################

hold_risk_factors <- data.frame()
hold_outcome <- data.frame()
hold_outcome2 <- data.frame()

for(run in 1:(model_specification$model$number_of_runs)) {
  
  #population uncertainty is taken out and not characterised/ characterised spearately
  cat(paste('################################### \n run : ', run, ' \n###################################### \n'))
  #transition_lifestyle_physiological_parameters <- prep_fn(multiplier_up=0,multiplier_down=0,af_intervention = 0.6)

# y <- reduce(.x = c(list(x),1:model_specification$population$duration),
#        .f = transition_lifestyle_physiological_parameters)

y <- x

for (time in 1:model_specification$model$duration){
  
cat(paste('###################################### \n Time, t : ', run, time, ' \n###################################### \n'))
  
  if (run<=(model_specification$model$number_of_runs/2)){
    af_intervention <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)[time]
  }else{
    af_intervention <- c(1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)[time]
  }
    
cat(paste('###################################### \n Intervention : ', run, time, 'intervention', ' \n###################################### \n'))

  if (run<=(model_specification$model$number_of_runs/2)){
      transition_lifestyle_physiological_parameters <- prep_fn(multiplier_up=0.2 ,multiplier_down=0, af_intervention = 1)
      y <- transition_lifestyle_physiological_parameters(.dummy = time,y)
  } else{
    transition_lifestyle_physiological_parameters <- prep_fn(multiplier_up=1 ,multiplier_down=0, af_intervention = 1)
    y <- transition_lifestyle_physiological_parameters(.dummy = time,y)
  }

}
  
risk_factors <- y %>% 
  pivot_longer(cols = -c(1:14)) %>% 
  mutate(category = str_extract(string = name, pattern = 'cholesterol|overweight|bp|smoking|diabetic|atrial_fibrillation')) %>% 
  group_by(id,category,year) %>% 
  arrange(desc(value)) %>% 
  slice_head() %>% 
  ungroup() %>% 
  filter(category %in% c('cholesterol','bp','atrial_fibrillation')) %>% 
  mutate(run=run) %>% 
  count(year,LGD2014NAME, sex,age, name,run,category)

hold_risk_factors <- rbind(hold_risk_factors,risk_factors)

outcome <- count(y,year, sex, age, wt=year_risk) %>% #LGD2014NAME,
  mutate(run=run)

hold_outcome <- rbind(hold_outcome, outcome)


# outcome2 <- count(y,year, sex, age, wt=year_stroke_risk) %>% #LGD2014NAME,
#   mutate(run=run)
# hold_outcome2 <- rbind(hold_outcome, outcome2)

}

 ##################### GRAPH OUTPUT #####################

stroke_incidence_scotland <-  data.frame(
 year = c(2013, 2014, 2015,	2016,	2017,	2018,	2019,	2020,	2021,	2022),
 per100k = c(159.8, 163.6, 166.6, 169.7, 168.9, 168.9, 169.8, 171.3, 175.3, 179.4)
 
   )
  
plot(stroke_incidence_scotland)

############ GRAPH 1 #####################

( hold_risk_factors %>%
     mutate(multiplier=run) %>% 
    filter(!run%in%c(0,1)) %>% 
     mutate(`run` = as.factor(run),
            category1 = case_when(
              category == 'bp' ~ 'Blood Pressure',
              category == 'cholesterol' ~ 'Cholesterol',
              category == 'atrial_fibrillation' ~ 'atrial_fibrillation',
            ),
            name1 = case_when(
            name =='high_bp' ~ 'Normal',
            name =='normal_bp' ~ 'Risky',
            name == 'high_cholesterol' ~ 'Risky',
            name == 'normal_cholesterol' ~ 'Normal') ) %>% 
  #pivot_wider(id_cols = -value,names_from = category,values_from = name) %>%
   count(year,name1,run,category1) %>%
   ggplot() +
   geom_line(aes(year,
                 n,
                 #alpha=`impact parameter`,
                 group = run)) +
   #facet_wrap(~category1+name1,scales = 'free') +
   theme_classic() # +
       # labs(title='Plot of the impacted physiological states of individuals over 20yr of a population wide statin prescription',
       #       subtitle = 'Simulating the effect of a universal, population wide statin prescription.\nThe "impact parameter" is a measure of impact of the prescription. It is simulated from 0% to 10% efficacy at 1% increments\n'
       #      ) 
  )

################## GRAPH 2 #####################

hold_outcome %>% 
  ggplot() +
  geom_line(aes(
    group=run,
    color=run, 
    age,
    n*model_specification$population$scale_down_factor),
    alpha=0.5) +
  geom_smooth(method='gam',
              aes(
    group=run,
    color=run, 
    age,
    n * model_specification$population$scale_down_factor),
    alpha = 0.5)# +
  #facet_wrap(~LGD2014NAME)

###################### GRAPH 3 #########################



hold_outcome %>% 
   #mutate(run=as.character(run)) %>% 
    mutate(intervention = ifelse(run<=(model_specification$model$number_of_runs/2),'normal','intervention')) %>%
   group_by(year,intervention) %>%# = run>=5 #filter(!run%in%c(0,1)) %>% 
   summarise(n=sum(n)) %>% 
   #group_by(year,intervention) %>%# = run>=5 #filter(!run%in%c(0,1)) %>%
   #summarise(n=mean(n)) %>%
             mutate(
            # multiplier=run,
            #`impact parameter` = as.factor(10-run),
            incidence = n*model_specification$population$scale_down_factor*2+400)  %>%#*10.18+345
 #*0.6 -18970# 25130 #- 3449e1
  ggplot() +
  geom_line(aes(year,`incidence`,
                 group = intervention, #`impact parameter`,
                colour =  intervention #,#`impact parameter`,
                 #alpha = run
                ),lwd=0.8
            ) +
    theme_bw() +
    labs(title='Plot of the Incidence of Serious Cerebrovascular Disease (Stroke -all types)',
             #subtitle = '7 year horizon, calibrated to scotland 2016-2020\naggressive intervention commences 2018 and continues '
            ) +
  #facet_wrap(~LGD2014NAME,scales = 'free')
  geom_smooth(method='glm',
              data = stroke_incidence_scotland,
              mapping = aes(x= year , y=per100k*19*0.3+2340,
                            #color='calibration'
                            ),lty=2 )

  
   #  geom_line(data = yr7_scale100_runs2_baseline_hold_outcome %>% 
   #              group_by(year,run) %>% #filter(!run%in%c(0,1)) %>% 
   # summarise(n=sum(n)) %>% 
   #           mutate(
   #          # multiplier=run,
   #          #`impact parameter` = as.factor(10-run),
   #          incidence = n * model_specification$population$scale_down_factor*0.6 -18970) ,
   #            mapping = aes(year,`incidence`,
   #               group = run,#`impact parameter`,
   #              colour =  run), color='green')
  
  #1.9e6/1e5

x1%>% 
  ggplotly()

############ GRAPH 4 #####################


( 
  y1 <- hold_outcome2 %>% 
   #mutate(run=as.character(run)) %>% 
   group_by(year,run) %>%# = run>=5 #filter(!run%in%c(0,1)) %>% 
   summarise(n=sum(n)) %>% 
    filter(run!=4) %>% 
    mutate(intervention = ifelse(run<=2,'normal','intervention')) %>%
   # group_by(year,run= run<=2) %>%# = run>=5 #filter(!run%in%c(0,1)) %>%
   # summarise(n=mean(n)) %>%
    
             mutate(
            # multiplier=run,
            #`impact parameter` = as.factor(10-run),
            incidence = n*model_specification$population$scale_down_factor) %>% #*0.6 -18970# 25130 #- 3449e1
   
  ggplot() +
  geom_line(aes(year,`incidence`,
                 group = run,#`impact parameter`,
                colour =  intervention#,#`impact parameter`,
                 #alpha = run
                ),lwd=0.8
            )) +
    theme_bw() +
    labs(title='Plot of the incidence of serious Coronary Heart Disease Disease \n(Predominantly onset of Infarctions)',
             subtitle = '7 year horizon, calibrated to scotland 2016-2020\nintervention 2018'
            ) 
  )

ggplotly(y1)
############ GRAPH 3 #####################
data("economics_long")
data <- hold_outcome %>% #filter(!run%in%c(0,1)) %>% 
   group_by(year,run) %>%   
   summarise(n=sum(n)) %>% 
 #filter(run!=0) %>% 
   group_by(year) %>% 
   summarise(n=mean(n),
             sd=sd(n)) %>% 
             mutate(
            # multiplier=run,
            #`impact parameter` = as.factor(10-run),
            incidence = n*model_specification$population$scale_down_factor *0.6 -18970) #3459e1 # View


apex(data = economics_long, 
     type = "line", 
     mapping = aes(x = date, y = value01, group = variable)) %>% 
  ax_yaxis(decimalsInFloat = 2) # number of decimals to keep

apex(data = economics_long, 
     type = "line", 
     mapping = aes(x = date, y = value01, group = variable)) %>% 
  ax_yaxis(decimalsInFloat = 2) # number of decimals to keep


(x3 <- hold_outcome %>% #filter(!run%in%c(0,1)) %>% 
   group_by(year,run) %>%   
   summarise(n=sum(n)) %>% 
 #filter(run!=0) %>% 
   group_by(year) %>% 
   summarise(n=mean(n),
             sd=sd(n)) %>% 
             mutate(
            # multiplier=run,
            #`impact parameter` = as.factor(10-run),
            incidence = n*model_specification$population$scale_down_factor *0.6 -18970) %>% # - 3459e1 # View

  ggplot() +
  geom_line(aes(year,`incidence`,
                 # group = run,#`impact parameter`,
                 # colour =  run#`impact parameter`,
                 # alpha = `impact parameter`),lwd=0.8
            )) +
     geom_point(aes(year,`incidence`))+

    theme_bw() +
    labs(title='Plot of the incidence of serious Cerebrovascular Disease',
             subtitle = '10 year probability is the acknowledged risk metric for prognosis of cardiovascular disease that primary care \nproviders calculate to determine the need for intervention.\nCurrent NICE guidelines suggest a greater than 10% chance of onset of serious CVD merits intervention'
            ) ) +
  geom_line(data = stroke_incidence_scotland,mapping = aes(x= year , y=per100k*19), color='red')+
    geom_point(data = stroke_incidence_scotland,mapping = aes(x= year , y=per100k*19), color='red')+
    geom_smooth(method  ='glm',data = stroke_incidence_scotland,mapping = aes(x= year , y=per100k*19), color='orange',alpha=0.5)

   geom_line(data = yr7_scale100_runs2_baseline_hold_outcome %>% 
                group_by(year,run) %>% #filter(!run%in%c(0,1)) %>% 
   summarise(n=sum(n)) %>% 
     group_by(year) %>% 
    summarise(n=mean(n)) %>% 
             mutate(
            # multiplier=run,
            #`impact parameter` = as.factor(10-run),
            incidence = n*model_specification$population$scale_down_factor*0.6 -18970) ,
              mapping = aes(year,`incidence`,
              
               ), color='green')+
    
x3%>% 
  ggplotly()

# y <- x %>% rowwise() %>% mutate(
#   across(
#     contains('smoking'),
#     function(x){
#   ifelse(x==2022,
#          2022-round(abs(rnorm(n = 1,mean = (age-16)/4,sd = age/10))),
#          x)
#     }
#   )
# )


#########################################
# REASSIGN TEMP VARS TO GLOBAL EMPIRICAL VARIABLES

hold_outside_global_scope <- function(hold_risk_factors, hold_outcome) {
  
  yr7_scale100_runs2_baseline_hold_risk_factors<- hold_risk_factors
  yr7_scale100_runs2_baseline_hold_outcome <- hold_outcome
  
  write_rds(x = yr7_scale100_runs2_baseline_hold_risk_factors, "outputs/yr7_scale100_runs2_baseline_hold_risk_factors.rds")
  write_rds(x = yr7_scale100_runs2_baseline_hold_outcome, "outputs/yr7_scale100_runs2_baseline_hold_outcome.rds")
  
  yr10_scale800_runs5_baseline_hold_risk_factors<- hold_risk_factors
  yr10_scale800_runs5_baseline_hold_outcome <- hold_outcome
  
  write_rds(x = yr10_scale800_runs5_baseline_hold_risk_factors, "outputs/yr10_scale800_runs5_baseline_hold_risk_factors.rds")
  write_rds(x = yr10_scale800_runs5_baseline_hold_outcome, "outputs/yr10_scale800_runs5_baseline_hold_outcome.rds")

}

################################################################################

yr10_af_interventions_hold_risk_factors <- hold_risk_factors
yr10_af_interventions_hold_outcome <- hold_outcome

write_rds(yr10_af_interventions_hold_risk_factors,'outputs/yr10_af_interventions_hold_risk_factors.rds')
write_rds(yr10_af_interventions_hold_outcome,'outputs/yr10_af_interventions_hold_outcome.rds')


#################################################################################
#yr20_hypertension_cholesterol_interventions_hold_risk_factors <- hold_risk_factors
#yr20_hypertension_cholesterol_interventions_hold_outcome <- hold_outcome
#################################################################################
#20YEARS
#HYPERTENSION
#  MULTIPLIER UP 1:10*0.04
#  MULTIPLIER DOWN  1:10*0.04

#CHOLESTEROL
#  MULTIPLIER UP 1:10*0.04
#  MULTIPLIER DOWN  1:10*0.04
#########################################
