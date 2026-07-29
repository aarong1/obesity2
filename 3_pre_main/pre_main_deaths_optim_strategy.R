  library(progress)
  
  ages_df <- data.frame(
    age_band_death =   c('0', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39' ,'40-44' ,'45-49' ,'50-54' ,'55-59' ,'60-64', '65-69' ,'70-74', '75-79', '80-84', '85-89' ,'90+'),
    age_band_death10 = c('0', '1-9', '1-9', '10-19', '10-19', '20-29', '20-29', '30-39', '30-39', '40-49' ,'40-49' ,'50-59' ,'50-59' ,'60-69' ,'60-69' ,'70-79', '70-79', '80-89', '80-89' ,'90+'),
    age_band_death20 = c('0', '1-19','1-19','1-19',   '1-19', '20-39' ,'20-39' ,'20-39' ,'20-39' ,'40-59' ,'40-59' ,'40-59' ,'40-59' ,'60-69' ,'60-69', '70-79', '70-79', '80-89', '80-89' ,'90+')
  )
  
  time_one_population <- time_one_population %>% 
mutate(age_band_death =
         cut(age, include.lowest = T,
             breaks = c(-Inf, 0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
             labels = c('0', '1-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39' ,'40-44' ,'45-49' ,'50-54' ,'55-59' ,'60-64', '65-69' ,'70-74', '75-79' ,'80-84', '85-89' ,'90+')
         )) %>% 
  left_join(ages_df) 
  
# time_one_population%>% select(disease_w_modelled_deaths,age_band_death10,sex)

pop <- time_one_population %>% count(sex,age_band_death10) %>% mutate(n=n*model_specification$population$scale_down_factor) %>% pull(n)

other_deaths_df <- fatality %>% 
  filter(std == 'other') %>%
  ungroup() %>% 
  mutate(disease = pop) %>% 
  mutate(other_fatality = deaths/disease) %>% 
  select(age_band_death10,sex,other_fatality)

fatality_wide <- fatality %>% 
  pivot_wider(id_cols = c(sex, age_band_death10), names_glue = '{std}_fatality',names_from = std, values_from = fatality)

x <- time_one_population

x <- x %>% 
  # left_join(fatality %>% filter(std=='chd')) %>% 
  left_join(fatality_wide %>% select(- other_fatality)) %>% 
  mutate("chd_deaths" = (chd!=0)*chd_fatality) %>% 
  mutate("stroke_deaths"= (stroke!=0)*stroke_fatality) %>% 
  mutate("heart_failure_deaths"= (heart_failure!=0)*heart_failure_fatality) %>% 
  mutate("diabetes_deaths"= (diabetes!=0)*diabetes_fatality) %>% 
  mutate("chronic_kidney_disease_deaths"= (chronic_kidney_disease!=0)*chronic_kidney_disease_fatality) %>% 
  mutate("dementia_deaths"= (dementia!=0)*dementia_fatality) %>% 
  mutate("asthma_deaths"= (asthma!=0)*asthma_fatality) %>% 
  mutate("copd_deaths"= (copd!=0)*copd_fatality) %>% 
  mutate("lung_cancer_deaths"= (lung_cancer!=0)*lung_cancer_fatality) %>% 
  mutate("colorectal_cancer_deaths"= (colorectal_cancer!=0)*colorectal_cancer_fatality) %>% 
  mutate("prostate_cancer_deaths"= (prostate_cancer!=0)*prostate_cancer_fatality) %>% 
  mutate("female_breast_cancer_deaths"= (female_breast_cancer!=0)*female_breast_cancer_fatality) %>% 
  mutate("oral_cancer_deaths"= (oral_cancer!=0)*oral_cancer_fatality) %>% 
  mutate("pancreatic_cancer_deaths"= (pancreatic_cancer!=0)*pancreatic_cancer_fatality) %>% 
  mutate("uterine_cancer_deaths"= (uterine_cancer!=0)*uterine_cancer_fatality) %>% 
  mutate("ovarian_cancer_deaths"= (ovarian_cancer!=0)*ovarian_cancer_fatality) %>% 
  mutate("renal_cancer_deaths"= (renal_cancer!=0)*renal_cancer_fatality) %>% 
  left_join(ppp) %>% 
  left_join(y) %>% 
  mutate(tot_prob_sum = .$survival_prob + .$other_fatality + .$chd_deaths+ .$stroke_deaths+ .$heart_failure_deaths+ .$diabetes_deaths+ .$chronic_kidney_disease_deaths+ .$dementia_deaths+ .$asthma_deaths+ .$copd_deaths+ .$lung_cancer_deaths+ .$colorectal_cancer_deaths+ .$prostate_cancer_deaths+ .$female_breast_cancer_deaths+ .$oral_cancer_deaths+ .$pancreatic_cancer_deaths+ .$uterine_cancer_deaths+ .$ovarian_cancer_deaths+ .$renal_cancer_deaths
         ) %>% 
  mutate(modelled_prob_sum = .$chd_deaths+ .$stroke_deaths+ .$heart_failure_deaths+ .$diabetes_deaths+ .$chronic_kidney_disease_deaths+ .$dementia_deaths+ .$asthma_deaths+ .$copd_deaths+ .$lung_cancer_deaths+ .$colorectal_cancer_deaths+ .$prostate_cancer_deaths+ .$female_breast_cancer_deaths+ .$oral_cancer_deaths+ .$pancreatic_cancer_deaths+ .$uterine_cancer_deaths+ .$ovarian_cancer_deaths+ .$renal_cancer_deaths
         ) %>% 
  mutate(deaths_prob_sum = .$other_fatality + .$chd_deaths+ .$stroke_deaths+ .$heart_failure_deaths+ .$diabetes_deaths+ .$chronic_kidney_disease_deaths+ .$dementia_deaths+ .$asthma_deaths+ .$copd_deaths+ .$lung_cancer_deaths+ .$colorectal_cancer_deaths+ .$prostate_cancer_deaths+ .$female_breast_cancer_deaths+ .$oral_cancer_deaths+ .$pancreatic_cancer_deaths+ .$uterine_cancer_deaths+ .$ovarian_cancer_deaths+ .$renal_cancer_deaths
         ) #%>% 
  # mutate(across(.names = '{.col}_new', ends_with('_deaths'), ~ .x / tot_prob_sum))

x %>% ungroup() %>% filter(age>80 ) %>% 
  summarise(sum(na.rm = T, pancreatic_cancer_deaths),
                              sum(pancreatic_cancer_deaths/tot_prob_sum,na.rm=T))

x <- x  %>%
  mutate(all_cause_rr_obese = case_when(bmi == 'obese' ~ 1.20,
                                        bmi == 'overweight' ~ 1.07,
                                        bmi == 'normal' ~ 1  )) %>% 
  mutate(all_cause_rr_diabetes = ifelse(diabetes_status != 'no_diabetes',1.7,1) ) %>% 
  mutate(all_cause_rr_smoking = ifelse(smoking == 'current_smoker',3.7,1) ) %>% 
  mutate(all_cause_rr_active = ifelse(pa != 'meets_recommendations',1.86,1) ) %>% 
  mutate(all_cause_rr_alcohol = ifelse(alcohol %in% c('increased_risk','higher_risk'),1.36,1) ) %>% 
  left_join(all_cause_paf) %>% 
  mutate(tm_mortality_prob = mortality_prob * (1 - paf_all_cause)) %>%
  mutate(risk_weight_mortality_prob = tm_mortality_prob*all_cause_rr_obese *all_cause_rr_diabetes *all_cause_rr_smoking *all_cause_rr_active *all_cause_rr_alcohol) %>% 
  mutate(risk_weight_survival_prob = 1 - max(0.99,risk_weight_mortality_prob)) %>% 
  mutate(risk_weight_survival_prob = coalesce(risk_weight_survival_prob,survival_prob) )

x <- x %>%
  # mutate(surv = ifelse(modelled_prob_sum ==0, risk_weight_survival_prob, max(0, min(risk_weight_survival_prob, 1 - modelled_prob_sum - other_fatality) )) )%>%
  mutate(surv = 1 - modelled_prob_sum - other_fatality ) %>%
  mutate(other = ifelse(modelled_prob_sum ==0, other_fatality, max(0, min(other_fatality, 1 - modelled_prob_sum - risk_weight_survival_prob)) ))
  # mutate(other = other_fatality)

# sum(x$other)
# sum(x$other_fatality)

x %>% group_by(age_band_death10) %>% summarise(min = min(modelled_prob_sum),max = max(modelled_prob_sum) )
x %>% group_by(age_band_death10) %>% summarise(min = min(deaths_prob_sum),max = max(deaths_prob_sum) )


xx <- x %>% 
  rowwise() %>% 
  mutate(prob_list = list(c('chd' = chd_deaths,
                            'stroke' = stroke_deaths,
                            'heart_failure' = heart_failure_deaths,
                            'diabetes' = diabetes_deaths,
                            'chronic_kidney_disease' = chronic_kidney_disease_deaths,
                            'dementia' = dementia_deaths,
                            'asthma' = asthma_deaths,
                            'copd' = copd_deaths,
                            'lung_cancer' = lung_cancer_deaths,
                            'colorectal_cancer' = colorectal_cancer_deaths,
                            'prostate_cancer' = prostate_cancer_deaths,
                            'female_breast_cancer' = female_breast_cancer_deaths,
                            'oral_cancer' = oral_cancer_deaths,
                            'pancreatic_cancer' = pancreatic_cancer_deaths,
                            'uterine_cancer' = uterine_cancer_deaths,
                            'ovarian_cancer' = ovarian_cancer_deaths,
                            'renal_cancer' = renal_cancer_deaths,
                            'other' = other_fatality,
                            # 'survive' = survival_prob
                            # 'other' = other,
                            'survive' = surv
                            ))) 

xx <- xx %>% 
  # 'survive' = risk_weight_survival_prob))) %>% 
  ungroup() %>% 
  # filter(age>70) %>% 
  pull(prob_list)

xx <- lapply(xx, function(x) x/sum(x, na.rm = T)) 

message('Applying Deaths')
nxx <- length(xx)
counter <- 0
pb <- progress_bar$new(total = 100)

death_samples <- sapply(xx, 
       function(list_item){
         pb$update(counter/nxx)
         counter <<- counter+1
         sample(list_item, size = 1, prob = list_item,replace = F)}
       )

table(names(death_samples))/length(death_samples)
expt <- as.data.frame(table(names(death_samples))) %>% 
  mutate(Freq = Freq *model_specification$population$scale_down_factor)

fatality %>% 
  ungroup() %>% 
  count(std,wt = deaths,name = 'actual') %>% 
  rbind(list(std='survive',actual=nxx*model_specification$population$scale_down_factor)) %>% 
  right_join(expt, by = c('std'='Var1')) %>% 
  mutate(err=(actual-Freq)/actual) %>% 
  arrange(desc(err)) #%>% 
  # pull(err) %>% sum()

time_one_population$death_reason <- names(death_samples)

time_one_population %>%
  filter(age>60) %>% 
  count( death_reason) %>%
  left_join(fatality %>% ungroup()%>% count( a=age_band_death10 %in% c('50-59' ,'50-59' ,'60-69' ,'60-69' ,'70-79', '70-79', '80-89', '80-89' ,'90+'),
                                std,wt=deaths, name='expt'), by = c('death_reason' = 'std')) %>% 
  filter(a==T) %>% 
  # group_by(age_band_death10) %>% 
  mutate(n = n*model_specification$population$scale_down_factor) %>% 
  mutate(err = abs(expt-n)/expt) %>% select(-a) %>% 
  arrange(desc(err)) 

sum(x$pancreatic_cancer_deaths)
sum(x$asthma_deaths)
sum(x$chronic_kidney_disease_deaths)

time_one_population %>%
  filter(age>50) %>% 
  count(age_band_death10,sex, death_reason) %>%
  left_join(fatality %>% select(std, age_band_death10,sex,deaths), by = c('death_reason' = 'std','sex','age_band_death10')) %>% 
  # group_by(age_band_death10) %>% 
  mutate(n = n*model_specification$population$scale_down_factor) %>% 
  mutate(err = (n-deaths)/deaths) %>% 
  arrange(desc(err)) %>% View()


time_one_population %>%
  count(age20, death_reason) %>%
  pivot_wider(names_from = death_reason, values_from = n) %>%
  View()

time_one_population %>%
  count(age,sex, d=death_reason=='survive') %>%
  add_count(age,sex,wt=n) %>% 
  mutate(s=n/nn) %>% 
  filter(d==TRUE) %>% 
  left_join(ppp) %>% 
  mutate(e=(s-survival_prob)/survival_prob) %>%
  ggplot(aes(age,e,col=sex))+geom_line()
  #View()


fatality %>% filter(std!='other') %>% ggplot() +geom_col(aes(age_band_death10,deaths,fill=std))
count(x,pancreatic_cancer)

253/321*290

xx[[1000]]
sum(xx[[1000]]/sum(xx[[1000]]) )
sample(xx[[1000]],size = 1,prob = xx[[1000]],replace = F)

xx[[38]]
sum(xx[[38]]/sum(xx[[38]], na.rm = T), na.rm = T )

xx[[38]]/sum(xx[[38]], na.rm = T)
sample(xx[[1000]],size = 1,prob = xx[[1000]],replace = F)

#check all prob sets sum to 1
lapply(xx, function(x) x/sum(x, na.rm = F)) %>% 
  sapply(sum,na.rm=F)




deaths_fn <- function(xarg){
        chd_arg <- xarg[[1]]
        stroke_arg <- xarg[[2]]
        heart_arg <- xarg[[3]]
        diabetes_arg <- xarg[[4]]
        chronic_arg <- xarg[[5]]
        dementia_arg <- xarg[[6]]
        asthma_arg <- xarg[[7]]
        copd_arg <- xarg[[8]]
        lung_arg <- xarg[[9]]
        colorectal_arg <- xarg[[10]]
        prostate_arg <- xarg[[11]]
        female_arg <- xarg[[12]]
        oral_arg <- xarg[[13]]
        pancreatic_arg <- xarg[[14]]
        uterine_arg <- xarg[[15]]
        ovarian_arg <- xarg[[16]]
        renal_arg <- xarg[[17]]
  
  print(xarg)
  
  x <- time_one_population
  
  
  x <- x %>% 
    # left_join(fatality %>% filter(std=='chd')) %>% 
    left_join(fatality_wide %>% select(- other_fatality)) %>% 
    mutate("chd_deaths" = (chd!=0)*chd_fatality) %>% 
    mutate("stroke_deaths"= (stroke!=0)*stroke_fatality) %>% 
    mutate("heart_failure_deaths"= (heart_failure!=0)*heart_failure_fatality) %>% 
    mutate("diabetes_deaths"= (diabetes!=0)*diabetes_fatality) %>% 
    mutate("chronic_kidney_disease_deaths"= (chronic_kidney_disease!=0)*chronic_kidney_disease_fatality) %>% 
    mutate("dementia_deaths"= (dementia!=0)*dementia_fatality) %>% 
    mutate("asthma_deaths"= (asthma!=0)*asthma_fatality) %>% 
    mutate("copd_deaths"= (copd!=0)*copd_fatality) %>% 
    mutate("lung_cancer_deaths"= (lung_cancer!=0)*lung_cancer_fatality) %>% 
    mutate("colorectal_cancer_deaths"= (colorectal_cancer!=0)*colorectal_cancer_fatality) %>% 
    mutate("prostate_cancer_deaths"= (prostate_cancer!=0)*prostate_cancer_fatality) %>% 
    mutate("female_breast_cancer_deaths"= (female_breast_cancer!=0)*female_breast_cancer_fatality) %>% 
    mutate("oral_cancer_deaths"= (oral_cancer!=0)*oral_cancer_fatality) %>% 
    mutate("pancreatic_cancer_deaths"= (pancreatic_cancer!=0)*pancreatic_cancer_fatality) %>% 
    mutate("uterine_cancer_deaths"= (uterine_cancer!=0)*uterine_cancer_fatality) %>% 
    mutate("ovarian_cancer_deaths"= (ovarian_cancer!=0)*ovarian_cancer_fatality) %>% 
    mutate("renal_cancer_deaths"= (renal_cancer!=0)*renal_cancer_fatality) %>% 
    left_join(ppp) %>%
    
    mutate(all_cause_rr_obese = case_when(bmi == 'obese' ~ 1.20,
                                          bmi == 'overweight' ~ 1.07,
                                          bmi == 'normal' ~ 1  )) %>% 
    mutate(all_cause_rr_diabetes = ifelse(diabetes_status != 'no_diabetes',1.7,1) ) %>% 
    mutate(all_cause_rr_smoking = ifelse(smoking == 'current_smoker',3.7,1) ) %>% 
    mutate(all_cause_rr_active = ifelse(pa != 'meets_recommendations',1.86,1) ) %>% 
    mutate(all_cause_rr_alcohol = ifelse(alcohol %in% c('increased_risk','higher_risk'),1.36,1) ) %>% 
    left_join(all_cause_paf) %>% 
    mutate(tm_mortality_prob = mortality_prob * (1 - paf_all_cause)) %>%
    mutate(risk_weight_mortality_prob = tm_mortality_prob*all_cause_rr_obese *all_cause_rr_diabetes *all_cause_rr_smoking *all_cause_rr_active *all_cause_rr_alcohol) %>% 
    mutate(risk_weight_survival_prob = 1 - max(0.90,risk_weight_mortality_prob)) %>% 
    mutate(risk_weight_survival_prob = coalesce(risk_weight_survival_prob,survival_prob) ) %>% 
    left_join(other_deaths_df) 
  
  # chd=1.1;
  # stroke=1.1;
  # heart=1.1;
  # diabetes=1.1;
  # chronic=1.1;
  # dementia=1.1;
  # asthma=1.1;
  # copd=1.1;
  # lung=1.1;
  # colorectal=1.1;
  # prostate=1.1;
  # female=1.1;
  # oral=1.1;
  # pancreatic=1.1;
  # uterine=1.1;
  # ovarian=1.1;
  # renal=1.1
  
  xx <- x %>% 
    rowwise() %>% 
    mutate(prob_list = list(c('chd' = chd_deaths * chd_arg,
                              'stroke' = stroke_deaths * stroke_arg,
                              'heart_failure' = heart_failure_deaths * heart_arg,
                              'diabetes' = diabetes_deaths * diabetes_arg,
                              'chronic_kidney_disease' = chronic_kidney_disease_deaths * chronic_arg,
                              'dementia' = dementia_deaths * dementia_arg,
                              'asthma' = asthma_deaths * asthma_arg,
                              'copd' = copd_deaths * copd_arg,
                              'lung_cancer' = lung_cancer_deaths * lung_arg,
                              'colorectal_cancer' = colorectal_cancer_deaths * colorectal_arg,
                              'prostate_cancer' = prostate_cancer_deaths * prostate_arg,
                              'female_breast_cancer' = female_breast_cancer_deaths * female_arg,
                              'oral_cancer' = oral_cancer_deaths * oral_arg,
                              'pancreatic_cancer' = pancreatic_cancer_deaths * pancreatic_arg,
                              'uterine_cancer' = uterine_cancer_deaths * uterine_arg,
                              'ovarian_cancer' = ovarian_cancer_deaths * ovarian_arg,
                              'renal_cancer' = renal_cancer_deaths * renal_arg,
                              'other' = other_fatality,
                              'survive' = survival_prob))) %>% 
    # 'survive' = risk_weight_survival_prob))) %>% 
    
    ungroup() %>% 
    # filter(age>70) %>% 
    pull(prob_list)
  
  print(head(xx))
  
  message('Applying Deaths')
  nxx <- length(xx)
  counter <- 0
  pb <- progress_bar$new(total = 100)
  
  death_samples <- sapply(xx, 
                          function(list_item){
                            pb$update(counter/nxx)
                            counter <<- counter+1
                            sample(list_item, size = 1, prob = list_item,replace = F)}
  )
  
  # print(head(death_samples))
  
  table(names(death_samples))/length(death_samples)
  expt <- as.data.frame(table(names(death_samples))) %>% 
    mutate(Freq = Freq *model_specification$population$scale_down_factor)
  
  # print(table(names(death_samples)))
  # print(head(fatality))
  # print(head(expt))
  
  fatality %>% 
    ungroup() %>% 
    count(std,wt = deaths) %>% 
    rbind(list(std='survive',n=nxx*model_specification$population$scale_down_factor)) %>% 
    right_join(expt, by = c('std'='Var1')) %>% 
    mutate(err=abs(n-Freq)/n) %>% 
    arrange(desc(err)) %>% 
    # head() %>% 
    print()
  
  
  fatality %>% 
    ungroup() %>% 
    count(std,wt = deaths) %>% 
    rbind(list(std='survive',n=nxx*model_specification$population$scale_down_factor)) %>% 
    right_join(expt, by = c('std'='Var1')) %>% 
    mutate(err=abs(n-Freq)/n) %>% 
    arrange(desc(err)) %>% 
    pull(err) %>% sum()
}

out <- deaths_fn(c(chd = 1,
            stroke = 1,
            heart = 1,
            diabetes = 1,
            chronic = 1,
            dementia = 1,
            asthma = 1,
            copd = 1,
            lung = 1,
            colorectal = 1,
            prostate = 1,
            female = 1,
            oral = 1,
            pancreatic = 1,
            uterine = 1,
            ovarian = 1,
            renal = 1
))

optimal_par <- optim(par = c(chd = 1.1,
              stroke = 1.1,
              heart = 1.1,
              diabetes = 1.1,
              chronic = 1.1,
              dementia = 1.1,
              asthma = 1.1,
              copd = 1.1,
              lung = 1.1,
              colorectal = 1.1,
              prostate = 1.1,
              female = 1.1,
              oral = 1.1,
              pancreatic = 1.1,
              uterine = 1.1,
              ovarian = 1.1,
              renal = 1.1
),fn = deaths_fn)
