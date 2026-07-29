
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
  mutate(tot_prob_sum = .$survival_prob + .$other_fatality + .$chd_deaths+ .$stroke_deaths+ .$heart_failure_deaths+ .$diabetes_deaths+ .$chronic_kidney_disease_deaths+ .$dementia_deaths+ .$asthma_deaths+ .$copd_deaths+ .$lung_cancer_deaths+ .$colorectal_cancer_deaths+ .$prostate_cancer_deaths+ .$female_breast_cancer_deaths+ .$oral_cancer_deaths+ .$pancreatic_cancer_deaths+ .$uterine_cancer_deaths+ .$ovarian_cancer_deaths+ .$renal_cancer_deaths) %>% 
  mutate(modelled_prob_sum = .$chd_deaths+ .$stroke_deaths+ .$heart_failure_deaths+ .$diabetes_deaths+ .$chronic_kidney_disease_deaths+ .$dementia_deaths+ .$asthma_deaths+ .$copd_deaths+ .$lung_cancer_deaths+ .$colorectal_cancer_deaths+ .$prostate_cancer_deaths+ .$female_breast_cancer_deaths+ .$oral_cancer_deaths+ .$pancreatic_cancer_deaths+ .$uterine_cancer_deaths+ .$ovarian_cancer_deaths+ .$renal_cancer_deaths) %>% 
  
  mutate(across(.names = '{.col}_new', ends_with('_deaths'), ~ .x / tot_prob_sum))

  x %>% ungroup() %>% summarise(sum(na.rm = T, pancreatic_cancer_deaths),
                sum(pancreatic_cancer_deaths_new,na.rm=T))
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
                            'survive' = survival_prob))) %>% 
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
expt <- as.data.frame(table(names(death_samples))) %>% mutate(Freq = Freq *model_specification$population$scale_down_factor)

fatality %>% 
  ungroup() %>% 
  count(std,wt = deaths) %>% 
  rbind(list(std='survive',n=nxx*model_specification$population$scale_down_factor)) %>% 
  right_join(expt, by = c('std'='Var1')) %>% 
  mutate(err=(n-Freq)/n) %>% 
  arrange(desc(err))# %>% 
  # pull(err) %>% sum()

sum(x$pancreatic_cancer_deaths)
sum(x$asthma_deaths)



count(x,pancreatic_cancer)

253/321*290