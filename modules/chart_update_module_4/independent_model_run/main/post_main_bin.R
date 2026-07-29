
morbidity_incidence_over_time <- rbind(lungcancer_outcome_incidence,dementia_outcome_incidence,ckd_outcome_incidence,hypertension_outcome_incidence,af_outcome_incidence,chd_outcome_incidence,diabetes_outcome_incidence,stroke_outcome_incidence)
unique(morbidity_incidence_over_time$run)
unique(morbidity_incidence_over_time$year)

# show_incidence( initial_time_zero_population, stroke_year_risk, stroke,  stroke, run, year )

unique(past_populations$year)
unique(past_populations$run)
unique(past_populations$stroke) |> sort()

count(past_populations, year, hf=year==heart_failure) |> 
  filter(hf != F,year != 2023)

# current_population |> 
#   group_by(run) |> 
#   group_by(year, stroke, .add = T) |> 
#   summarise(counted_states = sum(stroke!=0),
#             .groups = "drop") |> 
#   group_by(year,stroke) |> 
#   summarise(counted_states = mean(counted_states),
#             .groups = "drop") 

# show_survivor_prevalence_by_discrete_states(input_population = current_population,
#                                             morbidity = stroke,
#                                             year,
#                                             stroke
#                                             )

# show_survivor_prevalence_by_discrete_states(input_population = initial_time_zero_population,
#                                             morbidity = stroke,
#                                             year,
#                                             HSCT)

show_survivor_prevalence_by_discrete_states(input_population = past_populations,
                                            morbidity= stroke,
                                            year,
                                            stroke,
                                            HSCT) 


#incidence over time by summed risk 
morbidity_incidence_over_time |> 
  ungroup() |> 
  count(year,morbidity, run, wt=summed_risk, name = 'summed_risk') |> 
  ggplot() +
  geom_line(aes(year, summed_risk, group=run, color=as.character(run))) +
  facet_wrap(~morbidity,scales='free_y') +
  geom_smooth(aes(year, summed_risk), col='orange',lwd=1) +
  theme_minimal()

#incidence over time by absolute states summed risk
morbidity_incidence_over_time |> 
  ungroup() |> 
  count(year, morbidity, run, wt = counted_states, name = 'counted_states') |> 
  ggplot() +
  geom_line(aes(year, counted_states, group=run, color=as.character(run))) +
  facet_wrap(~morbidity,scales='free_y') +
  geom_smooth(aes(year, counted_states), col='orange',lwd=1) +
  theme_minimal()

#incidence
hf_model_incidence <- count(past_populations, year, hf=year==heart_failure) |> 
  filter(hf != F,year != 2023)

#prevalence
count(past_populations, year, hf_hist = heart_failure != 0) |> 
  filter(hf_hist != F) |> 
  mutate(hf_hist = hf_hist*model_specification$population$scale_down_factor)






#####################  Investigating Lung Cancer and Heart Failure #################### 

stroke_incidence_scotland <-  data.frame(
  year = c(2013, 2014, 2015,	2016,	2017,	2018,	2019,	2020,	2021,	2022),
  per100k = c(159.8, 163.6, 166.6, 169.7, 168.9, 168.9, 169.8, 171.3, 175.3, 179.4) ,
  NI = c(159.8, 163.6, 166.6, 169.7, 168.9, 168.9, 169.8, 171.3, 175.3, 179.4) * 19
  
)

plot(stroke_incidence_scotland)

# Public Health Scotland ASR
# https://publichealthscotland.scot/publications/scottish-heart-disease-statistics/scottish-heart-disease-statistics-year-ending-31-march-2022/


heart_failure_incidence_scotland <-  data.frame(
  year = c(2013, 2014, 2015,	2016,	2017,	2018,	2019,	2020,	2021,	2022),
  ASRper100k = c(105.7,	101.3,	100.3,	102.3,	102.7,	102.6,	104.2,	105.2,	95.7,	103.4),
  NI = c(105.7,	101.3,	100.3,	102.3,	102.7,	102.6,	104.2,	105.2,	95.7,	103.4) * 19
)

# plot(heart_failure_incidence_scotland)

########## heart failure ##########

#incidence
hf_model_incidence_count_states <- count(past_populations, run, year, hf=year==heart_failure) |> 
  filter(hf != F,year != 2023) |> 
  group_by(year) |> 
  summarise(model_count = mean(n) * model_specification$population$scale_down_factor)

hf_model_incidence_summed_risk <- count(past_populations, 
                                        run,
                                        year, 
                                        name = 'sum_hf_risk', 
                                        wt = heart_failure_year_risk) |> 
  filter(year != 2023) |> 
  group_by(year) |> 
  summarise(sum_hf_risk = mean(sum_hf_risk) * model_specification$population$scale_down_factor)

ggplot()+
  geom_line(data = hf_model_incidence_count_states[hf_model_incidence_count_states$year!=2020,], mapping = aes(year,model_count)) + #1.7
  geom_line(data = heart_failure_incidence_scotland, mapping = aes(year, NI),color='orange') +
  geom_line(data = hf_model_incidence_summed_risk, mapping = aes(year, sum_hf_risk),color='mediumseagreen')+
  theme_minimal()

cancer_incidence_ni <- read_excel("data/ni_cancer_registry/all_cancers_data tables.xlsx", 
                                  sheet = "T03", skip = 5)

cancer_incidence_ni <- cancer_incidence_ni |> 
  select(year = `Year of diagnosis`, cases = `Total number of cases`) |> 
  mutate(year = as.numeric(year))

#plot(cancer_incidence_ni)


########## Cancer ##########


#incidence
cancer_model_incidence_count_states <- count(past_populations, run, year, cancer=year==lung_cancer) |> 
  filter(cancer != F,year != 2023) |> #arrange(year,desc(n))
  group_by(year) |> 
  summarise(model_count = mean(n) * model_specification$population$scale_down_factor*20)

cancer_model_incidence_summed_risk <- count(past_populations, 
                                            run,
                                            year, 
                                            name = 'sum_lung_cancer_risk', 
                                            wt = lung_cancer_year_risk) |> 
  filter(year != 2023) |> 
  group_by(year) |> 
  summarise(sum_lung_cancer_risk = mean(sum_lung_cancer_risk) * model_specification$population$scale_down_factor*20)

ggplot()+
  geom_line(data = cancer_model_incidence_count_states, mapping = aes(year,model_count)) + #1.7
  geom_line(data = cancer_incidence_ni, mapping = aes(year, cases),color='orange') +
  geom_line(data = cancer_model_incidence_summed_risk, mapping = aes(year, sum_lung_cancer_risk),color='mediumseagreen')+
  theme_minimal()


######## Dementia ##########


# https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(23)00214-1/fulltext#supplementary-material

# supplementary-material
dementia_incidence <- data.frame(
  year = c( 2004, 2006, 2008, 2010, 2012, 2014, 2016),
  year_range = c('2002-2006', '2004-2008', '2006-2010', '2008-2012', '2010-2014', '2012-2016', '2014-2018'),
  crude = c( 7.8, 8.0, 7.1, 6.2, 6.1, 6.4, 7.3),
  standardised = c( 9.8, 9.8, 9.0, 8.5, 7.0, 7.4, 8.0),
  'lt75' = c( 3.8, 3.5, 2.5, 2.7, 3.0, 2.8, 3.4),
  'gte75' = c( 26.5, 27.0, 28.2, 24.3, 19.6, 20.5, 21.2)
)

dementia_incidence_ni <- dementia_incidence |> 
  mutate(NI = crude/4000*1.9e6)|> 
  mutate(NI_standardised = standardised/4000*1.9e6)

dementia_model_incidence_count_states <- count(past_populations, run, year, dementia=year==dementia) |> 
  filter(dementia != F,year != 2023) |> #arrange(year,desc(n))
  group_by(year) |> 
  summarise(model_count = mean(n) * model_specification$population$scale_down_factor)

dementia_model_incidence_summed_risk <- count(past_populations, 
                                              run,
                                              year, 
                                              name = 'sum_dementia_risk', 
                                              wt = dementia_year_risk) |> 
  filter(year != 2023) |> 
  group_by(year) |> 
  summarise(sum_dementia_risk = mean(sum_dementia_risk) * model_specification$population$scale_down_factor)

dementia_tracked_incidence_counted_states <- dementia_outcome_incidence |> 
  count(year, run, wt = counted_states,name='counted_states') |> 
  group_by(year) |> 
  summarise(counted_states = mean(counted_states)*model_specification$population$scale_down_factor)

ggplot()+
  geom_line(data = dementia_model_incidence_count_states, mapping = aes(year,model_count)) + #1.7
  geom_line(data = dementia_model_incidence_summed_risk, mapping = aes(year, sum_dementia_risk),color='mediumseagreen')+
  geom_line(data = dementia_incidence_ni, mapping = aes(year, NI),color='orange') +
  geom_line(data = dementia_incidence_ni, mapping = aes(year, NI_standardised),color='gray') +
  geom_line(data = dementia_tracked_incidence_counted_states, mapping = aes(year, counted_states),color='#B33C7E')+
  geom_smooth(data = dementia_tracked_incidence_counted_states, mapping = aes(year, counted_states),color='#B33C7E',fill='#B33C7E')+
  
  theme_minimal()

######## Diabetes ##########
# Diabetes in pregnancy 'going unchecked'
# https://www.bbc.co.uk/news/uk-england-47625488
# Gestational diabetes an 'epidemic'
# https://www.bbc.co.uk/news/av/health-43649487

# denmark diabetes to 2006 
# source : https://link.springer.com/article/10.1007/s00125-008-1156-z/tables/1
year <- c("≤1989", 1990:2006, "1995–2006", "Total")
men <- c(1480, 21347, 10681, 8554, 9165, 12103, 7745, 8015, 7923, 8800, 9295, 
         9614, 10181, 11123, 12385, 12465, 11607, 12007, 121160, 184490)
women <- c(1310, 24738, 9987, 7855, 7639, 10733, 7148, 7388, 7528, 8039, 8537,
           8881, 9468, 10745, 11378, 11465, 10535, 10865, 111977, 174239)
all <- c(2790, 46085, 20668, 16409, 16804, 22836, 14893, 15403, 15451, 16839, 17832,
         18495, 19649, 21868, 23763, 23930, 22142, 22872, 233137, 358729)

# denmark population year 2000 5.34 million
# northern ireland population 1.9 million

denmark_diabetes_incidence <- data.frame(year = year, 
                                         men = men, 
                                         women = women, 
                                         all = all,
                                         all_ni = all * 1.9 / 5.34 )

# the denmark diabeetes register is only considered valid as of 1995

print(denmark_diabetes_incidence)


# Estimates of Diabetes Incidence and Prevalence from prescription data in ireland
# source: https://drc.bmj.com/content/5/1/e000288#sec-11

breakdown <- c( "Total population", "Total population ≥15 years", "<15 years", "15–24 years", "25–34 years", "35–44 years", "45–54 years", "55–64 years", "65–69 years", "70+ years", "Women", "Men")
GMS <- c(15788, 15353, 213, 405, 936, 1584, 2459, 3485, 2406, 5585, 7242, 8165)
LTI <- c(5786, 5679, 234, 55, 293, 819, 1523, 2026, 971, 413, 1959, 3815)
Total <- c(21574, 21032, 447, 460, 1229, 2403, 3982, 5511, 3377, 5998, 9201, 11980)
Population_CSO <- c(4470043, 3476995, 992881, 552678, 731033, 692610, 569248, 437596, 161984, 323305, 2268601, 2202383)
Estimate_percent <- c(0.48, 0.60, 0.05, 0.08, 0.17, 0.35, 0.70, 1.26, 2.08, 1.86, 0.41, 0.54)
CI_95 <- c( "0.48 to 0.49", "0.60 to 0.61", "0.04 to 0.05", "0.08 to 0.09", "0.16 to 0.18", "0.33 to 0.36", "0.68 to 0.72", "1.23 to 1.29", "2.02 to 2.15", "1.81 to 1.9", "0.40 to 0.41", "0.53 to 0.55"
)

ireland_diabetes_incidence <- data.frame(
  breakdown,
  GMS,
  LTI,
  Total,
  Population_CSO,
  Estimate_percent,
  CI_95
)

print(df)


diabetes_incidence_ni <- diabetes_incidence |> 
  mutate(NI = crude/4000*1.9e6)|> 
  mutate(NI_standardised = standardised/4000*1.9e6)

diabetes_model_incidence_count_states <- count(past_populations, run, year, diabetes=year==diabetes) |> 
  filter(diabetes != F,year != 2023) |> #arrange(year,desc(n))
  group_by(year) |> 
  summarise(model_count = mean(n) * model_specification$population$scale_down_factor)

diabetes_model_incidence_summed_risk <- count(past_populations, 
                                              run,
                                              year, 
                                              name = 'sum_diabetes_risk', 
                                              wt = diabetes_year_risk) |> 
  filter(year != 2023) |> 
  group_by(year) |> 
  summarise(sum_diabetes_risk = mean(sum_diabetes_risk) * model_specification$population$scale_down_factor)

diabetes_tracked_incidence_counted_states <- diabetes_outcome_incidence |> 
  count(year, run, wt = counted_states,name='counted_states') |> 
  group_by(year) |> 
  summarise(counted_states = mean(counted_states)*model_specification$population$scale_down_factor)

ggplot()+
  geom_line(data = diabetes_model_incidence_count_states, mapping = aes(year,model_count)) + #1.7
  geom_line(data = diabetes_model_incidence_summed_risk, mapping = aes(year, sum_diabetes_risk),color='mediumseagreen')+
  geom_line(data = diabetes_incidence_ni, mapping = aes(year, NI),color='orange') +
  geom_line(data = diabetes_incidence_ni, mapping = aes(year, NI_standardised),color='gray') +
  geom_line(data = diabetes_tracked_incidence_counted_states, mapping = aes(year, counted_states),color='#B33C7E')+
  geom_smooth(data = diabetes_tracked_incidence_counted_states, mapping = aes(year, counted_states),color='#B33C7E',fill='#B33C7E')+
  
  theme_minimal()

########################## Population ###############################

# not already summarised
dead_population |> 
  count(run,year,dementia,name='dead') |> 
  group_by(year,d=dementia!=0) |> 
  summarise(dead = mean(dead)* model_specification$population$scale_down_factor,.groups = 'drop') |> 
  count(year,wt=dead,name='dead') |> 
  ggplot() +
  geom_point(aes(year,dead),lwd =3, col='black') +
  geom_smooth(aes(year,dead),lwd =3, col='orange',alpha =0.1)+
  theme_minimal() #+
# geom_smooth(aes(year,dead))

# look at alive population by age at start
initial_time_zero_population |> 
  group_by(year, age20, run) |> 
  summarise(n = n()) |> 
  summarise(`Population (K)` = mean(n)* model_specification$population$scale_down_factor/1000) |> 
  ggplot(aes(age20, `Population (K)`, fill = age20,color = age20)) + 
  geom_col() +
  scale_color_manual(values = set3) +
  scale_fill_manual(values = set3) +
  geom_label(show.legend = F,aes(label = round(`Population (K)`)),fill='white')+
  theme_minimal()

# look at alive population by HSCT at start
initial_time_zero_population |> 
  group_by(year, HSCT, run) |> 
  summarise(n = n()) |> 
  summarise(`Population (K)` = mean(n)* model_specification$population$scale_down_factor/1000) |> 
  ggplot(aes(HSCT, `Population (K)`, fill = HSCT,color = HSCT)) + 
  geom_col() +
  scale_color_manual(values = pastel_colors) +
  scale_fill_manual(values = pastel_colors) +
  geom_label(show.legend = F,aes(label = round(`Population (K)`)),fill='white')+
  theme_minimal()

# look at alive population by age over time
past_populations |> 
  group_by(year, age20, run) |> 
  filter(age20 != '0-20') |> 
  summarise(n = n()) |> 
  summarise(`Population (K)` = mean(n) * sppg_specification$population$scale_down_factor/1000) |> 
  ggplot(aes(year, `Population (K)`, group = age20, colour = age20)) + 
  geom_line() +
  scale_color_manual(values = pastel_colors) +
  scale_fill_manual(values = pastel_colors) +
  geom_label(show.legend = F,aes(label = paste(round(`Population (K)`)))) +
  theme_minimal()






# onset to pervalence distribution -----


count(  heart_failure_outcome_incidence, wt= counted_states*model_specification$population$scale_down_factor)


count(past_populations,run,year,heart_failure) |> 
  group_by(heart_failure,year) |> 
  summarise(n = mean(n)) |> 
  filter(heart_failure!=0) |> 
  
  ggplot(aes(year,n,fill = as.character(heart_failure)))+
  geom_col()+theme_minimal()+scale_fill_manual(values = set3)

library(RColorBrewer)
set3 <- RColorBrewer::brewer.pal(name='Set3', n =12)

count(past_populations,run,year,stroke) |> 
  group_by(stroke,year) |> 
  summarise(n = mean(n)) |> 
  filter(stroke!=0) |> 
  
  ggplot(aes(year,n,fill = as.character(stroke)))+
  geom_col()+theme_minimal()+scale_fill_manual(values = set3)



count(past_populations,run,year,lung_cancer) |> 
  group_by(lung_cancer,year) |> 
  summarise(n = mean(n)) |> 
  filter(lung_cancer!=0) |> 
  ggplot(aes(year,n,fill = as.character(lung_cancer)))+
  geom_col()+theme_minimal()+scale_fill_manual(values = set3)


##################### GRAPH OUTPUT #####################

stroke_incidence_scotland <-  data.frame(
  year = c(2013, 2014, 2015,	2016,	2017,	2018,	2019,	2020,	2021,	2022),
  per100k = c(159.8, 163.6, 166.6, 169.7, 168.9, 168.9, 169.8, 171.3, 175.3, 179.4) 
)

plot(stroke_incidence_scotland)

# Public Health Scotland ASR
# https://publichealthscotland.scot/publications/scottish-heart-disease-statistics/scottish-heart-disease-statistics-year-ending-31-march-2022/

heart_failure_incidence_scotland <-  data.frame(
  year = c(2013, 2014, 2015,	2016,	2017,	2018,	2019,	2020,	2021,	2022),
  ASRper100k = c(105.7,	101.3,	100.3,	102.3,	102.7,	102.6,	104.2,	105.2,	95.7,	103.4),
  NI = c(105.7,	101.3,	100.3,	102.3,	102.7,	102.6,	104.2,	105.2,	95.7,	103.4) * 19
)

plot(heart_failure_incidence_scotland)

library(readxl)
cancer_incidence_ni <- read_excel("data/ni_cancer_registry/all_cancers_data tables.xlsx", 
                                  sheet = "T03", skip = 5)

cancer_incidence_ni <- cancer_incidence_ni |> 
  select(year = `Year of diagnosis`, `Total number of cases`)

plot(cancer_incidence_ni)
############ GRAPH risk factors #####################

hold_risk_factors %>%
  count(year,sex,category,name,run,wt=n) |> 
  
  #mutate(multiplier=run) %>% 
  #filter(!run%in%c(0,1)) %>% 
  
  mutate(  category1 = case_when(
    category == 'bp' ~ 'Blood Pressure',
    category == 'cholesterol' ~ 'Cholesterol',
    category == 'atrial_fibrillation' ~ 'atrial_fibrillation',
  ),
  name1 = case_when(
    name =='high_bp' ~ 'Normal',
    name =='normal_bp' ~ 'Risky',
    name == 'high_cholesterol' ~ 'Risky',
    name == 'normal_cholesterol' ~ 'Normal',
    name == 'atrial_fibrillation' ~ 'AF') ) %>% 
  #pivot_wider(id_cols = -value,names_from = category,values_from = name) %>%
  #count(year,name1,run,category1,wt=n) %>%
  mutate(intervention = run <= (model_specification$model$number_of_runs/2)) |> 
  group_by(year,name1,category1,intervention) |> 
  summarise(n = mean(n)) |> 
  ggplot() +
  geom_line(aes(year,
                n,
                #alpha=`impact parameter`,
                group = run)) +
  facet_wrap(~category1+name1,scales = 'free') +
  theme_classic() # +
# labs(title='Plot of the impacted physiological states of individuals over 20yr of a population wide statin prescription',
#       subtitle = 'Simulating the effect of a universal, population wide statin prescription.\nThe "impact parameter" is a measure of impact of the prescription. It is simulated from 0% to 10% efficacy at 1% increments\n'
#      ) 

################## GRAPH AGE #####################

hold_outcome %>% 
  filter(year == 2021) |> 
  select(-year) |> 
  count(age,sex,run,wt=n) |> 
  mutate(intervention = run <= (model_specification$model$number_of_runs/2)) |> 
  group_by(sex,age,intervention) |> 
  summarise(n = mean(n)) |> 
  ggplot() +
  geom_line(aes(
    group=interaction(sex,intervention),
    lty=sex,
    color=intervention, 
    age,
    n*model_specification$population$scale_down_factor),
    alpha=0.5) +
  geom_smooth(method='gam',
              aes(
                group=interaction(sex,intervention),
                lty=sex,
                color=intervention, 
                age,
                n * model_specification$population$scale_down_factor),
              alpha = 0.5)# +
#facet_wrap(~LGD2014NAME)

################## Graph Deprivation #####################

hold_outcome %>% 
  filter(year == 2021) |> 
  select(-year) |> 
  count(age,sex,run,wt=n) |> 
  mutate(intervention = run <= (model_specification$model$number_of_runs/2)) |> 
  group_by(sex,age,intervention) |> 
  summarise(n = mean(n)) |> 
  ggplot() +
  geom_line(aes(
    group=interaction(sex,intervention),
    lty=sex,
    color=intervention, 
    age,
    n*model_specification$population$scale_down_factor),
    alpha=0.5) +
  geom_smooth(method='gam',
              aes(
                group=interaction(sex,intervention),
                lty=sex,
                color=intervention, 
                age,
                n * model_specification$population$scale_down_factor),
              alpha = 0.5)# +
#facet_wrap(~LGD2014NAME)


################## GRAPH GENDER #####################

hold_outcome %>% 
  filter(year == 2021) |> 
  select(-year) |> 
  count(sex,run,wt=n) |> 
  mutate(intervention = run <= (model_specification$model$number_of_runs/2)) |> 
  group_by(sex,intervention) |> 
  summarise(n = mean(n)) |> 
  ggplot() +
  geom_col(position = 'dodge',aes(
    y=sex,
    fill=intervention, 
    x=n*model_specification$population$scale_down_factor),
    alpha=0.5)

###################### Epi Plot #########################

hold_outcome %>% 
  #mutate(run=as.character(run)) %>% 
  group_by(year,run) %>%# = run>=5 #filter(!run%in%c(0,1)) %>% 
  summarise(n=sum(n)) %>% 
  mutate(intervention = ifelse(run <= (model_specification$model$number_of_runs/2),
                               'normal',
                               'intervention')) %>%
  group_by(year,intervention) %>%# = run>=5 #filter(!run%in%c(0,1)) %>%
  summarise(n=mean(n)) %>%
  mutate(
    # multiplier=run,
    #`impact parameter` = as.factor(10-run),
    incidence = n*model_specification$population$scale_down_factor*11.18-10)%>% #*0.6 -18970# 25130 #- 3449e1
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
              mapping = aes(x= year , y=per100k*19*0.3+2340,color='calibration'),lty=2 )


############ GRAPH 4 #####################


( 
  y1 <- hold_outcome2 %>% 
    #mutate(run=as.character(run)) %>% 
    group_by(year,run) %>%# = run>=5 #filter(!run%in%c(0,1)) %>% 
    summarise(n=sum(n)) %>% 
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
