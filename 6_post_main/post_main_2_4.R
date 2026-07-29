library(fst)
library(echarts4r)
library(data.table)
library(tidyverse)
library(DBI)
# past_populations <- read.fst(path = './past_populations/past_populations_obesity_interventions_10_01_2026.fst')
#open connection

library(foreach)
library(doParallel)
library(fst)
registerDoParallel(4L)
threads_fst(5)
data.table::setDTthreads(5) #this is so that don't use all the processors



con <- dbConnect(duckdb::duckdb(), dbdir = 'past_populations_db/past_populations.duckdb', read_only = F)
latest_tbl <- sort(decreasing = T,dbListTables(con))[1]

# x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' USING SAMPLE 60 PERCENT (bernoulli);'))  # Set cache size to 2MB
x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' ;'))  # Set cache size to 2MB

# past_populations_20260116_015236

past_populations <- dbFetch(x)
dbClearResult(x)
dbDisconnect(con, shutdown=TRUE)
setDT(past_populations)

# write.fst(past_populations,path = 'past_populations_new_schama.fst')
# read.fst(path = 'past_populations_new_schama.fst')

# past_populations[ intervention == 'non-intervention' & year == 2021 & run>5,
#            intervention :='intervention']

past_populations[,.(.N),by=.(intervention,year,run)
][,.(N=mean(N)),by=.(intervention,year)
]

past_populations[,.(.N),by=.(intervention,year,run)
][,.(N=mean(N)),by=.(intervention,year)
][, year := as.character(year)] %>% 
  group_by(intervention) %>%
  e_charts(year) %>% 
  e_tooltip(trigger='axis') %>% 
  e_title(subtext = 'Population Count across interventions') %>% 
  e_line(N) 

#Target

past_populations[,.(target =sum(target,na.rm = T),n=.N),by=.(year,intervention,run)
][!is.na(target), .(perc=mean(target)/mean(n)*100,n=mean(target)),by=.(year,intervention)
][, year := as.character(year)] %>%
  group_by(intervention) %>%
  e_charts(year) %>% 
  e_tooltip() %>% 
  e_title(subtext = 'Percentage of the population targeted across interventions') %>% 
  e_line(perc) #%>% 
# e_line(n)

past_populations[,.(target =sum(target,na.rm = T),n=.N),by=.(year,intervention,run)
][!is.na(target), .(perc=mean(target)/mean(n)*100,n=mean(target)),by=.(year,intervention)
][, year := as.character(year)] %>%
  group_by(intervention) %>%
  e_charts(year) %>% 
  e_tooltip()%>% 
  e_title(subtext = 'Number of the population targeted across interventions') %>%
  e_line(n)

past_populations[intervention == 'intervention',.(target =sum(target,na.rm = T),n=.N),by=.(year,run,HSCT)
][!is.na(target), .(perc=mean(target)/mean(n)*100),by=.(year,HSCT)
][, year := as.character(year)] %>%
  group_by(HSCT) %>% 
  e_charts(year) %>% 
  e_tooltip() %>% 
  e_title(subtext = 'Percentage of the population targeted per HSCT') %>%
  e_line(perc)


past_populations[
  !is.na(target) & !is.na(qrisk_score) &intervention == 'non-intervention',
  .(mean_risk = mean(qrisk_score, na.rm = TRUE)),
  by = .(year, target, run)
][ , .(mean_risk = mean(mean_risk, na.rm = TRUE)),  # average across MC runs
   by = .(year,target)
][, year := as.character(year)
] %>%
  mutate(target = ifelse(target == 1, "Targeted", "Rest of Population")) %>%
  group_by(target) %>% 
  e_charts(year) %>%
  e_tooltip() %>%
  e_title(subtext = 'Risk of target and rest of the population') %>% 
  e_line(mean_risk)

idf <- past_populations[
  target==T & !is.na(qrisk_score),
  .(mean_risk = mean(qrisk_score, na.rm = TRUE)),
  by = .(year, target,intervention, run)
][ , .(mean_risk = mean(mean_risk, na.rm = TRUE)),  # average across MC runs
   by = .(year,target,intervention)
][, year := as.character(year)
] 

idf%>%
  # mutate(target = ifelse(target == 1, "Targeted", "Rest of Population")) %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_tooltip(trigger = 'axis') %>%
  e_title(subtext = 'Mean Risk of target in and out of intervention') %>% 
  e_line(mean_risk)


past_populations[
  target==T & !is.na(lung_cancer_year_risk),
  .(.N,mean_risk = mean(lung_cancer_year_risk, na.rm = TRUE)),
  by = .(year, target, run,intervention)
][
  , .(mean_risk = mean(mean_risk*N, na.rm = TRUE)/sum(N)),  # average across MC runs
  by = .(year,target,intervention)
][
  , year := as.character(year)
] %>%
  mutate(target = ifelse(target == 1, "Targeted", "Rest of Population")) %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_tooltip() %>%
  e_title(subtext = 'Mean Lung Cancer Risk of target in and out of intervention') %>% 
  e_line(mean_risk)

past_populations[
  target==T & !is.na(qrisk_score),
  .(.N,mean_risk = mean(qrisk_score, na.rm = TRUE)),
  by = .(year, target, run,intervention)
][
  , .(mean_risk = mean(mean_risk*N, na.rm = TRUE)/sum(N)),  # average across MC runs
  by = .(year,target,intervention)
][
  , year := as.character(year)
] %>%
  group_by(intervention) %>%
  e_charts(year) %>%
  e_tooltip() %>%
  e_title(subtext = 'Mean QRisk Score of target in and out of intervention') %>%
  e_line(mean_risk)

past_populations[
  target==T & !is.na(stroke_year_risk),
  .(.N,mean_risk = mean(stroke_year_risk, na.rm = TRUE)),
  by = .(year, target, run,intervention)
][
  , .(mean_risk = mean(mean_risk*N, na.rm = TRUE)/sum(N)),  # average across MC runs
  by = .(year,target,intervention)
][
  , year := as.character(year)
] %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_tooltip() %>%
  e_title(subtext = 'Mean Stroke Risk of target in and out of intervention') %>% 
  e_line(mean_risk)


past_populations[
  target==T & !is.na(hypertension_year_risk),
  .(.N,mean_risk = mean(hypertension_year_risk, na.rm = TRUE)),
  by = .(year, target, run,intervention)
][
  , .(mean_risk = mean(mean_risk*N, na.rm = TRUE)/sum(N)),  # average across MC runs
  by = .(year,target,intervention)
][
  , year := as.character(year)
] %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_tooltip(trigger='axis')%>%
  e_title(subtext = 'Hypertension Risk of target in and out of intervention') %>%
  e_line(mean_risk)

past_populations[
  target==T & !is.na(chronic_kidney_disease_year_risk),
  .(.N,mean_risk = mean(chronic_kidney_disease_year_risk, na.rm = TRUE)),
  by = .(year, target, run,intervention)
][
  , .(mean_risk = mean(mean_risk*N, na.rm = TRUE)/sum(N)),  # average across MC runs
  by = .(year,target,intervention)
][
  , year := as.character(year)
] %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_tooltip(trigger='axis') %>%
  e_title(subtext = 'Kidney Disease Risk of target in and out of intervention')%>%
  e_line(mean_risk)

past_populations[
  target==T & !is.na(chronic_kidney_disease_year_risk),
  .(.N,mean_risk = mean(chronic_kidney_disease_year_risk, na.rm = TRUE)),
  by = .(year, target, run,intervention)
][
  , .(mean_risk = mean(mean_risk*N, na.rm = TRUE)/sum(N)),  # average across MC runs
  by = .(year,target,intervention)
][
  , year := as.character(year)
] %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_tooltip(trigger='axis') %>%
  e_title(subtext = 'chd Disease Risk of target in and out of intervention')%>%
  e_line(mean_risk)

past_populations[
  target==T & !is.na(diabetes_year_risk),
  .(.N,mean_risk = mean(diabetes_year_risk, na.rm = TRUE)),
  by = .(year, target, run,intervention)
][
  , .(mean_risk = mean(mean_risk*N, na.rm = TRUE)/sum(N)),  # average across MC runs
  by = .(year,target,intervention)
][
  , year := as.character(year)
] %>%
  group_by(intervention) %>% 
  e_charts(year) %>%
  e_tooltip(trigger='axis') %>%
  e_title(subtext = 'diabetes Disease Risk of target in and out of intervention')%>%
  e_line(mean_risk)

past_populations[year==min(year)+2,.(.N),by=.(bmi,mdm_quintile_soa_name,run,intervention,year)
][!is.na(bmi),.(N=mean(N,na.rm = T)),by=.(bmi,mdm_quintile_soa_name,intervention,year)
] %>% 
  dcast(bmi+mdm_quintile_soa_name + year ~ intervention, value.var = 'N'
  ) %>%
  group_by(bmi) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(mdm_quintile_soa_name) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_bar(intervention,lineStyle = list(type='solid')) %>%
  e_bar(`non-intervention`,itemStyle = list(decal= list(symbol='rect' )))


past_populations[year==min(year)+2,.(.N),by=.(bmi,mdm_quintile_soa_name,run,intervention,year)
][!is.na(bmi),.(N=mean(N,na.rm = T)),by=.(bmi,mdm_quintile_soa_name,intervention,year)
] %>% 
  dcast(bmi+mdm_quintile_soa_name + year ~ intervention, value.var = 'N'
  ) %>%
  mutate(mdm_quntile_soa_name = factor(mdm_quintile_soa_name,ordered = T,
                                       levels = c('Most Deprived',
                                                  'Quintile 2',
                                                  'Quintile 3',
                                                  'Quintile 4',
                                                  'Least Deprived'))) %>%
  mutate(bmi = factor(bmi,
                      ordered = T,
                      levels = c('normal',
                                 'overweight',
                                 'obese'))) %>%
  arrange(mdm_quntile_soa_name,bmi) %>% 
  mutate(diff =   intervention - `non-intervention`   ) %>% 
  group_by(bmi) %>% 
  mutate(year = as.character(year)) %>% 
  mutate(as.factor(mdm_quintile_soa_name)) %>% 
  echarts4r::e_chart(mdm_quintile_soa_name) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_legend(show = T,top=30) %>% 
  e_title('Change in Risk Exposure Upon Intervention in Population', 'Deprivation Quintile') %>%
  # e_bar(intervention,lineStyle = list(type='solid')) %>%
  e_bar(diff,itemStyle = list(decal= list(symbol='circle' )))

##RISK

# past_populations[ 
#   intervention=='intervention',
#                   bmi:=case_when(
#   bmi=='obese'~ "overweight",
#   bmi=='overweight' ~ "normal",
#   TRUE ~ bmi  # Keep original if something unexpected
# )
#   ]

past_populations[,.(.N),by=.(bmi,run,intervention,year)
][!is.na(bmi),.(N=mean(N,na.rm = T)),by=.(bmi,intervention,year)
] %>% 
  dcast(bmi + year ~ intervention, value.var = 'N'
  ) %>%
  group_by(bmi) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_legend(top=30) %>% 
  e_title('Change in Risk Exposure Upon Intervention in Population', 'Deprivation Quintile') %>%
  e_line(intervention,lineStyle = list(type='solid')) %>%
  e_line(`non-intervention`,lineStyle = list(type='dashed'))

#### Incidence ###
past_populations[year!=min(year),.(stroke = sum(stroke==year),stroke_risk = sum(stroke_risk),.N),by=.(run,intervention,year)
][,.(stroke = mean(stroke)*1,stroke_risk =mean(stroke_risk),N=mean(N)),by=.(intervention,year)
] %>% 
  group_by(intervention) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_legend(top=30) %>% 
  e_title('Stroke Risk in Population') %>%
  e_line(stroke,serie_name = 'Stroke') %>%
  e_line(stroke_risk,serie_name = 'stroke_risk',lineStyle = list(type='dashed'))

### cumulative incidence ###
past_populations[year!=min(year),.(stroke = sum(stroke==year),stroke_risk = sum(stroke_risk),.N),by=.(run,intervention,year)
][,.(stroke = mean(stroke),stroke_risk =mean(stroke_risk),N=mean(N)),by=.(intervention,year)
][, `:=` (c_stroke = cumsum(stroke)*10,
     c_stroke_risk = cumsum(stroke_risk)),by=.(intervention),
] %>% 
  group_by(intervention) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_line(c_stroke) #%>% 
  # e_line(c_stroke_risk,lineStyle = list(type='dashed'))

## averted_incidence
past_populations[year!=min(year),
][,.(stroke = sum(stroke==year),stroke_risk = mean(stroke_risk),.N),by=.(run,intervention,year)
][,.(stroke = mean(stroke),stroke_risk =mean(stroke_risk),N=mean(N)),by=.(intervention,year)
] %>% 
  dcast(year~intervention,value.var='stroke') %>%
  .[,averted := `non-intervention`-intervention] %>%
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_line(averted) 

## cumulative averted_incidence
idf <- past_populations[year!=min(year),.(stroke = sum(stroke==year),stroke_risk = mean(stroke_risk),.N),by=.(run,intervention,year)
][,.(stroke = mean(stroke),stroke_risk =mean(stroke_risk)*1900,N=mean(N)),by=.(intervention,year)
] %>% 
  dcast(year~intervention,value.var='stroke') 

idf[,averted := `non-intervention`-intervention
][,c_averted := cumsum(averted)] %>%
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_line(c_averted) 

# Prevalence
past_populations[,.(stroke = sum(stroke!=0),.N),by=.(run,intervention,year)
][,.(stroke = mean(stroke),N=mean(N)),by=.(intervention,year)
] %>% 
  group_by(intervention) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_line(stroke,serie_name = 'Strokes')

### Averted Prevalence ###
past_populations[,.(stroke = sum(stroke!=0),.N),by=.(run,intervention,year)
][,.(stroke = mean(stroke),N=mean(N)),by=.(intervention,year)
] %>% dcast(year~intervention,value.var='stroke') %>% 
  .[,averted := `non-intervention`-intervention] %>% 
  # group_by(intervention) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_line(averted,serie_name = 'Strokes')

### Cumulative Averted ###
df <- past_populations[,.(stroke = sum(stroke!=0),.N),by=.(run,intervention,year)
][,.(stroke = mean(stroke),N=mean(N)),by=.(intervention,year)
] %>% dcast(year~intervention,value.var='stroke') 

df[,averted := `non-intervention`-intervention
][,c_averted := cumsum(averted)] %>% 
  # group_by(intervention) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_line(c_averted,serie_name = 'Strokes')

past_populations[,.(stroke = sum(stroke!=0),.N),by=.(age20,run,intervention,year)
][,.(stroke = mean(stroke),N=mean(N)),by=.(age20,intervention,year)
] %>% 
  group_by( age20,intervention) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_tooltip(trigger = 'axis') %>% 
  e_line(stroke,serie_name = 'Strokes')

past_populations[,.(stroke = stroke!=0,.N),by=.(age20,run,intervention,year)
][stroke==T,.(N=mean(N)),by=.(age20,intervention,year)
]

stroke_rates <- past_populations[
  ,
  .(
    stroke = stroke != 0,
    pop = .N
  ),
  by = .(HSCT, run, intervention, year)
][
  ,
  .(
    stroke_n = sum(stroke),
    pop_n    = mean(pop)
  ),
  by = .(HSCT, run, intervention, year)
][
  ,
  .(
    stroke_n = mean(stroke_n),
    pop_n    = mean(pop_n)
  ),
  by = .(HSCT, intervention, year)
][
  ,
  rate_per_100 := (stroke_n / pop_n) * 100
]

wide <- stroke_rates %>% 
  dcast(
    HSCT + year ~ intervention,
    value.var = "rate_per_100"
  )


wide[,diff :=(  `non-intervention`-intervention)*model_specification$population$scale_down_factor ]
wide[,csum := cumsum(diff), by =HSCT] %>% 
  group_by(HSCT) %>% 
  mutate(year = as.character(year)) %>% 
  echarts4r::e_chart(year) %>%
  e_line(diff,serie_name = 'diff')

# wide %>% 
#   group_by(age20) %>% 
#   mutate(year = as.character(year)) %>% 
#   echarts4r::e_chart(year) %>%
#   e_line(diff,serie_name = 'diff')
# 
# stroke_rates %>% 
#   group_by(age20,intervention) %>% 
#   mutate(year = as.character(year)) %>% 
#   echarts4r::e_chart(year) %>%
#   e_line(rate_per_100,serie_name = 'Strokes')

plot_diff_by_facet <- function(past_populations, f = HSCT, facet = "HSCT", morbidity = "stroke",diff=T) {
  stopifnot(data.table::is.data.table(past_populations))
  
  stroke_rates <- past_populations[
    ,
    .(
      morb = get(morbidity) != 0,
      pop  = .N
    ),
    by = c(facet, "run", "intervention", "year")
  ][
    morb == TRUE,
    .(
      morb_n = sum(morb),
      pop
    ),
    by = c(facet, "run", "intervention", "year")
  ][
    ,
    .(
      morb_n = mean(morb_n)*model_specification$population$scale_down_factor,
      pop_n  = mean(pop)*model_specification$population$scale_down_factor
    ),
    by = c(facet, "intervention", "year")
  ][
    ,
    rate_per_100 := (morb_n / pop_n) * 100
  ]
  
  wide <- data.table::dcast(
    stroke_rates,
    stats::as.formula(paste0(facet, " + year ~ intervention")),
    value.var = 'morb_n'#"rate_per_100"
  )
  
  wide[
    , diff := ( `non-intervention` - intervention ) 
  ]
  
  x <- wide %>%
    dplyr::group_by({{f}}) %>%
    dplyr::mutate(year = as.character(year)) %>%
    echarts4r::e_charts(year)
  if(diff){
    x <- x %>%
      echarts4r::e_bar(diff,
                       lineStyle = list(type='solid'))
  }else{
    x <- x %>% 
      echarts4r::e_line(intervention,
                        lineStyle = list(type='solid')) %>% 
      # e_data() %>% 
      echarts4r::e_line(`non-intervention`,
                        lineStyle = list(type='dashed')) %>% 
      e_title(subtext = 'intervention solid line')
  }
  x
}

plot_diff_by_facet(past_populations, f = mdm_quintile_soa_name, facet = "mdm_quintile_soa_name", morbidity = "stroke", diff = T)

plot_diff_by_facet(past_populations, f = HSCT, facet = "HSCT", morbidity = "stroke", diff = T)
plot_diff_by_facet(past_populations, f = HSCT, facet = "HSCT", morbidity = "stroke", diff = F)

past_populations[,.(target =sum(target,na.rm = T),n=.N),by=.(year,run,HSCT)
][!is.na(target), .(perc=mean(target)/mean(n)*100),by=.(year,HSCT)
][, year := as.character(year)] %>%
  group_by(HSCT) %>% 
  e_charts(year) %>% 
  e_tooltip() %>% 
  e_line(perc)

past_populations[,.(n=.N),by=.(year,run,HSCT)
][, .(n=mean(n)),by=.(year,HSCT)
][, year := as.character(year)] %>% 
  group_by(HSCT) %>% 
  e_charts(year) %>% 
  e_line(n)

past_populations[,.(deaths =sum(death!=0 & !is.na(death)),n=.N),by=.(year,run,HSCT)
][!is.na(deaths), .(perc=mean(deaths)/mean(n),
                    deaths=mean(deaths)*model_specification$population$scale_down_factor),by=.(year,HSCT)
][, year := as.character(year)] %>% 
  group_by(HSCT) %>% 
  e_charts(year) %>% 
  e_tooltip() %>% 
  e_line(deaths) %>%
  e_line(perc)

past_populations[,.(stroke = sum(stroke==year), N = .N), by=.(year,run,intervention)]


# write_fst(past_populations, './past_populations/past_populations_04_01_2026.fst')
# write_fst(past_populations, './past_populations/past_populations_obesity_interventions_10_01_2026.fst')

### Mortality -----

count(current_population,lung_cancer,wt=qmortality_risk)  #mean(qmortality_risk),

current_population %>% 
  filter(is.na(qmortality_risk)) %>% 
  apply_qmortality_mortality() %>% pull(qmortality_risk) %>% 
view()

#Cancer 

expand.grid(year = model_specification$model$start_year:(model_specification$model$start_year+model_specification$model$duration-1),
                      run = 1:model_specification$model$number_ofq_runs
) %>% 
  mutate( death = model_specification$model$start_year:(model_specification$model$start_year+model_specification$model$duration-1))


yearly_dead_has_cancer <- dead_population %>% 
  count( run, death=(death), lung_cancer = (lung_cancer != 0), name = 'count' ) %>% 
  filter( lung_cancer == TRUE ) %>% select(-c(lung_cancer)) %>% 
  left_join(expand.grid(death = model_specification$model$start_year:(model_specification$model$start_year+model_specification$model$duration-1),
                        run = 1:model_specification$model$number_of_runs
  ),.) %>%
  replace_na( list(count = 0) ) %>%
  group_by( death) %>% 
  summarise( count = mean(count) ) %>% 
  mutate(NI = count * model_specification$population$scale_down_factor/0.6)

cancer_mortality <- read_excel("data/NIcancer_registry/all_cancers_data_tables.xlsx", 
                               sheet = "T19", skip = 5)

names(cancer_mortality)[1:5] <- c('year', 'count', 'per100k', 'standarised_per100k','ci')

cancer_mortality %>% 
  mutate(NI = 19 * per100k) %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot() +
  geom_line(aes(year,NI), lwd = 2)+
  geom_line(yearly_dead_has_cancer,mapping = aes(death,NI),col='orange')+
  theme_minimal()

##### STROKES are different - they are ACUTE and REPEATABLE #####
# look at instances where a persons latest stroke has occurred
# STROKES

past_populations %>% 
  group_by(year,stroke,run) %>% #View() 
  summarise(counted_states = sum(stroke!=0),
            .groups = "drop") %>%
  group_by(year,stroke) %>% 
  summarise(counted_states = mean(counted_states),
            .groups = "drop") %>%  
  #filter(!stroke %in% c(0, 2017)) %>% 
  ggplot() +
  geom_point(aes(year,counted_states,col = as.character(stroke))) +
  geom_line(aes(year,counted_states,group = stroke, col = as.character(stroke)))

# total number of strokes suffered by survivors that are still alive
count(past_populations,id,stroke) %>% 
  filter(stroke!=0) %>% #filter(id==878)
  count(id) %>% 
  summarise(total_strokes_survivors_alive = sum(n),
            stroke_survivors_alive = n())

