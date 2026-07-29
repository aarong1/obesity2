library(echarts4r)
library(dplyr)
library(htmltools)
library(data.table)

plot_outputs_averted_incidence <- function(past_populations, morbidity = 'stroke') {
  
  # print(past_populations[,get('morbidity')])
  
  past_populations[,morb := get('stroke')]

x <- past_populations[ year!=min(year),.(morbidity=sum(get(morbidity)==year,na.rm=T)), by=.(year,run,intervention)
][, .(morbidity=mean(morbidity)), by=.(year,intervention)
][, year := as.character(year)
] 
# print(x)
x%>%
  dcast(formula =   year ~ intervention, value.var = 'morbidity', fill = 0L) %>%
  mutate(averted=`non-intervention` - intervention) %>%
  mutate(averted=cumsum(averted)) %>%
  e_charts(year) %>%
  e_line(averted) 
}

# past_populations[,morb := get('stroke')]
# sum(past_populations$morb!=0)

plot_outputs_averted_incidence(past_populations, morbidity = 'stroke')

plot_outputs_incidence <- function(past_populations, morbidity = 'stroke') {
  
  past_populations[year!=min(year),.(inc = sum(get(morbidity)==year),.N),by=.(run,intervention,year)
  ][,.(inc = mean(inc),N=mean(N)),by=.(intervention,year)
  ] %>% 
    group_by( intervention) %>% 
    mutate(year = as.character(year)) %>% 
    echarts4r::e_chart(year,height = '150px') %>%
    e_tooltip(trigger = 'axis') %>% 
    e_line(inc,serie_name = morbidity) %>% 
    e_tooltip(trigger = 'axis') %>% 
    e_title(subtext = paste0('Incidence of ',morbidity))%>%
    e_x_axis(name = 'Year') %>%
    # e_x_axis(show = FALSE) %>%
    # e_y_axis(show = FALSE) %>%
    e_grid(
      left = 0,
      right = 0,
      top = 0,
      bottom = 0,
      containLabel = FALSE
    ) %>%
    e_tooltip(show = FALSE) %>%
    e_legend(show = FALSE)
  
}

# past_populations <- past_populations
# setDT(past_populations)

browsable(
  div(
plot_outputs_incidence(past_populations, 'chd'),

plot_outputs_incidence(past_populations, 'pad'),

plot_outputs_incidence(past_populations, 'chronic_kidney_disease'),

# plot_outputs_incidence(past_populations, 'vte'),

plot_outputs_incidence(past_populations, 'diabetes'),

plot_outputs_incidence(past_populations, 'rheumatoid_arthritis'),

plot_outputs_incidence(past_populations, 'copd'),

plot_outputs_incidence(past_populations, 'asthma'),

# plot_outputs_incidence(past_populations, 'depression'),

plot_outputs_incidence(past_populations, 'non_diabetic_hyperglycaemia'),

plot_outputs_incidence(past_populations, 'osteoporosis'),

plot_outputs_incidence(past_populations, 'cancer'),

plot_outputs_incidence(past_populations, 'osteoarthritis'),

plot_outputs_incidence(past_populations, 'epilepsy'),

plot_outputs_incidence(past_populations, 'hypothyroidism'),

plot_outputs_incidence(past_populations, 'colorectal_cancer'),

plot_outputs_incidence(past_populations, 'prostate_cancer'),

plot_outputs_incidence(past_populations, 'female_breast_cancer'),

plot_outputs_incidence(past_populations, 'renal_cancer'),

plot_outputs_incidence(past_populations, 'oesophageal_cancer'),

plot_outputs_incidence(past_populations, 'stomach_cancer'),

plot_outputs_incidence(past_populations, 'osteogastric_cancer'),

plot_outputs_incidence(past_populations, 'oral_cancer'),

plot_outputs_incidence(past_populations, 'pancreatic_cancer'),
             
plot_outputs_incidence(past_populations, 'uterine_cancer'),

plot_outputs_incidence(past_populations, 'blood_multiple_myeloma'),

plot_outputs_incidence(past_populations, 'blood_lymphoma'),

plot_outputs_incidence(past_populations, 'blood_leukaemia'),

plot_outputs_incidence(past_populations, 'blood_cancer'),

plot_outputs_incidence(past_populations, 'ovarian_cancer'),

plot_outputs_incidence(past_populations, 'lung_cancer'),

plot_outputs_incidence(past_populations, 'stroke'),

plot_outputs_incidence(past_populations, 'dementia'),

plot_outputs_incidence(past_populations, 'heart_failure'),

plot_outputs_incidence(past_populations, 'atrial_fibrillation'),

plot_outputs_incidence(past_populations, 'hypertension'),

plot_outputs_incidence(past_populations, 'chronic_kidney_disease')
))


plot_outputs_prevalence <- function(past_populations, morbidity = 'stroke') {
  
  
  past_populations[,.(inc = sum(get(morbidity)!=0),.N),by=.(run,intervention,year)
  ][,.(inc = mean(inc),N=mean(N)),by=.(intervention,year)
  ] %>% 
    group_by( intervention) %>% 
    mutate(year = as.character(year)) %>% 
    echarts4r::e_chart(year) %>%
    e_tooltip(trigger = 'axis') %>% 
    e_line(inc,serie_name = morbidity) %>% 
    e_tooltip(trigger = 'axis') %>% 
    e_title(subtext = paste0('Prevalence of ',morbidity))
  
  
}

browsable(
  div(
    plot_outputs_prevalence(past_populations, 'chd'),
    
    plot_outputs_prevalence(past_populations, 'pad'),
    
    plot_outputs_prevalence(past_populations, 'chronic_kidney_disease'),
    
    # plot_outputs_prevalence(past_populations, 'vte'),
    
    plot_outputs_prevalence(past_populations, 'diabetes'),
    
    plot_outputs_prevalence(past_populations, 'rheumatoid_arthritis'),
    
    plot_outputs_prevalence(past_populations, 'copd'),
    
    plot_outputs_prevalence(past_populations, 'asthma'),
    
    # plot_outputs_prevalence(past_populations, 'depression'),
    
    plot_outputs_prevalence(past_populations, 'non_diabetic_hyperglycaemia'),
    
    plot_outputs_prevalence(past_populations, 'osteoporosis'),
    
    plot_outputs_prevalence(past_populations, 'cancer'),
    
    plot_outputs_prevalence(past_populations, 'osteoarthritis'),
    
    plot_outputs_prevalence(past_populations, 'epilepsy'),
    
    plot_outputs_prevalence(past_populations, 'hypothyroidism'),
    
    plot_outputs_prevalence(past_populations, 'colorectal_cancer'),
    
    plot_outputs_prevalence(past_populations, 'prostate_cancer'),
    
    plot_outputs_prevalence(past_populations, 'female_breast_cancer'),
    
    plot_outputs_prevalence(past_populations, 'renal_cancer'),
    
    plot_outputs_prevalence(past_populations, 'oesophageal_cancer'),
    
    plot_outputs_prevalence(past_populations, 'stomach_cancer'),
    
    plot_outputs_prevalence(past_populations, 'osteogastric_cancer'),
    
    plot_outputs_prevalence(past_populations, 'oral_cancer'),
    
    plot_outputs_prevalence(past_populations, 'pancreatic_cancer'),
    
    plot_outputs_prevalence(past_populations, 'uterine_cancer'),
  
    plot_outputs_prevalence(past_populations, 'blood_multiple_myeloma'),
    
    plot_outputs_prevalence(past_populations, 'blood_lymphoma'),
    
    plot_outputs_prevalence(past_populations, 'blood_leukaemia'),
    
    plot_outputs_prevalence(past_populations, 'blood_cancer'),
    
    plot_outputs_prevalence(past_populations, 'ovarian_cancer'),
    
    plot_outputs_prevalence(past_populations, 'lung_cancer'),
    
    plot_outputs_prevalence(past_populations, 'stroke'),
    
    plot_outputs_prevalence(past_populations, 'dementia'),
    
    plot_outputs_prevalence(past_populations, 'heart_failure'),
    
    plot_outputs_prevalence(past_populations, 'atrial_fibrillation'),
    
    plot_outputs_prevalence(past_populations, 'hypertension'),
    
    plot_outputs_prevalence(past_populations, 'chronic_kidney_disease')
  ))


# mult <- case_fatality_rate_past_populations[case_fatality_rate_past_populations$morbidity==morbidity,'case_fatality_rate']
# 
# d <- past_populations[,.(m = sum(get(morbidity)!=0)), by=c('year','run','intervention')
#                                         ][, .(prevalence = mean(m)), by=c('year','intervention')][
#                                           ,deaths:=prevalence * mult]

plot_outputs_deaths <- function(past_populations, morbidity = 'stroke') {
  
  agg_dead <- past_populations[year != min(year) & death_reason == morbidity, .(dead =.N),by=.(run,intervention,year)
  ] 
  
  full_dead <- CJ(run = unique(past_populations$run),year = unique(past_populations$year)[-1])
  
  full_dead[,intervention := ifelse(run>max(run)/2,'intervention','non-intervention')]
  
  full_dead <- full_dead[agg_dead, on = .(run, year,intervention), `:=` (dead   = i.dead)]
  
  full_dead[is.na(dead), dead := 0]
  
  full_dead[,.( dead= mean(dead)),by=.(intervention,year)
  ] %>%
    group_by( intervention) %>% 
    mutate(year = as.character(year)) %>% 
    echarts4r::e_chart(year) %>%
    e_tooltip(trigger = 'axis') %>% 
    e_line(dead,serie_name = morbidity) %>% 
    e_tooltip(trigger = 'axis') %>% 
    e_title(subtext = paste0('Deaths due to ',morbidity))
  
}

sum(pp$death)

range(current_population$year)
range(current_population$death)

# [1] 2024 2024
range(past_populations$year)
# [1] 2021 2024
range(past_populations$death)
# [1]    0 2024
range(dead_population$death)

setDT(past_populations)


browsable(
  div(
    plot_outputs_deaths(past_populations, 'chd'),
    
    plot_outputs_deaths(past_populations, 'pad'),
    
    plot_outputs_deaths(past_populations, 'chronic_kidney_disease'),
    
    # plot_outputs_deaths(past_populations, 'vte'),
    
    plot_outputs_deaths(past_populations, 'diabetes'),
    
    plot_outputs_deaths(past_populations, 'rheumatoid_arthritis'),
    
    plot_outputs_deaths(past_populations, 'copd'),
    
    plot_outputs_deaths(past_populations, 'asthma'),
    
    # plot_outputs_deaths(past_populations, 'depression'),
    
    plot_outputs_deaths(past_populations, 'non_diabetic_hyperglycaemia'),
    
    plot_outputs_deaths(past_populations, 'osteoporosis'),
    
    plot_outputs_deaths(past_populations, 'cancer'),
    
    plot_outputs_deaths(past_populations, 'osteoarthritis'),
    
    plot_outputs_deaths(past_populations, 'epilepsy'),
    
    plot_outputs_deaths(past_populations, 'hypothyroidism'),
    
    plot_outputs_deaths(past_populations, 'colorectal_cancer'),
    
    plot_outputs_deaths(past_populations, 'prostate_cancer'),
    
    plot_outputs_deaths(past_populations, 'female_breast_cancer'),
    
    plot_outputs_deaths(past_populations, 'renal_cancer'),
    
    plot_outputs_deaths(past_populations, 'oesophageal_cancer'),
    
    plot_outputs_deaths(past_populations, 'stomach_cancer'),
    
    plot_outputs_deaths(past_populations, 'osteogastric_cancer'),
    
    plot_outputs_deaths(past_populations, 'oral_cancer'),
    
    plot_outputs_deaths(past_populations, 'pancreatic_cancer'),
    
    plot_outputs_deaths(past_populations, 'uterine_cancer'),
    
    plot_outputs_deaths(past_populations, 'blood_multiple_myeloma'),
    
    plot_outputs_deaths(past_populations, 'blood_lymphoma'),
    
    plot_outputs_deaths(past_populations, 'blood_leukaemia'),
    
    plot_outputs_deaths(past_populations, 'blood_cancer'),
    
    plot_outputs_deaths(past_populations, 'ovarian_cancer'),
    
    plot_outputs_deaths(past_populations, 'lung_cancer'),
    
    plot_outputs_deaths(past_populations, 'stroke'),
    
    plot_outputs_deaths(past_populations, 'dementia'),
    
    plot_outputs_deaths(past_populations, 'heart_failure'),
    
    plot_outputs_deaths(past_populations, 'atrial_fibrillation'),
    
    plot_outputs_deaths(past_populations, 'hypertension')
    
  ))



# births


plot_outputs_births<- function(past_populations) {
  
  past_populations[age == 0, .(births =.N), by=.(run,year)
  ][,.(births = mean(births)*model_specification$population$scale_down_factor), by=.(year)
  ][,year:=as.character(year)] %>% 
    e_charts(year) %>% 
    e_line(births)
}
  
  plot_outputs_births(past_populations)
