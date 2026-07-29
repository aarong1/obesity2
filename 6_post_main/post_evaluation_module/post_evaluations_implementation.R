# Multimorbidity Score 

library(echarts4r)
library(tidyverse)

# Example data
df <- data.frame(
  x = 1:6,
  y = c(25, 75, 125, 175, 250, 350)
)

df |> 
  e_charts(x) |> 
  e_line(y) |> 
  e_visual_map(
    y,
    type = "piecewise",
    top = 50,
    right = 10,
    pieces = list(
      list(gt = 0, lte = 50, color = "#93CE07"),
      list(gt = 50, lte = 100, color = "#FBDB0F"),
      list(gt = 100, lte = 150, color = "#FC7D02"),
      list(gt = 150, lte = 200, color = "#FD0100"),
      list(gt = 200, lte = 300, color = "#AA069F"),
      list(gt = 300, color = "#AC3B2A")
    ),
    outOfRange = list(
      color = "#999"
    )
  )


library(echarts4r)

# 1. Create a mock dataset mimicking your structure
# (In your real app, replace this with your actual dataframe)
dates <- seq(as.POSIXct("2009-06-12 02:00:00"), by = "hour", length.out = 400)
df <- data.frame(
  time = format(dates, "%Y/%m/%d %H:%M"),
  flow = rnorm(200, 10, 2),
  rainfall = rnorm(400, 5, 1)
)

df |> 
  e_charts(time) |> 
  # --- Series ---
  # e_line(flow, name = "Flow",y_index = 0, areaStyle = list()) |> 
  e_area(flow, name = "Flow",y_index = 0) %>%  
  e_area(rainfall, name = "Rainfall",y_index = 1) |> 
  # --- Title ---
  e_title("Rainfall and Flow Relationship", left = "center") |> 
  # --- Grid ---
  e_grid(bottom = 80) |> 
  # --- Tooltip (Mirroring axis trigger and crosshair) ---
  e_tooltip(
    trigger = "axis",
    axisPointer = list(
      type = "cross",
      animation = T,
      label = list(backgroundColor = "#505765")
    )
  ) |> 
  # --- Legend ---
  e_legend(left = 10) |> 
  e_y_axis(serie = flow, inverse=T, index = 0 ) %>% 
  
  e_y_axis(serie = rainfall, inverse=F, index = 1 ) %>% 

  # --- X Axis Customization ---
  e_x_axis(
    type = "category",
    boundaryGap = FALSE,
    axisLine = list(onZero = FALSE)
  )


library(echarts4r)
library(dplyr)

# 1. Sample Data
df <- data.frame(
  year = rep(2021:2024, each = 3),
  rank = c(1, 2, 3, 2, 1, 3, 2, 3, 1, 1, 2, 3),
  team = rep(c("Team A", "Team B", "Team C"), 4)
)

# 2. Build the Bump Chart



  disease_ranking <- past_populations %>% 
    melt(id.vars = c('id','intervention','year'),
       measure.vars =   c(
    'stroke',               'chd',                   'diabetes',             'hypothyroidism',      
    'asthma',               'copd',                  'non_diabetic_hyperglycaemia',                  'chronic_kidney_disease',      
    'dementia',             'heart_failure',         'lung_cancer',          'prostate_cancer',     
    'female_breast_cancer',         'colorectal_cancer',          'atrial_fibrillation',  'rheumatoid_arthritis',
    'osteoarthritis',       'epilepsy',              'osteoporosis',         'renal_cancer',        
    'oesophageal_cancer',   'stomach_cancer',        'oral_cancer',          'pancreatic_cancer',   
    'uterine_cancer',       'ovarian_cancer',        'blood_cancer')
  ) %>% 
  count(intervention, year, variable, value=value==year) %>% 
    filter(value==TRUE) %>% 
    dcast(variable +year~ intervention,value.var = 'n') %>% 
    mutate(averted = `non-intervention` - intervention) %>% 
    group_by(variable) %>% 
    filter(year != min(year)) %>% 
    mutate(cumulative_averted = cumsum(averted)) %>% 
    mutate(cumulative_averted = cumulative_averted-first(cumulative_averted)) %>% 
    
    # ungroup() %>%
    mutate(s=sum(cumulative_averted)) %>% 
    group_by(year) %>% 
    mutate(rk=rank(-cumulative_averted)) 
    
  disease_ranking %>% 
    ungroup() %>% 
    filter(year != min(year)) %>% 
    mutate(year = as.character(year)) %>% 
    group_by(variable) |> 
    e_charts(year) |> 
    e_line(rk, smooth = T, symbolSize = 10, symbol='circle',legend = F) |> 
    e_y_axis(
      inverse = T,          # Rank #1 at the top
      min = 1,                 # Start axis at 1
      max = 8,                 # End axis at total count
      interval = 1             # Ensure ranks are integers
    ) |> e_labels() %>% 
    e_theme('westeros') %>% 
    e_legend(type = 'scroll') %>%
    e_tooltip(trigger = "item") |> 
    # e_title("Averted Disease Over Time")%>% 
    e_tooltip(
      textStyle = list(fontSize = 10) # Adjust font size here
    ) %>%
    e_text_style(fontSize = 10) %>% 
    e_axis(
      axisLabel = list(fontSize = 10) # Set to desired size (default is 12)
    ) %>% 
    e_y_axis(
      axisLabel = list(fontSize = 10)
    )

  disease_ranking %>% 
    ungroup() %>% 
    filter(year %in% c(min(year)+1,max(year))) %>% 
    mutate(year = as.character(year)) %>% 
    group_by(variable) |> 
    e_charts(year) |> 
    e_line(rk, smooth = T, symbolSize = 10, symbol='circle',legend = F) |> 
    e_y_axis(
      inverse = T,          # Rank #1 at the top
      min = 1,                 # Start axis at 1
      max = 16,                 # End axis at total count
      interval = 1             # Ensure ranks are integers
    ) |> e_labels() %>% 
    e_theme('westeros') %>% 
    e_legend(type = 'scroll') %>%
    e_tooltip(trigger = "item") |> 
    # e_title("Averted Disease Over Time")%>% 
    e_tooltip(
      textStyle = list(fontSize = 10) # Adjust font size here
    ) %>%
    e_text_style(fontSize = 10) %>% 
    e_axis(
      axisLabel = list(fontSize = 10) # Set to desired size (default is 12)
    ) %>% 
    e_y_axis(
      axisLabel = list(fontSize = 10)
    )
  
past_populations <- compute_cmms_dt(past_populations)
past_populations <- add_multimorbidity_fn(past_populations)

past_populations %>% 
  # filter(year!=min(year)) %>%
  group_by(year, age10, run, intervention ) %>%
  summarise(multimorbidity = mean(multimorbidity,na.rm=T),n=n()) %>% 
  ungroup() %>% 
  complete(year,  age10, nesting(run,intervention), fill = list(cmms = 0,n=0)) %>% #View()
  group_by(year, intervention, age10 ) %>%
  summarise(multimorbidity = mean(multimorbidity),n=sum(n)) %>% 
  mutate(year = as.character(year)) %>%
  pivot_wider(id_cols = c(year,age10),names_from = intervention,values_from= multimorbidity, values_fill = 0L) %>%
  # dcast(formula = age10 + year ~ intervention, value.var = 'cmms', fill = 0L) %>%
  group_by(age10) %>%
  mutate(year=as.character(year)) %>%
  e_charts(year) %>% 
  e_tooltip() %>% 
  e_title(subtext = 'Intervention is solid, baseline is dashed') %>% 
  e_line(intervention) %>% 
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) 

past_populations %>% 
  # filter(year!=min(year)) %>%
  group_by(year, age10, run, intervention ) %>%
  summarise(cmms = mean(cmms,na.rm=T),n=n()) %>% 
  ungroup() %>% 
  complete(year,  age10, nesting(run,intervention), fill = list(cmms = 0,n=0)) %>% #View()
  group_by(year, intervention, age10 ) %>%
  summarise(cmms = mean(cmms),n=sum(n)) %>% 
  mutate(year = as.character(year)) %>%
  pivot_wider(id_cols = c(year,age10),names_from = intervention,values_from= cmms, values_fill = 0L) %>%
  # dcast(formula = age10 + year ~ intervention, value.var = 'cmms', fill = 0L) %>%
  group_by(age10) %>%
  mutate(year=as.character(year)) %>%
  e_charts(year) %>% 
  e_tooltip() %>% 
  e_title(subtext = 'Intervention is solid, baseline is dashed') %>% 
  e_line(intervention) %>% 
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) 


past_populations[year==max(year),.N,by=.(multimorbidity,run,age20, intervention)
                 ][,.(N = mean(N)),by = .(intervention, age20,multimorbidity)] %>% 
  mutate(multimorbidity=(as.character(multimorbidity))) %>%
  filter(!is.na(multimorbidity)) %>% 
  mutate(age20 = factor(age20,
                                        ordered=T,
                                        levels=c( '0-20',
                                                  '20-40',
                                                  '40-60',
                                                  '60-80',
                                                  '80-100',
                                                  '100-120'
                                                  ))) %>%
  group_by(age20,intervention) %>% 
  echarts4r::e_chart(multimorbidity) %>%
  # e_density(N,itemStyle=list(opacity=0.1)) #%>%
  echarts4r::e_bar(N,label = list(show=F)) %>% 
  e_title(subtext = 'Count of NCD Multimorbidity') %>% 
  e_tooltip() 


past_populations[year==max(year),.N,by=.(multimorbidity,run,age20, intervention)
][,.(N = mean(N)),by = .(intervention, age20,multimorbidity)] %>% 
  mutate(multimorbidity=(as.character(multimorbidity))) %>%
  filter(!is.na(multimorbidity)) %>% 
  mutate(age20 = factor(age20,
                        ordered=T,
                        levels=c( '0-20',
                                  '20-40',
                                  '40-60',
                                  '60-80',
                                  '80-100',
                                  '100-120'
                        ))) %>%
  
  group_by(age20) %>% 
  # summarise(multimorbidity = sum(multimorbidity*,na.rm=T)) %>%
  pivot_wider( names_from = intervention,values_from =  N) %>% 
  arrange(multimorbidity) %>%
  mutate(change = `non-intervention`-intervention) %>%
  echarts4r::e_chart(multimorbidity) %>%
  # e_density(N,itemStyle=list(opacity=0.1)) #%>%
  # echarts4r::e_bar(N,label = list(show=F)) %>% 
  echarts4r::e_bar(change,label = list(show=F)) %>% 
  # echarts4r::e_bar(intervention,label = list(show=F)) %>% 
  # echarts4r::e_bar(`non-intervention`, itemStyle = list(decal = list(symbol = 'rect'))) %>% 
  
  e_title(subtext = 'Count of NCD Multimorbidity') %>% 
  e_tooltip() 

past_populations[year==max(year),.N,by=.(multimorbidity,run,age20, intervention)
][,.(N = mean(N)),by = .(intervention, age20,multimorbidity)] %>% 
  mutate(multimorbidity=(as.character(multimorbidity))) %>%
  filter(!is.na(multimorbidity)) %>% 
  mutate(age20 = factor(age20,
                        ordered=T,
                        levels=c( '0-20',
                                  '20-40',
                                  '40-60',
                                  '60-80',
                                  '80-100',
                                  '100-120'
                        ))) %>%
  
  group_by(multimorbidity,intervention) %>% 
  # summarise(multimorbidity = sum(multimorbidity*,na.rm=T)) %>%
  # pivot_wider( names_from = intervention,values_from =  N) %>% 
  # arrange(multimorbidity) %>%
  # mutate(change = `non-intervention`-intervention) %>%
  echarts4r::e_chart(age20) %>%
  # e_density(N,itemStyle=list(opacity=0.1)) #%>%
  # echarts4r::e_bar(N,label = list(show=F)) %>% 
  echarts4r::e_bar(N,label = list(show=F)) %>% 
  # echarts4r::e_bar(intervention,label = list(show=F)) %>% 
  # echarts4r::e_bar(`non-intervention`, itemStyle = list(decal = list(symbol = 'rect'))) %>% 
  
  e_title(subtext = 'Count of NCD Multimorbidity') %>% 
  e_tooltip() 

past_populations[year==max(year),.N,by=.(multimorbidity,run,age20, intervention)
][,.(N = mean(N)),by = .(intervention, age20,multimorbidity)] %>% 
  mutate(multimorbidity=(as.character(multimorbidity))) %>%
  filter(!is.na(multimorbidity)) %>% 
  mutate(age20 = factor(age20,
                        ordered=T,
                        levels=c( '0-20',
                                  '20-40',
                                  '40-60',
                                  '60-80',
                                  '80-100',
                                  '100-120'
                        ))) %>%
  
  # summarise(multimorbidity = sum(multimorbidity*,na.rm=T)) %>%
  pivot_wider( names_from = intervention,values_from =  N) %>%
  arrange(multimorbidity) %>%
  mutate(change = `non-intervention`-intervention) %>%
  group_by(multimorbidity) %>% 
  echarts4r::e_chart(age20) %>%
  # e_density(N,itemStyle=list(opacity=0.1)) #%>%
  # echarts4r::e_bar(N,label = list(show=F)) %>% 
  echarts4r::e_bar(change,label = list(show=F)) %>%
e_title(subtext = 'Improved Multimorbidity Posture') %>% 
  # echarts4r::e_bar(intervention,label = list(show=F)) %>%
  # echarts4r::e_bar(`non-intervention`, itemStyle = list(decal = list(symbol = 'rect'))) %>%
  # e_title(subtext = 'Count of NCD Multimorbidity') %>% 
  e_tooltip() 



full_age <- CJ(run = unique(past_populations$run),
   year = unique(past_populations$year),
   age20 = unique(past_populations$age20),
   multimorbidity = unique(past_populations$multimorbidity)
   )

full_age[,intervention := ifelse(run>max(run)/2,'intervention','non-intervention')]
   
#year==max(year)
x <- past_populations[,.N,by=.(multimorbidity,run,year, age20, intervention)
]#year==max(year)-1,

full_age[x,on = .(run,
                  year,
                  age20,
                  
                  multimorbidity), nomatch=0L
][,.( N = mean(N,na.rm=T)),by = .(intervention,year,  age20,multimorbidity)
][,.(multimorbidity_per_cap = sum(N*multimorbidity,na.rm=T)/sum(N)), by = .(intervention,year, age20)
][,year:=as.character(year)]  %>% 
  # mutate(multimorbidity=(as.character(multimorbidity))) %>%
  # filter(!is.na(multimorbidity)) %>% 
  pivot_wider(id_cols = c(age20,year), names_from = intervention, values_from = multimorbidity_per_cap) %>%
  mutate(age20 = factor(age20,
                        ordered=T,
                        levels=c( '0-20',
                                  '20-40',
                                  '40-60',
                                  '60-80',
                                  '80-100',
                                  '100-120'
                        ))) %>%
  mutate(reduced = `non-intervention`-intervention) %>% 
  # group_by(age20) %>% 
  # mutate(cumulative_reduced = cumsum(reduced)) %>% 
  group_by(age20) %>% 
  echarts4r::e_chart(year) %>%
  # e_density(N,itemStyle=list(opacity=0.1)) #%>%
  echarts4r::e_bar(reduced,label = list(show=F)) %>% 
  e_title(subtext = 'Change in average number of Morbidities') %>% 
  e_tooltip() 

full_age[x,on = .(run,
                  year,
                  age20,
                  
                  multimorbidity), nomatch=0L
         ][,.( N = mean(N,na.rm=T)),by = .(intervention,year,  age20,multimorbidity)
        ][,.(multimorbidity_per_cap = sum(N*multimorbidity,na.rm=T)/sum(N)), by = .(intervention,year, age20)
      ][,year:=as.character(year)]  %>% 
  # mutate(multimorbidity=(as.character(multimorbidity))) %>%
  # filter(!is.na(multimorbidity)) %>% 
  pivot_wider(id_cols = c(age20,year), names_from = intervention, values_from = multimorbidity_per_cap) %>%
  mutate(age20 = factor(age20,
                        ordered=T,
                        levels=c( '0-20',
                                  '20-40',
                                  '40-60',
                                  '60-80',
                                  '80-100',
                                  '100-120'
                        ))) %>%
  mutate(reduced = `non-intervention`-intervention) %>% 
  group_by(age20) %>% 
  mutate(cumulative_reduced = cumsum(reduced)) %>% 
  group_by(age20) %>% 
  echarts4r::e_chart(year) %>%
  # e_density(N,itemStyle=list(opacity=0.1)) #%>%
  echarts4r::e_bar(cumulative_reduced,label = list(show=F)) %>% 
  e_title(subtext = 'Cumulative Change in average number of Morbidities') %>% 
  e_tooltip() 

past_populations[year==max(year),.N,by=.(multimorbidity,run,age20, intervention)
][,.( N = mean(N,na.rm=T)),by = .(intervention, age20,multimorbidity)
][,.(multimorbidity_per_cap = sum(N*multimorbidity,na.rm=T)), by = .(intervention, age20)] %>% 
  # mutate(multimorbidity=(as.character(multimorbidity))) %>%
  # filter(!is.na(multimorbidity)) %>% 
  pivot_wider(id_cols = c(age20,), names_from = intervention, values_from = multimorbidity_per_cap) %>%
  mutate(age20 = factor(age20,
                        ordered=T,
                        levels=c( '0-20',
                                  '20-40',
                                  '40-60',
                                  '60-80',
                                  '80-100',
                                  '100-120'
                        ))) %>%
  mutate(reduced = `non-intervention`-intervention) %>% 
  group_by(age20) %>% 
  echarts4r::e_chart(age20) %>%
  # e_density(N,itemStyle=list(opacity=0.1)) #%>%
  echarts4r::e_bar(reduced,label = list(show=F)) %>% 
  e_title(subtext = 'Change in number of Morbidities') %>% 
  e_tooltip() 

past_populations %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                           ordered=T,
                                           levels=c('Least Deprived',
                                                    'Quintile 4',
                                                    'Quintile 3',
                                                    'Quintile 2',
                                                    'Most Deprived'
                                                     ))) %>%
  group_by(year, run, mdm_quintile_soa_name,intervention ) %>%
  summarise(cmms = mean(cmms,na.rm=T)) %>% 
  group_by(year,mdm_quintile_soa_name, intervention ) %>%
  summarise(cmms = mean(cmms)) %>% 
  mutate(year = as.character(year)) %>%
  group_by(intervention,mdm_quintile_soa_name) %>% 
  # pivot_wider(id_cols = c(year,mdm_quintile_soa_name),names_from = intervention,values_from= cmms, values_fill = 0L) %>%
  e_charts(year) %>%
  e_tooltip(trigger='axis') %>%
  e_title(subtext = 'CMMS - Intervention is solid, baseline is dashed') %>%
  e_line(cmms,lineStyle = list(type = 'dashed')) %>%
  # e_line(intervention, lineStyle = list(type = 'dashed')) %>%
  # e_line(`non-intervention`,lineStyle = list(type = 'dashed')) %>%
  e_y_axis(min = 'dataMin') #%>%
  # e_graphic_g(
  #   type = "text",
  #   left = "35%",
  #   top  = "50%",
  #   style = list(
  #     text = "Not Modelled",
  #     fontSize = 32
  #   )
  # )

past_populations %>%
  filter(year!=min(year)) %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c('Least Deprived',
                                                 'Quintile 4',
                                                 'Quintile 3',
                                                 'Quintile 2',
                                                 'Most Deprived'
                                        ))) %>%
  group_by(year, run, mdm_quintile_soa_name ) %>%
  summarise(cmms = mean(cmms,na.rm=T)) %>% 
  group_by(year,mdm_quintile_soa_name ,run) %>%
  summarise(cmms = mean(cmms)) %>% 
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  mutate(avg =mean(cmms)) %>%
  group_by(year,mdm_quintile_soa_name ,run) %>%
  
  mutate(dev= sum(abs(cmms-avg))) %>% 
  ungroup() %>% 
  # group_by(mdm_quintile_soa_name) %>%
  e_charts(year) %>%
  e_tooltip(trigger='axis') %>%
  e_title(subtext = 'CMMS - Intervention is solid, baseline is dashed') %>%
  e_scatter(dev) %>% 
  # e_boxplot(dev) %>%
  
  e_lm(dev~year)# %>% 
  e_bar(dev,lineStyle = list(type = 'dashed'))


past_populations %>%
  filter(year!=min(year)) %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c( 'Least Deprived',
                                                  'Quintile 4',
                                                  'Quintile 3',
                                                  'Quintile 2',
                                                  'Most Deprived'))) %>%
  group_by(year, run, mdm_quintile_soa_name,intervention ) %>%
  summarise(cmms = mean(cmms,na.rm=T),n()) %>% 
  group_by(year,mdm_quintile_soa_name, intervention ) %>%
  summarise(cmms = mean(cmms),n=mean(n)) %>% 
  # mutate(cmms=cmms/n) %>%
  mutate(year = as.character(year)) %>%
  pivot_wider(id_cols = c(year,mdm_quintile_soa_name),names_from = intervention,values_from= cmms, values_fill = 0L) %>%
  mutate(reduced = intervention-`non-intervention`) %>%
  group_by(mdm_quintile_soa_name) %>% 
  mutate(cumulative_reduced = cumsum(reduced)) %>%
  e_charts(year) %>% 
  # e_line(intervention) %>% 
  # e_line(`non-intervention`,lineStyle = list(type = 'dashed')) %>% 
  e_tooltip(trigger='axis') %>%
  e_bar(reduced) #%>%
  # e_y_axis(min = 'dataMin') #%>%


sick_days_mdm <- sick_days_fn(past_populations,group_vars = 'mdm_quintile_soa_name',year_cut_off = 2024)

sick_days_mdm[,year:=as.character(year)
][, .(n=sum(prevalence)), by = .(mdm_quintile_soa_name,intervention,year)] %>% 
  group_by(intervention) %>%
  arrange(mdm_quintile_soa_name) %>% 
  e_charts(mdm_quintile_soa_name ) %>% 
  e_tooltip(trigger='axis') %>% 
  e_bar(n)

sick_days <- sick_days_fn(past_populations)
sick_days[,year:=as.character(year)
][, .(n=sum(prevalence)), by = .(intervention,year)] %>% 
  group_by(intervention) %>%
  e_charts(year ) %>% 
  e_tooltip(trigger='axis') %>% 
  e_bar(n)

sick_days[,year:=as.character(year)
][, .(n=sum(prevalence)), by = .(intervention,year)] %>% 
  dcast(year ~ intervention, value.var = 'n', fill = 0L) %>%
  mutate(averted = intervention - `non-intervention`) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  # group_by(intervention) %>%
  e_charts(year) %>% 
  e_tooltip(trigger='axis') %>% 
  e_bar(cumulative_averted,name='Averted') %>% 
  e_mark_area(
    name = 'Intervention',
    data = list(
      list(xAxis = '2022', yAxis = NA),
      list(xAxis = '2028', yAxis = NA)
    ),
    itemStyle = list(color = "rgba(144,238,144,0.8)")
  ) %>% 
  e_title('Sick Days') %>%
  e_legend(data = list(name = 'Intervention',
                       icon= 'circle',
                       textStyle= list(
                         color= 'red'
                       )
  )
  ) %>% 
  e_graphic_g(
    type = "text",
    right = "15%",
    top  = "10%",
    style = list(
      fill =  "#B8EFAE",#rgb(184/255,239/255, 174/255),
      text = "Intervention Ends",
      fontSize = 12
    )
  )
# e_theme('walden')

bed_days_age20_2024 <- bed_days_fn(past_populations = past_populations,'HSCT',year_cut_off = 2024)

bed_days_age20 <- bed_days_fn(past_populations = past_populations,'HSCT')

bed_days_age20[,.(admissions  = sum(admissions,na.rm=T),
                  emergency_admissions =sum(emergency_admissions,na.rm=T),
                  bed_days = sum(bed_days,na.rm=T)),
                  by = .(intervention,HSCT,year)
][,year := as.character(year)
] %>% 
  dcast(formula = year+HSCT ~ intervention,value.var = ('bed_days'), fill = 0L) %>%
  mutate(averted = `non-intervention` - intervention) %>%
  group_by(HSCT) %>% 
  mutate(cumulative_averted = cumsum(averted)) %>%
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_line(cumulative_averted)

bed_days_age20[,.(admissions  = sum(admissions,na.rm=T),
                  emergency_admissions =sum(emergency_admissions,na.rm=T),
                  bed_days = sum(bed_days,na.rm=T)),
               by = .(intervention,HSCT,year)
][,year := as.character(year)
] %>% 
  dcast(formula = year+HSCT ~ intervention,value.var = ('emergency_admissions'), fill = 0L) %>%
  mutate(averted = `non-intervention` - intervention) %>%
  group_by(HSCT) %>% 
  mutate(cumulative_averted = cumsum(averted)) %>%
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_line(cumulative_averted)

bed_days <- bed_days_fn(past_populations = past_populations)

bed_days[,.(admissions  = sum(admissions,na.rm=T),
                  emergency_admissions =sum(emergency_admissions,na.rm=T),
                  bed_days = sum(bed_days,na.rm=T)),
               by = .(intervention,year)
][,year := as.character(year)
] %>% 
  dcast(formula = year ~ intervention,value.var = ('emergency_admissions'), fill = 0L) %>%
  mutate(averted = `non-intervention` - intervention) %>%
  # group_by(HSCT) %>% 
  mutate(cumulative_averted = cumsum(averted)) %>%
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_line(cumulative_averted)

costs <- calculate_costs_fn(past_populations, group_vars = 'HSCT', year_cut_off = NULL)

costs[,.(total_cost = sum(total_cost)), by = .(intervention,HSCT,year)
      ][,year := as.character(year)
        ] %>%
  group_by(HSCT,intervention) %>%
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_line(total_cost)

costs[,.(total_cost = sum(total_cost)), by = .(intervention,HSCT,year)
][,year := as.character(year)
] %>%
  pivot_wider(id_cols = c(HSCT,year), names_from = intervention, values_from = total_cost, values_fill = 0L) %>%
  mutate(saved = `non-intervention` - intervention) %>% 
  group_by(HSCT) %>%
  mutate(cumulative_saved =cumsum(saved)) %>% 
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_bar(cumulative_saved)


costs[,.(total_cost = sum(total_cost)), by = .(intervention,HSCT,year)
][,year := as.character(year)
] %>%
  pivot_wider(id_cols = c(HSCT,year), names_from = intervention, values_from = total_cost, values_fill = 0L) %>%
  mutate(saved = `non-intervention` - intervention) %>% 
  group_by(HSCT) %>%
  mutate(cumulative_saved =cumsum(saved)) %>% 
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_bar(cumulative_saved)

costs[,.(total_cost = sum(total_cost)), by = .(intervention,HSCT,year)
][,year := as.character(year)
] %>%
  pivot_wider(id_cols = c(HSCT,year), names_from = intervention, values_from = total_cost, values_fill = 0L) %>%
  mutate(saved = `non-intervention` - intervention) %>% 
  group_by(HSCT) %>%
  mutate(cumulative_saved =cumsum(saved)) %>% 
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_bar(cumulative_saved)

costs[,.(total_cost = sum(total_cost)), by = .(intervention,disease,HSCT,year)
][,year := as.character(year)
] %>%
  pivot_wider(id_cols = c(HSCT,disease,year), names_from = intervention, values_from = total_cost, values_fill = 0L) %>%
  mutate(saved = `non-intervention` - intervention) %>% 
  group_by(HSCT) %>%
  mutate(cumulative_saved =cumsum(saved)) %>% 
  filter(year==max(year)) %>% 
  pivot_wider(id_cols = c(disease), names_from = HSCT, values_from = cumulative_saved, values_fill = 0L) %>%
    reactable::reactable()


#target population
past_populations[, .(diabetes = sum(diabetes!=0,na.rm=T)), by=.(year,run, mdm_quintile_soa_name,intervention)
                 ][, .(diabetes = mean(diabetes,na.rm=T)), by=.(year,  intervention)
                 ][, year := as.character(year)
                   ] %>%
  # pivot_wider(id_cols = c(year), names_from = intervention, values_from = diabetes, values_fill = 0L) %>%
  # mutate(averted = `non-intervention` - intervention) %>% 
  group_by(intervention) %>%
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_line(diabetes)

past_populations[, .(overweight = sum(bmi!='normal',na.rm=T)), by=.(year,run, mdm_quintile_soa_name,intervention)
][, .(overweight = mean(overweight,na.rm=T)), by=.(year,mdm_quintile_soa_name,  intervention)
][, year := as.character(year)
] %>%
  pivot_wider(id_cols = c(year,mdm_quintile_soa_name), names_from = intervention, values_from = overweight, values_fill = 0L) %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c( 'Least Deprived',
                                                  'Quintile 4',
                                                  'Quintile 3',
                                                  'Quintile 2',
                                                  'Most Deprived'))) %>%
  mutate(averted = `non-intervention` - intervention) %>%
  group_by(mdm_quintile_soa_name) %>%
  mutate(lag(averted), roll_mean = (averted  + lag(averted))/2) %>%#View()
  mutate(cumulative_averted = cumsum(averted)) %>%#View()
  # filter(year!=min(year)) %>%
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_bar(cumulative_averted) #%>% 
  # e_bar(roll_mean)# %>%
  # e_line(roll_mean)

past_populations[year==min(year)+1, .(.N), by=.(diabetes_status,run,target, mdm_quintile_soa_name,intervention)
][, .(N=mean(N)), by=.(mdm_quintile_soa_name, target,diabetes_status, intervention)
] %>%
  # pivot_wider(id_cols = c(year,mdm_quintile_soa_name), names_from = intervention, values_from = diabetes, values_fill = 0L) %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c( 'Least Deprived',
                                                  'Quintile 4',
                                                  'Quintile 3',
                                                  'Quintile 2',
                                                  'Most Deprived'))) %>%
  mutate(diabetes_status = factor(diabetes_status,
                                        ordered=T,
                                        levels=c( 'no_diabetes',
                                                  'diagnosed_diabetes',
                                                  'undiagnosed_diabetes'))) %>%
  arrange(mdm_quintile_soa_name,diabetes_status) %>% 
  filter(!is.na(diabetes_status)) %>%
  # mutate(averted = `non-intervention` - intervention) %>%
  group_by(diabetes_status,target) %>%
  # mutate(lag(averted), roll_mean = (averted  + lag(averted))/2) %>%#View()
  # filter(year!=min(year)) %>%
  e_charts(mdm_quintile_soa_name) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_bar(N,stack = 'grp') #%>%


past_populations[, .(overweight = sum(bmi!='normal',na.rm=T)), by=.(year,run, mdm_quintile_soa_name,intervention)
][, .(overweight = mean(overweight,na.rm=T)), by=.(year,mdm_quintile_soa_name,  intervention)
][, year := as.character(year)
] %>%
  pivot_wider(id_cols = c(year,mdm_quintile_soa_name), names_from = intervention, values_from = overweight, values_fill = 0L) %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c( 'Least Deprived',
                                                  'Quintile 4',
                                                  'Quintile 3',
                                                  'Quintile 2',
                                                  'Most Deprived'))) %>%
  mutate(averted = `non-intervention` - intervention) %>%
  group_by(mdm_quintile_soa_name) %>%
  mutate(lag(averted), roll_mean = (averted  + lag(averted))/2) %>%#View()
  filter(year!=min(year)) %>%
  e_charts(year) %>%
  e_tooltip(contain = T, trigger='axis') %>%
  e_bar(roll_mean) #%>%



yld_age <- daly_yld_fn(past_populations ,'age10')
yll_age <- calculate_daly_yll(past_populations,group_vars = 'age10')

yld_age <- yld_age[disease == 'combined_dw',]


yll_age <- yll_age[death_reason != 'death_reason', .(yll=sum(yll)), by= .(intervention,age10,year)]

DALYS_age <- yld_age[yll_age, on = c('intervention','age10','year'),
                     `:=` (yll = i.yll)
]

DALYS_age[is.na(yll), yll := 0]

DALYS_age[, daly := total_dw + yll]

popp_age <- past_populations[,.N,by = c('intervention','age10','year','run')
][, .(N = mean(N)), by = .(intervention,age10,year)
]


yld_age[popp_age,on = c('intervention','age10','year')] %>% 
  # filter(!age10 %in% c('0-15','16-34','35-44','45-54')) %>%
   mutate(total_dw_per_cap=total_dw/N) %>% 
  count(year, age10, d=disease == 'combined_dw', wt = total_dw_per_cap) %>% 
  dcast(year+age10 ~ d, value.var = 'n', fill = 0L) %>%
  group_by(age10) %>%
  rename( 'combined_dw'=`TRUE`,'other_diseases' = `FALSE`) %>%
  e_charts(`other_diseases`) %>%
  e_scatter(`combined_dw`) %>% 
  e_line(`combined_dw`) %>% 
  e_lm(name=c('0-15','16-34','35-44','45-54','55-64','65-74','75-110'),`combined_dw`~`other_diseases`) %>% #name=c('0-15','16-34','35-44','45-54','55-64','65-74','75-110')
  e_data(data.frame(combined_dw=1:65,other_diseases=1:65)) %>% 
  e_line(other_diseases,combined_dw,symbol = 'none',name='y=x')
  

yld_age[popp_age,on = c('intervention','age10','year')] %>% 
  # filter(!age10 %in% c('0-15','16-34','35-44','45-54')) %>%
  mutate(total_dw_per_cap=total_dw) %>% 
  # filter(intervention != 'intervention') %>%
  group_by(year, age10, d=disease == 'combined_dw') %>% 
  summarise(n=sum( total_dw_per_cap,na.rm=T)) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  dcast(year+age10 ~ d, value.var = 'n', fill = 0L) %>%
  group_by(age10) %>%
  rename( 'combined_dw'=`TRUE`,'other_diseases' = `FALSE`) %>%
  e_charts(`other_diseases`) %>%
  e_scatter(`combined_dw`) %>% 
  e_line(`combined_dw`) %>% 
  e_lm(name=c('0-15','16-34','35-44','45-54','55-64','65-74','75-110'),`combined_dw`~`other_diseases`) %>% #name=c('0-15','16-34','35-44','45-54','55-64','65-74','75-110')
  e_data(data.frame(combined_dw=1:65000,other_diseases=1:65000)) %>%
  e_line(other_diseases,combined_dw,symbol = 'none',name='y=x')


yld_age[popp_age,on = c('intervention','age10','year')] %>% 
  # filter(!age10 %in% c('0-15','16-34','35-44','45-54')) %>%
  mutate(total_dw_per_cap=total_dw) %>% 
  # filter(intervention != 'intervention') %>%
  group_by(year, age10, d=disease == 'combined_dw') %>% 
  summarise(n=sum( total_dw_per_cap,na.rm=T)) %>% 
  ungroup() %>% 
  as.data.table() %>% 
  dcast(year+age10 ~ d, value.var = 'n', fill = 0L) %>%
  group_by(age10) %>%
  rename( 'combined_dw'=`TRUE`,'other_diseases' = `FALSE`) %>%
  mutate(change = other_diseases-combined_dw) %>% 
  e_charts(`other_diseases`) %>%
  # e_bar(change) %>% 
  e_scatter(`change`) %>% 
  # e_line(`combined_dw`) %>% 
  # e_lm(`other_diseases`~`change`)
  e_lm(`change`~`other_diseases`,name=c('0-15','16-34','35-44','45-54','55-64','65-74','75-110'))
  # e_data(data.frame(combined_dw=1:65000,other_diseases=1:65000)) %>%
  # e_line(other_diseases,combined_dw,symbol = 'none',name='y=x')


DALYS_age[popp_age,on = c('intervention','age10','year')] %>% 
  mutate(daly=daly/N) %>%
  dcast(formula = age10 + year ~ intervention, value.var = 'daly', fill = 0L) %>% 
  # mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
  #                                       ordered=T,
  #                                       levels=c( 'Least Deprived',
  #                                                 'Quintile 4',
  #                                                 'Quintile 3',
  #                                                 'Quintile 2',
  #                                                 'Most Deprived'))) %>%
  arrange(year, age10) %>%
  mutate(averted = `non-intervention` - intervention) %>% 
  filter(year!=min(year)) %>%
  group_by(age10) %>%
  mutate(cumulative_averted = intervention-first(intervention)) %>%
  mutate(year = as.character(year)) %>% 
  e_charts(year) %>%
  e_bar(cumulative_averted) %>%
  e_line(intervention) %>%
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>%
  # e_line(averted, lineStyle = list(type = 'dashed')) %>%
  e_tooltip(trigger='axis')


DALYS_age[popp_age,on = c('intervention','age10','year')] %>% 
  mutate(daly=daly/N) %>%
  dcast(formula = age10 + year ~ intervention, value.var = 'daly', fill = 0L) %>% 
  # mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
  #                                       ordered=T,
  #                                       levels=c( 'Least Deprived',
  #                                                 'Quintile 4',
  #                                                 'Quintile 3',
  #                                                 'Quintile 2',
  #                                                 'Most Deprived'))) %>%
  arrange(year, age10) %>%
  mutate(averted = `non-intervention` - intervention) %>% 
  filter(year!=min(year)) %>%
  group_by(age10) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  mutate(year = as.character(year)) %>% 
  e_charts(year) %>%
  e_bar(cumulative_averted) %>%
  e_line(intervention) %>%
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>%
  # e_line(averted, lineStyle = list(type = 'dashed')) %>%
  e_tooltip(trigger='axis')

DALYS_age %>% 
  dcast(formula = age10 + year ~ intervention, value.var = 'daly', fill = 0L) %>% 
  # mutate(age10 = factor(age10,
  #                                       ordered=T,
  #                                       levels=c( 'Least Deprived',
  #                                                 'Quintile 4',
  #                                                 'Quintile 3',
  #                                                 'Quintile 2',
  #                                                 'Most Deprived'))) %>%
  arrange(year, age10) %>%
  mutate(averted = `non-intervention` - intervention) %>% 
  group_by(age10) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  mutate(year = as.character(year)) %>% 
  e_charts(year) %>%
  e_bar(cumulative_averted) %>%
  e_line(intervention) %>%
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>%
  # e_line(averted, lineStyle = list(type = 'dashed')) %>%
  e_tooltip()


yld_mdm <- daly_yld_fn(past_populations ,'mdm_quintile_soa_name')
yll_mdm <- calculate_daly_yll(past_populations,group_vars = 'mdm_quintile_soa_name')

yld_mdm <- yld_mdm[disease == 'combined_dw',]


yll_mdm <- yll_mdm[death_reason != 'death_reason', .(yll=sum(yll)), by= .(intervention,mdm_quintile_soa_name,year)]

DALYS_mdm <- yld_mdm[yll_mdm, on = c('intervention','mdm_quintile_soa_name','year'),
            `:=` (yll = i.yll)
            ]

DALYS_mdm[is.na(yll), yll := 0]

DALYS_mdm[, daly := total_dw + yll]

popp_mdm <- past_populations[,.N,by = c('intervention','mdm_quintile_soa_name','year','run')
                ][, .(N = mean(N)), by = .(intervention,mdm_quintile_soa_name,year)
                ]

DALYS_mdm[popp_mdm,on = c('intervention','mdm_quintile_soa_name','year')] %>% 
  mutate(daly=daly/N) %>%
  dcast(formula = mdm_quintile_soa_name + year ~ intervention, value.var = 'daly', fill = 0L) %>% 
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c( 'Least Deprived',
                                                  'Quintile 4',
                                                  'Quintile 3',
                                                  'Quintile 2',
                                                  'Most Deprived'))) %>%
  arrange(year, mdm_quintile_soa_name) %>%
  mutate(averted = `non-intervention` - intervention) %>% 
  filter(year!=min(year)) %>%
  group_by(mdm_quintile_soa_name) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  mutate(year = as.character(year)) %>% 
  e_charts(year) %>%
  e_bar(cumulative_averted) %>%
  e_line(intervention) %>%
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>%
  # e_line(averted, lineStyle = list(type = 'dashed')) %>%
  e_tooltip(trigger='axis')

DALYS_mdm %>% 
  dcast(formula = mdm_quintile_soa_name + year ~ intervention, value.var = 'daly', fill = 0L) %>% 
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c( 'Least Deprived',
                                                  'Quintile 4',
                                                  'Quintile 3',
                                                  'Quintile 2',
                                                  'Most Deprived'))) %>%
  arrange(year, mdm_quintile_soa_name) %>%
  mutate(averted = `non-intervention` - intervention) %>% 
  group_by(mdm_quintile_soa_name) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  mutate(year = as.character(year)) %>% 
  e_charts(year) %>%
  e_bar(cumulative_averted) %>%
  e_line(intervention) %>%
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>%
  # e_line(averted, lineStyle = list(type = 'dashed')) %>%
  e_tooltip()

# DALYS_mdm %>% 
DALYS_mdm[popp_mdm,on = c('intervention','mdm_quintile_soa_name','year')]%>%
  mutate(daly=daly/N) %>%
  # filter(year!=min(year)) %>%
  dcast(formula = mdm_quintile_soa_name + year ~ intervention, value.var = 'daly', fill = 0L) %>% 
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c( 'Least Deprived',
                                                  'Quintile 4',
                                                  'Quintile 3',
                                                  'Quintile 2',
                                                  'Most Deprived'))) %>%
  arrange(year, mdm_quintile_soa_name) %>%
  group_by(mdm_quintile_soa_name) %>%
  mutate(ni_inc = `non-intervention` - min(`non-intervention`)) %>% 
  mutate(i_inc = intervention - min(intervention)) %>% 
  
  mutate(year = as.character(year)) %>% 
  e_charts(year) %>%
  e_line(ni_inc) %>%
  e_line(i_inc, lineStyle = list(type = 'dashed')) %>%
  # e_line(averted, lineStyle = list(type = 'dashed')) %>%
  e_tooltip()


yld_age_mdm <- daly_yld_fn(past_populations,group_vars = c('age10','mdm_quintile_soa_name'))
popp <- past_populations[,.N,by = c('intervention','year','age10','mdm_quintile_soa_name','run')
][, .(N = mean(N)), by = .(intervention,year,age10,mdm_quintile_soa_name)
]

yld_age_mdm <- yld_age_mdm[disease=='combined_dw']

yld_age_mdm[popp,on = c('intervention','year','age10','mdm_quintile_soa_name')
][,year := as.character(year)] %>% 
  mutate(total_dw=total_dw/N) %>%
  # filter(year==max(year)) %>% 
  # dcast(formula = year +age10 +mdm_quintile_soa_name ~ intervention, value.var = 'total_dw', fill = 0L) %>%
  group_by(year, mdm_quintile_soa_name,age10 ) %>%
  summarise(total_dw = mean(total_dw,na.rm=T)) %>% 
  group_by(year,mdm_quintile_soa_name,age10 ) %>%
  summarise(total_dw = mean(total_dw)) %>% 
  mutate(year = as.character(year)) %>%
  group_by(year) %>%
  mutate(avg =mean(total_dw)) %>%
  group_by(year,age10) %>%
  mutate(dev= sum(abs(total_dw-avg))) %>% 
  ungroup() %>% 
  group_by(age10) %>%
  e_charts(year) %>%
  e_tooltip(trigger='axis') %>%
  e_title(subtext = 'CMMS - Intervention is solid, baseline is dashed') %>%
  e_line(dev) #%>% 
  # e_boxplot(dev) %>%
  
  # e_lm(dev~year)# %>%
# e_bar(dev,lineStyle = list(type = 'dashed'))


yld <- daly_yld_fn(past_populations)
yll <- calculate_daly_yll(past_populations)

yld <- yld[disease == 'combined_dw',]
yll <- yll[death_reason != 'death_reason', .(yll=sum(yll)), by= .(intervention,year)]

DALYS <- yld[yll, on = c('intervention','year'),
                     `:=` (yll = i.yll)]

DALYS[, daly := total_dw + yll]

DALYS_mdm[is.na(yll), yll := 0]

popp <- past_populations[,.N,by = c('intervention','year','run')
][, .(N = mean(N)), by = .(intervention,year)
]

DALYS[popp,on = c('intervention','year')
          ][,year := as.character(year)] %>% 
  mutate(daly=daly/N) %>%
  group_by(intervention) %>%
  e_charts(year) %>%
  e_line(daly,) %>% 
  e_title(subtext = 'DALYs per Capita') 

DALYS[popp,on = c('intervention','year')
][,year := as.character(year)] %>% 
  mutate(daly=daly/N) %>%
  dcast(formula = year ~ intervention, value.var = 'daly', fill = 0L) %>%
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  # group_by(intervention) %>%
  e_charts(year) %>%
  e_line(cumulative_averted) %>% 
  e_title(subtext = 'Cumulative Averted DALYs per Capita') 

popp_ethnicity <- past_populations[,.N,by = c('intervention','broad_ethnicity','year','run')
][, .(N = mean(N)), by = .(intervention,broad_ethnicity,year)
]

args <- list( past_populations = past_populations, group_vars = c("broad_ethnicity"), year_cut_off = NULL)
qalys_ethnicity <- do.call(qaly_yld_fn, args)
qalys_ethnicity[disease == "combined_uw", .(total = sum(total_uw))]

popp_ethnicity[qalys_ethnicity,on=c('intervention', 'broad_ethnicity', 'year' )
               ][disease == 'combined_uw', ][,total_uw := total_uw / N ] %>%
  dcast(formula = year +broad_ethnicity ~intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>% 
  mutate(averted = `non-intervention` - intervention) %>%
  group_by(broad_ethnicity) %>% 
  mutate(cumulative_averted = cumsum(averted)) %>%
  e_charts(year) %>%
  e_title(subtext= 'Incremental QALYs') %>% 
  e_bar(cumulative_averted)


popp_ethnicity[qalys_ethnicity,on=c('intervention', 'broad_ethnicity', 'year' )
][disease == 'combined_uw', ][,total_uw := total_uw / N ] %>%
  dcast(formula = year +broad_ethnicity ~intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>% 
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  group_by(broad_ethnicity) %>% 
  e_charts(year) %>%
  e_line(intervention) %>% 
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>% 
  e_title(subtext = 'QALYs')

args <- list( past_populations = past_populations, year_cut_off = NULL)
qalys <- do.call(qaly_yld_fn, args)
qalys[disease == "combined_uw", .(total = sum(total_uw))]

qalys[disease == 'combined_uw', ] %>%
  dcast(formula = year~intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>% 
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  e_charts(year) %>%
  e_theme('walden') %>%
  e_line(intervention,
         sampling = 'lttb',
         areaStyle = list(
           color = htmlwidgets::JS("
    new echarts.graphic.LinearGradient(1, 0, 0, 0, [
      {offset: 0, color: 'lightblue'},
      {offset: 1, color: 'white'}
    ])
  ")
         )) %>%
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>% 
  e_y_axis(min = 'dataMin') %>%
  # e_line(averted) %>% 
  # e_bar(cumulative_averted) %>% 
  e_title(subtext = 'QALYs') 
  
data.frame(x=1:10,y=1:10) %>% 
  e_charts(x) %>% 
  e_line(y,
         symbol = 'none',
         smooth = T,
         name = 'Line with Gradient Area',
areaStyle = list(
  color = htmlwidgets::JS("
    new echarts.graphic.LinearGradient(0, 0, 0, 1, [
      {offset: 0, color: 'blue'},
      {offset: 1, color: 'white'}
    ])
  ")
  ))

args <- list( past_populations = past_populations,group_vars = c('target','age10'), year_cut_off = NULL)
qalys_age <- do.call(qaly_yld_fn, args)
qalys_age[disease == "combined_uw", .(total = sum(total_uw)), by=.(age10)]

qalys[disease == 'combined_uw', ] %>%
  dcast(formula = year + age10 + target~intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>% 
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  group_by(age10) %>% 
  e_charts(year) %>%
  e_theme('walden') %>%
  e_line(intervention,
         sampling = 'lttb',
         areaStyle = list(
           color = htmlwidgets::JS("
    new echarts.graphic.LinearGradient(1, 0, 0, 0, [
      {offset: 0, color: 'lightblue'},
      {offset: 1, color: 'white'}
    ])
  ")
         )) %>%
  e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>% 
  e_y_axis(min = 'dataMin') %>%
  # e_line(averted) %>% 
  # e_bar(cumulative_averted) %>% 
  e_title(subtext = 'QALYs') 

data.frame(x=1:10,y=1:10) %>% 
  e_charts(x) %>% 
  e_line(y,
         symbol = 'none',
         smooth = T,
         name = 'Line with Gradient Area',
         areaStyle = list(
           color = htmlwidgets::JS("
    new echarts.graphic.LinearGradient(0, 0, 0, 1, [
      {offset: 0, color: 'blue'},
      {offset: 1, color: 'white'}
    ])
  ")
         ))


x <- qalys[disease == 'combined_uw', ] %>%
  dcast(formula = year  ~intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>% 
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted)) %>%
  e_charts(year) %>%
  # e_line(intervention) %>% 
  # e_line(`non-intervention`, lineStyle = list(type = 'dashed')) %>% 
  # e_y_axis(min = 'dataMin') %>%
  e_line(averted,yindex =1,symbol = 'none', smooth = T, name = 'Averted') %>%
  e_bar(cumulative_averted, name = 'Cumulative Averted') %>%
  e_title(subtext = 'QALYs') %>% 
  e_theme('shine') %>%
  e_text_style(fontFamily = 'Avenir',fontSize = 15) %>% 
  e_tooltip() %>% 
  e_visual_map(
    inRange = list(
      # color = c('#ffffff', 'rgba(3,4,5,0.1)', 'blue')#,
      # color=c(),
      opacity = c(0, 1)
    ),
    show= F,
    type= 'continuous',
    seriesIndex= 1,
    dimension= 0,
    min= 0,
    max= 10
  )%>% 
  e_visual_map(
    inRange = list(
      # color = c('#ffffff', 'rgba(3,4,5,0.1)', 'blue')#,
      # color=c(),
      opacity = c(0, 1)
    ),
    show= F,
    type= 'continuous',
    seriesIndex= 0,
    dimension= 1,
    min= -40,
    max= 1000
  );x
    # (
    #   show= false,
    #   type= 'continuous',
    #   seriesIndex= 1,
    #   dimension= 0,
    #   min= 0,
    #   max= 9
    # )

#EPI
#STROKE 

# x <- past_populations[ year!=min(year),.(stroke=sum(stroke==year,na.rm=T)), by=.(year,run,intervention)
# ][, .(stroke=mean(stroke), stroke_sd=sd(stroke),N=.N), by=.(year,intervention)
# ][, year := as.character(year)
# ] 
# 
# x%>% 
#   mutate(N=model_specification$model$number_of_runs/2)
# 
# x <- x[,`:=`(uc =  stroke +1.96 * stroke_sd/sqrt(N),lc = stroke -  1.96 * stroke_sd/sqrt(N))] #%>%
#   # mutate(averted=`non-intervention` - intervention) %>%
#   # mutate(averted=cumsum(averted)) %>%
#   # group_by(intervention) %>%
# 
# 
# x %>% 
#   filter(intervention=='intervention') %>% 
#   e_charts(year,backgroundColor='white', borderRadius =50) %>%
#   e_line(stroke) %>% 
#   # e_band(lc, uc) %>% 
#   e_tooltip() %>% 
# 
#   e_line(name = 'Upper',
#          symbol= 'none',
#          serie = uc,
#          #stack = 'confidence-band',
#          legend = F,
#          
#          color = "#ff0000",
#          areaStyle =
#            list(color = "#ff0000AA", opacity = 0.4
#            )
#   ) |> 
#   # 95% interval ribbon (between lwr_pct and upr_pct)
#   e_line(name = 'Lower',
#          symbol= 'none',
#          serie = lc,
#          #stack = 'confidence-band',
#          #max = Upper,
#          legend = F,
#          color = "#ff0000",
#          itemStyle = list(
#            list(color = "green", opacity = 1)
#          ),
#          areaStyle =
#            list(color = "#ffffffAA", opacity = 1)
#   ) %>% 
#   e_data(x %>% 
#            filter(intervention=='intervention') )
# 



past_populations[ year!=min(year),.(stroke=sum(stroke==year,na.rm=T)), by=.(year,run,intervention)
][, .(stroke=mean(stroke)), by=.(year,intervention)
][, year := as.character(year)
] %>%
  group_by(intervention) %>%
  e_charts(year) %>%
  e_line(stroke) %>% 
  e_tooltip()



past_populations[ year!=min(year),.(stroke=sum(stroke==year,na.rm=T)), by=.(year,run,intervention)
                  ][, .(stroke=mean(stroke)), by=.(year,intervention)
                  ][, year := as.character(year)
                    ] %>%
  dcast(formula =   year ~ intervention, value.var = 'stroke', fill = 0L) %>%
  mutate(averted=`non-intervention` - intervention) %>%
  mutate(averted=cumsum(averted)) %>%
  e_charts(year) %>%
  e_line(averted) 
  
  past_populations[ year!=min(year),.(stroke=sum(stroke==year,na.rm=T)), by=.(year,run,intervention)
  ][, .(stroke=mean(stroke)), by=.(year,intervention)
  ][, year := as.character(year)
  ] %>%
    group_by(intervention) %>%
    e_charts(year) %>%
    e_line(stroke) %>% 
    e_tooltip()
  
  past_populations[ death !=0 & death_reason != 'age_sex_std',
                    .N, 
                   by=.(run,year, intervention) 
                   ][, .(N = mean(N)) ,
                         by=.(year, intervention)  
                     ][,year := as.character(year)] %>% 
    group_by(intervention) %>%
    e_charts(year) %>% 
    e_line(N)

#demographics
  
  past_populations[, .(age = mean(age)), 
    by=.(year,run,intervention)
  ][, .(age = mean(age)), 
    by=.(year,intervention)] %>% 
  mutate(year = as.character(year)) %>% 
           group_by(intervention) %>% 
    e_charts(year) %>%
    e_line(age) %>% 
    e_y_axis(min='dataMin') %>%
    e_tooltip()

past_populations[age==0,.N,by=.(year,run)
                 ][, .(births = mean(N)*model_specification$population$scale_down_factor), 
                   by=.(year)
                 ]

past_populations[16<age&age<45,.N,by=.(year,run)
][, .(childbearing = mean(N)*model_specification$population$scale_down_factor), 
  by=.(year)
]

past_populations[,.N,by=.(year,run)
][, .(births = mean(N)*model_specification$population$scale_down_factor), 
  by=.(year)
]

past_populations[death!=0 & year!=min(year),.N,by=.(year,run)
][, .(deaths = mean(N)*model_specification$population$scale_down_factor), 
  by=.(year)
]

past_populations[death!=0 & year!=min(year),.(age=mean(age)),by=.(year,run)
][, .(age = mean(age)), 
  by=.(year)
] %>% 
  cbind(DALYS[year!=min(year),.(daly = mean(daly)),year][,'daly']
) %>% 
  mutate(daly=(daly-first(daly))/first(daly),
         age=(age-first(age))/first(age)
         ) %>% 
  # mutate(age = as.character(age)) %>%
  e_charts(age) %>%
  e_scatter(daly) %>% 
  e_line(daly) %>%
  e_lm(daly~age) %>% 
  e_tooltip()

past_populations[death==0 & year!=min(year) & age>50,.N,by=.(year,run,age10)
][, .(N = mean(N)*model_specification$population$scale_down_factor), 
  by=.(year,age10)
] %>% 
  mutate(year = as.character(year)) %>%
  group_by(age10) %>%
  e_charts(year) %>%
  e_line(N) %>% 
  e_tooltip()

