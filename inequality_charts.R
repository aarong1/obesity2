# x <- pop %>%
#   count(age20,mdm_quintile_soa = as.numeric(mdm_quintile_soa),bmi) %>%
#   mutate(n=n * model_specification$population$scale_down_factor) %>%
#   filter(!is.na(age20)) %>%
#   filter(bmi=='obese') %>%
#   group_by(age20) %>%
#   e_charts(mdm_quintile_soa) %>%
#   e_line(n) %>%
#   e_lm(n~mdm_quintile_soa) %>%
#   e_theme('infographic')

pop %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c('Least Deprived',
                                                 'Quintile 4',
                                                 'Quintile 3',
                                                 'Quintile 2',
                                                 'Most Deprived'
                                        ))) %>% 
  count(mdm_quintile_soa_name,bmi) %>% 
  add_count(mdm_quintile_soa_name,wt = n,name='tot') %>% 
  
  filter(bmi %in% c('overweight', 'obese')) %>%
  # filter(bmi=='overweight') %>%
  # filter(bmi=='obese') %>%
  filter(mdm_quintile_soa_name %in% c('Least Deprived',
                                      'Most Deprived')) %>% 
  
  mutate(per=n/tot) #%>% 
  # pull(per) %>% {pluck(.,1) - pluck(.,2)}
# 
# mdm_quintile_soa_name        bmi     n   tot       per
# 1        Least Deprived overweight 11466 33961 0.3376226
# 2        Least Deprived      obese  7256 33961 0.2136568
# 3         Most Deprived overweight 11418 35956 0.3175548
# 4         Most Deprived      obese  9422 35956 0.2620425


0.3376226 - 0.3175548
0.2620425 - 0.2136568 

metric_card(top = '2.0%', 'Prevalence Most and Least Deprived Quintile','Inequality in Overweight', color='lightcoral') 
metric_card(top = '4.8%', '','Inequality in Obesity', color='lightcoral')

# %>%page_fluid() %>% browsable()

(
obese_inequality_chart <- pop %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c('Least Deprived',
                                                 'Quintile 4',
                                                 'Quintile 3',
                                                 'Quintile 2',
                                                 'Most Deprived'
                                        ))) %>% 
  count(age20,mdm_quintile_soa_name,bmi) %>% 
  add_count(age20,mdm_quintile_soa_name,wt = n,name='tot') %>% 
  
  filter(!is.na(age20)) %>% 
  # filter(bmi %in% c('overweight', 'obese')) %>% 
  # filter(bmi=='overweight') %>%
  filter(bmi=='obese') %>%
  filter(mdm_quintile_soa_name %in% c('Least Deprived',
                                      'Most Deprived')) %>% 
  
  group_by(age20) %>% 
  mutate(per=n/tot) %>% 
  e_charts(mdm_quintile_soa_name,
           emphaisis = list(focus = 'self')) %>% 
    e_tooltip(backgroundColor = 'white',
              formatter = e_tooltip_item_formatter('percent')) %>%
  e_line(per, symbol='none') %>% 
  e_scatter(per, symbol='circle', symbol_size = 15) %>%
  e_scatter(per,symbol='square') %>% 
  e_theme('azul')%>% 
  e_axis( axis = 'y', formatter = e_axis_formatter('percent')) %>% 
    e_axis( axis = 'x', name = 'end quintile')  
    
)

(
overweight_inequality_chart <- pop %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c('Least Deprived',
                                                 'Quintile 4',
                                                 'Quintile 3',
                                                 'Quintile 2',
                                                 'Most Deprived'
                                        ))) %>% 
  count(age20,mdm_quintile_soa_name,bmi) %>% 
  add_count(age20,mdm_quintile_soa_name,wt = n,name='tot') %>% 
  
  filter(!is.na(age20)) %>% 
  # filter(bmi %in% c('overweight', 'obese')) %>% 
  filter(bmi=='overweight') %>%
  # filter(bmi=='obese') %>%
  filter(mdm_quintile_soa_name %in% c('Least Deprived',
                                      'Most Deprived')) %>% 
  
  group_by(age20) %>% 
  mutate(per=n/tot) %>% 
  e_charts(mdm_quintile_soa_name,
             emphaisis = list(focus = 'self')) %>% 
    
    e_line(per, symbol='none') %>% 
  e_scatter(per, symbol='circle', symbol_size = 15) %>%
  e_theme('azul')%>% 
  e_axis( axis = 'y', formatter = e_axis_formatter('percent')) %>% 
    e_axis( axis = 'x', name = 'end quintile')  %>% 
    e_tooltip(backgroundColor = 'white',
              formatter = e_tooltip_item_formatter('percent'))
)


(
inequality_chart <-pop %>%
  mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                        ordered=T,
                                        levels=c('Least Deprived',
                                                 'Quintile 4',
                                                 'Quintile 3',
                                                 'Quintile 2',
                                                 'Most Deprived'
                                        ))) %>% 
  count(age20,mdm_quintile_soa_name,bmi) %>% 
  add_count(age20,mdm_quintile_soa_name,wt = n,name='tot') %>% 
  
  filter(!is.na(age20)) %>% 
  # filter(bmi=='overweight') %>%
  # filter(bmi=='obese') %>% 
  filter(mdm_quintile_soa_name %in% c('Least Deprived',
                                      'Most Deprived')) %>% 
  group_by(age20,bmi) %>% 
  mutate(per=n/tot) %>% 
  filter(!is.na(bmi)) %>% 
  mutate(diff = lag(per)-per) %>% 
  filter(!is.na(diff)) %>%
  ungroup() %>% 
  group_by(bmi) %>% 
  e_charts(age20,emphasis = list(focus = 'self')) %>% 
  e_line(diff,symbol='none') %>% 
  e_scatter(diff,symbol='square') %>% 
  e_theme('azul')  %>% 
  e_scatter(diff, symbol='circle', symbol_size = 15) %>%
  e_axis( axis = 'y', formatter = e_axis_formatter('percent'))  %>% 
    e_axis( axis = 'x', name = 'age')  %>% 
    
  e_tooltip(backgroundColor = 'white',
            formatter = e_tooltip_item_formatter('percent', digits = 1))

)




save(list = c(
  'obese_inequality_chart',
  'overweight_inequality_chart',
  'inequality_chart'
),
file = './preprocess/inequality_charts.RData')

