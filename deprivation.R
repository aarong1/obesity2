library(fst)


# deprivation_risk_by_age20_chart

(
  deprivation_bmi_age_chart <- pop |> 

    group_by(age20,soa_code) |>  
    # group_by(age20) %>% 
    #group_by(DEA2014_code,age20) |>

    # group_by(dz_id,age20) |>
    # 
    # group_by(DEA2014_name,age20) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            # dep = mean(custom_townsend_score_dz)
            dep  = mean(mdm_rank)
  ) |> 
  filter(tot > 5) |> 
  mutate(prev = big/tot) |> 
    sample_n(size = 292) |>
    
    # filter(big>3) %>% 
  ungroup() |> 
  group_by(age20) |> 
  e_charts(prev, emphasis = list(focus = 'series') ) |> #,height = '100%', width = '100%'
  e_scatter(dep ) |> 
    e_legend( selector = TRUE) %>% 
  e_lm( dep ~ prev,
       name = c('0-20',
                '20-40',
                '40-60',
                '60-80',
                 '80-100'#,
                #'100-120'
                )) |>
  e_grid(  containLabel = T ) %>% 
  e_tooltip(confine = F) %>%
    e_x_axis(name = 'Prevalence ',formatter = e_axis_formatter('percent')) %>%
    # e_legend() %>% 
  e_theme('default')
  )


(
   DEA_obesity_prevalence <- reduced_pop |> 
    # group_by(sdz_code,age20) |> 
    group_by(DEA2014_name) |> 
    summarise(big = sum(bmi %in% c('overweight','obese')),
              tot = n(),
              dep=mean(custom_townsend_score_dz),
              HSCT = first(HSCT)
              #dep  = mean(mdm_rank)
    ) |> 
    filter(tot > 10) |>
    mutate(prev = big/tot) |> 
    arrange((prev)) %>% 
    ungroup() |> 
    group_by(HSCT) %>% 
    e_charts(DEA2014_name, emphasis = list(focus = 'series') ) |> #,height = '100%', width = '100%'
    e_mark_line( data = list(type = "average")) |> 
    e_bar(prev, barGapCategory = 4, barGap='-100%', barWidth = 4 ,
          itemStyle = list(
            
            decal = list(symbol = "rect",
                         rotation=-0.4,
                         color = "white",
                         dashArrayX = c(1,0), dashArrayY = c(2, 5))
          )) |> 
    e_flip_coords() %>% 
    e_legend( selector = TRUE)  %>% 
    
    e_tooltip(backgroundColor = 'white',
              valueFormatter = JS(" (value) =>  Math.round(value*100) + '%'")
              
    #           formatter = e_tooltip_item_formatter('percent')
    )%>% 
    e_axis( axis = 'x', formatter = e_axis_formatter('percent')) %>% 
    # e_theme('roma')
    e_theme('azul')
)

#top deprived areas
top_town_dep_table <- pop |> 
  mutate(SETTLEMENT2015_name = str_to_title(SETTLEMENT2015_name)) %>% 
  filter(!is.na(SETTLEMENT2015_name)) |> 
  group_by(SETTLEMENT2015_name) |> 
  
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            dep=mean(mdm_rank),
            DEA2014_name = first(DEA2014_name),
            HSCT = first(HSCT),
            SETTLEMENT2015_name = first(SETTLEMENT2015_name),
            NRA_name = first(NRA_name)
  ) |> 
  filter(tot>4) |> 
  mutate(prev = big/tot) |> 
  arrange((dep)) |> 
  group_by(HSCT) %>%
  slice_head(n=3)
# head(20)
(
top_town_per_hsct_plot <- top_town_dep_table %>% 
  # head(4) %>%
  ungroup() %>%
  arrange(desc(prev)) %>%
  arrange(HSCT) %>% 
  group_by(HSCT) %>%
  e_charts(SETTLEMENT2015_name, emphasis = list(focus = 'series')) %>% 
  
  e_scatter(prev,symbol_size = 20,
            itemStyle = list(opacity = 1)) %>%
  e_labels(position = 'right',formatter = JS("function(value, index) {
console.log(value.value[0]);
  return (Math.round(value.value[0]*100) + '%');

}")) %>% 
  e_bar(prev,
        barGap= -1,
        barMinWidth=4,
        barCategoryGap = 100
  ) %>%
  e_tooltip(backgroundColor = 'white',
            formatter = e_tooltip_item_formatter('percent')) %>% 
  # e_color(
  #   c("red", "blue")) %>% 
  # e_color_range(prev, color = c('#f7fbff','#08306b')) %>%
  e_flip_coords() %>%
  e_theme('roma') %>% 
  e_axis( axis = 'x', formatter = e_axis_formatter('percent'))  
)
# e_bar(prev, name = 'At Risk BMI') #%>% 


top_dea_overweight_prev <- pop |> 
  group_by(DEA2014_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            overweight = sum(bmi == 'overweight',na.rm=T),
            obese = sum(bmi == 'obese',na.rm = T),
            tot = n(),
            dep=mean(custom_townsend_rank)
            #dep  = mean(mdm_rank)
  ) |> 
  filter(tot>10) |> 
  mutate(big_prev = big/tot,
         obese_prev = obese/tot,
         overweight_prev = overweight/tot) |> 
  arrange(desc(big_prev))


top_town_overweight_prev <- pop |> 
  group_by(SETTLEMENT2015_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            overweight = sum(bmi == 'overweight',na.rm=T),
            obese = sum(bmi == 'obese',na.rm = T),
            tot = n(),
            HSCT = first(HSCT),
            dep = mean(custom_townsend_rank),
            town  = mean(custom_townsend_score_dz)
  ) |> 
  filter(tot>30) |> 
  mutate(big_prev = big/tot,
         obese_prev = obese/tot,
         overweight_prev = overweight/tot) |> 
  arrange(desc(big_prev)) |> 
  head(20)

# x=top_town_overweight_prev

pop |> 
  group_by( Urban_mixed_rural_status) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            overweight = sum(bmi == 'overweight',na.rm=T),
            obese = sum(bmi == 'obese',na.rm = T),
            tot = n(),
            dep=mean(custom_townsend_rank)
            #dep  = mean(mdm_rank)
  ) |> 
  #filter(tot>10) |> 
  mutate(big_prev = big/tot,
         obese_prev = obese/tot,
         overweight_prev = overweight/tot) |> 
  arrange(desc(big_prev)) #|> # View()
  #filter(SETTLEMENT2015_name=='BELFAST CITY')


pop |> 
  group_by(soa_code,soa_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            dep=mean(custom_townsend_score_dz),
            first(DEA2014_name),
            first(SETTLEMENT2015_name),
            first(NRA_name)
            
  ) |> 
  filter(tot>4) |> 
  mutate(prev = big/tot) |> 
  arrange(desc(dep)) |> 
  head(20)


  

top_soa_dep_table <- pop |> 
  group_by(soa_code) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            dep=mean(mdm_rank),
            townsend=mean(custom_townsend_score_dz),
            DEA = first(DEA2014_name),
            #first(HSCT),
            town = first(SETTLEMENT2015_name),
            renewal_area = first(NRA_name)
  ) |> 
  filter(tot>4) |> 
  mutate(prev = big/tot) |> 
  arrange(desc(dep)) |> 
  head(20)

############################
############################

# deprivation_bmi_age_chart
# top_town_dep_table
# top_soa_dep_table
# top_dea_overweight_prev
# top_town_overweight_prev

############################
############################




save(list = c(
  'deprivation_bmi_age_chart',
  'DEA_obesity_prevalence',
  'top_town_per_hsct_plot',
  'top_town_dep_table'
  ),
  file = './preprocess/deprivation.RData')


############################
############################
############################

