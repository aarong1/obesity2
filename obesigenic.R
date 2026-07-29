# parks

sdz_pts <- read_excel("data/geography-census-2021-population-weighted-centroids-for-data-zones-and-super-data-zones.xlsx",
                  sheet = 2)

parks
fast_food

sdz_pts <- st_as_sf(sdz_pts, coords = c('X', 'Y'), crs = 29902) |>
  st_transform(4326) 

sdz_pts%>% 
  leaflet() %>% 
  leaflet::addTiles() %>% 
  addCircles()
  
parks$area
dist_parks <- st_distance( sdz_pts, parks) 

dist_parks_df <- dist_parks %>% 
  data.frame() %>% 
  mutate(sdz = sdz_pts$SDZ2021_code) %>% 
  # set_names() %>% 
  pivot_longer(cols = c(everything(),-sdz)) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(value<3000) %>% 
  mutate(park_id = as.numeric(sub(x = name, pattern ='X', replacement = ''))) %>% 
  mutate(park_area = parks$area[park_id])

dist_parks_df <- dist_parks_df %>% 
  group_by(sdz) %>% 
  summarise(no_pks = n(), area_pks = sum(park_area))

dist_food <- st_distance( sdz_pts, fast_food)

dist_food_df <- dist_food %>% 
  data.frame() %>% 
  mutate(sdz = sdz_pts$SDZ2021_code) %>% 
  # set_names() %>% 
  pivot_longer(cols = c(everything(),-sdz)) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(value<1000) %>% 
  mutate(fast_food_id = as.numeric(sub(x = name, pattern ='X', replacement = ''))) 

dist_food_df <- dist_food_df %>% 
  group_by(sdz) %>% 
  summarise(no_food = n ())

pubs <- read_sf("pubs_bars.geojson")

dist_pubs <- st_distance( sdz_pts, pubs)

dist_pubs_df <- dist_pubs %>% 
  data.frame() %>% 
  mutate(sdz = sdz_pts$SDZ2021_code) %>% 
  # set_names() %>% 
  pivot_longer(cols = c(everything(),-sdz)) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(value<2000) %>% 
  mutate(fast_food_id = as.numeric(sub(x = name, pattern ='X', replacement = ''))) 

dist_pubs_df <- dist_pubs_df %>% 
  group_by(sdz) %>% 
  summarise(no_pubs = n ())


pop <- pop %>% 
  left_join(dist_parks_df, by=(c('sdz_code' = 'sdz') ) ) %>%
  left_join(dist_food_df, by=(c('sdz_code' = 'sdz') ) ) %>%
  left_join(dist_pubs_df, by=(c('sdz_code' = 'sdz') ) ) 
  
  

### At Super Data Zone level ----

base_facet_mdm_quintile_soa_name <- pop %>% 
  # group_by(DEA2014_name) %>%
  group_by(sdz_code) %>%
  replace_na(list(no_pks = 0, area_pks = 0, no_food = 0, no_pubs = 0)) %>%
  summarise( bmi = sum(bmi %in% c('', 'obese')),
             tot= n(),
             mdm_rank = mean(mdm_rank),
             tdep = mean(custom_townsend_score_dz),
             no_pks = mean(no_pks,na.rm=T),
             sum_pks = sum(no_pks,na.rm=T),
             area_pks = mean(area_pks,na.rm=T),
             sum_area_pks = sum(area_pks,na.rm=T),
             no_food = mean(no_food,na.rm=T),
             no_pubs = mean(no_pubs,na.rm=T),
             
             # mdm_quintile_soa_name = first(mdm_decile_soa)
             mdm_quintile_soa_name = first(mdm_quintile_soa_name)
  ) %>% 
  mutate(pubs_per = no_pubs/tot) %>% 
  add_count(sdz_code, wt = tot, name = 'sdz_pop') %>%
  
  mutate(bmi_per = bmi/sdz_pop) %>%
  mutate(per_pubs = no_pubs/sdz_pop) %>%
  mutate(per_sum_area_pks = sum_area_pks/sdz_pop) %>%
  mutate(per_no_pks = no_pks/sdz_pop) %>%
  mutate(per_no_food = no_food/sdz_pop) %>%
  mutate(per_no_pubs = no_pubs/sdz_pop) %>%
  
  # mutate(bmi_per = bmi/sdz_pop) %>%
  # mutate(per_pubs = no_pubs) %>%
  # mutate(per_sum_area_pks = sum_area_pks) %>%
  # mutate(per_no_pks = no_pks) %>%
  # mutate(per_no_food = no_food) %>%
  # mutate(per_no_pubs = no_pubs) %>%
  
  # filter(no_pubs !=0) %>% 
  # filter(no_food>0) %>%
  filter(tot>50) %>%
  group_by(mdm_quintile_soa_name) 
  
 (
   deprivation_bmi_pk_area <- base_facet_mdm_quintile_soa_name %>%
    # e_charts(sum_area_pks, emphasis = list(focus = 'series')) %>% 
    # e_charts(no_pks, emphasis = list(focus = 'series')) %>%
    # e_charts(no_food, emphasis = list(focus = 'series')) %>% 
    e_charts(per_sum_area_pks, emphasis = list(focus = 'series')) %>%
    e_scatter(bmi_per) %>%
    e_lm(  bmi_per ~ per_sum_area_pks ,    #no_food sum_area_pks no_pks mdm_rank no_pubs
         name = c('Least Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Most Deprived')
        # names = 1:10,
  )
  )
  
  base_facet_mdm_quintile_soa_name %>%
    # e_charts(sum_area_pks, emphasis = list(focus = 'series')) %>% 
    # e_charts(no_pks, emphasis = list(focus = 'series')) %>%
    # e_charts(no_food, emphasis = list(focus = 'series')) %>% 
    e_charts(per_no_pks, emphasis = list(focus = 'series')) %>%
    e_scatter(bmi_per) %>%
    e_lm(  bmi_per ~ per_no_pks ,    #no_food sum_area_pks no_pks mdm_rank no_pubs
           name = c('Least Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Most Deprived')
           # names = 1:10,
    )
  
  base_facet_mdm_quintile_soa_name %>%
    # e_charts(sum_area_pks, emphasis = list(focus = 'series')) %>% 
    # e_charts(no_pks, emphasis = list(focus = 'series')) %>%
    # e_charts(no_food, emphasis = list(focus = 'series')) %>% 
    e_charts(per_no_pks, emphasis = list(focus = 'series')) %>%
    e_scatter(bmi_per) %>%
    e_lm(  bmi_per ~ per_no_pks ,    #no_food sum_area_pks no_pks mdm_rank no_pubs
           name = c('Least Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Most Deprived')
           # names = 1:10,
    )
  
  base_facet_mdm_quintile_soa_name %>%
    # e_charts(sum_area_pks, emphasis = list(focus = 'series')) %>% 
    # e_charts(no_pks, emphasis = list(focus = 'series')) %>%
    # e_charts(no_food, emphasis = list(focus = 'series')) %>% 
    e_charts(per_pubs, emphasis = list(focus = 'series')) %>%
    e_scatter(bmi_per) %>%
    e_lm(  bmi_per ~ per_pubs ,    #no_food sum_area_pks no_pks mdm_rank no_pubs
           name = c('Least Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Most Deprived')
           # names = 1:10,
    )


### At Super Data Zone level by quintile ----
(
mdm_rank_quintile <- pop %>% 
  # group_by(DEA2014_name) %>%
  group_by(sdz_code) %>%
  replace_na(list(no_pks = 0, area_pks = 0, no_food = 0)) %>%
  summarise( bmi = sum(bmi %in% c('overweight', 'obese')),
             tot= n(),
             mdm_rank = mean(mdm_rank),
             tdep = mean(custom_townsend_score_dz),
             no_pks = mean(no_pks,na.rm=T),
             sum_pks = sum(no_pks,na.rm=T),
             area_pks = mean(area_pks,na.rm=T),
             sum_area_pks = sum(area_pks,na.rm=T),
             no_food = mean(no_food,na.rm=T),
             # mdm_quintile_soa_name = first(mdm_decile_soa)
             mdm_quintile_soa_name = first(mdm_quintile_soa_name)
             ) %>% 
  mutate(per = bmi/tot) %>% 
  # filter(no_food>0) %>%
  # filter(tot>100) %>%
  group_by(mdm_quintile_soa_name) %>%
  # e_charts(sum_area_pks, emphasis = list(focus = 'series')) %>% 
  # e_charts(no_pks, emphasis = list(focus = 'series')) %>%
  # e_charts(no_food, emphasis = list(focus = 'series')) %>% 
  e_charts(mdm_rank, emphasis = list(focus = 'series')) %>%
  
  e_scatter(per) %>%
  e_lm( per ~ mdm_rank ,    #no_food  sum_area_pks  no_pks mdm_rank no_pubs
        name = c('Least Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Most Deprived')
     # names = 1:10,
     )
)

### At Super Data Zone level by sex----


base_facet_sex  <- pop %>% 
    # group_by(DEA2014_name) %>%
    group_by(sdz_code,sex) %>%
    replace_na(list(no_pks = 0, area_pks = 0, no_food = 0, no_pubs = 0)) %>%
    filter_out(is.na(bmi)) %>% 
    filter_out(is.na(sex)) %>% 
    
    summarise( bmi = sum(bmi %in% c('overweight', 'obese')),
               tot = n(),
               mdm_rank = mean(mdm_rank),
               tdep = mean(custom_townsend_score_dz),
               no_pks = mean(no_pks,na.rm=T),
               sum_pks = sum(no_pks,na.rm=T),
               area_pks = mean(area_pks,na.rm=T),
               sum_area_pks = sum(area_pks,na.rm=T),
               no_food = mean(no_food,na.rm=T),
               no_pubs = mean(no_pubs,na.rm=T),
               # mdm_quintile_soa_name = first(mdm_decile_soa)
               mdm_quintile_soa_name = first(mdm_quintile_soa_name)
    ) %>% 
    add_count(sdz_code, wt = tot, name = 'sdz_pop') %>% 
    
    mutate(per_pubs = no_pubs/sdz_pop) %>% 
    mutate(per_sum_area_pks = sum_area_pks/sdz_pop) %>% 
    mutate(per_no_pks = no_pks/sdz_pop) %>% 
    mutate(per_no_food = no_food/sdz_pop) %>% 
    mutate(per_no_pubs = no_pubs/sdz_pop) %>% 
  
    # mutate(per = tot/sdz_pop) %>%
    mutate(per = bmi/tot) %>%
    
    # filter(area_pks>0) %>% 
    # filter(tot>100) %>% 
    # group_by(sex) %>% 
    group_by(sex) 
  
  ( males_affected_by_pubs_more_than_women <- base_facet_sex %>% 
    e_charts(per_no_pubs, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
      e_y_axis(formatter = e_axis_formatter("percent")) %>%
    e_lm(per ~ per_no_pubs ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         name = c( 'Females', 'Males')
         #name = c('normal', 'overweight', 'obese')
         )  %>%
      e_theme('auritus')
    
  )
  
  (males_affected_by_food_outlets_more_than_women <- base_facet_sex %>% 
    e_charts(per_no_food, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
      e_y_axis(formatter = e_axis_formatter("percent")) %>%
      
    e_lm(per ~ per_no_food ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         name = c( 'Females', 'Males')
         #name = c('normal', 'overweight', 'obese')
    ) %>%
      
      e_theme('auritus')
    
  )
  
  base_facet_sex %>% 
    e_charts(per_no_pks, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
    e_lm(per ~ per_no_pks ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         name = c( 'Females', 'Males')
         #name = c('normal', 'overweight', 'obese')
    ) #%>%
  
(  
  females_respond_less_positively_to_green_space <- base_facet_sex %>% 
    e_charts(per_sum_area_pks, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
    e_y_axis(formatter = e_axis_formatter("percent")) %>%
    
    e_lm(per ~ per_sum_area_pks ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         name = c( 'Females', 'Males')
         #name = c('normal', 'overweight', 'obese')
    ) %>%
    e_theme('auritus')
  
  )

### At Super Data Zone level - not facet ----


 base_no_facet <-  pop %>% 
    # group_by(DEA2014_name) %>%
    group_by(sdz_code) %>%
    replace_na(list(no_pks = 0, area_pks = 0, no_food = 0 , no_food = 0)) %>%
    summarise( bmi = sum(bmi %in% c('overweight', 'obese')),
               tot = n(),
               mdm_rank = mean(mdm_rank),
               tdep = mean(custom_townsend_score_dz),
               no_pks = mean(no_pks,na.rm=T),
               sum_pks = sum(no_pks,na.rm=T),
               area_pks = mean(area_pks,na.rm=T),
               sum_area_pks = sum(area_pks,na.rm=T),
               no_food = mean(no_food,na.rm=T),
               no_pubs = mean(no_pubs, na.rm=T),
               # mdm_quintile_soa_name = first(mdm_decile_soa)
               mdm_quintile_soa_name = first(mdm_quintile_soa_name)
    ) %>% 
    # add_count(sdz_code, wt = tot, name = 'sdz_pop') %>% 
    mutate(per = bmi/tot) %>%
    mutate(sdz_pop = tot) %>% 
    mutate(per_pubs = no_pubs/sdz_pop) %>%
    mutate(per_sum_area_pks = sum_area_pks/sdz_pop) %>% 
    mutate(per_no_pks = no_pks/sdz_pop) %>% 
    mutate(per_no_food = no_food/sdz_pop) %>% 
    mutate(per_no_pubs = no_pubs/sdz_pop) %>% 
    mutate(per = bmi/tot) #%>% 
    # filter(no_food>0) %>%
    # filter(tot>100) %>%
    # group_by(mdm_quintile_soa_name) %>%
    # e_charts(sum_area_pks, emphasis = list(focus = 'series')) %>% 
    # e_charts(no_pks, emphasis = list(focus = 'series')) %>%
    # e_charts(no_food, emphasis = list(focus = 'series')) %>% 
  
   base_no_facet %>%  e_charts(per_no_pubs, emphasis = list(focus = 'series')) %>%
    e_scatter(per) %>%
    e_lm( per ~ per_no_pubs,    #no_food sum_area_pks no_pks mdm_rank no_pubs
          # names = 1:10,
          # name = c('Most Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Least Deprived')
    )
  
  base_no_facet %>%  
    e_charts(per_no_food, emphasis = list(focus = 'series')) %>%
    e_scatter(per) %>%
    e_lm( per ~ per_no_food,    #no_food sum_area_pks no_pks mdm_rank no_pubs
          # names = 1:10,
          # name = c('Most Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Least Deprived')
    )
  
  base_no_facet %>% #filter(per_no_pks!=0) %>% 
    e_charts(per_no_pks, emphasis = list(focus = 'series')) %>%
    e_scatter(per) %>%
    e_lm( per ~ per_no_pks,    #no_food sum_area_pks no_pks mdm_rank no_pubs
          # names = 1:10,
          # name = c('Most Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Least Deprived')
    )
  
  base_no_facet %>% 
    # filter(per_sum_area_pks !=0) %>% 
    e_charts(per_sum_area_pks, emphasis = list(focus = 'series')) %>%
    e_scatter(per) %>%
    e_lm( per ~ per_sum_area_pks,    #no_food sum_area_pks no_pks mdm_rank no_pubs
          # names = 1:10,
          # name = c('Most Deprived', 'Quintile 4', 'Quintile 3', 'Quintile 2', 'Least Deprived')
    )
  
  base_facet_urban_rural <-  pop %>% 
    # group_by(DEA2014_name) %>%
    group_by(sdz_code,Urban_mixed_rural_status) %>%
    
    replace_na(list(no_pks = 0, area_pks = 0, no_food = 0, no_pubs = 0)) %>%
    filter_out(is.na(bmi)) %>% 
    summarise( bmi = sum(bmi %in% c('overweight', 'obese')),
      tot = n(),
      mdm_rank = mean(mdm_rank),
      tdep = mean(custom_townsend_score_dz),
      no_pks = mean(no_pks,na.rm=T),
      sum_pks = sum(no_pks,na.rm=T),
      area_pks = mean(area_pks,na.rm=T),
      sum_area_pks = sum(area_pks,na.rm=T),
      no_food = mean(no_food,na.rm=T),
      no_pubs = mean(no_pubs,na.rm=T),
      
      # mdm_quintile_soa_name = first(mdm_decile_soa)
      
      mdm_quintile_soa_name = first(mdm_quintile_soa_name)
    ) %>% 
    add_count(sdz_code, wt = tot, name = 'sdz_pop') %>% 
    mutate(per = bmi/tot) %>%
    mutate(per_pubs = no_pubs/sdz_pop) %>% 
    mutate(per_sum_area_pks = sum_area_pks/sdz_pop) %>% 
    mutate(per_no_pks = no_pks/sdz_pop) %>% 
    mutate(per_no_food = no_food/sdz_pop) %>% 
    mutate(per_no_pubs = no_pubs/sdz_pop) %>% 
    
    
    # mutate(per = per/sdz_pop) %>% View()
    # filter(area_pks>0) %>% 
    filter(tot>10) %>%
    # group_by(sex) %>% 
    group_by(Urban_mixed_rural_status) 
  

  base_facet_urban_rural %>% 
    e_charts(per_no_pubs, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
    e_lm(per ~ per_no_pubs ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         # name = c( 'Females', 'Males')
         name = c('Rural',  'Mixed',  'Urban')
         #name = c('normal', 'overweight', 'obese')
    ) #%>%
  
  base_facet_urban_rural %>% 
    e_charts(per_no_food, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
    e_lm(per ~ per_no_food ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         # name = c( 'Females', 'Males')
         name = c('Rural',  'Mixed',  'Urban')
         #name = c('normal', 'overweight', 'obese')
    ) #%>%
  
  base_facet_urban_rural %>% 
    e_charts(per_no_pks, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
    e_lm(per ~ per_no_pks ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         # name = c( 'Females', 'Males')
         name = c('Rural',  'Mixed',  'Urban')
         #name = c('normal', 'overweight', 'obese')
    ) #%>%
  
  (
  semi_rural_responds_best_to_green_space <- base_facet_urban_rural %>% 
    e_charts(per_sum_area_pks, emphasis = list(focus = 'series')) %>% 
    e_scatter(per) %>%
    e_y_axis(formatter = e_axis_formatter("percent")) %>%
    e_lm(per ~ per_sum_area_pks ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         # name = c( 'Females', 'Males')
         name = c('Rural',  'Mixed',  'Urban')
         #name = c('normal', 'overweight', 'obese')
    ) %>%
      e_theme('auritus')
    
    )
  
  
  base_facet_bmi <- pop %>% 
    # group_by(DEA2014_name) %>%
    group_by(sdz_code,bmi) %>%
    replace_na(list(no_pks = 0, area_pks = 0, no_food = 0)) %>%
    filter_out(is.na(bmi)) %>% 
    summarise( #bmi = sum(bmi %in% c('overweight', 'obese')),
      tot = n(),
      mdm_rank = mean(mdm_rank),
      tdep = mean(custom_townsend_score_dz),
      no_pks = mean(no_pks,na.rm=T),
      sum_pks = sum(no_pks,na.rm=T),
      area_pks = mean(area_pks,na.rm=T),
      sum_area_pks = sum(area_pks,na.rm=T),
      no_food = mean(no_food,na.rm=T),
      no_pubs = mean(no_pubs,na.rm=T),
      # mdm_quintile_soa_name = first(mdm_decile_soa)
      mdm_quintile_soa_name = first(mdm_quintile_soa_name)
    ) %>% 
    add_count(sdz_code, wt = tot, name = 'sdz_pop') %>% 
    mutate(per = tot/sdz_pop) %>%
    mutate(per_pubs = no_pubs/sdz_pop) %>% 
    mutate(per_sum_area_pks = sum_area_pks/sdz_pop) %>% 
    mutate(per_no_pks = no_pks/sdz_pop) %>% 
    mutate(per_no_food = no_food/sdz_pop) %>% 
    mutate(per_no_pubs = no_pubs/sdz_pop) %>% 
    
    # filter(area_pks>0) %>% 
    # filter(tot>100) %>% 
    group_by(bmi) 
  
    (
    fast_food_outlets_weight_on_obesity <- base_facet_bmi %>% 
      e_charts(per_no_food, emphasis = list(focus = 'series')) %>% 
      e_scatter(per) %>%
        e_y_axis(formatter = e_axis_formatter("percent")) %>%
      e_lm(per ~ per_no_food ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
         name = c('normal', 'overweight', 'obese')
      ) %>% 
        e_theme('auritus')
  )
  
  (
    pubs_weight_on_obesity <- base_facet_bmi %>% 
      e_charts(per_no_pubs, emphasis = list(focus = 'series')) %>% 
      e_scatter(per) %>%
      e_y_axis(formatter = e_axis_formatter("percent")) %>%
      e_y_axis(formatter = e_axis_formatter("percent")) %>%
      e_lm(per ~ per_no_pubs ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
           name = c('normal', 'overweight', 'obese')
      ) %>% 
      e_theme('auritus')
    )
    base_facet_bmi %>% 
      e_charts(per_no_pks, emphasis = list(focus = 'series')) %>% 
      e_scatter(per) %>%
      e_lm(per ~ per_no_pks ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
           name = c('normal', 'overweight', 'obese')
      ) 
    
    base_facet_bmi %>% 
      e_charts(per_sum_area_pks, emphasis = list(focus = 'series')) %>% 
      e_scatter(per) %>%
      e_lm(per ~ per_sum_area_pks ,   #no_food  sum_area_pks  no_pks mdm_rank no_pubs
           name = c('normal', 'overweight', 'obese')
      ) 
    
    

    
    save(list = c(
      'males_affected_by_pubs_more_than_women',
      'males_affected_by_food_outlets_more_than_women',
      'females_respond_less_positively_to_green_space',
      'semi_rural_responds_best_to_green_space',
      'fast_food_outlets_weight_on_obesity',
      'pubs_weight_on_obesity'
    ),
    file = './preprocess/obesigenic.RData')

  # x$x$opts$series %>% View()
  # x$x$data[[3]] %>% View()
  # x$x$data[[3]] %>% View()
  
 # ( x$x$data$obese$.fitted - x$x$data$obese$.fitted [1]) * 100 * 10  / ( x$x$data$obese$no_food - 0 ) 
  
