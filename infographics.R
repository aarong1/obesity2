# infographics.R
library(data.tree)

style <- list(
  normal = list(opacity = 0.5), # normal
  emphasis = list(opacity = 0.9) # on hover
)

symbols = c(
  'path://M31.193,4.507a4.507,4.507 0 1,0 -9.014,0a4.507,4.507 0 1,0 9.014,0 M28.256,11.163c-1.123-0.228-2.344-0.218-3.447,0.042c-7.493,0.878-9.926,9.551-9.239,16.164 c0.298,2.859,4.805,2.889,4.504,0c-0.25-2.41-0.143-6.047,1.138-8.632c0,3.142,0,6.284,0,9.425c0,0.111,0.011,0.215,0.016,0.322 c-0.003,0.051-0.015,0.094-0.015,0.146c0,7.479-0.013,14.955-0.322,22.428c-0.137,3.322,5.014,3.309,5.15,0 c0.242-5.857,0.303-11.717,0.317-17.578c0.244,0.016,0.488,0.016,0.732,0.002c0.015,5.861,0.074,11.721,0.314,17.576 c0.137,3.309,5.288,3.322,5.15,0c-0.309-7.473-0.32-14.949-0.32-22.428c0-0.232-0.031-0.443-0.078-0.646 c-0.007-3.247-0.131-6.497-0.093-9.742c1.534,2.597,1.674,6.558,1.408,9.125c-0.302,2.887,4.206,2.858,4.504,0 C38.678,20.617,36.128,11.719,28.256,11.163z',
  'path://M49.437,19.672c5.424,0,9.836-4.413,9.836-9.836S54.861,0,49.437,0c-5.423,0-9.835,4.413-9.835,9.836 S44.013,19.672,49.437,19.672z M71.508,52.416L62.73,25.217c-0.47-1.456-2.037-2.596-3.566-2.596h-2.127c-0.031,0-0.059,0.009-0.09,0.01 c-0.032-0.001-0.062-0.01-0.094-0.01H42.023c-0.058,0-0.112,0.014-0.169,0.017c-0.055-0.003-0.106-0.017-0.161-0.017h-1.654 c-1.53,0-3.096,1.14-3.566,2.596l-8.777,27.198c-0.26,0.807-0.152,1.623,0.297,2.24c0.449,0.617,1.193,0.971,2.041,0.971h1.38 c1.526,0,3.098-1.135,3.579-2.584l4.031-12.159v6.562c-0.678,0.403-1.265,0.954-1.616,1.572l-6.617,11.684 c-0.414,0.73-0.478,1.553-0.175,2.258c0.302,0.705,0.942,1.226,1.757,1.43l7.232,1.809v29.005c0,2.206,1.794,4,4,4h0.976 c2.206,0,4-1.794,4-4V68.348c0.34,0.033,0.699,0.052,1.069,0.052c0.472,0,0.925-0.03,1.344-0.083v26.886c0,2.206,1.794,4,4,4h0.976 c2.206,0,4-1.794,4-4V66.08l6.542-1.68c0.812-0.208,1.45-0.733,1.75-1.44s0.236-1.53-0.177-2.259l-6.617-11.684 c-0.35-0.619-0.938-1.169-1.616-1.572V40.56l4.336,12.505c0.499,1.437,2.08,2.562,3.6,2.562h1.382c0.848,0,1.591-0.354,2.041-0.971 C71.66,54.039,71.768,53.222,71.508,52.416z'
)

bmi_sex_df <- pop |> 
  filter(!is.na(bmi)) |> 
  count(bmi=bmi%in% c('obese','overweight'),sex) |> 
  mutate(n=n * pop_scale_up) %>% 
  #pivot_wider(names_from = sex,values_from = n) |> 
  filter(bmi==TRUE) |> 
  mutate(x = c( 0.5, 1.5)) |>
  mutate(x = c( 'Males', 'Females')) |>
  
  mutate(symbols=symbols)
   

# males_bmi_sex_df <- bmi_sex_df|> 
#   select('Males') |> 
#   mutate(x=1.5)
# 
# females_bmi_sex_df <- bmi_sex_df|> 
#   select(c(1)) |> 
#   mutate(x=0.5)

(
overweight_obese_sex <- bmi_sex_df |> 
   group_by(sex) |>
  e_charts(x,emphasis = list(focus = 'self')) |> 
  e_pictorial(name = c('Male','Female'),n, symbol = symbols, 
              barCategoryGap = "50%", legend = T,
              itemStyle = style) |> 
    # e_visual_map(n, show = T) |>
    e_labels(formatter = '{c}') |> 
  # e_data(females_bmi_sex_df) |> 
  # e_pictorial(name = 'Females',Females, symbol = symbols[2], 
  #             barCategoryGap = "50%", 
  #             itemStyle = style) |> 
  e_axis(axis = 'y',    
         splitLine = list(show = FALSE),
         axisLabel = list(show = FALSE),
         axisTick = list(show = FALSE),
         axisLine = list(show = FALSE)
) |>
    # e_title('Sex') |> 
  # e_mark_line( serie = "n",name='Male'#,  
  #              data = 
  #   list(yAxis = bmi_sex_df$n[1]
  # )
  # ) |> 
    # e_mark_line(name='Female',  data = 
    #               list(xAxis =c(1),yAxis = bmi_sex_df$n[2]
    #               )
    #             ) |> 
  e_x_axis(
    # max = 1,
    splitLine = list(show = FALSE),
    axisLabel = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLine = list(show = FALSE)
    
  ) |>
  e_legend() |> 
  e_tooltip()  %>% 
    e_grid(containLabel = T,left='15%') |> 
  # e_title("SVG path") |> 
  # e_theme_custom("westeros", th) |>
   e_theme('roma')
)



df <- data.frame(
  price = rnorm(5, 10),
  amount = rnorm(5, 15),
  letter = LETTERS[1:5]
)

(
BMI_parallel_chart <- reduced_pop |> 
    select(mdm_rank,
           qrisk_score,
           bmi,
           multimorbidity,
           age20,
           age) |> 
  filter(!is.na(bmi)) |> 
  filter(qrisk_score>=0.02) |> 
  arrange(desc(age20)) |>
  mutate(bmi = 
           case_when(bmi == 'normal' ~ 25+runif(min = -1,n()),
                     bmi == 'overweight' ~ 30+runif(min = -1,n()),
                     bmi == 'obese' ~ 35+runif(min = -1,n()),
                     TRUE ~ 0)) |>
  mutate(multimorbidity = multimorbidity+runif(n())/2) |>
  # group_by(mdm_quintile_soa) |>
  e_charts(emphasis = list(focus = 'series')) |> 
  e_parallel(
             mdm_rank,
             qrisk_score, 
             bmi,
             multimorbidity,
             age,
            opts = list(
              lineStyle = list(
                opacity = 0.01))) |> 
  # e_visual_map(age,type = 'continuous') %>% 
  
  # e_title("BMI Chart with contributing and adjacent characteristics") |> 
  e_theme('roma') %>% 
    e_brush() %>% 
  e_grid(containLabel = T))


(slope_chart <- pop %>% 
  count(bmi=bmi %in% c('overweight', 'obese'), mdm_quintile_soa, mdm_quintile_soa_name) %>% 
  arrange(mdm_quintile_soa) %>% 
  group_by(mdm_quintile_soa==1) %>% 
  mutate(bmi = as.character(bmi)) %>% 
 mutate( mdm_quintile_soa_name =
           factor(  mdm_quintile_soa_name,
                       levels = c('Most Deprived',     
                        'Quintile 2',        
                        'Quintile 3',        
                        'Quintile 4',        
                        'Least Deprived'    ))) %>% 
  e_chart(bmi) %>% 
  e_line(n,) %>% 
    # e_labels(
    #   show = TRUE,
    #   position = "end",
    #   formatter = htmlwidgets::JS("function(p){ return p.data.label; }")
    # ) %>% 
  # e_color(c('red','black','red','red','red')) %>%
  e_tooltip() %>% 
    e_theme('walden')
) 

(
slope_chart_age_deprivation <- pop %>%
  # count(age20,mdm_quintile_soa_name,bmi = bmi %in% c('overweight' , 'obese')) %>% 
    count(age20,mdm_quintile_soa_name,bmi = bmi %in% c( 'obese')) %>% 
    
  add_count(age20,mdm_quintile_soa_name,wt = n,name='tot') %>% 
  filter_out(!bmi) %>% 
  # filter(bmi %in% c('overweight' , 'obese')) %>%
  filter(!is.na(age20)) %>% 
  filter(mdm_quintile_soa_name %in% c('Least Deprived',
                                      'Most Deprived')) %>% 
  
  group_by(age20) %>% 
  mutate(per=n/tot) %>% 
  e_charts(mdm_quintile_soa_name,emphasis = list(focus = 'self')) %>% 
  e_line(per,symbol='none') %>%
  e_scatter(per,symbol='circle',symbol_size = 15) %>%
  # e_theme %>%
  e_theme('azul') %>%
  # e_theme(name = 'azul') %>% 
  e_axis( axis = 'y', formatter = e_axis_formatter('percent'))  
)

(
  slope_chart_age_sex <- pop %>%
    # count(age20,mdm_quintile_soa_name,bmi = bmi %in% c('overweight' , 'obese')) %>% 
    count(sex,age20,bmi = bmi %in% c( 'obese')) %>% 
    
    add_count(sex,age20,wt = n,name='tot') %>% 
    filter_out(!bmi) %>% 
    # filter(bmi %in% c('overweight' , 'obese')) %>%
    filter(!is.na(sex)) %>% 

    
    group_by(sex) %>% 
    mutate(per=n/tot) %>% 
    e_charts(age20,emphasis = list(focus = 'self')) %>% 
    e_line(per,symbol='none') %>%
    e_scatter(per,symbol='circle',symbol_size = 15) %>%
    # e_theme %>%
    e_theme('azul') %>%
    # e_theme(name = 'azul') %>% 
    e_axis( axis = 'y', formatter = e_axis_formatter('percent'))  
)


bmi_sya_age <- pop |> 
  count(bmi,age) |>  #=as.character(age)
  mutate(n = n * pop_scale_up) |> 
  mutate(age = as.character(age)) |> 
  filter(!is.na(bmi)) |> 
  # filter(age>20,age<90) |>
  pivot_wider(names_from = bmi, values_from = n) |>
  # mutate(  apples = runif(n()),
  #          bananas = runif(n()),
  #          pears = runif(n()),
  #          dates = seq(1, n()),
  #          dates = as.character(seq.Date(Sys.Date() - n()+1 , Sys.Date(), by = "day"))) |> 
   # group_by(bmi) |>
  e_charts(age,emphasis = list(focus = 'self')) |>
  # e_single_axis(index = 1,type = 'category') |> 
  # e_tooltip,(trigger='axis') |> 
  # e_river(bmi) |>
    # e_single_axis(index = 2,type = 'value',max = 'dataMax') |>  #|>,    coord_system = "singleAxis"
  # echarts_from_json(txt='t')
  # e_river(apples) #|>
  # e_river(bananas) |> 
  e_grid(left='15%', bottom = '10%' ,  containLabel = T ) |> 
  e_river(obese) |>
  e_single_axis(index = 0,type = 'value',max = 'dataMax') |>  #|>,    coord_system = "singleAxis"
  e_theme('roma') |>
  # e_x_axis(type = 'category') |>
  e_river(overweight) |>     
  e_single_axis(index = 0,type = 'value',max = 'dataMax') |>  #|>,    coord_system = "singleAxis"
  
  e_legend(bottom=29) |>
  e_river(normal)  |> 
  e_single_axis(index = 0,type = 'value',max = 'dataMax')|>
  
  e_tooltip(trigger = "axis");bmi_sya_age #|> 
  # e_title("BMI with Age", "Continuouse Single year of Age")

deprivation_sya_age <- pop |> 
  count(mdm_quintile_soa_name,age) |>  #=as.character(age)
  filter(!is.na(mdm_quintile_soa_name)) |> 
  group_by(mdm_quintile_soa_name) %>% 
  e_charts(age,emphasis = list(focus = 'self')) |>
  e_river(n) |>
  e_single_axis(index = 0,type = 'value',max = 'dataMax') |>  #|>,    coord_system = "singleAxis"
  e_theme('roma') |>
  e_grid(left='15%',  containLabel = T ) |> 
  e_tooltip(trigger = "axis") 



trusts <- sf::read_sf('./data/trustboundaries.geojson')

trusts <- st_transform(trusts, 'WGS84')

trusts <- st_make_valid(trusts)

trusts1 <- st_simplify(trusts,
                       preserveTopology = TRUE,
                       dTolerance = 1000)

#rm(list='trusts')
# plot(trusts1)

file.remove("./data/trusts.geojson")
st_write(trusts1,'./data/trusts.geojson', append = FALSE)

trusts_json <- jsonlite::read_json("./data/trusts.geojson")
object.size(trusts_json)

trusts_df <- pop |> 
  add_count(HSCT,name='trust_pop') |> 
  count(HSCT,trust_pop,overweight=bmi%in%c('obese','overweight')) |> 
  filter(overweight == T) |> 
  mutate(trust_pop = trust_pop *model_specification$population$scale_down_factor,
         n = n*model_specification$population$scale_down_factor
         ) |> 
  rename(c( 'TrustName' = 'HSCT')) |> 
  mutate(Name = sort(trusts1$TrustName))

trusts1 <- trusts1 |> 
  left_join(trusts_df, by =c('TrustName' = 'Name'))

pop |> 
  add_count(soa_name,soa_code,name='soa_pop') |> 
  count(,soa_name,soa_code,soa_pop,overweight=bmi%in%c('obese','overweight')) |> 
  filter(overweight == T) |> 
  select( soa_name, soa_code, soa_pop, n)


pop |> 
  add_count(LGD2014_name,LGD2014_code,name='soa_pop') |> 
  count(,soa_name,soa_code,soa_pop,overweight=bmi%in%c('obese','overweight')) |> 
  filter(overweight == T) |> 
  select( soa_name, soa_code, soa_pop, n)

  

cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 10000);
}"



  


(
trust_bar <-  
    trusts_df %>% 
  #head() |> 
  # group_by(Name) |>
  e_charts(Name,emphasis = list(focus = 'self'),reorder = FALSE ) %>% #height = '450px', width='450px'
  e_bar(universalTransition = TRUE,
        animationDurationUpdate = 2000L,
        name='TrustName',
        legend = F,
        tooltip = T,
        serie = n) |> 
    e_tooltip(backgroundColor='white') |> 
    e_x_axis(axisLabel = list(rotate = 15)) |> 
  # e_flip_coords() |>
  e_visual_map(serie = n, show =F) |>
  # e_grid(containLabel = T) |> 
  e_theme('roma') %>% 
  e_x_axis(show = FALSE) |>
  e_y_axis(show = FALSE) |>
  # remove grid padding
  e_grid(
    left = 0, right = 0, top = 0, bottom = 0,
    # containLabel = FALSE
  ) |>
  
  # remove legend
  e_legend(show = FALSE)
  )
(
 trusts_map <-    trusts1 %>% 
    #head() |> 
    e_charts(TrustName,emphasis = list(focus = 'self'),
             reorder = FALSE) %>%
    e_map_register("custom_map", trusts_json) |> 
      e_visual_map(n, show = F) |> 
      e_theme('roma') |> 
    e_map(universalTransition = TRUE,
          animationDurationUpdate = 5000L,
          serie=n, 
          name='TrustName',
          nameProperty = "TrustName",
          roam=F,
          map = "custom_map",
          itemStyle=list( borderColor='white'))
)



# data.frame(lon=1,lat=1,value = 10) |>
#   dplyr::filter(value > 8) |>
#   e_charts(lon) |>
#   e_leaflet() |>
#   e_leaflet_tile(  template = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',#"https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png",
#                    options = list(maxZoom = 10)) |>
#   e_scatter(lat, size = value, coord_system = "leaflet")

# trusts_map %>%

  # e_charts(trusts1) %>%
  #   e_leaflet() %>% 
  #   e_leaflet_tile(  template = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png") 
  
  url <- paste0(
    "https://echarts.apache.org/examples/",
    "data-gl/asset/data/population.json"
  )
  data <- jsonlite::fromJSON(url)
  data <- as.data.frame(data)
  names(data) <- c("lon", "lat", "value")
  data$value <- log(data$value)
  
  data |>
    sample_n(size = 100) %>% 
    e_charts(lat,emphasis = list(focus = 'self')) |>
    e_leaflet() |>
    e_leaflet_tile() |>
    e_effect_scatter(lon, size = value)
  
bar_map_morph <- e_morph(trusts_map, trust_bar, callback = cb)


cb <- "() => {
  let x = 0;

  
    chart.on('click', function(e) {
      x = x + 1;
      chart.setOption(opts[x % 2], true);
    });
  
}"

bar_map_morph_click <- e_morph(trusts_map, trust_bar, callback = cb)


library(leaflet)
library(htmlwidgets)

{ pal <- colorNumeric(palette = "Blues", domain = trusts1$n, na.color = "transparent")
  
 leaflet_trust <-  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    htmlwidgets::onRender("
    function(el, x) {
      L.control.zoom({ position: 'topright' }).addTo(this);
    }
  ") %>%
    addTiles() %>%
    addPolygons(
      data = trusts1,
      stroke = TRUE,
      color = "transparent",
      weight = 1,
      opacity = 1,
      fill = TRUE,
      fillColor = ~pal(n),
      fillOpacity = 0.7,
      label = ~lapply(
        paste0(
          "<b> ", TrustCode, "</b><br>",
          "<b>n:</b> ", format(n, big.mark = ",")
        ),
        htmltools::HTML
      ),
      labelOptions = labelOptions(
        direction = "auto",
        sticky = TRUE,
        textsize = "13px",
        offset = c(5, -5)#,
        # style = list(
        #   "background-color" = "rgba(255,255,255,0.9)",
        #   "border" = "1px solid grey",
        #   "border-radius" = "4px",
        #   "padding" = "4px"
        # )
      ),
      highlight = highlightOptions(
        weight = 2,
        color = '#555',
        fillOpacity = 0.8,
        bringToFront = TRUE
      )
    ) %>%
    setView(lng = -6.988054, lat = 54.60701, zoom = 7)
}

# 
lrg_leaflet <- leaflet(width='100vw',height='100vh',
        #options = leafletOptions(zoomControl = FALSE)
) %>%
  # htmlwidgets::onRender("function(el, x) {
  #   L.control.zoom({ position: 'bottomright' }).addTo(this)}") |>
  addTiles() |>
  setView(lng = -5.9576, lat = 54.904, zoom = 8) |>
  # addMarkers(lng = -0.1276, lat = 51.5074, popup = "London") |>

  addCircles(data = parks,
             weight = 15,
             # radius = 150,
             fillOpacity = 1,
             fillColor  = 'mediumseagreen',
             fill = F,
             opacity=0.5,
             color = 'mediumseagreen',
             stroke = T,
             label = ~name#,

             #popup = ~as.character(name)
  ) |>
  addCircles(data = fast_food,
             weight = 15,
             fillOpacity = 1,
             fillColor  = 'steelblue',
             fill = F,
             opacity=0.5,
             color = 'steelblue',
             stroke = T,
             label = ~name) |>
  addLegend(position = 'bottomright',
            colors = c('mediumseagreen','steelblue'),
            labels = c('Parks','Fast Food Outlets'),
            opacity = 1)


########################################
########################################
BMI_parallel_chart
overweight_obese_sex
bmi_sya_age
# obesity_effects_sunburst
# obesity_effects_treemap

bar_map_morph
trust_bar
trusts_map
########################################
########################################

# ---- example usage ----
# df must be long: country, x, value
# df <- your_data
# slope_like_chart(df, highlight = c("Germany","France","Italy","Spain","UK"))

# -----------------------------------------------------------
# Click-down hierarchy of obesity-related disease (Sunburst)
# -----------------------------------------------------------
# library(dplyr)
# library(tibble)
# library(echarts4r)
# 
# # 1) Define the hierarchy (replace `value` with your own metrics)
# hier <- tribble(
#   ~item,                      ~parent,                      ~value,
#   "Obesity-related disease",  '',                           1,
#   # Cardiovascular
#   "Cardiovascular disease",   "Obesity-related disease",    100,
#   "Cardiology",               "Cardiovascular disease",     40,    # CHD, HF, AF, HTN
#   "Cerebrovascular",          "Cardiovascular disease",     25,    # Stroke, TIA
#   "Peripheral vascular",      "Cardiovascular disease",     15,    # PAD
#   "Other CVD",                "Cardiovascular disease",     20,
#   # Metabolic
#   "Metabolic",                "Obesity-related disease",    80,
#   "Type 2 diabetes",          "Metabolic",                  50,
#   "Chronic kidney disease",   "Metabolic",                  30,    # CKD
#   # Respiratory
#   "Respiratory",              "Obesity-related disease",    55,
#   "Obstructive sleep apnoea", "Respiratory",                30,
#   "Asthma (worse control)",   "Respiratory",                25,
#   # Musculoskeletal
#   "Musculoskeletal",          "Obesity-related disease",    40,
#   "Osteoarthritis",           "Musculoskeletal",            40,
#   # Gastro-hepatic
#   "Gastro-hepatic",           "Obesity-related disease",    45,
#   "NAFLD/NASH",               "Gastro-hepatic",             30,
#   "GERD",                     "Gastro-hepatic",             15,
#   # Cancer (illustrative)
#   "Cancer",                   "Obesity-related disease",    60,
#   "Breast",                   "Cancer",                     15,
#   "Colorectal",               "Cancer",                     15,
#   "Endometrial",              "Cancer",                     10,
#   "Pancreatic",               "Cancer",                     10,
#   "Other cancers",            "Cancer",                     10
# )
# 
# 
# # 2) Build the interactive Sunburst
# #    - Click a segment to drill down (zoom)
# #    - Click the center to zoom back out
# 
# 
# 
# x <- pop |>
#   count(HSCT=first(HSCT),DEA2014_name=first(DEA2014_name),soa_name)
# 

xx0 <- pop |>
  group_by(item = HSCT) %>% 
  summarise(parent = 'NI', 
            value = sum(bmi%in% c('overweight','obese'))/n(),
            )  %>% 
  add_count(parent,wt = value,name='tot') %>% 
  mutate(value = value/tot) %>% 
  select(-tot)

xx1 <- pop |>
  group_by(item = DEA2014_name) %>% 
  summarise(  parent = first(HSCT),value = sum(bmi%in% c('overweight','obese'))/n()
              ) %>% 
  add_count(parent,wt = value,name='tot') %>% 
  mutate(value = value/tot) %>% 
  select(-tot)
  

xx2 <- pop |>
  group_by( item = sdz_name) |>
           summarise( parent = first(DEA2014_name),
                      value = sum(bmi %in% c('obese','overweight'))/n() )  %>% 
  add_count(parent,wt = value,name='tot') %>% 
  mutate(value = value/tot) %>% 
  select(-tot)

xni <- pop %>% summarise(item = "NI",  parent = '',value = sum(bmi%in% c('overweight','obese'))/n()) 

xx_normalised <- rbind(
xni,
xx0,
xx1,
xx2 %>% arrange(desc(value)) %>% head(20)
) %>% select(parent,item,value)
# 

geo_normalised <- data.tree::FromDataFrameNetwork(xx_normalised)
# 
# #add decal
# 
# (
# geo_sunburst <- geo_normalised |>
#   e_charts(value#height = '100%', width = '100%'
#            ) |>
#   e_sunburst() |>
#   e_theme('roma') |>
#   e_labels(show = FALSE) |>
#   e_tooltip(formatter = e_tooltip_pie_formatter('percent',digits=1)) %>%
#   e_visual_map( min=min(xx_normalised$value),max=max(xx_normalised$value) )|>
#     # e_aria(enabled = T,
#     #        decal = list(show =T)) %>% 
#   e_grid(left='15%',  containLabel = F )
# )
# 
# (
#   geo_treemap <- geo_normalised |>
#     e_charts(x = value#height = '100%', width = '100%'
#     ) |>
#     e_treemap(roam=F , 
#               upperLabel = list(show=T),
#               leafDepth = 2) |>
#     e_theme('roma')  |>
#     e_labels(show = T, position='insidetop') |>
#     e_tooltip(formatter = e_tooltip_pie_formatter('percent',digits=1)) %>%
#     e_tooltip(formatter= '{a}{b}{c}{d}{e}') %>% 
#     e_visual_map( min=min(xx_normalised$value),max=max(xx_normalised$value) )|>
#     e_aria(enabled = T,
#            decal = list(show =T)) %>%
#     e_grid(left='15%',  containLabel = F )
# )
########################################################################################
########################################################################################

x0 <- pop |>
  group_by(item = HSCT) %>% 
  summarise(parent = 'NI', 
            value = sum(bmi%in% c('overweight','obese')),
  )%>% 
  mutate(value = value * pop_scale_up)

x1 <- pop |>
  group_by(item = DEA2014_name) %>% 
  summarise(  parent = first(HSCT),
              value = sum(bmi%in% c('overweight','obese'))
  ) %>% 
  mutate(value = value * pop_scale_up)

x2 <- pop |>
  group_by(item = soa_name) |>
  summarise( parent = first(DEA2014_name), 
    value = sum(bmi %in% c('obese','overweight')) ) %>% 
  mutate(value = value * pop_scale_up)

x3 <- pop |>
  group_by( item = sa_code) |>
  summarise(  parent = first(soa_name),
    value = sum(bmi %in% c('obese','overweight')) ) %>% 
  mutate(value = value * pop_scale_up)

ni <- pop %>% summarise(  item = "NI", parent = '',value = sum(bmi%in% c('overweight','obese'))) %>% 
  mutate(value = value * pop_scale_up)

xx_count <- rbind(
  ni,
  x0,
  x1,#[1:20,],
  x2[1:500,]
  # x3[1:20,]
  )

geo_count <- data.tree::FromDataFrameNetwork(xx_count)

(
geo_treemap <- geo_count |>
  e_charts(
     height = '100%', width = '100%'
    ) |>
  e_treemap(roam=F , 
            upperLabel = list(show=T),
            leafDepth = 1) |>
  e_theme('roma')  |>
  e_labels(show = T, position='insidetop') |>
  e_tooltip() %>%
  e_visual_map(left=0,
               min=min(xx_count$value), 
               max=sort(xx_count$value,decreasing = T)[2])|>
  e_grid(left='25%',  containLabel = F )
  )


(
  geo_sunburst <- geo_count |>
    e_charts(emphasis = list(focus = 'relative'),
      height = '100%', width = '100%'
    ) |>
    e_sunburst(roam=F , 
              upperLabel = list(show=T),
              leafDepth = 1) |>
    e_theme('roma')  |>
    e_labels(show = F, position='insidetop') |>
    e_tooltip() %>%
    e_visual_map(left=0,
                 min=min(xx_count$value), 
                 max=sort(xx_count$value,decreasing = T)[2])|>
    e_grid(left='5%',  containLabel = F )
)


save(list = c(
  'geo_sunburst',
  'geo_treemap',
  'overweight_obese_sex',
  'BMI_parallel_chart',
  'slope_chart',
  'slope_chart_age_deprivation',
  'slope_chart_age_sex',
  'bmi_sya_age',
  'deprivation_sya_age',
  'trust_bar',
  'trusts_map',
  'bar_map_morph',
  'lrg_leaflet'
  ),
  file = './preprocess/infographics.RData')


# sapply(list(
#     geo_sunburst,
#     geo_treemap,
#     overweight_obese_sex,
#     BMI_parallel_chart,
#     slope_chart,
#     slope_chart_age_deprivation,
#     slope_chart_age_sex,
#     bmi_sya_age,
#     deprivation_sya_age,
#     trust_bar,
#     trusts_map,
#     bar_map_morph,
#     lrg_leaflet
#   
#   ),
#   function(x){print(object.size(x))}
# )

# 
# universe <- data.tree::FromDataFrameNetwork(hier)
# 
# # create a tree object
# 
# obesity_effects_sunburst <- universe |> 
#   e_charts() |> 
#   e_sunburst(    universalTransition = TRUE,
#                  animationDurationUpdate = 2000L) |> 
#   e_theme('london')|>
#   e_grid(left='15%',  containLabel = T )
# 
# obesity_effects_treemap <- universe |> 
#   e_charts() |> 
#   e_treemap(    universalTransition = TRUE,
#                 animationDurationUpdate = 2000L,
#                 roam = F,
#                 upperLabel = list(
#     show = F,
#     height = 30,
#     color='grey',
#     # backgroundColor='black',
#     opacity=1),
#     itemStyle = list(
#       borderColor = '#fff'
#     )
#   ) |> 
#   e_title("Treemap chart") |> 
#   e_tooltip() |> 
#   e_theme('london')|>
#   e_grid(left='15%',  containLabel = T )
# 
# cb <- "() => {
#   let x = 0;
#   setInterval(() => {
#     x++
#     chart.setOption(opts[], true);
#   }, 5000);
# }"
# 
# e_morph(obesity_effects_sunburst, obesity_effects_treemap, callback = cb)
# 
# rm( list =c('x','x0','x1','x2','xx','geo','universe'))