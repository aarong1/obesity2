library(tidyverse)
library(geofacet)
library(echarts4r)
library(sf)
library(fst)

lookups <- read.fst("data/dz_federation.fst")

pop <- pop %>% 
  left_join(lookups, by =c('dz_id' = 'DZ2021_cd'))
  
fed_poly <- read_sf('./data/shapefiles/simp.geojson')

fed_hsct_lookup <- fed_poly %>% 
  st_drop_geometry()


geo_pc <- tribble(
  ~code, ~row,  ~col,   ~name_Federation, 
  1,    3+1,   2+1,  'Armagh & Dungannon', 
  2,    3+1,   3+1,          'Craigavon', 
  3,    3,   4,            'Lisburn', 
  4,    3+2,   5,   'Newry & District', 
  5,    3+1,   6,               'Down', 
  6,    3,   7,               'Ards', 
  7,    2+1,   1+1,         'South West', 
  8,    2,   2+1,         'Mid-Ulster', 
  9,    3,   5,     'South Belfast', 
  10,   2,   5,       'West Belfast', 
  11,   2,   6,       'East Belfast', 
  12,   3,   6,         'North Down', 
  13,   1,   2,              'Derry', 
  14,   1,   3,           'Causeway', 
  15,   0,   4,     'Antrim Ballymena', 
  16,   1,   5,    'North Belfast', 
  17,   1,   6,        'East Antrim' 
) %>% 
  mutate(name = name_Federation) %>% 
  mutate(row = row + 1) %>% 
  mutate(col = col - 1) %>% 
  left_join(fed_hsct_lookup, by = c('name_Federation' = 'Federation'))

df <- data.frame(name_Federation=geo_pc$name_Federation,
                 HSCT = geo_pc$HSCT,
                 y=runif(17*2),gp =c('a','b')) %>%
  mutate(name = name_Federation)

df <- pop %>% count(Federation, bmi) %>% 
  mutate(n = n*pop_scale_up) %>% 
  filter_out(is.na(bmi))

(
INT_bmi_geo_matrix <- df |>
  filter_out(bmi=='normal') %>% 
  group_by(Federation ) |>
    # mutate(bmi = recode_values(bmi, 'overweight' ~ 'OWeight', 'obese' ~ 'Obese')) |>
  e_chart(bmi, emphasis = list(focus = 'series'),bind = 'HSCT',
          width = '100%', height = '100%') |>
  e_bar(n, symbol = "none", legend = T#bind = gp
  ) |>
  
  # e_flip_coords() %>%
  e_visual_map(serie = n, show=F,type = 'continuous') %>%
  e_group('r') %>%
  e_connect_group('r') %>%
  e_x_axis(splitNumber = 2, axisLabel=list(textStyle = list(fontSize = 9, rotate = -0.2 ))) |>
  e_y_axis(splitNumber = 2,axisLabel=list(textStyle = list(fontSize = 9, rotate = -0.2 ))) |>
  # e_matrix(cols = 6, rows = 6) #%>%
  e_geoFacet(legend_pos = 'top', legend = TRUE, #cols = 7, rows = 6,
             body = list(itemStyle=list(borderWidth=0,borderColor='white',color='white')),
             
             backgroundStyle = list(borderWidth=0,borderColor='white',color='white'),
             dividerLineStyle = list( borderColor='white',color = 'white'),
             x = list(show = F),
             grid = geo_pc,
             margin_trbl = c("t"="50%"),
             left = "5%",
             # top='50%',
             width = "90%") |>
  e_title(text = "Integrated Neighbourhood Teams: Northern Ireland") |>
  e_tooltip(trigger = "axis") %>% 
  e_title_matrix(top='20%', textStyle = list(fontSize = 10) ) %>%
  # e_legend() %>%
  e_theme('walden') #%>% 
# e_matrix_raw(rows = 5, cols = 8)
)

# json <- jsonlite::read_json("https://raw.githubusercontent.com/shawnbot/topogram/master/data/us-states.geojson")

fed_json <- jsonlite::read_json('./data/shapefiles/simp.geojson')

# x <- st_simplify( st_make_valid(fed_poly),preserveTopology =  T, dTolerance = 100) %>% 
#   st_make_valid()
# 
# file.remove('simp.geojson')
# st_write(x,'simp.geojson',append = FALSE)
# fed_json <- jsonlite::read_json("./simp.geojson")

(map_bmi_int <- pop %>% 
    group_by(Federation) %>% 
    summarise(bmi = sum(bmi %in% c('overweight','obese')), n=n()) %>%     # group_by(HSCT) %>%
    # mutate(z=runif(n())*100) %>% 
    mutate(z=bmi*pop_scale_up) %>% 
    # group_by(HSCT) %>% 
    # tibble::rownames_to_column("states") |> 
    e_charts(Federation) |>
    e_legend() %>% 
    e_map_register("ni", fed_json) |>
    e_map(z, map = "ni",
          bind = 'Federation', 
          reorder = TRUE,
          name='Federation',
          roam=F,
          itemStyle=list(
             color = 'lightblue',
          borderColor ='white',
          borderWidth= 1
          ),
          # label=list(show=FALSE),
          emphasis = list(
          focus = 'series',
          itemStyle = list(
             backgroundColor = 'green',
             opacity=1)
          ),
          nameProperty = "Federation"
    ) %>%
    # e_visual_map(serie = z) %>%
    e_visual_map( #type = 'piecewise',
      # categories = c('BHSCT','WHSCT','SEHSCT','SHSCT','NHSCT'),
      serie = z) %>%
    e_group('group') %>% 
    e_tooltip(trigger = "item",
              triggerOn = "mousemove|click",
              formatter = htmlwidgets::JS("
  function(params) {
    return `Area: ${params.name} <br> Overweight or Obese: ${Math.round(params.value)}`;
  }")) |>
    e_theme('auritus')
)


(bar_overweight <- pop %>% 
    group_by(Federation) %>% 
    summarise(overweight = sum(bmi %in% c('overweight')), n=n()) %>% 
    # group_by(HSCT) %>%
    # mutate(z=runif(n())*100) %>% 
    mutate(z=overweight * pop_scale_up) %>%
    # arrange(desc(z)) %>% 
    
    e_charts(Federation,
             reorder = TRUE,width = '100%' ,height = '100%',
             emphasis =list(focus ='series')
    )%>%
    e_bar(z,
          bind=Federation, 
          name = 'Overweight',
          legend = T,
          nameProperty = "Federation",
          emphasis = list(
            focus = 'series')
    ) |> 
   
    # e_grid(bottom='50%') %>%
    e_tooltip() %>% 
    e_flip_coords() %>% 
    e_theme('auritus') %>% 
    e_group('group') |> 
    e_connect_group('group')
)

(bar_obese <- pop %>% 
    group_by(Federation) %>% 
    summarise(obese = sum(bmi %in% c('obese')), n=n()) %>% 
    # group_by(HSCT) %>%
    # mutate(z=runif(n())*100) %>% 
    mutate(z=obese * pop_scale_up) %>%
    # arrange(desc(z)) %>% 
    
    e_charts(Federation,reorder = TRUE, width = '100%',height = '100%' ) %>%
    e_bar(z,
          bind=Federation, 
          name = 'Obese',
          legend = T,
          nameProperty = "Federation",
          emphasis = list(
            focus = 'series')
    ) |> 
   
    # e_grid(top=0) %>%
    e_tooltip() %>% 
    e_flip_coords() %>% 
    e_theme('walden') %>% 
    e_group('group') |> 
    e_connect_group('group')
)

(bar_each_bmi_normalised <- pop %>% 
    group_by(Federation) %>% 
    summarise(
              overweight = sum(bmi %in% c('overweight' )),
              obese = sum(bmi %in% c( 'obese')),
              unhealthy = sum(bmi %in% c('overweight', 'obese')),
              n=n()) %>% 
    
    mutate(overweight = overweight * pop_scale_up,
           obese = obese * pop_scale_up,
           unhealthy  = unhealthy * pop_scale_up,
           n =  n *pop_scale_up) |> 

    # mutate(z = bmi/ n ) %>%
    e_charts(Federation,reorder = TRUE, emphasis = list(focus='series'),
             ,width = '100%' ,height='100%') %>%
    
    #,width = '250px' ,height='450px') %>%
    e_bar(overweight , stack = 'Fed') %>%    
    e_bar(obese, stack = 'Fed') |> 
    e_line(unhealthy) %>% 
    e_tooltip() %>% 
    e_x_axis(splitNumber = 2, axisLabel=list(textStyle = list(fontSize = 9, rotate = -80.2 ))) %>%  
    e_grid(left='30%') %>% 
    e_legend(orient = 'vertical',left = 0, top='30%') %>% 
    
    # e_flip_coords() %>%
    e_theme('auritus') %>% 
    e_group('group') |> 
    e_connect_group('group')
  # plot_aesthetics()
)


(bar_bmi_normalised <- pop %>% 
    group_by(Federation) %>% 
    summarise(bmi = sum(bmi %in% c('overweight', 'obese')),
              overweight = sum(bmi %in% c('overweight' )),
              obese = sum(bmi %in% c( 'obese')),
              n=n()) %>% 
    # group_by(HSCT) %>%
    # mutate(z=runif(n())*100) %>% 
    mutate(z = bmi/ n ) %>%
    # arrange(desc(z)) %>% 
    e_charts(Federation,reorder = TRUE,
             emphasis = list(focus ='series'),
             width = '100%' ,height='100%'
             ) %>%
   
    e_bar(z,
          bind=Federation, 
          name = 'Unhealthy BMI',
          legend = T,
          nameProperty = "Federation",
          # itemStyle = list(
          #    opacity=0.2),
          emphasis = list(
            focus = 'series')
          # itemStyle = list(
          #    color = 'lightblue',
          #    opacity=1)),
          # symbol_size=8
    ) |> e_tooltip() %>% 
    # e_flip_coords() %>%
    
    # e_visual_map(serie = Federation, type = 'piecewise',
    #             categories = c('Antrim Ballymena',
    #             'Ards', 'Armagh & Dungannon',        'Causeway',      'Craigavon',            'Derry',           'Down',     'East Antrim',    'East Belfast',        'Lisburn',     'Mid-Ulster', 'Newry & District',  'North Belfast',     'North Down', 'South Belfast',     'South West',    'West Belfast')) %>% 
    
    # e_labels() |> 
    # e_title("Population by Deprivation") %>%
    
    #   e_tooltip(trigger = "item", 
    #             triggerOn = "mousemove|click", 
    #             alwaysShowContent = FALSE,
    #             formatter = htmlwidgets::JS("
    #   function(params) {
    #   return `Area: ${params.name} <br> Population: ${Math.round(params.value[1])} <br> MDM Rank: ${Math.round(params.value[0])}`;
    #   }
    # ")) |>  
    # e_tooltip(formatter = e_tooltip_item_formatter(style = "percent", digits = 0)) %>%
    e_tooltip(formatter = JS('function(params) { return `${params.marker}  ${params.name} <br> 
                            Risk: ${Math.round(params.data.value[1]*1000)/10}% `; }')) %>%
    
    e_y_axis(formatter = e_axis_formatter(style = "percent", digits = 0)) %>%
    
    e_theme('auritus') %>% 
    e_group('group') |> 
    e_connect_group('group')
  # plot_aesthetics()
)

# e_arrange(map_bmi_int ,
#           bar_overweight ,
#           bar_obese ,
#           bar_each_bmi_normalised ,
#           bar_bmi_normalised)

div(class = 'd-flex flex-row flex-wrap',div(style = 'width:300px;height:300px;',map_bmi_int) ,
    div(style = 'width:300px;height:300px;',bar_overweight) ,
    div(style = 'width:300px;height:300px;',bar_obese) ,
    div(style = 'width:300px;height:300px;',bar_each_bmi_normalised) ,
    div(style = 'width:300px;height:300px;',bar_bmi_normalised)
    ) |> page_fluid() |>  browsable()

(
BMI_cmms_plot <- pop %>% 
  group_by(bmi) %>% 
  summarise(cmms = mean(cmms)) %>% 
  filter_out(is.na(bmi)) %>% 
  # group_by(bmi) %>% 
  e_charts(bmi) %>% 
  e_bar(cmms, bind = bmi, name = 'CMMS', 
        legend = T) %>% 
  e_theme('walden') #%>% 
  # e_tooltip() %>% 
  # e_tooltip(formatter = e_tooltip_item_formatter(style = "percent", digits = 0)) %>%
  # e_y_axis(formatter = e_axis_formatter("percent", digits = 1))
)


(
  federation_bmi_hsct_beeswarm <- pop %>% 
    group_by(sdz_code,Federation,HSCT) %>%
    summarise(bmi = sum(bmi %in% c('overweight', 'obese')),
              town = mean(custom_townsend_score_dz ),
              n = n()) %>% 
    mutate(z = bmi/n) %>%
    group_by(Federation) %>% 
    e_chart(HSCT, height ='100%', width = '100%',
            emphasis = list(focus = 'series'),
            dimension = 'sdz_code',
            labelLayout = list(
              x = "55%",
              moveOverlap = "shiftY"
            )) %>% 
    e_scatter(z, symbol_size = 5,
              tooltip = list(valueFormatter = JS(" (value) => Math.round(value*100)+'%'")),
              #formatter = e_tooltip_item_formatter('percent')
              labelLayout = list(
                verticalAlign = 'middle',
                
                x = "55%",
                moveOverlap = "shiftY"
              )
    ) %>% 
    e_x_axis( jitter = 60, jitterOverlap = FALSE) %>% 
    e_y_axis( formatter = e_axis_formatter(style = 'percent')) %>% 
    
    # e_mark_point(data = list(type = 'mean', name = 'Max'), symbolSize = 10) %>%
    e_mark_line(data = list(type = 'average', name = 'Average', 
                            tooltip = list(valueFormatter = JS(" (value) => Math.round(value*100)+'%'")),
                            
                            jitter =10,
                            label=list(formatter = '{a}',
                                       labelLayout = list(
                                         verticalAlign = 'middle',
                                         
                                         x = "55%",
                                         moveOverlap = "shiftY"
                                       )),
                            labelLayout = list(
                              verticalAlign = 'middle',
                              x = "55%",
                              moveOverlap = "shiftY"
                            ),
                            emphasis = list(
                              focus = "self"
                            )#,
                            #label= list(show=F)
    )) %>%
    # e_legend(show = T, type = 'scroll') %>% 
    e_grid(right = '30%',bottom = '20%') |> 
    e_tooltip() |> 
    # e_tooltip(formatter = e_tooltip_item_formatter('percent')) %>%
    e_theme('walden')
)

(
  federation_town_hsct_beeswarm <- pop %>% 
    group_by(sdz_code,Federation,HSCT) %>%
    summarise(bmi = sum(bmi %in% c('overweight', 'obese')),
              town = mean(custom_townsend_score_dz ),
              n = n()) %>% 
    mutate(z = town) %>%
    group_by(Federation) %>% 
    e_chart(HSCT, height ='100%', width = '100%',
            emphasis = list(focus = 'series'),
            dimension = 'sdz_code',
            labelLayout = list(
              x = "55%",
              moveOverlap = "shiftY"
            )) %>% 
    e_scatter(z, symbol_size = 5,
              labelLayout = list(
                # verticalAlign = 'middle',
                # x = "55%",
                moveOverlap = "shiftY"
              )
    ) %>% 
    e_grid(right = '30%',bottom = '20%') |> 
    e_x_axis( jitter = 60, jitterOverlap = FALSE) %>% 
    # e_mark_point(data = list(type = 'mean', name = 'Max'), symbolSize = 10) %>%
    e_mark_line(data = list(type = 'average', name = 'Average', 
                            jitter =10,
                            label=list(formatter = '{a}',
                                       labelLayout = list(
                                         # verticalAlign = 'middle',
                                         
                                         # x = "55%",
                                         moverOverlap = "shiftY"
                                       )),
                            labelLayout = list(
                              verticalAlign = 'middle',
                              x = "55%",
                              moveOverlap = "shiftY"
                            ),
                            emphasis = list(
                              focus = "self"
                            )#,
                            #label= list(show=F)
    )) %>%
    e_legend(show = F) %>% 
    e_tooltip() %>% 
    e_theme('walden')
)






save(list = c(
  
  'INT_bmi_geo_matrix',
  'map_bmi_int',
  'bar_overweight',
  'bar_obese',
  'bar_each_bmi_normalised',
  'bar_bmi_normalised',
  'BMI_cmms_plot',
  'federation_bmi_hsct_beeswarm',
  'federation_town_hsct_beeswarm'
),
file = './preprocess/int.RData')

