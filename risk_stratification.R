  library(tidyverse)
#source('app_prep.R')
  
141884896/834792 #169
  
( risk_prob_density_fn <-  pop |>
    filter(qrisk_score>=0.01) |>
    # filter(qrisk_score<0.3) |>
    filter(!is.na(bmi)) |>
    # group_by(age10) |>
    select(bmi,qrisk_score) |> 
    e_charts(timeline = F,emphasis = list(focus = 'self')) |> 
    # e_histogram(qrisk_score) #|>
    e_density(qrisk_score, 
              breaks = 20,
              name = "1yr risk of CVD",
              #stack='stack',
              areaStyle = list(opacity = 0.9), 
              #y_index = 1
    ) |> 
    e_tooltip() %>% 
   e_grid(containLabel = T)
  )

146690248/1144912 #128

 (
   risk_prob_density_fn_age10 <- pop |>
  filter(qrisk_score>=0.01) |>
  # filter(qrisk_score<0.3) |>
  
  filter(!is.na(bmi)) |> 
  group_by(age10) |>
  select(bmi,qrisk_score) |> 
     
  e_charts(timeline = F,emphasis = list(focus = 'self')) |> 
  # e_histogram(qrisk_score) #|>
  e_density(qrisk_score, 
            # breaks = ,
            # name = "density",
            #stack='stack',
            areaStyle = list(opacity = 0.9), 
            lineStyle = list(opacity = 0.1), 
            symbol = 'none'
            #y_index = 1
            ) |> 
   e_legend( selector = TRUE) %>%
  e_tooltip(show=T) %>% 
   e_grid(containLabel = T) %>% 
   e_theme('walden')
)
  
(  
 risk_prob_density_fn_bmi<- pop |>
   filter(qrisk_score>=0.02) |>
   # filter(qrisk_score<0.3) |>
   filter(!is.na(bmi)) |> 
   group_by(bmi) |>
    select(bmi,qrisk_score) |> 
   e_charts(timeline = F,emphasis = list(focus = 'self')) |> 
   # e_histogram(qrisk_score) #|>
   e_density(qrisk_score, 
             breaks = ,
             # name = "density",
             #stack='stack',
             symbol='none',
             areaStyle = list(opacity = 0.5), 
             lineStyle = list(opacity = 0.1), 
             
             #y_index = 1
   ) |> 
   e_tooltip() %>% 
   e_grid(containLabel = T) %>% 
   e_theme('walden')
 )
  
  (
zoomout_risk_age_bmi <- pop |>
  filter(qrisk_score>=0.1) |>
  filter(qrisk_score<0.4) |>
  
  filter(!is.na(bmi)) |> 
  group_by(paste(age20,bmi)) |>
  e_charts(timeline = F) |> 
  # e_histogram(qrisk_score) #|>
  e_density(qrisk_score, breaks = 10,
              # name = "density",
              # stack='stack',
              areaStyle = list(opacity = 0)
              #y_index = 1
  ) |> 
  e_title() |> 
  e_tooltip() %>% 
  e_grid(containLabel = T)
)
  
  (
zoomin_risk_age_bmi <- pop |>
  filter(qrisk_score>=0.02) |>
  filter(qrisk_score<0.3) |>
  
  filter(!is.na(bmi)) |> 
  group_by(paste(age20,bmi)) |>
  e_charts(timeline = F) |> 
  # e_histogram(qrisk_score) #|>
  e_density(qrisk_score, breaks = 5,
            # name = "density",
            # stack='stack',
            areaStyle = list(opacity = 0)
            #y_index = 1
  ) |> 
  e_title() |> 
  e_tooltip() %>% 
  e_grid(containLabel = T)
)

(
comorbidities_curve_wo_0_or_1 <- pop |> 
    select(age10,
           multimorbidity) |> 
  group_by(age10) |>
  filter(multimorbidity>1) |> 
  e_charts() |>
  e_density(breaks = 8,
            multimorbidity, 
            # name = "density",
            # stack = 'stack',
            areaStyle = list(opacity = .4), 
            smooth = TRUE, y_index = 1) |>
  e_tooltip() %>% 
  e_grid(containLabel = T)
)

  
(
comorbidities_age <- pop |> 
    select(age20,
           multimorbidity) |> 
  group_by(age20) |>
  e_charts(,emphasis = list(focus = 'self')) |>
  e_density(breaks = 7,
            multimorbidity, 
            # name = "density",
            # stack = 'stack',
            symbol='none',
            areaStyle = list(opacity = .6), 
            smooth = TRUE, y_index = 1) |>
  e_tooltip() %>% 
  e_grid(containLabel = T) %>% 
    e_y_axis(name = 'avg comorbidities') %>% 
  e_theme('walden')
  )
  
(
comorbidities_bmi <- pop |> 
    select(bmi,
           multimorbidity) |> 
  filter(!is.na(bmi)) |> 
  group_by(bmi) |>
  e_charts() |>
  e_boxplot(breaks = 1,
            multimorbidity, 
            name = "density",
            # stack = 'stack',
            areaStyle = list(color='black',opacity = .9), 
            smooth = TRUE, 
            y_index = 1) |>
  e_flip_coords() |> 
  e_tooltip() |> 
  e_theme('walden') %>% 
  e_grid(containLabel = T)
)
  
 ( 
 cmms_bmi <-  pop %>% 
    group_by(bmi) %>% 
    summarise(cmms = mean(cmms)) %>% 
    filter_out(is.na(bmi)) %>% 
    # group_by(bmi) %>% 
    e_charts(bmi) %>% 
    e_bar(cmms, bind = bmi, name = 'CMMS', 
          legend = T) %>% 
    e_theme('walden') %>% 
    # e_tooltip() %>% 
    e_tooltip(formatter = e_tooltip_item_formatter(style = "percent", digits = 0)) %>%
    e_y_axis(formatter = e_axis_formatter("percent", digits = 1))
  )
  
reduced_pop <- pop |>
  mutate(percentile = rank(qrisk_score)/max(rank(qrisk_score))) |> 
  ungroup() |> 
  sample_frac(0.1) 

reduced_pop <- reduced_pop |> 
  mutate(percentile = rank(qrisk_score)/max(rank(qrisk_score))) 

(
risk_bmi <- reduced_pop |> 
    select(bmi,qrisk_score) |> 
    group_by(bmi) |>
  filter(!is.na(bmi)) |> 
  e_charts() |> 
  e_boxplot(qrisk_score,outliers = T  ) |> 
  # e_color(color = c('orange','red','blue','green')) |>
  e_flip_coords() |> 
  e_grid(left='20%') |> 
  e_theme('roma') %>% 
    e_tooltip() |> 
  e_grid(containLabel = T)
)

(
  deprivation_risk_by_age20_chart <- reduced_pop |> 
  slice_sample (prop=0.1) %>%
  filter(qrisk_score>0.00) |>
  filter_out(is.na(age20) |is.na(qrisk_score)| is.na(mdm_rank)) |>
    # filter(age20 == '20-40') |>
    group_by(age20, soa_code) |> 
    summarise(qrisk_score = mean(qrisk_score),mdm_rank=mean(mdm_rank)) |> 
  group_by(age20) |>
  mutate(mdm_rank = as.numeric(mdm_rank)) %>% 
  e_charts(mdm_rank,emphasis = list(focus = 'series')) |>
  e_scatter(qrisk_score) |>
    # e_lm( mdm_rank ~ qrisk_score )
    # e_lm( qrisk_score ~ mdm_rank  ) %>% 
  e_lm(name = c('20-40','40-60','60-80','80-100'),
       itemStyle = list(opacity=0.5),
    # name = unique(reduced_pop$age20),
       # legend = T,
       formula =  qrisk_score ~ mdm_rank  ) |>
  # e_grid(containLabel = T) #%>%
  e_theme('azul')  %>% 
  e_tooltip(#backgroundColor = 'white',
            trigger='axis',
            formatter = e_tooltip_pointer_formatter(style = 'percent',digits=2)
            ) %>%
    # e_tooltip(backgroundColor = 'white') %>% 
    e_axis( axis = 'y', formatter = e_axis_formatter('percent')) 
)

(
deprivation_risk_by_bmi_chart <- reduced_pop |> 
  filter(!is.na(bmi)) |> 
  group_by(bmi,soa_code) |> 
  summarise(qrisk_score = mean(qrisk_score),mdm_rank=mean(mdm_rank)) |> 
  e_charts(mdm_rank,emphasis = list(focus = 'series')) |>
  e_scatter(qrisk_score) |>
  e_lm(name = rev(c('obese','overweight','normal'  )),
       # itemStyle = list(opacity=0.5),symbol='square',
    # legend = T,
    formula =   qrisk_score~mdm_rank)  %>% 
  e_grid(containLabel = T)%>% 
  e_theme('azul') %>% 
    e_tooltip(backgroundColor = 'white',
      trigger='axis',
      formatter = e_tooltip_pointer_formatter(style = 'percent',digits=2)
    ) %>%    e_axis( axis = 'y', formatter = e_axis_formatter('percent')) 
    
  )
  
  


#dumbbell plot echarts -----

(
df <- reduced_pop |> 
  # mutate(townsend_half = (custom_townsend_score_dz <max(custom_townsend_score_dz)/2)) |> 
  mutate(townsend_ends =percent_rank(custom_townsend_score_dz)) |> 
  # filter(townsend_ends <= 0.2| townsend_ends >= 0.8) |>
  mutate(townsend_ends = ifelse(custom_townsend_score_dz <max(custom_townsend_score_dz)/2,"Top Quintile", "Bottom Quintile")) |>
  
    group_by(townsend_ends,bmi) |>
  filter(!is.na(bmi)) |> 
    summarise(multimorbidity = mean(multimorbidity))
)

(
comorbidities_bmi_townsend_extreme <- df |> 
  group_by(townsend_ends,bmi) |> 
  # mutate(sym=ifelse(townsend_ends,'circle','square')) |>
  e_charts(bmi) |>
  e_line(multimorbidity, symbolSize = 20, symbol = 'circle') |> 
  e_tooltip() |> 
  echarts4r::e_flip_coords() |>
  # e_x_axis(min=3) |> 
    e_grid(containLabel = T) |> 
  e_theme('walden') 
)

df_heatmap <- pop |>
  transmute(id,
            Overweight  = bmi%in%c('obese','overweight'),
            `Poor Diet`  = diet!='meets_5_a_day',
            `Lacks Activity`  = pa=='inactive',
            Smokes  = smoking == 'current_smoker',
            Drinks = alcohol %in% c('increased_risk', 'higher_risk'),
            Cholesterol  = (cholesterol_status == 'normal_cholesterol' ),
            `Atrial Fibrillation`  = (atrial_fibrillation == 'af'),
            `Kidney Disease`  = (ckd_status == 'ckd'),
            Hypertension  = (hypertension_status != 'normotensive_untreated'),
            Diabetes  = (diabetes_status != 'no_diabetes'),
            `Peripheral Arterial Disease`  = (pad_status == 'pad')
  ) |> 
  pivot_longer(-id) |> 
  filter(!is.na(value) & value == TRUE) %>%
  left_join(data.frame(
    name = c('Overweight',
             'Poor Diet',
             'Lacks Activity',
             'Smokes',
             'Drinks',
             'Cholesterol',
             'Atrial Fibrillation',
             'Kidney Disease',
             'Hypertension',
             'Diabetes',
             'Peripheral Arterial Disease'),
    precedence = seq(1,11)
  ),by='name') %>%
  left_join(.,.,by='id') |> 
  count(name.x,name.y) %>% 
  mutate(n = n *pop_scale_up)


(
  heatmap_risk_factors <- df_heatmap |> 
    e_charts(name.x) |> 
    e_heatmap(name.y,z = n) |> 
    e_tooltip(trigger = 'item',formatter = '{a}{b}{c}{d}') |> 
    e_visual_map(n) |> 
    e_grid(containLabel = T) |> 
    e_y_axis(axisLabel = list(textStyle = list( color='black', fontSize = 12, fontWeight = 'normal'))) |> 
    e_x_axis(show = T,axisLabel = list(rotate = 40)) |> 
    e_title(text = 'Correlation of Risk factors') |>
    e_theme('walden') 
)

(
  reduced_heatmap_risk_factors <- df_heatmap |> 
    filter(name.y %in% c('Overweight',
                         'Poor Diet',
                         'Lacks Activity',
                         'Smokes',
                         'Drinks')) %>% 
    # e_flip_coords() %>% 
    e_charts(name.x) |> 
    e_heatmap(name.y,z = n) |> 
    e_tooltip(trigger = 'item',formatter = '{a}{b}{c}{d}') |> 
    e_visual_map(n) |> 
    e_grid(containLabel = T) |> 
    e_y_axis(axisLabel = list(textStyle = list( color='black', fontSize = 12, fontWeight = 'normal'))) |> 
    e_x_axis(show = T,axisLabel = list(rotate = 40)) |> 
    e_title(text = 'Correlation of Risk factors') |>
    e_theme('walden') 
)


edges_df <- pop |>
  transmute(id,
  Overweight  = bmi%in%c('obese','overweight'),
  `Poor Diet`  = diet!='meets_5_a_day',
  `Lacks Activity`  = pa!='meets_rec',
  Smokes  = smoking == 'current_smoker',
  Drinks = alcohol %in% c('increased_risk', 'higher_risk'),
  Cholesterol  = cholesterol_status!='normal_cholesterol',
  `Atrial Fibrillation`  = af_status=='af',
  `Kidney Disease`  = ckd_status=='ckd',
  Hypertension  = hypertension_status=='hypertension',
  Diabetes  = diabetes_status!= 'no_diabetes'
    ) |> 
    pivot_longer(-id) |> 
  filter(!is.na(value) & value == TRUE) %>%
  left_join(data.frame(
    name = c('Overweight',
             'Poor Diet',
             'Lacks Activity',
             'Smokes',
             'Drinks',
             'Cholesterol',
             'Atrial Fibrillation',
             'Kidney Disease',
             'Hypertension',
             'Diabetes'),
    precedence = seq(1,10)
  ),by='name') %>%
  left_join(.,.,by='id') |> 
  filter(precedence.x > precedence.y) |> 
  count(from = name.x,to = name.y)%>% 
  mutate(n = n *pop_scale_up)
  
  
  nodes = pop |>
    transmute(id,
              Overweight  = bmi%in%c('obese','overweight'),
              `Poor Diet`  = diet!='meets_5_a_day',
              `Lacks Activity`  = pa!='meets_rec',
              Smokes  = smoking == 'current_smoker',
              Drinks = alcohol %in% c('increased_risk', 'higher_risk'),
              Cholesterol  = cholesterol_status!='normal_cholesterol',
              `Atrial Fibrillation`  = af_status=='af',
              `Kidney Disease`  = ckd_status=='ckd',
              Hypertension  = hypertension_status=='hypertension',
              Diabetes  = diabetes_status!= 'no_diabetes'
    ) |> 
    pivot_longer(-id) |> 
    count(name, wt=value) %>% 
  mutate(n = n *pop_scale_up)|> 
    
    mutate(size = n/10000) |> 
    mutate(value = n,.keep = 'unused') |> 
    mutate(grp = row_number()) |> 
    mutate(grp = name) |> 
    group_by(grp)
  
  edges = edges_df |> 
    mutate(size = n/4000) |> 
     mutate(grp = from) |> 
    group_by(from)
  
(  
 risk_graph <-  e_charts(emphasis = list(focus = 'self'), #height='100%', width = '100%'
                         ) |> 
    e_graph(
       layout = "circular", 
      circular = list(
        rotateLabel = TRUE
      ),
      roam = F,
      lineStyle = list(
        color = "source",
        curveness = 0.1
      ),
      label = list(
        position = "right",
        formatter = "{a}{b}{c}"
      )
    ) |>
    e_graph_nodes(
      nodes = nodes, 
      names = name,
      value = value,
      size = size,
      legend = T,
      category = grp
    ) |> 
    e_tooltip(formatter = "{a}{b}{c}") |> 
    e_labels(show=FALSE,formatter = "") |>
   e_title(text = 'Risk Correlation') |>
    # e_legend(show=F) |> 
    # e_grid(containLabel = T) |> 
    e_graph_edges(
      edges = edges, 
      source = from,
      target = to,
      size = size) |> 
    # e_legend(show=F, bottom = 0, orient='horizontal') |> #,type ='scroll'
    # e_grid( top = '10%') |> 
    # e_theme('dark-digerati')
    e_theme('walden')  %>% 
    e_grid(containLabel = T)

  
)
  
  
  chord_data <- data.frame(
    source = c("a", "b", "c", "d", "c"),
    target = c("b", "c", "d", "e", "e"),
    value = ceiling(rnorm(5, 10, 1)),
    stringsAsFactors = FALSE
  )
  
  chord_data |>
    e_charts() |>
    e_chord(source, target, value, lineStyle = list(
      opacity= 0.3,
      color= 'gradient' # or 'source' (default), 'target'
)) %>% 
    e_tooltip() %>% 
    e_theme('walden')

  
  
  save(list = c(
  'risk_prob_density_fn',
  'risk_prob_density_fn_age10',
  'risk_prob_density_fn_bmi',
  # 'comorbidities_curve_wo_0_or_1',
  # 'comorbidities_age',
  # 'comorbidities_bmi',
  'cmms_bmi',
  'risk_bmi',
  'deprivation_risk_by_age20_chart',
  'deprivation_risk_by_bmi_chart',
  'comorbidities_bmi_townsend_extreme',
  'df_heatmap',
  'heatmap_risk_factors',
  'reduced_heatmap_risk_factors',
  'risk_graph'),
       file = './preprocess/risk_stratification.RData')
  
  
  # sapply(list(
  #   risk_prob_density_fn,
  #   risk_prob_density_fn_age10,
  #   risk_prob_density_fn_bmi,
  #   # comorbidities_curve_wo_0_or_1,
  #   # comorbidities_age,
  #   # comorbidities_bmi,
  #   cmms_bmi,
  #   risk_bmi,
  #   deprivation_risk_by_age20_chart,
  #   deprivation_risk_by_bmi_chart,
  #   comorbidities_bmi_townsend_extreme,
  #   df_heatmap,
  #   heatmap_risk_factors,
  #   reduced_heatmap_risk_factors,
  #   risk_graph),
  #   function(x){print(object.size(x))}
  # )
  
# reduced_pop |> 
#     filter(age>25) |> 
#     slice_sample(weight_by = (1-qrisk_percentile),prop = 0.1) |> 
#     group_by(bmi) |>
#     filter(!is.na(bmi)) |>
#     e_charts(height=290) |> 
#     e_density(qrisk_percentile,breaks=5) |>
#      
#     e_mark_line(title = 'Baseline',
#       data = list(
#       type = "average",
#       name = "Average"
#     )) |> 
#   e_theme('walden')
  
  
    # e_color(color = c('orange','red','blue','green')) |>
    # e_flip_coords()
  
######################################
######################################
######################################
  # risk_bmi
  # risk_prob_density_fn_age10 
  # heatmap_risk_factors
  # comorbidities_bmi_townsend_extreme
  # 
  # zoomout_risk_age_bmi
  # zoomin_risk_age_bmi
  # 
  # deprivation_risk_by_bmi_chart
  # comorbidities_curve_wo_0_or_1
  # comorbidities_age
  # comorbidities_bmi
  # risk_graph
  
######################################
######################################
######################################
  
  
  # helper: build transition nodes/edges from consecutive years per id
  # risk_transition_graph <- function(
    #   data = map_filtered_chart(),      # your filtered microsim
  #   id    = id,                       # individual id column
  #   time  = year,                     # time/order column
  #   risk  = qrisk_score,              # numeric risk column (0–1 or 0–100)
  #   breaks = seq(0, 0.4, by = 0.02)   # risk bins for nodes
  # ) {
  #   id   <- rlang::enquo(id)
  #   time <- rlang::enquo(time)
  #   risk <- rlang::enquo(risk)
  #   
  #   df <- data |>
  #     dplyr::select(!!id, !!time, !!risk) |>
  #     dplyr::filter(!is.na(!!risk), !is.na(!!time)) |>
  #     dplyr::mutate(
  #       .risk = dplyr::if_else(!!risk > 1, !!risk / 100, !!risk),      # 0–1
  #       .risk = pmin(pmax(.risk, min(breaks)), max(breaks)),           # clamp
  #       bin   = cut(.risk, breaks = breaks, right = FALSE,
  #                   include.lowest = TRUE),
  #       bin   = as.character(bin)
  #     )
  #   
  #   # nodes: how often each bin occurs overall
  #   nodes <- df |>
  #     dplyr::count(bin, name = "value") |>
  #     dplyr::mutate(
  #       name = bin,
  #       size = scales::rescale(value, to = c(8, 40))  # node size
  #     ) |>
  #     dplyr::select(name, value, size)
  #   
  #   # edges: consecutive transitions within each id ordered by time
  #   edges <- df |>
  #     dplyr::arrange(!!id, !!time) |>
  #     dplyr::group_by(!!id) |>
  #     dplyr::mutate(from = dplyr::lag(bin), to = bin) |>
  #     dplyr::ungroup() |>
  #     dplyr::filter(!is.na(from), !is.na(to)) |>
  #     dplyr::count(from, to, name = "value") |>
  #     dplyr::rename(source = from, target = to)
  #   
  #   list(nodes = nodes, edges = edges)
  # }
  
  # 
  # new_year_pop <- reduced_pop |> 
  #   mutate(year = year +1) |> 
  #   mutate(
  #     .risk = dplyr::if_else(qrisk_score > 1, qrisk_score / 100, qrisk_score),      # 0–1
  #     .risk = pmin(pmax(.risk, 0), 1),           # clamp
  #     qrisk_score = .risk + rnorm(n(),mean=0,sd=0.1)
  #   ) |> 
  #   select(-.risk)
  # 
  # pop1 <- rbind(reduced_pop,
  #               new_year_pop)
  # 
  # risk_ds <- risk_transition_graph(pop1)
  
  #risk_ds$nodes[['name']] = as.character(c(rep(1,10),rep(2,10)))
  
  # echarts4r::e_charts() |>
  #   echarts4r::e_graph(
  #     layout   = "circular",
  #     circular = list(rotateLabel = TRUE),
  #     roam     = TRUE,
  #     lineStyle = list(color = "source", curveness = 0.3),
  #     label     = list(position = "right", formatter = "{b}")
  #   ) |>
  #   echarts4r::e_graph_nodes(
  #     nodes = risk_ds$nodes,
  #     names = name,
  #     value = value,
  #     size  = size,
  #     category = name    # color by bin (simple default)
  #   ) |>
  #   echarts4r::e_graph_edges(
  #     edges  = risk_ds$edges,
  #     source = source,
  #     target = target,
  #     value  = value     # shows in tooltip; can map to width with edgeStyle if desired
  #   ) |>
  #   echarts4r::e_tooltip()  

  