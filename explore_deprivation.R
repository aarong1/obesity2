library(e_charts)

 pop |> 
  # group_by(bmi) |>
  e_chart() |> 
  e_density(stack = 'bmi',
            name = 'Townsend',
            serie = custom_townsend_score_dz,       
            itemStyle = list(opacity=0),
            lineStyle = list(width=2)#,color='white'
  ) |> 
  e_grid( containLabel = T)  |> 
  e_theme('walden')



library(echarts4r)


  e1 <- dat %>%
    e_charts_(score_col) %>%
    e_density_(score_col) %>%
    e_mark_line(data = lapply(q_edges, function(v) list(xAxis = v, name = "Q edge")), title = "Quantile edges") %>%
    e_mark_line(data = lapply(ew_edges, function(v) list(xAxis = v, name = "Equal edge")), title = "Equal-width edges") %>%
    e_title("Density with Bin Boundaries") %>%
    e_theme("walden")
  
  pop$custom_townsend_quintile <- ntile(pop$custom_townsend_score_dz, 5)
  
  pop %>% group_by(custom_townsend_quintile) %>%
    summarise(n = n(),min = min(custom_townsend_score_dz),max = max(custom_townsend_score_dz)) %>%
    arrange(custom_townsend_quintile)

  reduce(.init =   pop |> 
           # group_by(bmi) |>
           e_chart() |> 
           e_density(stack = 'bmi',
                     name = 'Townsend',
                     serie = custom_townsend_score_dz,
                     itemStyle = list(opacity=0),
                     lineStyle = list(width=2)#,color='white'
           ),
         .f = function(x,y) {x %>% 
             e_mark_line(    data =
                               list( xAxis = c(as.character(round(y,2))),
                                     tooltip = list(formatter = '<b>{c0} </b> <br /> Newest cohort now turns 74<br /> Demand  Plateaus')
                               ),
                             symbol = "none",
                             lineStyle = list(type = "dashed", color = "black")) 

           },
         .x = pop %>% group_by(custom_townsend_quintile) %>%
           summarise(n = n(),min = min(custom_townsend_score_dz),max = max(custom_townsend_score_dz)) %>%
           arrange(custom_townsend_quintile) %>% pull(max) %>% .[1:4]
  )
  
  reduce(.init =   pop |> 
           # group_by(bmi) |>
           e_chart() |> 
           e_density(stack = 'bmi',
                     name = 'Townsend',
                     serie = custom_townsend_score_dz,
                     itemStyle = list(opacity=0),
                     lineStyle = list(width=2)#,color='white'
           ),
         .f = function(x,y) {
           x %>% 
             e_mark_line(    data =
                               list( xAxis = c(as.character(round(y,2))),
                                     tooltip = list(formatter = '<b>{c0} </b> <br /> Newest cohort now turns 74<br /> Demand  Plateaus')
                               ),
                             symbol = "none",
                             lineStyle = list(type = "dashed", color = "black")) 
           
         },
         .x = seq( min(pop$custom_townsend_score_dz), max(pop$custom_townsend_score_dz),length.out=6) %>% 
           .[2:5]
  )
  
  pop %>% 
    group_by(dz_id) %>%
    summarise(n=n(), custom_townsend_score_dz=first(custom_townsend_score_dz),custom_townsend_quintile=first(custom_townsend_quintile)) %>% 
    group_by(custom_townsend_quintile) %>%
    summarise(n = sum(n),min = min(custom_townsend_score_dz),max = max(custom_townsend_score_dz)) %>%
    arrange(custom_townsend_quintile)
  
  pop %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    group_by(tq) %>%
    filter(!is.na(tq)) %>% 
    summarise(n=n(), custom_townsend_score_dz=first(custom_townsend_score_dz),custom_townsend_quintile=first(custom_townsend_quintile)) %>% 
    e_chart(tq) %>%
    e_grid(containLabel = T) %>% 
    e_theme('walden') %>% 
    e_tooltip() %>%
    e_bar(n)
    
  pop %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    group_by(tq) %>%
    summarise(bmi = sum(bmi%in%c('overweight','obese')), n=n()) %>% 
    e_charts(tq) %>% 
    e_bar(bmi)%>%
    e_grid(containLabel = T) %>% 
    e_theme('walden') %>% 
    e_tooltip() 
  
  pop %>% 
    group_by(custom_townsend_quintile) %>%
    summarise(bmi = sum(bmi%in%c('overweight','obese')), n=n()) %>% 
    e_charts(custom_townsend_quintile) %>% 
    e_bar(bmi)%>%
    e_grid(containLabel = T) %>% 
    e_theme('walden') %>% 
    e_tooltip() 
  
  pop %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    filter(!is.na(tq)) %>% 
    group_by(tq) %>%
    summarise(bmi = mean(bmi%in%c('overweight','obese')), n=n()) %>% 
    mutate(tq = as.numeric(tq)) %>% 
    e_charts(tq) %>% 
    e_bar(bmi)%>%
    e_grid(containLabel = T) %>% 
    e_theme('walden') %>% 
    e_tooltip() %>% 
    e_y_axis(min=0.54,max=0.59) %>% 
    e_lm(bmi~tq)
  
  pop %>% 
    group_by(custom_townsend_quintile) %>%
    summarise(bmi = mean(bmi%in%c('overweight','obese')), n=n()) %>% 
    e_charts(custom_townsend_quintile) %>% 
    e_bar(bmi)%>%
    e_grid(containLabel = T) %>% 
    e_theme('walden') %>% 
    e_tooltip() %>% 
    e_y_axis(min=0.54,max=0.59) %>%
    e_lm(bmi~custom_townsend_quintile)
  
  pop %>% 
    group_by(dz_id) %>% 
    summarise(bmi = mean(bmi%in%c('overweight','obese')),
              custom_townsend_score_dz = first(custom_townsend_score_dz),
              n=n()) %>% 
    e_charts(custom_townsend_score_dz) %>% 
    e_scatter(bmi) %>% 
    e_lm(bmi ~ custom_townsend_score_dz, name = 'bmi',symbol = 'square',legend = T) %>% 
    e_y_axis(min=0.45,max=0.85) %>% 
    e_tooltip()
    
  pop %>% 
  mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    filter(tq%in%c(2,3)) %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    count(bmi,tq) %>% 
    add_count(tq,wt=n,name = 'nn') %>% 
    mutate(n = n/nn) %>% 
    filter(!is.na(bmi),!is.na(tq)) %>%
    group_by(tq) %>% 
    e_charts(bmi) %>% 
    e_bar(n, legend = T) %>% 
    e_lm(n~bmi, name = as.character(1:5), legend = T) %>% 
    e_y_axis(min=0.2)
    
  pop %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    filter(tq%in%c(2,3)) %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    count(bmi,tq) %>% 
    add_count(bmi,wt=n,name = 'nn') %>% 
    mutate(n = n/nn) %>% 
    filter(!is.na(bmi),!is.na(tq)) %>%
    mutate(tq = as.numeric(tq)) %>% 
    group_by(bmi) %>%
    e_charts(tq) %>% 
    e_bar(n, legend = T) %>% 
    # e_data( pop %>% 
    #           mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    #           filter(tq%in%c(2,3)) %>% 
    #           mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    #           count(bmi,tq) %>% 
    #           add_count(bmi,wt=n,name = 'nn') %>% 
    #           mutate(n = n/nn) %>% 
    #           filter(!is.na(bmi),!is.na(tq)) %>%
    #           mutate(tq = as.numeric(tq))%>% group_by(tq)) %>% 
    e_lm(n~tq, legend = T) %>% 
    e_y_axis(min=0.1)
  
  pop %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    filter(tq%in%c(2,3)) %>% 
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    count(bmi,custom_townsend_score_dz) %>% 
    add_count(bmi,wt=n,name = 'nn') %>% 
    # mutate(n = n/nn) %>% 
    filter(!is.na(bmi),!is.na(custom_townsend_score_dz)) %>%
    # mutate(tq = as.numeric(tq)) %>% 
    group_by(bmi) %>%
    e_charts(custom_townsend_score_dz) %>% 
    e_scatter(n, legend = T) %>%
    e_lm(n~custom_townsend_score_dz,name = c('obese','overweight','normal'), legend = T) %>%
    e_y_axis(max=40)
    
  pop %>% 
    # mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>%
    # filter(tq%in%c(2,3)) %>%
    mutate(tq=cut(custom_townsend_score_dz, seq( min(custom_townsend_score_dz), max(custom_townsend_score_dz),length.out=6),as.character(1:5))) %>% 
    count(bmi,custom_townsend_score_dz) %>% 
    add_count(bmi,wt=n,name = 'nn') %>% 
    # mutate(n = n/nn) %>%
    filter(!is.na(bmi),!is.na(custom_townsend_score_dz)) %>%
    # mutate(tq = as.numeric(tq)) %>% 
    group_by(bmi) %>%
    e_charts(custom_townsend_score_dz) %>% 
    # e_density(n, legend = T) %>%
    e_scatter(n, legend = T) %>%
    e_lm(n~custom_townsend_score_dz,name = c('obese','overweight','normal'), legend = T) %>%
    e_tooltip() #%>% 
    # e_y_axis(max=80)
  
  
  
  
    # group_by(custom_townsend_quintile) %>%
    # e_charts(custom_townsend_quintile) %>% 
    # e_bar(bmi) %>%
    # e_grid(containLabel = T) %>% 
    # e_theme('walden') %>% 
    # e_tooltip() %>% 
    # e_y_axis(min=0.54,max=0.59) %>%

  
  e_mark_line(    data =
                    list( xAxis = c(as.character(5)),
                          tooltip = list(formatter = '<b>{c0} </b> <br /> Newest cohort now turns 74<br /> Demand  Plateaus')
                    ),
                  symbol = "none",
                  lineStyle = list(type = "dashed", color = "black")) 
    
  pop |> 
    # group_by(bmi) |>
    e_chart() |> 
    e_density(stack = 'bmi',
              name = 'Townsend',
              serie = custom_townsend_score_dz,
              itemStyle = list(opacity=0),
              lineStyle = list(width=2)#,color='white'
    ) |>  
    e_mark_line(    data =
                      list( xAxis = c(as.character(5)),
                            tooltip = list(formatter = '<b>{c0} </b> <br /> Newest cohort now turns 74<br /> Demand  Plateaus')
                      ),
                    symbol = "none",
                    lineStyle = list(type = "dashed", color = "black")) %>% 

  e_mark_area(title = 'w',
        'custom_townsend_score_dz',
    data = list(
      list(xAxis = '-1.35' , yAxis = 0),
      list(xAxis = ' 0.0881 ', yAxis = 0.18)
    )
  ) %>% 
    e_grid( containLabel = T)  |> 
    e_theme('walden') 
  
  
3# Counts per bin


  kpi_icon <- function(title, value, icon = bsicons::bs_icon("dot")) {
    div(class="kpi-card",
        div(style="display:flex; align-items:center; gap:10px;",
            div(style="opacity:.55; transform: translateY(-1px);",
                icon
            ),
            div(style="flex:1;",
                div(class="kpi-title", title),
                div(class="kpi-value", value)
            )
        )
    )
  }
  proto_C <- div(
    class="kpi-row",
    kpi_icon("Latest year", demo_vals$latest_year, bs_icon("calendar")),
    kpi_icon("Annual averted", demo_vals$annual_averted, bs_icon("arrow-down")),
    kpi_icon("Cumulative averted", demo_vals$cumulative_averted, bs_icon("stack")),
    kpi_icon("Peak cumulative", demo_vals$peak_cumulative, bs_icon("graph-up"))
  )