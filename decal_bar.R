library(echarts4r)

df <- data.frame(
  group = "Total",
  reached = 40,
  targeted = 10
  
)


df |>
  e_charts(group, height = '300px', emphasis = list(focus = 'series')) |>
  e_bar(reached, stack = "x",
        itemStyle = list(
          color = "#74A9F5",
          decal = list(symbol = "rect", 
                       rotation=-0.4,
                       
                       dashArrayX = c(1,0), dashArrayY = c(2, 5))
        )) |>
  e_bar(targeted, stack = "x",
        itemStyle = list(
          color = "#C9DDFB22",
          decal = list(symbol = "rect", 
                       rotation=0.4,
                       dashArrayX = c(1, 1), dashArrayY = c(2, 6))
        )) |>
  e_flip_coords() |>
  e_legend(show = FALSE) |>
  e_x_axis(show = FALSE) |>
  e_y_axis(show = FALSE) |>
  e_tooltip(trigger = "item") %>% 
  e_grid(left = 0, right = 0, top = 0, bottom = 0)|>
  e_axis(max=100) 



demo_chart_2 <- df |>
  e_charts(group, height = '300px', width= '400px', emphasis = list(focus = 'series')) |>
  e_theme('walden') %>% 
  e_bar(reached, stack = "x",
        itemStyle = list(
          color = "#74A9F5",
          decal = list(symbol = "rect", 
                       rotation=-0.4,
                       
                       dashArrayX = c(1,0), dashArrayY = c(2, 5))
        )) |>
  e_bar(targeted, stack = "x",
        itemStyle = list(
          color = "#C9DDFB22",
          decal = list(symbol = "rect", 
                       rotation=0.4,
                       dashArrayX = c(1, 1), dashArrayY = c(2, 6))
        )) #|>
  # e_flip_coords() |>
  # e_legend(show = FALSE) |>
  # e_x_axis(show = FALSE) |>
  # e_y_axis(show = FALSE) |>
  # e_tooltip(trigger = "item") %>% 
  # e_grid(left = 0, right = 0, top = 0, bottom = 0)|>
  # e_axis(max=100) 


library(echarts4r)

df <- data.frame(
  x = 1:10,
  y = c(5, 6, 7, 10, 9, 12, 15, 13, 16, 18),
  z = c(5-8, 6-3, 7-1, 10, 9, 12, 15, 13+2, 16+1, 18)+8
)


(
  
demo_chart <- df |>
  e_charts(x, 
           height = '300px', width= '400px',
           
           emphasis = list( focus = 'series')) |>
  
  e_theme('walden') %>%
  e_line(
    y,
    smooth = TRUE,
    symbol = "none",
    lineStyle = list(
      width = 3,
      color = 'white'
      
    ),
    areaStyle = list(
      color = htmlwidgets::JS(
        "new echarts.graphic.LinearGradient(0, 0, 0, 1, [
          {offset: 0, color: 'rgba(74,144,226,1)'},
          {offset: 1, color: 'rgba(74,144,226,1)'}
        ])"
      )
    )
  ) |>
  e_line(
    z,
    name='r',
    smooth = TRUE,
    symbol = "none",
    lineStyle = list(
      width = 3,
      color = 'white'
    ),
    areaStyle = list(
      color = htmlwidgets::JS(
        "new echarts.graphic.LinearGradient(0, 0, 0, 1, [
          {offset: 0, color: 'rgba(60, 179, 113,1)'},
        {offset: 1, color: 'rgba(60, 179, 113,0.2)'} ])"
      )
    )
  ) |>
  e_legend(show=T) %>% 
  e_x_axis(min = 1) |>
  e_tooltip(trigger = "axis") %>% 
  
  e_y_axis(#name = 'Epi Outputs: Incidence', nameGap = 50, nameLocation = 'middle', nameTextStyle = list(color = 'black', fontWeight ='bolder', fontSize = 25), show =T ,label ='f',
           axisLabel = list(show = F,fontWeight ='normal', fontSize = 10),
           axisLine = list(show =F),
           axisTick = list(show =F)
  ) |>
    e_x_axis(#name = 'Epi Outputs: Incidence', nameGap = 50, nameLocation = 'middle', nameTextStyle = list(color = 'black', fontWeight ='bolder', fontSize = 25), show =T ,label ='f',
      axisLabel = list(show = F, fontWeight ='normal', fontSize = 10),
      axisLine = list(show =F),
      axisTick = list(show =F)
    ) |>
  e_axis(show=F)

)
  
