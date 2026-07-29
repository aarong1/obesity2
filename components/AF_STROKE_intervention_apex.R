# library(apexcharter)
# af_stroke_output_agg <- read.csv(textConnection('year, 	intervention, 	normal, 	delta, `Discounting factor`, 
# 2016,	3293,	3293,	 0,  1
# 2017,	3301,	3308,	-7,	 0.9650000,
# 2018,	3308,	3314,	-6,	 0.9312250,
# 2019,	3320,	3325,	-5,	 0.8986321,
# 2020,	3334,	3333,	 1,  0.8671800,
# 2021,	3333,	3341,	-8,	 0.8368287,
# 2022,	3337,	3359,	-22, 0.8075397,
# 2023,	3346,	3364,	-18, 0.7792758'))
# 
# 
# (af_stroke_output_delta <- apex(height='250px',
#   af_stroke_output_agg, 
#   aes(x = year, y = delta),
# 
#   type = "area", 
#   serie_name = "Strokes Relative to baseline"
# ) %>% 
#     ax_colors('lightgreen') %>% 
#   ax_chart(toolbar = list(show=FALSE), 
#            #sparkline = list(enabled=TRUE),
#            animations = list(enabled = TRUE,
#                              speed=900, 
#                              animateGradually=list(enabled=TRUE,delay=300)
#                              ))  %>% 
#      ax_legend(show=TRUE,position = 'bottom') %>% 
#   ax_yaxis(tickAmount = 7, labels = list(formatter = format_num(",", suffix = ""))) %>% 
#   #ax_colors(c("#8485854D", "#FF0000")) %>%
#   ax_stroke(show = TRUE,curve ='smooth',width = c(0)) %>% 
#   ax_fill(
#   type = "gradient",
#   gradient = list(
#     shade = "light",
#     type = "vertical",
#     opacityFrom = 0.9,
#     opacityTo = 0,
#     colorStops =list(
#   list(opacity = 0.6, offset = 1, color = "#FB0000"),
#   list(opacity = 0.6, offset = 1, color = "#FFFFFF"),
#   list(opacity = 0.9, offset = 48, color = "#00FB15")
# )
# #     colorStops =list(
# #   list(opacity = 0.9, offset = 80, color = "#00FB15"),
# #   list(opacity = 0.6, offset = 99, color = "#FFFFFF"),
# #   list(opacity = 0.6, offset = 1, color = "#FB0000")
# # )
#   )
# ) %>% 
#      ax_markers(enable = TRUE) %>%
#      ax_dataLabels(enabled = FALSE,
#                    offsetY = 10,
#                    textAnchor = end
#                    ) %>%
#      ax_grid(show = FALSE) %>%
#      ax_tooltip(enable=TRUE) #%>%
# )
# 
# 
#  (
#  af_stroke_output_agg_plot <- af_stroke_ouput_agg %>% 
#    pivot_longer(cols = c(intervention,normal),
#                 names_to = 'intervention',
#                 values_to='incidence') %>% 
#  
# 
#  apex(height = '250px',
#   ., 
#   aes(x = year, y = incidence, fill = intervention) ,
#   type = "area", 
#   serie_name = "intervention"
# ) %>% 
#   ax_chart(toolbar = list(show=FALSE), 
#            #sparkline = list(enabled=TRUE),
#            animations = list(enabled = TRUE,speed=900, 
#                              animateGradually=list(enabled=TRUE,delay=300)
#                              )) %>% 
#      ax_legend(show=TRUE,position = 'bottom') %>% 
#   ax_yaxis(tickAmount = 7, labels = list(formatter = format_num(",", suffix = ""))) %>% 
#      ax_xaxis(labels = list(show=FALSE,    
#                             axisBorder= list(
#                               show=FALSE),
#                             axisTicks= list(
#                               show= FALSE)
#                             ) 
#               ) %>% 
#   #ax_colors(c("#8485854D", "#FF0000")) %>%
#   ax_stroke(curve ='smooth',width = c(3, 3)) %>% 
#   ax_fill(
#   type = "gradient",
#   gradient = list(
#     shade = "light",
#     type = "vertical",
#     opacityFrom = 1,
#     opacityTo = 0
#   )
# ) %>% 
#      ax_markers(enable = TRUE) %>% 
#      ax_dataLabels(enabled = TRUE) %>% 
#      ax_grid(show = FALSE) %>% 
#      ax_tooltip(enable=TRUE) #%>% 
#  )
# 
# 
# 
