

(
bmi_pie <- pop %>%
  count(bmi, name = "count") %>%
  mutate(
    count = count * model_specification$population$scale_down_factor
  ) %>%
  filter(!is.na(bmi)) %>%
  e_charts(bmi, height ='100%', width = '100%') %>%
  e_pie(
    count,
    name = 'BMI',
    legend = F,
    emphasis = list(focus = "self")
  ) %>%
  e_grid(top=0,bottom=0,left=0,right=0) %>%
  e_labels(show=T,
           position = 'inside',
           textStyle = list(color = 'white')) %>% 
  # e_title("BMI Breakdown") %>%
  e_tooltip() %>%
  e_theme("walden") %>%
  e_aria(
    enabled = T,
    decal = list(show = TRUE)
  )
)
