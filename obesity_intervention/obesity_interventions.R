library(echarts4r)

data.frame(
  x = c(0, 0.25, 0.5, 0.75, 1),
  y = c(0, 0.25, 0.5, 0.75, 0.75)
) |>
  e_charts(x) |>
  e_line(y, symbol = "") |>
  e_tooltip(trigger = "axis") |>
  e_x_axis(min = 0, max = 1) |>
  e_y_axis(min = 0, max = 1)


library(echarts4r)

baseline <- data.frame(
  category = c("Normal", "Overweight", "Obese"),
  count = c(209, 293, 50)
)

baseline |>
  e_charts(category) |>
  e_bar(count, name = "Baseline Count") |>
  e_tooltip() |>
  e_title("Weight Distribution")



data.frame(
  group = c("Normal", "Original", "Obese→Overweight"),
  category = c("Normal","Overweight", "Obese"),
  Normal = c(209, 293, 0),
  Intervention =  c(0, 100, 0)
) |>
  e_charts(category) |>
  e_bar(Normal, stack = "group") |>
  e_bar(Intervention, stack = "group") |>  #, bind = group
  e_tooltip() |>
  e_title("Intervention Scenario:") |>
  e_legend()


df <- data.frame(
  x = LETTERS[1:10],
  a = runif(10),
  b = runif(10),
  c = runif(10),
  d = runif(10)
)

df |> 
  e_charts(x) |> 
  e_bar(a, stack = "grp") |> e_tooltip() |> 
  e_bar(b, stack = "grp") |> 
  e_bar(c, stack = "grp2") |> 
  e_bar(d, stack = "grp2") 

