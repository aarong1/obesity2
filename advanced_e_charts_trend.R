

#################################################


library(echarts4r)
library(dplyr)
library(purrr)



##############
##############
##############

library(echarts4r)
library(dplyr)
library(tidyr)
library(purrr)
library(echarts4r)
library(dplyr)
library(tidyr)
library(purrr)
library(echarts4r)
library(dplyr)
library(tidyr)

trend_mdm_hsct <- { 
# --- 1. Data Transformation ---
plot_data <- pop %>%
  count(mdm_quintile_soa_name, bmi=(bmi %in% c( "obese" , "overweight")&!is.na(bmi)), HSCT) %>%
  
  add_count(HSCT,mdm_quintile_soa_name,wt=n,name='nn') %>%
  # filter(bmi == "overweight") %>%
  # filter(bmi == "obese") %>%
  filter(bmi==T) %>%
  mutate(n=n/nn) %>% 
  select(HSCT, mdm_quintile_soa_name, n) %>%
  arrange(mdm_quintile_soa_name) %>% 
  pivot_wider(names_from = mdm_quintile_soa_name, values_from = n, values_fill = 0) %>%
  mutate(idx = row_number() - 1)

quintile_cols <- setdiff(colnames(plot_data), c("HSCT", "idx"))

# Robust data conversion to Array of Arrays
custom_data_matrix <- plot_data %>%
  select(idx, all_of(quintile_cols)) %>%
  as.matrix() %>%
  unname()

# --- 2. Construct the Option List ---

# Create Bar Series
bar_series <- lapply(quintile_cols, function(name) {
  list(
    type = "bar",
    name = as.character(name),
    itemStyle = list(opacity = 0.7),
    data = plot_data[[name]]
  )
})

# Create Custom Series
custom_series <- list(list(
  type = "custom",
  name = "trend",
  renderItem = htmlwidgets::JS("function (params, api) {
    var xValue = api.value(0);
    var currentSeriesIndices = api.currentSeriesIndices();
    
    // Calculate layout for all bars in the group
    var barLayout = api.barLayout({
      barGap: '30%',
      barCategoryGap: '20%',
      count: currentSeriesIndices.length - 1
    });

    var points = [];
    // We iterate through the number of bars (quintiles)
    for (var i = 0; i < (currentSeriesIndices.length - 1); i++) {
        // api.value(i + 1) gets the y-value from our custom_data_matrix
        var val = api.value(i + 1); 
        var coords = api.coord([xValue, val]);
        
        if (barLayout[i]) {
          coords[0] += barLayout[i].offsetCenter;
          coords[1] -= 10; 
          points.push(coords);
        }
    }

    return {
      type: 'polyline',
      shape: { points: points },
      style: api.style({
        stroke: '#333', // Explicit color to verify rendering
        fill: 'none',
        lineWidth: 2
      })
    };
  }"),
  encode = list(x = 0, y = 1:length(quintile_cols)),
  data = custom_data_matrix,
  z = 100
))

# --- 3. Final Render ---
adv_echarts <- e_charts() %>% 
  e_list(list(
    emphasis = list(focus = 'series'),
    tooltip = list(trigger = "axis"),
    legend = list(data = c(quintile_cols, "Trend"), top = 20),
    xAxis = list(type = "category", data = plot_data$HSCT),
    yAxis = list(type = "value"),
    series = append(bar_series, custom_series)
  )) %>% 
  e_theme('roma') %>% 
  e_axis(axis = 'y', formatter = e_axis_formatter('percent'))

}


# plot_data <- pop %>%
#   count(mdm_quintile_soa_name, bmi=(bmi %in% c( "obese" , "overweight")&!is.na(bmi)), HSCT) %>%
#   
#   add_count(HSCT,mdm_quintile_soa_name,wt=n,name='nn') %>%
#   # filter(bmi == "overweight") %>%
#   # filter(bmi == "obese") %>%
#   filter(bmi==T) %>%
#   # mutate(n=n/nn) %>% 
#   select(HSCT, mdm_quintile_soa_name, n) %>%
#   arrange(mdm_quintile_soa_name) %>% 
#   pivot_wider(names_from = mdm_quintile_soa_name, values_from = n, values_fill = 0) %>%
#   mutate(idx = row_number() - 1)
# 

# # --- 1. Constants and Data Prep ---
# year_count <- 7
# category_count <- 30
# years <- as.character(2010:(2010 + year_count - 1))
# x_axis_data <- paste0("category", 0:(category_count - 1))
# 
# # Generate random data in R
# set.seed(42)
# data_list <- map(1:year_count, ~ numeric(category_count))
# custom_data <- map(0:(category_count - 1), function(i) {
#   val <- runif(1, 0, 1000)
#   row_vals <- numeric(year_count)
#   
#   for (j in 1:year_count) {
#     if (j == 1) {
#       row_vals[j] <- round(val, 2)
#     } else {
#       row_vals[j] <- round(max(0, row_vals[j - 1] + (runif(1) - 0.5) * 200), 2)
#     }
#     data_list[[j]][i + 1] <<- row_vals[j] # Update data_list for bar series
#   }
#   return(c(i, row_vals)) # Format: [index, y1, y2, y3...]
# })
# 
# # --- 2. Construct the Option List ---
# option <- list(
#   tooltip = list(trigger = "axis"),
#   legend = list(data = c("trend", years), top = 20),
#   dataZoom = list(
#     # list(type = "slider", start = 50, end = 70),
#     # list(type = "inside", start = 50, end = 70)
#   ),
#   xAxis = list(data = plot_data$HSCT),
#   yAxis = list(type = "value"),
#   series = append(
#     # The Custom Trend Series
#     list(list(
#       type = "custom",
#       name = "trend",
#       renderItem = htmlwidgets::JS("function (params, api) {
#         var xValue = api.value(0);
#         var currentSeriesIndices = api.currentSeriesIndices();
#         var barLayout = api.barLayout({
#           barGap: '30%',
#           barCategoryGap: '20%',
#           count: currentSeriesIndices.length - 1
#         });
#         var points = [];
#         for (var i = 0; i < currentSeriesIndices.length; i++) {
#           var seriesIndex = currentSeriesIndices[i];
#           if (seriesIndex !== params.seriesIndex) {
#             var point = api.coord([xValue, api.value(seriesIndex)]);
#             point[0] += barLayout[i - 1].offsetCenter;
#             point[1] -= 20;
#             points.push(point);
#           }
#         }
#         return {
#           type: 'polyline',
#           shape: { points: points },
#           style: api.style({ stroke: api.visual('color'), fill: 'none' })
#         };
#       }"),
#       itemStyle = list(borderWidth = 2),
#       #encode = list(x = 0, y = 1:year_count),
#       data = custom_data,
#       z = 100
#     )),
#     map(quintile_cols, function(name) {
#       list(
#         type = "bar",
#         name = name,
#         itemStyle = list(opacity = 0.7),
#         data = plot_data[[name]]
#       )
#     })
#     # The Bar Series
#     # map2(data_list, years, ~ list(
#     #   type = "bar",
#     #   name = .y,
#     #   animation = FALSE,
#     #   itemStyle = list(opacity = 0.5),
#     #   data = .x
#     # ))
#   )
# )
# 
# # --- 3. Render ---
# e_charts() %>% e_list(option)