library(echarts4r)
library(dplyr)
library(tidyr)

# --- 1. Data Transformation ---
# We pivot so Males and Females are separate columns
plot_data <- brain_agex_incidence %>%
  select(age, sex, per100k) %>%
  pivot_wider(names_from = sex, values_from = per100k) %>%
  mutate(idx = row_number() - 1) # 0-based index for JavaScript

sex_cols <- c("Males", "Females")

# Create the custom data matrix: [idx, Males_val, Females_val]
custom_data_matrix <- plot_data %>%
  select(idx, all_of(sex_cols)) %>%
  as.matrix() %>%
  unname()

# --- 2. Construct the Option List ---

# Create the Bar Series
bar_series <- lapply(sex_cols, function(name) {
  list(
    type = "bar",
    name = name,
    itemStyle = list(opacity = 0.7),
    data = plot_data[[name]]
  )
})

# Create the Custom Trend Series
custom_series <- list(list(
  type = "custom",
  name = "trend",
  renderItem = htmlwidgets::JS("function (params, api) {
    var xValue = api.value(0);
    var currentSeriesIndices = api.currentSeriesIndices();
    
    // barLayout needs to know there are 2 bar series
    var barLayout = api.barLayout({
      barGap: '30%',
      barCategoryGap: '20%',
      count: currentSeriesIndices.length - 1
    });

    var points = [];
    // Loop through the 2 bars (Males and Females)
    for (var i = 0; i < (currentSeriesIndices.length - 1); i++) {
        var val = api.value(i + 1); 
        var coords = api.coord([xValue, val]);
        
        if (barLayout[i]) {
          coords[0] += barLayout[i].offsetCenter;
          // Offset the point slightly above the bar
          coords[1] -= 5; 
          points.push(coords);
        }
    }

    return {
      type: 'polyline',
      shape: { points: points },
      style: api.style({
        stroke: '#333',
        fill: 'none',
        lineWidth: 2
      })
    };
  }"),
  encode = list(x = 0, y = 1:2),
  data = custom_data_matrix,
  z = 100
))

# --- 3. Render ---
option <- list(
  tooltip = list(trigger = "axis"),
  legend = list(data = c(sex_cols, "trend"), top = 20),
  xAxis = list(type = "category", data = plot_data$age),
  yAxis = list(type = "value", name = "Rate per 100k"),
  series = append(bar_series, custom_series)
)

e_charts() %>% e_list(option)