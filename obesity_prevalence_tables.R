# top_town_overweight_prev|> 
#   rowwise() |>
#   transmute(first_col = as.character(
#     tagList(h5(SETTLEMENT2015_name),
#             span(class='text-muted pe-2',tot),
#             span(class='text-muted pe-2',HSCT)))) |> 
#   reactable(#colnames = "", 
#             defaultColDef = colDef(html = TRUE, 
#                                    align = "left"),
#             bordered = F,
#             highlight = F,
#             striped = F,
#             pagination = F) |> 
#   page_fluid() |> 
#   browsable()


# install.packages(c("reactable", "reactablefmtr", "dplyr", "purrr", "scales"))
library(dplyr)
library(purrr)
library(scales)
library(reactable)
library(reactablefmtr)

# ---- THEME & COLORS ----------------------------------------------------------
hsct_cols <- c(
  "BHSCT" = "#7FB3D5",
  "SEHSCT"= "#A3E4D7",
  "SHSCT" = "#F7DC6F",
  "NHSCT" = "#F5B7B1",
  "WHSCT" = "#C39BD3"
)

hsct_cols <- c(
  "BHSCT" = '#c6b38e',
  "SEHSCT"= '#001852',
  "SHSCT" = '#f5e8c8',
  "NHSCT" = '#b8d2c7',
  "WHSCT" = '#E01F54'
)




# df = top_town_overweight_prev 

# ---- WIDGET FUNCTION ---------------------------------------------------------
library(dplyr)
library(reactable)
library(reactablefmtr)
library(purrr)
library(scales)

n=model_specification$population$scale_down_factor

df  = top_town_overweight_prev = pop |> 
  group_by(SETTLEMENT2015_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            overweight = sum(bmi == 'overweight',na.rm=T),
            obese = sum(bmi == 'obese',na.rm = T),
            tot = n(),
            HSCT = first(HSCT),
            dep = mean(custom_townsend_rank),
            town  = mean(custom_townsend_score_dz),
            cmms = mean(cmms)
  ) |> 
  filter(tot>30) |> 
  mutate(big_prev = big/tot,
         obese_prev = obese/tot,
         overweight_prev = overweight/tot) |> 
  arrange(desc(big_prev)) |> 
  head(20)


(

  formatted_table <- df %>%
    rowwise() |>
    mutate(Settlement = as.character(
      tagList(h5(SETTLEMENT2015_name),
              div(class=' pe-2 fw-bold',HSCT),
              span(class=' pe-2 rounded-pill',prettyNum(tot*n,big.mark=',')),
              span(class=' float-right ms-2','Deprivation:',round(digits = 2,town)),
              span(class=' pe-2  ms-2','Multimorbidity:',round(digits = 2,cmms))
              
              ))) |> 
    dplyr::relocate(Settlement, .before=SETTLEMENT2015_name)|> 
    left_join(data.frame(hsct_cols) |> rownames_to_column('HSCT')) |> 
    mutate(
      # nice labels
      # Settlement = SETTLEMENT2015_name,
      # mini vectors for sparklines (overweight vs obese prevalence)
      prev_spark = map2(overweight_prev, obese_prev, ~c(.x, .y)),
      # mini vectors for bar microchart (absolute counts)
      counts_spark = pmap(list(big, overweight, obese), c),
      # display helpers
      HSCT_badge = HSCT,
      dep_num = as.numeric(dep)  # if dep is numeric-like but character
    ) %>%
    reactable(
      bordered = FALSE,
      highlight = F,
      searchable = F,
      resizable = F,
      sortable = F,
      defaultPageSize = 30,
      wrap = FALSE,
      defaultSorted = list(big_prev = "desc"),
      defaultColDef = colDef(style = list(margin =c(0,20)), align = "center", headerClass = "header"),
      columns = list(
        cmms = colDef(show = F),
        hsct_cols = colDef(show=F),
        Settlement = colDef(
          # cell = color_tiles(
          #   data = .,
          #   color_ref = 'hsct_cols'
          # ),
          style = color_scales(., "hsct_cols", colors = c("lightyellow", "orange", "#bb0000")),
          html = T,
          name = "Settlement",
          minWidth = 150, 
          align = "left",
          sticky = "left"),
        
        HSCT = colDef(show = FALSE),
        HSCT_badge = colDef(show = FALSE),
        SETTLEMENT2015_name = colDef(show = FALSE),
        # HSCT_badge = colDef(
        #   name = "HSCT",
        #   minWidth = 100,
        #   html = TRUE,
        #   cell = function(value) {
        #     bg <- hsct_cols[[value]] %||% "#e0e0e0"
        #     paste0(
        #       "<span style='padding:4px 8px; border-radius:999px; background:", bg,
        #       "; color:#222; font-weight:600;'>", value, "</span>"
        #     )
        #   }
        # ),
        dep = colDef(show = FALSE),
        dep_num = colDef(show = FALSE,
          name = "Deprivation (index)",
          minWidth = 120,
          #cell = color_tiles(., "dep_num", colors = c("#e8f0ff", "#0033aa")),
          format = colFormat(digits = 1)
        ),
        
        # Absolute counts as compact bar microchart
        counts_spark = colDef( show=F, style = list(padding = "0px 2px"),
          name = "Counts (big / overweight / obese)",
          minWidth = 100,
          cell = reactablefmtr::react_sparkbar(fill_color = 'lightblue',
                                               statline = 'mean',
                                               margin = margin(0,20,0,20),
                                               data = .,          # pass the whole data being rendered
                                               height = 75,
                                               min_value = 0,     # since these are proportions
                                               max_value = 1000,
                                               decimals = 0,      # tooltip/label rounding
                                               tooltip = TRUE
            )),
        big = colDef(show = FALSE),
        overweight = colDef(show = FALSE),
        obese = colDef(show = FALSE),
  # counts_spark = colDef(
  #   name = "Counts (big / overweight / obese)",
  #   minWidth = 210,
  #   cell = function(values) {
  #     # names for tooltips
  #     nms <- c("big","overweight","obese")
  #     reactablefmtr::bar_chart(
  #       values,
  #       max_value = max(values),
  #       # no explicit colors -> a consistent palette
  #       labels = paste0(nms, ": ", comma(values)),
  #       height = "22px",
  #       width = "100%",
  #       round_edges = TRUE,
  #       tooltip = TRUE
  #     )
  #   }
  # )
  

# Prevalence columns with vivid color mapping
big_prev = colDef(
  name = "High BMI",
  minWidth = 120,
  cell = color_tiles(number_fmt = scales::percent_format(),data= ., 
                     colors = c("#fff5f0",  "#E01F54")),
  format = colFormat(percent = TRUE, digits = 1),
  # cell = reactablefmtr::color_tiles(
  #   data   = .,
  #   # color_by = "big_prev",
  #   #colors  = c("#fff5f0", "#fb6a4a")
  #   ),

  footer = function(values) strong(percent(mean(values, na.rm = TRUE)))
),
# overweight_prev = colDef(
#   name = "Prev: Overweight",
#   minWidth = 140,
#   format = colFormat(percent = TRUE, digits = 1),
#   cell = color_tiles(., colors = c("#f0f9e8", "#41ab5d")),
#   footer = function(values) strong(percent(mean(values, na.rm = TRUE)))
# ),

overweight_prev = colDef(
  name = "Overweight",
  cell = data_bars(
    data = .,
    fill_color = c('#f5e8c8','#c6b38e'),#c('#FFF2D9','#FFE1A6','#FFCB66','#FFB627'),
    fill_gradient = TRUE,
    background = 'transparent',
    number_fmt = scales::percent_format(accuracy = 0.01)
),
  minWidth = 140,
  # format = colFormat(percent = TRUE, digits = 1),
  footer = function(values) strong(percent(mean(values, na.rm = TRUE)))
),
obese_prev = colDef(
  name = "Obese",
  minWidth = 120,
  format = colFormat(percent = TRUE, digits = 1),
  cell = color_tiles(.,
                     number_fmt = scales::percent_format(),
                     colors = c("#f7f4ff", "#001852")),
  footer = function(values) strong(percent(mean(values, na.rm = TRUE)))
),

# Tiny sparkline comparing overweight vs obese prevalence
prev_spark = colDef( show=F,  style = list(padding = "6px 5px"),

  #format = colFormat(percent = TRUE, digits = 1),
  
  name = "Distribution",
  minWidth = 100,
  cell = reactablefmtr::react_sparkbar(fill_color = 'violet',
    # statline = 'mean',
    margin = margin(3,20,4,20),
    data = .,          # pass the whole data being rendered
    height = 85,
    min_value = 0,     # since these are proportions
    # max_value = 1.8,
    decimals = 2,      # tooltip/label rounding
    # tooltip = TRUE
  )
),
town = colDef(show=F),
# obese_prev = colDef(show=F),
# overweight_prev = colDef(show=F),
# Totals & meta
tot = colDef(show = FALSE, name = "Total", format = colFormat(separators  = T), minWidth = 90),
dep = colDef(show = FALSE) # hide original
)#,
# theme = nytimes(cell_padding = 20)#cosmo()
#   reactableTheme(
#   cellStyle = list(padding = "15px 30px"),
#   borderColor = "#eee",
#   highlightColor = "#f3f8ff",
#   inputStyle = list(borderRadius = "10px"),
#   rowSelectedStyle = list(backgroundColor = "#eef5ff")
# )
)
) #%>% page_fluid()

save(formatted_table, file = './preprocess/obesity_prevalence_tables.RData')

