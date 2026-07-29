# install.packages(c("shiny", "dplyr", "tidyr", "forcats", "echarts4r", "stringr"))
library(shiny)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(echarts4r)

# ---- helpers --------------------------------------------------------------

# 1) Ensure we have a BMI category column named `bmi_cat`


# 2) Tidy shares per panel
compute_panel_shares <- function(df,...) {
  df %>%
    mutate(
      # Make sure these exist and are readable
      #mdm_quintile_soa_name = coalesce(.data[["mdm_quintile_soa_name"]]) %>% as.character(),
      HSCT = .data[["HSCT"]] %>% as.character()
    ) %>%
    filter(!is.na(mdm_quintile_soa_name), !is.na(HSCT), !is.na(bmi)) %>%
    # mutate(
    #   bmi = fct(bmi, levels = c("Normal", "overweight", "obese"))
    # ) %>%
    count(HSCT, mdm_quintile_soa_name, bmi, name = "n") %>%
    # add_count(mdm_quintile_soa_name, bmi, name = "hscni_n") %>%
    # add_count(HSCT, bmi, name = "hscni_n") %>%
    
    group_by(...) %>%
    mutate(share = n / sum(n)) %>%
    # mutate(share = n / sum(n)) %>%
    # mutate(share = n / sum(n)) %>%
    
    ungroup()
}

# 3) Minimal echarts bar for a single panel
render_panel_chart <- function(dat, title_line1, title_line2 = NULL) {
  # dat: rows for one HSCT x quintile, columns bmi + share
  graph <- dat %>%
    #group_by(bmi) |> 
    e_charts(bmi,width='200',height='250',textStyle=list(fontStyle=12)) %>%#
    e_bar(share,name = paste (title_line1,str_remove(title_line2,pattern = 'quintile (SOA)')), roundCap = TRUE) %>%
    # e_tooltip(#trigger = "item", 
    #   e_tooltip_item_formatter(style = 'percent')
    # # formatter = htmlwidgets::JS("
    # #   function(p){return p.marker + p.name + ': ' + (p.value[[1]]*100).toFixed(0);  } 
    # # ")
    # ) |> 
    e_theme('roma') %>%  #+ '%'+ '/n'+p.seriesName.split(' ')[0]+ '\n'+p.seriesName.split(' ')[1]; 
    # e_color(c("#4add8c", "#f4d35e", "#ee6c4d")) |> 
    e_y_axis(min=0,max=0.75,
      show = TRUE,
      axisLabel = list(formatter = htmlwidgets::JS("function(v){return (v*100)+'%';}")),
      splitLine = list(show = TRUE)
    ) %>% #e_tooltip()
    e_tooltip(confine=T, backgroundColor = 'white',
              formatter = e_tooltip_item_formatter(style = 'percent')
              ) |>
    e_axis_labels() |>
    e_x_axis(axisTick = list(show = FALSE)) %>%
    e_grid(top = '30%', right = 10, bottom = 30, left = 40) %>%
    e_title(
      text = title_line1,
      subtext = title_line2,
      left = "left",
      textStyle = list(fontSize = 16, fontWeight = 500),
      subtextStyle = list(fontSize = 14, color = "#666")
    ) %>%
    e_legend(show = F)
  # print(graph)
  
  return(graph)
}

# ---- demo UI --------------------------------------------------------------

# ---- server ---------------------------------------------------------------

df  = pop# initial_time_zero_population
  
  # Prepare data
  dat_panels <- 
    df %>%
      # as_bmi_cat(bmi_col = "bmi") %>%
      compute_panel_shares( HSCT,mdm_quintile_soa_name)
  
  # Create one output per panel
  
    panels <- dat_panels %>%
      distinct(HSCT, mdm_quintile_soa_name) %>%
      arrange(HSCT, mdm_quintile_soa_name) %>%
      mutate(panel_id = paste0( str_replace_all(HSCT, "\\W+", "_"), '_', mdm_quintile_soa_name))
    
    # UI for all panels
    
      lapply(seq_len(nrow(panels)), function(i) {
        div(
          class = "panel-card",
          echarts4rOutput(panels$panel_id[i], height = "220px")
        )
      })
    
    # Render each chart
      group_echarts = lapply(seq_len(nrow(panels)), function(i) {
      id <- panels$panel_id[i]
      hsct_i <- panels$HSCT[i]
      q_i    <- panels$mdm_quintile_soa_name[i]
      
      dat_i <- dat_panels %>% filter(HSCT == hsct_i, mdm_quintile_soa_name == q_i)
      
     render_panel_chart(
          dat_i,
          title_line1 = paste0(hsct_i),
          title_line2 = paste0( q_i)
        )
      })
      
      
      pop %>% 
        count(mdm_quintile_soa_name,HSCT,bmi) %>% 
        add_count(mdm_quintile_soa_name, HSCT, wt=n) %>%
        mutate(per = n/nn) %>% 
        arrange(desc(per)) %>% 
        filter_out(bmi == 'normal')
      
      # #top obese
      # 26         Most Deprived  NHSCT      obese  646 2237 0.28877962
      # 27         Most Deprived  SHSCT      obese  942 3370 0.27952522
      # 
      # #top overweight
      # 1         Least Deprived  WHSCT overweight  143  341 0.41935484
      # 2             Quintile 2  WHSCT overweight 2105 5076 0.41469661
      
      
      
      # div(class="grid-5x5 mx-3 px-3",
      #     style= 'display: grid;
      #            grid-template-columns: repeat(5, 1fr);
      #            grid-auto-rows: 1fr;            /* equal heights within a fixed container */
      #              gap: 10px;
      #            /* optional: fix container height so rows are equal; or remove and let content size */
      #             ',
      #     group_echarts) %>%
      # 
      #   browsable()

save(group_echarts, file = './preprocess/pages_prep.RData')
      
      