#prevalence

# lung_cancer
# colorectal_cancer
# oral_cancer
# pancreatic_cancer
# uterine_cancer
# blood_cancer
# ovarian_cancer
# osteogastric_cancer
# prostate_cancer
# female_breast_cancer
# renal_cancer

oral_prev_df
ovarian_prev_df

past_populations %>%
  group_by(id,run) %>% 
  arrange(year) %>% 
  fill(death,death_reason) %>% 
  ungroup() %>% 
  filter(is.na(death_reason)) %>% 
  filter(lung_cancer != 0) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

library(dplyr)
library(tidyr)
library(rlang)

compute_mean_prevalence_by_year <- function( past_populations, cancer_col, model_specification) {
  
  cancer_col <- enquo(cancer_col)
  
  past_populations %>%
    group_by(id,run) %>% 
    arrange(year) %>% 
    fill(death,death_reason) %>% 
    ungroup() %>% 
    filter(is.na(death_reason)) %>% 
    filter(!!cancer_col != 0) %>%
    count(year, run, name = "n") %>%
    complete(
      year = sort(unique(past_populations$year)),
      run  = seq_len(model_specification$model$number_of_runs),
      fill = list(n = 0)
    ) %>%
    group_by(year) %>%
    summarise(n = mean(n), .groups = "drop") %>%
    mutate(
      n = n * model_specification$population$scale_down_factor
    )
  
}

plot_mean_prevalence_by_year <- function( past_populations, cancer_col, model_specification, plot_df) {
  
  cancer_col <- enquo(cancer_col)
  
  xxx <- past_populations %>%
    group_by(id,run) %>% 
    arrange(year) %>% 
    fill(death,death_reason) %>% 
    ungroup() %>% 
    filter(is.na(death_reason)) %>%     filter(!!cancer_col != 0) %>%
    count(year, run, name = "n") %>%
    complete(
      year = sort(unique(past_populations$year)),
      run  = seq_len(model_specification$model$number_of_runs),
      fill = list(n = 0)
    ) %>%
    group_by(year) %>%
    summarise(n = mean(n), .groups = "drop") %>%
    mutate(
      n = n * model_specification$population$scale_down_factor
    ) 
  
  message(last(plot_df$prevalence)/first(xxx$n))
  message(last(plot_df$prevalence))
  message(first(xxx$n))
  
  xxx%>% 
    ggplot(aes(x=year,y=n)) +
    geom_line()+
    geom_point()+
    geom_label(aes(x=year, label= n))+
    geom_line(data = plot_df, aes(x=year, y= prevalence),color = 'blue')+
    geom_point(data = plot_df, aes(x=year, y= prevalence), color = 'blue')+    
    geom_label(data = plot_df, aes(x=year, y= prevalence, label= prevalence), color = 'blue')+
    ylim(c(0,NA))
  
  
}



count(prev_df,source_file) 

lung_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Lung')) %>% 
  mutate(year = as.numeric(year))

colorectal_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Colorectal')) %>% 
  mutate(year = as.numeric(year))

oral_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Oral')) %>% 
  mutate(year = as.numeric(year))

pancreatic_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Pancreatic')) %>% 
  mutate(year = as.numeric(year))

uterine_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Uterine')) %>% 
  mutate(year = as.numeric(year))

blood_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Blood')) %>% 
  group_by(year) %>% 
  summarise(prevalence = sum(prevalence))%>% 
  mutate(year = as.numeric(year))

ovarian_prev_df <- prev_df %>%
  filter(str_detect(string = source_file, pattern = 'Ovarian'))%>% 
  mutate(year = as.numeric(year))

osteogastric_prev_df <- prev_df %>%
  filter(str_detect(string = source_file, pattern = 'Stomach|Oesophageal')) %>% 
  group_by(year) %>% 
  summarise(prevalence = sum(prevalence)) %>%
  mutate(year = as.numeric(year))

prostate_prev_df <- prev_df %>%
  filter(str_detect(string = source_file, pattern = 'Prostate'))%>% 
  mutate(year = as.numeric(year))

breast_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Female_breast_cancer'))%>% 
  mutate(year = as.numeric(year))

renal_prev_df <- prev_df %>% 
  filter(str_detect(string = source_file, pattern = 'Kidney'))%>% 
  mutate(year = as.numeric(year))


compute_mean_prevalence_by_year(past_populations,lung_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,colorectal_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,oral_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,pancreatic_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,uterine_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,blood_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,ovarian_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,osteogastric_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,prostate_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,female_breast_cancer,model_specification)
compute_mean_prevalence_by_year(past_populations,renal_cancer,model_specification)

plot_mean_prevalence_by_year(past_populations,lung_cancer,model_specification,lung_prev_df)
plot_mean_prevalence_by_year(past_populations,colorectal_cancer,model_specification,colorectal_prev_df)
plot_mean_prevalence_by_year(past_populations,oral_cancer,model_specification,oral_prev_df)
plot_mean_prevalence_by_year(past_populations,pancreatic_cancer,model_specification,pancreatic_prev_df)
######### !!!!!!!!!! pancreatic cancer prevalence not right !!!!!!!!!! ###########
plot_mean_prevalence_by_year(past_populations,uterine_cancer,model_specification,uterine_prev_df)
plot_mean_prevalence_by_year(past_populations,blood_cancer,model_specification,blood_prev_df)
plot_mean_prevalence_by_year(past_populations,ovarian_cancer,model_specification,ovarian_prev_df)
plot_mean_prevalence_by_year(past_populations,osteogastric_cancer,model_specification,osteogastric_prev_df)
plot_mean_prevalence_by_year(past_populations,prostate_cancer,model_specification,prostate_prev_df)
plot_mean_prevalence_by_year(past_populations,female_breast_cancer,model_specification,breast_prev_df)
plot_mean_prevalence_by_year(past_populations,renal_cancer,model_specification,renal_prev_df)


lung_prev_df
colorectal_prev_df
oral_prev_df
pancreatic_prev_df
uterine_prev_df
blood_prev_df
ovarian_prev_df
osteogastric_prev_df
prostate_prev_df
breast_prev_df
renal_prev_df

library(dplyr)
library(tidyr)
library(rlang)
library(echarts4r)

plot_mean_prevalence_by_year_echarts <- function(
    past_populations,
    cancer_col,
    model_specification,
    plot_df,
    title = NULL,
    model_name = "Model",
    ref_name   = "Reference"
) {
  cancer_col <- enquo(cancer_col)
  
  # Modelled prevalence/count series
  xxx <- past_populations %>%
    group_by(id, run) %>%
    arrange(year, .by_group = TRUE) %>%
    fill(death, death_reason, .direction = "down") %>%
    ungroup() %>%
    filter(is.na(death_reason)) %>%
    filter(!!cancer_col != 0) %>%
    count(year, run, name = "n") %>%
    complete(
      year = sort(unique(past_populations$year)),
      run  = seq_len(model_specification$model$number_of_runs),
      fill = list(n = 0)
    ) %>%
    group_by(year) %>%
    summarise(n = mean(n), .groups = "drop") %>%
    mutate(n = n * model_specification$population$scale_down_factor)
  
  # Optional sanity messages (same as yours)
  message(last(plot_df$prevalence) / first(xxx$n))
  message(last(plot_df$prevalence))
  message(first(xxx$n))
  
  # Prep reference series
  ref <- plot_df %>%
    select(year, prevalence) %>%
    mutate(prevalence = as.numeric(prevalence))
  
  # Build echarts
  title <- title %||% as_label(cancer_col)
  print(8)
  print(names(xxx))
  xxx %>%
    arrange(year) %>%
    e_charts(year) %>%
    e_line(n, name = 'model_name') %>%
    e_scatter(n, name = 'model_name') %>%
    # e_line(data = ref, prevalence, name = 'ref_name') #%>%
    # e_scatter(data = ref, prevalence, name = ref_name) %>%
    e_tooltip(trigger = "axis") %>%
    e_title(text = title, subtext = "Mean across runs (scaled) vs reference") %>%
    e_legend(show = F,bottom = 10) %>%
    e_x_axis(type = "category") %>%
    e_y_axis(min = 0) #%>%
    # e_axis_pointer(type = "cross")
}

lung_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, lung_cancer, model_specification, lung_prev_df)
colorectal_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, colorectal_cancer, model_specification, colorectal_prev_df)
oral_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, oral_cancer, model_specification, oral_prev_df)
pancreatic_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, pancreatic_cancer, model_specification, pancreatic_prev_df,title = "Pancreatic cancer (check prevalence logic)")
uterine_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, uterine_cancer, model_specification, uterine_prev_df)
blood_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, blood_cancer, model_specification, blood_prev_df)
ovarian_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, ovarian_cancer, model_specification, ovarian_prev_df)
osteogastric_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, osteogastric_cancer, model_specification, osteogastric_prev_df)
prostate_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, prostate_cancer, model_specification, prostate_prev_df)
female_breast_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, female_breast_cancer, model_specification, breast_prev_df)
renal_cancer_prevalence_echart <- plot_mean_prevalence_by_year_echarts(past_populations, renal_cancer, model_specification, renal_prev_df)





save(lung_cancer_prevalence_echart,
     colorectal_cancer_prevalence_echart,
     oral_cancer_prevalence_echart,
     pancreatic_cancer_prevalence_echart,
     uterine_cancer_prevalence_echart,
     blood_cancer_prevalence_echart,
     ovarian_cancer_prevalence_echart,
     osteogastric_cancer_prevalence_echart,
     prostate_cancer_prevalence_echart,
     female_breast_cancer_prevalence_echart,
     renal_cancer_prevalence_echart, 'site_cancers.RData')



