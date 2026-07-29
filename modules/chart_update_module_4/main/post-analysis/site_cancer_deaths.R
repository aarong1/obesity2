#deaths

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



past_populations %>%
  filter(death_reason == 'renal_cancer') %>%
  group_by(id,run) %>% 
  filter(year==first(year)) %>%
  ungroup() %>% 
  # filter(year >= death) %>%
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

compute_mean_deaths_by_year <- function( past_populations, cancer_col, model_specification) {
  
  cancer_col <- enquo(cancer_col)
  
  
  cancer_col <- enquo(cancer_col)
  cancer_col_str = as.character(cancer_col)[2]

  past_populations %>%
    filter(!!death_reason == cancer_col_str) %>%
    group_by(id,run) %>% 
    filter(year==first(year)) %>%
    ungroup() %>% 
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

plot_mean_deaths_by_year <- function( past_populations, cancer_col, model_specification, plot_df) {
  
  # cancer_str = as_string(cancer_col)
  # print(cancer_str)
  cancer_col <- enquo(cancer_col)
  cancer_col_str = as.character(cancer_col)[2]

  xxx <- past_populations %>%
    filter( death_reason == cancer_col_str) %>%
    group_by(id,run) %>% 
    filter(year==first(year)) %>%
    ungroup() %>%     
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
  
  message(last(plot_df$deaths)/first(xxx$n))
  message(last(plot_df$deaths))
  message(first(xxx$n))
  
  xxx %>% 
    ggplot(aes(x=year,y=n)) +
    geom_line()+
    geom_point()+
    geom_label(aes(x=year, label= n))+
    geom_line(data = plot_df, aes(x=year, y= deaths),color = 'blue')+
    geom_point(data = plot_df, aes(x=year, y= deaths), color = 'blue')+    
    geom_label(data = plot_df, aes(x=year, y= deaths, label= deaths), color = 'blue')
  
}

count(mortality_df,source_file) 

lung_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Lung')) %>% 
  mutate(year = as.numeric(year))

colorectal_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Colorectal')) %>% 
  mutate(year = as.numeric(year))

oral_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Oral')) %>% 
  mutate(year = as.numeric(year))

pancreatic_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Pancreatic')) %>% 
  mutate(year = as.numeric(year))

uterine_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Uterine')) %>% 
  mutate(year = as.numeric(year))

blood_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Blood')) %>% 
  group_by(year) %>% 
  summarise(deaths = sum(deaths))%>% 
  mutate(year = as.numeric(year))

ovarian_mortality_df <- mortality_df %>%
  filter(str_detect(string = source_file, pattern = 'Ovarian'))%>% 
  mutate(year = as.numeric(year))

osteogastric_mortality_df <- mortality_df %>%
  filter(str_detect(string = source_file, pattern = 'Stomach|Oesophageal')) %>% 
  group_by(year) %>% 
  summarise(deaths = sum(deaths)) %>%
  mutate(year = as.numeric(year))

prostate_mortality_df <- mortality_df %>%
  filter(str_detect(string = source_file, pattern = 'Prostate'))%>% 
  mutate(year = as.numeric(year))

breast_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Female_breast_cancer'))%>% 
  mutate(year = as.numeric(year))

renal_mortality_df <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Kidney'))%>% 
  mutate(year = as.numeric(year))

compute_mean_deaths_by_year(past_populations,lung_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,colorectal_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,oral_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,pancreatic_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,uterine_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,blood_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,ovarian_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,osteogastric_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,prostate_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,female_breast_cancer,model_specification)
compute_mean_deaths_by_year(past_populations,renal_cancer,model_specification)


plot_mean_deaths_by_year(past_populations,lung_cancer,model_specification,lung_mortality_df)
plot_mean_deaths_by_year(past_populations,colorectal_cancer,model_specification,colorectal_mortality_df)
plot_mean_deaths_by_year(past_populations,oral_cancer,model_specification,oral_mortality_df)
plot_mean_deaths_by_year(past_populations,pancreatic_cancer,model_specification,pancreatic_mortality_df)
plot_mean_deaths_by_year(past_populations,uterine_cancer,model_specification,uterine_mortality_df)
plot_mean_deaths_by_year(past_populations,blood_cancer,model_specification,blood_mortality_df)
plot_mean_deaths_by_year(past_populations,ovarian_cancer,model_specification,ovarian_mortality_df)
plot_mean_deaths_by_year(past_populations,osteogastric_cancer,model_specification,osteogastric_mortality_df)
plot_mean_deaths_by_year(past_populations,prostate_cancer,model_specification,prostate_mortality_df)
plot_mean_deaths_by_year(past_populations,female_breast_cancer,model_specification,breast_mortality_df)
plot_mean_deaths_by_year(past_populations,renal_cancer,model_specification,renal_mortality_df)


lung_mortality_df
colorectal_mortality_df
oral_mortality_df
pancreatic_mortality_df
uterine_mortality_df
blood_mortality_df
ovarian_mortality_df
osteogastric_mortality_df
prostate_mortality_df
breast_mortality_df
renal_mortality_df

