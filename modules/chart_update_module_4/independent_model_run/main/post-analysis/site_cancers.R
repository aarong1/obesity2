library(tidyverse)

cancer_data <- readRDS("outputs/cancer_population_3d_10yr.rds")
cancer_data_non_sex <- readRDS("outputs/cancer_population_all_10yr.rds")

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

(oral_cancer_incidence_df <- past_populations %>%
  filter(oral_cancer == year) %>%
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor))

(pancreatic_cancer_incidence_df <- past_populations %>%
  filter(pancreatic_cancer == year) %>% 
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor) )

(uterine_cancer_incidence_df <- past_populations %>%
  filter(uterine_cancer == year) %>% 
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  # group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor))

(blood_cancer_incidence_df <- past_populations %>%
  filter(blood_cancer == year) %>% 
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor) )

(ovarian_cancer_incidence_df <- past_populations %>%
  filter(ovarian_cancer == year) %>% 
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor))

(osteogastric_cancerincidence_df <- past_populations %>%
  filter(osteogastric_cancer == year) %>% 
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor) )

(female_breast_cancer_incidence_df <- past_populations %>%
  filter(female_breast_cancer == year) %>% 
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor))

(renal_cancer_incidence_df <- past_populations %>%
  filter(renal_cancer == year) %>% 
  count(year,run) %>% 
    complete(
      year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
      run  = 1:model_specification$model$number_of_runs,
      fill = list(n = 0)    ) %>%
  group_by(year) %>% 
  summarise(n=mean(n))%>%
    mutate(n=n*model_specification$population$scale_down_factor) )


####################################
# ORAL CANCER----
####################################
# past_populations %>%
#   count(year,stroke)

past_populations %>%
  count(year,oral_cancer)

past_populations %>%
  count(year, wt = oral_cancer_year_risk)

#incidence
past_populations %>%
    filter(oral_cancer == year) %>%
    count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
    group_by(year) %>% 
    summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
    # filter(oral_cancer == year) %>%
    count(year,oral_cancer,run) %>% 
    group_by(year,oral_cancer) %>% 
    summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalencee
past_populations %>%
  filter(oral_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)


cancer_data %>% 
  filter(cancer_site == 'Oral Cancer') %>% 
  summarise(sum(prevalence))


####################################
# PANCREATIC CANCER----
####################################

#!!!!!!!!!!!!!!!!!
# has incidence nearly equal to prevalence 
# very high
#!!!!!!!!!!!!!!!!!

past_populations %>%
  count(year,pancreatic_cancer)

past_populations %>%
  count(year, wt = pancreatic_cancer_year_risk)

#incidence
past_populations %>%
  filter(pancreatic_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  # filter(pancreatic_cancer == year) %>%
  count(year,pancreatic_cancer,run) %>% 
  group_by(year,pancreatic_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalencee
past_populations %>%
  filter(pancreatic_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data %>% 
  filter(cancer_site == 'Pancreatic Cancer') %>% 
  summarise(sum(prevalence))

####################################
# UTERINE CANCER----
####################################

past_populations %>%
  count(year,uterine_cancer)

past_populations %>%
  count(year, wt = uterine_cancer_year_risk)

#incidence
past_populations %>%
  filter(uterine_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),#model_specification$model$start_year + (seq_len(model_specification$model$duration) - 1),   # or sort(unique(past_populations$year))
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  # filter(uterine_cancer == year) %>%
  count(year,uterine_cancer,run) %>% 
  group_by(year,uterine_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalencee
past_populations %>%
  filter(uterine_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)


cancer_data_non_sex %>% 
  filter(cancer_site == 'Uterine Cancer') %>% 
  summarise(sum(prevalence))


####################################
# COLORECTAL CANCER----
####################################

#!!!!!!!!!!!!!!!!!
#still has a 10 year range of look backs 
#!!!!!!!!!!!!!!!!!

past_populations %>%
  count(year,colorectal_cancer)

past_populations %>%
  count(year, wt = colorectal_cancer_year_risk)

#incidence
past_populations %>%
  filter(colorectal_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,colorectal_cancer,run) %>% 
  group_by(year,colorectal_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalence
past_populations %>%
  filter(colorectal_cancer != 0) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data %>% 
  filter(cancer_site == 'Colorectal Cancer') %>% 
  summarise(sum(prevalence))


####################################
# LUNG CANCER----
####################################
#!!!!!!!!!!!!!!!!!
#has NAs
#!!!!!!!!!!!!!!!!!
past_populations %>%
  count(year,lung_cancer)

past_populations %>%
  count(year, wt = lung_cancer_year_risk)

#incidence
past_populations %>%
  filter(lung_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,lung_cancer,run) %>% 
  group_by(year,lung_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalence
past_populations %>%
  filter(lung_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data %>% 
  filter(cancer_site == 'Lung Cancer') %>% 
  summarise(sum(prevalence))

####################################
# OSTEOGASTRIC CANCER----
####################################

#!!!!!!!!!!!!!!!!!
#was missing prevalence
#!!!!!!!!!!!!!!!!!

past_populations %>%
  count(year,osteogastric_cancer)

past_populations %>%
  count(year, wt = osteogastric_cancer_year_risk)

#incidence
past_populations %>%
  filter(osteogastric_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,osteogastric_cancer,run) %>% 
  group_by(year,osteogastric_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalence
past_populations %>%
  filter(osteogastric_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data %>% 
  filter(cancer_site %in% c('Stomach Cancer', 'Oesophageal Cancer' )) %>%
  summarise(sum(prevalence))

####################################
# LIVER CANCER
####################################

# past_populations %>%
#   count(year,liver_cancer)
# 
# past_populations %>%
#   count(year, wt = liver_cancer_year_risk)
# 
# #incidence
# past_populations %>%
#   filter(liver_cancer == year) %>%
#   count(year,run) %>% 
#   complete(
#     year = sort(unique(past_populations$year)),
#     run  = 1:model_specification$model$number_of_runs,
#     fill = list(n = 0)    ) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n)) %>%
#   mutate(n=n * model_specification$population$scale_down_factor)

# # sum of year and incidence/prevalence
# past_populations %>%
#   count(year,liver_cancer,run) %>% 
#   group_by(year,liver_cancer) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)

# #prevalence
# past_populations %>%
#   filter(liver_cancer != 0) %>%
#   count(year,run) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)

# cancer_data %>% 
#   filter(cancer_site == 'Liver Cancer') %>% 
#   summarise(sum(prevalence))

####################################
# BRAIN CANCER
####################################

# past_populations %>%
#   count(year,brain_cancer)
# 
# past_populations %>%
#   count(year, wt = brain_cancer_year_risk)
# 
# #incidence
# past_populations %>%
#   filter(brain_cancer == year) %>%
#   count(year,run) %>% 
#   complete(
#     year = sort(unique(past_populations$year)),
#     run  = 1:model_specification$model$number_of_runs,
#     fill = list(n = 0)    ) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n)) %>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# # sum of year and incidence/prevalence
# past_populations %>%
#   count(year,brain_cancer,run) %>% 
#   group_by(year,brain_cancer) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# #prevalence
# past_populations %>%
#   filter(brain_cancer != 0) %>%
#   count(year,run) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# cancer_data %>% 
#   filter(cancer_site == 'Brain Cancer') %>% 
#   summarise(sum(prevalence))


####################################
# BLADDER CANCER
####################################

# past_populations %>%
#   count(year,bladder_cancer)
# 
# past_populations %>%
#   count(year, wt = bladder_cancer_year_risk)
# 
# #incidence
# past_populations %>%
#   filter(bladder_cancer == year) %>%
#   count(year,run) %>% 
#   complete(
#     year = sort(unique(past_populations$year)),
#     run  = 1:model_specification$model$number_of_runs,
#     fill = list(n = 0)    ) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n)) %>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# # sum of year and incidence/prevalence
# past_populations %>%
#   count(year,bladder_cancer,run) %>% 
#   group_by(year,bladder_cancer) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# #prevalence
# past_populations %>%
#   filter(bladder_cancer != 0) %>%
#   count(year,run) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# cancer_data %>% 
#   filter(cancer_site == 'Bladder Cancer') %>% 
#   summarise(sum(prevalence))


####################################
# BLOOD ALL
####################################

past_populations %>%
  count(year,blood_cancer)

past_populations %>%
  count(year, wt = blood_cancer_year_risk)

#incidence
past_populations %>%
  filter(blood_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,blood_cancer,run) %>% 
  group_by(year,blood_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalence
past_populations %>%
  filter(blood_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data %>% 
  # filter(cancer_site == 'Blood Leukaemia') %>% 
  filter(str_starts(cancer_site, 'Blood')) %>% #count(cancer_site)
  summarise(sum(prevalence))

####################################
# OVARIAN CANCER
####################################

past_populations %>%
  count(year,ovarian_cancer)

past_populations %>%
  count(year, wt = ovarian_cancer_year_risk)

#incidence
past_populations %>%
  filter(ovarian_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,ovarian_cancer,run) %>% 
  group_by(year,ovarian_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalence
past_populations %>%
  filter(ovarian_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data_non_sex %>% 
  filter(cancer_site == 'Ovarian Cancer') %>%
  # filter(str_starts(cancer_site, 'varian')) %>% #count(cancer_site)
  summarise(sum(prevalence))

####################################
# PROSTATE CANCER
####################################

#!!!!!!!!!!!!!!!!!
# prostate cancer has NAs investigate #
#!!!!!!!!!!!!!!!!!

past_populations %>%
  count(year,prostate_cancer)

past_populations %>%
  count(year, wt = prostate_cancer_year_risk)

#incidence
past_populations %>%
  filter(prostate_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,prostate_cancer,run) %>% 
  group_by(year,prostate_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalence
past_populations %>%
  filter(prostate_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data_non_sex %>% 
  filter(cancer_site == 'Prostate Cancer') %>%
  # filter(str_starts(cancer_site, 'varian')) %>% #count(cancer_site)
  summarise(sum(prevalence))


####################################
# FEMALE BREAST CANCER
####################################

past_populations %>%
  count(year,female_breast_cancer)

past_populations %>%
  count(year, wt = female_breast_cancer_year_risk)

#incidence
past_populations %>%
  filter(female_breast_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,female_breast_cancer,run) %>% 
  group_by(year,female_breast_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

# prevalence
past_populations %>%
  filter(female_breast_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data_non_sex %>% 
  filter(cancer_site == 'Female Breast Cancer') %>%
  summarise(sum(prevalence))

####################################
# RENAL CANCER
####################################

#!!!!!!!!!!!!!!!!!
# renal cancer not populating 2023 !!1
#!!!!!!!!!!!!!!!!!

past_populations %>%
  count(year,renal_cancer)

past_populations %>%
  count(year, wt = renal_cancer_year_risk)

#incidence
past_populations %>%
  filter(renal_cancer == year) %>%
  count(year,run) %>% 
  complete(
    year = sort(unique(past_populations$year)),
    run  = 1:model_specification$model$number_of_runs,
    fill = list(n = 0)    ) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>%
  mutate(n=n * model_specification$population$scale_down_factor)

# sum of year and incidence/prevalence
past_populations %>%
  count(year,renal_cancer,run) %>% 
  group_by(year,renal_cancer) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

#prevalence
past_populations %>%
  filter(renal_cancer != 0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%
  mutate(n=n * model_specification$population$scale_down_factor)

cancer_data %>% 
  filter(cancer_site == 'Female Breast Cancer') %>%
  # filter(str_starts(cancer_site, 'varian')) %>% #count(cancer_site)
  summarise(sum(prevalence))

####################################
# BLOOD LYMPHOMA
####################################

# past_populations %>%
#   count(year,blood_lymphoma)
# 
# past_populations %>%
#   count(year, wt = blood_lymphoma_year_risk)
# 
# #incidence
# past_populations %>%
#   filter(blood_lymphoma == year) %>%
#   count(year,run) %>% 
#   complete(
#     year = sort(unique(past_populations$year)),
#     run  = 1:model_specification$model$number_of_runs,
#     fill = list(n = 0)    ) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n)) %>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# # sum of year and incidence/prevalence
# past_populations %>%
#   count(year,blood_lymphoma,run) %>% 
#   group_by(year,blood_lymphoma) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# #prevalence
# past_populations %>%
#   filter(blood_lymphoma != 0) %>%
#   count(year,run) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# cancer_data %>% 
#   filter(cancer_site == 'Blood Lymphoma') %>% 
#   summarise(sum(prevalence))


####################################
# OESOPHAGEAL CANCER
####################################

# past_populations %>%
#   count(year,oesophageal_cancer)
# 
# past_populations %>%
#   count(year, wt = oesophageal_cancer_year_risk)
# 
# #incidence
# past_populations %>%
#   filter(oesophageal_cancer == year) %>%
#   count(year,run) %>% 
#   complete(
#     year = sort(unique(past_populations$year)),
#     run  = 1:model_specification$model$number_of_runs,
#     fill = list(n = 0)    ) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n)) %>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# # sum of year and incidence/prevalence
# past_populations %>%
#   count(year,oesophageal_cancer,run) %>% 
#   group_by(year,oesophageal_cancer) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# #prevalence
# past_populations %>%
#   filter(oesophageal_cancer != 0) %>%
#   count(year,run) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# cancer_data %>% 
#   filter(cancer_site == 'Oesophageal Cancer') %>% 
#   summarise(sum(prevalence))


####################################
# GALLBLADDER AND OTHER BILIARY CANCER
####################################

# past_populations %>%
#   count(year,gallbladder_and_other_biliary_cancer)
# 
# past_populations %>%
#   count(year, wt = gallbladder_and_other_biliary_cancer_year_risk)
# 
# #incidence
# past_populations %>%
#   filter(gallbladder_and_other_biliary_cancer == year) %>%
#   count(year,run) %>% 
#   complete(
#     year = sort(unique(past_populations$year)),
#     run  = 1:model_specification$model$number_of_runs,
#     fill = list(n = 0)    ) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n)) %>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# # sum of year and incidence/prevalence
# past_populations %>%
#   count(year,gallbladder_and_other_biliary_cancer,run) %>% 
#   group_by(year,gallbladder_and_other_biliary_cancer) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# #prevalence
# past_populations %>%
#   filter(gallbladder_and_other_biliary_cancer != 0) %>%
#   count(year,run) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# cancer_data %>% 
#   filter(cancer_site == 'Gallbladder And Other Biliary Cancer') %>% 
#   summarise(sum(prevalence))


####################################
# STOMACH CANCER
####################################

# past_populations %>%
#   count(year,stomach_cancer)
# 
# past_populations %>%
#   count(year, wt = stomach_cancer_year_risk)
# 
# #incidence
# past_populations %>%
#   filter(stomach_cancer == year) %>%
#   count(year,run) %>% 
#   complete(
#     year = sort(unique(past_populations$year)),
#     run  = 1:model_specification$model$number_of_runs,
#     fill = list(n = 0)    ) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n)) %>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# # sum of year and incidence/prevalence
# past_populations %>%
#   count(year,stomach_cancer,run) %>% 
#   group_by(year,stomach_cancer) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# #prevalence
# past_populations %>%
#   filter(stomach_cancer != 0) %>%
#   count(year,run) %>% 
#   group_by(year) %>% 
#   summarise(n=mean(n))%>%
#   mutate(n=n * model_specification$population$scale_down_factor)
# 
# cancer_data %>% 
#   filter(cancer_site == 'Stomach Cancer') %>% 
#   summarise(sum(prevalence))


####################################

past_populations %>%
  count(year,run,osteoarthritis) %>% 
  group_by(year,osteoarthritis) %>% 
  summarise(mean(n))

past_populations %>%
  count(year, run,wt = osteoarthritis_year_risk) %>% 
  group_by(year) %>% 
  summarise(mean(n))

past_populations %>%
  count(year,rheumatoid_arthritis)
past_populations %>%
  count(year, wt = rheumatoid_arthritis_year_risk)



rheumatoid_arthritis_risk_sum_df <- past_populations %>%
  # filter(rheumatoid_arthritis == year) %>%
  count(year,run,wt = rheumatoid_arthritis_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor)

osteoporosis_risk_sum_df <- past_populations %>%
  # filter(osteoporosis == year) %>% 
  count(year,run,wt = osteoporosis_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor)

osteoarthritis_risk_sum_df <- past_populations %>%
  # filter(osteoarthritis == year) %>% 
  count(year,run,wt = osteoarthritis_year_risk) %>% 
  group_by(year) %>% 
  summarise(n=mean(n))%>%mutate(n=n*model_specification$population$scale_down_factor)

ggplot() +
  geom_point(data = rheumatoid_arthritis_incidence_df, aes(year,n))+
  geom_point(data = rheumatoid_arthritis_risk_sum_df, aes(year,n),colour='blue')+
  ylim(c(0,NA))

ggplot() +
  geom_point(data = osteoarthritis_incidence_df, aes(year,n))+
  geom_point(data = osteoarthritis_risk_sum_df, aes(year,n),colour='blue')+
  ylim(c(0,NA))

ggplot() +
  geom_point(data = osteoporosis_incidence_df, aes(year,n))+
  geom_point(data = osteoporosis_risk_sum_df, aes(year,n),colour='blue')+
  ylim(c(0,NA))

####-----------

# initial_time_zero_population %>%
#   apply_osteoarthritis_prevalence() %>%
#   ungroup() %>%
#   count(osteoarthritis)

# initial_time_zero_population %>%
#   apply_osteoarthritis_risk(osteoarthritis_incidence) %>%
#   transmute( sex, age_group = cut(age,breaks = c(-Inf,20,30,40,50,60,70,80,90, Inf),
#                                     labels =c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-110")
#   ),
#   osteoarthritis_year_risk,
#             rank(osteoarthritis_year_risk),
#             max(frank(osteoarthritis_year_risk))) %>%
#   mutate(osteoarthritis_percentile = frank(osteoarthritis_year_risk)/max(frank(osteoarthritis_year_risk))) %>%
#   left_join(osteoarthritis_prevalence,relationship = 'many-to-one',by=c('age_group', sex = 'sex')) %>%
#   mutate(osteoarthritis = ifelse(runif(n()) < osteoarthritis_prevalence_prob/0.5 * osteoarthritis_percentile ,2023,0)) %>% count(osteoarthritis)

# initial_time_zero_population %>% count(osteoarthritis_percentile)

past_populations %>%
  filter(osteoarthritis !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor) %>% 
  ggplot()+
  geom_point(aes(year,n))

past_populations %>%
  # filter(osteoarthritis !=0) %>% 
  count(year,rheumatoid_arthritis,run) %>% 
  group_by(year, rheumatoid_arthritis) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

past_populations %>%
  # filter(osteoarthritis !=0) %>% 
  count(year,osteoporosis,run) %>% 
  group_by(year, osteoporosis) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

past_populations %>%
  # filter(osteoarthritis !=0) %>% 
  count(year,osteoarthritis,run) %>% 
  group_by(year, osteoarthritis) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

past_populations %>%
  filter(rheumatoid_arthritis !=0) %>% 
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n=n*model_specification$population$scale_down_factor) %>% 
  full_join(actual_rheumatoid_arthritis_perv) %>% 
  ggplot()+
  geom_point(aes(year,Count),col='blue') +
  geom_point(aes(year,n))

past_populations %>% #count(osteoporosis)
  filter(osteoporosis !=0) %>%
  count(year,run) %>% 
  group_by(year) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor) %>% 
  # mutate(prev=cumsum(n)) %>% 
  full_join(actual_osteoporosis_perv) %>% 
  ggplot()+
  geom_point(aes(year,Count),col='blue') +
  geom_point(aes(year,n))

past_populations %>% #count(osteoporosis)
  # filter(epilepsy !=0) %>% 
  count(year,epilepsy,run) %>% 
  group_by(year,epilepsy) %>% 
  summarise(n=mean(n)) %>% 
  mutate(n = n*model_specification$population$scale_down_factor)

actual_osteoporosis_perv <- prevalence %>% 
  filter(Disease == 'Osteoporosis') %>% 
  filter(!is.na(Count)) %>% 
  mutate(year = as.numeric(str_extract(string = Year, group=0, pattern = '[0-9]*')))

actual_rheumatoid_arthritis_perv <- prevalence %>% 
  filter(Disease == 'Rheumatoid Arthritis') %>% 
  filter(!is.na(Count)) %>% 
  mutate(year = as.numeric(str_extract(string = Year, group=0, pattern = '[0-9]*')))


