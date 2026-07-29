 # main_depression
library(fst)
library(tidyverse)
library(readxl)

pop <- read.fst('./main/initial_time_zero_population10down.fst')

ppp <- read_xlsx(path =  'data/ni/ni_ppp_machine_readable.xlsx', sheet = 'Mortality_assumptions')

names(ppp)[-c(1,2)] <- str_extract( pattern = '[0-9]*', string = names(ppp)[-c(1,2)])

names(ppp)[1] <- 'sex'
names(ppp)[2] <- 'age'

# https://www.thelancet.com/journals/landia/article/PIIS2213-8587(18)30288-2/fulltext
# supp

all_cause_rr_df <- data.frame(
  rr = c(1, 2.10),
  depressed = c( 'normal', 'depressed')
)

# Relative risk values from literature
# suicide_rr_df: 9.89
# natural_causes: 1.63

anti_depressive_prescription <- tribble(
  ~age_band, ~Males, ~Females,
  '0-17', 0.5, 0.8,
  '18-24', 8.8, 17.3,
  '25-34', 15.2, 24.0,
  '35-44', 19.7, 30.5,
  '45-64', 24.9, 39.6,
  '65-74', 24.5, 38.8,
  '75-84', 22.9, 37.0,
  '85-110', 23.1, 34.6
) %>%
  mutate(
    age_min = as.numeric(str_extract(age_band, "^\\d+")),
    age_max = ifelse(str_detect(age_band, "\\+"), 120, as.numeric(str_extract(age_band, "\\d+$")))
  ) %>%
  pivot_longer(cols = c(Males, Females), names_to = "sex", values_to = "prescription_pct") %>%
  mutate(prescription_prob = prescription_pct / 100)

# Function to populate synthetic population with anti-depression prescriptions
populate_antidepressant_prescription <- function(population_df, prescription_data = anti_depressive_prescription) {
  
  # Create a copy to avoid modifying the original
  pop_with_rx <- population_df %>%
    mutate(
      # Assign age band for matching with prescription data
      age_band = case_when(
        age >= 0 & age <= 17 ~ "0-17",
        age >= 18 & age <= 24 ~ "18-24",
        age >= 25 & age <= 34 ~ "25-34",
        age >= 35 & age <= 44 ~ "35-44",
        age >= 45 & age <= 64 ~ "45-64",
        age >= 65 & age <= 74 ~ "65-74",
        age >= 75 & age <= 84 ~ "75-84",
        age >= 85 ~ "85-110",
        TRUE ~ NA_character_
      )
    )
  
  # Join with prescription probabilities
  pop_with_rx <- pop_with_rx %>%
    left_join(
      prescription_data %>% select(age_band, sex, prescription_prob),
      by = c("age_band", "sex")
    ) %>%
    mutate(
      # Adjust probability based on wellbeing status
      # Those with poor wellbeing are more likely to have prescriptions
      # Those with good wellbeing are less likely
      adjusted_prob = case_when(
        is.na(wellbeing) | is.na(prescription_prob) ~ 0,  # No prescription if no wellbeing data
        wellbeing == "poor_wellbeing" ~ prescription_prob * 2.5,  # 2.5x more likely
        wellbeing == "moderate_wellbeing" ~ prescription_prob * 1.5,  # 1.5x more likely
        wellbeing == "good_wellbeing" ~ prescription_prob * 0.3,  # Much less likely
        TRUE ~ prescription_prob
      ),
      # Cap probability at 1.0
      adjusted_prob = pmin(adjusted_prob, 1.0),
      # Assign prescription status probabilistically
      # Note: Children rarely receive antidepressants, so age 0-17 rates are very low
      antidepressant_prescription = runif(n()) < adjusted_prob
    ) %>%
    select(-age_band, -prescription_prob, -adjusted_prob)
  
  return(pop_with_rx)
}

# Example usage:
# Load your synthetic population
# pop <- read.fst('./synthetic_population/pop.fst')
# 
# # Populate with antidepressant prescriptions
# pop_with_prescriptions <- populate_antidepressant_prescription(pop)
# 
# # Save the result
# write.fst(pop_with_prescriptions, './synthetic_population/pop_with_antidepressants.fst')
# 
# # Check the results
# pop_with_prescriptions %>%
#   group_by(sex, wellbeing) %>%
#   summarise(
#     n = n(),
#     pct_prescribed = mean(antidepressant_prescription, na.rm = TRUE) * 100
#   )

# Note: Antidepressant use (versus no antidepressant use) 
# significantly lower all‐cause mortality in people with depression 
# Hazard ratio: 0.79

# Populate depression for children and adolescents
# Point prevalence: 2.8% children, 5.6% adolescents
# Wellbeing is populated for 16+ only, so we need to add it for younger ages
pop <- pop %>%
  mutate(
    wellbeing = case_when(
      # Keep existing wellbeing for 16+ (already populated)
      age >= 16 & !is.na(wellbeing) ~ wellbeing,
      
      # Children (ages 0-12): 2.8% prevalence
      age < 4  ~'good_wellbeing',
      
      age >= 4 & age <= 12 ~ ifelse(runif(n()) < 0.028, 'poor_wellbeing', 'good_wellbeing'),
      
      # Adolescents (ages 13-15): 5.6% prevalence
      age >= 13 & age <= 15 ~ ifelse(runif(n()) < 0.056, 'poor_wellbeing', 'good_wellbeing'),
      
      # Default to good wellbeing if somehow missing
      TRUE ~ 'good_wellbeing'
    )
  ) %>% 
  replace_na(list(wellbeing='good_wellbeing'))

pop <- populate_antidepressant_prescription(pop)

suicide_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                            sheet = "Table 6.4a", range = "AA4:AY845")%>%
  mutate(sex ='Males') %>% 
  filter(Block == 'Intentional self-harm (X60-X84)'& 
           ICD != 'All')

suicide_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                              sheet = "Table 6.4a", range = "BA4:BY845") %>%
  mutate(sex ='Females') %>% 
  filter(Block == 'Intentional self-harm (X60-X84)'& 
           ICD != 'All')

suicide_females
# A tibble: 21 × 26
# Chapter   Block ICD   Description `All Ages`   `0` `1-4` `5-9` `10-14` `15-19` `20-24`
# <chr>     <chr> <chr> <chr>            <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
#   1 Chapter … Inte… X60   Intentiona…          0     0     0     0       0       0       0
# 2 Chapter … Inte… X61   Intentiona…          0     0     0     0       0       0       0
# 3 Chapter … Inte… X62   Intentiona…          0     0     0     0       0       0       0
# 4 Chapter … Inte… X63   Intentiona…          0     0     0     0       0       0       0
# 5 Chapter … Inte… X64   Intentiona…          5     0     0     0       0       0       0
# 6 Chapter … Inte… X66   Intentiona…          0     0     0     0       0       0       0
# 7 Chapter … Inte… X67   Intentiona…          0     0     0     0       0       0       0
# 8 Chapter … Inte… X68   Intentiona…          0     0     0     0       0       0       0
# 9 Chapter … Inte… X69   Intentiona…          0     0     0     0       0       0       0
# 10 Chapter … Inte… X70   Intentiona…         37     0     0     0       1       3  


natural_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                            sheet = "Table 6.4a", range = "AA4:AY845")%>%
  mutate(sex ='Males') %>% 
  filter(substr(ICD, 1, 1) != "V" &
           substr(ICD, 1, 1) != "W" &
           substr(ICD, 1, 1) != "X" &
           substr(ICD, 1, 1) != "Y"& 
           ICD != 'All')
natural_males
# Chapter   Block ICD   Description `All Ages`   `0` `1-4` `5-9` `10-14` `15-19` `20-24`
# <chr>     <chr> <chr> <chr>            <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>
#   1 Chapter … Inte… A02   "Other sal…          0     0     0     0       0       0       0
#  2 Chapter … Inte… A04   "Other bac…         11     0     0     0       0       0       0
# 3 Chapter … Inte… A08   "Viral and…          0     0     0     0       0       0       0
#  4 Chapter … Inte… A09   "Diarrhoea…         14     0     0     0       0       0       0
# 5 Chapter … Tube… A16   "Respirato…          2     0     0     0       0       0       0
#  6 Chapter … Tube… A17   "Tuberculo…          0     0     0     0       0       0       0
# 7 Chapter … Tube… A18   "Tuberculo…          0     0     0     0       0       0       0
#  8 Chapter … Tube… A19   "Miliary t…          0     0     0     0       0       0       0
# 9 Chapter … Othe… A31   "Infection…          2     0     0     0       0       0       0
# 10 Chapter … Othe… A32   "Listerios…          1     0     0     0       0       0       0

natural_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                              sheet = "Table 6.4a", range = "BA4:BY845") %>%
  mutate(sex ='Females') %>% 
  filter(substr(ICD, 1, 1) != "V" &
           substr(ICD, 1, 1) != "W" &
           substr(ICD, 1, 1) != "X" &
           substr(ICD, 1, 1) != "Y" & 
           ICD != 'All')


# Load all deaths (including 'All' row for totals)
all_deaths_males <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                               sheet = "Table 6.4a", range = "AA4:AY845")%>%
  mutate(sex ='Males') %>% 
  filter(ICD == 'All')

all_deaths_females <- read_excel("data/registrar_general_annual_reports/Section 6 - Cause_Death_Tables_2023.xlsx", 
                                 sheet = "Table 6.4a", range = "BA4:BY845") %>%
  mutate(sex ='Females') %>% 
  filter(ICD == 'All')


# Process suicide deaths by age and sex
suicide_deaths <- bind_rows(suicide_males, suicide_females) %>%
  select(sex, ICD, Description, `0`:`90+`) %>%
  pivot_longer(cols = `0`:`90+`, names_to = 'age_group', values_to = 'deaths') %>%
  group_by(sex, age_group) %>%
  summarise(suicide_deaths = sum(deaths, na.rm = TRUE), .groups = 'drop')

# Process natural deaths by age and sex
natural_deaths <- bind_rows(natural_males, natural_females) %>%
  select(sex, ICD, Description, `0`:`90+`) %>%
  pivot_longer(cols = `0`:`90+`, names_to = 'age_group', values_to = 'deaths') %>%
  group_by(sex, age_group) %>%
  summarise(natural_deaths = sum(deaths, na.rm = TRUE), .groups = 'drop')

# Process total deaths
total_deaths <- bind_rows(all_deaths_males, all_deaths_females) %>%
  select(sex, `0` :`90+`) %>%
  pivot_longer(cols = `0`:`90+`, names_to = 'age_group', values_to = 'total_deaths')

# Get population by age and sex from synthetic population
recode_age_groups <- function(pop){
pop <- pop %>%
  mutate(age_group = case_when(
    age == 0 ~ "0",
    age >= 1 & age <= 4 ~ "1-4",
    age >= 5 & age <= 9 ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    age >= 90 ~ "90+",
    TRUE ~ "All Ages"
  )) 
return(pop)
}

pop <- recode_age_groups(pop)

population_by_age <- pop %>% 
  count(sex, age_group, name = 'population') %>%
  mutate(population = population *10)  

# Combine death types with population
death_rates_by_age <- total_deaths %>%
  full_join(natural_deaths, by = c('sex', 'age_group')) %>%
  full_join(suicide_deaths, by = c('sex', 'age_group')) %>%
  left_join(population_by_age, by = c('sex', 'age_group')) %>%
  replace_na(list(suicide_deaths = 0, natural_deaths = 0, total_deaths = 0)) %>%
  filter(age_group != 'All Ages') %>% 
  mutate(

    qx_suicide =  suicide_deaths / population, 
    qx_not_suicide =  (total_deaths - suicide_deaths) / population, 
    
    qx_natural = natural_deaths / population, 
    qx_not_natural = (total_deaths - natural_deaths) / population, 
    
    qx_total =  total_deaths / population, 
    qx_surv =  (population - total_deaths) / population,
    
    qx_other =  (total_deaths - suicide_deaths - natural_deaths) / population
  )

pop <- pop %>% 
  left_join(death_rates_by_age)

pop <- pop %>% 
  mutate(depressed = ifelse(wellbeing == 'poor_wellbeing','depressed','healthy'))

paf <- pop %>%
  count(age_group,sex, depressed  ) %>% 
  mutate(n = n*10) %>% 
  # filter(!is.na(depressed)) %>% 
  pivot_wider(id_cols = c(age_group,sex),
              names_from = depressed,
              values_from = n,values_fill = 0) %>% # View()
  mutate(paf_natural = (1 - (depressed + healthy)/ (depressed * 1.63 + healthy * 1  ) ) ) %>%
  mutate(paf_suicide = (1 - (depressed + healthy)/ (depressed * 9.89 + healthy * 1 ) ) ) %>%
  mutate(paf_all_cause = (1 - (depressed + healthy)/ (depressed * 2.10  + healthy * 1 ) ) ) %>%
  mutate(paf_all_cause_comorbid_matched = (1 - (depressed + healthy)/ (depressed * 1.29 + healthy * 1 ) ) ) 

paf_min_df <- paf[c('sex',
                    'age_group',
                    'paf_suicide',
                    'paf_natural',
                    'paf_all_cause',
                    'paf_all_cause_comorbid_matched')] %>% 
  left_join(death_rates_by_age[c('age_group',
                                 'sex',
                                 'qx_suicide',
                                 'qx_other',
                                 'qx_natural',
                                 'qx_total')],
            by = c('sex', 'age_group')) %>% 
  mutate(q_min_suicide = qx_suicide * (1- paf_suicide )) %>% 
  mutate(q_min_natural = qx_natural * (1- paf_natural )) %>% 
  mutate(q_min_all_cause = qx_total * (1- paf_all_cause )) %>% 
  mutate(q_min_all_cause_comorbid_matched  = qx_total * (1- paf_all_cause_comorbid_matched)) %>% 
  mutate(q_min_other  = qx_other * (1))


pp <- ppp %>% 
  filter(age!= 'Birth') %>% 
  mutate(age = as.numeric(age)) %>% 
  select(qx = `2024`, sex, age) %>% 
  mutate(qx=qx/100000)

# ex     age   `2022` `2023` `2024` `2025` `2026` `2027` `2028` `2029` `2030` `2031`
# <chr>   <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#   1 Females Birth 203.   193.   187.   181.   176.   172.   167.   164.   160.   157.  
# 2 Females 0      44.6   42.2   40.9   39.7   38.6   37.6   36.7   35.8   35.0   34.3 
# 3 Females 1      14.1   13.3   12.9   12.5   12.2   11.8   11.5   11.3   11.0   10.8 
# 4 Females 2       9.70   9.18   8.90   8.63   8.37   8.15   7.94   7.76   7.58   7.42
# 5 Females 3       8.11   7.68   7.45   7.23   7.01   6.83   6.66   6.50   6.35   6.22
# 6 Females 4       7.32   6.92   6.71   6.52   6.33   6.17   6.01   5.87   5.74   5.62
# 7 Females 5       6.60   6.22   6.03   5.85   5.69   5.55   5.41   5.29   5.17   5.06
# 8 Females 6       5.68   5.35   5.18   5.02   4.88   4.76   4.65   4.54   4.45   4.36
# 9 Females 7       4.84   4.56   4.40   4.26   4.14   4.04   3.95   3.87   3.78   3.71
# 10 Females 8       4.55   4.29   4.14   4.01   3.89   3.79   3.71   3.63   3.56   3.49
  
 y <-  pop %>% 
  select(id,age,  age_group, sex, year, bmi, depressed) %>% 
  mutate(year = as.character(year)) %>% 
  left_join(paf_min_df[
    c(
      'sex', 
      'age_group',
      'q_min_suicide',
      'q_min_natural',
      'q_min_all_cause',
      'q_min_other',
      'q_min_all_cause_comorbid_matched')
  ],
  relationship = "many-to-one" ,
  by = c('age_group','sex')) %>% 
  left_join(pp) %>% 
  
  mutate(rr_all_cause_comorbid_matched = case_when(
    depressed == 'depressed'  ~ 1.29,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>%
  
  mutate(rr_all_cause = case_when(
    depressed == 'depressed' ~ 2.1,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>%
  
  mutate(rr_suicide = case_when(
    depressed == 'depressed'  ~ 9.89,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>% 
  
  mutate(rr_natural = case_when(
    depressed == 'depressed'  ~   1.63,
    depressed == 'healthy'  ~ 1,
    T  ~ 1)) %>% 
  
  mutate(rr = rr_all_cause_comorbid_matched ) %>% 
  mutate(q_min =  q_min_all_cause_comorbid_matched) %>% 
  
  mutate(q_modelled_all_cause = q_min_all_cause * rr_all_cause ) %>%
  mutate(q_modelled_all_cause_comorbid_matched = q_min_all_cause_comorbid_matched * rr_all_cause_comorbid_matched ) %>%
  mutate(q_modelled_suicide = q_min_suicide * rr_suicide ) %>%
  mutate(q_modelled_natural = q_min_natural * rr_natural ) %>%
  mutate(q_modelled_other = q_min_other * 1 ) %>%
  
  # mutate(q_modelled_other = max(0,q_modelled_all_cause - q_modelled_natural - q_modelled_suicide) )%>% 
  # mutate(q_modelled_other = q_modelled_all_cause - q_modelled_natural - q_modelled_suicide) %>% 
   #filter(q_modelled_other<0) %>% View()
  
  mutate(q_surv = 1 - q_modelled_all_cause ) %>% 
  mutate(q_surv = 1 - q_modelled_other -  q_modelled_natural - q_modelled_suicide) %>% 
    
  rowwise() %>% 
  mutate(q_all = list(list(q_modelled_other = q_modelled_other,
                           q_modelled_natural = q_modelled_natural,
                           q_modelled_suicide = q_modelled_suicide,
                           q_surv = q_surv))) %>%
  ungroup() %>% 
  # mutate(q_modelled = q_min * rr ) %>%
  # mutate(q_modelled = q_min * rr_intervene ) %>%
  # mutate(bern_trial = runif( n = n() )) %>% 
  # mutate(death = (bern_trial < q_modelled )) %>% 
  rowwise() %>% 
  mutate(death = sample( sample(size = 1, x = names(q_all),prob = unlist(q_all)) ) ) %>% 
  ungroup()

y %>%
  count(death)

y %>% 
  slice_sample(prop = 0.01) %>% 
  mutate(q_rec = q_modelled_other + q_modelled_natural + q_modelled_suicide + q_surv) %>% 
  # mutate(q_rec = qx ) %>%
  
  # group_by(sex) %>%
  e_charts(age) %>% 
  
  e_scatter(q_modelled_natural) %>%
  e_scatter(q_modelled_suicide) %>% 
  e_scatter(q_modelled_other) %>%
  
  e_scatter(q_surv) %>% 
  e_scatter(q_rec) %>% 
  e_scatter(qx) %>% 
  
  e_tooltip() %>% 
  e_theme('walden')

pop1 <- pop %>% 
  slice_sample(prop=0.1)

pop_d = data.frame(); pop_a = data.frame(); for( k in  1:20){ #1:20
  
  message(k)
  
  for(j in c('intervention','non-intervention')){
    
    message(j)
    # pop1 <- pop
    
    pop1 <- pop %>% 
      slice_sample(prop = 0.1)
    
    for( i in 2023:2040){#2023:2050
      
      message(i)
      pop1 <- pop1 %>%
        reindex_risk_percentile() %>% 
        apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) %>% 
        mutate(
          wellbeing = case_when(
            age >= 16 & !is.na(wellbeing) ~ wellbeing,
            age < 4  ~'good_wellbeing',
            age >= 4 & age <= 12 ~ ifelse(runif(n()) < 0.028, 'poor_wellbeing', 'good_wellbeing'),
            age >= 13 & age <= 15 ~ ifelse(runif(n()) < 0.056, 'poor_wellbeing', 'good_wellbeing'),
            TRUE ~ 'good_wellbeing'
          )
        ) %>% 
        replace_na(list(wellbeing='good_wellbeing')) %>% 
        mutate(depressed = ifelse(wellbeing == 'poor_wellbeing','depressed','healthy')) %>% 
        select(-any_of(
                c('q_modelled_all_cause',
                  'q_modelled_all_cause_comorbid_matched',
                  'q_modelled_suicide',
                  'q_modelled_natural',
                  'q_modelled_other',
                  'q_min_all_cause',
                  'q_min_suicide',
                  'q_min_natural',
                  'q_min_other')
        ))
      
      pop2 <- pop1 %>% 
        select(id,age, 
               mdm_quintile_soa_name, HSCT, Urban_mixed_rural_status, 
               age_group,depression_percentile, sex, year, bmi, depressed) %>% 
        mutate(year = as.character(i)) %>% 
        mutate(intervene = j) %>%
        mutate(age = age + 1) %>% 
        recode_age_groups(.)
      
      pop2 <- pop2 %>% 
        left_join(paf_min_df[
          c(
            'sex', 
            'age_group',
            'q_min_suicide',
            'q_min_natural',
            'q_min_all_cause',
            'q_min_all_cause_comorbid_matched',
            'q_min_other')
        ],
        relationship = "many-to-one" ,
        by = c('age_group','sex')) %>% 

        left_join(pp) %>% 
        
        mutate(rr_all_cause_comorbid_matched = case_when(
          depressed == 'depressed' ~ 1.29,
          depressed == 'healthy'  ~ 1,
          T  ~ 1)) %>%
        
        mutate(rr_all_cause = case_when(
          depressed == 'depressed' ~ 2.1,
          depressed == 'healthy'  ~ 1,
          T  ~ 1)) %>%
        
        mutate(rr_suicide = case_when(
          depressed == 'depressed'  ~ 9.89,
          depressed == 'healthy'  ~ 1,
          T  ~ 1)) %>% 
        
        mutate(rr_natural = case_when(
          depressed == 'depressed'  ~   1.63,
          depressed == 'healthy'  ~ 1,
          T  ~ 1)) %>% 
        
        mutate(q_modelled_all_cause = q_min_all_cause * rr_all_cause ) %>%
        mutate(q_modelled_all_cause_comorbid_matched = q_min_all_cause_comorbid_matched * rr_all_cause_comorbid_matched ) %>%
        mutate(q_modelled_suicide = q_min_suicide * rr_suicide ) %>%
        mutate(q_modelled_natural = q_min_natural * rr_natural ) %>%
        mutate(q_modelled_other = q_min_other * 1 ) #%>%
        
        # mutate(q_modelled_other = max(0,q_modelled_all_cause - q_modelled_natural - q_modelled_suicide) )%>% 
        # mutate(q_modelled_other = q_modelled_all_cause - q_modelled_natural - q_modelled_suicide) %>% 
        #filter(q_modelled_other<0) %>% View()
        
        # mutate(q_surv = 1 - q_modelled_all_cause ) %>% 
        # mutate(q_modelled_other = max(0,q_modelled_all_cause - q_modelled_natural - q_modelled_suicide) ) 
        # mutate(q_modelled_other = q_modelled_all_cause - q_modelled_natural - q_modelled_suicide) #%>% 
        # mutate(q_surv = 1 - q_modelled_other -  q_modelled_natural - q_modelled_suicide)
        
        if(j == 'intervention' & i >= 2026 & i <= 2030){
          message('intervene')
          
          pop2 <- pop2 %>% 
            mutate(q_modelled_all_cause = q_min_all_cause * (1 + (rr_all_cause - 1) * 0.5 ) ) %>%
            # mutate(q_modelled_all_cause_comorbid_matched = q_min_all_cause_comorbid_matched * (1 + (rr_all_cause_comorbid_matched - 1) * 0.5 ) ) %>%
            mutate(q_modelled_suicide = q_min_suicide * (1 + (rr_suicide - 1 ) * 0.5  ) ) %>%
            mutate(q_modelled_natural = q_min_natural * (1 + (rr_natural - 1 ) * 0.5 ) ) %>%
            mutate(q_modelled_other = q_min_other * 1 ) 
        }
       
      pop2 <- pop2 %>% 
        mutate(q_surv = 1 - q_modelled_other -  q_modelled_natural - q_modelled_suicide) %>%
        # mutate(q_surv = 1 - q_modelled_all_cause) %>% 
        
        rowwise() %>% 
        mutate(q_all = list(list(q_modelled_other = q_modelled_other,
                                 q_modelled_natural = q_modelled_natural,
                                 q_modelled_suicide = q_modelled_suicide,
                                 q_surv = q_surv))) %>%
        # mutate(q_all = list(list(q_modelled_all_cause = q_modelled_all_cause,
        #                          q_surv = q_surv))) %>%
        
        
        mutate(death = sample(size = 1, x = names(q_all),prob = unlist(q_all)) )  %>% 
        ungroup()
      
      # print('add births')
      # pop2 <- pop2 %>%
      #   mutate(year = as.numeric(year)) %>%
      #   mutate(id = as.character(id ))%>%
      #   asfr_births( fertility = fertility)
      
      pop_dead <- pop2 %>%
        filter(death != 'q_surv' ) %>% 
        filter(age>30) %>%
        count(year,intervene,death, run=k)
      
      pop1 <- pop2 %>% 
        filter(death == 'q_surv' ) 
      
      pop_alive <- pop1 %>% 
        filter(age>30) %>%
        count(year,intervene,run = k)
      
      pop_d <- rbind( pop_d, pop_dead ) 
      pop_a <- rbind( pop_a, pop_alive ) 
    }
  }
}

pop_a %>% 
  group_by( year, intervene) %>% 
  summarise(n=mean(n)) %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene) %>% 
  e_charts( year) %>% 
  e_tooltip(trigger = 'axis') %>% 
  e_y_axis(min=11700) %>%
  e_title('Alive') %>% 
  e_line(n)

# pop_a %>% 
#   mutate(year = as.character(year)) %>% 
#   group_by( intervene,run) %>% 
#   e_charts( year) %>% 
#   e_tooltip(trigger = 'axis') %>% 
#   e_y_axis(min=10000) %>%
#   e_title('Alive') %>% 
#   e_line(n)

pop_d %>% 
  # filter(death == 'q_modelled_other') %>%
  mutate( year = as.character( year ) ) %>% 
  group_by( intervene, run ,year) %>% 
  summarise( n = sum( n ) ) %>% 
  group_by( intervene, year) %>% 
  summarise( n = mean( n ) ) %>% 
  e_charts( year )  %>% 
  e_tooltip( trigger = 'axis' ) %>% 
  e_y_axis(min=170) %>%
  e_title('Dead') %>% 
  # e_y_axis(min=170000) %>% 
  e_line( n )

pop_d %>% 
  # filter(death == 'q_modelled_other') %>%
  mutate( year = as.character( year ) ) %>% 
  group_by( intervene, run ,death, year) %>% 
  summarise( n = sum( n ) ) %>% 
  group_by( intervene, death, year) %>% 
  summarise( n = mean( n ) ) %>% 
  group_by( death) %>%
  pivot_wider(names_from = intervene, values_from = n) %>% 
  mutate(delta = intervention - `non-intervention`) %>% 
  mutate(delta_norm = (intervention - `non-intervention`)/intervention) %>% 
  mutate(delta_cum = cumsum(delta)) %>% 
  mutate(delta_norm_cum = cumsum(delta_norm)) %>% 
  e_charts( year )  %>% 
  e_tooltip( trigger = 'axis' ) %>% 
  # e_y_axis(min=150) %>%
  e_title('Dead') %>% 
  # e_y_axis(min=170000) %>% 
  
  e_bar( delta_norm_cum , stack = 'staeck')
  e_bar( delta_cum, stack = 'staeck')
  e_bar( delta_norm, stack = 'staeck')
  e_bar( delta , stack = 'staeck')
  





all_deltas_tbl <- pop_d %>% 
  mutate( year = as.character( year ) ) %>% 
  group_by( intervene, death, year) %>% 
  summarise( n = mean( n ) ) %>% 
  group_by( death) %>%
  pivot_wider(names_from = death, values_from = n) 


all_deltas_tbl %>% 
  # filter(death == 'q_modelled_natural') %>%
  group_by(intervene) %>% 
  e_charts( year )  %>% 
  e_tooltip( trigger = 'axis' ) %>% 
  # e_y_axis(min=150) %>%
  e_title('Dead','Suicide') %>% 
  # e_y_axis(min=300) %>%
  e_line( q_modelled_suicide) #%>%
  # e_loess(q_modelled_suicide~year)
  
  all_deltas_tbl %>% 
    # filter(death == 'q_modelled_natural') %>%
    group_by(intervene) %>% 
    e_charts( year )  %>% 
    e_tooltip( trigger = 'axis' ) %>% 
    # e_y_axis(min=150) %>%
    e_title('Dead','Natural ') %>% 
    e_y_axis(min=170) %>%
    e_line( q_modelled_natural) %>% 
    e_mark_area(
      data = list(
        list(
          xAxis = "2025",
          itemStyle = list(color = "rgba(60, 179, 113, 0.05)")
        ),
        list(xAxis = "2030")
      )
    )
  
  all_deltas_tbl %>% 
    # filter(death == 'q_modelled_natural') %>%
    group_by(intervene) %>% 
    e_charts( year )  %>% 
    e_tooltip( trigger = 'axis' ) %>% 
    e_y_axis(min=6) %>%
    e_title('Dead','Other') %>% 
    # e_y_axis(min=300) %>%
  e_line( q_modelled_other) #%>% 
    # e_loess(q_modelled_other~year,name = c('intervention','f'))
    
  

  
ggplot(pop_d) +ggplot(pop_d) +ggplot(pop_d) +
  geom_point(aes(year, n, colour = as.character(death) )) +
  geom_line(aes(year, n, group = paste(run, death, intervene), lty=as.character(intervene) ,colour = as.character(death)))

ggplot(pop_a) +
  geom_point(aes(year, n, colour = as.character(run) )) +
  geom_line(aes(year, n, group = paste(run, intervene), lty=as.character(intervene) ,colour = as.character(run)))


library(echarts4r)

pop_a %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene,run) %>% 
  e_charts( year) %>% 
  e_tooltip(trigger = 'axis') %>% 
  # e_y_axis(min=170000) %>% 
  e_line(n)

ggplot(pop_d) +
  geom_point(aes(year, n, colour = intervene)) #+
# geom_line(aes(year, n, group = intervene)) +
# geom_rect(aes(xmin = '2026', xmax = '2030', ymin = 1401, ymax = 2600), fill = 'green', alpha = 0.005) +
#ylim(c(1400, 2600))

pop_a %>% 
  mutate(year = as.character(year)) %>% 
  group_by( intervene,run) %>% 
  e_charts( year) %>% 
  e_line(n) #%>% 
  # e_y_axis(min=170000) 

pop_a %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  ggplot() +
  geom_point(aes(year, n, colour = intervene)) +
  geom_line(aes(year, n, colour = intervene))

pop_d %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot() +
  
  # geom_rect(aes(xmin = 2025, xmax = 2030, ymin = 601, ymax = 2000),
  #           colour = 'white', 
  #           fill = 'green', alpha = 0.01) +
  
  # ylim(c(1400, 2600))+
  geom_point(aes(year, n, colour = intervene))+
  geom_line(aes(year, n, group = intervene, colour = intervene)) +
  # scale_x_continuous(
  #   breaks = seq(2023, 2065, by = 5),
  #   labels = scales::label_number()
  # )+
  theme_minimal(base_family = "Graphik")

natural_var <- pop_d %>% 
  group_by(year,intervene) %>%
  summarise(nn=n(), sd =sd(n), n = mean(n)) %>% 
  mutate( error = sd / sqrt(nn) ) %>% 
  ungroup() %>% 
  summarise(error = mean(error)) %>% 
  pull(error)
# ungroup() %>% 
# ggplot() + 
# geom_point(aes(year, error)) 

x <- pop_d %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  pivot_wider(names_from=intervene,values_from = n) %>% 
  mutate(delta = intervention - `non-intervention`) %>% 
  ungroup() %>% 
  mutate(cum_delta = cumsum(delta))

x %>% 
  ggplot() +
  geom_col(aes(year, delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'none',
    delta - natural_var * 1.96 < 0 ~ 'down',
    delta - natural_var * 1.96 > 0 ~ 'up'
  )))  +
  scale_fill_manual(values = c("up" = "salmon", "down" = "lightgreen",'none' = 'black'))+
  geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  geom_smooth(method = 'loess',data  = x %>% filter(year>2030), aes(y =delta, x = year)) +
  theme_minimal() +
  labs(
    fill = 'title_text',
    x = "Year",
    y = "Delta"
  )

geom_bar(aes(year, delta )) +
  geom_line(aes(year, delta )) #+ 
# geom_rect(aes(xmin = '2026', xmax = '2030', ymin = 01, ymax = 2600), fill = 'green', alpha = 0.005) +
# ylim(c(1400, 2600))


x %>% 
  ggplot() +
  geom_col(aes(year, cum_delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'none',
    delta - natural_var * 1.96 < 0 ~ 'down',
    delta - natural_var * 1.96 > 0 ~ 'up'
  )))  +
  scale_fill_manual(values = c("up" = "salmon", "down" = "lightgreen",'none' = 'black'))+
  geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  geom_smooth(method = 'loess', data  = x %>% filter(year>2030), aes(y =delta, x = year)) +
  theme_minimal() +
  labs(
    fill = 'title_text',
    x = "Year",
    y = "Delta"
  )
# geom_bar(aes(year, delta )) +
  geom_line(aes(year, delta )) 
  
  
  
  library(readxl)
  official <- read_excel("data/ni/ni_ppp_machine_readable.xlsx", 
                         sheet = "Population")
  
  official %>% 
    pivot_longer(-c(Sex,Age), names_to = 'year', values_to = 'pop') %>% 
    count(Age, year, wt = pop) %>% 
    filter(Age>30) %>%
    filter(year<2055) %>% 
    count(year, wt= n) %>% 
    ggplot() +
    geom_point(aes(year, n))
  
  

