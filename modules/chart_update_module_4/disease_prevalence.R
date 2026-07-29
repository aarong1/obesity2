library(readxl)
library(tidyverse)
library(RColorBrewer)
# display.brewer.all(type = "qual")

pastel_colors <- c(
  "#AEC6CF", # pastel blue
  "#FFB347", # pastel orange
  "#77DD77", # pastel green
  "#FF6961", # pastel red
  "#F49AC2", # pastel pink
  "#CBAACB", # pastel purple
  "#FFFFB3", # pastel yellow
  "#B0E0E6"  # pastel cyan
)

#b0e0e6	#e6b6b0

prevalence_per_cap <- read_excel("data/DiseasePrevDoH_2324.xlsx",
                         sheet = "Table 2 Prevalence per 1000 pts",
                         skip = 5)
# view(prevalence_per_cap)

( prevalence <- read_excel("data/DiseasePrevDoH_2324.xlsx", 
                         sheet = "Table 1 Prevalence Registers", 
                         skip = 2) )

# View(prevalence)

names(prevalence)[1] <- 'Disease'

prevalence <- prevalence |> pivot_longer(-Disease,
                           names_to = 'Year',
                           values_to = 'Count')

target_diseases <- c(
'Coronary Heart Disease',
'Stroke & TIA',
'Hypertension',
'Diabetes', 
'Diabetes Mellitus',
'COPD', 
'Chronic Obstructive Pulmonary Disease',
'Cancer',
'Depression',
'Epilepsy',
'Rheumatoid Arthritis',
'Osteoporosis',
# Mental Health ,
'Asthma' ,
'Heart Failure', 
'Heart Failure due to LVSD',
'Heart Failure 1',
'Heart Failure 3',
'Dementia',
'Chronic Kidney Disease',
'Peripheral Arterial Disease',
'Atrial Fibrillation',
'Non-Diabetic Hyperglycaemia'
)

last_10yr <- c(
#'2013/14',    
#'2014/15',  
'2015/16',    
'2016/17',    
'2017/18',    
'2018/19',    
'2019/20',    
'2020/21',    
'2021/22',    
'2022/23',    
'2023/24')

################################################
# Mishandled Heart Failure 
################################################

# prevalence <- prevalence |> 
#   mutate(Disease = ifelse(Disease %in% c('Heart Failure', 
#          'Heart Failure due to LVSD'), 'Heart Failure', Disease)) |> 
#   group_by(Disease, Year) |> 
#   summarise(Count = sum(Count)) #|> view()
#   # count(Disease) |> 
#   # print(n=50)

#view(prevalence)
################################################

prevalence <- prevalence |> 
   filter(Disease != 'Heart Failure due to LVSD ')

################################################

prevalence |> 
  filter(Disease %in% target_diseases#,
         #Year %in% last_10yr
         ) |> 
ggplot() +
  geom_point(aes(Year,Count,group=Disease), color = 'black',lwd = 1)+
  geom_line(aes(Year,Count,group=Disease), color = 'black',lwd = 0.5)+
  geom_smooth(aes(Year,Count,group=Disease), fill = 'orange', color = 'orange' )+
  facet_wrap(~Disease,scales = 'free_y')+
  theme_void()+
  # theme(axis.text.x = element_text(angle = -45, hjust = 1,size = 6))+
  ggtitle(subtitle = '2005 - 2024',label = 'Plot of QoF Disease Prevalence')


prevalence_hsct <- read_excel("data/DisPrevHsct_nisra_2324.xlsx", 
                              sheet = "Unpivoted") |> 
  pivot_wider(values_from = VALUE, names_from = `Statistic Label`) |> 
  mutate(prob = `Raw disease prevalence per 1,000 patients`/1000) |> 
  rename('Per1k' = "Raw disease prevalence per 1,000 patients",
         'Count' = `Number of patients on the register`,
         'Year' = "Financial Year",
         'HSCT' = "Health and Social Care Trust") |> 
  select(-UNIT)

prevalence_hsct %>% 
  mutate(HSCT = case_when(
    HSCT == 'Belfast' ~ 'BHSCT',
    HSCT == 'Northern' ~ 'NHSCT',
    HSCT == 'Southern' ~ 'SHSCT',
    HSCT == 'South Eastern' ~ 'SEHSCT',
    HSCT == 'Western' ~ 'WHSCT',
    HSCT == 'NI Total' ~ 'Northern Ireland',
    TRUE ~ HSCT
  )) -> prevalence_hsct
################################################
# This was a line of code that combined 
# Heart Failure 1
# Heart Failure 3
# It has now been confirmed that HF1 is a superset of HF3

# prevalence_hsct <- prevalence_hsct |> 
#     mutate(Disease = str_remove_all(Disease,' [0-9]')) |> 
#     group_by(Disease,Year,HSCT) |> 
#     summarise(across(c(Count,Per1k,prob),sum)) #|> view()

################################################



# This is the correct way to handle Heart Failure in the Qof returns

prevalence_hsct <- prevalence_hsct |>
    filter(Disease != 'Heart Failure 3') |> #count(Disease)
    mutate(Disease = 
      case_when(Disease =='Heart Failure 1' ~ 'Heart Failure',
                T ~ Disease)
      ) #|> view()


################################################
prevalence_hsct |> 
  filter(Disease %in% target_diseases) |> 
  filter(!HSCT %in% 'Northern Ireland') |> 
  #filter(`Year` %in% last_10yr) |> 
  ggplot() +
  geom_point(aes(`Year`, Per1k, col=HSCT, group=HSCT),lwd = 1) +
  geom_line(aes(`Year`, Per1k, col=HSCT, group=HSCT), lwd = 0.5) +
  #geom_smooth(aes(`Year`, per100k, group=Disease), fill = 'orange', color = 'orange' )+
  facet_wrap(~Disease,scales = 'free_y') +
  theme_minimal() +
  # scale_color_brewer(palette = "Paired") +
  scale_color_manual(values = pastel_colors) +
  theme(axis.text.x = element_text(angle = -45, hjust = 1)) +
  ggtitle('Plot of QoF Prevalence 2017-2024 by Trust')


prevalence_hsct <-
  prevalence_hsct |> 
  filter(Disease %in% target_diseases) |> 
  filter(!HSCT %in% 'Northern Ireland') |> 
  mutate(Interpolated = ifelse(is.na(Disease),TRUE,FALSE)) |> 
  select(Year, HSCT, Disease, prob, Count, Per1k, Interpolated) |> 
  group_by(Disease,HSCT) |> 
  arrange(desc(Year)) |> 
  fill(prob,.direction = 'down') |> 
  fill(Count,.direction = 'down') |> 
  ungroup()

prevalence_hsct <- prevalence_hsct |> 
  # mutate(HSCT = paste(HSCT, 'HSCT')) |> 
  mutate(Year = str_trunc(Year,side='right',width = 4,ellipsis = ''))

prevalence_hsct |>  
ggplot() +
  geom_point(aes(`Year`, Count, col=HSCT, group=HSCT),lwd = 1) +
  geom_line(aes(`Year`, Count, col=HSCT, group=HSCT), lwd = 0.5) +
  #geom_smooth(aes(`Year`, per100k, group=Disease), fill = 'orange', color = 'orange' )+
  facet_wrap(~Disease,scales = 'free_y')+
  theme_minimal()+
  # scale_color_brewer(palette = "Paired") +
  scale_color_manual(values = pastel_colors) +
  theme(axis.text.x = element_text(angle = -45, hjust = 1))+
  ggtitle(label = 'Plot of QoF Prevalence 2017-2024 by Trust',
          subtitle = 'With tidying and feature transformations')


