morb_cols <- c('cvd'='pad', 'msk'='osteoporosis',     'cancer'='cancer',                               'msk'='osteoarthritis',             'msk'='rheumatoid_arthritis',       'other'='epilepsy',                  'other'='hypothyroidism','resp'='asthma',                     'resp'='copd',                       'other'='depression',                 'cvd'='non_diabetic_hyperglycaemia','cancer'='colorectal_cancer',          'cancer'='prostate_cancer',           'cancer'='female_breast_cancer',      'cancer'='renal_cancer',                         'cancer'='oesophageal_cancer',                   'cancer'='stomach_cancer',                       'cancer'='osteogastric_cancer',                  'cancer'='oral_cancer',                          'cancer'='pancreatic_cancer',                    'cancer'='uterine_cancer',            'cancer'='blood_multiple_myeloma',               'cancer'='blood_lymphoma',                       'cancer'='blood_leukaemia',                      'cancer'='blood_cancer',                         'cancer'='ovarian_cancer',            'cancer'='lung_cancer','cvd'='stroke','cvd'='chd','cvd'='diabetes','other'='dementia','cvd'='heart_failure','cvd'='atrial_fibrillation','cvd'='hypertension','cvd'='chronic_kidney_disease')
library(data.table)
library(tidyverse)

morb_map <- unique(data.table(
  class   = names(morb_cols),
  disease = unname(morb_cols)
))

disease_cols <- morb_map$disease

pp <- copy(past_populations)

# pp[, (disease_cols) := lapply(.SD, function(x) as.integer(x > 0)),
#    .SDcols = disease_cols]

x <- pp %>% select(disease_cols) %>% 
  as.matrix() %>% 
  {.!=0}

melt(x)

long_disease <- melt(
  pp[age>50 ,] ,
  id.vars = c("id"),                # or person_id / sim_id
  measure.vars = disease_cols,
  variable.name = "disease",
  value.name = "has_disease"
)[has_disease != 0]

long_disease <- morb_map[long_disease, on = "disease"]

disease_pairs <- long_disease[
  long_disease,
  on = .(id),
  allow.cartesian = TRUE
][
  disease != i.disease,
  .N,
  by = .(
    disease_from = disease,
    disease_to   = i.disease
  )
]

base_counts <- long_disease[, .N, by = disease]
setnames(base_counts, c("disease", "N"), c("disease_from", "N_from"))

disease_crossover <- disease_pairs[
  base_counts,
  on = "disease_from"
][
  , prob := N / N_from
][
  order(disease_from, -prob)
]

class_long <- unique(long_disease[, .(id, class)])

class_pairs <- class_long[
  class_long,
  on = .(id),
  allow.cartesian = TRUE
][
  class != i.class,
  .N,
  by = .(
    class_from = class,
    class_to   = i.class
  )
]

class_base <- class_long[, .N, by = class]
setnames(class_base, c("class", "N"), c("class_from", "N_from"))

class_crossover <- class_pairs[
  class_base,
  on = "class_from"
][
  , prob := N / N_from
][
  order(class_from, -prob)
]

library(dplyr)
library(echarts4r)

class_crossover %>%
  mutate(
    class_from = factor(class_from),
    class_to   = factor(class_to),
    pct = prob * 100
  ) %>%
  arrange(desc(class_to)) %>% 
  # group_by(class_from) %>%
  e_charts(class_from) %>%
  e_heatmap(class_to, pct, name = "P(class_to | class_from) %") %>%
  e_visual_map(pct, calculable = F) %>%
  # e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
  #   "function(p){
  #     return 'From: ' + p.name +
  #            '<br>To: ' + p.value[1] +
  #            '<br>P(To | From): ' + p.value[2].toFixed(2) + '%';
  #   }"
  # )) %>%
e_tooltip() %>% 
  e_axis_labels(x = "From class", y = "To class") %>%
  e_title("Multimorbidity crossover by class", "Conditional probability heatmap")


library(data.table)
library(dplyr)
library(echarts4r)

top_pairs <- as.data.table(disease_crossover)[
  order(-prob)
][
  , head(.SD, 200)   # top 200 directional pairs
]

top_pairs %>%
  filter(disease_from != 'depression',
         disease_to != 'depression') %>% 
  mutate(pct = prob ) %>%
  # group_by(disease_from) %>%
  e_charts(disease_from) %>%
  e_heatmap(disease_to, pct, name = "P(to | from) %") %>%
  e_visual_map(pct, calculable = TRUE) %>%
  e_tooltip() %>%
  e_axis_labels(x = "From disease", y = "To disease") %>%
  e_datazoom(type = "slider", y_index = 0) %>%
  e_datazoom(type = "inside", y_index = 0) %>%
  e_title("Disease crossover (top directional pairs)", "Scroll/zoom to explore")


library(dplyr)
library(echarts4r)

# attach target disease class (for colouring/labeling if needed)
dc <- disease_crossover %>%
  left_join(morb_map, by = c("disease_from" = "disease")) %>%
  rename(class_from = class) %>%
  left_join(morb_map, by = c("disease_to" = "disease")) %>%
  rename(class_to = class)

# choose a weight: counts are most intuitive for Sankey
links <- dc %>%
  group_by(class_from, disease_to) %>%
  summarise(value = sum(N), .groups = "drop") %>%
  arrange(desc(value)) %>%
  slice_head(n = 40) %>%   # keep readable
  rename(source = class_from, target = disease_to)

nodes <- unique(c(links$source, links$target)) %>%
  data.frame(name = .)

# nodes %>%
links %>% 
  e_charts() %>%
  e_sankey(source, target, value) |> #e_sankey(links, nodes = nodes, focus_node_adjacency = TRUE) %>%
  e_title("Which diseases drive crossovers from each class?", "Sankey (top 40 links by co-occurrence count)")

library(dplyr)
library(echarts4r)

edges <- disease_crossover %>%
  filter(N >= 200) %>%          # tune threshold
  mutate(value = N) %>%
  rename(source = disease_from, target = disease_to) %>%
  select(source, target, value) #%>% 
  # mutate(value = ifelse(value > max(value)/100, 0)   # scale for visualization

nodes <- unique(c(edges$source, edges$target)) %>%
  data.frame(name = .) %>% mutate(value=3,size=10)

e_charts() |> 
  e_graph_gl(  ) |> 
  e_graph_nodes(nodes, name, value, size) |> 
  e_graph_edges(edges, source, target) |>
  e_modularity() |> 
  e_tooltip()

nodes %>%
  e_charts() %>%
  e_graph(edges, layout = "force") %>%
  # e_force(repulsion = 120, edgeLength = 60) %>%
  e_tooltip() %>%
  e_title("Disease multimorbidity network", "Edges filtered by co-occurrence count threshold")

# con <- dbConnect(duckdb::duckdb(), dbdir = 'past_populations_db/past_populations.duckdb', read_only = F)
# latest_tbl <- sort(decreasing = T,dbListTables(con))[1]
# 
# x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' USING SAMPLE 50 PERCENT (bernoulli);'))  # Set cache size to 2MB
# 
# # past_populations_20260116_015236
#
# past_populations <- dbFetch(x)
# dbClearResult(x)
# dbDisconnect(con, shutdown=TRUE)
# setDT(past_populations)

# Comorbidities by Risk
# Urban /rural gradients
# Neighbourhood Renewal Area

#Obesity is associated with greater multi- morbidity in older age groups

past_populations[age>50,.(m=mean(multimorbidity,na.rm=T)), by= .(age10,bmi)
                 ] %>% 
  group_by(bmi) %>% 
  e_charts(age10) %>%
  e_bar(m)

past_populations[age>50 & year != min(year),
                 .(m=mean(stroke==year,na.rm=T)), 
                 by= .(age10,bmi)
] %>% 
  group_by(age10) %>% 
  e_charts(bmi) %>%
  e_bar(m)

past_populations[age>50 ,
                 .(m=mean(stroke!=0,na.rm=T)), 
                 by= .(age10,bmi)
] %>% 
  group_by(age10) %>% 
  e_charts(bmi) %>%
  e_bar(m)

past_populations[age>50,.(m=mean(multimorbidity,na.rm=T)), by= .(Urban_mixed_rural_status,bmi)
] %>% 
  group_by(Urban_mixed_rural_status) %>% 
  e_charts(bmi) %>%
  e_bar(m)
  
past_populations[,nra:='NA'!=NRA_code][age>50,.(m=mean(multimorbidity,na.rm=T)), 
                 by= .(bmi,nra)
] %>% 
  group_by(nra) %>% 
  e_charts(bmi) %>%
  e_bar(m)

past_populations[age>50 & age<95,.(m=sum(stroke!=0,na.rm=T)), 
                                       by= .(age,sex)
] %>% 
  group_by(sex) %>%
  e_charts(age) %>%
  e_line(m)

past_populations[age>50 & age<95,.(m=sum(stroke!=0,na.rm=T)), 
                 by= .(age,year)
] %>% 
  group_by(year) %>%
  e_charts(age) %>%
  e_line(m)

past_populations[age>50 & age<95,.(m=sum(stroke!=0,na.rm=T)), 
                 by= .(age,year)
][,cm:=(m+lag(m)+lag(m,2))/3] %>% 
  group_by(year) %>%
  e_charts(age) %>% 
  # e_histogram(m) #%>%
  e_line(cm)

past_populations[age>50 & age<95&year != min(year),.(m=sum(stroke==year,na.rm=T)), 
                 by= .(age)
] %>% 
  # group_by(nra) %>% 
  e_charts(age) %>%
  e_line(m)

past_populations[age>50 & age<95&year != min(year)&death_reason=='stroke',.(m=sum(stroke==year,na.rm=T)), 
                 by= .(age)
] %>% 
  # group_by(nra) %>%
  e_charts(age) %>%
  e_line(m)

