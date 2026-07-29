library(visNetwork)
library(readr)

vars <- read_csv("components/sandbox/risk_data/Input Vars-Table 1.csv")

vars <- vars[c( 'Standardised_btm_lvl','Risk Equation')] %>% 
fill(.direction = 'down',`Risk Equation`)

edges <- vars %>% drop_na() %>% setNames(c('from','to'))

nodes <- unique(c(edges$`from`,edges$to)) %>%
  na.omit() %>% 
  data.frame(id=.,
             label=.,
             color='black',
             directed=T,
             shadow=F )

col_df=tibble(label= c('Atrial Fibrillation', 
      'Congestive Heart Failure',
 'Coronary Heart Disease (Hard)',
                  'Hypertension',
   'Intermittent claudification',
              'QDiabetes (2013)',
                   'QThrombosis',
                       'QStroke',
           'PCE Atherocslerosis'),
         col='red')

nodes <- left_join(nodes,col_df) %>% 
  mutate(color=coalesce(col,color)) %>% 
  select(-col)

network_risk_lineage <- visNetwork(nodes = nodes,edges = edges) %>% 
  visPhysics(solver = 'forceAtlas2Based',enabled = T,
             hierarchicalRepulsion = list(SpringLength=10,
                                          springConstant=0.1,
                                          damping=0.5,
                                          avoidOverlap=1
                                          )) %>% 
    visEdges(arrows = 'middle') %>% 
  visNodes() %>% 
  visOptions(highlightNearest = list(hover =T,
                                     enabled =T,
                                     degree = 1),
                                     nodesIdSelection = F)

# igraph::graph_from_data_frame(edges) %>% plot
# 
# igraph::graph_from_data_frame(edges) %>% 
#   visNetwork::visIgraph()


