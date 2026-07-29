library(reactable)
library(reactablefmtr)
library(htmltools)
library(data.table)
library(shiny)
library(echarts4r)
library(bslib)
library(leaflet)
library(reactable)
library(fst)
library(qs)
library(readxl)
library(tidyverse)
library(sf)

dot <- function(col){
  div(style=paste0('display:inline-block;background:',col,';border-radius:50%;height:10px;width:10px;'),'')
}

# load(".RData")

source('app_prep.R')
source('global.R')
source('1_2_utils/main_configuration.R')

print(paste('running','/intervention_module.R')); source('modules/intervention_module/intervention_module.R')
print(paste('running','/progress_pair_module.R')); source('modules/progress_pair_module/progress_pair_module.R')
print(paste('running','/specificInterventionModule_3.R'));source('./modules/specificInterventionModule_3.R')
print(paste('running','/chart_update_module.R')); source('modules/chart_update_module_4/chart_update_module.R')
source('./modules/slide_panel_module/slide_panel_module.R')

# initial_time_zero_population <- first_population

source('./components/disabled_risk_to_intervene_on.R')
source('./help_component.R')
source('./progress_component.R')
source('./components/button_block_box_shadow.R')
source('./components/startup_overlay_div.R')
source('./components/hatched_sub_title.R')
source('./components/circular_value.R')
source('./decal_bar.R')
source('./components/rag_line.R')

source('./components/model_registry_list.R')
source('./scenarios_div.R')
source('./components/sticky_side_bar.R')

source('./obesity_intervention/engine_bmi.R')
source('./post_evaluation_functions.R')

print(paste('running','/advanced_e_charts_trend.R')); source('./advanced_e_charts_trend.R')
print(paste('running','/pivottable.R')); source('modules/pivot_module/pivottable.R')
print(paste('running','/pivottable_module.R'));source('modules/pivot_module/pivottable_module.R')
source('./pivot_columns.R')

source('./6_post_main/post_evaluation_module/bed_days_estimator.R')
source('./6_post_main/post_evaluation_module/lost_productivity_estimator.R')

# source('./components/sticky_side_bar.R')

graph_wrapper <- function(..., header =NULL){
  
  div(class = "p-2 grid-item grid-item--graph theme-green", #grid-item grid-item--graph
      div(class = "grid-item-content",
          div(class = "chart-card",
              if(!is.null(header)){
                div(class = "card-header", # style = "font-size: 0.5em;",
                    header
                )},
              ...
          )
      )
  )
}



ui <- page_fluid( id = 'main-content',
                  theme = bs_theme(version = 5, font_scale = 0.8,
                                   bootswatch = 'litera',
                                   primary = '#2196F3'),
                  #e_theme_register(paste0(theme_x,collapse =""), name = "myTheme"),
                  
                  # Include external dependencies
                  
                  startup_overlay_div(5000,7000),
                  slide_panel_ui('main1'),
                  tags$head(
                    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/css/ion.rangeSlider.min.css"),
                    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/js/ion.rangeSlider.min.js"),
                    # Packery CSS and JS from CDN (no Draggabilly needed)
                    # tags$script(src = "https://unpkg.com/packery@2/dist/packery.pkgd.min.js"),
                    HTML('<script>
                        window.FontAwesomeConfig = {
                          searchPseudoElements: true
                        }
                      </script>'),
                    
                    # Custom CSS styling
                    # HTML('<style></style>'),
                    # tags$style("")
                    includeCSS('./www/styles.css')
                  ),
                  
                  # Main layout container
                  
                  # Control Panel
                  
                  shiny::tags$nav(
                    
                    class = "navbar fixed-top shadow-lg glass-card mb-3 p-2 rounded", # bg-light
                    shiny::h4(  "Population Health ", span(class = 'lead','Population Health Model'),
                                span(HTML('<h7><span class="badge rounded-pill text-white bg-opacity-75 bg-primary">Obesity', '' ,'</span></h7>')),
                                
                                HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-warning ">SPPG </span></h7>'),
                                HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-info ">PHA </span></h7>'),
                                HTML('<h7 id = "move" style = "float:right;">
                       <a href="https://apply-for-innovation-funding.service.gov.uk/competition/2186/overview/e51c18bc-21b3-450d-bdbc-2f43dad3b268">
                       <span class="badge rounded-pill bg-light">
                        <i class="fa-solid fa-arrow-up-right-from-square"></i>
                       OPIP bid</span>
                       </a>
                       </h7>'),
                                
                                class = "navbar-brand mb-0 w-100"
                    )
                  ),
                  
                  
                  div(style='top:100px;width:150px;z-index:1000',class = 'nav nav-tabs ms-2 p-3 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card',
                      
                      div(class = "nav-section",
                          h6(class = "bg-opacity-25 border-5  p-2 rounded-3 text-bg-dark", "Health Burden"), #text-body-secondary
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Dashboard"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Analytics"
                          ),
                          div(class = "nav-item",
                              "Reports"
                          )
                      ),
                      
                      div(class = "nav-section",
                          h6(class = "bg-opacity-25 border-5  p-2 rounded-3 text-bg-dark",
                             "OPIP"),
                          
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Obesity Risk"
                          ),  #
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Geography"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Population"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Inequalities"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "NHSCT"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "INT (Neighhbourhood)"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Lifestyle"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Deprivation"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Society"
                          )
                      ),
                      div(class = "nav-section",
                          h6(class = "bg-opacity-25  border-5  p-2 rounded-3 text-bg-dark", "Intervention"), #nav-section-title
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Specify"
                          ),
                          div(class = "nav-item",
                              # span(class = "nav-icon"),
                              "Scenarios"
                          )
                      )
                      
                  ),
                  
                  div(class='mt-5 pt-5'),
                  div(class = "main-container",
                      
                      # Main content area
                      div(class = "content-area",
                          # div( class="tab-pane active", id="overview", role="tabpanel", `aria-labelledby`="overview",
                          
                          div(id = 'main-tab-area', class = " vw-80 tab-content",
                              
                              # Dashboard Tab Content
                              # dashboard-tab -----
                              div(id = "dashboard-tab", class = "tab-pane fade show", #
                                  div(class = "container-fluid",
                                      style = "padding-left: 200px;",
                                      
                                      fluidRow(
                                        
                                        column(10,offset=0,
                                               
                                               # Dashboard Tab Content (default active)
                                               div(id = "gri", class = "grid1",
                                                   div(class = 'd-flex flex-row gap-3 flex-nowrap align-items-center',
                                                       div(style = 'width:300px;height:200px',
                                                           bmi_pie),
                                                   
                                                  #  metic_card_prev_total_obesity,
                                                  #  metric_card_total_bed_days,
                                                  #  metric_card_total_episodes,
                                                  #  metric_card_total_deaths_obesity,
                                                  #  metric_card_YLL_total,
                                                  #  metic_card_daly_total_obesity,
                                                  #  metic_card_yld_total_obesity,
                                                  #  metic_card_prev_cancer_obesity,
                                                  #  metric_card_costs_total_obesity,
                                                  # div(class = "grid-item ",
                                                  

                                                  
                                                       div(class = "h-100 grid-item nav-card analytics bg-opacity-50",
                                                           div(onclick = "$('#dashboard-tab').removeClass('active');$('#analytics-tab').addClass(' active');var map = $('#mymap').data('leaflet-map');map.invalidateSize();",
                                                               class = "nav-card-icon",
                                                               icon("arrow-up-right-from-square")),
                                                           div(class = "nav-card-title", "Analytics"),
                                                           div(class = "nav-card-description", "View detailed analytics"),
                                                       ),
                                                       div(class = "h-100 grid-item nav-card settings bg-opacity-50",
                                                           div(onclick = "$('#dashboard-tab').removeClass('active');$('#geography-tab').addClass(' active')",
                                                               class = "nav-card-icon",
                                                               icon("arrow-up-right-from-square")),
                                                           div(class = "nav-card-title", "Geography"),
                                                           div(class = "nav-card-description", "View detailed analytics"),
                                                       ),
                                                       div(class = "h-100 grid-item nav-card reports bg-opacity-50",
                                                           div(onclick = "$('#dashboard-tab').removeClass('active');$('#deprivation-tab').addClass(' active')",
                                                               class = "nav-card-icon",
                                                               icon("arrow-up-right-from-square")),
                                                           div(class = "nav-card-title", "Deprivation"),
                                                           div(class = "nav-card-description", "View detailed analytics"),
                                                       )
                                                   # )
                                                   ),
                                                   
                                                   div(class ='grid-item my-3 ',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),
                                                   
                                                   div(class = 'd-flex justify-content-evenly',
                                                       
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "BMI sex"
                                                               ),
                                                               metric_chart_bmi_sex
                                                               
                                                           )
                                                       )
                                                   ),
                                                   
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "BMI age"
                                                               ),
                                                               metric_chart_bmi_age
                                                           )
                                                       )
                                                   ),
                                                   
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "# Comorbidities by Deprivation extreme quintileand and BMI "
                                                               ),
                                                               comorbidities_bmi_townsend_extreme
                                                               
                                                           )
                                                       )
                                                   )
                                                   ),
                                                   
                                                   div(class ='grid-item my-3',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),
                                                   
                                                   div(class = 'd-flex justify-content-evenly',
                                                       
                                                   div(class = "grid-item grid-item--graph theme-green",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               #no card header
                                                               div(class = "text-secondary",
                                                                   "Overweight and Obese "
                                                               ),
                                                               overweight_obese_sex
                                                           )
                                                       )
                                                   ),
                                                   
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "Townsend Material Deprivation "
                                                               ),
                                                               townsend_distribution_chart
                                                           )
                                                       )
                                                   ),
                                                   
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "Distribution of Risk" ,
                                                                   span(class = 'text-secondary', 
                                                                   "Stratification and Priority"),
                                                               ),
                                                               qrisk_distribution_chart
                                                           )
                                                       )
                                                   )
                                                   ),
                                                   
                                                   div(class ='grid-item my-3',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),
                                                   
                                                   div(class = 'd-flex justify-content-evenly',
                                                   
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "Comorbidities",
                                                                   span(class= 'text-secondary', 'Number of Long Term Chronic Conditions' )
                                                               ),  comorbidities_plot
                                                           )
                                                       )
                                                   ),
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "Depression and Obesity"
                                                               ),  depression_obesity_chart
                                                           )
                                                       )
                                                   ),
                                                   
                                                   # div(class = "grid-item grid-item--graph",
                                                   #     div(class = "grid-item-content",
                                                   #         leaflet_trust
                                                   #     )
                                                   # ),
                                                   
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "Trust",
                                                                   tags$small(class=' text-muted','Raw count of overweight and obese')
                                                                   
                                                               ),  bar_map_morph
                                                           )
                                                       )
                                                   )
                                                   
                                                   ),
                                                  
                                                   div(class ='grid-item my-3',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),
                                                   
                                                   div(class = 'd-flex justify-content-evenly',
                                                   
                                                   # Sales Performance Chart (Wide)
                                                   div(class = "grid-item grid-item--graph theme-blue",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   "Ranked Risk by BMI",
                                                                   tags$small(class=' text-muted','1-yr prob of serious CVD')
                                                               ),  risk_bmi
                                                               
                                                               
                                                           )
                                                       )
                                                   ),
                                                   div(class = "grid-item grid-item--graph theme-red",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   tags$i(class = "fas fa-chart-scatter me-2"),
                                                                   
                                                                   "Risk by Age",
                                                                   tags$small(class=' text-muted','1-yr prob of serious CVD')
                                                               ),
                                                               # revenue_chart
                                                               risk_prob_density_fn_age10
                                                           )
                                                       )
                                                   ),
                                                   div(class = "grid-item--graph theme-red",
                                                       div(class = "grid-item-content",
                                                           div(class = "chart-card",
                                                               div(class = "card-header",
                                                                   tags$i(class = "fas fa-chart-scatter me-2"),
                                                                   
                                                                   "Distribution of Risk by BMI",
                                                                   tags$small(class=' text-muted','1-yr prob of serious CVD')
                                                               ),
                                                               # revenue_chart
                                                               risk_prob_density_fn_bmi
                                                           )
                                                           
                                                       )
                                                   ),
                                                   
                                                
                                                   ),
                                                   div(class ='grid-item my-3',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),
                                                   
                                                   # Revenue Analysis Chart (Tall)
                                                   
                                                   
                                                   
                                                   
                                               )
                                               
                                        ) # End of dashboard grid
                                      )#, # End of dashboard tab content
                                      
                                      # ) # End of fluidRow
                                  ) #end of fluid container
                              ),
                              
                              # Analytics Tab Content
                              
                              # analytics-tab ----
                              div(id = "analytics-tab", class = "tab-pane fade active vh-100", 
                                  # style = "
                                  #           position: fixed;
                                  #           top: 41px;
                                  #           left: 0;
                                  #           right: 0;
                                  #           bottom: 0;
                                  #           overflow: hidden;
                                  #           padding: 0;
                                  #           ",
                                  
                                  #style = "width:100%",
                                  # div(style = 'position:absolute;top:0;left:0;margin-inline:-1px;',#class = "container-fluid",
                                  #div(style = 'width:100vw;height:100vh;',
                                  div(class="outer",
                                      style = "
                                      position: fixed;
                                      width:75%;
                                      top: 41px;
                                      left: 0;
                                      right: 0;
                                      bottom: 0;
                                      overflow: hidden;
                                      padding: 0;
                                      ",
                                      
                                      # div(style = "height:100%;width:50%;",
                                      leafletOutput("mymap", width = '100%', height = '100%'),
                                      # )
                                      
                                      div(class = 'glass-card p-3 me-4 ',
                                          style = 'position:absolute;top:50px;right:0;height:85vh; overflow:auto; background: rgba(255, 255, 255, 0.14)!important; /* 0.14 */',#class = "container-fluid",
                                          # style='top:15vh;width:15vw;z-index:1000',class = ' ms-3 p-4 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card'
                                          div(class = 'd-flex flex-column gap-3 justify-content-between',
                                              div( span(class = '','BMI'),
                                                   div(style = 'height:130px;width:200px;',echarts4rOutput('bmi_chart',height = 200,width=200))
                                              ),
                                              div( span(class = '','Age'),
                                                   div(style = 'height:130px;width:200px;',echarts4rOutput('age_chart',height = 200,width=200))
                                              ),
                                              div(span(class = '','Sex'),
                                                  div(style = 'height:130px;width:200px;',echarts4rOutput('sex_chart',height = 200,width=200))
                                              ),
                                              div(span(class = '','Deprivation'),
                                                  div(style = 'height:130px;width:200px;',echarts4rOutput('depriv_chart',height = 200,width=200))
                                              ),
                                              div(span(class = '','Risk'),
                                                  div(style = 'height:130px;width:200px;',echarts4rOutput('qrisk_chart1',height = 200,width=200))
                                              ),
                                              div(span(class = '','Risk'),
                                                  div(style = 'height:160px;width:200px;',echarts4rOutput('qrisk_chart',height = 200,width=200))
                                              ),
                                              # span( class='ms-4',
                                              #       h6('Population:',textOutput('headline_count'))
                                              # ),
                                              # span( class='ms-4',
                                              #       h6('Qrisk Score:',textOutput('qrisk_average'))
                                              # )
                                              #   )
                                          )
                                      ),
                                      div(style = 'position:absolute; bottom:5px;left:5px;',#class = "container-fluid",
                                          # div(
                                          #   div(class = 'grid-item p-3 m-3',
                                          #       span( class='ms-4 text-center',
                                          #             h6('Population:',textOutput('headline_count'))
                                          #       )),
                                          #   
                                          #   # div(style = 'width:20px;'),
                                          #   
                                          #   div(class = 'grid-item p-3 m-3',
                                          #       span( class='ms-4 text-center',
                                          #             h6('Qrisk Score:',textOutput('qrisk_average'))
                                          #       )
                                          #   ),
                                          #   
                                          #   div(class = 'grid-item p-3 m-3',
                                          #       span( class='ms-4 text-center',
                                          #             h6( 'Percentage Overweight',h6(class = 'text-danger',span(textOutput(inline = T,'overweight_percentage'),'%')))
                                          #       )
                                          #   )
                                          # ),
                                          div(class = 'd-flex flex-row gap-3 justify-content-start align-items-end',
                                          # metric_card(633,
                                          #             'NI', 
                                          #             'Parks and Green Spaces', 
                                          #             color = 'mediumseagreen',
                                          #             opacity = 'opacity-75'),
                                          # # div(style = 'width:20px;'),
                                          # 
                                          # metric_card(890,
                                          #             'Ulster',
                                          #             'Fast Food Outlets',
                                          #             color = 'mediumseagreen'),
                                          
                                          
                                          div(class = 'grid-item grid-item--small m-2 p-1',style = 'width:120px;',
                                              div(class= 'grid-item-content',
                                                  
                                                  div(class = 'metric-card',
                                                  
                                              input_switch(id = "switch", label = "Lock Zoom Filter "), 
                                              h6('Dynamic filtering ' )
                                                  
                                              ))
                                              
                                          ),
                                          
                                          metric_card_html(textOutput('areaPer100k'),
                                                           'Map',
                                                           'Area of Recreational Greens',
                                                           color = 'mediumseagreen'),
                                          
                                          metric_card_html(textOutput('parkPer100k'),
                                                           'Map /per 100k',
                                                           'Parks',
                                                           color = 'mediumseagreen'),
                                          
                                          metric_card_html(textOutput('ffPer100k'),
                                                           'Fast Food',
                                                           'Map /per 100k',
                                                           color = 'steelblue'),
                                          
                                          # div(style = 'width:20px;'),
                                          
                                          
                                            div(class = 'd-flex flex-column gap-3 justify-content-start',
                                              div(class = 'grid-item p-3 m-1',
                                                  span( class='ms-4 text-center',
                                                        h6('Population:',
                                                           textOutput('headline_count'))
                                                  )),
                                              
                                              # div(style = 'width:20px;'),
                                              
                                              div(class = 'grid-item p-3 m-1',
                                                  span( class='ms-4 text-center',
                                                        h6('Risk Score:',
                                                           textOutput('qrisk_average'))
                                                  )
                                              ),
                                              
                                              div(class = 'grid-item p-3 m-1',
                                                  span( class='ms-4 text-center',
                                                        h6( '% Unhealthy weight',h6(class = 'text-danger',span(textOutput(inline = T,'overweight_percentage'),'%')))
                                                  )
                                              )
                                            
                                            )
                                          )
                                          
                                      )
                                  ),
                                  
                                  div(class="p-5",
                                      style = "
                                      position: fixed;
                                      backdrop-filter:blur(5px);
                                      width:26%;
                                      top: 41px;
                                      right: 0;
                                      bottom: 0;
                                      overflow: hidden;
                                      padding: 0;
                                      ",
                                      
                                      h3(" Excedance"),
                                      p(class = 'lead', "Analysis on resolving key regional drivers of Obesity"),
                                      
                                      div(echarts4rOutput('excedance_bmi',height = 230,width=300)),
                                      div(echarts4rOutput('excedance_age',height = 230,width=300)),
                                      div(echarts4rOutput('excedance_bmi_deprivation',height = 230,width=300))
                                  )
                              ),
                              
                              # Reports Tab Content
                              #reports-tab -----
                              div(id = "reports-tab", class = " tab-pane active show",# style = "display: none;",
                                  div(class = "container-fluid", style = "padding-left: 200px;",
                                      h3("Population Health Data - Interactive Pivot Analysis"),
                                      p(class='lead',"Drag columns to create custom analysis. Use dimensions for grouping, measures for aggregation."),
                                      
                                      help_component(),
                                      div(style = "font-size: 0.7rem !important;",
                                      
                                          # Pivot module UI
                                          pivot_module_ui("pivot_reports",
                                                          # data_names = c("sex", "age_risk", "county", "hsct", "bmi",
                                                          #                "Urban_status", "mdm_quintile_soa_name", "ethnicity",
                                                          #                "stroke", "chd", "diabetes", "dementia", "heart_failure"),
                                                          fun_names = c("sum","mean","median","min","max","count","n_distinct")
                                                          ),
                                          
                                          # Results section
                                          div(style = "margin-top: 30px;",
                                              #h4("Pivot Analysis Results"),
                                              div(id = "pivot_reports-table_container",
                                                  reactableOutput("pivot_reports-table")
                                              ),
                                              )
                                      )
                                  )
                              ),
                              
                              # Orders Tab Content
                              # ObesityRisk tab ----
                              div(id = "ObesityRisk-tab", class = "  tab-pane show active ", style = "",
                                  div( style = "padding-left: 150px",
                                       h3("Contributions to Obesity"),
                                       p(class = 'lead', "Factors leading to the cause of Obesity and often accompany it "),
                                       div(class = "my-5 py-5 d-flex flex-row gap-3 flex-nowrap justify-content-around align-items-center",
                                             div(  
                                             # div(class = " mx-1 card-header",# style = "font-size: 0.5em;",
                                             #       '',
                                             #       span(class = 'text-secondary',
                                             #            icon('info'),'Obesi-genic pressures')
                                             #       ),
                                           div(class = "d-flex flex-column gap-3 flex-wrap justify-content-center align-items-center",
                                                        metric_cards_parks,
                                               metric_cards_fast_food)),
                                           # graph_wrapper(header = 'BMI in Income Quintiles', income_plot),
                                           # graph_wrapper(header = 'Distribution of Modifiable Risk', comorbidities_plot),
                                           # graph_wrapper(header = span(class='text-muted',
                                           #                             'Higher BMI correlates with greater propensity for depression'),
                                           #               depression_obesity_chart),
                                           
                                           div(style = 'height:300px;width:300px', 
                                               div(class = " mx-3 card-header",# style = "font-size: 0.5em;",
                                                   'Income Decile',
                                                   span(class = 'text-secondary',
                                                        'A component of MDM'
                                                        #icon('info'),''
                                                   )
                                               ),
                                               income_plot),
                                           div(style = 'height:300px;width:300px;',
                                               div(class = " mx-3 card-header",# style = "font-size: 0.5em;",
                                                   'Comorbidities ',
                                                   span(class = 'text-secondary',
                                                        icon('info'),''
                                                   )
                                               ),comorbidities_plot),
                                           div(style = 'height:300px;width:300px;',
                                               div(class = " mx-3 card-header",# style = "font-size: 0.5em;",
                                                   'Depression',
                                                   span(class = 'text-secondary',
                                                        icon('info'),''
                                                   )
                                               ),
                                               depression_obesity_chart)
                                       ),
                                       # Performance Chart (Small)
                                       
                                       div(class = 'my-5 py-5 alert bg-subtle  text-black',
                                       h3(" Demographic Contribution to Obesity"),
                                       p(class = 'lead', "Factors leading to the cause of Obesity and often accompany it ")
                                       ),
                                       
                                       div(class = "mx-5 px-5 d-flex flex-row gap-3 flex-wrap justify-content-start",
                                           div(class = "card-header w-100",# style = "font-size: 0.5em;",
                                               'BMI with Single year of Age',
                                               span(class = 'text-secondary',
                                                    # icon('info'),
                                                    'Demographic contribution to obesity',
                                               )
                                           ),
                                           bmi_sya_age),
                                       
                                       
                                       div(class = 'my-5 py-5 alert bg-subtle  text-black',
                                           h3("Factors Associated with Overweight and Obesity"),
                                           p(class = 'lead', " Correlation and cuausation effects may be distinct")
                                       ),
                                           div(class = "m-5 p-5 d-flex flex-row gap-3 flex-wrap justify-content-start",
                                               #class= 'grid-item border-4 w-75 p-5 m-5',
                                               div(class = "card-header",# style = "font-size: 0.5em;",
                                                   'Investigate Spurious Correlations among multiple dimensions',
                                                   span(class = 'text-secondary',
                                                        # icon('info'),
                                                        'Click and drag an axis to highlight contiguous values on an axis. Choose another axis at will. 
                                                        Click an axis outside the highlighted are to disregard axis selection.'
                                                   )
                                               ),
                                               BMI_parallel_chart ),
                                       
                                          div(class = 'my-5 py-5 alert bg-subtle text-black',
                                       h3("Risk Factors"),
                                       p(class = 'lead', " ")
                                  ),
                                  
                                       div(class = "d-flex flex-row gap-3 justify-content-around align-items-center", #flex-wrap justify-content-around
                                           div(
                                             div(class = 'pb-5', style = 'height:300px;width:250px;',
                                                    div(class = "card-header text-secondary", 'Age'),
                                                   metric_chart_bmi_age),
                                           div(style = 'height:300px;width:250px;',
                                               div(class = "card-header text-secondary", 'Sex'),
                                               metric_chart_bmi_sex)),
                                           div(
                                             div(class = 'pb-5', style = 'height:300px;width:250px;',
                                                   div(class = "card-header text-secondary", 'Diet'),
                                                   diet_plot),
                                           div(style = 'height:300px;width:250px;',
                                               div(class = "card-header text-secondary", 'Physical Activity'),
                                               pa_plot)),
                                           
                                           div(class= 'grid-item w-50', risk_graph ) #p-5 m-5
                                       ),
                                  
                                  div(class = 'my-5 py-5 alert bg-subtletext-black',
                                      h3("Modifiable Risk"),
                                      p(class = 'lead', "Modifable with an extended range of modifiable and physiological risk factors")
                                  ),
                                  div(class = "m-5 p-5 d-flex flex-row gap-3 flex-wrap justify-content-start",
                                      #class= 'grid-item border-4 w-75 p-5 m-5',
                            
                                      reduced_heatmap_risk_factors )
                                  
                                       # div(class = "d-flex flex-row gap-3 flex-wrap justify-content-around",
                                       #     graph_wrapper(header = 'BMI Age', metric_chart_bmi_age),
                                       #     graph_wrapper(header = 'BMI Sex', metric_chart_bmi_sex),
                                       #     graph_wrapper(header = 'BMI and Diet Risk', diet_plot),
                                       #     graph_wrapper(header = 'BMI and Adequate Physical Activity', pa_plot)
                                       # )
                                  )
                              ),
                              
                              # Geography tab ----
                              div(id = "geography-tab", class = "tab-pane show active", style = " ",
                                  
                                  
                                  # div(style="top:50px;right:15px;width:150px;z-index:1000",
                                  #     class=" ms-2 p-3 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card",
                                  # h6('On this Page'),
                                  #     tags$ul(
                                  #        tags$li(tags$a( href="#table",'Drilldown Geography hotspots')),
                                  #        tags$li(tags$a( href="#table",'Top settlements')
                                  #        )
                                  #     )
                                  # ),
                                  
                                  div(class="position-fixed glass-card mb-3 p-2 z-3 w-100 rounded shadow-lg",
                                  style="top: 40px; left: 0px; height: 40px;",
                                  
                                  div(class=  'align-items-end d-flex justify-content-evenly',
                                      
                                    h6(class = 'text-muted','On this Page'), 
                                    # p('|'),
                                    h6(class = 'fw-lighter', 'Hotspot Drilldown'), 
                                    # p('|'),
                                    h6(class = 'fw-lighter', 'Towns and Villages Table')
                                      ),
                                  ),
                                    
                                    
                                  div(class = "container-fluid",   style = "padding-left: 150px", #;width:100%
                                      h3("Geography Analytics"),
                                      
                                      
                                      #div(class = '',style = 'height:100px;width:100px;position:absolute;top:20px;right:40px;',  leafletOutput('custom')),
                                      
                                      p(class = 'lead',"A supplement to the analytics showing the heirarchical 
                        'hotspots' broken down by geographical, administrative or statistical areas. The values plotted are at-risk weight, meaning overweight and obesity"),
                                      div(class = " rounded-5 bg-light d-flex flex-row gap-2 justify-content-around",
                                          div(style = 'width:500px;', class= 'grid-item p-1 m-3', #width:30vw;height:60vh;
                                              div( class= 'grid-item-content', #need this to render echarts correctly
                                              div(class = "chart-card",
                                                  div(class = "card-header",# style = "font-size: 0.5em;",
                                                      'Investigate Hierachical Geography',
                                                      span(class = 'text-secondary',
                                                           icon('warning'),
                                                           'Due to the large data size involved, only most deprived and most prevalent Geographies are plotted.'
                                                      )
                                                  ),
                                                  # geo_sunburst
                                                  echarts4rOutput('geo_sunburst',height = '800px')
                                              )
                                                  
                                                  
                                                  )), 
                                          div(style = '', class= 'flex-2 grid-item p-1 m-3', #width:30vw;height:60vh;
                                              div(class= 'grid-item-content', #need this to render echarts correctly
                                              div(class = "chart-card", style = 'height:500px;',
                                                  
                                                  div(class = "card-header",# style = "font-size: 0.5em;",
                                                      'Trust Level Obesity Prevalence',
                                                      span(class = 'text-bg-secondary',
                                                           icon('mouse'),'Use mouse to scroll to zoom in/out, click to transform'
                                                      )
                                                  ),
                                                  echarts4rOutput('geo_treemap')
                                              )
                                              
                                                  # geo_treemap
                                              )#,
                                              
                                              #bar_map_morph
                                          )
                                          
                                      ),
                                      
                                      div(class = 'mt-5 pt-5', style='font-size:12px;',id='table',
                                          
                                      h3(" Settlement Analytics"),
                                      
                                      p(class = 'lead', "Analysis of Obesity dynamics in the Population by their settlement"),
                                      
                                      div(class = 'mx-5 px-5',
                                          
                                          formatted_table
                                      )
                                      
                                      )
                                      
                                      # div(class = "rounded-5 bg-light d-flex flex-row gap-3 m-5 flex-wrap justify-content-centre",
                                      #     div(style = 'width:70vw', class= 'grid-item p-1 m-2',
                                      #         geo_treemap
                                      #     )
                                      # )
                                  )
                              ),
                              
                              #population tab ----
                              div(id = "population-tab", class = "tab-pane show active ", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 200px;",
                                      h3(" Obesity Analytics"),
                                      
                                      p(class = 'lead', "Analysis of Obesity dynamics in the Population and interaction with socio-economics and comorbidity"),
                                      
                                      div(class = "rounded-5 bg-light d-flex flex-row gap-3 flex-wrap justify-content-around",
                                          div(style = '', class = 'grid-item p-5 m-5',
                                              div(class = "card-header",# style = "font-size: 0.5em;",
                                                  'The distribution of Risk (CVD) with Age',
                                                  span(class = 'text-bg-secondary p-1 m-1 rounded-1',
                                                       'Note Age is the predominant factor in determining risk, yet there is modifiable 
                                                       variance to the risk per age group determining if an individual is at greater or lesser risk')
                                              # ),
     
                                          ),
                                              risk_prob_density_fn_age10
                                          )
                                      ),
                                      
                                      div(class = "rounded-5 bg-light d-flex flex-row gap-3 flex-wrap justify-content-around",
                                          div(style = '', class = 'grid-item p-5 m-5',
                                              div(class = "card-header",# style = "font-size: 0.5em;",
                                                  'Prevalence of Obesity and Overweight with Deprivation by age',
                                                  span(class = 'text-bg-secondary p-1 m-1 rounded-1',
                                                       'The gradient is neutral to negative, to varying degrees among age groups'),
                                              tags$small(#class = 'text-bg-secondary',
                                                'The dynamics of obesity with deprivation become clearer by hovering the legend, or points one by one, left to right.')
                                              ),
                                              deprivation_bmi_age_chart
                                          )
                                      ),
                                      
                                      br(),br(),br(),
                                      #h6('Top Obesity Towns by Prevalence'),
                                      br(),br(),br(),
                                      
             
                                      
                                      h3(" Obesity Analytics"),
                                      p(class = 'lead', "Analysis of Obesity dynamics in the Population and interaction with socio-economics and comorbidity"),
                                      
                                      div(class='d-flex flex-row flex-wrap gap-3 justify-content-evenly',# justify-content-between
                                          
                                          #div(style = 'width:70vw',bar_map_morph),
                                          # graph_wrapper(obesity_effects_treemap),
                                          # graph_wrapper(obesity_effects_sunburst),
                                          
                                          graph_wrapper(header = 'Sex and BMI',
                                                        metric_chart_bmi_sex %>% e_grid(top='0%') %>%  e_theme('walden')),
                                          graph_wrapper(header = 'Age and BMI',
                                                        metric_chart_bmi_age %>% e_theme('walden')),
                                          
                                          graph_wrapper(header = 'Depression and BMI',
                                                        depression_obesity_chart),
                                          # graph_wrapper(pm25g_urban_chart),
                                          
                                          graph_wrapper(header = span('Environmental Pollution and BMI',
                                                                      span(class = 'text-muted',
                                                                    'Propensity for Obesity rises moderately with pollution levels'
                                                                      )),
                                                        pm25g_bmi_scatter_chart),
                                          
                                          graph_wrapper(header = 'Sleep and BMI',
                                                        sleep_bmi_chart),
                                          
                                          graph_wrapper(header = '[Reminder] Townsend Distribution ',
                                                        townsend_distribution_chart),
                                          
                                          graph_wrapper(header = '[Reminder] Risk Distribution ',
                                                        qrisk_distribution_chart),
                                          
                                          graph_wrapper(header = 'Comorbidities and BMI',
                                                        comorbidities_plot),
                                          
                                          graph_wrapper(header = 'Avg Cambridge Multimorbidity Score with BMI',
                                                        BMI_cmms_plot
                                                        ),
                                          
                                          graph_wrapper(header = span('Modifiable Risk', 
                                                                      span(class = 'text-secondary',
                                                                           'Risk factors that are behavioural and can be changed'
                                                                           )),
                                                        corisk_modifiable_chart),
                                          graph_wrapper(header = 'Servere Comorbid Risks',
                                                        span(class = 'text-secondary',
                                                             'Minor or mild morbidities that act or signal advanced risk'),
                                                        comorbid_risk_chart),
                                          graph_wrapper(header = 'All Risk Correlates',
                                                        span(class = 'text-secondary',
                                                             'All contributors to a risk profile'),
                                                        corisks_chart)
                                          
                                          # graph_wrapper(metric_chart_bmi_age),
                                          # graph_wrapper(metric_chart_bmi_sex),
                                      ),
                                      
                                      div(class = 'alert py-5 my-5 bg-subtle text-black',
                                      h3(" Socio-economics Analytics"),
                                      p(class = 'lead', "Analysis of Obesity dynamics in the Population and interaction with socio-economics and comorbidity")
                                      ),
                                      
                                      div(class='d-flex flex-row flex-wrap gap-3 justify-content-evenly',# justify-content-between
                                          
                                          
                                          graph_wrapper(header = 'Income Decile and BMI',
                                                        income_plot %>% e_grid(top = '0%')
                                                        ),
                                          graph_wrapper(header = 'Employment Decile and BMI',
                                                        employment_plot %>% e_grid(top='0%')
                                                        ),
                                          graph_wrapper(header = 'Neighbourhood Renewal Area and BMI', NRA_plot),
                                          graph_wrapper(header = 'Health and Social Care Trust, and BMI',HSCT_plot)
                                          ),
                                      
                                      h3(" Comorbidity Analytics"),
                                      p(class = 'lead', "Analysis of Obesity dynamics in the Population and interaction with socio-economics and comorbidity"),
                                      div(class='d-flex flex-row flex-wrap gap-3 justify-content-evenly',# justify-content-between
                                          
                                          
                                          graph_wrapper(header = 'Hypertension and BMI',hypertension_plot),
                                          graph_wrapper(header = 'Atrial Fibrillation and BMI',af_plot),
                                          graph_wrapper(header = 'Ethnicity and BMI',ethnicity_plot),
                                          graph_wrapper(header = 'Peripheral Arterial Disease and BMI',pad_plot),
                                          graph_wrapper(header = 'Chronic Kidney Disease and BMI',ckd_plot),
                                          graph_wrapper(header = 'Cholesterol and BMI',cholesterol_plot),
                                          graph_wrapper(header = 'Smoking and BMI',smoke_plot),
                                          graph_wrapper(header = 'Alcohol and BMI',alcohol_plot),
                                          graph_wrapper(header = 'Diet and BMI',diet_plot),
                                          graph_wrapper(header = 'Physical Activity and BMI',pa_plot)
                                      
                                      )
                                  )
                              ),
                              
                              # Inequalities tab ----
                              div(id = "Inequalities-tab", class = "tab-pane active show", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 150px;",
                                      h3(" Deprivation Analytics"),
                                      div(style= 'width:100%;height:100%;',
                                          adv_echarts
                                      )
                                  )
                                  
                              ),
                              
                              # INT tab ----
                              div(id = "int-tab", class = "tab-pane active show", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 150px;",
                                      h3(" Integrated Neighbourhood Team Analytics"),
                                      p(class='lead', "Drill down on Obesity in the Integrated Neighbourhood Teams - a particular theme of 'left shifting'
                                        healthcare provision in Northern Ireland. INTs were formly GP Federations and Integrated Care Services"),
                                      br(),br(),
                                      # tags$ul(tags$li('Top SOA DEA Settlements'),
                                      #         # tags$li('Split'),
                                      #         # tags$li(' Overview'),
                                      #         # metric_card_costs_total_obesity,
                                      #         # metric_card_nhs_obesity,
                                      #         # metric_card_society_obesity,
                                      # )
                                      div(class = 'py-3 my-3 d-flex justify-content-center bg-light rounded-5 ', 
                                      div(class = 'w-50 h-100 rounded-4 bg-white p-2 m-2', 
                                          
                                          INT_bmi_geo_matrix)
                                      ),
                                      br(),br(),
                                      
                                      div(class= 'd-flex flex-column flex-wrap gap-3 justify-content-start pt-3 mt-3',
                                          div(class = 'h-25',
                                          div(class= 'd-flex flex-row flex-wrap gap-3 justify-content-start',
                                              div(class= 'd-flex flex-column flex-wrap gap-3 justify-content-start',
                                                  
                                                div(style = 'height:200px;',
                                                    class = 'border rounded-3',
                                                    bar_each_bmi_normalised ),
                                                div(style = 'width:500px;height:500px;', 
                                                    class = 'border rounded-3', 
                                                    map_bmi_int )
                                                ),
                                              div(class = 'border rounded-3',
                                              div(class= 'd-flex flex-row flex-wrap gap-3 justify-content-start',
                                                  
                                                div(style = 'width:300px;height:450px;', bar_overweight ),
                                                div(style = 'width:300px;height:450px;', bar_obese )
                                              ),
                                                div(style = 'height:250px;', bar_bmi_normalised )
                                              )
                                          )
                                          )
                                  
                              ),
                              
                              div(class = 'pt-5 mt-5',
                              h3(" INTs and HSCT distributions"),
                              p(class='lead', " The distribution of at- risk BMI (overweight or obese) in TRUSTs with averages by INT.
                                Each component unit is a Super Data Zone.")
                              ),
                              br(), br(), br(),
                              div(class= 'd-flex flex-row flex-wrap gap-1 justify-content-evenly',
                                  span(class ='text-secondary', 'BMI populations'),
                                  div(style = 'width:550px;height:400px', federation_bmi_hsct_beeswarm),
                                  span(class ='text-secondary', 'Deprivation'),
                                  div(style = 'width:550px;height:400px', federation_town_hsct_beeswarm)
                              ),
                              br(), br(), br(),
                              h3(" Obesity and the Environment"),
                              p(class='lead', "Look at Obesity and the environment, including factors causing and alleviating heightened BMI"),
                              
                              div(class= 'd-flex flex-row flex-wrap gap-3 justify-content-center',
                                
                                  graph_wrapper(header = 'Obesity with pub concentration by sex',
                                  span(class = 'text-muted','Males are affected by pubs more than women'),
                                                males_affected_by_pubs_more_than_women),
                                  
                                  graph_wrapper(header = 'Obesity with Food Outlet concentration by sex',
                                  span(class = 'text-muted','Males are affected by Food Outlets more than women'),
                                                
                                                males_affected_by_food_outlets_more_than_women),
                                  
                                  graph_wrapper(header = 'Obesity with Green Space concentration by sex',
                                                span(class = 'text-muted', 'Females respond less positively to Green Space'),
                                                females_respond_less_positively_to_green_space)
                                  ),
                              div(class= 'd-flex flex-row flex-wrap gap-3 justify-content-center',
                                  
                                  graph_wrapper(header = 'Obeesity with Green Space Proximity by Urban - Rural',
                                                span(class = 'text-muted', 'Semi Rural responds best to Green Space'),
                                                
                                                semi_rural_responds_best_to_green_space),
                              
                                  graph_wrapper(header = 'Obesity with Fast Food Concentration',
                                                span(class = 'text-muted', 'Fast Food Outlets weight on Obesity'),
                                                
                                                fast_food_outlets_weight_on_obesity),
                              
                                  graph_wrapper(header = 'Obesity with Pub concentration',
                                                span(class = 'text-muted', 'Pubs Weight on Obesity'),
                                                
                                                pubs_weight_on_obesity)
                              ),
                              
                              
                                  )
                              ),
                              
                              div(id = "NorthernTrust-tab", class = "tab-pane active show", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 150px;",
                                      h3(" Northern Trust Analytics"),
                                      p(class='lead', "Drill down on Obesity in the (Northern) Trust - a particular focus of the OPIP deployment"),
                                      # tags$ul(tags$li('Top SOA DEA Settlements'),
                                      #         # tags$li('Split'),
                                      #         # tags$li(' Overview'),
                                      #         # metric_card_costs_total_obesity,
                                      #         # metric_card_nhs_obesity,
                                      #         # metric_card_society_obesity,
                                      # )
                                              top_town_per_hsct_plot
                                  )
                              ),
                              
                              #Lifestyle-tab ----
                              
                              div(id = "lifestyle-tab", class = "  tab-pane show active ", style = " ",
                                  div(style = "padding-left: 150px;",
                                      h3("Lifestyle Dashboard"),
                                      p(class = 'lead',"Combinations of Factors including socio-economic ones affecting
                                        obesity"),
                                      # h6('Health Burden Attributable to obesity'),
                                          
                                          # graph_wrapper(header = span(div('BMI with deprivation'),
                                          #                             span(class='text-muted','Positive Trend')),
                                          #               deprivation_risk_by_bmi_chart),
                                          
                                          div(class = "rounded-5 bg-light d-flex flex-row gap-3 flex-wrap justify-content-around",
                                              div(style = '', class = 'grid-item p-5 m-5',
                                                  div(class = "card-header",# style = "font-size: 0.5em;",
                                                      'Proportion of population at each BMI with Increasing Deprivation',
                                                      span(class = 'text-bg-secondary',
                                                           'As is sometimes the case Obesity grows faster than overweight in the 
                                                           presence of real catalyst ')
                                                  ),
                                                  deprivation_risk_by_bmi_chart
                                              )
                                          ),
                                      br(),br(),br(),
                                      
                                      div(class = "d-flex flex-row flex-wrap gap-3 justify-content-evenly",
                                          
                                          
                                          #https://www.figma.com/colors/cyan/
                                          metric_card(top = '2.0%',  opacity = 'opacity-75',
                                                      'Prevalence Most and Least Deprived Quintile','Inequality in Overweight', 
                                                      color='#00CCCC'),
                                          metric_card(top = '4.8%',  opacity = 'opacity-75',
                                                      'Prevalence Most and Least Deprived Quintile',
                                                      'Inequality in Obesity', 
                                                      color='#00CCCC'),
                                          
                                          metric_card(top = '34%',  opacity = 'opacity-75',
                                                      'Prevalence of Overweight','Absolute Prevalence in Overweight', 
                                                      color='#00FF80'),
                                          metric_card(top = '22%', opacity = 'opacity-75',
                                                      'Prevalence of Obesity','Absolute Prevalence in Obesity', 
                                                      color='#00AAFF'),
                                          
                                          metric_card(top = '80-100',  opacity = 'opacity-75',
                                                      'Relative Prevalence', 
                                                      'Greatest Inequality At Risk BMI',
                                                      color='#ffc800'),  # %>% browsable() %>% page_fluid()
                                          
                                          metric_card(top = '60-80',  opacity = 'opacity-75',
                                                      'Absolute Prevalence', 
                                                      'Most At Risk BMI',
                                                      color='#0055FF'),  ##00FFAA' %>% browsable() %>% page_fluid()
                                          
                                          metric_card(top = 'Bottom Quintiles',  opacity = 'opacity-75',
                                                      'Absolute Prevalence', 
                                                      'Most At Risk BMI',
                                                      color='#0055FF')#,  # %>% browsable() %>% page_fluid()
                                          
                                          # metric_card(top = 'Bottom 2 Quintiles, 40-80 year olds',  opacity = 'opacity-75',
                                          #             'Absolute Prevalence',
                                          #             'Remain highest At Risk BMI',
                                          #             color='#0055FF')
                                      ),
                                      br(),br(),br(),
                                      div(class = "d-flex flex-row flex-wrap gap-3 justify-content-evenly",
                                          graph_wrapper(header = 'Inequality in Obesity',obese_inequality_chart),
                                          graph_wrapper(header = 'Inequality in Overweight',overweight_inequality_chart),
                                          graph_wrapper(header = span('Slope of inequality ',
                                                                      span(class = 'text-muted','Least Deprived minus Most Deprived')),inequality_chart),
                                      ),
                                      br(),br(),br(),
                                          
                                            h3("Deprivation Risk with Age "),
                                          p(class = 'lead',"Track the best fit of the percentage of the population that are at risk BMI with 
                                            increasing deprivation."), 
                                          span(class = 'text-bg-secondary p-1 m-1 rounded-2',
                                          'The trend shows the clear predominanc of obesity prevalence on age, but that older obesity prevalence increases
                                          with deprivation '),
                                          span(class = 'text-secondary',
                                               'Deprivation increases going right'),
                                          
                                          
                                      div(class = "d-flex flex-row flex-wrap gap-5 justify-content-evenly",
                                      deprivation_risk_by_age20_chart
                                      ),
                                      
                                      
                                      h3(" DEA level Analysis"),
                                      p(class = 'lead',"Obesity and Overweight prevalence by District Electoral Area (nested by HSCT)"),
                                      
                                      
                                      div(class = "d-flex flex-row flex-wrap gap-5 justify-content-evenly",
                                      DEA_obesity_prevalence
                                      )
                                      
                                      
                                          
                                          
                                          # metric_card(top = 'Health Metric','','',color='teal'),
                                      
                                          
                       
                                      
                                  )
                              ),
                              
                              # deprivation Tab -----
                              div(id = "deprivation-tab", class = "  tab-pane active show", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 150px;",
                                      h3("HotSpots "),
                                      
                                      h6(class='lead', "Hotspots of deprivation gradients"),
                                      
                                      
                                      div(class= 'd-flex flex-row gap-5 mx-5',
                                      div(style = 'background-color: rgb(206,55,88);',
                                          class = 'p-2 w-25 rounded-3 text-white', 
                                          span('Top Obese'),
                                          h4('28.8%'),
                                          span('MDM Most Deprived, NHSCT')
                                          ),
                                      
                                      div(style = 'background-color: rgb(206,55,88);',    
                                          class = 'p-2 w-25 rounded-3 text-white', 
                                          'Top Overweight',
                                          h4('41.9%'),
                                          span('MDM Most Deprived, WHSCT')
                                      ),
                                      
                                      div(style = 'background-color: rgb(206,55,88);',   
                                          class = 'p-2 w-25 rounded-3 text-white', 
                                          'Second Obese',
                                          h4('27.9%'),
                                          'MDM Most Deprived, SHSCT'
                                      ),
                                      
                                      div(style = 'background-color: rgb(206,55,88);',   
                                          class = 'p-2 w-25 rounded-3 text-white', 
                                          'Second Overweight',
                                          h4('41.4%'),
                                          'MDM Second Most Deprived, WHSCT'
                                      )
                                      ),
                                      
                                      #top obese
                                      # 26         Most Deprived  NHSCT      obese  646 2237 0.28877962
                                      # 27         Most Deprived  SHSCT      obese  942 3370 0.27952522
                                      
                                      #top overweight
                                      # 1         Least Deprived  WHSCT overweight  143  341 0.41935484
                                      # 2             Quintile 2  WHSCT overweight 2105 5076 0.41469661
                                      
                                      div(class="grid-5x5 mx-3 px-3",
                                          style= 'display: grid;
                                             grid-template-columns: repeat(5, 1fr);
                                             grid-auto-rows: 1fr;            /* equal heights within a fixed container */
                                               gap: 10px;
                                             /* optional: fix container height so rows are equal; or remove and let content size */
                                              ',
                                          group_echarts),
                                  )
                              ),
                              
                              # Society tab ----
                              div(id = "society-tab", class = "tab-pane show active", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 150px;",
                                      h3(" Effects of Disease"),
                                      p(class = 'lead', "Analysis on societal impact of Obesity including social and informal costs, lost productivity, societal pressures, and hospital infrastructure"),
                                      # tags$ul(tags$li('Cost'),
                                      #         tags$li('Sick days'),
                                      #         tags$li('Bed days'),
                                      #         tags$li('Avg LoS for a bed related morbidity'),
                                      
                                      # h5('Incident Disease and Risk',class = 'p-3 m-3 border-bottom bg-light'),
                                      # sm_hatched_subtitle('Incident Disease and Risk'),
                                      
                                      # hatched_subtitle('Attributable Disease'),
                                      
                                      div(class= 'p-1 m-1',
                                      div(class = 'bg-light px-5 m-4 rounded-4'  ,   
                                      sm_hatched_subtitle('Attributable Fractions of high BMI on select morbidity across disease classes'),
                                      # h5('Attributable Fractions',class = 'p-3 border-bottom bg-light'),
                                      
                                      p(class = 'lead', "Incidence is reported, inferred or derived. The Interpretation here is how much new incident Disease is averted by completely 
                                        eradictating exposure to the risk factor")
                                      )
                                      ),
                                      
                                      div(class = 'mb-5 pb-5 d-flex flex-row flex-wrap justify-content-center gap-5',
                                          
                                      paf_bmi
                                      
                                      ),
                                      
                                      div(class= 'pt-5 mt-5 px-3 mx-3',
                                          
                                      sm_hatched_subtitle('Absolute Attributable Disease of high BMI grouped by disease classes'),
                                      p(class = 'lead', "Incidence is reported, inferred or derived. The Interpretation here is how much new incident Disease is averted by completely 
                                        eradictating exposure to the risk factor")
                                      
                                      ),
                                      
                                      div(class = 'd-flex flex-row flex-wrap justify-content-center gap-5',
                                          
                                      absf_bmi
                                      
                                      ),
                                      
                                      br(),br(),br(),
                                      
                                      div(class ='pt-5 mt-5',
                                      h3(
                                         "Society, Productivity and Cost Analysis on the effects of Disease"),
                                      p(class = 'lead', "Analysis on societal impact of Obesity including social and informal costs, lost productivity, societal pressures, and hospital infrastructure"),
                                      ),
                                      div(class = 'my-3',
                                          h5('Costs, Pounds (millions) of Cardiovascular Disease',
                                             class = 'alert border-bottom border-danger',
                                             style = 'background-color: rgb(233,74,94);'
                                             )
                                      ),
                                      
                                      div(class = 'mx-5 px-5 d-flex flex-row flex-wrap justify-content-start gap-3',
                                          metric_card_nhs_obesity,
                                          metric_card_society_obesity,
                                          metric_card_inpatient_obesity,
                                          metric_card_outpatient_obesity,
                                          metric_card_AE_obesity,
                                          metric_card_primary_care_obesity,
                                          metric_card_med_obesity,
                                          metric_card_medical_device_obesity,
                                          metric_card_long_term_obesity,
                                          metric_card_morbidity_obesity,
                                          metric_card_mortality_obesity,
                                          metric_card_informal_care_obesity
                                          
                                          
                                          ),
                                          span(class=' text-muted mb-3', 'Source: The British Heart Foundation'),
                                      
                                      div(class = 'my-3',
                                      h5('Productivity costs',class = 'mt-3 pt-3 alert alert-info border-bottom border-info ')
                                          ),
                                      
                                      div(class = 'mx-5 px-5  d-flex flex-row flex-wrap justify-content-start gap-3 pt-3 mt-3',
                                          metric_card_obesity_days_lost_obesity,
                                          metric_card_obesity_spells_obesity,
                                          metric_card_obesity_cost_obesity,
                                          metric_card_obesity_days_lost_population,
                                          metric_card_obesity_spells_population,
                                          metric_card_obesity_cost_population
                                      ),
                                      
                                      span(class=' text-muted mb-3', 'Source: NISRA - NICS sickness stats, PHA- Population health modelling'),
                                      
                                      #h6('Resource'),
                                      
                                      div(class = 'my-3',
                                      h5('Burden of Disease',class = 'alert alert-info border-bottom border-info')
                                      ),
                                      
                                      div(class = "mx-5 px-5  d-flex flex-row flex-wrap gap-3 justify-content-start  pt-3 mt-3",
                                          #metric_card(top = 'Resource','','',color='Purple',opacity = 'opacity-75'),
                                          # h2('Health Metrics'),
                                          metic_card_prev_total_obesity,
                                          metic_card_inc_total_obesity,
                                          metric_card_YLL_total,
                                          metic_card_daly_total_obesity,
                                          metic_card_yld_total_obesity
                                      ),
                                      
                                      span(class=' text-muted mb-3', 'Source: PHA- Population health modelling'),
                                          
                                      div(class = 'my-3',
                                    h5('Resource Metrics',class = 'alert alert-info border-bottom border-info')
                                      ),
                                          div(class = " mx-5 px-5 d-flex flex-row flex-wrap gap-3 justify-content-start ms-5 ps-5 pt-3 mt-3",
                                              
                                          metric_card_total_bed_days,
                                          metric_card_costs_total_obesity,
                                          metric_card_obesity_days_lost_obesity,
                                          metric_card_obesity_cost_obesity
                                          
                                      ),
                                      span(class=' text-muted mb-3', 'Source: DoH - Outpatient Statistics, PHA- Population health modelling'),
                                      
                                      # h6('Comorbidities'),
                                      #div(class = "d-flex flex-row flex-wrap gap-3 justify-content-between",
                                          
                                          # metric_card(top = 'Resource','','',color='purple',opacity = 'opacity-100'),
                                          
    
                                          # graph_wrapper(comorbidities_curve_wo_0_or_1),
                                          # graph_wrapper(comorbidities_age),
                                          # graph_wrapper(comorbidities_bmi),
                                          
                                      #),
                                      #     
                                      #     h6('Relative Risks'),
                                      # 
                                      #      obesity_rr_table(obesity_rr_demo, p_obesity = 0.30),
                                      # 
                                      # h6('Disability Weights'),
                                      # 
                                      #     prevalence_dw_table(morbidity_prev_dw_demo, show_yld = TRUE)
                                  )
                              ),
                              
                              #scenarios tab ----
                              div(id = "scenarios-tab", class = "tab-pane active show", style = " ",
                                  div(
                                    style = "padding-left: 170px;",
                                    #h3("Scenarios Overview"),
                                    # p(class='lead',"Scenarios overview content is coming soon")
                                    scenarios_div()#,
                                    
                                    # div( #class = 'vh-100',
                                    #    
                                    #    div(class = 'd-flex justify-content-evenly align-center',
                                    #        div(id= 'left',
                                    #            sm_hatched_subtitle('Intervention Scenarios and Interventions'),
                                    #           render_model_registry(models)
                                    #        ),
                                    #        div(id = 'right vh-100 ',
                                    #            div(style = 'width:700px;',
                                    #                sm_hatched_subtitle('Intervention Outputs')
                                    #            ),
                                    #            div(class= 'position-static',
                                    #                # br(),br(),br(),br(),
                                    #                div(class ='d-flex align-center gap-2 flex-row justify-content-center',
                                    #                    div(style = 'height:350px;width:500px;padding:5px;',
                                    #                        plot
                                    #                    ), 
                                    #                    
                                    #                    div(class= 'd-flex justify-content-center flex-column gap-2 align-items-end',
                                    #                        div(style = 'width:70%', circular_value('10,212')),
                                    #                        div(style='height:20px;width:100%;margin-block:20px;',
                                    #                           rag_line()
                                    #                    ))
                                    #                )
                                    #            ))))
                                  )
                              ),
                              
                              #specifiy tab ----
                              div(id = "specify-tab", class = "tab-pane show active ", #style = "width:100vw",
                                  div(
                                    style = "width:95%;padding-left: 150px;",
                                    h3("Intervene "),
                                    h5(class = 'lead mb-5 ',
                                       "Simulate Interventions and innovative pathways to treat obesity by
                         estimating the disease output before and after modulating population prevalence for obesity"),
                                    # In UI
                                    
                                    div(class = 'd-flex flex-row gap-5',
                                        div(style = 'flex: 1 1 200px;',
                                            h5('Define Intervention Population'),
                                            p(class = 'lead ','Select the sub-population to target for intervention based on BMI and Health Risk Profile'),
                                            interventionSelectorUI("htn_int"),
                                            progress_pair_module_ui(
                                              "intervention_progress",
                                              left_width = 40,
                                              right_width = 10
                                            ),
#                                             HTML('<label for="customRange1" class="form-label">Example range</label>
# <input type="range" class="form-range" id="customRange1">'),
                                            
                                        ),
                                        div(style = 'flex: 2 1 100px;',
                                            
                                            h5('Choose Risk Profile to Modulate'),
                                            p(class = 'lead','Fixed at Obesity for the purposes of the usecase. 
                                            Choose the risk profile to modulate for the intervention population. This will determine the expected health benefits of the intervention'),
                                            
                                            disabled_risk_to_intervene,
                                            br(),br(),br(),
                                                h5(class ='mt-5 p-3 bg-light ', 'Specify Intervention Details'),
                                                p(class = 'lead','Choose intervention type, efficacy, duration and cost parameters'),
                                            div(class =' p-3 rounded-3', #bg-success-subtle
                                                intervention_module_ui("intervention1"),
                                                # actionButton(inputId = 'reset', label = 'Reset ', class = 'btn-success')
                                            ),
  br(),br(),
                                            div(class = '',
                                                div(width="80%", class="m-5 p-5 mb-3",
                                                    #bg-yellow
                                                HTML('
                                        
  <!-- <label class="form-label mt-4">Input addons</label> -->
  <h2> Cost Inputs </h2>
  <p class = "lead"> Choose any combination of dynamic cost to calculate final costs. Set to zero to disregard. </p>
  <div class = "px-5 mx-5">
  
        <p class="form-label mt-1 text-muted "> A flat or start up cost assigned to the first year of the intervention </p>
  <div>
    <div class="input-group mb-3">
      <span class="input-group-text  border-0">£</span> 
      <input id="cost_input_flat" type="number"  value="10000" class="form-control" aria-label="Amount (to the nearest pound)">
      
      <span class="input-group-text bg-white">.00</span>
                  <span class="input-group-text bg-dark">Flat Fixed Cost</span>

      <!-- <button class="btn btn-primary" type="button" id="apply-btn1">Apply</button> -->

    </div>
<p>A recurring cost of the intervention for its duration for each person <em> reached </em> </p> 
        <div class="input-group mb-3">
      <!--<span class="input-group-text bg-teal">£</span> 
      <span class="input-group-text"> 
      </span>
      -->

      <span class="input-group-text bg-teal">£</span>
      <input id="cost_input_per_person_per_year" type="number"  value="2" class="form-control" aria-label="Amount (to the nearest pound)">
            <span class="input-group-text bg-white">.00</span>
            <span class="input-group-text bg-dark">per person</span>
            <span class="input-group-text bg-dark">per year</span>

     <!--  <button class="btn btn-primary" type="button" id="apply-btn2">Apply</button>  -->

    </div>
        
      <p class="form-label mt-1 text-muted ">   A cost per number of people <em>reached </em>. It is assigned to the first year for accounting </p>
        <div class="input-group mb-3">
      <span class="input-group-text bg-teal">£</span>
      <input id="cost_input_per_person" type="number"  value="2" class="form-control" aria-label="Amount (to the nearest pound)">
      <span class="input-group-text bg-white">.00</span>
      <span class="input-group-text bg-dark ">per person</span>

     <!--  <button class="btn btn-primary" type="button" id="apply-btn3">Apply</button>  -->

    </div>

      <p class="form-label mt-1 text-muted "> A recurring cost assigned to each year of the intervention for its duration, regardless of intensity </p>
     <div class="input-group mb-3">
      <span class="input-group-text bg-teal">£</span>

      <input id="cost_input_per_year" type="number" value="1000" class="form-control" aria-label="Amount (to the nearest pound)">
      <span class="input-group-text bg-white">.00</span>
      <span class="input-group-text bg-dark ">per year</span>
      
      </div>


    </div>

  </div>'),
actionButton(class = 'btn-dark', 'apply_costs','Apply'),
                                            ),
                                            # Display current cost values
                                            
      
                                            div(class = "mt-4 p-3 bg-light rounded",
                                                h6(class='lead'," Cost Estimates:"),
                                                div(class = "row",
                                                    div(class = "col-md-6",
                                                        tags$div(tags$strong("Flat Fixed Cost: "), textOutput("cost_flat_fixed", inline = TRUE)),
                                                        tags$div(tags$strong("Per Person Per Year: "), textOutput("cost_per_person_per_year", inline = TRUE))
                                                    ),
                                                    div(class = "col-md-6",
                                                        tags$div(tags$strong("Per Person: "), textOutput("cost_per_person", inline = TRUE)),
                                                        tags$div(tags$strong("Per Year: "), textOutput("cost_per_year", inline = TRUE))
                                                    )
                                                ),
                                                tags$br(),
                                                tags$p(class = "lead mt-3 ", tags$strong(textOutput("total_intervention_cost", inline = TRUE)))
                                            ),
                                            
                                        )
                                                )
                                    ),
                                    
                                    # inputPanel(
                                    #   input_switch('hello','Simplify'),
                                    #   radioButtons('target_mode',
                                    #                'Target Sub Population:',
                                    #                choices = c('Most unhealthy BMI',
                                    #                            'Riskiest Health profile(Not necessarily highest BMI)',
                                    #                            'Random target of unhealthy BMI')
                                    #   )
                                    #   ),
                                    # p('Assumes 100% Efficacy'),
                                    
  br(),
  br(),
            
div( class='w-100',                       actionButton(class='float-right w-100 btn-warning pb-2 mb-3 ',inputId = 'e','Run',icon = icon('play')),
                                    br(),
                                    br(),
  
  h3("Intervene "),
  h5(class = 'lead mb-5 ',
     "Outputs")
  ),       
             chartUpdateModuleUI("demo-chart"),
                                    br(),
                                    br(),
                                    #numericInput('n_sim','Number of Simulations',value = 10,min=1,max=1000,step=1),
                                    
                                    progress_component(),
                                    
                                    # Morbidity summary reactable table
  
                                    # div(class = "mt-5",
                                    #     div(class = "card-header", "Morbidity Summary Table"),
                                    #     reactableOutput("morbidity_summary_table", height = "600px")
                                    # ),
                                    
   
div(id = 'outputs', class= 'container',                                
div(id = 'outputs-left', class= 'w-75',
                                    br(),br(),
div(class= 'd-flex gap-2',
                                    div(class = "bg-light-subtle p-2 m-2 w-50 rounded-4",
                                        div(class = "grid-item-content",
                                            div(class = "p-2 fw-bold fs-5", "Stroke Incidence"),
                                            echarts4rOutput("stroke_incidence_chart", height = "250px")
                                        )
                                    ),
  
  div(class = "bg-light-subtle p-2 m-2 w-50 rounded-4",
      div(class = "grid-item-content",
          div(class = "p-2 fw-bold fs-5", "Stroke Prevalence"),
          echarts4rOutput("stroke_prevalence_chart", height = "250px")
      )
  ) ),
                                    
div(class= 'd-flex gap-2 flex-row',
    
  div(class = "bg-light-subtle p-2 m-2 w-50 rounded-4",
      div(class = "grid-item-content",
          div(class = "p-2 fw-bold fs-5", "CHD Incidence"),
          echarts4rOutput("chd_incidence_chart", height = "250px"),
      )
  ),
  
  div(class = "bg-light-subtle p-2 m-2 w-50 rounded-3 rounded-4",
      div(class = "grid-item-content",
          div(class = "p-2 fw-bold fs-5", "CHD Prevalence"),
          echarts4rOutput("chd_prevalence_chart", height = "250px")
      )
  )
  ),

div(class = 'd-flex flex-row justify-content-center align-items-center gap-3',
    
                                    # ICER charts
                                    div(class = "bg-light-subtle p-2 m-2 rounded-4 w-75",
                                        div(class = "grid-item-content",
                                            div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Intervention Costs and Savings"),
                                            echarts4rOutput("icer_costs_chart", height = "250px")
                                        )
                                    ),
    
    div(
      
        div(dot('#3fb1e3'),  div(style = 'display:inline;font-weight:bold;color:grey', ' Cost of intervention ')),br(),
        div(dot('#6be6c1'), div(style = 'display:inline;font-weight:bold;color:grey', 'Concrete healthcare provision Savings  ')),br(),
        div(dot('#626c91'), div(style = 'display:inline;font-weight:bold;color:grey', ' Net Return of Savings Minus Cost')),br(),
        div(dot('#a0a7e6'), div(style = 'display:inline;font-weight:bold;color:grey', ' Quality Adjusted Life Years (QALYS) costed at a rate of 1 QALY = £20,000')),br()
      
    )
    ),

  div(class = 'd-flex flex-row gap-3',
                                    div(class = "bg-light-subtle p-2 m-2 theme-green w-50 rounded-4",
                                        div(class = "grid-item-content",
                                            div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Return on Investment (ROI)"),
                                            echarts4rOutput("icer_roi_chart", height = "250px")
                                        )
                                    ),
                                    div(class = "bg-light-subtle p-2 m-2 theme-green w-50 rounded-4",
                                        div(class = "grid-item-content",
                                            div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "ICER and QALY Gains"),
                                            echarts4rOutput("icer_qaly_chart", height = "250px")
                                        )
                                    )
                                    ),
                                    
                                    br(),
                                    br(),


div(class = 'd-flex flex-row gap-3',
  div(class = "bg-light-subtle p-2 m-2 theme-green w-50 rounded-4",
        div(class = "grid-item-content",
            div(class = "p-2 fw-bold fs-5 pt-2 ps-2", " Productivity impact"),
            div(class = "lead fs-5 ps-2  pb-2", "Sick Spells"),
            echarts4rOutput("illness_chart", height = "200px"),
            div(class = "lead fs-5 ps-2  pb-2", "Cost"),
            echarts4rOutput("illness_chart1", height = "180px"),
            div(class = "lead fs-5 ps-2  pb-2", "Productive Days Lost"),
            echarts4rOutput("illness_chart2", height = "180px")
            
        )
    ),
    div(class = "bg-light-subtle p-2 m-2 theme-green w-50 rounded-4",
        div(class = "grid-item-content",
            div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Hospital Impact"),
            div(class = "lead fs-5 ps-2  pb-2", "Bed Days"),
            echarts4rOutput("hospital_admissions_chart", height = "200px"),
            div(class = "lead fs-5 ps-2  pb-2", "Emergency Admissions"),
            echarts4rOutput("hospital_admissions_chart1", height = "180px"),
            div(class = "lead fs-5 ps-2  pb-2", "Admissions"),
            echarts4rOutput("hospital_admissions_chart2", height = "180px")
            
        )
    )
),

h3("Advanced Morbidity Metrics "),
h6(class='lead', "Investigate morbidity in combinations, and considering the effect of quality and quantity on life"),


div(class = "bg-light-subtle p-2 m-2 theme-green w-100 rounded-3 rounded-4 d-flex flex-row gap-3",
    div(class = "grid-item-content",
        div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Disability Adjusted Life Years (DALYs)"),
        echarts4rOutput("daly_chart", height = "250px")
    ),
    
    div(class = "grid-item-content",
        div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Averted DALYs"),
        echarts4rOutput("daly_chart_averted", height = "250px")
    ),
    
    div(class = "grid-item-content",
        div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Equality in DALYs"),
        echarts4rOutput("daly_chart_mdm", height = "250px")
    ),
    
    div(class = "grid-item-content",
        div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Averted DALYs"),
        echarts4rOutput("daly_chart_mdm_averted", height = "250px")
    )
),

div(class = "bg-light p-2 m-2 theme-green w-100 rounded-4 d-flex flex-row gap-3",
    div(class = "grid-item-content",
        div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Composition of Disability Adjusted Life Years by years lived with disease and years of life lost"),
        echarts4rOutput("daly_chart_composition", height = "250px")
    )
    ),
    
    div(class = "bg-light p-2 m-2 theme-green w-100 rounded-4 d-flex flex-row gap-3",
        div(class = "grid-item-content",
            div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Modelled Long Term Conditions Disease Cost"),
            echarts4rOutput("disease_cost_chart", height = "250px")
        ),
    br(), 
    br()
),
div(class = "bg-light p-2 m-2 theme-green w-100 rounded-4 d-flex flex-row gap-3",
    div(class = "grid-item-content",
        div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Modelled Long Term Conditions Disease Cost saved"),
        echarts4rOutput("disease_cost_chart_saved", height = "250px")
    ),
    br(), 
    br()
),

div(class = "bg-light p-2 m-2 theme-green w-100 rounded-4 d-flex flex-row gap-3",
    div(class = "grid-item-content",
        div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Modelled Disease Cost by indivdual Condition "),
        echarts4rOutput("disease_cost_breakdown", height = "250px")
    ),
    br(), 
    br()
),


br(),br(),

div(class = 'd-flex flex-row gap-3',
  div(class = "bg-light-subtle p-2 m-2 theme-green w-50 rounded-4",
        div(class = "grid-item-content",
            div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Average Cambridge Multimorbidity Score"),
            echarts4rOutput("cmms_chart", height = "250px")
        )
    ),
    div(class = "bg-light-subtle p-2 m-2 theme-green w-50 rounded-4",
        div(class = "grid-item-content",
            div(class = "p-2 fw-bold fs-5  pt-2 ps-2", "Raw Multimorbidity Count"),
            echarts4rOutput("multimorbidity_chart", height = "250px")
        )
    )
),

br(),
br()
),

div( id = 'outputs-right', class= 'w-25 h-75 p-5 position-sticky',style = 'top:7vh;',
     sticky_side_bar(),
     
     # div(class = 'alert alert-success', h6('DALYS'), h4('456,454')  ),
     # div(class = 'alert alert-danger',  h6('DALYS'), h4('456,454')  ),
     # div(class = 'alert alert-warning',  h6('DALYS'), h4('456,454') ),
     # div(class = 'alert alert-primary',  h6('DALYS'), h4('456,454') ),
     # div(class = 'alert alert-secondary', h6('DALYS'), h4('456,454')  ),
     # div(class = 'alert alert-info',  h6('DALYS'), h4('456,454')  ),
     # div(class = 'alert alert-light',  h6('DALYS'), h4('456,454') ),
     # div(class = 'alert alert-dark',  h6('DALYS'), h4('456,454') ),
     # div(class = 'alert bg-info',  h6('DALYS'), h4('456,454') ),
     # div(class = 'alert bg-primary',  h6('DALYS'), h4('456,454') ),
     # circular_value('45,324')
     
     )

),


  tags$div(
    class = " py-3", #container
    h4("Morbidity Focus"),
    selectizeInput(
      inputId = "disease",
      label = "Disease",
      choices = character(0),
      selected = NULL,
      options = list(
        valueField = "email",
        labelField = "name",
        render = I("{
    item: function(item, escape) {
      console.log(item);
      var name = item.email ? '<span class=\"name\">' + item.email + '</span>' : '';
      return '<div class =  m-2 p-2>' + '<span class=\"email m-2\">' + item.name + '</span></div>';
    },
    option: function(item, escape) {
      var label = item.name || item.email;
      var caption = item.name ? item.email : null;
      return '<div class =  \"m-2 p-2 rounded-3 \">' +
       (caption ? '<div class=\"label position-relative\">' + item.email + '</div>' : '') 
        
      '</div>';
    }
  }") 
      )),
    icon('globe',class='visually-hidden')#,
    # tags$div(class = "mt-2 mb-2", textOutput("selected_disease"))
  ),

  # Selected Morbidity Prevalence trend charts
div(class = 'd-flex flex-row align-items-start',
                                    div(class = "grid-item--graph p-2 m-2 theme-green",
                                        div(class = "grid-item-content",
                                            div(class = "fw-bold fs-4 p-2",
                                                textOutput("selected_disease1"), 'Prevalence'
                                                #"Selected Prevalence"
                                                ),
                                            echarts4rOutput("selected_prevalence_chart", height = "250px")
                                        )
                                    ),

div(class = "grid-item--graph p-2 m-2 theme-green", #grid-item 
    div(class = "grid-item-content",
        div(class = " p-2", #card-header
            # textOutput("selected_disease")
            div(class = "fw-bold fs-4 p-2",
                'Incidence'
            )
            #"Selected Prevalence"
        ),
        echarts4rOutput("selected_incidence_chart", height = "250px")
    )
)
),
  





# div(
#   h3('Prevalence'),
#
#   # Prevalence trend charts
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Diabetes Prevalence"),
#                                             echarts4rOutput("diabetes_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "COPD Prevalence"),
#                                             echarts4rOutput("copd_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Asthma Prevalence"),
#                                             echarts4rOutput("asthma_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     
#                                     

#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Non-Diabetic Hyperglycaemia Prevalence"),
#                                             echarts4rOutput("non_diabetic_hyperglycaemia_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Osteoporosis Prevalence"),
#                                             echarts4rOutput("osteoporosis_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Osteoarthritis Prevalence"),
#                                             echarts4rOutput("osteoarthritis_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#   div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#       div(class = "grid-item-content",
#           div(class = "card-header", "Rheumatoid Arthritis Prevalence"),
#           echarts4rOutput("rheumatoid_arthritis_prevalence_chart", height = "250px")
#       )
#   ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Cancer Prevalence"),
#                                             echarts4rOutput("cancer_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#   
#                                     # div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                     #     div(class = "grid-item-content",
#                                     #         div(class = "card-header", "Epilepsy Prevalence"),
#                                     #         echarts4rOutput("epilepsy_prevalence_chart", height = "250px")
#                                     #     )
#                                     # ),
#                                     # div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                     #     div(class = "grid-item-content",
#                                     #         div(class = "card-header", "Hypothyroidism Prevalence"),
#                                     #         echarts4rOutput("hypothyroidism_prevalence_chart", height = "250px")
#                                     #     )
#                                     # ),
#   
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Colorectal Cancer Prevalence"),
#                                             echarts4rOutput("colorectal_cancer_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Lung Cancer Prevalence"),
#                                             echarts4rOutput("lung_cancer_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Dementia Prevalence"),
#                                             echarts4rOutput("dementia_prevalence_chart", height = "250px")
#                                         )
#                                     ),
#                                     div(class = "grid-item grid-item--graph p-2 m-2 theme-green",
#                                         div(class = "grid-item-content",
#                                             div(class = "card-header", "Heart Failure Prevalence"),
#                                             echarts4rOutput("heart_failure_prevalence_chart", height = "250px")
#                                         )
#                                     )
#   ),

                                    br(),
                                    br()

                                  )
                              )
                          ) # End of tab-content-container
                          
                      ), # Close content-area
                      
                      # Initialize Packery with Click and Expand JavaScript
                      tags$script(HTML("
$(document).ready(function() {
  // Initialize Packery when the page loads
  setTimeout(function() {
  // var $grid = $('.grid').packery({
  //   itemSelector: '.grid-item',
  //   columnWidth: 80,
  //   gutter:10
  // });

// Handle click events on grid items
//    $grid.on('doubleclick', '.grid-item-content', function(event) {
//      event.preventDefault();
//      event.stopPropagation();
//      
//      var itemElem = event.currentTarget.parentNode;
//      var $item = $(itemElem);
//      var isExpanded = $item.hasClass('is-expanded');
//      
//      // Toggle the expanded class
//      $item.toggleClass('is-expanded');
//      
//      // Force a layout update after the CSS transition
//      setTimeout(function() {
//        if (isExpanded) {
//          // If contracting, use shiftLayout to compact everything
//          $grid.packery();
//        } else {
//          // If expanding, first layout normally, then fit the expanded item
//          $grid.packery('shiftLayout');
//          setTimeout(function() {
//            $grid.packery('shiftLayout', itemElem);
//          }, 50);
//        }
//      }, 50); // Small delay to let CSS transition start
//      // Also trigger layout after CSS transition completes
//      setTimeout(function() {
//        $grid.packery();
//      }, 150); // Match CSS transition duration (0.4s + buffer)
//   });

 //   $('.chart-card').on('click', function() {
 //     console.log('resize');
 //     // Trigger ECharts resize
 //     setTimeout(function() {
 //       $('.echarts4r').each(function() {
 //         //if (this.echartsInstance) {
 //         //console.log('resizeIn');
 //         //console.log(this);
 //         echarts.getInstanceByDom(this).resize();
 //           //this.echartsInstance.resize();
 //         //}
 //       });
 //     }, 0);
 //   });
    
    
        $(' .nav-item').on('click', function() {
      console.log('nav click resize');
      // Trigger ECharts resize
      setTimeout(function() {
        $('.echarts4r').each(function() {

          // Safely resize only initialized ECharts instances
          var inst = echarts.getInstanceByDom(this);
          if (inst) {
            inst.resize();
          }
        });
      }, 100);
    });
    
    
          $(' .nav-card-icon').on('click', function() {
      console.log('nav click resize');
      // Trigger ECharts resize
      setTimeout(function() {
        $('.echarts4r').each(function() {

          // Safely resize only initialized ECharts instances
          var inst = echarts.getInstanceByDom(this);
          if (inst) {
            inst.resize();
          }
        });
      }, 100);
    });
    
    // Tab Navigation Functionality
    $('.nav-item').on('click', function() {
      // Remove active class from all nav items
      $('.nav-item').removeClass('active');
      // Add active class to clicked item
      $(this).addClass('active');
      
      // Hide all tab content
      $('.tab-pane').removeClass('active')
      
      // Show corresponding tab content based on nav item text
      var navText = $(this).text().trim().toLowerCase();
      var tabId = '';
      
      switch(navText) {
        case 'dashboard':
          tabId = 'dashboard-tab';
          break;
        case 'analytics':
          tabId = 'analytics-tab';
          break;
        case 'reports':
          tabId = 'reports-tab';
          break;
        case 'geography':
          tabId = 'geography-tab';
          break;
        case 'deprivation':
          tabId = 'deprivation-tab';
          break;
        case 'population':
          tabId = 'population-tab';
          break;
        case 'intervention':
          tabId = 'intervention-tab';
          break;
        case 'specify':
          tabId = 'specify-tab';
          break;
        case 'scenarios':
          tabId = 'scenarios-tab';
          break;
        case 'obesity risk':
          tabId = 'ObesityRisk-tab';
          break;
        case 'inequalities':
          tabId = 'Inequalities-tab';
          break;
        case 'nhsct':
          tabId = 'NorthernTrust-tab';
          break;
            case 'int (neighhbourhood)':
          tabId = 'int-tab';
          break;
        case 'lifestyle':
          tabId = 'lifestyle-tab';
          break;
         case 'society':
          tabId = 'society-tab';
          break;
        default:
          tabId = 'dashboard-tab';
      }
      
      $('#' + tabId).addClass('active')
      
      // Re-initialize packery if dashboard tab is shown
      // if (tabId === 'dashboard-tab') {
      //   setTimeout(function() {
      //     $('.grid').packery();
      //   }, 100);
      // }
      
      // Invalidate Leaflet map size when geography tab is shown
      
      //if (tabId === 'analytics-tab') {
        setTimeout(function() {
          var map = $('#mymap').data('leaflet-map');
          //if (map) {
            map.invalidateSize();
          //}
        }, 100);
      //}
      
      // Initialize pivot module if reports tab is shown
      if (tabId === 'reports-tab') {
        setTimeout(function() {
          // Trigger pivot module column update
          if (window.Shiny && window.Shiny.setInputValue) {
            window.Shiny.setInputValue('pivot_reports_tab_shown', Math.random());
          }
          
          // Re-trigger any drag-drop initialization if needed
          if (typeof window.setupDropzone === 'function') {
            console.log('Re-initializing pivot drag-drop zones');
            window.setupDropzone('#column-pool');
            window.setupDropzoneCat('#groups-drop');
            window.setupDropzoneCat('#wide-by-drop');
            window.setupDropzone('#values-drop');
            window.setupDropzoneValueFunc('#value-func-drop');
          }
        }, 200);
      }
    });
    
    
     setTimeout(function() {
      $('#main-tab-area .tab-pane').removeClass('active')
      $('#' + 'specify-tab').addClass('active show')
     }, 1000);
     
    // setTimeout(function() {
    //     $('.grid').packery();
    //   }, 100);
     
  }, 500); // Small delay to ensure DOM is ready
   
  // Handle window resize for Leaflet map
    var resizeTimeout;
  window.addEventListener('resize', function() {
    clearTimeout(resizeTimeout);
    resizeTimeout = setTimeout(function() {
      var map = $('#mymap').data('leaflet-map');
      if (map) {
        map.invalidateSize();
      }
    }, 250);
  });
});
"
                      ))), # tab pane
                  HTML('<!-- Footer -->
                                 <footer class="dashboard-footer">
                                 <div class="footer-content">
                                 <div class="footer-info">
                                 <p>&copy;  Obesity - Data runs of the Population health Models microsimulation, data are estimates </p>
                                 <p>Data is updated periodically when underlying datasets are released from their respective providers </p>
                                 </div>
                                 <div class="footer-links">
                                 <p> Public Health Agency - Population Health Model 2025  </p>
                                 <!--<a href="#methodology">Methodology</a>
                                 <a href="#privacy">Privacy Policy</a>
                                 <a href="#contact">Contact</a> -->
                                 </div>
                                 </div>
                                </footer>
                                 <style>
                                 .dashboard-footer {
                                 margin-inline:-12px;
                                 width:100vw;
                                   position:relative;
                                   display:fixed;
                                   bottom: 0;
                                   background: #f8f9fa;
                                   padding: 10px 15px;
                                   border-top: 1px solid #e9ecef;
                                   margin-top: 20px;
                                 }
                                 
                                 .footer-content {
                                   display: flex;
                                   justify-content: space-between;
                                   align-items: center;
                                   flex-wrap: wrap;
                                 }
                                 .footer-info p {
                                   margin: 0;
                                   font-size: 0.9em;
                                   color: #6c757d;
                                 }
                                 .footer-links a {
                                   margin-left: 15px;
                                   font-size: 0.9em;
                                   color: #007bff;
                                   text-decoration: none;
                                 }
                                 .footer-links a:hover {
                                   text-decoration: underline;
                                 }
                                 @media (max-width: 600px) {
                                   .footer-content {
                                     flex-direction: column;
                                     align-items: flex-start;
                                   }
                                   .footer-links {
                                     margin-top: 10px;
                                   }
                                   .footer-links a {
                                     margin-left: 0;
                                     margin-right: 15px;
                                   }
                                 }
                                 </style>')
)

################################################
# SERVER
################################################
server <- function(input, output, session) {

  # specify intervention server code ----

  observeEvent(input$go, {
    output$status <- renderText("Loader triggered from server")

    session$sendCustomMessage(
      "loader_start",
      list(
        wrapId = "loaderWrap",
        barId  = "loaderBar",
        totalMs  = 10000,   # fixed duration
        tickMs   = 50,      # smoothness
        lingerMs = 1000,    # linger after completion then hide
        stages = list(
          list(t = 0,    cls = "bg-success"),
          list(t = 3000, cls = "bg-info"),
          list(t = 7000, cls = "bg-warning"),
          list(t = 9000, cls = "bg-danger")
        )
      )
    )
  })

  # observeEvent(input$apply,{
  #   print(input$cost_input_flat)
  #   print(input$cost_input_per_person)
  #   print(input$cost_input_per_year)
  #   print(input$cost_input_per_person_per_year)
  # })

  pop_reactive <- reactive({
    
    past_populations <- read.fst('./3_pre_main/intermediate_populations/full_history_past_populations.fst')
    past_populations <- past_populations %>% mutate(age10 = replace_values(age10,
                                                                           from = '75+',
                                                                           to = '75-110')) %>% 
      mutate(age10 = case_when(age==0~'0-15',
                               age==1~'0-15',
                               T ~ age10))
    past_populations %>%
      filter(year==min(year),run==1)
                           })

  ## Intervention People Select Server ----
  api <- interventionSelectorServer("htn_int", data = pop_reactive)

  targeted_pop <- reactive(
    {
    reached <- api$data () %>% filter(intervention_reached == T)
    targets <- api$data () %>% filter(intervention_target == T)

    # print(count(api$data(),intervention_reached))
    # print(count(api$data(),intervention_target))
    # print(count(reached,intervention_reached))
    # print(count(targets,intervention_target))

    # print(  past_populations %>%
    #           mutate(intervention_target = ifelse(id %in% targets$id,T,F)) %>%
    #           mutate(intervention_reached = ifelse(id %in% reached$id,T,F)) %>%
    #           summarise(sum(intervention_target)/n()*100,
    #                     sum(intervention_reached)*100/n() )
    #                     )
    
    past_populations <- read.fst('./3_pre_main/intermediate_populations/full_history_past_populations.fst')
    
    past_populations <- past_populations %>% mutate(age10 = replace_values(age10,
                                                                           from = '75+',
                                                                           to = '75-110')) %>% 
      mutate(age10 = case_when(age==0~'0-15',
                               age==1~'0-15',
                               T ~ age10))
    
    past_populations %>%
      mutate(intervention_target = ifelse(id %in% targets$id,T,F)) %>%
      mutate(intervention_reached = ifelse(id %in% reached$id,T,F))
    }
  )

  # observeEvent(targeted_pop(), {
  #   # print('printing....')
  #   # print(class(api$data))
  #   # print(typeof(api$data))
  #   # print(class(api$data()))
  #   # print(typeof(api$data()))
  #   # print(targeted_pop()$intervention_target)
  #   print(count(targeted_pop(),intervention_target))
  #   print(count(targeted_pop(),intervention_reached))
  #   print(count(targeted_pop(),run))
  #   print(count(targeted_pop(),year))
  # 
  # }
  # )

  # observeEvent(api$spec(), {
  #   print('printing123')
  #   print(class(api$spec))
  #   print(typeof(api$spec))
  #   print(class(api$spec()))
  #   print(typeof(api$spec()))
  #   print(api$spec())
  # })

  # observeEvent(api$modified_attributes(), {
  #   print(api$modified_attributes())
  # })
  # observeEvent(api$spec(), {
  #   print(api$spec())
  # })
  # observeEvent(api$risk_spec(), {
  #   print(api$risk_spec())
  # })
  # observeEvent(api$reset_all(), {
  #   print(api$reset_all())
  # })
  
  # spec         <- api$spec
  # modified     <- api$modified_attributes
  # risk_spec    <- api$risk_spec
  # reset_all    <- api$reset_all

  # Cost input reactive values
  cost_values <- reactiveValues()

  cost_values$flat_fixed_cost = 0
  cost_values$per_person_per_year = 0
  cost_values$per_person = 0
  cost_values$per_year = 0

  observeEvent(input$apply_costs, {

    req(!is.null(input$draggable_data))
    req(!is.null(targeted_pop()) & nrow(targeted_pop()) != 0)

    ts_targeted <- targeted_pop() %>%
      #simulation_state$results() %>%
      filter(intervention_reached) %>%
      count(year,run,intervention) %>%
      group_by(year,intervention) %>%
      summarise(n=mean(n)) %>%
      ungroup()

    # print(ts_targeted)
    
    persons <- ts_targeted %>%
      filter(n==first(n)) %>%
      pull(n)

    # person_years <- ts_targeted %>%
    #   pull(n) %>%
    #   sum(n)

    years <- input$draggable_data[2] %>%
      unlist() %>%
      matrix(ncol=2,byrow = T) %>%
      as.data.frame() %>%
      setNames(c("year","intervention")) %>%
      filter( abs(intervention-1)>0.05 ) %>%
      nrow()

    person_years <- persons * years

    # print(years)

    # print(input$draggable_data)
    # print(input$draggable_data[2])

    print(input$draggable_data[2] %>%
            unlist() %>%
            matrix(ncol=2,byrow = T) %>%
            as.data.frame() %>%
            setNames(c("year","intervention"))
          )

    cost_values$flat_fixed_cost = input$cost_input_flat

    cost_values$per_person_per_year = person_years *
      model_specification$population$scale_down_factor *
      input$cost_input_per_person_per_year

    cost_values$per_person = persons *
      model_specification$population$scale_down_factor *
      input$cost_input_per_person

    cost_values$per_year = years * input$cost_input_per_year

    # req(input$input1)
    # cost_values$flat_fixed_cost <- input$input1

    message("Flat Fixed Cost updated: £")
  })

  # observeEvent(input$apply_costs,{
  #   print('cost values:')
  #   print(cost_values$flat_fixed_cost)
  #   print(cost_values$per_person_per_year)
  #   print(cost_values$per_person)
  #   print(cost_values$per_year)
  # 
  # })

  # observeEvent(input$apply_costs,{
  #   print('total cost:')
  #   print(
  #     sum(
  #       c(cost_values$flat_fixed_cost,
  #         cost_values$per_person_per_year,
  #         cost_values$per_person,
  #         cost_values$per_year)
  #     )
  #   )
  # 
  # })

  # observeEvent(input$apply_costs,ignoreNULL = T,ignoreInit = T,{
  #   print('total_cost_time_series:')
  #   # print(total_cost_time_series())
  # 
  # })

  total_cost_time_series <- reactiveVal({NULL})

  observeEvent(input$apply_costs,ignoreNULL = T,ignoreInit = T,{

    print('-------')
    # print(simulation_state$results())
    print(nrow(simulation_state$results()))
    print(nrow(print(nrow(output_df()))))

    print('-------')
    print('-------')
    print( targeted_pop()[1,1])
    print('-------')
    print('-------')
    print( pop_reactive()[1,1])
    print('-------')

    years <- ifelse(is.null(input$draggable_data),
                    0,
                    input$draggable_data[2] %>%
                      unlist() %>%
                      matrix(ncol=2,byrow = T) %>%
                      as.data.frame() %>%
                      setNames(c("year","intervention")) %>%
                      filter( abs(intervention-1)>0.05 ) %>%
                      nrow()
    )

    # if(nrow(simulation_state$results())==0){
    #   temp_past_populations <- targeted_pop()
    # }else{
    #   temp_past_populations <- simulation_state$results()
    # }
    
    temp_past_populations <- targeted_pop()
    
    setDT(temp_past_populations)

    print(nrow(temp_past_populations))

    total_people <- temp_past_populations[
      ,
      .(N=sum(intervention_reached)) , by = .(run, year)
    ][, .(total_people = mean(N)), by = .( year)
    ]  #[,.(tp=mean(total_people))]¢[['tp']]

    # print(total_people)
    total_people[year == min(year)+1,'fixed_cost':= input$cost_input_flat]
    total_people[year != min(year),'annual_cost':= input$cost_input_per_year ]
    total_people[year != min(year),'ongoing_patient_cost':= input$cost_input_per_person_per_year * total_people * model_specification$population$scale_down_factor ]
    total_people[year == min(year)+1,'patient_cost':= input$cost_input_per_person * total_people  * model_specification$population$scale_down_factor]

    # input$cost_input_flat
    # input$cost_input_per_person_per_year
    # input$cost_input_per_person
    # input$cost_input_per_year

    total_people[is.na(fixed_cost), fixed_cost:=0]
    total_people[is.na(annual_cost), annual_cost:=0]
    total_people[is.na(ongoing_patient_cost), ongoing_patient_cost:=0]
    total_people[is.na(patient_cost), patient_cost:=0]

    total_people[,total_cost := fixed_cost + annual_cost + ongoing_patient_cost + patient_cost]

    total_people[,cumulative_total_cost := cumsum(total_cost)]

    total_people[,year := as.character(year)]

    # print(total_people)

    total_cost_time_series(total_people)

  })

  qalys <- reactiveVal({NULL})
  cost_per_qaly <- reactiveVal({NULL})
  icer <- reactiveVal({NULL})
  costs <- reactiveVal({NULL})

  observe({
    # req(!is.null(simulation_state$results()) & nrow(simulation_state$results())>0)
    req(!is.null(output_df()) & nrow(output_df())>0)

    # x  <- calculate_costs_fn(as.data.table(simulation_state$results()))
    x  <- calculate_costs_fn(as.data.table(output_df()))

    costs(x)
  })

  observe({
    # req(!is.null(simulation_state$results()) & nrow(simulation_state$results())>0)
    req(!is.null(output_df()) & nrow(output_df())>0)
    # print(output_df())
    args <- list( past_populations = as.data.table(output_df()), year_cut_off = NULL)
    x <- do.call(qaly_yld_fn, args)
    x[disease == "combined_uw", .(total = sum(total_uw))]

    qalys(x)

  })

  observe({
    req(qalys())
    req(costs())

    # print('cost_per_qaly')
    # print( names( qalys() ) )
    # print( class( qalys() ) )
    # print( costs()[,.(total_cost = sum(total_cost)), by = .(intervention,year)
    # ][,year := as.character(year)
    # ])

    qaly_combined <- as.data.table(qalys())[disease == 'combined_uw', ]
    if (nrow(qaly_combined) == 0) {
      cost_per_qaly(NULL)
      return()
    }

    x <- qaly_combined %>%
      dcast(formula = year  ~ intervention, value.var = 'total_uw', fill = 0L) %>%
      mutate(year = as.character(year)) %>%
      mutate(
        intervention = if ('intervention' %in% names(.)) intervention else 0,
        `non-intervention` = if ('non-intervention' %in% names(.)) `non-intervention` else 0
      ) %>%
      mutate(averted = `non-intervention` - intervention) %>%
      mutate(cumulative_averted = cumsum(averted))
    # print(x)

    cost_summary <- as.data.table(costs())[, .(total_cost = sum(total_cost)), by = .(intervention, year)]
    if (nrow(cost_summary) == 0) {
      cost_per_qaly(NULL)
      return()
    }

    x <- x %>%
      left_join(
        cost_summary[, year := as.character(year)
        ] %>%
          dcast(formula = year  ~intervention, value.var = 'total_cost', fill = 0L) %>%
          mutate(
            intervention = if ('intervention' %in% names(.)) intervention else 0,
            `non-intervention` = if ('non-intervention' %in% names(.)) `non-intervention` else 0
          ) %>%
          mutate(savings = `non-intervention` - intervention) %>%
          mutate(cumulative_savings = cumsum(savings)),
        by='year'
      ) %>%
      mutate(cost_per_qaly_gained = cumulative_savings / cumulative_averted
      )

    cost_per_qaly(x)
  })

  observe({
    print('icer updated')
    req(cost_per_qaly())
    req(total_cost_time_series())

    cost_per_qaly_df <- as.data.frame(cost_per_qaly())
    intervention_cost_df <- as.data.frame(total_cost_time_series())

    if (nrow(cost_per_qaly_df) == 0 || nrow(intervention_cost_df) == 0) {
      icer(NULL)
      return()
    }

    x <- cost_per_qaly_df %>%
      left_join(intervention_cost_df, by = 'year') %>%
      mutate(cumulative_total_cost = replace_na(cumulative_total_cost, 0)) %>%
      mutate(cost_per_qaly_gained = (cumulative_savings-cumulative_total_cost) / cumulative_averted) %>%
      mutate(cumulative_monetised_qalys = cumulative_averted * 20000) %>%
      mutate(cost_per_qaly_gained_monetised = (cumulative_savings+cumulative_monetised_qalys-cumulative_total_cost) / cumulative_averted) %>%

      mutate(cumulative_net_money = cumulative_savings-cumulative_total_cost) %>%
      mutate(roi = cumulative_net_money/cumulative_total_cost)

    icer(x)
  })

  # observe({
  #   print('icer')
  #   print(icer())
  #   print('cost_per_qaly_')
  #   print(cost_per_qaly())
  #   print('qalys')
  #   print(qalys())
  #   print('costs')
  #   print(costs())
  #   print('total_cost_time_series')
  #   print(total_cost_time_series())
  # })

## ICER charts 
  
  
  output$icer_costs_chart <- renderEcharts4r({
    req(icer())
    # validate(
    #   need(
    #     is.data.frame(icer()) && nrow(icer()) > 0,
    #     "Specify target population and intervention shape and time to calculate cost parameters"
    #   ))
    
    print('icer chart')
    icer() %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      e_line(cumulative_total_cost, name = "Cumulative Total Cost") %>%
      e_line(cumulative_savings, name = "Cumulative Savings") %>%
      e_line(cumulative_net_money, name = "Cumulative Net Money") %>%
      e_line(cumulative_monetised_qalys, name = "Monetised QALYs") %>%
      e_tooltip(
        trigger = "axis",
        formatter = e_tooltip_pointer_formatter(
          style = "currency",
          digits = 2,
          locale = NULL,
          currency = "GBP"
        )
      ) %>%
      e_theme('walden') %>%
      e_format_y_axis(suffix = "£") %>%
      e_grid(containLabel = T)
  })

  output$icer_roi_chart <- renderEcharts4r({
    req(icer())
    # validate(
    #   need(
    #     nrow(icer()) > 0,
    #     "Specify target population and intervetniton shape and time to calculate cost parameters"
    #   )
    # )
    icer() %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      e_line(roi, name = "ROI") %>%
      e_format_y_axis(suffix = "x") %>%
      e_tooltip()%>%
      e_theme('walden') %>%
      e_grid(containLabel = T)
  })

  output$icer_qaly_chart <- renderEcharts4r({
    req(icer())
    # validate(
    #   need(
    #     nrow(icer()) > 100,
    #     "Specify target population and intervetniton shape and time to calculate cost parameters"
    #   )
    # )

    icer() %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      e_line(cost_per_qaly_gained, name = "Cost per QALY gained") %>%
      e_line(cumulative_averted, name = "Cumulative QALYs", y_index = 1) %>%
      e_line(cost_per_qaly_gained_monetised, name = "ICER") %>%
      e_format_y_axis(suffix = "£/QALY") %>%
      e_y_axis(name = "QALYs",y_index = 1 ) %>%
      e_tooltip() %>%
      e_grid(containLabel = T) %>%
      e_theme('walden')
  })

  # Output displays for cost values
  output$cost_flat_fixed <- renderText({
    paste0("£", format(cost_values$flat_fixed_cost, big.mark = ","), " ")
  })

  output$cost_per_person_per_year <- renderText({
    paste0("£", format(cost_values$per_person_per_year, big.mark = ","), " ")
  })

  output$cost_per_person <- renderText({
    paste0("£", format(cost_values$per_person, big.mark = ","), " ")
  })

  output$cost_per_year <- renderText({
    paste0("£", format(cost_values$per_year, big.mark = ","), " ")
  })

  # Calculated total cost output (example)
  output$total_intervention_cost <- renderText({
    total <- cost_values$flat_fixed_cost +
      (cost_values$per_person_per_year # * model_specification$population$scale_down_factor
       ) + # Assuming population size
      (cost_values$per_person #* model_specification$population$scale_down_factor
         ) +
      cost_values$per_year
    paste0("Total Estimated Cost: £", format(total, big.mark = ","))
  })

  output$multimorbidity_chart <- renderEcharts4r({
    output_df() %>%
      select(-c(1,multimorbidity,275)) |>
      add_multimorbidity_fn() |>
      group_by(year,run,intervention) %>%
      summarise(multimorbidity = mean(multimorbidity,na.rm=T)) %>%
      group_by(year,intervention) %>%
      summarise(multimorbidity = mean(multimorbidity,na.rm=T)) %>%
      mutate(year = as.character(year)) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      e_line(multimorbidity) %>%
      e_tooltip(backgroundColor='white') %>%
      e_grid(containLabel = T) %>%
      e_theme('shine')
  })

  output$cmms_chart <- renderEcharts4r({
    output_df() %>%
      select(-cmms) |> 
      compute_cmms() %>%
      group_by(year,run,intervention) %>%
      summarise(cmms = mean(cmms,na.rm=T)) %>%
      group_by(year,intervention) %>%
      summarise(cmms = mean(cmms,na.rm=T)) %>%
      mutate(year = as.character(year)) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      e_line(cmms) %>%
      e_tooltip(backgroundColor = 'white') %>%
      e_grid(containLabel = T) %>%
      e_theme('shine')
  })

  sick_days_df <- reactive({
    
    # sick_days_fn( as.data.table(output_df()))
    lost_productivity_estimator(output_df())
    
    })

  bed_days_df <- reactive({
    
    # bed_days_fn(as.data.table(output_df()))
    bed_days_estimator(output_df(),stats2223_agg)
    
    })

  output$hospital_admissions_chart <- renderEcharts4r({
    # print(bed_days_df())
    # print(class(bed_days_df()))

    bed_days_df() %>%
      group_by(year,intervention) %>%
      summarise(
        emergency_admissions = sum(emergency_admissions, na.rm =T),
        admissions = sum(admissions, na.rm =T),
        bed_days = sum(bed_days, na.rm =T)
      ) %>%
      mutate(year = as.character(year)) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      # e_line(emergency_admissions, name = "Emergency Admissions") %>%
      # e_line(admissions, name = "Admissions ", y_index = 1) %>%
      e_line(bed_days) %>% #, name = "Bed days"
      e_grid(top = '0%') |> 
      
      e_tooltip() %>%
      e_grid(containLabel = T) %>%
      e_theme('westeros')
  })
  
  
  output$hospital_admissions_chart1 <- renderEcharts4r({
    # print(bed_days_df())
    # print(class(bed_days_df()))
    
    bed_days_df() %>%
      group_by(year,intervention) %>%
      summarise(
        emergency_admissions = sum(emergency_admissions, na.rm =T),
        admissions = sum(admissions, na.rm =T),
        bed_days = sum(bed_days, na.rm =T)
      ) %>%
      mutate(year = as.character(year)) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      e_line(emergency_admissions) %>% #, name = "Emergency Admissions"
      # e_line(admissions, name = "Admissions ", y_index = 1) %>%
      # e_line(bed_days, name = "Bed days") %>%
      e_grid(top = '0%') |> 
      
      e_tooltip() %>%
      e_grid(containLabel = T) %>%
      e_theme('westeros')
  })
  
  output$hospital_admissions_chart2 <- renderEcharts4r({
    # print(bed_days_df())
    # print(class(bed_days_df()))
    
    bed_days_df() %>%
      group_by(year,intervention) %>%
      summarise(
        emergency_admissions = sum(emergency_admissions, na.rm =T),
        admissions = sum(admissions, na.rm =T),
        bed_days = sum(bed_days, na.rm =T)
      ) %>%
      mutate(year = as.character(year)) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      # e_line(emergency_admissions, name = "Emergency Admissions") %>%
      e_line(admissions) %>% # y_index = 1, name = "Admissions ",
      # e_line(bed_days, name = "Bed days") %>%
      e_grid(top = '0%') |> 
      
      e_tooltip() %>%
      e_grid(containLabel = T) %>%
      e_theme('westeros')
  })

  output$illness_chart <- renderEcharts4r({

    # print(sick_days_df())

    sick_days_df() %>%
      group_by(year,intervention) %>%
      summarise(
        days_lost = sum(days_lost, na.rm =T),
        cost = sum(cost, na.rm =T),
        sick_spells = sum(sick_spells, na.rm =T)
        ) %>%
      mutate(year = as.character(year)) %>%
      group_by(year) |> 
      mutate(averted = c(last(sick_spells) - first(sick_spells),NA )) |> 
      group_by(intervention) %>%
      e_charts(year, name = 'fddd', emphasis = list(focus = "series")) %>%
      # e_line(days_lost, name = "Days lost") %>%
      # e_line(cost, name = "cost", y_index = 1) %>%
      e_line(sick_spells) %>% #, name = "Sick spells"
      e_format_y_axis(y_index = 1, suffix = "spells") %>%
      e_bar(name = 'averted', averted) |> 
      # e_format_y_axis(index = 2, suffix = "Count") %>%
      e_grid(top = '0%') |> 
      e_tooltip() %>%
      e_grid(containLabel = T) %>%
      e_theme('westeros')
  })
  
  output$illness_chart1 <- renderEcharts4r({
    
    # print(sick_days_df())
    
    sick_days_df() %>%
      group_by(year,intervention) %>%
      summarise(
        days_lost = sum(days_lost, na.rm =T),
        cost = sum(cost, na.rm =T),
        sick_spells = sum(sick_spells, na.rm =T)
      ) %>%
      mutate(year = as.character(year)) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      # e_line(days_lost, name = "Days lost") %>%
      e_line(cost) %>% # name = "cost",
      # e_line(sick_spells, name = "Sick spells") %>%
      e_format_y_axis(y_index = 1, suffix = "£") %>%
      # e_format_y_axis(index = 2, suffix = "Count") %>%
      e_grid(top = '0%') |> 
      
      e_tooltip(trigger = 'axis') %>%
      e_grid(containLabel = T) %>%
      e_theme('westeros')
  })
  
  output$illness_chart2 <- renderEcharts4r({
      

    sick_days_df() %>%
      group_by(year,intervention) %>%
      summarise(
        days_lost = sum(days_lost, na.rm =T),
        cost = sum(cost, na.rm =T),
        sick_spells = sum(sick_spells, na.rm =T)
      ) %>%
      mutate(year = as.character(year)) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = "series")) %>%
      e_line(days_lost) %>% #, name = "Days lost"
      # e_line(cost, name = "cost", y_index = 1) %>%
      # e_line(sick_spells, name = "Sick spells") %>%
      e_format_y_axis(y_index = 1, suffix = "Days") %>%
      # e_format_y_axis(index = 2, suffix = "Count") %>%
      e_grid(top = '0%') |> 
      
      e_tooltip() %>%
      e_grid(containLabel = T) %>%
      e_theme('westeros')
  })

  daly_df <-  reactive({
  yld <- daly_yld_fn(as.data.table(output_df()))
  yll <- calculate_daly_yll(as.data.table(output_df()))

  yld <- yld[disease == 'combined_dw',]
  yll <- yll[death_reason != 'death_reason', .(yll = sum(yll)), by = .(intervention,year)]

  DALYS <- yld[yll, on = c('intervention','year'),
               `:=` (yll = i.yll)]

  DALYS[, daly := total_dw + yll]

  # DALYS_mdm[is.na(yll), yll := 0]
  temp_past_populations <- output_df()
  setDT(temp_past_populations)

  popp <- temp_past_populations[,.N,by = c('intervention','year','run')
  ][, .(N = mean(N)), by = .(intervention,year)
  ]

  DALYS[popp,on = c('intervention','year')
  ][,year := as.character(year)] #%>%
    # mutate(daly=daly/N) %>%
    # group_by(intervention) %>%
    # e_charts(year) %>%
    # e_line(daly) %>%
    # e_tooltip() |> 
    # e_title(subtext = 'DALYs per Capita')
  })
  
     output$daly_chart <- renderEcharts4r({
       daly_df() %>%
         mutate(daly=daly/N) %>%
         group_by(intervention) %>%
         e_charts(year, emphasis = list(focus = "series")) %>%
         e_line(daly) %>%
         e_tooltip(trigger = 'axis') |> 
         e_title(subtext = 'DALYs per Capita')
       })
     
     
     output$daly_chart_averted <- renderEcharts4r({
       daly_df() %>%
         mutate(daly=daly/N) %>%
         group_by(year) %>%
         mutate(averted = first(daly)-last(daly)) |> 
         ungroup() |> 
         e_charts(year, emphasis = list(focus = "series")) %>%
         e_bar(averted,legend = F) %>%
         e_tooltip() |> 
         e_visual_map(type = 'piecewise',orient = 'horizontal',
                      pieces = list(
                        list(gt= 0,
                             # lte= 50,
                             color= 'rgb(200,0,0)'# '#ffcdd2'
                        ),
                        list(lte= 0,
                             # lte= 50,
                             color= 'steelblue'
                        ))
                      )|> 
         e_title(subtext = 'DALYs per Capita averted')
     })
     
     output$daly_chart_composition <- renderEcharts4r({
       
       daly_df()  |>
       filter(intervention == 'non-intervention') |> 
       mutate(year = as.character(year)) |> 
       mutate( yll = yll*model_specification$population$scale_down_factor)  |> 
       mutate(daly = yll + total_dw) |> 
       e_charts(year) |>
       e_bar(yll,name = 'Years of Life lost', stack = 'f') |>
       e_bar(total_dw,name = 'Years of Life Disabled', stack = 'f') |> 
       e_line(daly,name = 'DALYs') |> 
       e_tooltip()
     })
     
     
     output$disease_cost_chart <- renderEcharts4r({
       print(costs())
       
       as.data.table(costs())[,.(total_cost = sum(total_cost)), by = .(intervention,year)
       ][,year := as.character(year)     ] |> 
         group_by(intervention) |> 
         e_charts(year,emphasis = list(focus = 'series')) |> 
         e_line(total_cost) |> 
         e_tooltip(
           trigger = "axis",
           formatter = e_tooltip_pointer_formatter(
             style = "currency",
             digits = 2,
             locale = NULL,
             currency = "GBP"
           )
         ) |>
         e_format_y_axis(suffix = "£")
         
     })
     
     
     output$disease_cost_chart_saved <- renderEcharts4r({
       
       costs()[,.(total_cost = sum(total_cost)), by = .(intervention,year)
       ][,year := as.character(year)     ] |> 
         group_by(year) |> 
         mutate(saved = first(total_cost) - last(total_cost)) |> 
         filter(row_number()==1) |> 
         # group_by(intervention) |> 
         ungroup() |> 
         e_charts(year,emphasis = list(focus = 'series')) |> 
         e_bar(saved) |> 
         e_tooltip(
           trigger = "axis",
           backgroundColor = 'white',
           formatter = e_tooltip_pointer_formatter(
             style = "currency",
             digits = 2,
             locale = NULL,
             currency = "GBP"
           )
         ) |>
         e_format_y_axis(suffix = "£") |>
         e_theme('london')
       
       
     })
     
     output$disease_cost_breakdown <- renderEcharts4r({
       
     costs()[intervention == 'non-intervention',
     ][,year := as.character(year)     ] |> 
       group_by(disease) |> 
       e_charts(year,emphasis = list(focus = 'series')) |> 
       e_bar(total_cost, stack = 'stack',
             symbol = 'none') |> 
       e_tooltip() |> 
       e_grid(right='30%') |> 
       e_legend(right=0,orient = 'vertical',type = 'scroll') |> 
       e_tooltip()
     })
     
     
     
     daly_mdm_fn <- reactive({
       
       yld_mdm <- daly_yld_fn(as.data.table(output_df()),group_vars = 'mdm_quintile_soa_name')
       yll_mdm <- calculate_daly_yll(as.data.table(output_df()),group_vars = 'mdm_quintile_soa_name')
       
       DALYS_mdm <- yld_mdm[yll_mdm, on = c('intervention','year'),
                            `:=` (yll = i.yll)]
       
       DALYS_mdm[, daly := total_dw + yll]
       
     })
     
     
    output$daly_chart_mdm <- renderEcharts4r({
      daly_mdm_fn() %>%
        mutate(mdm_quintile_soa_name = factor( mdm_quintile_soa_name, 
                                               labels = c('Least Deprived', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Most Deprived'),
                                               levels = c('Least Deprived', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Most Deprived'),
                                               ordered = T
        )) |> 
        group_by(intervention,mdm_quintile_soa_name) |> 
        summarise(n = n(), daly = mean(daly)) |> 
        group_by(intervention) |> 
        mutate(delta = first(daly) - last(daly)) |> 
        e_charts(mdm_quintile_soa_name) |> 
        e_bar(daly) |> 
        e_tooltip()
     })
     
     output$daly_chart_mdm_averted <- renderEcharts4r({
       daly_mdm_fn()|> 
         mutate(mdm_quintile_soa_name = factor( mdm_quintile_soa_name, 
                                                labels = c('Least Deprived', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Most Deprived'),
                                                levels = c('Least Deprived', 'Quintile 2', 'Quintile 3', 'Quintile 4', 'Most Deprived'),
                                                ordered = T
         )) |> 
         group_by(intervention,mdm_quintile_soa_name) |> 
         summarise(n = n(), daly = mean(daly)) |> 
         group_by(mdm_quintile_soa_name) |> 
         summarise(averted = last(daly) - first(daly) ) |> 
         e_charts(mdm_quintile_soa_name) |> 
         e_bar(averted) |> 
         # e_line(delta) |> 
         e_tooltip()
     })
  
  #   output$daly_chart <- renderEcharts4r({
  # 
  #   print(daly_df())
  #     daly_df() %>%
  #     group_by(year,intervention) %>%
  #     summarise(
  #       days_lost = sum(days_lost, na.rm =T),
  #       cost = sum(cost, na.rm =T),
  #       sick_spells = sum(sick_spells, na.rm =T)
  #     ) %>%
  #     mutate(year = as.character(year)) %>%
  #     group_by(intervention) %>%
  #     e_charts(year) %>%
  #     e_line(days_lost, name = "days_lost") %>%
  #     e_line(cost, name = "cost", y_index = 1) %>%
  #     e_line(sick_spells, name = "sick_spells") %>%
  #     e_format_y_axis(suffix = "£/QALY") %>%
  #     e_tooltip() %>%
  #     e_grid(containLabel = T) %>%
  #     e_theme('westeros')
  # 
  # })

  morbidity_list <- c(
    'pad', 'ckd', 'vte', 'diabetes', 'rheumatoid_arthritis', 'copd', 'asthma',
    'depression', 'non_diabetic_hyperglycaemia', 'osteoporosis', 'cancer',
    'osteoarthritis', 'epilepsy', 'hypothyroidism', 'colorectal_cancer',
    'prostate_cancer', 'female_breast_cancer', 'renal_cancer',
    'oesophageal_cancer', 'stomach_cancer', 'osteogastric_cancer',
    'oral_cancer', 'pancreatic_cancer', 'uterine_cancer',
    'blood_multiple_myeloma', 'blood_lymphoma', 'blood_leukaemia',
    'blood_cancer', 'ovarian_cancer', 'lung_cancer', 'stroke',
    'dementia', 'heart_failure', 'atrial_fibrillation', 'hypertension',
    'chronic_kidney_disease'
  )

  # get_time_series <- function(df, morbidity, fun) {
  #   if (fun == "prevalence") {
  #     ts <- df[df[[morbidity]]!=0,] %>%
  #       count(run, year) %>%
  #       group_by(year) %>%
  #       summarise(n = mean(n)) %>%
  #       arrange(year)
  #   } else {
  #     ts <- df[df[[morbidity]]!=df[['year']],] %>%
  #       count(run, year) %>%
  #       group_by(year) %>%
  #       summarise(n = mean(n)) %>%
  #       arrange(year)
  #   }
  #   ts$n * model_specification$population$scale_down_factor
  # }


  # output$morbidity_summary_table <- renderReactable({
  #   df <- simulation_state$results()
  #   # df <- past_populations
  #
  #   if (is.null(df) || nrow(df) == 0) return(NULL)
  #
  #   table_data <- lapply(morbidity_list, function(morb) {
  #     prev <- get_time_series(df, morb, "prevalence")
  #     inc  <- get_time_series(df, morb, "incidence")
  #     list(
  #       Morbidity = gsub("_", " ", tools::toTitleCase(morb)),
  #       Prevalence = list(prev),
  #       Incidence = list(inc),
  #       DALY = "-",
  #       YLL = "-",
  #       YLD = "-"
  #     )
  #   })
  #
  #   table_df <- do.call(rbind, lapply(table_data, as_tibble))
  #   # table_df$Prevalence <- lapply(table_data, function(x) x$Prevalence)
  #   # table_df$Incidence <- lapply(table_data, function(x) x$Incidence)
  #
  #   reactable(
  #     table_df,
  #     columns = list(
  #       Morbidity = colDef(name = "Morbidity", minWidth = 180),
  #       Prevalence = colDef(
  #         name = "Prevalence (Sparkline)",
  #         cell = react_sparkline(
  #           height = 30,
  #           line_color = "#1e88e5",
  #           fill_color = "#bbdefb",
  #           highlight_points = TRUE
  #         ),
  #         minWidth = 120
  #       ),
  #       Incidence = colDef(
  #         name = "Incidence (Sparkline)",
  #         cell = react_sparkline(
  #           height = 30,
  #           line_color = "#e53935",
  #           fill_color = "#ffcdd2",
  #           highlight_points = TRUE
  #         ),
  #         minWidth = 120
  #       ),
  #       DALY = colDef(name = "DALYs", align = "center", minWidth = 70),
  #       YLL = colDef(name = "YLL", align = "center", minWidth = 70),
  #       YLD = colDef(name = "YLD", align = "center", minWidth = 70)
  #     ),
  #     bordered = TRUE,
  #     highlight = TRUE,
  #     striped = TRUE,
  #     resizable = TRUE,
  #     defaultPageSize = 15,
  #     theme = reactableTheme(
  #       borderColor = "#e0e0e0",
  #       stripedColor = "#f5f5f5"
  #     )
  #   )
  # })


  # This one below works !!!!
  observeEvent(input$draggable_data,
               {
                 qsave(input$draggable_data, "draggable_data.qs")
                 }
  )

## Intervention module server ----
  result <- intervention_module_server("intervention1", reactive({runButton()}))
  
  progress_pair <- progress_pair_module_server("intervention_progress")
  
  observe({
    
    # input$apply_costs
    
    selected_df <- api$data()
    
    # print(df)
    
    n_eligible <- nrow(selected_df)
    
    n_reached  <- sum(selected_df$intervention_reached, na.rm = TRUE)
    n_targeted  <- sum(selected_df$intervention_target, na.rm = TRUE)
    
    # if (n_eligible > 0) paste0(round(100 * n_reached / n_eligible, 2), "%") else "NA"
    
    progress_pair$set_width( n_reached/n_eligible*100,
                            (n_targeted-n_reached)/n_eligible*100)
  })
  
  
  observeEvent(input$reset,{
    print('reseting chart')
    result$reset_chart()
  })

## Chart update module server ----
  runButton <- reactiveVal(NULL)

  observeEvent(input$e,ignoreInit = T,ignoreNULL=T,{
    print('start1')
    runButton(input$e+1)
  })
  
  drag_data <- reactiveVal(NULL)
  
  observeEvent(input$e, ignoreInit = T, ignoreNULL=T,{
    print('start11')
    drag_data(input$draggable_data)
  })

  simulation_state <- NULL
  # observeEvent(input$e,ignoreInit = T,ignoreNULL=T,{

  simulation_state <- chartUpdateModuleServer("demo-chart",
                                              reactive({runButton()}),
                                              targeted_pop,
                                              drag_data
                                              )

  #assume targeted pop is the past population with runs, years and target flags
  output_df <- reactive({
  
    if(nrow(simulation_state$results())>0){

    message('run data packet')
      print(simulation_state$results())
      print(simulation_state$results)
      
  
    target_populations_df <- simulation_state$results() #simulation_state$past_populations()#
  
    t_past_populations <- isolate(targeted_pop()) %>%
      # filter(run <= model_specification$model$number_of_runs ) %>%
      # filter(year <= max(target_populations_df$year) &
      #          year >= min(target_populations_df$year)
      #        ) %>%
      # take out runs and year previously, but not now modelled
      filter(!intervention_reached) %>%
      # take out runs those that are targeted
      bind_rows(target_populations_df ) %>%
      # put in those that are targeted
      mutate(intervention = 'intervention')
      # mark as an intervention

    t_past_populations <- t_past_populations %>%
      select(-any_of( c('cmms', 'multimorbidity'))) %>%
      compute_cmms() %>%
      add_multimorbidity_fn()
  
    total_pop <- bind_rows(
      isolate(targeted_pop()) %>%
        # filter(run <= max(target_populations_df$run) ) %>%
        # filter(year <= max(target_populations_df$year) &
        #          year >= min(target_populations_df$year)
        # ) %>%
        mutate(intervention = 'non-intervention'),
      t_past_populations
    )
    print(nrow(target_populations_df))
    print(count(isolate(targeted_pop()),intervention_reached))
    print(nrow(t_past_populations))
    print(nrow(total_pop))
    
    # total_pop <- compute_cmms(total_pop)
    # total_pop <- add_multimorbidity_fn(total_pop)

    qsave(total_pop, 'total_pop.qs')
    total_pop #%>%
      # select(-any_of( c('cmms', 'multimorbidity'))) %>%
      # compute_cmms() %>%
      # add_multimorbidity_fn()

    }else{

      message('no run yet')
      # past_populations
      total_pop <- qread( 'total_pop.qs')
      

    }

    # target_populations_df
    # targeted_pop - intervention reached
    # t_past_populations
    # total_pop
    # 
    # 7461
    # 47311
    # 54772
    # 102083


    })


  # observe({
  #   print('output_df')
  # 
  #   qsave(output_df(), "output_df.qs")
  # 
  #   session$sendCustomMessage(
  #     "loader_start",
  #     list(
  #       wrapId = "loaderA_wrap",
  #       barId  = "loaderA_bar",
  #       totalMs  = 10000,
  #       tickMs   = 50,
  #       lingerMs = 1000,
  #       stages = list(
  #         list(t = 0,    cls = "bg-success"),
  #         list(t = 3000, cls = "bg-info"),
  #         list(t = 7000, cls = "bg-warning"),
  #         list(t = 9000, cls = "bg-danger")
  #       )
  #     )
  #   )
  # 
  # })
# 
# 
#   # observe({
#   #   print('simulation_state')
#   #   qsave(simulation_state$past_populations(), "myfile.qs")
#   # })
# 
  observe({
    #print(simulation_state$result())
    qsave(simulation_state$results(), "result.qs")
  })
# 
#   # df <- reactiveVal({NULL})
# 
#   observe({
#     req(simulation_state$results() )
# 
#     req(nrow(simulation_state$results() )>0)
#     print(paste('number of rows in past_populations', nrow(simulation_state$results() )))
#     # print(simulation_state$results())
#   })
# 
  plot_outputs_prevalence <- function(df, morbidity = 'stroke') {

    print(morbidity)

    # Build a complete run/intervention/year grid so absent morbidity rows are treated as zero.
    base_combinations <- df %>%
      distinct(run, intervention, year)

    morbidity_counts <- df[df[[morbidity]] != 0, ] %>%
      count(run, intervention, year, name = "n")

    plot = base_combinations %>%
      left_join(morbidity_counts, by = c("run", "intervention", "year")) %>%
      mutate(n = replace_na(n, 0)) %>%
      group_by(year , intervention) %>%
      summarise(n = mean(n)) %>%
      mutate(year = as.character(year),
             n = n*model_specification$population$scale_down_factor) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = 'series')) %>%
      e_line(n) %>%
      e_tooltip(trigger = "axis", confine=T,backgroundColor = 'white', ) %>%
      e_grid(containLabel = T) %>%
      # e_theme("shine") %>%
      e_theme('roma') |> 
      e_y_axis(name = "Count")

    # return(plot)
  }
  
  plot_outputs_incidence <- function(df, morbidity = 'stroke') {
    
    print(morbidity)

    # Build a complete run/intervention/year grid so absent incidence rows are treated as zero.
    base_combinations <- df %>%
      distinct(run, intervention, year)

    morbidity_counts <- df[df[[morbidity]] == df[['year']], ] %>%
      count(run, intervention, year, name = "n")
    
    plot = base_combinations %>%
      left_join(morbidity_counts, by = c("run", "intervention", "year")) %>%
      mutate(n = replace_na(n, 0)) %>%
      group_by(year , intervention) %>%
      summarise(n = mean(n)) %>%
      mutate(year = as.character(year),
             n = n*model_specification$population$scale_down_factor) %>%
      group_by(intervention) %>%
      e_charts(year, emphasis = list(focus = 'series')) %>%
                 e_bar(n) %>%
      e_tooltip(trigger = "axis", confine=T,backgroundColor = 'white', ) %>%
      e_grid(containLabel = T) %>%
      # e_theme("shine") %>%
      e_theme('roma') |> 
      e_y_axis(name = "Count")
    
    # return(plot)
  }
# 
#   plot_outputs_incidence <- function(df, morbidity = 'stroke') {
# 
#     print(morbidity)
#     print('incidence')
# 
#     # plot = df[df[[morbidity]]==df[['year']],] %>%
#       # simulation_state$results() %>%
#     output_df() %>%
#       filter( !!sym(morbidity) == year) %>%
#       count(run, intervention, year) %>%
#       group_by(year , intervention) %>%
#       summarise(n = mean(n)) %>%
#       mutate(year = as.character(year),
#              n = n*model_specification$population$scale_down_factor) %>%
#       group_by(intervention) %>%
#       e_charts(year) %>%
#       e_line(n) %>%
#       e_tooltip(trigger = "axis", confine=T,) %>%
#       e_grid(containLabel = T,backgroundColor = 'white', itemStyle = list(background = 'white')) %>%
#       e_theme("roma") %>%
#       e_y_axis(name = "Count")
# 
#     # return(plot)
#   }
# 
  output$chd_prevalence_chart <- renderEcharts4r({

    # simulation_state$results() %>%
    output_df() %>%
      # simulation_results() %>%
    # simulation_state$results()
      filter(chd != 0) %>%
      count(run, intervention, year) %>%
      group_by(intervention, year) %>%
      summarise(n = mean(n)) %>%
      mutate(year = as.character(year),
             n = n*model_specification$population$scale_down_factor) %>%
      group_by(intervention) %>%
      e_charts(year) %>%
      e_line(n) %>%
      e_tooltip(trigger = "axis") %>%
      e_theme("roma") %>%
      e_y_axis(name = "Average Count") %>%
      e_grid(containLabel = TRUE) %>%
      e_x_axis(name = "Year")
  })

  output$chd_incidence_chart <- renderEcharts4r({
    message('chd')
    # print(simulation_state$results())
    # print(class(simulation_state$results()))

    # simulation_state$results() %>%
    # print(simulation_state$results())
    output_df() %>%
      filter(!is.na(chd)) %>%
      # simulation_results() %>%
      # filter(chd!=min(year)) %>% 
      filter(chd == year) %>%
      count(run, intervention,year) %>%
      group_by(intervention, year) %>%
      summarise(n = mean(n)) %>%
      mutate(year = as.character(year),
             n = n*model_specification$population$scale_down_factor) %>%
      group_by(intervention) %>%
      e_charts(year) %>%
      e_line(n) %>%
      e_tooltip(trigger = "axis") %>%
      e_theme("walden") %>%
      e_y_axis(name = "Average Count") %>%
      e_grid(containLabel = TRUE) %>%
      e_x_axis(name = "Year")
  })

#   output$chronic_kidney_disease_incidence_chart <- renderEcharts4r({
#     # print(    simulation_state$results() %>%
#     #             pull(chronic_kidney_disease))
# 
#     # print(    output_df() %>%
#     #             pull(chronic_kidney_disease))
# 
#     output_df() %>%
#       # simulation_results() %>%
#       filter(chronic_kidney_disease != 0) %>%
#       count(run,intervention, year) %>%
#       group_by(intervention, year) %>%
#       summarise(n = mean(n)) %>%
#       mutate(year = as.character(year),
#              n = n*model_specification$population$scale_down_factor) %>%
#       group_by(intervention) %>%
#       e_charts(year) %>%
#       e_line(n) %>%
#       e_tooltip(trigger = "axis") %>%
#       e_theme("walden") %>%
#       e_y_axis(name = "Average Count") %>%
#       e_grid(containLabel = TRUE) %>%
#       e_x_axis(name = "Year")
#   })
# 
  output$stroke_prevalence_chart <- renderEcharts4r({
    df = output_df()# simulation_state$results()
    plot_outputs_prevalence(df,morbidity = 'stroke')
  })
# 
  output$stroke_incidence_chart <- renderEcharts4r({
    # req(simulation_state$past_populations())
    message('stroke plot')
    # simulation_state$past_populations() %>%

    # simulation_state$results()  %>%
    # output_df() %>%
    #   # simulation_results() %>%
    #   filter(year!=min(year)) %>% 
    #   filter(stroke == year) %>%
    #   count(run, intervention, year) %>%
    #   group_by(intervention, year) %>%
    #   summarise(n = mean(n)) %>%
    #   mutate(year = as.character(year),
    #          n = n*model_specification$population$scale_down_factor) %>% 
    #   print()
    
    # print(output_df())
    
    output_df() %>%
      # simulation_results() %>%
      # filter(year!=min(year)) %>% 
      filter(stroke == year) %>%
      count(run, intervention, year) %>%
      group_by(intervention, year) %>%
      summarise(n = mean(n)) %>%
      mutate(year = as.character(year),
             n = n*model_specification$population$scale_down_factor) %>%
      group_by(intervention) %>%
      e_charts(year) %>%
      e_line(n) %>%
      e_tooltip(trigger = "axis") %>%
      e_theme("walden") %>%
      e_y_axis(name = "Average Count") %>%
      e_grid(containLabel = TRUE) %>%
      e_x_axis(name = "Year")
  })
# 
#   output$diabetes_incidence_chart <- renderEcharts4r({
#     df = output_df()# simulation_state$results()
#     plot_outputs_incidence(df, 'diabetes')
#   })
#   output$copd_incidence_chart <- renderEcharts4r({
#     df = simulation_state$result()
#     plot_outputs_incidence(df, 'copd')
#   })
#   output$asthma_incidence_chart <- renderEcharts4r({
#     df = simulation_state$result()
#     plot_outputs_incidence(df, 'asthma')
#   })
#   output$rheumatoid_arthritis_incidence_chart <- renderEcharts4r({
#     df = output_df() #simulation_state$results()
#     plot_outputs_incidence(df, 'rheumatoid_arthritis')
#   })
# 
#   output$dementia_incidence_chart <- renderEcharts4r({
#     df = output_df()# simulation_state$results()
#     plot_outputs_incidence(df, 'dementia')
#   })
#   output$heart_failure_incidence_chart <- renderEcharts4r({
#     df = simulation_state$results()
#     plot_outputs_incidence(df, 'heart_failure')
#   })
#   output$non_diabetic_hyperglycaemia_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, 'non_diabetic_hyperglycaemia')
#   })
#   output$osteoporosis_incidence_chart <- renderEcharts4r({
#     df = output_df()#simulation_state$past_populations()
#     plot_outputs_incidence(df, 'osteoporosis')
#   })
#   output$cancer_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, 'cancer')
#   })
#   output$osteoarthritis_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, 'osteoarthritis')
#   })
#   output$epilepsy_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, 'epilepsy')
#   })
#   output$hypothyroidism_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, 'hypothyroidism')
#   })
#   output$colorectal_cancer_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, 'colorectal_cancer')
#   })
#   output$prostate_cancer_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, 'prostate_cancer')
# })
#   output$lung_cancer_incidence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_incidence(df, morbidity = 'lung_cancer')
#   })
# 
# 
#   output$diabetes_prevalence_chart <- renderEcharts4r({
#     df = output_df()# simulation_state$results()
#     plot_outputs_prevalence(df, 'diabetes')
#   })
#   output$copd_prevalence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_prevalence(df, 'copd')
#   })
#   output$asthma_prevalence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_prevalence(df, 'asthma')
#   })
# 
#   output$dementia_prevalence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_prevalence(df, morbidity ='dementia')
#   })
#   output$heart_failure_prevalence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_prevalence(df, morbidity ='heart_failure')
#   })
#   output$cancer_prevalence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_prevalence(df, morbidity ='cancer')
#   })
#   output$colorectal_cancer_prevalence_chart <- renderEcharts4r({
#     req(nrow(output_df() )>0)
#     df = output_df()
#     plot_outputs_prevalence(df, morbidity ='colorectal_cancer')
#   })
#   output$lung_cancer_prevalence_chart <- renderEcharts4r({
#     df = output_df()
#     plot_outputs_prevalence(df, morbidity ='lung_cancer')
#   })





  # observe({
  #   req(simulation_state$past_populations())   # <- note the ()
  #
  #     print(simulation_state$past_populations())
  #     x <- simulation_state$past_populations()
  #     cat("Saving past_populations to myfile.qs\n")
  #     qsave(x, "myfile.qs")
  #   }
  # )

  # 3. Example debug observer
  # observe({
  #   cat("--------------\n")
  #   # simulation_state itself is just a list
  #   print(names(simulation_state))
  #   # This is the *data*
  #   pp <- simulation_state$past_populations()
  #   if (is.null(pp) || nrow(pp) == 0) {
  #     cat("past_populations is empty or NULL\n")
  #   } else {
  #     cat("past_populations rows:", nrow(pp), "\n")
  #   }
  #   cat("--------------\n")
  # })

  # print(simulation_state)


  # observe({
  #   print('--------------')
  #   print(isolate(simulation_state))
  #   # cat(simulation_state)
  #   qsave(simulation_state$past_populations, "myfile.qs")
  #   print('--------------')
  # })  
  

  updateSelectizeInput(session, 
                       inputId='disease', 
                       selected = 'Stroke',
                       choices = y
                       # choices = pop %>%
                       #   count(age20) %>%
                       #   t() %>%
                       #   as.data.frame() %>%
                       #   setNames(format(
                       #     big.mark=",",
                       #     as.numeric(.[2,])*10 #model_specification$population$scale_down_factor
                       #   )) %>%
                       #   sapply(FUN = function(x){
                       #     list(
                       #       x[[1]]
                       #       )})
  )
  
  output$selected_disease <- renderText({
    req(input$disease)
    
    # print(paste("Selected:", input$disease))
    
    # risk_matrix$disease[str_detect(pattern = risk_matrix$disease_pretty_name, string = input$disease)]    
    risk_matrix$disease_pretty_name [str_detect(pattern = risk_matrix$disease_pretty_name, string = input$disease)]

    
    # paste("Selected:", input$disease)
  })
  
  output$selected_disease1 <- renderText({
    
    req(input$disease)
    risk_matrix$disease_pretty_name [str_detect(pattern = risk_matrix$disease_pretty_name, string = input$disease)]

  })
  
  output$selected_prevalence_chart <- renderEcharts4r({
    df = output_df()# simulation_state$results()
    disease = risk_matrix$disease[str_detect(pattern = risk_matrix$disease_pretty_name, string = input$disease)]
    plot_outputs_prevalence(df, disease)
  })
  
  output$selected_incidence_chart <- renderEcharts4r({
    df = output_df()# simulation_state$results()
    disease = risk_matrix$disease[str_detect(pattern = risk_matrix$disease_pretty_name, string = input$disease)]
    plot_outputs_incidence(df, disease)
    
  })
  
  output$side_emergency_admissions <- renderText({
    sick_days_df() %>%
      group_by(year, intervention) %>%
      summarise(
        # days_lost = sum(days_lost, na.rm =T),
        # cost = sum(cost, na.rm =T),
        sick_spells = sum(sick_spells, na.rm =T)
      ) %>%
      ungroup() |> 
      filter(year == min(year)) %>%
      filter(intervention == 'non-intervention') |> 
      pull(sick_spells)|> 
      format(big.mark = ",")
      
      })
  
  output$side_cost <- renderText({
    # validate(
    #   need(
    #     !is.null(cost_values,'Apply Intervention')
    #   )
    
    total <- cost_values$flat_fixed_cost +
      (cost_values$per_person_per_year # * model_specification$population$scale_down_factor
      ) + # Assuming population size
      (cost_values$per_person #* model_specification$population$scale_down_factor
      ) +
      cost_values$per_year
    
    paste0(" £",
           format(total, big.mark = ",")
           )
    
  })
  
  output$side_multimorbidity <- renderText({
    
    output_df() %>%
      select(-c(1,multimorbidity,275)) |>
      add_multimorbidity_fn() |>
      group_by(year,run,intervention) %>%
      summarise(multimorbidity = mean(multimorbidity,na.rm=T)) %>%
      group_by(year,intervention) %>%
      summarise(multimorbidity = sum(multimorbidity,na.rm=T)) %>%
      ungroup() |> 
      filter(year == min(year)) %>%
      filter(intervention =='non-intervention')   |> 
      pull(multimorbidity)|> 
      format(big.mark = ",")
    
    })
  
  output$side_dalys <- renderText({
          daly_df()  |>
       filter(intervention == 'non-intervention') |> 
       filter(year == min(year)) |> 
       mutate( yll = yll * model_specification$population$scale_down_factor)  |> 
       mutate(daly = yll + total_dw) |> 
      pull(daly) |> 
      format(big.mark = ",")
    
  })
  
  
#End specify intervention ----

#   start commented out production data ----
#   output$geo_sunburst <- renderEcharts4r({geo_sunburst})
#   output$geo_treemap <- renderEcharts4r({geo_treemap })
# 
#   # observe({
# # 
#   # print(input$geo_sunburst_clicked_data)
#   # print(input$geo_sunburst_clicked_data_value)
#   # print(input$geo_sunburst_clicked_row)
#   # print(input$geo_sunburst_clicked_serie)
#   #
#   # print(input$geo_treemap_clicked_data)
#   # print(input$geo_treemap_clicked_data_value)
#   # print(input$geo_treemap_clicked_row)
#   # print(input$geo_treemap_clicked_serie)
# 
#   # })
# 
# 
  # Reactive data source for pivot module
  # population_data <- reactive({
  #   # df <- read.csv("./populations/test_population.csv", stringsAsFactors = FALSE)
  #   df <- read.fst('./populations/k20_population.fst')
  # 
  #   # Convert logical health conditions to factors for better pivoting
  #   health_cols <- c("stroke", "chd", "diabetes", "dementia", "heart_failure",
  #                    "atrial_fibrillation", "hypertension", "chronic_kidney_disease")
  #   df[health_cols] <- lapply(df[health_cols], function(x) factor(ifelse(x, "Yes", "No")))
  # 
  #   # Ensure proper factor ordering for age groups
  #   if("age_risk" %in% names(df)) {
  #     df$age_risk <- factor(df$age_risk, levels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75-110"))
  #   }
  # 
  #   # Convert BMI to factor with proper ordering
  #   if("bmi" %in% names(df)) {
  #     df$bmi <- factor(df$bmi, levels = c("normal", "overweight", "obese"))
  #   }
  # 
  #   return(df)
  # })
  
  
  population_data <- reactive({
    # df <- read.csv("./populations/test_population.csv", stringsAsFactors = FALSE)
    df <- read.fst('./3_pre_main/intermediate_populations/initial_time_zero_population.fst')
    df <- df |> select(all_of(pivot_columns))
    # Ensure proper factor ordering for age groups
    if("age_risk" %in% names(df)) {
      df$age20 <- factor(df$age20, levels = c("0-20", "20-40", '40-60','06-80','80-100'))
    }
    
    # Convert BMI to factor with proper ordering
    if("bmi" %in% names(df)) {
      df$bmi <- factor(df$bmi, levels = c("normal", "overweight", "obese"))
    }
    
    return(df)
  })
  

  # Pivot module server ----
  pivot_result <- pivot_module_server("pivot_reports", data = population_data)

#   output$mymap <- renderLeaflet({
#     leaflet(#width='99%',height='99%',
#       options = leafletOptions(zoomControl = FALSE)
#     ) %>%
#       htmlwidgets::onRender("function(el, x) {
#         //L.control.zoom({ position: 'bottomright' }).addTo(this);
#         var map = this;
#         setTimeout(function() { map.invalidateSize(); }, 100);
#       }") %>%
#       addTiles() %>%
#       setView(lng = -5.9576, lat = 54.904, zoom = 8) %>%
#       # addMarkers(lng = -0.1276, lat = 51.5074, popup = "London") %>%
# 
#       addCircles(data = parks,
#                  weight = 15,
#                  # radius = 150,
#                  fillOpacity = 1,
#                  fillColor  = 'mediumseagreen',
#                  fill = F,
#                  opacity=0.5,
#                  color = 'mediumseagreen',
#                  stroke = T,
#                  label = ~name#,
#                  #popup = ~as.character(name)
#       ) %>%
#       addCircles(data = fast_food,
#                  weight = 15,
#                  fillOpacity = 1,
#                  fillColor  = 'steelblue',
#                  fill = F,
#                  opacity=0.5,
#                  color = 'steelblue',
#                  stroke = T,
#                  label = ~name
#       ) %>%
#       addLegend(position = 'topright',
#                 colors = c('mediumseagreen','steelblue'),
#                 labels = c('Parks','Fast Food Outlets'),
#                 opacity = 1
#       ) %>%
#       addPolygons(data = st_as_sfc(
#         st_bbox(c(
#           xmin = -1.796265 + 0.2,
#           ymin = 53.40626 + 0.2,
#           xmax = -10.1239 - 0.2,
#           ymax = 56.34699 - 0.2
#         ), crs = st_crs(4326))
#       ),
#       color = 'black',
#       weight = 2,
#       fill = F,
#       group = 'bbox')
#     # addPolygons(data = isolate(bb()),
#     #             color = 'black',
#     #             weight = 2,
#     #             fill = F)
# 
#   })
# 
#   observe({
#     print(input$switch)
#     default_val(input$switch)
#   })
# 
#   trig= reactiveVal({FALSE})
# 
#   observe({
#     x <- isolate(trig())
#     debounced_bounds()
#     if(default_val()==F){
#       trig(!x)
#     }
#   })
# 
#   observeEvent(ignoreInit = T, ignoreNULL = T, trig(),{ #debounced_bounds()
#     req(default_val()==F)
#     leafletProxy('mymap')  %>%
#       clearGroup('bbox') %>%
#       addPolygons(data = isolate(bb()),
#                   color = 'black',
#                   weight = 2,
#                   fill = F,group = 'bbox')#%>%
#     # flyTo(lng = -6.1576, lat = 54.704, zoom = 7)
#     # setView(lng = -5.9576, lat = 54.904, zoom = 8) %>% %>%
#     # clearShapes() %>
# 
# 
#   })
# 
#   map_filtered_chart <- reactiveVal(pop)
# 
#   debounced_bounds <- reactive({
#     req(bb()!= list(ymax = 57.0706,
#                     xmax =  0.2636719,
#                     ymin = 52.19414,
#                     xmin = -12.56836))
# 
#     input$mymap_bounds
#   }) %>%
#     debounce(3000)   # 4000 milliseconds = 4 seconds
# 
#   bb <- reactiveVal({
#     st_as_sfc(
#       st_bbox(c(
#         xmin = -1.796265 + 0.2,
#         ymin = 53.40626 + 0.2,
#         xmax = -10.1239 - 0.2,
#         ymax = 56.34699 - 0.2
#       ), crs = st_crs(4326))
#     )
#   })
# 
#   default_val <- reactiveVal({FALSE})
#   # 2. Replace input$mymap_bounds with debounced_bounds() in your observeEvent
# 
#   observeEvent(debounced_bounds() , { #debounced_bounds()
#     req(debounced_bounds())
#     req(default_val()==F)
#     req(input$mymap_bounds$south != input$mymap_bounds$north)
#     
#     # print(bb())
#     req(bb()!=   st_as_sfc(
#       st_bbox(c(ymax = 57.0706,
#                 xmax =  0.2636719,
#                 ymin = 52.19414,
#                 xmin = -12.56836))))
# 
# 
# 
#     # print('printing bounds')
#     # print(input$mymap_bounds)
# 
#     ytol = abs(input$mymap_bounds$south - input$mymap_bounds$north)/10
#     xtol = abs(input$mymap_bounds$west - input$mymap_bounds$east)/10
# 
# 
#     bbox_poly <- st_as_sfc(
#       st_bbox(c(
#         xmin = input$mymap_bounds$west + xtol,
#         ymin = input$mymap_bounds$south + ytol,
#         xmax = input$mymap_bounds$east - xtol,
#         ymax = input$mymap_bounds$north - ytol
#       ), crs = st_crs(4326))
#     )
# 
#     bb(bbox_poly)
#     # print(bbox_poly)
# 
#     # req(input$mymap_bounds$west != input$mymap_bounds$west)
#     # req(input$mymap_bounds$north != input$mymap_bounds$south)
# 
#     print(bbox_poly)
#     print(bbox_poly$geometry)
#     inside_mat <- st_within(csv_pts_wgs84, bbox_poly, sparse = FALSE)
#     #st_within(csv_pts_wgs84, x, sparse = FALSE)
# 
#     csv_pts_wgs84 <- csv_pts_wgs84 %>%
#       mutate(in_bbox = as.logical(inside_mat[, 1]))
# 
#     dz_in_bbox <- csv_pts_wgs84 %>%
#       filter(in_bbox)
# 
#     # dz_in_bbox$DZ2021_code
#     # dz_in_bbox$DZ2021_name
# 
#     # print(head(pop))
#     # print(head(dz_in_bbox$DZ2021_code))
#     # print(head(map_filtered_chart()))
# 
#     # map_filtered_chart(pop %>%
#     #                      filter(dz_id %in% dz_in_bbox$DZ2021_code ))
# 
#     # print(dz_in_bbox)
#     # print(pop$sdz_code)
#     # print(dz_in_bbox$DZ2021_code)
# 
#     map_filtered_chart(pop %>%
#                          filter(sdz_code %in% dz_in_bbox$SDZ2021_code ))
# 
#     # print('##############')
#     # print(head(map_filtered_chart()))
#     # print('##############')
#   })
# 
#   output$group_echarts <- renderEcharts4r({
#     map_filtered_chart() %>%
#       group_by(Urban_status) %>%
#       summarise(count = n() ) %>%
#       e_charts(Urban_status) %>%
#       echarts4r::e_tooltip(trigger = "axis",confine = T) %>%
#       e_bar(count) %>%
#       e_title("Population by Urban Status") %>%
#       e_theme('walden') %>%
#       e_grid(containLabel = TRUE)
#   })
# 
#   output$excedance_bmi <- renderEcharts4r({
# 
#     # bmi_counts(); validate(need(nrow(dat) > 0, "BMI not available."))
# 
#     expect <- pop %>%
#       count(bmi,name = 'bmi_count') %>%
#       add_count(wt = bmi_count, name = 'total_count') %>%
#       mutate( expect = bmi_count/total_count )
# 
#     dat <-count(map_filtered_chart(),bmi, name = 'filter') %>%
#       add_count(wt = filter,name = 'filtered_count') %>%
#       mutate( actual = filter/filtered_count )
# 
#     expect <- left_join(expect, dat) %>%
#       mutate(exceed = actual - expect) %>%
#       mutate(isPos = (exceed>0)) %>%
#       filter(!is.na(bmi)) %>%
#       filter(bmi!='normal')
# 
#     # print(expect)
#     expect %>%
#       # group_by(isPos) %>%
#       echarts4r::e_charts(bmi,textStyle = list( fontSize=9)) %>%
#       echarts4r::e_bar(exceed) %>%
#       e_visual_map(type = 'piecewise', orient = 'horizontal',
#                    pieces = list(
#                      list(gt= 0,
#                           # lte= 50,
#                           color= '#ffcdd2'),
#                      list(lte= 0,
#                           # lte= 50,
#                           color= '#bbdefb'))
#       ) %>%
#       # )) %>%
#       # e_color((c("#ffcdd2",
#       #            "#bbdefb"
#       #            ))) %>%
#       e_x_axis(show = FALSE) %>%
#       # e_y_axis(show = FALSE) %>%
#       #echarts4r::e_title("BMI distribution (filtered)") %>%
#       echarts4r::e_tooltip(trigger = "axis",confine =T) %>%
#       e_theme('walden') %>%
#       echarts4r::e_title( text = "BMI Excedance", subtext = "Where values of BMI exceed expected") %>%
#       e_legend(show=F) %>%
#       e_y_axis(formatter = e_axis_formatter("percent", digits = 0)) %>%
#       # e_color(c('#2AFEB7','yellow')) %>%
#       #echarts4r::e_x_axis(name = "BMI band") %>%
#       # echarts4r::e_y_axis(name = "People") %>%
#       e_grid(containLabel = TRUE) # top = 40, right = 20, bottom = 40, left = 50)
#   })
# 
#   # exceedence_age
#   output$excedance_age <- renderEcharts4r({
# 
#     ## bmi_counts(); validate(need(nrow(dat) > 0, "BMI not available."))
# 
#     expect <- pop %>%
#       count(age10,bmi, name = 'bmi_count') %>%
#       add_count(age10, wt = bmi_count, name = 'total_count') %>%
#       mutate( expect = bmi_count/total_count )
# 
#     dat <-count(map_filtered_chart(), bmi, age10, name = 'filter') %>%
#       add_count(age10,wt = filter,name = 'filtered_count') %>%
#       mutate( actual = filter/filtered_count )
# 
#     expect <- left_join(expect, dat) %>%
#       mutate(exceed = actual - expect) %>%
#       mutate(isPos = (exceed>0)) %>%
#       filter(!is.na(bmi)) %>%
#       filter(bmi!='normal')
# 
#     # print(expect)
#     expect %>%
#       group_by(age10) %>%
#       echarts4r::e_charts(bmi,textStyle = list( fontSize=9)) %>%
#       echarts4r::e_bar(exceed) %>%
#       e_visual_map(type = 'piecewise',orient = 'horizontal',
#                    pieces = list(
#                      list(gt= 0,
#                           # lte= 50,
#                           color= '#ffcdd2'),
#                      list(lte= 0,
#                           # lte= 50,
#                           color= '#bbdefb'))
#       ) %>%
#       # )) %>%
#       # e_color((c("#ffcdd2",
#       #            "#bbdefb"
#       #            ))) %>%
#       e_x_axis(show = FALSE) %>%
#       # e_y_axis(show = FALSE) %>%
#       #echarts4r::e_title("BMI distribution (filtered)") %>%
#       echarts4r::e_tooltip(trigger = "axis",confine =T) %>%
#       e_theme('walden') %>%
#       echarts4r::e_title( text = "BMI Age Excedance", subtext = "Where values of BMI exceed expected") %>%
#       e_legend(show=F) %>%
#       e_y_axis(formatter = e_axis_formatter("percent", digits = 0)) %>%       # e_color(c('#2AFEB7','yellow')) %>%
# 
#       #echarts4r::e_x_axis(name = "BMI band") %>%
#       # echarts4r::e_y_axis(name = "People") %>%
#       e_grid(containLabel = TRUE) # top = 40, right = 20, bottom = 40, left = 50)
#   })
# 
#   output$excedance_bmi_deprivation <- renderEcharts4r({
#     ## bmi_counts(); validate(need(nrow(dat) > 0, "BMI not available."))
# 
#     expect <- pop %>%
#       count(bmi,mdm_quintile_soa_name, name = 'bmi_count') %>%
#       add_count(mdm_quintile_soa_name, wt = bmi_count, name = 'total_count') %>%
#       mutate( expect = bmi_count/total_count )
# 
#     dat <-count(map_filtered_chart(),bmi, mdm_quintile_soa_name, name = 'filter') %>%
#       add_count(mdm_quintile_soa_name, wt = filter,name = 'filtered_count') %>%
#       mutate( actual = filter/filtered_count )
# 
#     expect <- left_join(expect, dat) %>%
#       mutate(exceed = actual - expect) %>%
#       mutate(isPos = (exceed>0)) %>%
#       filter(!is.na(bmi))
# 
#     # print(expect)
#     expect %>%
#       group_by(mdm_quintile_soa_name) %>%
#       echarts4r::e_charts(bmi,textStyle = list( fontSize=9)) %>%
#       echarts4r::e_bar(exceed) %>%
#       echarts4r::e_title( text = "BMI Deprivation Exceedance", subtext = "Where values of BMI exceed expected") %>%
#       e_visual_map(type = 'piecewise',orient = 'horizontal',
#                    pieces = list(
#                      list(gt= 0,
#                           # lte= 50,
#                           color= 'rgb(0,200,0)'# '#ffcdd2'
#                           ),
#                      list(lte= 0,
#                           # lte= 50,
#                           color= 'steelblue'
#                           ))
#       ) %>%
#       e_x_axis(show = FALSE) %>%
#       e_theme('walden') %>%
#       e_y_axis(formatter = e_axis_formatter("percent", digits = 0)) %>%
#       # e_y_axis(show = FALSE) %>%
#       echarts4r::e_tooltip(trigger = "axis",confine =T) %>%
#       e_legend(show=F) %>%
#       e_grid(containLabel = TRUE)
#   })
# 
# 
#   # --- charts ---------------------------------------------------------------
#   output$bmi_chart <- renderEcharts4r({
#     dat <- count(map_filtered_chart(),bmi) %>% ## bmi_counts(); validate(need(nrow(dat) > 0, "BMI not available."))
#       mutate(n = n * pop_scale_up)
#     
#     dat %>%
#       echarts4r::e_charts(bmi,height = 190, width='200',textStyle = list( fontSize=9)) %>%
#       echarts4r::e_bar(n, name = "Count") %>%
#       #echarts4r::e_title("BMI distribution (filtered)") %>%
#       echarts4r::e_tooltip(trigger = "axis",confine =T) %>%
#       e_legend(show=F) %>%
#       # e_color(c('#2AFEB7','yellow')) %>%
#       #echarts4r::e_x_axis(name = "BMI band") %>%
#       echarts4r::e_y_axis(name = "People") %>%
#       e_theme('walden') %>%
#       e_grid(containLabel = TRUE) # top = 40, right = 20, bottom = 40, left = 50)
#   })
# 
#   output$age_chart <- renderEcharts4r({
#     dat <- count(map_filtered_chart(),age20) %>% ##age_counts(); validate(need(nrow(dat) > 0, "Age not available."))
#       mutate(n = n * pop_scale_up)
#     
#     dat %>%
#       echarts4r::e_charts(age20,height = 190, width='200') %>%
#       echarts4r::e_bar(n, name = "Count") %>%
#       #echarts4r::e_title("Age bands (filtered)") %>%
#       echarts4r::e_tooltip(trigger = "axis",confine =T) %>%
#       e_legend(show=F) %>%
#       #echarts4r::e_x_axis(name = "Age band") %>%
#       echarts4r::e_y_axis(name = "People") %>%
#       e_legend(show=F) %>%
#       e_theme('walden') %>%
#       #e_color(c('#2AFEB7','yellow')) %>%
#       e_grid(containLabel = TRUE)
#   })
# 
#   output$sex_chart <- renderEcharts4r({
# 
#     dat <- count(map_filtered_chart(),sex) %>% #sex_counts(); validate(need(nrow(dat) > 0, "Sex not available."))
#       mutate(n = n * pop_scale_up)
#     
#     dat %>%
#       # group_by(sex) %>%
#       echarts4r::e_charts(sex,height = 190,width='200') %>%
#       echarts4r::e_bar(n) %>%
#       e_legend(show=F) %>%
#       #echarts4r::e_title("Sex split (filtered)", textStyle = list( fontSize=9)) %>%
#       echarts4r::e_tooltip(formatter = "{b}: {c} ({d}%)",confine = T) %>%
#       #e_color(c('#2AFEB7','yellow')) %>%
#       echarts4r::e_grid(containLabel = TRUE) %>%
#       e_text_style(
#         #color = "white",
#         #fontStyle = "italic"
#         textStyle = list(fontSize = 9)
#         ) %>%
#       e_theme('walden')
#   })
# 
#   output$depriv_chart <- renderEcharts4r({
#     dat <- count(map_filtered_chart(), mdm_quintile_soa_name) %>% #depriv_counts(); validate(need(nrow(dat) > 0, "Deprivation not available."))
#       mutate(n = n * pop_scale_up)
#     
#     # Pick the x column dynamically
#     # xcol <- if ("mdm_quintile_soa_name" %in% names(dat)) "mdm_quintile_soa_name" else "mdm_quintile_soa"
# 
#     dat %>%
#       mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
#                                             levels = c("Most Deprived","Quintile 2","Quintile 3","Quintile 4","Least Deprived"))) %>%
#       arrange(mdm_quintile_soa_name) %>%
#       echarts4r::e_charts(mdm_quintile_soa_name,height = 190,width='200') %>%
#       echarts4r::e_bar(n, name = "Count") %>%
#       e_legend(show = F) %>%
#       #echarts4r::e_title("Deprivation quintile (filtered)",textStyle = list( fontSize=9)) %>%
#       #echarts4r::e_tooltip(trigger = "axis") %>%
#       echarts4r::e_tooltip(trigger = "axis",confine =T) %>%
#       # e_color(c('#2AFEB7','yellow')) %>%
# 
#       #echarts4r::e_x_axis(name = "MDM quintile") %>%
#       echarts4r::e_y_axis(name = "People") %>%
#       echarts4r::e_grid(containLabel = TRUE) %>%
#       e_theme('walden')
#   })
# 
#   output$qrisk_chart <- renderEcharts4r({
#     dat <- map_filtered_chart() %>%
#       slice_sample(n = 500)
# 
#     y_max <- ceiling(max(dat$qrisk_score, na.rm = TRUE))
#     y_max <- max(y_max, 1)  # ensure sensible upper bound
# 
#     dat %>%
#       echarts4r::e_charts(id,height = 190,width='200') %>%
#       # points (strip/list)
#       echarts4r::e_scatter(qrisk_percentile,
#                            name = "QRisk",
#                            symbolSize = 6,
#                            large = TRUE,
#                            largeThreshold = 2000,
#                            itemStyle=list(opacity=0.2)
#       ) %>%
#       # mean & median reference lines
#       echarts4r::e_mark_line(data = list(
#         list(type = "average", name = "Mean"),
#         list(type = "median", name = "Median")
#       )) %>%
#       # axes & layout
#       echarts4r::e_y_axis(
#         name = "QRisk (%)",
#         min = 0, max = y_max,
#         axisLabel = list(formatter = "{value}%")
#       ) %>%
#       echarts4r::e_x_axis(show = FALSE) %>%
#       echarts4r::e_grid(containLabel = TRUE) %>%
#       e_theme('walden')
# 
#   })
# 
#   output$qrisk_chart1 <- renderEcharts4r({
#     dat <- map_filtered_chart() %>%
#       slice_sample(n = 500)
# 
#     dat %>%
#       filter(age>25) %>%
#       filter(!is.na(bmi)) %>%
#       group_by(bmi) %>%
#       e_charts(height=290) %>%
#       e_density(qrisk_percentile,breaks=5) %>%
# 
#       e_mark_line(title = 'Baseline',
#                   data = list(
#                     type = "average",
#                     name = "Average"
#                   )) %>%
#       e_theme('walden')%>%
#       echarts4r::e_grid(containLabel = TRUE)
# 
#   })
# 
#   # --- headline card (optional) --------------------------------------------
#   output$headline_count <- renderText({
# 
#     format(nrow(map_filtered_chart())*pop_scale_up, big.mark = ",")
#   })
# 
#   output$qrisk_average <- renderText({
#     # print('##############')
#     # print(head(map_filtered_chart()))
#     # print('##############')
#     signif(digits = 3 ,mean(map_filtered_chart()$qrisk_score)) #qrisk_percentile
#   })
# 
#   output$areaPer100k <- renderText({
#     tryCatch({
#     inside <- st_within(parks, bb(), sparse = FALSE)
#     sum(parks$area[inside])/nrow(map_filtered_chart()) * 100000/1000
#   }, error = function(e) {
#     NA_real_
#   })
#   })
# 
#   output$parkPer100k <- renderText({
#     tryCatch({
#     inside <- st_within(parks, bb(), sparse = FALSE)
#     nrow(parks[inside,])/nrow(map_filtered_chart()) * 100000
#   }, error = function(e) {
#     NA_real_
#   })
#   })
# 
#   output$ffPer100k <- renderText({
#     tryCatch({
#       inside <- st_within(fast_food, bb(), sparse = FALSE)
#       nrow(fast_food[inside, ]) / nrow(map_filtered_chart()) * 100000
#     }, error = function(e) {
#       # NA_real_
#       bb_replace <- st_as_sfc(
#         st_bbox(c(ymax = 57.0706,
#                   xmax =  0.2636719,
#                   ymin = 52.19414,
#                   xmin = -12.56836), crs = st_crs(4326)
#                 ))
#       inside <- st_within(fast_food, bb_replace, sparse = FALSE)
#       nrow(fast_food[inside, ]) / nrow(map_filtered_chart()) * 100000
#     })
#   })
# 
#   output$overweight_percentage <- renderText({
#     x <-  map_filtered_chart() %>%
#       summarise(ow = sum(bmi%in%c('overweight','obese')), n=n()) %>%
#       mutate(pw_perc = ow/n) %>%
#       pull(pw_perc)
#     signif(digits = 2, x*100)
#   })
  # end commented out production data ----
  
}

options(shiny.autoreload = T);shinyApp(ui = ui, server = server)


