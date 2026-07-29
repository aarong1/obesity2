library(shiny)
library(echarts4r)
library(jsonlite)
library(dplyr)
library(purrr)

# load('prep_interactive_model_run.RData')

# save(file = 'run_requirements.RData',
#      initial_time_zero_population,
#      prevalence_hsct,
#      prevalence_hsct_new,
#      test_specification,
#      trusts,
#      morbidities,
#      apply_qmortality_mortality,
#      apply_age_sex_death,
#      lifetables,
#      qmortality_risk,
#      qmortality_female_risk,
#      qmortality_male_risk,dead_population,
#      apply_chd_risk,
#      transform_10y_probability_to_1y,
#      transform_probability_to_1y)

# rm(list=ls())

# load(file = 'run_requirements.RData')

# initial_time_zero_population <- read.csv('initial_time_zero_population.csv')
# source('main_utils_2_4.R')
#
# source('./modules/chart_update_module_4/prep_interactive_model_run.R')
source('load_all_files_wrap_in_fn.R')

# source('prep_interactive_model_run.R')


# source(echo = T,'prep_interactive_model_run.R')





# load("prep_interactive_model_run.RData")

# Module UI

chartUpdateModuleUI <- function(id) {
  ns <- NS(id)
  echart_id <- (ns('target_echart')) #demo_chart-target_echart
  tagList(
    # Include custom JavaScript
    tags$head(tags$script(HTML( paste0("
    
    setTimeout(function() {
    console.log('running add message handler');
    Shiny.addCustomMessageHandler('updateChart', function(seriesList) {
        console.log('inmessage handler');

        console.log(seriesList);
  const chartEl = document.querySelector('#",echart_id," > .echarts4r'); 
  if (!chartEl) {
    console.warn('Chart container not found');
    return;
  }
  
  //chart.resize();

  const chart = echarts.getInstanceByDom(chartEl);
  if (!chart) {
    console.warn('ECharts instance not ready');
    return;
  }

  const legendData = seriesList.map(s => s.name);
  const seriesConfig = seriesList.map(s => ({
    name: s.name,
    type: 'line',
    data: s.data
  }));

  chart.setOption({
    xAxis: { type: 'time', scale: true, max: '2030-01-01' },
    yAxis: { type: 'value', scale: true },
    tooltip: { trigger: 'axis' },
    legend: { data: legendData },
    series: seriesConfig
  });
});
    }, 500);")))
    ),
    
    # Chart container
  #  div(class = "card",
     #   div(class = "card-header",
   #         h4("Real-time Simulation Results")
    #    ),
        div(class = "d-flex align-items-center justify-content-center",
            # actionButton(ns("start_simulation"), "Start Simulation", 
            #              class = "btn btn-primary", icon = icon("play")),
            div(id = ns('target_echart'), 
                class = 'shadow-sm rounded-5 d-flex align-items-center justify-content-center',
                style = "height: 70%;width:100%;",
                
                # Initial empty chart
                #e_charts()
                data.frame(year = 2026, incidence = 100000) |>
                  mutate(year = as.character(year)) |>
                  e_charts(year) |> #,width = '100rem' ,width = 500
                  e_theme('walden') |> 
                  e_axis_labels(x='Year',y='Incidence') |> 
                e_line(incidence, name = "Waiting for Input")
        #    )
        )
    ),
    
    # Status display
    # div(class = "mt-3",
    #     verbatimTextOutput(ns("simulation_status"))
    # )
  
  )
}

# Module Server
chartUpdateModuleServer <- function(id, 
                                    runButton = reactive(run), 
                                    input_pop = reactive({first_population}),
                                    intervention_shape = reactive({list(1)}) ) {
  # print(intervention_shape)
  
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for simulation state
    simulation_active <- reactiveVal(FALSE)
    simulation_results <- reactiveVal(data.frame())
    simulation_deaths <- reactiveVal(data.frame())
    
    # Status output
    # output$simulation_status <- renderText({
    #   if (simulation_active()) {
    #     "Simulation is running..."
    #   } else {
    #     "Simulation stopped."
    #   }
    # })
    
    # Start simulation
    observeEvent(runButton(), ignoreInit = T,{ #input$start_simulation, {
      print('start')
      simulation_active(TRUE)
      # simulation_results(data.frame())
      
      # Run simulation in background using reactiveTimer or observe
      past_populations_run <<- run_simulation_async(input_pop(), session, intervention_shape())
      simulation_results(past_populations_run)
      simulation_deaths(filter( past_populations_run, !is.na(death) & !is.null(death) & !death==0 ))
      
      simulation_active(FALSE)
      
    })
    
    # Stop simulation
    observeEvent(input$stop_simulation, {
      simulation_active(FALSE)
    })
    
    # Async simulation function
    run_simulation_async <- function(input_pop, session, intervention_shape) {
      
      # print(input_pop$intervention_target)
      past_populations_run <- run_model(input_pop, session, intervention_shape)
      

      return(past_populations_run)
    }

    # print(range(past_populations_run$year))
    # Return reactive for external use
    return(list(
      past_populations = reactive(past_populations_run),
      active = reactive(simulation_active()),
      results = simulation_results, #reactive(simulation_results()),
      deaths = reactive(simulation_deaths())
    )
    )
  })
}

# shinyApp(
#   ui = fluidPage(
#     chartUpdateModuleUI("chart1"),
#     actionButton("run", "Run Simulation")
#   ),
#   server = function(input, output, session) {
#     chartUpdateModuleServer("chart1", runButton = reactive(input$run))
#   }
# )

