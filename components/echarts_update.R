library(shiny)
library(echarts4r)

# echarts.getInstanceByDom(document.getElementById("htmlwidget-ff15bafa3bfe980a1fb7"))

# // Example: new mpg values (make sure they align with the same x-axis categories)
# const newMpgValues = [
#   25, 22, 30, 28, 26, 24, 27, 31, 29, 23,
#   21, 20, 24, 26, 28, 27, 22, 25, 23, 29,
#   30, 24, 21, 26, 28, 58, 23, 22, 20, 25, 24, 23
# ];

# // Update the series data only
# if (x) {
#   x.setOption({
#     series: [{
#       name: "mpg",
#       type: "bar",
#       data: newMpgValues
#     }]
#   });
# }


ui <- fluidPage(
  
  div(id = 'my_chart_wrapper',
      (  e_chart(data.frame(x = '2015', y = 4000),x) |> 
           e_line(y,name = 'y') |>
           # 1) Quick drop‐zero approach:
           e_x_axis(min = 2014, max = 2020) |> 
           htmlwidgets::onRender("
    function(el, x) {
      // 1. grab the echarts instance
      console.log(el)
      var chart = echarts.getInstanceByDom(el, 
      {
     series: [{
       name: 'y',
       type: 'line',
   xAxis: {
     type: 'time',
      min: 2015,  // <-- explicitly start at the smallest value
     max: 2030,   // (optional) 
         scale: true      // <-- don’t force zero
     //data: ['Jan', 'Feb', 'Mar', 'Apr']
   },
   yAxis: {
     type: 'value',
     scale: true      // <-- don’t force zero
   },
   scale:true,
       data: [['2015', 4000], ['2016', 4323],['2017',4534]]
     }]
   });
      console.log(chart)
      //if (!chart) {
        chart = echarts.init(el,);
        console.log(chart)
        window.c = chart
        console.log(1);
      //}

      // 2. define a global updater
      window.updateChart = function(chart, newData) {
        // newData should be an array of [x, y] pairs or whatever you expect
  chart.setOption({
     series: [{
       name: 'y',
       type: 'line',
   xAxis: {
     type: 'time',
      min: 2015,  // <-- explicitly start at the smallest value
     max: 2030,   // (optional) 
         scale: true      // <-- don’t force zero
     //data: ['Jan', 'Feb', 'Mar', 'Apr']
   },
   yAxis: {
     type: 'value',
     scale: true      // <-- don’t force zero
   },
   scale:true,
       data:newData// [['2015', 4000], ['2016', 4323],['2017',4534]]
     }]
   });

      }

      // 3. listen for Shiny messages
      if (HTMLWidgets.shinyMode) {
        Shiny.addCustomMessageHandler('echart-update', function(message) {
          // message.data is what we sent from R
          window.updateChart(message.data);
        });
        console.log(3);
      }
    }
  ")
      )
),

  tags$head(
    tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function () {
      console.log('DOM loaded');

  wrapper = document.getElementById('my_chart_wrapper');
      console.log(wrapper);

  chartDom = wrapper.querySelector('.echarts4r');
     console.log(chartDom);

     window.chartDom = chartDom;

  echarts_object = echarts.getInstanceByDom(chartDom);
      console.log(echarts.getInstanceByDom(chartDom));

 // window.echarts_object = echarts_object;

 // if (echarts_object) {
 //   echarts_object.setOption({
 //     series: [{
 //       name: 'y',
 //       type: 'line',
 //       data: [[2015, 1], [2, 1]]
 //     }]
 //   });
 // }

  document.getElementById('start').addEventListener('click', function () {

  echarts_object = echarts.getInstanceByDom(window.chartDom);

  window.echarts_object = echarts_object;

 if (echarts_object) {
  echarts_object.setOption({
    series: [{
      name: 'y',
      type: 'line',
  xAxis: {
    type: 'time',
     min: 2015,  // <-- explicitly start at the smallest value
    max: 2030,   // (optional)
        scale: true      // <-- don’t force zero
    //data: ['Jan', 'Feb', 'Mar', 'Apr']
  },
  yAxis: {
    type: 'value',
    scale: true      // <-- don’t force zero
  },
  scale:true,
      data: [['2015', 4000], ['2016', 4323],['2017',4534]]
    }]
  });
 }

  console.log('Start button clicked');

});
});

                     ")
    )
  ),
  

  actionButton("start", "Start JS Model Run"),

)

server <- function(input, output, session) {
  observeEvent(input$start, {
    session$sendCustomMessage("startModelRun", list())
  })
}

# shinyApp(ui, server)
