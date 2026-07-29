library(shiny)
library(fontawesome)
library(htmltools)

#source 
#https://www.r-bloggers.com/2024/01/icons-in-a-shiny-dropdown-input/

selectInputWithIcons <- function(
    inputId, 
    inputLabel,
    labels, values,
    icons,
    iconStyle = NULL,
    selected = NULL, 
    multiple = FALSE, 
    width = NULL
){
  options <- mapply(function(label, value, icon){
    list(
      "label" = label,
      "value" = value,
      "icon"  = paste0(
             lapply(icon, \(x) as.character(fa_i(x,style = 'padding-inline:5px;'))),
             collapse = "&nbsp;"
         )
    )
  }, labels, values, icons, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  render <- paste0(
    "{",
    "  item: function(item, escape) {", 
    "    return '<span ",'style="list-style-type: none; padding: 10px;"',">' 
+
    '<h5>'+escape(item.label)+'</h5>'     + item.icon + ' ' +  '</span><br><br>';", 
    "  },",
    "  option: function(item, escape) {", 
    "    return '<li ",'style="list-style-type: none;padding: 10px;"',">'  + '<h4>'+escape(item.label)+'</h4>' +'<br>'+ item.icon + '</li><br>';", 
    "  }",
    "}"
  )
  widget <- selectizeInput(
    
    inputId  = inputId, 
    label    = inputLabel,
    choices  = NULL, 
    selected = selected,
    multiple = multiple,
    width    = width,
    options  = list( 
      placeholder = 'Select a disease', 
      "options"    = options,
      "valueField" = "value", 
      "labelField" = "label",
      "render"     = I(render),
      "items"      = as.list(selected)
    )
  )
  attachDependencies(widget, fa_html_dependency(), append = TRUE)
}
ui <- fluidPage(
  br(),
  selectInputWithIcons(
    "slctz",
    "Select an animal:",
    labels    = c("Stroke", "Coronary Heart Disease"),
    values    = c("stroke" ,'CHD'),
    icons     = list( c("smoking", "burger",'utensils',"weight", "wine-glass"),
                      c("smoking", "burger",'utensils',"weight", "wine-glass")),
    iconStyle = "font-size: 3rem; vertical-align: middle;",
    selected  = NULL
  )
)


server <- function(input, output, session){
  
  observe({
    print(input[["slctz"]])
  })
  
}

# shinyApp(ui, server)
