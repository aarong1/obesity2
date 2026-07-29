


#########################################
#navigation cards
########################################
metric_card_html <- function( top ='top', 
                              change = 'change', 
                              text ='text',
                              change_icon =  '',
                              color = 'red',
                              opacity='opacity-75'){
  
  change_class = if(change_icon=='negative'){  "fa-arrow-down me-1"
  }else if(change_icon=='negative'){ "fa-arrow-up me-1"
  }else{''}
  
  color_class = case_when(color == '#8F00FF' ~ 'theme-purple',
                          color == 'teal' ~ 'theme-teal',
                          color == 'steelblue' ~ 'theme-teal',
                          
                          
                          color == '#dc3545' ~ 'theme-red', 
                          color == 'mediumseagreen' ~ 'theme-green'
  )
  
  
  div(class = paste("grid-item grid-item--small",opacity),
      div(class = "grid-item-content",
          div(class = "metric-card",
              #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
              div(class = "metric-value", style = paste("color:",color), top),
              div(class = "metric-label", text),
              div(class = paste("metric-change", change_icon),
                  tags$i(class = change_class, change)
              )
          )
      )
  )
}

metric_cards1 <- function(top,text,change,change_icon = NULL,change_class=NULL,
                          color = 'mediumseagreen',
                          opacity = 'opacity-75'){
  div(class = paste("",opacity),
      
      #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
      div(class = "", style = paste("color:",color), format(top,big.mark = ',',digits=3)),
      div(class = "", text),
      div(class = paste("", change_icon),
          tags$i(class = change_class, change)
      )
  )
}


metric_card <- function( top ='top', 
                         change = 'change', 
                         text ='text',
                         change_icon =  '',
                         color = 'red',
                         opacity='opacity-50'){
  
  change_class = if(change_icon=='negative'){  "fa-arrow-down me-1"
  }else if(change_icon=='negative'){ "fa-arrow-up me-1"
  }else{''}
  
  color_class = case_when(color == '#8F00FF' ~ 'theme-purple',
                          color == 'teal' ~ 'theme-teal',
                          color == 'steelblue' ~ 'theme-teal',
                          
                          
                          color == '#dc3545' ~ 'theme-red', 
                          color == 'mediumseagreen' ~ 'theme-green'
  )
  
  
  div(class = paste("grid-item grid-item--small",opacity),
      div(class = "grid-item-content",
          div(class = "metric-card",
              #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
              div(class = "metric-value", style = paste("color:",color), format(top,big.mark = ',',digits=3)),
              div(class = "metric-label", text),
              div(class = paste("metric-change", change_icon),
                  tags$i(class = change_class, change)
              )
          )
      )
  )
}

