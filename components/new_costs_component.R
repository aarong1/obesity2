cost_component <- function(
                           prescription_costs = '£8,400,000',
                           screening_costs = '£750,000',
                           total_costs = '£9,150,000',
                           
                           stroke_follow_savings = '£2,500,000',
                           stroke_initial_savings = '£2,800,000',
                           total_savings = '£5,300,000',
    
                           nmb = '- £3,850,000',
                           roi = '£0.42 back per £1 spent',
                           qalys_gained = '896',
                           icer = '10,212.05 £/QALY'
                           ){


div(style='display:flex;font-family:Avenir;flex-direction:column;gap:0px;',
    div(style='border-bottom:solid 2px;',
      h3(style='float:left;','Money flow'),
      h3(style='float:right;','Cost')),
br(),br(),
    ######## ######## ######## ######## ######## ######### ####### ######## ######## ######## ######## ########
  div(style='display:flex;justify-content:space-between;padding-left:13px;',
  p( 'Screening Costs'), 
    p(screening_costs)
  ),
 
  ######## ######## ######## ######## ######## ######### ####### ######## ######## ######## ######## ########
  div(style='display:flex;justify-content:space-between;padding-left:13px;',
  p( 'Prescription Costs'), 
    p(prescription_costs)
  ),
 
  ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ######## ########
 div(style='display:flex;justify-content:space-between;font-size:15px;', 
     
    p('Total Costs',style = 'font-weight:bold'),
    p(total_costs,style = 'font-weight:bold')
 ),
br(),br(),
######## ######## ######## ######## ######## ####### ######### ######## ######## ######## ######## ########
     div(style = 'display:flex;justify-content:space-between;padding-left:13px;',
 p('First Year Stroke Savings'),
 p(stroke_initial_savings) 
 ),
 
  ######## ######## ######## ######## ######## ####### ######### ######## ######## ######## ######## ########
     div(style = 'display:flex;justify-content:space-between;padding-left:13px;',
 p('Subsequent Year Stroke Savings'),
 p(stroke_follow_savings) 
 ),
 
 ######## ######## ######## ######## ######## ####### ######### ######## ######## ######## ######## ########
     div(style = 'display:flex;justify-content:space-between;font-size:15px;',
 p('Total Savings',style = 'font-weight:bold'),
 p(total_savings,style = 'font-weight:bold') 
 ),
br(),br(),
 
 ######## ######## ######## ######## ######## ######### ####### ######## ######## ######## ######## ########
  div(style='display:flex;justify-content:space-between;font-size:15px;',
  p( 'Net Monetary benefit'), 
    p(nmb, style='color:#FF474C;font-weight:bold;' )
  ),
 
 ######## ######## ######## ######## ######## ######### ####### ######## ######## ######## ######## ########
  div(style='display:flex;justify-content:space-between;font-size:15px;',
  p(style = 'text-align:start;', 'Return On Investment'), 
    p(roi, style='color:#FF474C;font-weight:bold;text-align:end;' )
  ),

br(),br(),
    ######## ######## ######## ######## ######## ######### ####### ######## ######## ######## ######## ########
  div(style='display:flex;justify-content:space-between;padding-left:13px;',
  p( 'QALYs gained'), 
    p(qalys_gained, style = 'font-weight:bold;')
  ),
 ######## ######## ######## ######## ######## ######### ####### ######## ######## ######## ######## ########
  div(style='display:flex;justify-content:space-between;font-size:15px;',
  p(style = 'font-weight:bold;text-align:start;' ,'Incremental Cost Effectiveness Ratio'), 
    p(icer, style='color:mediumseagreen;font-weight:bold;text-align:end;')
  )
   )
}

browsable(fluidPage( cost_component()))

