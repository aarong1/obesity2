declare_box <- function(title='Prescriptions',  value= '£ 300, 000',second=' ' , color='white', background='lightgreen', border){#rgb(255,100,100)
  if(missing(border)){
    border = background}
  
  div(class = 'd-inline-block',
  div(style = paste0('display:flex;
      flex-direction:column;
      color:',color,';
      align-content:space-around;
      padding:10px;
      margin:5px;
  background-color:',background,';border: solid ',border,' 3px;border-radius:15px;'),
    div(h5(title, contenteditable = FALSE),
    h6(second) ,
    h4(style = 'margin-left:10px;',value, contenteditable = TRUE)
  )
  )
  )
}

browsable(fluidPage(
  div(style = 'display:flex;flex-direction:column;',
  div(style = 'display:flex;flex-direction:row;justify-content:baseline;',
declare_box('Discounting Rate', '3.5 %'),
declare_box('post stroke utility', 0.5),
declare_box('Screening cost', '£5' ),
declare_box('Prescription Costs','£ 80','per person per year'),
  div(style = 'display:flex;flex-direction:column;',

declare_box('Cost of a Stroke','£ 48,000','first year'),
declare_box('£ 24,000', '', second ='Subsequently')
)

#warfarin £24 -DOACs £800

),

  div(style = 'display:flex;flex-direction:row;',

declare_box('Average normal LE of Stroke victims',color='rgb(50,50,50)',background='transparent', '8 years',border='lightgreen'),
declare_box('Average LE wo Stroke', '20 years',background='rgb(85,172,189)',second='model')
),

#   div(style = 'display:flex;flex-direction:column;',
# 
# declare_box('Cost', '£24,399,021', 'discounted'),
# declare_box('Savings', '£3,360,000'),
# ),
#   div(style = 'display:flex;flex-direction:column;',
# 
# declare_box('net monetary benefit wQALYs','£ 7,970,439',second = '**QALY priced at £60'),
# declare_box('net monetary benefit wo QALYs', '£ 4,610,439'),
# declare_box('Return on Investment', '£ 4,610,439'),
# declare_box('ICER', '17,857.14 £/QALY')
# 
# 
# )
)

  ))

################     Parameters from referenced literature    #################
x=read.csv( file =
textConnection(object =
'year,  delta
2016,     0
2017,    -7
2018,    -6
2019,    -5
2020,     1
2021,    -8
2022,   -22
2023,   -18')
)

strokes <- c(2016,
2017,
2018,
2019,
2020,
2021,
2022,
2023)

averted <- data.frame(strokes, averted = 20)



