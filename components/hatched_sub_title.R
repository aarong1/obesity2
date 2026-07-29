

library(shiny)
library(htmltools)
library(bslib)
    
hatched_subtitle <- function(title = 'Analyst Ratings') {
      
      HTML(
        paste0(
    '<head>
    <style>
    
    .title{
    position:relative;
    margin:30px;
    }
    
    .title:before {
      content: "";
      display:block;
      position: absolute;
      top: 15px;
      left: 0px;
      width: 100%;
      height: 25px;
      border-radius:5px;
      background:repeating-linear-gradient(
    45deg,        /* 45-degree angle */
   #13b5cb,     /* Line colour */
    #13b5cb 5px, /* Line thickness */
    white 5px,    /* Gap between lines */
    white 12px    /* Total pattern width */
  );
  
      background-size: 50px 45px;
    }
      
      span.label{
          visibility: inherit;
      letter-spacing: 0.1em;
      text-transform: uppercase;
      color: rgb(100,100,100);
      box-sizing: inherit;
      border: 0;
      border-radius:15px;
      font: inherit;
      margin: 0;
      background: #fff;
      padding: 0px 10px 0px 10px;
      margin-left:20px;
      position: relative;
      display: inline-block;
      line-height: 1.5;}
      </style>
      </head>
    <div>
    <h3 class="title">
                  <span class="label">', title , '</span>
              </h3>
    </div>')
        )
  }

browsable(
  fluidPage(
    hatched_subtitle('ighv utcydrtuv')
  )
)
#### right click > copy> copy styles ####

# #13b5cb
# #3677a8

    # visibility: inherit;
    # box-sizing: inherit;
    # -webkit-tap-highlight-color: rgba(0,0,0,0);
    # padding: 0;
    # border: 0;
    # font: 800 .75rem/.85 Mulish,sans-serif;
    # position: relative;
    # display: inline-block;
    # letter-spacing: .025em;
    # text-transform: uppercase;
    # vertical-align: top;
    # width: 100%;
    # line-height: 1.375;
    # overflow: hidden;
    # color: #3677a8;
    # margin: 0;


browsable(
  fluidPage(
    hatched_subtitle('ighv utcydrtuv')
  )
)

library(shiny)
library(htmltools)

'background: repeating-linear-gradient(
  45deg,        /* 45-degree angle */
    white, /* #13b5cb,      Line colour */
    #13b5cb 3px, /* Line thickness */
    cyan 3px,    /* Gap between lines */ 
    white 18px    /* Total pattern width */
);
'
    
sm_hatched_subtitle <- function(title = 'Analyst Ratings') {
      
      div(
        singleton(HTML(
        paste0(
    '<head>
    <style>
    
    .sm_hatched_title{
    position:relative;
    margin:30px 0px 30px 0px;
    }
    
    .sm_hatched_title:before {
      content: "";
      display:block;
      position: absolute;
      top: 10px;
      left: 0px;
      width: 100%;
      height: 15px;
      border-radius:5px;
      background: repeating-linear-gradient(
      45deg,        
      lightcoral 0px 3px,
      white 2px 6px    /* Total pattern width */
  );
      background-size: 51px 40px;
    }
      
      span.sm_hatched_label{
          visibility: inherit;
      letter-spacing: 0.05em;
      text-transform: titlecase;
      color: rgb(100,100,100);
      box-sizing: inherit;
      border: 0;
      border-radius:15px;
      font: inherit;
      margin: 0;
      background: #fff;
      /*padding: 0px 10px 0px 10px;*/
      margin-left:20px;
      position: relative;
      display: inline-block;
      line-height:1;}
      </style>
      </head>'))
    ),
    div(
    div( class="sm_hatched_title",
                  span( class="sm_hatched_label p-1 fs-5", title )
    )
    
        )
      )
    
  }

browsable(
  page_fluid(    
    hatched_subtitle('ighv utcydrtuv'),
    sm_hatched_subtitle('ighv utcydrtuv')
  )
)
#### right click > copy> copy styles ####

# #13b5cb
# #3677a8

    # visibility: inherit;
    # box-sizing: inherit;
    # -webkit-tap-highlight-color: rgba(0,0,0,0);
    # padding: 0;
    # border: 0;
    # font: 800 .75rem/.85 Mulish,sans-serif;
    # position: relative;
    # display: inline-block;
    # letter-spacing: .025em;
    # text-transform: uppercase;
    # vertical-align: top;
    # width: 100%;
    # line-height: 1.375;
    # overflow: hidden;
    # color: #3677a8;
    # margin: 0;


browsable(
  fluidPage(
    hatched_subtitle('ighv utcydrtuv')
  )
)


