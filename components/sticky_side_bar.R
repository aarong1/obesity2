sticky_side_bar <- function() {
  div(style = 'position:sticky; top:8%; height:90vh;',
      div(class = 'py-2 px-5 me-5 rounded-5 position-relative',
          
          span(class="badge bg-info rounded-2 rounded-pill",
               style="top:-15px; left:10px; position:absolute;",
               h6(style="text-align:center;color:white;", 'Intervention Summary')
          ),
          
          div(class = '',
              # 'alert alert-success',
              style = "background-color:#f5e8c8; justify-content:center;border-radius:25px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
              tags$small(class='mb-1 text-white text-center','Emergency Admissions'),
              h3(textOutput('side_emergency_admissions') ),
              tags$small(class = 'text-centre text-center','Estimated from morbidity on inpatient data'),
              tags$h4(class = 'mt-3 text-white text-centre text-center','by intervention end')
          ),
          
          div(
            style = "background:white;border:5px solid #e01f54;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
            tags$small(class='text-muted','Actual Cost'),
            h4(textOutput('side_cost') ),
            # tags$small(class='text-muted text-black text-center','Pre-planning Pipeline'),
            # tags$div(class='h3 text-black text-centre text-center','updated from estimate')
          ),
          
          div(
            style = "background:white;border:5px solid #e01f54;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:10px auto;",
            tags$small(class='text-muted', 'Multimorbdity'),
            h3(textOutput('side_multimorbidity') ),
            tags$small(class='text-muted text-centre text-center',' by intervention end')
          ),
          
          
          
          div(class = 'shadow-sm',
              style = "background:white;border:5px solid #001852;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
              tags$small(class='text-muted', 'DALYs'),
              h4(textOutput('side_dalys') ),
              tags$small(class='text-muted text-centre text-center','by intervention end')
          ),
          
          # div(class = 'alert bg-info',  h6('DALYS'), h4('456,454') ),
          # div(class = 'alert bg-primary',  h6('DALYS'), h4('456,454') ),
          # circular_value('45,324')
      )
  )
}
