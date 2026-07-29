contact_bar_black <- function(){
  
  div(id='show', style = 'text-decoration:none;color:rgb(78,244,251);margin:20px;', #visibility:none;
      div(style='text-decoration:none;display:flex;justify-content:space-around;align-items:center;padding:10px;background-color:rgb(45,45,45);border-radius:15px;padding-inline:30px;', #color:rgb(85,172,189);rgb(56,77,124)
      
      div(
        icon('heart',
               style='display:inline;color:rgb(78,244,251);font-size:16px;'),
      p('Population Health Model',
        style='display:inline-block;color:white;font-size:small;')
      ),
            div(img(style= 'width:90px;',src = 'img/pha_logo_0.png')),
     
             div(img(style= 'width:170px;',src = 'img/inverse_logo.png'),

            #   p('Contact',style = 'color:white;')
             ),
     
      div(style='margin-inline:10px;',#'font-size:15px;',
   
              p(style='display:inline-block;color:white;margin-left:40px;font-size:small;','Contact'),
          
              
     
     div(style='margin-inline:10px;',#'font-size:15px;',
            a(href='mailto:aaron.gorman@hscni.net',
              target = '_blank',
              icon('envelope',style='display:inline-block;margin-left:50px;'),
              p(style='display:inline-block;','Developer           '),
              style='text-decoration:none;display:inline-block;color:white;font-size:x-small;')),

      div(style='margin-inline:10px;',
            a(href='mailto:paul.mcwilliams@hscni.net',
              target = '_blank',
              icon('envelope',style='display:inline;color:rgb(78,244,251);margin-left:50px;'),
              p(style='display:inline-block;','Digital Ownership') 
             ,style='text-decoration:none;display:inline-block;color:rgb(78,244,251);font-size:x-small;')
            ),

       div(style='margin-inline:10px;',
            a(href='mailto:declan.bradley@hscni.net', 
              target = '_blank',
              icon('envelope',style='display:inline-block;color:rgb(78,244,251);margin-left:50px;'),#rgb(85,172,189)
              p(style='display:inline-block;','Strategic PHA Ownership'),
              style='text-decoration:none;display:inline-block;color:rgb(78,244,251);font-size:x-small;')
           )
     # div(style='margin-left:20px;color:rgb(85,172,189);display:flex;align-items:center;justify-content:start;',
     #      img(src='img/ui_folder_family.svg',height='30px'),
     #    p('Digital',style='display:inline-block;color:rgb(85,172,189);font-size:medium;font-weight:bold;margin-left:5px;'),
     #    p('Twin',style='display:inline-block;color:rgb(85,172,189);font-size:medium;font-weight:bold;margin-left:5px;')
     #    ) 

      )
  )
)
}

browsable(contact_bar_black())
