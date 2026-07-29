div_cards <-
  function(text_colour = 'white',
           background_colour = 'lightgreen',
           title= 'title',
           text = 'Design, validate and optimize new processes and facilities before spending time and money',
           icon=NULL,
           class=NULL,
           border_colour='rgb(45,45,45)',
           icon_class=NULL
           ) {
    
    div(class = paste('info_card',class),
      style = paste0(
        'border:solid ',
        border_colour,
        ' 5px; border-radius:15px;background-color:',
        background_colour,
        ';color:',
        text_colour,
        ';padding:10px;width:50%;'
      ),
      if(!is.null(icon)){ icon(class= icon_class,style = paste0('float:right;font-size:20px;padding:3px;color:',background_colour),icon)}else{NULL},#color:',background_colour
      h4(title),
      p(text)
    )
  }

browsable(bslib::page_fluid(div_cards(icon='arrow-up')))
