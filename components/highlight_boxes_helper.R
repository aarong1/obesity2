css <- "<head>
	<meta charset='UTF-8'>
	
	<title>Slide-in Image Boxes</title>
	
	<link rel='stylesheet' href='css/style.css'>
	
	<style>

    #footer-boxes {
        display:flex;
        flex-direction:row;
        flex-wrap:wrap;
        gap:5px;
    }
    
    .footer-box {
        border-radius:5px;
        margin:0 0 10px 0;
        display:inline-block;
        width:222px;
        height:140px;
        padding:15px;
        background:rgb(240,240,240); 
        color:rgb(220,220,220);
        -webkit-transition:all 0.2s ease;
        -moz-transition:all 0.2s ease;
        text-decoration: none;
        text-decoration: none;
    }

    .footer-box:hover h5 {
        text-shadow:0 0 4px rgba(0,0,0,0.4);
        color:white;
    }
    
    .footer-box:hover p {
        color:white;
    }
    
    .footer-box p {
        font-size:12px;
        width:175px;
        line-height:1.5;
    }

  
    #f-diw:hover {
        background-color:#3fb1e3;
        background-position:186px -1288px;
    }
    #f-qod:hover {
        background-color:#6be6c1;
        background-position:186px -1448px;
    }
    #f-htmlipsum:hover {
        background-color:#626c91;
        background-position:186px -1608px;
    }
    #f-qod:hover p {
        color:grey;
    }
    #f-bookshelf:hover {
        background-color:#a0a7e6;
        background-position:186px -1768px;
    }
    #f-html-ipsum:hover p {
        color:#fff8da;
    }
    #f-twitter:hover {
        background-color:#4ed2fe;
        background-position:186px -1928px;
    }
    #f-forrst:hover {
        background-color:#203f16;
        background-position:186px -2088px;
    }
    #f-forrst:hover p {
        color: #92c59c;
    }
	
	</style>
</head>"; div(
  HTML(css),
div( id='demo-top-bar',
  
  div( id='demo-bar-inside',
    
   h2( id='demo-bar-badge',
      
        ),
        
        div( id='demo-bar-buttons',
          )
          
          )
          
          ),
          div( id='page-wrap',
            
            div( class='group',
                 id='footer-boxes',
                 div(class = 'footer-box', id='f-diw',
                h5('DigWP'),
                p('A book and blog co-authored by Jeff Starr and myself about the World\'s most popular publishing platform.'
                )
      ),
      div(class = 'footer-box', id='f-htmlipsum',
          
        h5('Quotes on Design'),
        p(
        'Design, like Art, can be an elusive word to define and an awfully fun thing to have opinions about.'
        )
      ),
      div(class = 'footer-box',id='f-qod',
         
    
          
        h5('HTML-Ipsum'),
        p(
        'One-click copy to clipboard access to <em,Lorem Ipsum</em, text that comes wrapped in a variety of HTML.'
        )
      ),
      div(class = 'footer-box',      id='f-bookshelf',
        h5('Bookshelf'),
        p(
        'Hey Chris, what books do you recommend? These, young fertile mind, these.'
        )
      )
        )
        
        ) 
)|> page_fluid() |> browsable()
