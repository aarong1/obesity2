# https://css-tricks.com/examples/SlideInImageBoxes/
  
  
  
 HTML("<html>

<head>
	<meta charset='UTF-8'>
	
	<title>Slide-in Image Boxes</title>
	
	<link rel='stylesheet' href='css/style.css'>
	
	<style>

    footer {
        clear:both;
        overflow:hidden;
        font-size:16px;
        line-height:1.3;
    }
    #footer-boxes {
        -moz-column-count:2;
        -moz-column-gap:10px;
        -webkit-column-count:2;
        -webkit-column-gap:10px;
        column-count:4;
        column-gap:10px;
    }
    .footer-box {
        margin:0 0 10px 0;
        display:inline-block;
        width:262px;
        height:140px;
        padding:15px;
        background:#e6e2df;
        color:#b2aaa4;
        -webkit-transition:all 0.2s ease;
        -moz-transition:all 0.2s ease;
        background-position:320px 50%;
        background-repeat:no-repeat;
        text-decoration: none;
    }
    .footer-box h5 {
        font: bold 24px Sans-Serif !important;
        text-transform:uppercase;
        font-size:38px;
        line-height:1;
        padding:0 0 10px 0;
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
    .footer-box:hover {
        background-position:200px 50%;
    }
    #f-diw {
        background-image:url(http://cdn.css-tricks.com/wp-content/themes/CSS-Tricks-8/images/css-tricks.png);
        background-position:290px -1288px;
    }
    #f-diw:hover {
        background-color:#237abe;
        background-position:186px -1288px;
    }
    #f-qod {
        background-image:url(http://cdn.css-tricks.com/wp-content/themes/CSS-Tricks-8/images/css-tricks.png);
        background-position:290px -1448px;
    }
    #f-qod:hover {
        background-color:#37597a;
        background-position:186px -1448px;
    }
    #f-htmlipsum {
        background-image:url(http://cdn.css-tricks.com/wp-content/themes/CSS-Tricks-8/images/css-tricks.png);
        background-position:290px -1608px;
    }
    #f-htmlipsum:hover {
        background-color:#333333;
        background-position:186px -1608px;
    }
    #f-qod:hover p {
        color:#adbde3;
    }
    #f-bookshelf {
        background-image:url(http://cdn.css-tricks.com/wp-content/themes/CSS-Tricks-8/images/css-tricks.png);
        background-position:290px -1768px;
    }
    #f-bookshelf:hover {
        background-color:#ff8400;
        background-position:186px -1768px;
    }
    #f-html-ipsum:hover p {
        color:#fff8da;
    }
    #f-twitter {
        background-image:url(http://css-tricks.com/images/css-tricks.png);
        background-position:290px -1928px;
    }
    #f-twitter:hover {
        background-color:#4ed2fe;
        background-position:186px -1928px;
    }
    #f-forrst {
        background-image:url(http://css-tricks.com/images/css-tricks.png);
        background-position:290px -2088px;
    }
    #f-forrst:hover {
        background-color:#203f16;
        background-position:186px -2088px;
    }
    #f-forrst:hover p {
        color: #92c59c;
    }
	
	</style>
</head>

<body>
  
<div id='demo-top-bar'>

  <div id='demo-bar-inside'>

    <h2 id='demo-bar-badge'>
      <a href='/'>CSS-Tricks Example</a>
    </h2>

    <div id='demo-bar-buttons'>
          </div>

  </div>

</div>
	<div id='page-wrap'>
	
		<div class='group' id='footer-boxes'>
      <a href='http://digwp.com' id='f-diw' class='footer-box'>
      <h5>DigWP</h5>
      <p>
          A book and blog co-authored by Jeff Starr and myself about the World's most popular publishing platform.
      </p>
        </a><a href='http://quotesondesign.com' id='f-qod' class='footer-box'>
        <h5>Quotes on Design</h5>
        <p>
        Design, like Art, can be an elusive word to define and an awfully fun thing to have opinions about.
      </p>
        </a><a href='http://html-ipsum.com' id='f-htmlipsum' class='footer-box'>
        <h5>HTML-Ipsum</h5>
        <p>
        One-click copy to clipboard access to <em>Lorem Ipsum</em> text that comes wrapped in a variety of HTML.
      </p>
        </a><a href='/bookshelf/' id='f-bookshelf' class='footer-box last'>
        <h5>Bookshelf</h5>
        <p>
        Hey Chris, what books do you recommend? These, young fertile mind, these.
      </p>
        </a>
        </div>
        
        </div>
        
        <style type='text/css' style='display: none !important;'>
        * {
          margin: 0;
          padding: 0;
        }
      body {
        overflow-x: hidden;
      }
      #demo-top-bar {
      text-align: left;
      background: #222;
        position: relative;
      zoom: 1;
      width: 100% !important;
      z-index: 6000;
      padding: 20px 0 20px;
      }
#demo-bar-inside {
width: 960px;
margin: 0 auto;
position: relative;
overflow: hidden;
}
#demo-bar-buttons {
padding-top: 10px;
float: right;
}
#demo-bar-buttons a {
font-size: 12px;
margin-left: 20px;
color: white;
margin: 2px 0;
text-decoration: none;
font: 14px 'Lucida Grande', Sans-Serif !important;
}
#demo-bar-buttons a:hover,
#demo-bar-buttons a:focus {
text-decoration: underline;
}
#demo-bar-badge {
display: inline-block;
width: 302px;
padding: 0 !important;
margin: 0 !important;
background-color: transparent !important;
}
#demo-bar-badge a {
display: block;
width: 100%;
height: 38px;
border-radius: 0;
bottom: auto;
margin: 0;
background: url(/images/examples-logo.png) no-repeat;
background-size: 100%;
overflow: hidden;
text-indent: -9999px;
}
#demo-bar-badge:before, #demo-bar-badge:after {
display: none !important;
}
</style>
  </body>
  
  </html>") %>% 
   browsable()
 
 