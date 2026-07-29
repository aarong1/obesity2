#https://cdn.jsdelivr.net/npm/coverflow@1.3.0/index.min.js
#https://cdnjs.com/libraries/jquery.flipster
#https://www.jqueryscript.net/demo/Responsive-Image-Cover-Flow-Plugin-with-jQuery-CSS3-flipster/
#https://newristics.com/assets/dependencies/jquery-flipster-master/demo/#demo-default
#https://coverflowjs.github.io/coverflow/tutorial/get-started/
#https://www.reddit.com/r/web_design/comments/1cuvfd/my_quick_attempt_to_recreate_the_itunes_store/
library(shiny)
addResourcePath("www", ".")


ui <- page_fluid(
  tags$head(
  #js ----
  HTML('<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery.flipster/1.1.6/jquery.flipster.min.js" integrity="sha512-IGPfWH/x5mAD5FzAQQ1fomCSHKymvEDf8W9uJyV+8bVjzIHwUAPuEkyxRZZrw5M35jFfkNeDELOiGzAcCUxVCA==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>'),
  HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/jquery.flipster/1.1.6/jquery.flipster.min.css" integrity="sha512-0OFs2tfng6J7BuNGgMTJoZEVBWsaBjWyz4a1p2uW34znd26WG2rwvIf8S+UZ79BXC7ewc0O9h5WSQqkujzAHww==" crossorigin="anonymous" referrerpolicy="no-referrer" />'),
  tags$script("$(function(){ $('.flipster').flipster(); });"),
  tags$style(" img {
  width:500px;
  overflow:hidden;

  border: double 0.5em transparent;
  border-radius: 10px;
  background-image: linear-gradient(white, white), 
                    linear-gradient(to right,  rgb(85,172,188), gold,  rgb(56,75,123));
  background-origin: border-box;
  background-clip: content-box, border-box;
  }
  li{
    height:400px;
    }"
  )),
  
  
  #html ----
  HTML('
 <div class="flipster p-3">
    <ul>
<li><img src="www/pubmed.ncbi.nlm.nih.gov_23422444_.png"/></li>
<li><img src="www/pubmed.ncbi.nlm.nih.gov_32015079_.png"/></li>
<li><img src="www/stroke.png"/></li>
<li><img src="www/www.thelancet.com_journals_lanhl_article_PIIS2666-7568(21)00146-X_fulltext.png"/></li>
    </ul>
</div>
<
')
)

#<link rel="stylesheet" type="text/css" href="coverflow.css" />

# <li><img src="https://i.imgur.com/x3wSoFU.jpeg" referrerpolicy="no-referrer"/></li>
# <li><img src="https://i.imgur.com/27fTqbA.jpeg" referrerpolicy="no-referrer"/></li>
# <li><img src="https://i.imgur.com/RjdFV6n.jpeg" referrerpolicy="no-referrer"/></li>
# <li><img src="https://i.imgur.com/6W8JOza.jpeg" referrerpolicy="no-referrer"/></li>
# <li><img src="https://i.imgur.com/rwLY1JH.jpeg" referrerpolicy="no-referrer"/></li>


# shinyApp(ui,function(input, output, session){})

