library(shiny)

ui <- page_fluid(
  tags$head(
  #js ----
  HTML("<script type = 'text/javascript' >
  /* Create an array to hold the different image positions */
var itemPositions = [];
var numberOfItems = $('#scroller .item').length;

/* Assign each array element a CSS class based on its initial position */
function assignPositions() {
    for (var i = 0; i &lt; numberOfItems; i++) {
        if (i === 0) {
            itemPositions[i] = 'left-hidden';
        } else if (i === 1) {
            itemPositions[i] = 'left';
        } else if (i === 2) {
            itemPositions[i] = 'middle';
        } else if (i === 3) {
            itemPositions[i] = 'right';
        } else {
            itemPositions[i] = 'right-hidden';
        }
    }
    /* Add each class to the corresponding element */
    $('#scroller .item').each(function(index) {
        $(this).addClass(itemPositions[index]);
    });
}

/* To scroll, we shift the array values by one place and reapply the classes to the images */
function scroll(direction) {
console.log('scroll');
    if (direction === 'prev') {
        itemPositions.push(itemPositions.shift());
    } else if (direction === 'next') {
        itemPositions.unshift(itemPositions.pop());
    }
    $('#scroller .item').removeClass('left-hidden left middle right right-hidden').each(function(index) {
        $(this).addClass(itemPositions[index]);
    });        
}

/* Do all this when the DOMs ready */
$(document).ready(function() {

    assignPositions();
    var autoScroll = window.setInterval('scroll('next')', 4000);
  
    /* Hover behaviours */
    $('#scroller').hover(function() {
        window.clearInterval(autoScroll);
        $('.nav').stop(true, true).fadeIn(200);
    }, function() {
        autoScroll = window.setInterval('scroll('next')', 4000);
        $('.nav').stop(true, true).fadeOut(200);
    });

    /* Click behaviours */
    $('.prev').click(function() {
        scroll('prev');
    });
    $('.next').click(function() {
        scroll('next');
    });

});
</script>"),
  
  #css -----
  tags$style("
  html, body {
  height: 100%;
  margin: 0;
}

body {
  background: -webkit-linear-gradient(top, #4D4D4D 0, #4D4D4D 180px, #939393 400px);                
}

.warning {
  margin: 10px auto 0;
  width: 500px;
  text-align: center;
  font-size: 20px;
}

#scroller {
  width: 500px;
  height: 200px;
  margin: 0 auto;
  padding: 50px 0;
  -webkit-perspective: 500px;
  -moz-perspective: 500px;
  -o-perspective: 500px;
}

#scroller .item {
  width: 500px;
  display: block;
  position: absolute;
  border-radius: 10px;
  -webkit-box-reflect: below 0px -webkit-gradient(linear, left top, left bottom, from(transparent), color-stop(.85, transparent), to(rgba(255,255,255,0.15)));
  -webkit-transition: all 0.4s ease-in-out;
  -moz-transition: all 0.4s ease-in-out;
  -o-transition: all 0.4s ease-in-out;
  z-index: 0;
}

/* Since inset shadows dont play nice with images, well create a pseudo element and apply our image styling to that instead */
#scroller .item:before {
  border-radius: 10px;
  width: 500px;
  display: block;
  content: ' ';
  position: absolute;
  width: 100%;
  height: 100%;
  box-shadow: inset 0 0 0 1px rgba(255,255,255,0.3), 0 0 0 1px rgba(0,0,0,0.4);
}

#scroller .item img {
  display: block;
  border-radius: 10px;
}

#scroller .left {
  -webkit-transform: rotateY(25deg) translateX(-320px) skewY(-5deg) scale(0.4, 0.6);
  -moz-transform: rotateY(25deg) translateX(-320px) skewY(-5deg) scale(0.4, 0.6);
  -o-transform: rotateY(25deg) translateX(-320px) skewY(-5deg) scale(0.4, 0.6);
}

#scroller .middle {
  z-index:1;
  -webkit-transform: rotateY(0deg) translateX(0) scale(1);
  -moz-transform: rotateY(0deg) translateX(0) scale(1);
  -o-transform: rotateY(0deg) translateX(0) scale(1);
}

#scroller .right {
  -webkit-transform: rotateY(-25deg) translateX(320px) skewY(5deg) scale(0.4, 0.6);
  -moz-transform: rotateY(-25deg) translateX(320px) skewY(5deg) scale(0.4, 0.6);
  -o-transform: rotateY(-25deg) translateX(320px) skewY(5deg) scale(0.4, 0.6);
}

#scroller .left-hidden {
  opacity: 0;
  z-index: -1;
  -webkit-transform: rotateY(25deg) translateX(-430px) skewY(-5deg) scale(0.3, 0.5);
  -moz-transform: rotateY(25deg) translateX(-430px) skewY(-5deg) scale(0.3, 0.5);
  -o-transform: rotateY(25deg) translateX(-430px) skewY(-5deg) scale(0.3, 0.5);
}

#scroller .right-hidden {
  opacity: 0;
  z-index: -1;
  -webkit-transform: rotateY(-25deg) translateX(430px) skewY(5deg) scale(0.3, 0.5);
  -moz-transform: rotateY(-25deg) translateX(430px) skewY(5deg) scale(0.3, 0.5);
  -o-transform: rotateY(-25deg) translateX(430px) skewY(5deg) scale(0.3, 0.5);
}

.nav {
  position: absolute;
  width: 500px;
  height: 30px;
  margin: 170px 0 0;
  z-index: 2;
  display: none;
}

.prev, .next {
  position: absolute;
  display: block;
  height: 30px;
  width: 30px;
  background-color: rgba(0,0,0,0.85);
  border-radius:15px;
  color: #E4E4E4;
  bottom: 15px;
  left: 15px;
  text-align: center;
  line-height: 26px;
  cursor: pointer;
  box-shadow: inset 0 0 0 1px rgba(255,255,255,0.5), 0 0 0 1px rgba(0,0,0,0.7);
}

.next {
  left: inherit;
  right: 15px;
}

.prev:hover, .next:hover {
  box-shadow: inset 0 0 0 2px rgba(255,255,255,0.5), 0 0 0 1px rgba(0,0,0,0.7);                
}") ) ,
  
  #html ----
  HTML('<p class="warning">Only works 100% in Chrome for now ;)</p>
<div id="scroller">
  <div class="nav">
    <a class="prev">&laquo;</a>
    <a class="next">&raquo;</a>
  </div>
  <a class="item" href="#"><img src="https://i.imgur.com/5Mk3EfW.jpeg" referrerpolicy="no-referrer"/></a>
  <a class="item" href="#"><img src="https://i.imgur.com/79aU67L.jpeg" referrerpolicy="no-referrer"/></a>
  <a class="item" href="#"><img src="https://i.imgur.com/x3wSoFU.jpeg" referrerpolicy="no-referrer"/></a>
  <a class="item" href="#"><img src="https://i.imgur.com/27fTqbA.jpeg" referrerpolicy="no-referrer"/></a>
  <a class="item" href="#"><img src="https://i.imgur.com/RjdFV6n.jpeg" referrerpolicy="no-referrer"/></a>
  <a class="item" href="#"><img src="https://i.imgur.com/6W8JOza.jpeg" referrerpolicy="no-referrer"/></a>
  <a class="item" href="#"><img src="https://i.imgur.com/rwLY1JH.jpeg" referrerpolicy="no-referrer"/></a>
</div>
')
)


shinyApp(ui,function(input, output, session){})

