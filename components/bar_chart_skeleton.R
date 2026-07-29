bar_chart_skeleton <- function(){
# bar_chart_skeleton(),
 
 #https://codepen.io/ecomrick77/details/dyXBXOM
  
  tagList(
  tags$head(tags$style(
   '.animate-pulse {
  animation: pulse 10.5s cubic-bezier(0.4, 0, 0.6, 1) infinite;
}

@keyframes pulse {
  0%, 100% {
    opacity: 0.1;
  }
  50% {
    opacity: 0.9;
  }
}' 
  )),
  
  HTML('<div style = "width:200px;height:100px;padding:20px 0px 0px 20px;;">
  <div class= "animate-pulse", style="border-radius:15px;display:flex;gap:5px;align-items:baseline;margin:10px;height:90%;width:90%;">
    <div style = "background-color:lightgrey;height:90%;width:5%" ></div>
    <div style = "background-color:lightgrey;height:80%;width:5%"  ></div>
    <div style = "background-color:grey;height:50%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:40%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:90%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:40%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:60%;width:5%"  ></div>
    <div style = "background-color:grey;height:90%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:80%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:70%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:80%;width:5%"  ></div>
    <div style = "background-color:lightgrey;height:90%;width:5%"  ></div>

  </div>
</div>')
  )
  
}