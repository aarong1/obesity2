  #https://semantic-ui.com/views/statistic.html
  library(shiny)
  library(htmltools)
  
  ui <- function(color = 'black'){
  HTML(paste0('
  <head>
  <style>
  body{
  
  color:',color,';
  background-color:transparent;
  
  }
  
  .statistics{
  padding:20px;
  display:flex;
  flex-direction:row;
  justify-content:space-around;
  align-items:center;
  flex:1;
  gap:5rem;
  }
  
  .icons{
  padding:10px;
  font-size:20px;
  display:flex;
  flex-direction:row;
  justify-content:space-around;
  
  }
  
  .statistic{
  flex:1;
  display:flex;
  flex-direction:column;
  text-align:center;
  justify-content:end;
  
  
  
  }
  
    
  .value{
  
  text-transform: uppercase;
  font-weight:bold;
  font-size: clamp(23px, 10vw, 30px); /* Adjusts dynamically */
  /* text-wrap: balance;
  white-space: nowrap;*/
  overflow: hidden;
  text-overflow: ellipsis;
  
  }
  
  .value-lg{
  
  text-transform: uppercase;
  font-weight:bold;
  font-size: clamp(23px, 10vw, 50px); /* Adjusts dynamically */
  /* text-wrap: balance;
  white-space: nowrap; */
  overflow: hidden;
  text-overflow: ellipsis;
  
  }
  
  .lab{
  
  text-transform: uppercase;
  font-size:1.5rem;
  
  }
  
  </style>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.2/js/all.min.js" integrity="sha512-b+nQTCdtTBIRIbraqNEwsjB6UvL3UEMkXnhzd8awtCYh0Kcsjl9uEgwVFVbhoj3uu1DO1ZMacNvLoyJJiNfcvg==" crossorigin="anonymous" referrerpolicy="no-referrer"></script>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.2/css/all.min.css" integrity="sha512-Evv84Mr4kqVGRNSgIGL/F/aIDqQb7xQ2vcrdIwxfjThSH8CSR7PBEakCr51Ck+w+/U6swU2Im1vVX0SVk9ABhg==" crossorigin="anonymous" referrerpolicy="no-referrer" /></head>
  
  <div class="statistics">
    <div class="statistic">
    
      <div class="value-lg">
        3
        <i class="fa-solid fa-layer-group"></i>
        <!--<img style = "height:50px;" src = https://cdn1.iconfinder.com/data/icons/basic-ui-vol-8-32-px/32/ui-layer-level-tier-sheet-512.png /> -->
      </div>
      
        <div class="lab">
          Competing Morbidities
        </div>
        
          </div>
          
          <div class="statistic">
            <div class="text value">
              Twenty Three<br>
             
            </div>
              <div class="lab">
                Risk Factors
              </div>
                </div>
                
                <div class="statistic">
                  <div class="value">
                  
                   <!--  <i class="plane icon"></i> -->
                   4
                    </div>
                    <div class = "icons">
                         <i class="fa-solid fa-user-doctor"></i>
                  <div><i class="fa-solid fa-lightbulb"></i>
                   <i class="fa-solid fa-keyboard"></i>
                   </div>
                  <!-- <i class="fa-solid fa-binoculars"></i> -->
                  <i class="fa-solid fa-book-open-reader"></i>
                  <i class="fa-solid fa-building-columns"></i>
                  </div>
                      <div class="lab">
                        Expert Groups
                      </div>
                        </div>
                        <div class="statistic">
                          <div class="value" style = "position:relative;" >
                            <!-- <img src="/images/avatar/small/joe.jpg" class="ui circular inline image"> -->
                            
                            <img style = "z-index:-1;position:absolute;top:30px;left:0px;display: inline;max-width:230px;max-height:45px;width: auto;height: auto;" src = "https://cdn.who.int/media/images/default-source/access-to-medicines/icon__19.png?sfvrsn=a023da42_3&Status=Master">
  
  Lifecycle Costs
  
  
                            </div>
                              <div class="lab" >
                                Interventions
                              </div>
                                </div>
                                </div>'))
  }
  
  
  browsable(fluidPage (ui()))
  #server <- function(input,output, session){}
  
  #ShinyApp(x, server)
  
  
