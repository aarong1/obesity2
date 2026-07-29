help_component <- function(){
  HTML('<head>

  <script>
  document.addEventListener("DOMContentLoaded", () => {

  
  const choiceArray = document.querySelectorAll(".choice")

choiceArray.forEach((card) => {
    card.addEventListener("click", () => {
        choiceArray.forEach((element) => {
            element.classList.remove("expand", "unset")
            element.classList.add("small")
        })
        card.classList.remove("small")
        card.classList.add("expand")  
    });
});

  });
</script>

  <style>
    .container {
      display: flex;
      width: 100%;
      padding: 0;
    }

    .choice {
      height: 120px;
      box-sizing: border-box;
      padding: 0;
      overflow: hidden;
      float: left;
      align-items: center;
      transition: width 0.2s;
      border-radius: 3px;
    }

    .expand {
      width: 65%;
    }

    .unset {
      width: 16%;
      color: black !important;
      background-color: #ddd !important;
    }

    .small {
      width: 5%;
      background-color: #ddd !important;
    }

    .small > div {
      opacity: 0;
    }

    .unset > div > p {
      opacity: 0;
    }

    .expand > div {
      transition-delay: 200ms;
      opacity: 1;
    }
  </style>
</head> 
<div class="container horizontal-accordion">

       <div class="card choice unset bg-secondary text-dark mx-2">
       <div class="card-body">
     <h5 class="card-title">Step 1</h5>
     <h5 class="end-0 fa fa-mouse p-2 position-absolute top-0"></h5>
        <h4>How to use this</h4><br/>
            <p>Pivot Table</p><br/>
    </div>
  </div>

  <div class="card choice unset bg-success text-white mx-2">
    <div class="card-body">
      <h5 class="card-title">Step 3</h5>
     <strong> Drag and Drop pills</strong><br/>
      <p> Drag pills to their zones</p><br/>
      </div>
       </div>
       
       <div class="card choice unset bg-warning text-dark mx-2">
       <div class="card-body">
       <h5 class="card-title">Step 4</h5>
 <strong> Start with Group-By</strong><br/>
      <p>This creates the rows</p><br/>    </div>
  </div>

  <div class="card choice unset bg-danger text-white mx-2">
    <div class="card-body">
          <h5 class="card-title">Step 5</h5>

     <strong> Optionally, pivot wider </strong><br/>
            <p>Drag a dimension, or groupBy, to the group column to set the headers</p><br/>

      <em>Only one is allowed here</em></div>
       </div>
       

  

       <div class="card choice unset bg-dark text-white mx-2">
       <div class="card-body">
              <h5 class="card-title">Final Steps</h5>

          <strong> Drag a Function and a Value </strong><br/>
      <p> Each new column has each function applied to every value</p><br/>
      <em> The default is mean</em>
      
      </div>
  </div>
  
  
        <div class="card choice unset bg-dark text-white mx-2">
       <div class="card-body">
                       <h5 class="card-title">....</h5>

        <strong> For control</strong><br/>
     <p> Drag a function-value pair to specify individual values and their transformations</p><br/>

      <em> Only one function can be applied to values</em>
    </div>
  </div>
  

</div>')
}

browsable(help_component())

