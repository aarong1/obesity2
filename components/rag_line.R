library(shiny)
library(htmltools)


rag_line <- function(per1k=10.9){HTML(sprintf('
<head>

  <style>
    /* Styling for the block */
    .colour-block {
    margin-block:10px;
      display: flex;
      width:100%;
      height: 20px;
      position: relative;
            gap:2px;

    }

    .colour-segment {
    border-radius:5%;
    flex:1
     
    }
    .segment-1 { background-color: lightgreen; flex:0.2;}
    .segment-2 { background-color: yellow;flex:0.05; }
    .segment-3 { background-color: orange; flex:0.05;}
    .segment-4 { background-color: #ff4741;flex:0.7; }

    /* Styling for the arrow */
    .arrow {
      width: 0;
      height: 0;
      border-left: 10px solid transparent;
      border-right: 10px solid transparent;
      border-top: 15px solid black;
      border-radius:35%;
      position: absolute;
      top: -20px; /* Position the tip of the arrow at the top of the block */
      transform: translateX(-50%);
    }
  </style>
</head>
<body>
  <div class="colour-block">
    <div class="colour-segment segment-1"></div>
    <div class="colour-segment segment-2"></div>
    <div class="colour-segment segment-3"></div>
    <div class="colour-segment segment-4"></div>
    <div class="arrow" id="arrow"></div>
  </div>
<!--  <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>-->

  <script>
    // Function to position the arrow
    function setArrowPosition(percentage) {
      if (percentage < 0 || percentage > 100) {
        console.error("Percentage value must be between 0 and 100");
        return; 
      }
      const blockWidth = 400; // Width of the block in pixels
      const arrowPosition = (percentage / 100) * blockWidth; // Calculate the position in pixels
      //$("#arrow").css("left", `${arrowPosition}px`);
      document.getElementById("arrow").style.left = arrowPosition + "px";
    }

  setTimeout(function() {
    // Example usage
    setArrowPosition(%f); // Move the arrow to 25% of the blocks width
  }, 1000);
  </script>
</body>
', per1k))
}


rag_line <- function(per1k=10.9) {
  
  
  htmltools::HTML(sprintf('
  <style>
    .rag-container {
      margin-block: 10px;
      position: relative;
      width: 100%%;
    }

    .colour-block {
      display: flex;
      width: 100%%;
      height: 20px;
      gap: 2px;
      position: relative;
    }

    .colour-segment {
      flex: 1;
      border-radius: 4px;
    }

    .segment-1 { background-color: lightgreen; flex: 0.20; }
    .segment-2 { background-color: yellow;     flex: 0.05; }
    .segment-3 { background-color: orange;     flex: 0.05; }
    .segment-4 { background-color: #ff4741;    flex: 0.70; }

    .arrow {
      position: absolute;
      top: -14px;
      width: 0;
      height: 0;
      border-left: 7px solid transparent;
      border-right: 7px solid transparent;
      border-top: 10px solid black;
      transform: translateX(-50%%);
      pointer-events: none;
      left: %s%%; 
    }
  </style>

  <div class="rag-container">
    <div class="colour-block">
      <div class="colour-segment segment-1"></div>
      <div class="colour-segment segment-2"></div>
      <div class="colour-segment segment-3"></div>
      <div class="colour-segment segment-4"></div>
      <div class="arrow"></div>
    </div>
  </div>

  <script>
    (function() {
      const container = document.currentScript.previousElementSibling;
      const block = container.querySelector(".colour-block");
      const arrow = container.querySelector(".arrow");

      function setArrowPosition(pct) {
        pct = Math.max(0, Math.min(100, pct));
        const width = block.clientWidth;
        arrow.style.left = (pct / 100) * width + "px";
      }

      //requestAnimationFrame(() => setArrowPosition(%f));
      //window.addEventListener("resize", () => setArrowPosition(%f));
    })();
  </script>
  ', as.character(round(per1k)), per1k, per1k))
}

browsable(rag_line(50.9))
