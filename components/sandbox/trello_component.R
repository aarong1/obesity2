
        library(shiny)

ui <- page_fluid(
  tags$head(
  #js ----
  HTML('<script>
    $(function() {
        // Make the cards draggable
        $(".card").draggable({
            revert: "invalid", // If not dropped in a valid target, the card will go back to its original position
            zIndex: 100,
            start: function(event, ui) {
                $(this).addClass("dragging");
            },
            stop: function(event, ui) {
                $(this).removeClass("dragging");
            }
        });

        // Make the columns droppable
        $(".column").droppable({
            accept: ".card", // Only accept elements with the class "card"
            drop: function(event, ui) {
                $(this).append(ui.helper);
                ui.helper.css({
                    "top": "auto",
                    "left": "auto"
                });
            }
        });
    });
</script>
'),
  
  #css -----
  tags$style("body {
            font-family: Arial, sans-serif;
            margin: 0;
            padding: 0;
        }
        .board {
            display: flex;
            justify-content: space-around;
            margin: 20px;
        }
        .column {
            width: 30%;
            background-color: #f0f0f0;
            padding: 10px;
            border-radius: 5px;
            box-shadow:  5px 5px 10px rgba(0, 0, 0, 0.3);
        }
        .column h3 {
            text-align: center;
            color: #333;
        }
        .card {
            background-color: #fff;
            border-radius: 5px;
            padding: 10px;
            margin: 10px 0;
            box-shadow: 1px 1px 5px rgba(0, 0, 0, 0.1);
            cursor: grab;
        }
        .card:hover {
            background-color: #e0e0e0;
        }
        
        ") ) ,
  
  #html ----
  HTML(' <div class="board">
        <div class="column" id="todo">
            <h3>To Do</h3>
            <div class="card">Task 1</div>
            <div class="card">Task 2</div>
            <div class="card">Task 3</div>
        </div>
        <div class="column" id="in-progress">
            <h3>In Progress</h3>
        </div>
        <div class="column" id="done">
            <h3>Done</h3>
        </div>
    </div>

    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"></script>

')
)


# shinyApp(ui,function(input, output, session){})

