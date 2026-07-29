library(shiny)

#ui <- fluidPage(

progress_component <- function(){
  div(
    singleton(
  tags$head(
    tags$style(HTML("
      /* Hidden by default */
      #loaderWrap { padding:10px;backdrop-filter:blur('5px');background:rgb(0,0,0,0.1);display:none;  position: fixed; left:15px; bottom:15px;   margin: 12px 0; }
      .progress { height: 20px; width:95vw;}
    ")),
    tags$script(HTML("
      (function () {
        // keep one timer per bar so repeated triggers reset cleanly
        let activeTimer = null;

        const colorClasses = [
          'bg-success','bg-info','bg-warning','bg-danger',
          'bg-primary','bg-secondary','bg-dark','bg-light'
        ];

        function setBarColor(bar, cls) {
          bar.classList.remove(...colorClasses);
          bar.classList.add(cls);
        }

        function startLoader(opts) {
          const wrap = document.getElementById(opts.wrapId || 'loaderWrap');
          const bar  = document.getElementById(opts.barId  || 'loaderBar');

          if (!wrap || !bar) return;

          // Stop any in-flight animation
          if (activeTimer) {
            clearInterval(activeTimer);
            activeTimer = null;
          }

          // Defaults
          const totalMs  = opts.totalMs  ?? 8000;
          const tickMs   = opts.tickMs   ?? 50;
          const lingerMs = opts.lingerMs ?? 1000;

          // stages: [{t:0, cls:'bg-success'}, {t:3000, cls:'bg-info'}, ...]
          const stages = opts.stages ?? [
            { t: 0,    cls: 'bg-success' },
            { t: 3000, cls: 'bg-info'    },
            { t: 6000, cls: 'bg-warning' },
            { t: 7500, cls: 'bg-danger'  }
          ];

          // Show + reset
          wrap.style.display = 'block';
          bar.style.width = '0%';
          bar.setAttribute('aria-valuenow', '0');
          bar.classList.add('progress-bar-striped', 'progress-bar-animated');
          setBarColor(bar, stages[0].cls);

          const start = performance.now();

          function stageFor(elapsed) {
            let chosen = stages[0].cls;
            for (const s of stages) if (elapsed >= s.t) chosen = s.cls;
            return chosen;
          }

          activeTimer = setInterval(() => {
            const now = performance.now();
            const elapsed = Math.min(now - start, totalMs);

            const pct = Math.round((elapsed / totalMs) * 100);
            bar.style.width = pct + '%';
            bar.setAttribute('aria-valuenow', String(pct));

            setBarColor(bar, stageFor(elapsed));

            if (elapsed >= totalMs) {
              clearInterval(activeTimer);
              activeTimer = null;

              // Optional: stop stripe animation at 100%
              bar.classList.remove('progress-bar-animated');

              // Linger, then hide
              setTimeout(() => {
                wrap.style.display = 'none';
              }, lingerMs);
            }
          }, tickMs);
        }

        function stopLoader(opts) {
          const wrap = document.getElementById(opts?.wrapId || 'loaderWrap');
          if (activeTimer) {
            clearInterval(activeTimer);
            activeTimer = null;
          }
          if (wrap) wrap.style.display = 'none';
        }

        // Custom message hooks from server
        Shiny.addCustomMessageHandler('loader_start', startLoader);
        Shiny.addCustomMessageHandler('loader_stop',  stopLoader);
      })();
    ")))
  ),
  
  actionButton("go", "Start loader (server signal)"),
  
  # The loader (hidden until triggered)
  div(
    id = "loaderWrap",
    div(
      class = "progress",
      div(
        id = "loaderBar",
        class = "progress-bar",
        role = "progressbar",
        # style = "width:0%",
        `aria-valuenow` = "0",
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      )
    )
  )
  
  #,verbatimTextOutput("status")
)
}

# server <- function(input, output, session) {
#   
#   output$status <- renderText("Idle")
#   
#   observeEvent(input$go, {
#     output$status <- renderText("Loader triggered from server")
#     
#     session$sendCustomMessage(
#       "loader_start",
#       list(
#         wrapId = "loaderWrap",
#         barId  = "loaderBar",
#         totalMs  = 10000,   # fixed duration
#         tickMs   = 50,      # smoothness
#         lingerMs = 1000,    # linger after completion then hide
#         stages = list(
#           list(t = 0,    cls = "bg-success"),
#           list(t = 3000, cls = "bg-info"),
#           list(t = 7000, cls = "bg-warning"),
#           list(t = 9000, cls = "bg-danger")
#         )
#       )
#     )
#   })
#   
#   # Optional: hide instantly on some other condition
#   # observeEvent(input$something, {
#   #   session$sendCustomMessage("loader_stop", list(wrapId="loaderWrap"))
#   # })
# }
# 
# shinyApp(ui, server)



# library(shiny)
# library(htmltools)
# 
# progress_component <- function(id = "loader") {
#   wrap_id <- paste0(id, "_wrap")
#   bar_id  <- paste0(id, "_bar")
#   div(
#   tagList(
#     singleton(
#       tags$head(
#         tags$style(HTML(sprintf("
#           #%s { 
#             display:none; 
#             position:fixed; 
#             left:10px; 
#             bottom:10px; 
#             margin:12px 0; 
#             z-index:9999;
#             width:min(380px, calc(100vw - 20px));
#           }
#           #%s .progress { height:10px; }
#         ", wrap_id, wrap_id))),
#         tags$script(HTML(sprintf("
#           (function () {
#             // one timer per loader instance
#             const state = window.__phmLoaderState || (window.__phmLoaderState = {});
#             state['%s'] = state['%s'] || { timer: null };
# 
#             const colorClasses = [
#               'bg-success','bg-info','bg-warning','bg-danger',
#               'bg-primary','bg-secondary','bg-dark','bg-light'
#             ];
# 
#             function setBarColor(bar, cls) {
#               bar.classList.remove.apply(bar.classList, colorClasses);
#               if (cls) bar.classList.add(cls);
#             }
# 
#             function startLoader(opts) {
#               opts = opts || {};
#               const wrap = document.getElementById(opts.wrapId || '%s');
#               const bar  = document.getElementById(opts.barId  || '%s');
#               if (!wrap || !bar) return;
# 
#               const st = state['%s'];
# 
#               // Stop any in-flight animation
#               if (st.timer) {
#                 clearInterval(st.timer);
#                 st.timer = null;
#               }
# 
#               const totalMs  = (opts.totalMs  != null) ? opts.totalMs  : 8000;
#               const tickMs   = (opts.tickMs   != null) ? opts.tickMs   : 50;
#               const lingerMs = (opts.lingerMs != null) ? opts.lingerMs : 1000;
# 
#               const stages = opts.stages || [
#                 { t: 0,    cls: 'bg-success' },
#                 { t: 3000, cls: 'bg-info'    },
#                 { t: 6000, cls: 'bg-warning' },
#                 { t: 7500, cls: 'bg-danger'  }
#               ];
# 
#               function stageFor(elapsed) {
#                 let chosen = stages[0] && stages[0].cls;
#                 for (let i = 0; i < stages.length; i++) {
#                   if (elapsed >= stages[i].t) chosen = stages[i].cls;
#                 }
#                 return chosen;
#               }
# 
#               // Show + reset
#               wrap.style.display = 'block';
#               bar.style.width = '0%';
#               bar.setAttribute('aria-valuenow', '0');
#               bar.classList.add('progress-bar-striped', 'progress-bar-animated');
#               setBarColor(bar, stages[0] && stages[0].cls);
# 
#               const start = performance.now();
# 
#               st.timer = setInterval(() => {
#                 const now = performance.now();
#                 const elapsed = Math.min(now - start, totalMs);
# 
#                 const pct = Math.round((elapsed / totalMs) * 100);
#                 bar.style.width = pct + '%%';
#                 bar.setAttribute('aria-valuenow', String(pct));
# 
#                 setBarColor(bar, stageFor(elapsed));
# 
#                 if (elapsed >= totalMs) {
#                   clearInterval(st.timer);
#                   st.timer = null;
# 
#                   // stop stripe animation at 100%%
#                   bar.classList.remove('progress-bar-animated');
# 
#                   setTimeout(() => {
#                     wrap.style.display = 'none';
#                   }, lingerMs);
#                 }
#               }, tickMs);
#             }
# 
#             function stopLoader(opts) {
#               opts = opts || {};
#               const wrap = document.getElementById(opts.wrapId || '%s');
#               const st = state['%s'];
#               if (st.timer) {
#                 clearInterval(st.timer);
#                 st.timer = null;
#               }
#               if (wrap) wrap.style.display = 'none';
#             }
# 
#             // Register handlers once Shiny exists
#             function registerWhenReady() {
#               if (window.Shiny && Shiny.addCustomMessageHandler) {
#                 Shiny.addCustomMessageHandler('loader_start', startLoader);
#                 Shiny.addCustomMessageHandler('loader_stop',  stopLoader);
#               } else {
#                 setTimeout(registerWhenReady, 25);
#               }
#             }
#             registerWhenReady();
#           })();
#         ", id, id, wrap_id, bar_id, id, wrap_id, id)))))
#   
#   ),
# 
# # loader DOM
# div(
#   id = wrap_id,
#   div(
#     class = "progress",
#     div(
#       id = bar_id,
#       class = "progress-bar",
#       role = "progressbar",
#       style = "width:0%",
#       `aria-valuenow` = "0",
#       `aria-valuemin` = "0",
#       `aria-valuemax` = "100"
#     )
#   )
# )
# )
# }

# ui <- fluidPage(
#   tags$h4("Loader demo"),
#   actionButton("go", "Start loader (server signal)"),
#   progress_component("loaderA")
# )
# 
# server <- function(input, output, session) {
#   observeEvent(input$go, {
#     session$sendCustomMessage(
#       "loader_start",
#       list(
#         wrapId = "loaderA_wrap",
#         barId  = "loaderA_bar",
#         totalMs  = 10000,
#         tickMs   = 50,
#         lingerMs = 1000,
#         stages = list(
#           list(t = 0,    cls = "bg-success"),
#           list(t = 3000, cls = "bg-info"),
#           list(t = 7000, cls = "bg-warning"),
#           list(t = 9000, cls = "bg-danger")
#         )
#       )
#     )
#   })
# }
# 
# shinyApp(ui, server)