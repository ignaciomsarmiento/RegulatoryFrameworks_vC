labor_ui <- function(id) {
  ns <- NS(id)
  
tagList(

  tags$style(HTML("
      .pill-button.active {
        background-color: #00C1FF !important;
        color: white !important;
      }
      .pill-button:hover {
        opacity: 0.85;
      }
      
      .component-btn.active {
        background-color: #00C1FF !important;
        color: white !important;
      }
      .component-btn:hover {
        opacity: 0.85;
      }
    ")),
  tags$script(HTML("
      $(document).on('click', '.topic-page .pill-button', function(e) {
        var $container = $(this).closest('.topic-page');
        $container.find('.pill-button').removeClass('active');
        $(this).addClass('active');
      });
      $(document).on('click', '.component-btn', function(e) {
        $('.component-btn').removeClass('active');
        $(this).addClass('active');
      });
      // Toggle helper bubbles
      $(document).on('click', '.info-icon', function(e) {
        e.stopPropagation();
        var target = $(this).data('target');
        if (!target) return;
        $('.info-bubble').not('#' + target).hide();
        $('#' + target).toggle();
      });
      $(document).on('click', function() {
        $('.info-bubble').hide();
        $('.component-dropdown-panel').hide();
      });
    ")),
    tags$style(HTML("
      .info-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 22px;
        height: 22px;
        border-radius: 50%;
        border: 1px solid #0f3b66;
        color: #0f3b66;
        font-size: 12px;
        cursor: pointer;
        background: #f8fbff;
        font-weight: 700;
      }
      .info-bubble {
        display: none;
        position: absolute;
        background: #5f778e;
        color: #fff;
        padding: 14px 16px 12px 16px;
        border-radius: 10px;
        margin-top: 8px;
        max-width: 320px;
        z-index: 30;
        left: 0;
        top: 110%;
        box-shadow: 0 10px 18px rgba(0,0,0,0.15);
      }
      .info-bubble a {
        color: #00b2ff;
        font-weight: 600;
      }
      .component-menu {
        display: flex;
        flex-direction: column;
        gap: 8px;
        max-width: 360px;
      }
      .component-menu .component-btn,
      .component-submenu .component-btn {
        width: 100%;
        text-align: left;
        justify-content: space-between;
        background: #fff;
        border: 1px solid #e0e7ef;
        color: #0f3b66;
        border-radius: 10px;
        padding: 12px 14px;
        font-weight: 600;
        display: flex;
        align-items: center;
        gap: 8px;
        min-height: 46px;
      }
      .component-menu .component-btn.active,
      .component-submenu .component-btn.active {
        background-color: #00b2ff !important;
        color: #fff !important;
        border-color: #00a0e0;
      }
      .component-submenu {
        display: flex;
        flex-direction: column;
        gap: 8px;
        margin-top: 8px;
        max-width: 340px;
        margin-left: 12px; /* indent to signal hierarchy */
      }
      .component-submenu .component-btn {
        font-size: 13px;
        padding: 10px 12px;
      }
    ")),

  tags$div(class = "topic-page", # nolint
  # Header across the full page
    fluidRow(
      column(
      width = 12,
        tags$div(
        style = "margin-bottom: 20px; background: #fff; padding: 16px; border-radius: 12px;",
        h1(class = "topic-title", "Non-salary Labor Costs"),
        p(class = "topic-subtitle",
        "Explore non-wage labor costs employers incur beyond direct wages, including legally required social contributions by payer (employer vs. employee) and by benefit component. Analyze how these costs vary across countries and wage levels.")
        )
    )
  ),

    # ============================================================
    # LAYOUT PRINCIPAL (2 COLUMNAS)
    # ============================================================
    fluidRow(
    # ----------------------------------------------------------
    # COLUMNA IZQUIERDA
    # ----------------------------------------------------------
              column(
                width = 3,
                class = "left-panel",
                 # -------- FILTERS --------
                 tags$div(
                    style = "margin-bottom: 30px;",
                    h3("FILTERS", style = "color: #1e3a5f; font-weight: bold; margin-top: 0; margin-bottom: 6px;"), # nolint: line_length_linter.
                    tags$hr(style = "border-top: 2px solid #00b8d4; margin-top: 0; margin-bottom: 14px;"),
                    tags$p(
                      style = "font-size: 13px; line-height: 1.5; margin-bottom: 15px;",
                      "Use these filters to compare non-salary labor costs across countries: (1) The first option allows you to decompose the total costs by who pays or by benefit line, (2) the second option allows you to see the costs by workers of different levels as multiples of the minimum wage, and (3) compare the same worker wage level across countries."
                    ),

                    # ---- SUMMARY FILTER ----
                    tags$div(
                      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
                      tags$span("Option 1 — Choose disaggregation level:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                      tags$div(
                        style = "display: flex; flex-direction: column; gap: 8px;",
                        tags$div(
                          style = "position: relative; display: inline-block;",
                          actionButton(ns("btn_total"), "TOTAL",
                                       class = "pill-button active",
                                       style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;")
                        ),                     
                       tags$div(
                         style = "position: relative; display: inline-block;",
                         actionButton(
                           ns("btn_component"),
                           HTML(paste0("BY COMPONENT ", "<span class='info-icon' data-target='", ns("component_help"), "'>i</span>")),
                           class = "pill-button",
                           style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;"
                         ),
                         tags$div(
                           id = ns("component_help"),
                           class = "info-bubble info-bubble-popup",
                           tags$p("Each component explains a distinct benefit or tax line. Interact with the chart to reveal details.", style = "margin: 0 0 6px 0;"),
                           tags$a(
                             href = "#",
                             onclick = "document.querySelector('a[data-value=\"Guide\"]').click(); return false;",
                             "Learn more"
                           )
                         )
                       )
                     ),
                      tags$div(
                        class = "component-wrapper-fixed",
                        uiOutput(ns("component_buttons"))
                      ),
                      tags$div(
                        class = "component-wrapper-fixed",
                        uiOutput(ns("bonus_buttons"))
                      )
                    ),

                    # ---- WAGE FILTER ----
                    tags$div(
                      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
                      tags$span("Option 2 — Select the worker wage level:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                      tags$p(
                        "Hold the job constant and change only earnings: choose a multiple of the minimum wage (MW) to see how statutory costs scale with pay.",
                        style = "font-size: 12px; color: #555; margin: 0 0 4px 0;"
                      ),
                      div(
                        class = "pretty-select",
                        selectInput(
                          inputId = ns("mw_selection"),
                          label = NULL,
                          choices = c(
                            "1 MW" = "1sm",
                            "2 MW" = "2sm",
                            "5 MW" = "5sm",
                            "10 MW" = "10sm",
                            "15 MW" = "15sm"
                          ),
                          selected = "1sm",
                          width = "100%"
                        )
                      )
                    ),

                    # ---- COUNTRY FILTER ----
                    tags$div(
                      style = "margin-top: 5px;",
                      tags$span("Option 3 — Choose a country:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                      tags$p(
                        "Switch countries to see how different regulatory frameworks change the composition and level of non-wage costs.",
                        style = "font-size: 12px; color: #555; margin: 0 0 6px 0;"
                      ),
                      uiOutput(ns("country_selection"))
                    )
                  ),


                 

                 # -------- DOWNLOAD & SHARE BUTTONS --------
                 tags$div(
                   style = "display: flex; gap: 10px;",
                   downloadButton(
                     outputId = ns("download_df"),
                     label = "DOWNLOAD",
                     style = "background-color: #1e3a5f; color: white; border-radius: 25px; padding: 10px 20px; font-weight: bold; border: none;"
                     
                   )
                 )
               ),
               
               # ----------------------------------------------------------
               # COLUMNA DERECHA
               # ----------------------------------------------------------
               column(
                 width = 9,
                 class = "right-panel",
                 style = "padding-top: 34px;",
                 tags$hr(style = "border-top: 2px solid #00b8d4; margin-top: 0; margin-bottom: 14px;"),
                 
                 # -------- GRÁFICO --------
                 plotlyOutput(ns("plot"), height = "500px"),
                 div(
                   style = "margin-top:30px;",
                 reactable::reactableOutput(ns("tabla_detalle"))
               )
             )
           )
          )
  )
}
