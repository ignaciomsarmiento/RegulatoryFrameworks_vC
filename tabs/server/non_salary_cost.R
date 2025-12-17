estatest_nuevo_grafico <- function(
    df_non_salary,
    df_non_salary_payer,
    wage_filter,
    countries = NULL,
    country_labels = NULL,
    font_family = "Source Sans Pro",
    use_patterns = TRUE,
    initial_view = c("total", "payer")
) {
  initial_view <- match.arg(initial_view)

  
  COL_MIN <- "#00C1FF"  # mínimo (base)
  COL_MIN_Employee <- "#00C1FF"  # mínimo (employee)
  COL_MIN_Employer <- "#002244"  # mínimo (employer)
  COL_MAX <- "#002244"  # máximo (base)
  COL_MAX_Employee <- "#00C1FF"  # máximo (employee)
  COL_MAX_Employer <- "#002244"  # máximo (employer)
  
  PAT_EMPLOYER <- "/"
  PAT_EMPLOYEE <- "x"

  if (is.null(wage_filter) || is.na(wage_filter) || wage_filter == "") {
    return(plotly::plotly_empty(type = "bar"))
  }

  countries_use <- countries
  if (is.null(countries_use) || length(countries_use) == 0 || "All" %in% countries_use) {
    countries_use <- NULL
  } else {
    countries_use <- unique(countries_use)
  }

  df_total <- df_non_salary %>%
    dplyr::filter(.data$wage == wage_filter, .data$type %in% c("t_min", "t_max"))
  if (!is.null(countries_use)) {
    df_total <- df_total %>% dplyr::filter(.data$country %in% countries_use)
  }

  df_payer <- df_non_salary_payer %>%
    dplyr::filter(
      .data$wage == wage_filter,
      .data$type_by_payer %in% c("st_er_min", "st_er_max", "st_ee_min", "st_ee_max")
    )
  if (!is.null(countries_use)) {
    df_payer <- df_payer %>% dplyr::filter(.data$country %in% countries_use)
  }

  if (nrow(df_total) == 0) {
    return(
      plotly::plotly_empty(type = "bar") %>%
        plotly::layout(
          title = list(text = "No data for this selection", x = 0.5, xanchor = "center"),
          font = list(family = font_family)
        )
    )
  }

  if (!is.null(country_labels)) {
    df_total <- dplyr::left_join(df_total, country_labels, by = "country")
    df_payer <- dplyr::left_join(df_payer, country_labels, by = "country")
  } else {
    df_total$country_name <- df_total$country
    df_payer$country_name <- df_payer$country
  }

  df_total <- df_total %>%
    dplyr::mutate(country_display = dplyr::coalesce(.data$country_name, .data$country))
  df_payer <- df_payer %>%
    dplyr::mutate(country_display = dplyr::coalesce(.data$country_name, .data$country))

  df_total_wide <- df_total %>%
    dplyr::select(.data$country_display, .data$type, .data$value) %>%
    tidyr::pivot_wider(names_from = .data$type, values_from = .data$value) %>%
    dplyr::arrange(.data$t_min) %>%
    dplyr::mutate(country_display = as.character(.data$country_display))

  order_country <- df_total_wide$country_display
  if (length(order_country) == 0) {
    return(plotly::plotly_empty(type = "bar"))
  }

  x_levels <- as.vector(t(cbind(
    paste(order_country, "Min", sep = "___"),
    paste(order_country, "Max", sep = "___")
  )))

  fmt <- function(x) {
    ifelse(is.na(x), "NA", sprintf("%.1f", x))
  }

  add_meta <- function(df) {
    df %>%
      dplyr::mutate(
        country = sub("___(Min|Max)$", "", .data$xcat),
        scenario = sub("^.*___", "", .data$xcat)
      )
  }

  mk_grid <- function(df) {
    df %>%
      dplyr::select(.data$xcat, .data$value) %>%
      dplyr::right_join(data.frame(xcat = x_levels, stringsAsFactors = FALSE), by = "xcat") %>%
      dplyr::mutate(xcat = factor(.data$xcat, levels = x_levels)) %>%
      dplyr::arrange(.data$xcat) %>%
      dplyr::mutate(xcat = as.character(.data$xcat)) %>%
      add_meta()
  }

  df_total_long <- df_total %>%
    dplyr::select(.data$country_display, .data$type, .data$value) %>%
    dplyr::mutate(
      country_display = factor(.data$country_display, levels = order_country),
      Scenario = ifelse(.data$type == "t_min", "Min", "Max"),
      Scenario = factor(Scenario, levels = c("Min", "Max")),
      xcat = paste(as.character(.data$country_display), as.character(Scenario), sep = "___")
    ) %>%
    dplyr::select(.data$Scenario, .data$xcat, .data$value)

  total_min_full <- df_total_long %>%
    dplyr::filter(.data$Scenario == "Min") %>%
    mk_grid() %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Country:</b> ", .data$country, "<br>",
        "<b>Scenario:</b> ", .data$scenario, "<br>",
        "<b>Total:</b> ", fmt(.data$value), "%"
      )
    )

  total_max_full <- df_total_long %>%
    dplyr::filter(.data$Scenario == "Max") %>%
    mk_grid() %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Country:</b> ", .data$country, "<br>",
        "<b>Scenario:</b> ", .data$scenario, "<br>",
        "<b>Total:</b> ", fmt(.data$value), "%"
      )
    )

  df_payer_long <- df_payer %>%
    dplyr::select(.data$country_display, .data$type_by_payer, .data$value) %>%
    dplyr::mutate(
      country_display = factor(.data$country_display, levels = order_country),
      Scenario = ifelse(grepl("_min$", .data$type_by_payer), "Min", "Max"),
      payer = ifelse(grepl("^st_er", .data$type_by_payer), "Employer", "Employee"),
      Scenario = factor(Scenario, levels = c("Min", "Max")),
      xcat = paste(as.character(.data$country_display), as.character(Scenario), sep = "___")
    ) %>%
    dplyr::select(.data$xcat, .data$Scenario, .data$payer, .data$value)

  payer_er_min <- df_payer_long %>%
    dplyr::filter(.data$payer == "Employer", .data$Scenario == "Min") %>%
    mk_grid() %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Country:</b> ", .data$country, "<br>",
        "<b>Scenario:</b> ", .data$scenario, "<br>",
        "<b>Payer:</b> Employer<br>",
        "<b>Value:</b> ", fmt(.data$value), "%"
      )
    )

  payer_ee_min <- df_payer_long %>%
    dplyr::filter(.data$payer == "Employee", .data$Scenario == "Min") %>%
    mk_grid() %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Country:</b> ", .data$country, "<br>",
        "<b>Scenario:</b> ", .data$scenario, "<br>",
        "<b>Payer:</b> Employee<br>",
        "<b>Value:</b> ", fmt(.data$value), "%"
      )
    )

  payer_er_max <- df_payer_long %>%
    dplyr::filter(.data$payer == "Employer", .data$Scenario == "Max") %>%
    mk_grid() %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Country:</b> ", .data$country, "<br>",
        "<b>Scenario:</b> ", .data$scenario, "<br>",
        "<b>Payer:</b> Employer<br>",
        "<b>Value:</b> ", fmt(.data$value), "%"
      )
    )

  payer_ee_max <- df_payer_long %>%
    dplyr::filter(.data$payer == "Employee", .data$Scenario == "Max") %>%
    mk_grid() %>%
    dplyr::mutate(
      hover_text = paste0(
        "<b>Country:</b> ", .data$country, "<br>",
        "<b>Scenario:</b> ", .data$scenario, "<br>",
        "<b>Payer:</b> Employee<br>",
        "<b>Value:</b> ", fmt(.data$value), "%"
      )
    )

  xaxis_minmax <- list(
    title = "",
    type = "category",
    categoryorder = "array",
    categoryarray = x_levels,
    tickmode = "array",
    tickvals = x_levels,
    ticktext = rep(c("Min", "Max"), times = length(order_country)),
    tickangle = 90,
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE
  )

  country_ann <- lapply(seq_along(order_country), function(i) {
    list(
      x = paste(order_country[i], "Min", sep = "___"),
      y = -6,
      text = order_country[i],
      showarrow = FALSE,
      xref = "x",
      yref = "y",
      xshift = 18,
      font = list(size = 11, color = "#000000", family = font_family)
    )
  })

  vis_total <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  vis_payer <- c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

  title_total <- list(
    text = paste0(
      "<b>Non-salary costs</b><br>",
      "<span style='font-size:14px'>Total (Min / Max) — ", wage_filter, "</span>"
    ),
    x = 0.5,
    xanchor = "center",
    y = 0.98,
    yanchor = "top"
  )

  title_payer <- list(
    text = paste0(
      "<b>Non-salary costs</b><br>",
      "<span style='font-size:14px'>Who pays (Employer vs Employee) — ", wage_filter, "</span>"
    ),
    x = 0.5,
    xanchor = "center",
    y = 0.98,
    yanchor = "top"
  )

  y_max <- max(c(df_total_wide$t_min, df_total_wide$t_max), na.rm = TRUE)
  if (!is.finite(y_max)) y_max <- 100
  y_max <- y_max * 1.08

  show_total_init <- initial_view == "total"

  fig <- plotly::plot_ly() %>%
    plotly::add_bars(
      data = total_min_full,
      x = ~xcat,
      y = ~value,
      name = "Total",
      visible = show_total_init,
      showlegend = TRUE,
      # hoverinfo = "text",
      # text = ~hover_text,
      marker = list(color = COL_MIN)
    ) %>%
    plotly::add_bars(
      data = total_max_full,
      x = ~xcat,
      y = ~value,
      name = "Total",
      visible = show_total_init,
      showlegend = FALSE,
      # hoverinfo = "text",
      # text = ~hover_text,
      marker = list(color = COL_MAX)
    ) %>%
    plotly::add_bars(
      data = payer_er_min,
      x = ~xcat,
      y = ~value,
      name = "Employer",
      visible = !show_total_init,
      legendgroup = "Employer",
      showlegend = TRUE,
      # hoverinfo = "text",
      # text = ~hover_text,
      marker = if (use_patterns) {
        list(
          color = COL_MIN_Employer,
          pattern = list(
            #shape = PAT_EMPLOYER,
            fillmode = "overlay",
            fgcolor = "rgba(0,0,0,0.35)"
          )
        )
      } else {
        list(color = COL_MIN, opacity = 0.85)
      }
    ) %>%
    plotly::add_bars(
      data = payer_ee_min,
      x = ~xcat,
      y = ~value,
      name = "Employee",
      visible = !show_total_init,
      legendgroup = "Employee",
      showlegend = TRUE,
      # hoverinfo = "text",
      # text = ~hover_text,
      marker = if (use_patterns) {
        list(
          color = COL_MIN_Employee,
          pattern = list(
            #shape = PAT_EMPLOYEE,
            fillmode = "overlay",
            fgcolor = "rgba(0,0,0,0.35)"
          )
        )
      } else {
        list(color = COL_MIN, opacity = 0.45)
      }
    ) %>%
    plotly::add_bars(
      data = payer_er_max,
      x = ~xcat,
      y = ~value,
      name = "Employer",
      visible = !show_total_init,
      legendgroup = "Employer",
      showlegend = FALSE,
      #hoverinfo = "text",
      #text = ~hover_text,
      marker = if (use_patterns) {
        list(
          color = COL_MAX_Employer,
          pattern = list(
            #shape = PAT_EMPLOYER,
            fillmode = "overlay",
            fgcolor = "rgba(255,255,255,0.30)"
          )
        )
      } else {
        list(color = COL_MAX, opacity = 0.85)
      }
    ) %>%
    plotly::add_bars(
      data = payer_ee_max,
      x = ~xcat,
      y = ~value,
      name = "Employee",
      visible = !show_total_init,
      legendgroup = "Employee",
      showlegend = FALSE,
      #hoverinfo = "text",
      #text = ~hover_text,
      marker = if (use_patterns) {
        list(
          color = COL_MAX_Employee,
          pattern = list(
            #shape = PAT_EMPLOYEE,
            fillmode = "overlay",
            fgcolor = "rgba(255,255,255,0.30)"
          )
        )
      } else {
        list(color = COL_MAX, opacity = 0.45)
      }
    ) %>%
    plotly::layout(
      title = if (show_total_init) title_total else title_payer,
      barmode = if (show_total_init) "group" else "stack",
      xaxis = xaxis_minmax,
      annotations = country_ann,
      font = list(family = font_family),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.22,
        yanchor = "top"
      ),
      margin = list(b = 130, t = 90, l = 60, r = 20),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      yaxis = list(
        title = "Non-salary costs as share of wages (%)",
        range = c(-10, y_max),
        showgrid = FALSE,
        zeroline = FALSE,
        showline = FALSE
      ),
      updatemenus = list(
        list(
          type = "buttons",
          direction = "right",
          showactive = TRUE,
          x = 0,
          y = 1.18,
          buttons = list(
            list(
              label = "TOTAL",
              method = "update",
              args = list(
                list(visible = vis_total),
                list(
                  title = title_total,
                  barmode = "group",
                  xaxis = xaxis_minmax,
                  annotations = country_ann
                )
              )
            ),
            list(
              label = "WHO PAYS",
              method = "update",
              args = list(
                list(visible = vis_payer),
                list(
                  title = title_payer,
                  barmode = "stack",
                  xaxis = xaxis_minmax,
                  annotations = country_ann
                )
              )
            )
          )
        )
      )
    )

  fig
}




labor_server <- function(input, output, session) {
  ns <- session$ns

  df_non_salary <- readRDS("data/non_salary/1. total_non_salary_costs.rds")
  df_non_salary_payer <- readRDS("data/non_salary/2. total_ns_costs_by_payer.rds")
  df_non_salary_component <- readRDS("data/non_salary/3. total_ns_costs_by_component.rds")

  df_country_labels <- readRDS("data/non_salary/total_non_salary_costs.rds") %>%
    dplyr::distinct(.data$country, .data$country_name) %>%
    dplyr::arrange(.data$country_name)

  selected_view <- shiny::reactiveVal("total")
  selected_component <- shiny::reactiveVal("all_component")
  selected_bonus <- shiny::reactiveVal("all_bonuses")
  selected_social <- shiny::reactiveVal("pensions")

  observeEvent(input$btn_total, { selected_view("total") })
  observeEvent(input$btn_payer, { selected_view("payer") })
  observeEvent(input$btn_component, { selected_view("component") })

  observeEvent(input$all_component, { selected_component("all_component") })
  observeEvent(input$bonus, { selected_component("bonuses_and_benefits") })
  observeEvent(input$social, { selected_component("social") })
  observeEvent(input$payroll, { selected_component("payroll_taxes") })

  observeEvent(input$all_bonuses, { selected_bonus("all_bonuses") })
  observeEvent(input$ab, { selected_bonus("ab") })
  observeEvent(input$pl, { selected_bonus("pl") })
  observeEvent(input$up, { selected_bonus("up") })
  observeEvent(input$ob, { selected_bonus("ob") })

  observeEvent(input$pensions, { selected_social("pensions") })
  observeEvent(input$health, { selected_social("health") })
  observeEvent(input$occupational_risk, { selected_social("occupational_risk") })

  output$component_buttons <- renderUI({
    if (selected_view() != "component") {
      return(div(style = "visibility:hidden;"))
    }

    groupC <- selected_component()

    div(
      class = "component-menu",
      tags$div(
        style = "font-weight: bold; color: #1e3a5f; margin-bottom: 8px;",
        "Component breakdown"
      ),
      actionButton(ns("all_component"), "All", class = "component-btn active"),
      tags$div(
        class = "component-item",
        actionButton(
          ns("bonus"),
          HTML("Bonuses and Benefits <span class='info-icon'>i</span>"),
          class = "component-btn"
        ),
        if (groupC == "bonuses_and_benefits") {
          div(
            class = "component-submenu",
            div(
              "Bonuses and Benefits Components",
              style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 4px;"
            ),
            actionButton(ns("all_bonuses"), "All Bonuses", class = "component-btn active"),
            actionButton(ns("ab"), "Annual and other bonuses", class = "component-btn"),
            actionButton(ns("pl"), "Paid Leave", class = "component-btn"),
            actionButton(ns("up"), "Unemployment Protection", class = "component-btn"),
            actionButton(ns("ob"), "Other bonuses and benefits", class = "component-btn")
          )
        }
      ),
      tags$div(
        class = "component-item",
        actionButton(
          ns("social"),
          HTML("Social Security Contributions <span class='info-icon'>i</span>"),
          class = "component-btn"
        ),
        if (groupC == "social") {
          div(
            class = "component-submenu",
            div(
              "Social Security Contributions",
              style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 4px;"
            ),
            actionButton(ns("pensions"), "Pension", class = "component-btn"),
            actionButton(ns("health"), "Health", class = "component-btn"),
            actionButton(ns("occupational_risk"), "Occupational Risk", class = "component-btn")
          )
        }
      ),
      actionButton(
        ns("payroll"),
        HTML("Payroll taxes <span class='info-icon'>i</span>"),
        class = "component-btn"
      )
    )
  })

  output$bonus_buttons <- renderUI({
    div(style = "display:none;")
  })

  output$country_selection <- renderUI({
    div(
      class = "pretty-select",
      selectizeInput(
        inputId = ns("country_selection_user"),
        label = "Country Analysis by:",
        choices = c("All" = "All", stats::setNames(df_country_labels$country, df_country_labels$country_name)),
        selected = "All",
        multiple = TRUE
      )
    )
  })

  observeEvent(input$country_selection_user, {
    if (is.null(input$country_selection_user) || length(input$country_selection_user) == 0) return()
    if (length(input$country_selection_user) > 1 && "All" %in% input$country_selection_user) {
      updateSelectizeInput(session, "country_selection_user", selected = setdiff(input$country_selection_user, "All"))
    }
  }, ignoreInit = TRUE)

  countries_selected_codes <- reactive({
    sel <- input$country_selection_user
    if (is.null(sel) || length(sel) == 0 || "All" %in% sel) return(NULL)
    unique(sel)
  })

  countries_selected_names <- reactive({
    codes <- countries_selected_codes()
    if (is.null(codes)) return(NULL)
    df_country_labels %>%
      dplyr::filter(.data$country %in% codes) %>%
      dplyr::pull(.data$country_name)
  })

  order_country_codes <- reactive({
    codes <- countries_selected_codes()
    df <- df_non_salary %>%
      dplyr::filter(.data$wage == wage_filter(), .data$type == "t_min")
    if (!is.null(codes)) df <- df %>% dplyr::filter(.data$country %in% codes)
    df %>% dplyr::arrange(.data$value) %>% dplyr::pull(.data$country)
  })

  wage_filter <- reactive({
    wage_code <- input$mw_selection
    if (is.null(wage_code) || is.na(wage_code) || wage_code == "") wage_code <- "1sm"
    paste0(sub("sm$", "", wage_code), " MW")
  })

  minmax_bar_plot <- function(df_wide, title = NULL) {
    plotly::plot_ly(df_wide) %>%
      plotly::add_bars(x = ~country, y = ~Min, name = "Min", marker = list(color = "#00C1FF")) %>%
      plotly::add_bars(x = ~country, y = ~Max, name = "Max", marker = list(color = "#002244")) %>%
      plotly::layout(
        title = title,
        barmode = "group",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE),
        yaxis = list(title = "Non-salary costs as % of wages", showgrid = FALSE, zeroline = FALSE, showline = FALSE),
        margin = list(b = 110, t = 50, l = 60, r = 20)
      )
  }

  output$plot <- plotly::renderPlotly({
    view <- selected_view()
    wage_val <- wage_filter()
    codes <- countries_selected_codes()
    country_names_sel <- countries_selected_names()

    if (view %in% c("total", "payer")) {
      initial <- if (view == "payer") "payer" else "total"
      return(
        tryCatch(
          estatest_nuevo_grafico(
            df_non_salary = df_non_salary,
            df_non_salary_payer = df_non_salary_payer,
            wage_filter = wage_val,
            countries = if (is.null(codes)) "All" else codes,
            country_labels = df_country_labels,
            font_family = "Source Sans Pro",
            use_patterns = TRUE,
            initial_view = initial
          ),
          error = function(e) {
            estatest_nuevo_grafico(
              df_non_salary = df_non_salary,
              df_non_salary_payer = df_non_salary_payer,
              wage_filter = wage_val,
              countries = if (is.null(codes)) "All" else codes,
              country_labels = df_country_labels,
              font_family = "Source Sans Pro",
              use_patterns = FALSE,
              initial_view = initial
            )
          }
        )
      )
    }

    if (view != "component") {
      return(plotly::plotly_empty(type = "bar"))
    }

    groupC <- selected_component()
    groupD <- selected_bonus()
    groupE <- selected_social()

    if (groupC == "all_component") {
      df_long <- df_non_salary_component %>%
        dplyr::filter(.data$wage == wage_val) %>%
        dplyr::filter(is.null(codes) || .data$country %in% codes) %>%
        dplyr::mutate(
          Scenario = ifelse(grepl("_min$", .data$type_by_component), "Min", "Max"),
          payer = dplyr::case_when(
            grepl("^st_p", .data$type_by_component) ~ "Pension",
            grepl("^st_h", .data$type_by_component) ~ "Health",
            grepl("^st_b", .data$type_by_component) ~ "Bonuses and Benefits",
            grepl("^st_or", .data$type_by_component) ~ "Labor Risk",
            TRUE ~ "Payroll Taxes"
          ),
          Scenario = factor(Scenario, levels = c("Min", "Max"))
        ) %>%
        dplyr::left_join(df_country_labels, by = "country")

      if (nrow(df_long) == 0) return(plotly::plotly_empty(type = "bar"))

      df_long$Type <- factor(
        df_long$payer,
        levels = c("Pension", "Health", "Labor Risk", "Bonuses and Benefits", "Payroll Taxes")
      )

      colors <- c(
        "Pension" = "#00C1FF",
        "Health" = "#002244",
        "Labor Risk" = "#B9BAB5",
        "Bonuses and Benefits" = "#335B8E",
        "Payroll Taxes" = "#726AA8"
      )

      order_codes <- order_country_codes()
      order_names <- df_country_labels$country_name[match(order_codes, df_country_labels$country)]
      order_names <- order_names[!is.na(order_names)]
      df_long$country_name <- factor(df_long$country_name, levels = order_names)

      paises <- unique(as.character(df_long$country_name))
      plot_list <- list()
      for (i in seq_along(paises)) {
        pais <- paises[[i]]
        data_pais <- df_long %>% dplyr::filter(.data$country_name == pais)
        show_legend <- i == 1
        p <- plotly::plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Type,
          colors = colors,
          legendgroup = ~Type,
          showlegend = show_legend,
          hoverinfo = "y+name"
        ) %>%
          plotly::layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            xaxis = list(title = pais, showgrid = FALSE, zeroline = FALSE, showline = FALSE, tickangle = 90),
            yaxis = list(title = ifelse(i == 1, "Non-salary costs as % of wages", ""), showgrid = FALSE, zeroline = FALSE, showline = FALSE),
            barmode = "stack"
          )
        plot_list[[i]] <- p
      }

      n_plots <- length(plot_list)
      return(
        plotly::subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots),
          margin = 0.01
        ) %>%
          plotly::layout(
            title = "",
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25),
            margin = list(l = 70, r = 30, b = 110, t = 20)
          )
      )
    }

    if (groupC == "social" && groupE == "health") {
      df_health_all <- readRDS("data/non_salary/health_all.rds") %>%
        dplyr::left_join(
          df_country_labels %>% dplyr::rename(country_code = .data$country, country = .data$country_name),
          by = "country"
        ) %>%
        dplyr::filter(.data$wage == wage_val) %>%
        dplyr::filter(!is.na(.data$country_code)) %>%
        dplyr::filter(is.null(codes) || .data$country_code %in% codes) %>%
        dplyr::transmute(
          wage = .data$wage,
          country = .data$country_code,
          type = ifelse(grepl("_min$", .data$min_max_total), "t_min", "t_max"),
          value = .data$value
        )

      df_health_payer <- readRDS("data/non_salary/health_payer.rds") %>%
        dplyr::left_join(
          df_country_labels %>% dplyr::rename(country_code = .data$country, country = .data$country_name),
          by = "country"
        ) %>%
        dplyr::filter(.data$wage == wage_val) %>%
        dplyr::filter(!is.na(.data$country_code)) %>%
        dplyr::filter(is.null(codes) || .data$country_code %in% codes) %>%
        dplyr::transmute(
          wage = .data$wage,
          country = .data$country_code,
          type_by_payer = dplyr::case_when(
            grepl("_er_min$", .data$min_max_payer) ~ "st_er_min",
            grepl("_er_max$", .data$min_max_payer) ~ "st_er_max",
            grepl("_ee_min$", .data$min_max_payer) ~ "st_ee_min",
            TRUE ~ "st_ee_max"
          ),
          value = .data$value
        )

      return(
        tryCatch(
          estatest_nuevo_grafico(
            df_non_salary = df_health_all,
            df_non_salary_payer = df_health_payer,
            wage_filter = wage_val,
            countries = if (is.null(codes)) "All" else codes,
            country_labels = df_country_labels,
            font_family = "Source Sans Pro",
            use_patterns = TRUE,
            initial_view = "total"
          ),
          error = function(e) {
            estatest_nuevo_grafico(
              df_non_salary = df_health_all,
              df_non_salary_payer = df_health_payer,
              wage_filter = wage_val,
              countries = if (is.null(codes)) "All" else codes,
              country_labels = df_country_labels,
              font_family = "Source Sans Pro",
              use_patterns = FALSE,
              initial_view = "total"
            )
          }
        )
      )
    }

    df_component <- NULL

    if (groupC == "bonuses_and_benefits" && groupD != "all_bonuses") {
      df_component <- readRDS("data/non_salary/bonuses_and_benefits_component.rds") %>%
        dplyr::filter(.data$wage == wage_val, .data$component == groupD) %>%
        dplyr::filter(is.null(country_names_sel) || .data$country %in% country_names_sel) %>%
        dplyr::transmute(country = .data$country, Scenario = ifelse(grepl("_min$", .data$min_max_component), "Min", "Max"), value = .data$value)
    } else if (groupC == "bonuses_and_benefits") {
      df_component <- readRDS("data/non_salary/bonuses_and_benefits_all.rds") %>%
        dplyr::filter(.data$wage == wage_val) %>%
        dplyr::filter(is.null(country_names_sel) || .data$country %in% country_names_sel) %>%
        dplyr::transmute(country = .data$country, Scenario = ifelse(grepl("_min$", .data$min_max_total), "Min", "Max"), value = .data$value)
    } else if (groupC == "social") {
      df_component <- readRDS(file.path("data/non_salary", paste0(groupE, "_all.rds"))) %>%
        dplyr::filter(.data$wage == wage_val) %>%
        dplyr::filter(is.null(country_names_sel) || .data$country %in% country_names_sel) %>%
        dplyr::transmute(country = .data$country, Scenario = ifelse(grepl("_min$", .data$min_max_total), "Min", "Max"), value = .data$value)
    } else if (groupC == "payroll_taxes") {
      df_component <- readRDS("data/non_salary/payroll_taxes_all.rds") %>%
        dplyr::filter(.data$wage == wage_val) %>%
        dplyr::filter(is.null(country_names_sel) || .data$country %in% country_names_sel) %>%
        dplyr::transmute(country = .data$country, Scenario = ifelse(grepl("_min$", .data$min_max_total), "Min", "Max"), value = .data$value)
    }

    if (is.null(df_component) || nrow(df_component) == 0) {
      return(plotly::plotly_empty(type = "bar"))
    }

    df_wide <- df_component %>%
      dplyr::group_by(.data$country, .data$Scenario) %>%
      dplyr::summarise(value = ifelse(.data$Scenario == "Min", min(.data$value, na.rm = TRUE), max(.data$value, na.rm = TRUE)), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = .data$Scenario, values_from = .data$value) %>%
      dplyr::arrange(.data$Min) %>%
      dplyr::mutate(
        Min = ifelse(is.na(.data$Min), 0, .data$Min),
        Max = ifelse(is.na(.data$Max), 0, .data$Max),
        country = factor(.data$country, levels = unique(.data$country))
      )

    minmax_bar_plot(df_wide, title = list(text = paste("Component:", groupC), x = 0.5, xanchor = "center"))
  })
}
