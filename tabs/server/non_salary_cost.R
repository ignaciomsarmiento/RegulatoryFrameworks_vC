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
  df_non_salary <- readRDS("data/non_salary/1. total_non_salary_costs.rds")
  df_non_salary_payer <- readRDS("data/non_salary/2. total_ns_costs_by_payer.rds")
  df_country_labels <- readRDS("data/non_salary/total_non_salary_costs.rds") %>%
    dplyr::distinct(.data$country, .data$country_name) %>%
    dplyr::arrange(.data$country_name)

  selected_view <- shiny::reactiveVal("total")

  observeEvent(input$btn_total, {
    selected_view("total")
  })

  observeEvent(input$btn_payer, {
    selected_view("payer")
  })

  observeEvent(input$btn_component, {
    selected_view("component")
  })

  output$country_selection <- renderUI({
    div(
      class = "pretty-select",
      selectizeInput(
        inputId = session$ns("country_selection_user"),
        label = "Country Analysis by:",
        choices = c(
          "All" = "All",
          stats::setNames(df_country_labels$country, df_country_labels$country_name)
        ),
        selected = "All",
        multiple = TRUE
      )
    )
  })

  observeEvent(input$country_selection_user, {
    if (is.null(input$country_selection_user) || length(input$country_selection_user) == 0) {
      return()
    }
    if (length(input$country_selection_user) > 1 && "All" %in% input$country_selection_user) {
      updateSelectizeInput(
        session = session,
        inputId = "country_selection_user",
        selected = setdiff(input$country_selection_user, "All")
      )
    }
  }, ignoreInit = TRUE)

  output$plot <- plotly::renderPlotly({
    view <- selected_view()

    wage_code <- input$mw_selection
    if (is.null(wage_code) || is.na(wage_code) || wage_code == "") {
      wage_code <- "1sm"
    }
    wage_filter <- paste0(sub("sm$", "", wage_code), " MW")

    countries_sel <- input$country_selection_user
    if (is.null(countries_sel) || length(countries_sel) == 0) {
      countries_sel <- "All"
    }

    if (view == "component") {
      return(
        plotly::plotly_empty(type = "bar") %>%
          plotly::layout(
            title = list(
              text = "BY COMPONENT not active yet",
              x = 0.5,
              xanchor = "center"
            ),
            font = list(family = "Source Sans Pro")
          )
      )
    }

    initial <- if (view == "payer") "payer" else "total"

    tryCatch(
      estatest_nuevo_grafico(
        df_non_salary = df_non_salary,
        df_non_salary_payer = df_non_salary_payer,
        wage_filter = wage_filter,
        countries = countries_sel,
        country_labels = df_country_labels,
        font_family = "Source Sans Pro",
        use_patterns = TRUE,
        initial_view = initial
      ),
      error = function(e) {
        estatest_nuevo_grafico(
          df_non_salary = df_non_salary,
          df_non_salary_payer = df_non_salary_payer,
          wage_filter = wage_filter,
          countries = countries_sel,
          country_labels = df_country_labels,
          font_family = "Source Sans Pro",
          use_patterns = FALSE,
          initial_view = initial
        )
      }
    )
  })
  
  
  
}
