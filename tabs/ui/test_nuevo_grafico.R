library(dplyr)
library(tidyr)
library(plotly)
library(tictoc)

# ------------------------------------------------------------------
tic("1 carga datos")
df_non_salary <- readRDS("data/non_salary/1. total_non_salary_costs.rds")
df_non_salary_payer <- readRDS("data/non_salary/2. total_ns_costs_by_payer.rds")
toc()

# ------------------------------------------------------------------
# PRocesamiento
tic("2 procesamiento datos")
# Filtrar por wage
wage_filter <- "1 MW"

# ------------------------------------------------------------------
# STYLE
# ------------------------------------------------------------------
COL_MIN <- "#00C1FF"  # mínimo (base)
COL_MIN_Employee <- "#00C1FF"  # mínimo (employee)
COL_MIN_Employer <- "#002244"  # mínimo (employer)
COL_MAX <- "#002244"  # máximo (base)
COL_MAX_Employee <- "#00C1FF"  # máximo (employee)
COL_MAX_Employer <- "#002244"  # máximo (employer)

# Texturas (requiere plotly.js con soporte de marker.pattern; si tu plotly es viejo, esto no funcionará)
PAT_EMPLOYER <- "/"   # rayado
PAT_EMPLOYEE <- "x"   # cuadriculado (crosshatch)

FONT_FAMILY <- "National Park"  # en Shiny, asegúrate de cargar la font (ver comentario al final)

# ------------------------------------------------------------------
# 1) TOTAL wide (define el orden por t_min)
# ------------------------------------------------------------------
df_total_wide <- df_non_salary %>%
  filter(wage == wage_filter) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  arrange(t_min) %>%
  mutate(country = as.character(country))

order_country <- df_total_wide$country

# x_levels intercalado country_Min, country_Max, ...
x_levels <- as.vector(t(cbind(
  paste(order_country, "Min", sep="___"),
  paste(order_country, "Max", sep="___")
)))

# TOTAL long con xcat
df_total_long <- df_non_salary %>%
  filter(wage == wage_filter) %>%
  select(country, type, value) %>%
  mutate(
    country  = factor(as.character(country), levels = order_country),
    Scenario = ifelse(type == "t_min", "Min", "Max"),
    Scenario = factor(Scenario, levels = c("Min", "Max")),
    xcat     = paste(as.character(country), as.character(Scenario), sep="___")
  ) %>%
  select(Scenario, xcat, value)

# TOTAL en grilla xcat (para mostrar Min/Max en eje)
total_min_full <- df_total_long %>%
  filter(Scenario == "Min") %>%
  select(xcat, value) %>%
  right_join(data.frame(xcat = x_levels), by = "xcat") %>%
  mutate(xcat = factor(xcat, levels = x_levels)) %>%
  arrange(xcat)

total_max_full <- df_total_long %>%
  filter(Scenario == "Max") %>%
  select(xcat, value) %>%
  right_join(data.frame(xcat = x_levels), by = "xcat") %>%
  mutate(xcat = factor(xcat, levels = x_levels)) %>%
  arrange(xcat)

# ------------------------------------------------------------------
# 2) WHO PAYS -> long y luego 4 series (Min/Max x Employer/Employee)
# ------------------------------------------------------------------
df_payer_long <- df_non_salary_payer %>%
  filter(wage == wage_filter) %>%
  select(country, type_by_payer, value) %>%
  mutate(
    country  = factor(as.character(country), levels = order_country),
    Scenario = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
    payer    = ifelse(grepl("^st_er", type_by_payer), "Employer", "Employee"),
    Scenario = factor(Scenario, levels = c("Min", "Max")),
    xcat     = paste(as.character(country), as.character(Scenario), sep="___")
  ) %>%
  select(xcat, Scenario, payer, value)

mk_grid <- function(df) {
  df %>%
    select(xcat, value) %>%
    right_join(data.frame(xcat = x_levels), by="xcat") %>%
    mutate(xcat = factor(xcat, levels = x_levels)) %>%
    arrange(xcat)
}

payer_er_min <- df_payer_long %>% filter(payer=="Employer", Scenario=="Min") %>% mk_grid()
payer_ee_min <- df_payer_long %>% filter(payer=="Employee", Scenario=="Min") %>% mk_grid()
payer_er_max <- df_payer_long %>% filter(payer=="Employer", Scenario=="Max") %>% mk_grid()
payer_ee_max <- df_payer_long %>% filter(payer=="Employee", Scenario=="Max") %>% mk_grid()

# Evitar rarezas de factor en plotly
total_min_full$xcat <- as.character(total_min_full$xcat)
total_max_full$xcat <- as.character(total_max_full$xcat)
payer_er_min$xcat <- as.character(payer_er_min$xcat)
payer_ee_min$xcat <- as.character(payer_ee_min$xcat)
payer_er_max$xcat <- as.character(payer_er_max$xcat)
payer_ee_max$xcat <- as.character(payer_ee_max$xcat)

# ------------------------------------------------------------------
# 3) Eje Min/Max + orden FORZADO
# ------------------------------------------------------------------
xaxis_minmax <- list(
  title = "",
  type = "category",
  categoryorder = "array",
  categoryarray = x_levels,
  tickmode = "array",
  tickvals = x_levels,
  ticktext = rep(c("Min","Max"), times = length(order_country)),
  tickangle = 90,
  showgrid = FALSE, zeroline = FALSE, showline = FALSE
)

country_ann <- lapply(seq_along(order_country), function(i) {
  list(
    x = paste(order_country[i], "Min", sep="___"),
    y = -6,
    text = order_country[i],
    showarrow = FALSE,
    xref = "x",
    yref = "y",
    xshift = 18,
    font = list(size = 11, color = "#000000", family = FONT_FAMILY)
  )
})

# Visibilidad:
# 2 traces total + 4 traces payer = 6
vis_total <- c(TRUE, TRUE,  FALSE, FALSE, FALSE, FALSE)
vis_payer <- c(FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE)

title_total <- list(
  text = "<b>Non-salary costs</b><br><span style='font-size:14px'>Total (Min / Max)</span>",
  x = 0.5, xanchor = "center",
  y = 0.98, yanchor = "top"
)

title_payer <- list(
  text = "<b>Non-salary costs</b><br><span style='font-size:14px'>Who pays (Employer vs Employee)</span>",
  x = 0.5, xanchor = "center",
  y = 0.98, yanchor = "top"
)

toc()
# ------------------------------------------------------------------
# 4) Plot
#   - TOTAL: solo 1 item de leyenda "Total" (la barra min)
#   - WHO PAYS: solo "Employer" y "Employee" (dos items)
#   - Leyenda abajo
#   - Font National Park
#   - Colores por Min/Max + texturas por Employer/Employee
# ------------------------------------------------------------------
tic("3 plotly")
fig <- plot_ly() %>%
  # TOTAL (2 trazas) — leyenda: solo una (Total)
  add_bars(
    data = total_min_full, x = ~xcat, y = ~value,
    name = "Total",
    visible = TRUE,
    showlegend = TRUE,
    marker = list(color = COL_MIN)
  ) %>%
  add_bars(
    data = total_max_full, x = ~xcat, y = ~value,
    name = "Total",
    visible = TRUE,
    showlegend = FALSE,  # <- no duplicar en leyenda
    marker = list(color = COL_MAX)
  ) %>%
  
  # WHO PAYS (4 trazas) — leyenda: solo Employer/Employee (en las trazas Min)
  add_bars(
    data = payer_er_min, x = ~xcat, y = ~value,
    name = "Employer",
    visible = FALSE,
    legendgroup = "Employer",
    showlegend = TRUE,
    marker = list(
      color = COL_MIN_Employer,
      pattern = list(
        #shape = PAT_EMPLOYER,
        fillmode = "overlay",
        fgcolor = "rgba(0,0,0,0.35)"
      )
    )
  ) %>%
  add_bars(
    data = payer_ee_min, x = ~xcat, y = ~value,
    name = "Employee",
    visible = FALSE,
    legendgroup = "Employee",
    showlegend = TRUE,
    marker = list(
      color = COL_MIN_Employee,
      pattern = list(
        #shape = PAT_EMPLOYEE,
        fillmode = "overlay",
        fgcolor = "rgba(0,0,0,0.35)"
      )
    )
  ) %>%
  add_bars(
    data = payer_er_max, x = ~xcat, y = ~value,
    name = "Employer",
    visible = FALSE,
    legendgroup = "Employer",
    showlegend = FALSE,
    marker = list(
      color = COL_MAX_Employer,
      pattern = list(
        #shape = PAT_EMPLOYER,
        fillmode = "overlay",
        fgcolor = "rgba(255,255,255,0.30)"
      )
    )
  ) %>%
  add_bars(
    data = payer_ee_max, x = ~xcat, y = ~value,
    name = "Employee",
    visible = FALSE,
    legendgroup = "Employee",
    showlegend = FALSE,
    marker = list(
      color = COL_MAX_Employee,
      pattern = list(
        #shape = PAT_EMPLOYEE,
        fillmode = "overlay",
        fgcolor = "rgba(255,255,255,0.30)"
      )
    )
  ) %>%
  layout(
    title = title_total,
    barmode = "group",
    xaxis = xaxis_minmax,
    annotations = country_ann,
    font = list(family = FONT_FAMILY),
    legend = list(
      orientation = "h",
      x = 0.5, xanchor = "center",
      y = -0.22, yanchor = "top"
    ),
    margin = list(b = 130, t = 90, l = 60, r = 20),  # un poco más de top para el título
    yaxis = list(
      title = "Non-salary costs as share of wages (%)",
      showgrid = FALSE, zeroline = FALSE, showline = FALSE
    ),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        showactive = TRUE,
        x = 0, y = 1.18,
        buttons = list(
          list(
            label = "TOTAL",
            method = "update",
            args = list(
              list(visible = vis_total),
              list(
                title = title_total,        # <- AQUÍ va el title
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
                title = title_payer,        # <- AQUÍ va el title
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
toc()
fig
#1 carga datos: 0.003 sec elapsed
#2 procesamiento datos: 1.684 sec elapsed
#3 plotly: 0.108 sec elapsed
# ------------------------------------------------------------------
# NOTE (Shiny): para que "National Park" funcione, carga la font en UI:
# tags$head(
#   tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
#   tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin="anonymous"),
#   tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=National+Park:wght@400;600&display=swap")
# )
# ------------------------------------------------------------------

# Guardar el gráfico como un archivo HTML
htmlwidgets::saveWidget(fig, "test_nuevo_grafico.html")
