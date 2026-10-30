# =====================================================================
#  Exploratory Analysis of Road Accident Patterns in India Using R
#  Interactive Shiny dashboard
#  Run:  shiny::runApp("road_accident_dashboard")   (or "Run App" in RStudio)
# =====================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib)
  library(dplyr); library(tidyr)
  library(ggplot2); library(plotly)
  library(DT); library(scales)
})

source("scripts/data_prep.R")
prep      <- load_accidents("data/indian_roads_dataset.csv")
acc       <- prep$data
clean_log <- prep$log

# ---------------------------------------------------------------------
# Constants & lookup tables
# ---------------------------------------------------------------------
PAL <- list(navy = "#0B1F3A", blue = "#1F4E79", amber = "#F5A623",
            teal = "#2A9D8F", red = "#D62839", ink = "#1A2B42", muted = "#64748B")
SEV_COLS <- c(Minor = "#2A9D8F", Major = "#F4A261", Fatal = "#D62839")
FONT     <- "Inter, 'Segoe UI', system-ui, -apple-system, Roboto, Arial, sans-serif"

# Which months does each year actually cover?  (2025 is partial)
coverage <- acc |> distinct(year, month)
yr_cov   <- coverage |> group_by(year) |>
  summarise(n_months = n(), lab = paste0(month.abb[min(month)], "-", month.abb[max(month)]),
            .groups = "drop") |>
  mutate(partial = n_months < 12)
YEARS         <- sort(unique(acc$year))
PARTIAL_YEARS <- yr_cov$year[yr_cov$partial]
year_choices  <- setNames(YEARS, ifelse(YEARS %in% PARTIAL_YEARS,
                                        paste0(YEARS, " (partial)"), YEARS))
CITIES   <- sort(unique(acc$city))
CAUSES   <- sort(unique(acc$cause))
WEATHERS <- sort(unique(acc$weather))
ROADS    <- sort(unique(acc$road_type))
TODS     <- levels(acc$time_of_day)
SEVS     <- levels(acc$severity)

FACTOR_LABELS <- c(Weather = "weather", `Road type` = "road_type", Cause = "cause",
                   `Traffic density` = "traffic_density", Visibility = "visibility",
                   City = "city", `Time of day` = "time_of_day",
                   `Day of week` = "weekday", `Weekday / weekend` = "is_weekend")

cause_actions <- c(
  "Overspeeding"  = "Strengthen speed enforcement (cameras, radar, average-speed zones), especially on highways.",
  "Drunk Driving" = "Run targeted breath-analyser checkpoints and awareness drives, focused on late-night and weekend hours.",
  "Distraction"   = "Enforce mobile-phone / distracted-driving rules and run driver-attention campaigns.",
  "Poor Road"     = "Audit and repair road surfaces, markings and signage at the locations with the most records.",
  "Weather"       = "Add fog / rain warning signs and variable-message boards; promote reduced-speed advice in poor weather."
)

# ---------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------
pc    <- function(x, d = 1) round(100 * x, d)
cm    <- function(x) comma(x)
group_count <- function(d, col) {
  d |> group_by(cat = .data[[col]]) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(share = 100 * n / sum(n))
}

theme_dash <- function(base = 12) {
  theme_minimal(base_size = base, base_family = "sans") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#E8ECF3"),
          axis.title = element_text(colour = PAL$muted, size = base - 1),
          axis.text  = element_text(colour = PAL$ink),
          legend.position = "bottom", legend.title = element_blank(),
          plot.margin = margin(6, 10, 4, 4))
}

# Convert a ggplot to a clean interactive plotly widget
gg2ply <- function(p, legend = FALSE) {
  w <- suppressWarnings(ggplotly(p, tooltip = "text")) |>
    config(displayModeBar = FALSE) |>
    layout(font = list(family = FONT, color = PAL$ink),
           hoverlabel = list(font = list(family = FONT, size = 13)),
           showlegend = legend)
  if (legend) w <- w |> layout(legend = list(orientation = "h", x = 0.5, xanchor = "center",
                                             y = -0.3, title = list(text = "")),
                               margin = list(b = 90))
  w
}

# Generic bar chart: df needs cat, val, tip.   Highest bar is highlighted.
bar_plot <- function(df, horizontal = FALSE, sort = TRUE, ytitle = "Recorded accidents",
                     fill = PAL$blue, label_acc = 1) {
  if (sort) {
    df <- df |> mutate(cat = factor(as.character(cat),
                                    levels = if (horizontal) as.character(cat)[order(val)]
                                             else as.character(cat)[order(-val)]))
  } else if (horizontal) {
    df <- df |> mutate(cat = factor(cat, levels = rev(levels(factor(cat)))))
  }
  df <- df |> mutate(hi = val == max(val))
  p <- ggplot(df, aes(cat, val, fill = hi, text = tip)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = c(`TRUE` = PAL$amber, `FALSE` = fill), guide = "none") +
    scale_y_continuous(labels = label_comma(accuracy = label_acc),
                       expand = expansion(mult = c(0, 0.08))) +
    labs(x = NULL, y = ytitle) + theme_dash()
  if (horizontal) p <- p + coord_flip() +
    theme(panel.grid.major.x = element_line(colour = "#E8ECF3"),
          panel.grid.major.y = element_blank())
  gg2ply(p)
}

# Standard count table -> bar chart input
count_df <- function(d, col, unit = "Accidents") {
  group_count(d, col) |>
    mutate(val = n,
           tip = paste0("<b>", cat, "</b><br>", unit, ": ", cm(n), "<br>Share: ", round(share, 1), "%"))
}

# 100% stacked severity mix by a chosen factor
mix_plot <- function(d, col) {
  nat <- is.factor(d[[col]])
  m <- d |> group_by(cat = as.character(.data[[col]]), severity) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(cat) |> mutate(pct = 100 * n / sum(n), tot = sum(n)) |> ungroup()
  ord <- if (nat) rev(levels(droplevels(d[[col]])))
         else { f <- m |> filter(severity == "Fatal") |> arrange(pct); f$cat }
  m <- m |> mutate(cat = factor(cat, levels = ord),
                   severity = factor(severity, levels = c("Fatal", "Major", "Minor")),
                   tip = paste0("<b>", cat, "</b><br>", severity, ": ", round(pct, 1), "%<br>Records: ", cm(n), " of ", cm(tot)))
  p <- ggplot(m, aes(cat, pct, fill = severity, text = tip)) +
    geom_col(width = 0.72) +
    geom_text(data = filter(m, pct >= 7), aes(label = paste0(round(pct), "%")),
              position = position_stack(vjust = 0.5), colour = "white", size = 3.2) +
    scale_fill_manual(values = SEV_COLS) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    coord_flip() + labs(x = NULL, y = "Share of accidents in category") + theme_dash() +
    theme(panel.grid.major.x = element_line(colour = "#E8ECF3"), panel.grid.major.y = element_blank())
  gg2ply(p, legend = TRUE)
}

# Heatmap with cell labels (counts)
heat_plot <- function(z, xlab = "", ylab = "", labels = TRUE, reverse_y = FALSE) {
  ann <- NULL
  if (labels) {
    ann <- list()
    for (i in seq_len(nrow(z))) for (j in seq_len(ncol(z)))
      ann[[length(ann) + 1]] <- list(x = colnames(z)[j], y = rownames(z)[i], text = cm(z[i, j]),
                                     showarrow = FALSE,
                                     font = list(size = 12, color = ifelse(z[i, j] > mean(range(z)), "white", PAL$ink)))
  }
  plot_ly(x = colnames(z), y = rownames(z), z = unclass(z), type = "heatmap",
          colorscale = list(c(0, "#EAF1FB"), c(1, PAL$blue)), showscale = FALSE,
          xgap = 2, ygap = 2,
          hovertemplate = paste0("%{y} / %{x}<br><b>%{z:,}</b> accidents<extra></extra>")) |>
    layout(xaxis = list(title = xlab, type = "category"),
           yaxis = list(title = ylab, autorange = if (reverse_y) "reversed" else TRUE),
           annotations = ann, font = list(family = FONT, color = PAL$ink),
           margin = list(l = 10, r = 10, t = 10, b = 10)) |>
    config(displayModeBar = FALSE)
}

# Statistical tests (chi-square) for the dashboard / findings
stat_tests <- function(d) {
  rows <- lapply(names(FACTOR_LABELS), function(lbl) {
    col <- FACTOR_LABELS[[lbl]]
    x <- droplevels(factor(d[[col]])); y <- droplevels(d$severity)
    if (nlevels(x) < 2 || nlevels(y) < 2 || nrow(d) < 30) return(NULL)
    tb  <- table(x, y)
    k   <- nlevels(x)
    pr  <- if (col == "is_weekend" && k == 2) c(5/7, 2/7) else rep(1 / k, k)
    gof <- suppressWarnings(chisq.test(table(x), p = pr))$p.value
    ct  <- suppressWarnings(chisq.test(tb))
    v   <- sqrt(unname(ct$statistic) / (sum(tb) * (min(dim(tb)) - 1)))
    data.frame(Factor = lbl, Levels = nlevels(x),
               `Even counts p` = gof, `Severity link p` = ct$p.value, `Cramers V` = v,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- bind_rows(rows)
  if (nrow(out) == 0) return(out)
  out |> mutate(
    `Counts even?`  = ifelse(`Even counts p` >= 0.05, "Yes (no sig. difference)", "No (uneven)"),
    `Linked to severity?` = ifelse(`Severity link p` < 0.05, "Yes (significant)", "No (not significant)"),
    `Effect size` = cut(`Cramers V`, c(-Inf, 0.1, 0.3, 0.5, Inf),
                        labels = c("Negligible", "Small", "Medium", "Large")))
}

# UI building blocks --------------------------------------------------
chart_card <- function(title, out_id, height = "320px", sub = NULL, controls = NULL, footer = NULL) {
  card(full_screen = TRUE,
       card_header(tags$span(title), if (!is.null(sub)) tags$small(sub)),
       if (!is.null(controls)) div(class = "card-controls", controls),
       plotlyOutput(out_id, height = height),
       if (!is.null(footer)) div(class = "card-foot", footer))
}

kpi <- function(icon_name, label, value, sub, colour = PAL$blue) {
  div(class = "kpi", style = paste0("--c:", colour),
      div(class = "ic", icon(icon_name)),
      div(class = "kb", div(class = "kl", label), div(class = "kv", value), div(class = "ks", sub)))
}

finding <- function(icon_name, title, ..., kind = "finding") {
  div(class = kind, div(class = "f-ic", icon(icon_name)),
      div(class = "f-body", tags$h6(title), tags$p(...)))
}

# ---------------------------------------------------------------------
# Theme + CSS
# ---------------------------------------------------------------------
app_theme <- bs_theme(
  version = 5, bg = "#F3F5F9", fg = PAL$ink, primary = PAL$blue, secondary = PAL$amber,
  base_font = font_collection("Inter", "Segoe UI", "system-ui", "-apple-system", "Roboto", "Arial", "sans-serif"),
  "navbar-bg" = PAL$navy
)

css <- "
:root{--navy:#0B1F3A;--blue:#1F4E79;--amber:#F5A623;--red:#D62839;--teal:#2A9D8F;--ink:#1A2B42;--muted:#64748B}
body{background:#F3F5F9}
.navbar{box-shadow:0 2px 12px rgba(11,31,58,.3)}
.navbar-brand{font-weight:700;letter-spacing:.2px}
.navbar .nav-link{font-weight:500}
.navbar .nav-link.active{border-bottom:3px solid var(--amber)}
.bslib-sidebar-layout > .sidebar{background:#fff}
.card{--bs-card-bg:#fff;background:#fff;border:0;border-radius:14px;box-shadow:0 1px 3px rgba(16,24,40,.06),0 6px 18px rgba(16,24,40,.05)}
.card-header{background:transparent;border-bottom:1px solid #EEF1F6;font-weight:600;font-size:.95rem;color:var(--ink)}
.card-header small{color:var(--muted);font-weight:400;margin-left:.6rem}
.card-controls{padding:.4rem 1rem 0 1rem;font-size:.85rem}
.card-controls .form-group,.card-controls .shiny-input-container{margin-bottom:0}
.card-foot{padding:.2rem 1rem .6rem 1rem;font-size:.78rem;color:var(--muted)}
.side-h{font-weight:700;font-size:.8rem;letter-spacing:.08em;text-transform:uppercase;color:var(--muted);margin:.2rem 0 .6rem}
.sidebar .control-label{font-weight:600;font-size:.85rem;color:var(--ink);margin-bottom:.15rem}
.sidebar .shiny-input-container{margin-bottom:.8rem}
.sidebar .checkbox label,.sidebar .radio label{font-size:.85rem}
.sel-box{background:#F3F5F9;border-radius:10px;padding:.7rem .8rem;margin-top:.6rem}
.sel-box .big{font-size:1.35rem;font-weight:700;color:var(--blue)}
.sel-box .bar{height:6px;background:#DDE3EE;border-radius:4px;margin-top:.35rem;overflow:hidden}
.sel-box .bar > div{height:100%;background:linear-gradient(90deg,var(--blue),var(--amber))}
.hero{background:linear-gradient(120deg,#0B1F3A 0%,#17406E 60%,#1F4E79 100%);color:#fff;border-radius:16px;padding:22px 26px;margin-bottom:16px;position:relative;overflow:hidden}
.hero:after{content:'';position:absolute;right:-60px;top:-60px;width:240px;height:240px;border-radius:50%;background:rgba(245,166,35,.18)}
.hero h2{font-weight:700;font-size:1.55rem;margin:0 0 .25rem 0}
.hero p{margin:0 0 .7rem 0;opacity:.85;max-width:780px}
.chip{display:inline-block;background:rgba(255,255,255,.14);border:1px solid rgba(255,255,255,.22);border-radius:20px;padding:.18rem .7rem;font-size:.78rem;margin:0 .35rem .25rem 0}
.kpi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:14px;margin-bottom:16px}
.kpi{background:#fff;border-radius:14px;padding:14px 16px;display:flex;gap:14px;align-items:center;border-left:5px solid var(--c);box-shadow:0 1px 3px rgba(16,24,40,.06),0 6px 18px rgba(16,24,40,.05)}
.kpi .ic{flex:0 0 44px;height:44px;border-radius:12px;background:var(--c);color:#fff;display:flex;align-items:center;justify-content:center;font-size:1.1rem}
.kpi .kl{font-size:.7rem;letter-spacing:.07em;text-transform:uppercase;color:var(--muted);font-weight:600}
.kpi .kv{font-size:1.45rem;font-weight:700;line-height:1.15;color:var(--ink)}
.kpi .ks{font-size:.76rem;color:var(--muted)}
.banner{background:#FFF6E5;border:1px solid #F7D9A0;color:#7A4B00;border-radius:10px;padding:.55rem .9rem;font-size:.85rem;margin-bottom:14px}
.banner.info{background:#EAF2FB;border-color:#C5D9F0;color:#1F4E79}
.finding,.rec{display:flex;gap:14px;background:#fff;border-radius:12px;padding:14px 16px;margin-bottom:12px;box-shadow:0 1px 3px rgba(16,24,40,.06),0 4px 12px rgba(16,24,40,.04);border-left:4px solid var(--amber)}
.rec{border-left-color:var(--teal)}
.f-ic{flex:0 0 38px;height:38px;border-radius:10px;background:#FFF2D9;color:#B97800;display:flex;align-items:center;justify-content:center}
.rec .f-ic{background:#DDF3F0;color:#1C7B70}
.f-body h6{margin:0 0 .15rem 0;font-weight:700}
.f-body p{margin:0;color:#3B4A60;font-size:.92rem}
.section-h{font-weight:700;margin:.4rem 0 .8rem 0;color:var(--ink)}
.dataTables_wrapper{font-size:.85rem}
.about-list li{margin-bottom:.35rem}
.nav-link{white-space:nowrap}
.card .datatables,.card .dataTables_wrapper{height:auto !important;min-height:0}
"

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(icon("road"), " Road Accident Patterns \u00b7 India"),
  theme = app_theme, bg = PAL$navy, inverse = TRUE, fillable = FALSE,
  window_title = "Road Accident Patterns - India (R Shiny)",
  header = tags$head(tags$style(HTML(css))),

  sidebar = sidebar(
    width = 300,
    div(class = "side-h", icon("filter"), " Filters"),
    checkboxGroupInput("f_year", "Year", choices = year_choices, selected = YEARS, inline = TRUE),
    selectizeInput("f_city", "City", choices = CITIES, selected = CITIES, multiple = TRUE,
                   options = list(plugins = list("remove_button"), placeholder = "Select cities")),
    checkboxGroupInput("f_weather", "Weather", choices = WEATHERS, selected = WEATHERS, inline = TRUE),
    checkboxGroupInput("f_road", "Road type", choices = ROADS, selected = ROADS, inline = TRUE),
    checkboxGroupInput("f_sev", "Severity", choices = SEVS, selected = SEVS, inline = TRUE),
    selectizeInput("f_cause", "Reported cause", choices = CAUSES, selected = CAUSES, multiple = TRUE,
                   options = list(plugins = list("remove_button"), placeholder = "Select causes")),
    checkboxGroupInput("f_tod", "Time of day", choices = TODS, selected = TODS),
    actionButton("reset", "Reset all filters", icon = icon("rotate-left"),
                 class = "btn-outline-primary btn-sm w-100"),
    uiOutput("sel_box")
  ),

  # ---------------- Overview ----------------
  nav_panel("Overview", icon = icon("gauge-high"),
    div(class = "hero",
        h2("Exploratory Analysis of Road Accident Patterns in India"),
        p("An interactive R Shiny dashboard exploring when, where and under which conditions accidents are recorded, and how severe they are."),
        span(class = "chip", icon("calendar"), paste0(" ", format(min(acc$date), "%d %b %Y"), " - ", format(max(acc$date), "%d %b %Y"))),
        span(class = "chip", icon("database"), paste0(" ", cm(nrow(acc)), " records")),
        span(class = "chip", icon("map-pin"), paste0(" ", length(CITIES), " cities")),
        span(class = "chip", icon("graduation-cap"), " Data Science using R")),
    uiOutput("year_banner"),
    uiOutput("kpis"),
    layout_columns(col_widths = c(7, 5),
      chart_card("Year-wise accident trend", "p_year", "330px", sub = "recorded accidents per year"),
      chart_card("Severity distribution", "p_sev_donut", "330px")),
    layout_columns(col_widths = c(4, 4, 4),
      chart_card("Accidents by city", "p_city_ov", "320px"),
      chart_card("Accidents by hour of day", "p_hour_ov", "320px"),
      chart_card("Reported causes", "p_cause_ov", "320px"))
  ),

  # ---------------- Time patterns ----------------
  nav_panel("Time Patterns", icon = icon("clock"),
    layout_columns(col_widths = c(6, 6),
      chart_card("Month-wise pattern", "p_month", "320px",
                 controls = radioButtons("month_mode", NULL, inline = TRUE,
                                         choices = c("Average per year" = "avg", "Total records" = "total")),
                 footer = "2025 only covers Jan to mid-Apr, so 'Average per year' divides each month by the number of selected years that cover it. Totals alone would make Jan-Apr look busier than other months."),
      chart_card("Hour-of-day pattern", "p_hour", "320px", sub = "00:00 - 23:00")),
    layout_columns(col_widths = c(5, 7),
      chart_card("Day-of-week pattern", "p_weekday", "330px"),
      chart_card("When do accidents cluster? Weekday x hour", "p_heat", "330px", sub = "darker = more records",
                 footer = "Colour runs from the lowest to the highest cell, which makes small differences look bigger. Hover for the actual counts.")),
    chart_card("Monthly timeline", "p_timeline", "300px", sub = "records per calendar month",
               footer = "The final month (April 2025) is incomplete in the source file.")
  ),

  # ---------------- Locations ----------------
  nav_panel("Locations", icon = icon("map-location-dot"),
    layout_columns(col_widths = c(7, 5),
      chart_card("Where accidents were recorded", "p_map", "460px",
                 controls = radioButtons("map_mode", NULL, inline = TRUE,
                                         choices = c("City bubbles" = "city", "Individual records (sample)" = "pts",
                                                     "Offline plot (no basemap)" = "plain")),
                 footer = "Base-map tiles load from OpenStreetMap and need an internet connection. Use the offline option if you are presenting without internet."),
      chart_card("City / state comparison", "p_geo", "400px",
                 controls = radioButtons("geo_level", NULL, inline = TRUE,
                                         choices = c("City" = "city", "State" = "state")))),
    chart_card("Severity mix by city", "p_city_mix", "360px", sub = "share of Minor / Major / Fatal in each city")
  ),

  # ---------------- Conditions ----------------
  nav_panel("Conditions & Causes", icon = icon("cloud-rain"),
    layout_columns(col_widths = c(3, 3, 3, 3),
      chart_card("Weather", "p_weather", "280px"),
      chart_card("Road type", "p_road", "280px"),
      chart_card("Traffic density", "p_traffic", "280px"),
      chart_card("Visibility", "p_vis", "280px")),
    layout_columns(col_widths = c(5, 7),
      chart_card("Reported causes", "p_cause", "340px"),
      chart_card("Severity mix by condition", "p_mix", "340px",
                 controls = selectInput("mix_by", NULL, width = "260px",
                                        choices = c("Weather" = "weather", "Road type" = "road_type",
                                                    "Cause" = "cause", "Traffic density" = "traffic_density",
                                                    "Visibility" = "visibility", "Time of day" = "time_of_day",
                                                    "Day of week" = "weekday", "City" = "city")))),
    chart_card("Reported cause x weather", "p_cause_weather", "320px", sub = "number of accidents in each combination",
               footer = "Colour runs from the lowest to the highest cell, so shading exaggerates small differences. Read the printed counts.")
  ),

  # ---------------- Severity & stats ----------------
  nav_panel("Severity & Stats", icon = icon("heart-pulse"),
    layout_columns(col_widths = c(4, 4, 4),
      chart_card("Casualties per accident", "p_cas", "320px", sub = "stacked by severity"),
      chart_card("Average dataset risk score", "p_risk", "320px",
                 controls = selectInput("risk_by", NULL, width = "220px",
                                        choices = c("Weather" = "weather", "Road type" = "road_type",
                                                    "Cause" = "cause", "Traffic density" = "traffic_density",
                                                    "Severity" = "severity")),
                 footer = "risk_score is a field supplied in the CSV (0.1 - 1.0). How it was calculated is not documented."),
      chart_card("Average casualties & vehicles by severity", "p_sev_avg", "320px")),
    card(fill = FALSE, card_header("Statistical checks", tags$small("chi-square tests on the currently selected records")),
         card_body(fillable = FALSE, fill = FALSE, DTOutput("tbl_stats")),
         div(class = "card-foot",
             HTML("<b>Even counts p</b>: tests whether records are spread equally across the categories. <b>Severity link p</b>: tests whether severity depends on the factor (p &lt; 0.05 = significant). <b>Cram\u00e9r's V</b> measures the size of any link (&lt; 0.1 = negligible). Weekday / weekend is tested against the expected 5:2 split. Traffic density and visibility are uneven by design in the file. Statistical significance shows association only, not cause.")))
  ),

  # ---------------- Findings ----------------
  nav_panel("Findings", icon = icon("lightbulb"),
    layout_columns(col_widths = c(7, 5),
      div(h5(class = "section-h", "Key findings (update with your filters)"), uiOutput("findings_ui")),
      div(h5(class = "section-h", "Suggested road-safety actions"), uiOutput("recs_ui"),
          h5(class = "section-h", style = "margin-top:18px", "Limitations"),
          div(class = "banner info", HTML(
            "<ul style='margin:0;padding-left:1.1rem'>
             <li>Counts show where records exist, <b>not risk</b>: no traffic-volume or population data is available.</li>
             <li>Associations in this dashboard do not prove causation.</li>
             <li>The source is a community-hosted CSV, not verified as an official record.</li>
             <li>2025 contains only Jan - mid-Apr, so it is not comparable with full years.</li></ul>")))
    )
  ),

  # ---------------- Data explorer ----------------
  nav_panel("Data Explorer", icon = icon("table"),
    card(fill = FALSE, card_header("Filtered accident records",
                     downloadButton("dl", "Download CSV", class = "btn-sm btn-outline-primary float-end")),
         card_body(fillable = FALSE, fill = FALSE, DTOutput("tbl")))
  ),

  # ---------------- About ----------------
  nav_panel("About", icon = icon("circle-info"),
    layout_columns(col_widths = c(6, 6), fill = FALSE,
      card(fill = FALSE, card_header("Project information"),
           card_body(tags$ul(class = "about-list",
             tags$li(HTML("<b>Title:</b> Exploratory Analysis of Road Accident Patterns in India Using R")),
             tags$li(HTML("<b>Subject:</b> Data Science using R (TY B.Tech, Artificial Intelligence &amp; Data Science)")),
             tags$li(HTML("<b>Tools:</b> R, RStudio, readr, dplyr, tidyr, lubridate, ggplot2, plotly, shiny, bslib, DT")),
             tags$li(HTML("<b>Dataset:</b> Indian Road Accident Dataset (community-hosted on GitHub: Aditya2213/INDIAN_ROAD_ACCIDENT_DATASET_DASHBOARD)")),
             tags$li(HTML("<b>Verified coverage:</b> 20,000 records, 24 variables, 01 Jan 2022 - 15 Apr 2025, 8 cities in 7 states")),
             tags$li("Each row is treated as one recorded accident; the file does not state whether it is official or simulated."),
             tags$li("Workflow: collect, clean, explore, visualise, build dashboard, report.")))),
      card(fill = FALSE, card_header("Data cleaning & validation log"), card_body(fillable = FALSE, fill = FALSE, DTOutput("tbl_log")))),
    card(fill = FALSE, card_header("Data dictionary (original CSV columns)"), card_body(fillable = FALSE, fill = FALSE, DTOutput("tbl_dict")))
  )
)

# ---------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- reset button ------------------------------------------------
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "f_year", selected = YEARS)
    updateSelectizeInput(session, "f_city", selected = CITIES)
    updateCheckboxGroupInput(session, "f_weather", selected = WEATHERS)
    updateCheckboxGroupInput(session, "f_road", selected = ROADS)
    updateCheckboxGroupInput(session, "f_sev", selected = SEVS)
    updateSelectizeInput(session, "f_cause", selected = CAUSES)
    updateCheckboxGroupInput(session, "f_tod", selected = TODS)
  })

  # ---- filtered data -----------------------------------------------
  fd <- reactive({
    acc |> filter(year %in% input$f_year, city %in% input$f_city,
                  weather %in% input$f_weather, road_type %in% input$f_road,
                  severity %in% input$f_sev, cause %in% input$f_cause,
                  time_of_day %in% input$f_tod)
  })
  need_data <- function() validate(need(nrow(fd()) > 0, "No records match the current filters. Try widening them."))

  output$sel_box <- renderUI({
    n <- nrow(fd())
    div(class = "sel-box",
        div(style = "font-size:.72rem;letter-spacing:.07em;text-transform:uppercase;color:#64748B;font-weight:600", "Records selected"),
        div(class = "big", cm(n)),
        div(style = "font-size:.78rem;color:#64748B", paste0(round(100 * n / nrow(acc), 1), "% of ", cm(nrow(acc)))),
        div(class = "bar", div(style = paste0("width:", min(100, 100 * n / nrow(acc)), "%"))))
  })

  output$year_banner <- renderUI({
    if (any(input$f_year %in% PARTIAL_YEARS)) {
      lab <- yr_cov$lab[yr_cov$year %in% PARTIAL_YEARS][1]
      div(class = "banner", icon("triangle-exclamation"),
          HTML(paste0(" <b>Note:</b> ", PARTIAL_YEARS[1], " is a partial year (", lab,
                      ", ending mid-April). It is shown dashed and should not be compared directly with full years.")))
    }
  })

  # ---- KPIs --------------------------------------------------------
  output$kpis <- renderUI({
    d <- fd(); need_data()
    hr <- d |> count(hour) |> slice_max(n, n = 1, with_ties = FALSE)
    div(class = "kpi-grid",
      kpi("car", "Accidents recorded", cm(nrow(d)), paste0(round(100 * nrow(d) / nrow(acc), 1), "% of all records"), PAL$blue),
      kpi("users", "Total casualties", cm(sum(d$casualties)), paste0(round(mean(d$casualties), 2), " per accident"), PAL$teal),
      kpi("skull", "Fatal share", paste0(pc(mean(d$severity == "Fatal")), "%"), paste0(cm(sum(d$severity == "Fatal")), " fatal accidents"), PAL$red),
      kpi("map-pin", "Cities covered", n_distinct(d$city), paste0(n_distinct(d$state), " states / UTs"), PAL$navy),
      kpi("clock", "Busiest hour", sprintf("%02d:00", hr$hour), paste0(cm(hr$n), " records"), PAL$amber),
      kpi("calendar", "Years covered",
          if (n_distinct(d$year) > 1) paste0(min(d$year), "-", substr(max(d$year), 3, 4)) else as.character(min(d$year)),
          paste0(n_distinct(d$year), " year(s)", if (any(d$year %in% PARTIAL_YEARS)) ", incl. partial" else ""), "#6C5CE7"))
  })

  # =================================================================
  #  Reusable plot reactives
  # =================================================================
  hour_plot <- reactive({
    d <- fd(); need_data()
    h <- d |> count(hour) |> complete(hour = 0:23, fill = list(n = 0)) |>
      mutate(hi = n == max(n),
             tip = paste0("<b>", sprintf("%02d:00 - %02d:59", hour, hour), "</b><br>Accidents: ", cm(n),
                          "<br>Share: ", round(100 * n / sum(n), 1), "%"))
    p <- ggplot(h, aes(hour, n, fill = hi, text = tip)) + geom_col(width = 0.8) +
      scale_fill_manual(values = c(`TRUE` = PAL$amber, `FALSE` = PAL$blue), guide = "none") +
      scale_x_continuous(breaks = seq(0, 23, 3), labels = function(x) sprintf("%02d", x)) +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Hour of day (0-23)", y = "Recorded accidents") + theme_dash()
    gg2ply(p)
  })
  cause_plot <- reactive({ d <- fd(); need_data(); bar_plot(count_df(d, "cause"), horizontal = TRUE) })

  # ---- Overview ----------------------------------------------------
  output$p_year <- renderPlotly({
    d <- fd(); need_data()
    yr <- d |> count(year) |>
      left_join(yr_cov |> select(year, lab, partial), by = "year") |>
      mutate(tip = paste0("<b>", year, ifelse(partial, paste0(" (partial: ", lab, ")"), ""),
                          "</b><br>Accidents: ", cm(n)))
    solid  <- filter(yr, !partial)
    dashed <- if (any(yr$partial) && any(!yr$partial)) {
      bind_rows(filter(yr, !partial) |> slice_max(year, n = 1), filter(yr, partial))
    } else NULL
    w <- plot_ly()
    if (nrow(solid) > 1) w <- w |> add_trace(data = solid, x = ~year, y = ~n, type = "scatter", mode = "lines",
        fill = "tozeroy", fillcolor = "rgba(31,78,121,.08)", line = list(color = PAL$blue, width = 3.5),
        hoverinfo = "skip", showlegend = FALSE)
    if (!is.null(dashed) && nrow(dashed) > 1) w <- w |> add_trace(data = dashed, x = ~year, y = ~n,
        type = "scatter", mode = "lines", line = list(color = PAL$amber, width = 3.5, dash = "dash"),
        hoverinfo = "skip", showlegend = FALSE)
    w |> add_trace(data = yr, x = ~year, y = ~n, type = "scatter", mode = "markers+text",
                   text = ~cm(n), textposition = "top center", textfont = list(size = 12, color = PAL$ink),
                   hovertext = ~tip, hoverinfo = "text", showlegend = FALSE,
                   marker = list(size = 15, color = ifelse(yr$partial, PAL$amber, PAL$blue),
                                 line = list(color = "#fff", width = 2))) |>
      layout(xaxis = list(title = "", tickmode = "array", tickvals = yr$year,
                          ticktext = paste0(yr$year, ifelse(yr$partial, " *", "")), showgrid = FALSE),
             yaxis = list(title = "Recorded accidents", rangemode = "tozero", range = c(0, max(yr$n) * 1.2),
                          gridcolor = "#E8ECF3", tickformat = ","),
             annotations = if (any(yr$partial)) list(list(
               text = "* partial year", x = 1, y = -0.12, xref = "paper", yref = "paper",
               showarrow = FALSE, xanchor = "right", font = list(size = 11, color = PAL$muted))) else NULL,
             font = list(family = FONT, color = PAL$ink), margin = list(t = 20, b = 50)) |>
      config(displayModeBar = FALSE)
  })

  output$p_sev_donut <- renderPlotly({
    d <- fd(); need_data()
    s <- d |> count(severity)
    plot_ly(s, labels = ~severity, values = ~n, type = "pie", hole = 0.62, sort = FALSE,
            direction = "clockwise", textinfo = "percent",
            marker = list(colors = unname(SEV_COLS[as.character(s$severity)]), line = list(color = "#fff", width = 3)),
            hovertemplate = "<b>%{label}</b><br>%{value:,} accidents (%{percent})<extra></extra>") |>
      layout(showlegend = TRUE,
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.02),
             annotations = list(list(text = paste0("<b>", cm(nrow(d)), "</b><br>accidents"),
                                     showarrow = FALSE, x = 0.5, y = 0.5, font = list(size = 16))),
             font = list(family = FONT, color = PAL$ink), margin = list(t = 10, b = 30)) |>
      config(displayModeBar = FALSE)
  })

  output$p_city_ov  <- renderPlotly({ d <- fd(); need_data(); bar_plot(count_df(d, "city"), horizontal = TRUE) })
  output$p_hour_ov  <- renderPlotly(hour_plot())
  output$p_cause_ov <- renderPlotly(cause_plot())

  # ---- Time patterns -----------------------------------------------
  output$p_month <- renderPlotly({
    d <- fd(); need_data()
    base <- tibble(month = 1:12, cat = factor(month.abb, levels = month.abb))
    yrs  <- coverage |> filter(year %in% input$f_year) |> count(month, name = "yrs")
    m <- d |> count(month) |> right_join(base, by = "month") |> left_join(yrs, by = "month") |>
      mutate(n = coalesce(n, 0L), yrs = coalesce(yrs, 0L))
    avg <- identical(input$month_mode, "avg")
    m <- m |> mutate(val = if (avg) ifelse(yrs > 0, n / yrs, 0) else as.numeric(n),
                     tip = paste0("<b>", cat, "</b><br>", if (avg) "Average per year: " else "Total: ",
                                  comma(val, accuracy = if (avg) 0.1 else 1),
                                  "<br>Years covering this month: ", yrs))
    bar_plot(m, sort = FALSE, ytitle = if (avg) "Average accidents per year" else "Recorded accidents",
             label_acc = if (avg) 1 else 1)
  })

  output$p_hour    <- renderPlotly(hour_plot())
  output$p_weekday <- renderPlotly({
    d <- fd(); need_data()
    wk <- count_df(d, "weekday") |>
      mutate(cat = factor(substr(as.character(cat), 1, 3), levels = substr(levels(d$weekday), 1, 3)))
    bar_plot(wk, sort = FALSE)
  })

  output$p_heat <- renderPlotly({
    d <- fd(); need_data()
    z <- xtabs(~ weekday + hour, data = d |> mutate(hour = factor(hour, levels = 0:23)))
    colnames(z) <- sprintf("%02d", 0:23)
    heat_plot(z, xlab = "Hour of day", ylab = NULL, labels = FALSE, reverse_y = TRUE)
  })

  output$p_timeline <- renderPlotly({
    d <- fd(); need_data()
    mt <- d |> mutate(ym = as.Date(format(date, "%Y-%m-01"))) |> count(ym)
    plot_ly(mt, x = ~ym, y = ~n, type = "scatter", mode = "lines+markers", fill = "tozeroy",
            line = list(color = PAL$blue, width = 2.5), marker = list(color = PAL$blue, size = 6),
            fillcolor = "rgba(31,78,121,.10)",
            hovertemplate = "%{x|%b %Y}<br><b>%{y:,}</b> accidents<extra></extra>") |>
      layout(xaxis = list(title = "", showgrid = FALSE),
             yaxis = list(title = "Recorded accidents", rangemode = "tozero", gridcolor = "#E8ECF3"),
             font = list(family = FONT, color = PAL$ink), margin = list(t = 10)) |>
      config(displayModeBar = FALSE)
  })

  # ---- Locations ---------------------------------------------------
  output$p_map <- renderPlotly({
    d <- fd(); need_data()
    lay <- function(w) w |> layout(
      mapbox = list(style = "open-street-map", center = list(lon = 78.5, lat = 21.5), zoom = 3.6),
      margin = list(l = 0, r = 0, t = 0, b = 0), font = list(family = FONT),
      legend = list(orientation = "h", x = 0.01, y = 0.02, bgcolor = "rgba(255,255,255,.85)")) |>
      config(displayModeBar = FALSE, scrollZoom = TRUE)
    if (identical(input$map_mode, "pts")) {
      set.seed(42)
      s <- if (nrow(d) > 3000) slice_sample(d, n = 3000) else d
      s <- s |> mutate(tip = paste0("<b>", city, "</b> - ", severity, "<br>", format(date, "%d %b %Y"),
                                    " ", sprintf("%02d:00", hour), "<br>", cause, " / ", weather, " / ", road_type))
      plot_ly(s, type = "scattermapbox", mode = "markers", lat = ~latitude, lon = ~longitude,
              color = ~severity, colors = SEV_COLS, text = ~tip, hoverinfo = "text",
              marker = list(size = 6, opacity = 0.65)) |> lay()
    } else {
      g <- d |> group_by(city, state) |>
        summarise(n = n(), fatal = mean(severity == "Fatal"), cas = sum(casualties),
                  lat = mean(latitude), lon = mean(longitude), .groups = "drop") |>
        mutate(size = 64 * sqrt(n / max(n)),
               tip = paste0("<b>", city, "</b> (", state, ")<br>Accidents: ", cm(n),
                            "<br>Fatal share: ", round(100 * fatal, 1), "%<br>Casualties: ", cm(cas)))
      if (identical(input$map_mode, "plain")) {
        return(plot_ly(g, x = ~lon, y = ~lat, type = "scatter", mode = "markers+text",
                       text = ~city, textposition = "top center", hovertext = ~tip, hoverinfo = "text",
                       marker = list(size = ~size, color = PAL$blue, opacity = 0.75,
                                     line = list(color = "#fff", width = 2))) |>
                 layout(xaxis = list(title = "Longitude", zeroline = FALSE, gridcolor = "#E8ECF3"),
                        yaxis = list(title = "Latitude", zeroline = FALSE, gridcolor = "#E8ECF3",
                                     scaleanchor = "x", scaleratio = 1),
                        font = list(family = FONT, color = PAL$ink), showlegend = FALSE,
                        margin = list(t = 10)) |>
                 config(displayModeBar = FALSE))
      }
      plot_ly(g, type = "scattermapbox", mode = "markers", lat = ~lat, lon = ~lon,
              hovertext = ~tip, hoverinfo = "text",
              marker = list(size = ~size, color = PAL$blue, opacity = 0.75),
              showlegend = FALSE) |>
        add_trace(type = "scattermapbox", mode = "text", lat = g$lat, lon = g$lon, text = g$city,
                  textfont = list(size = 11, color = "#0B1F3A"), textposition = "top center",
                  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE) |> lay()
    }
  })

  output$p_geo <- renderPlotly({
    d <- fd(); need_data()
    col <- if (identical(input$geo_level, "state")) "state" else "city"
    bar_plot(count_df(d, col), horizontal = TRUE)
  })
  output$p_city_mix <- renderPlotly({ d <- fd(); need_data(); mix_plot(d, "city") })

  # ---- Conditions & causes -----------------------------------------
  output$p_weather <- renderPlotly({ d <- fd(); need_data(); bar_plot(count_df(d, "weather")) })
  output$p_road    <- renderPlotly({ d <- fd(); need_data(); bar_plot(count_df(d, "road_type")) })
  output$p_traffic <- renderPlotly({ d <- fd(); need_data(); bar_plot(count_df(d, "traffic_density"), sort = FALSE) })
  output$p_vis     <- renderPlotly({ d <- fd(); need_data(); bar_plot(count_df(d, "visibility"), sort = FALSE) })
  output$p_cause   <- renderPlotly(cause_plot())
  output$p_mix     <- renderPlotly({ d <- fd(); need_data(); mix_plot(d, input$mix_by) })

  output$p_cause_weather <- renderPlotly({
    d <- fd(); need_data()
    z <- xtabs(~ cause + weather, data = d)
    heat_plot(z, xlab = "Weather", ylab = NULL, labels = TRUE)
  })

  # ---- Severity & stats --------------------------------------------
  output$p_cas <- renderPlotly({
    d <- fd(); need_data()
    m <- d |> count(casualties, severity) |>
      mutate(severity = factor(severity, levels = c("Fatal", "Major", "Minor")),
             tip = paste0("<b>", casualties, " casualties</b><br>", severity, ": ", cm(n)))
    p <- ggplot(m, aes(factor(casualties), n, fill = severity, text = tip)) +
      geom_col(width = 0.72) + scale_fill_manual(values = SEV_COLS) +
      scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.08))) +
      labs(x = "Casualties in the accident", y = "Recorded accidents") + theme_dash()
    gg2ply(p, legend = TRUE)
  })

  output$p_risk <- renderPlotly({
    d <- fd(); need_data()
    col <- input$risk_by
    r <- d |> group_by(cat = .data[[col]]) |>
      summarise(val = mean(risk_score), n = n(), .groups = "drop") |>
      mutate(tip = paste0("<b>", cat, "</b><br>Avg risk score: ", round(val, 3), "<br>Records: ", cm(n)))
    bar_plot(r, sort = !is.factor(d[[col]]), ytitle = "Average risk score (0.1 - 1.0)", label_acc = 0.01)
  })

  output$p_sev_avg <- renderPlotly({
    d <- fd(); need_data()
    a <- d |> group_by(severity) |>
      summarise(Casualties = mean(casualties), Vehicles = mean(vehicles), .groups = "drop") |>
      pivot_longer(-severity, names_to = "metric", values_to = "val") |>
      mutate(tip = paste0("<b>", severity, "</b><br>Avg ", tolower(metric), ": ", round(val, 2)))
    p <- ggplot(a, aes(severity, val, fill = metric, text = tip)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.68) +
      scale_fill_manual(values = c(Casualties = PAL$red, Vehicles = PAL$blue)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = "Average per accident") + theme_dash()
    gg2ply(p, legend = TRUE)
  })

  fmtp <- function(p) ifelse(p < 0.0001, "<0.0001", formatC(p, format = "f", digits = 4))
  tests <- reactive({ d <- fd(); stat_tests(d) })
  output$tbl_stats <- renderDT({
    t <- tests(); validate(need(nrow(t) > 0, "Not enough records for statistical tests."))
    t <- t |> transmute(Factor, Levels,
                        `Even counts p` = fmtp(`Even counts p`), `Counts even?`,
                        `Severity link p` = fmtp(`Severity link p`), `Linked to severity?`,
                        CramersV = round(`Cramers V`, 3), `Effect size` = as.character(`Effect size`))
    names(t)[names(t) == "CramersV"] <- "Cram\u00e9r's V"
    datatable(t, fillContainer = FALSE, height = "auto", rownames = FALSE, class = "compact stripe hover",
              options = list(dom = "t", pageLength = 20, ordering = FALSE)) |>
      formatStyle("Linked to severity?", color = styleEqual(c("Yes (significant)", "No (not significant)"), c(PAL$red, PAL$teal)), fontWeight = "600")
  })

  # ---- Findings ----------------------------------------------------
  output$findings_ui <- renderUI({
    d <- fd(); need_data(); N <- nrow(d)
    rk <- function(col) group_count(d, col) |> arrange(desc(n))
    city <- rk("city"); wx <- rk("weather"); rd <- rk("road_type"); cs <- rk("cause")
    hr   <- d |> count(hour) |> complete(hour = 0:23, fill = list(n = 0))
    wd   <- rk("weekday"); tod <- rk("time_of_day")
    hmax <- hr[which.max(hr$n), ]; hmin <- hr[which.min(hr$n), ]
    hgap <- 100 * (hmax$n / max(hmin$n, 1) - 1)
    fatal_all <- mean(d$severity == "Fatal")
    cg <- 100 * (max(city$n) / min(city$n) - 1)

    scan <- bind_rows(lapply(names(FACTOR_LABELS)[1:6], function(lbl) {
      col <- FACTOR_LABELS[[lbl]]
      d |> group_by(level = as.character(.data[[col]])) |>
        summarise(n = n(), fatal = mean(severity == "Fatal"), .groups = "drop") |>
        filter(n >= 50) |> mutate(factor = lbl)
    }))
    cas_f <- mean(d$casualties[d$severity == "Fatal"]); cas_o <- mean(d$casualties[d$severity != "Fatal"])
    tt <- tests(); n_sig <- if (nrow(tt)) sum(tt$`Severity link p` < 0.05) else 0

    items <- list(
      finding("car", "Overall volume",
        HTML(sprintf("<b>%s</b> accidents were recorded between %s and %s across <b>%d cities</b>, with <b>%s casualties</b> (%.2f per accident).",
                     cm(N), format(min(d$date), "%d %b %Y"), format(max(d$date), "%d %b %Y"),
                     n_distinct(d$city), cm(sum(d$casualties)), mean(d$casualties)))),
      finding("map-pin", "Location",
        HTML(sprintf("<b>%s</b> has the most records (%s, %.1f%%) and <b>%s</b> the fewest (%s). The gap between the two is only <b>%.0f%%</b>, so no single city dominates.",
                     city$cat[1], cm(city$n[1]), city$share[1], city$cat[nrow(city)], cm(city$n[nrow(city)]), cg))),
      finding("clock", "Time of day & week",
        HTML(sprintf("Records peak at <b>%02d:00</b> (%s) and dip at <b>%02d:00</b> (%s) - the peak is %.0f%% above the quietest hour. <b>%s</b> is the busiest weekday and <b>%s</b> the busiest period.",
                     hmax$hour, cm(hmax$n), hmin$hour, cm(hmin$n), hgap, as.character(wd$cat[1]), as.character(tod$cat[1])))),
      finding("cloud-rain", "Conditions & causes",
        HTML(sprintf("Most common weather: <b>%s</b> (%.1f%%). Most common road type: <b>%s</b> (%.1f%%). Most common reported cause: <b>%s</b> (%.1f%%).",
                     wx$cat[1], wx$share[1], rd$cat[1], rd$share[1], cs$cat[1], cs$share[1]))),
      finding("skull", "Severity",
        HTML(sprintf("<b>%.1f%%</b> of selected accidents are fatal. Fatal accidents average <b>%.1f casualties</b> versus %.1f for non-fatal ones.%s",
                     100 * fatal_all, cas_f, cas_o,
                     if (nrow(scan)) {
                       b <- scan[which.max(scan$fatal), ]
                       sprintf(" The highest fatal share is for <b>%s: %s</b> at %.1f%% (%+.1f points vs overall%s).", b$factor, b$level, 100 * b$fatal, 100 * (b$fatal - fatal_all),
                               if (abs(b$fatal - fatal_all) < 0.02) ", a very small gap" else "")
                     } else "")))
    )
    rw <- d |> group_by(weather) |>
      summarise(r = mean(risk_score), f = mean(severity == "Fatal"), n = n(), .groups = "drop") |> filter(n >= 50)
    if (nrow(rw) >= 2) {
      hi <- rw[which.max(rw$r), ]; lo <- rw[which.min(rw$r), ]
      fgap <- 100 * (max(rw$f) - min(rw$f))
      items[[length(items) + 1]] <- finding("gauge-high", "Dataset risk score",
        HTML(sprintf("The supplied <b>risk_score</b> averages %.2f in <b>%s</b> weather versus %.2f in <b>%s</b>, while the fatal share differs by only %.1f points between weather types.%s",
                     hi$r, hi$weather, lo$r, lo$weather, fgap,
                     if (fgap < 2 && hi$r - lo$r > 0.1) " The score seems to follow weather rules rather than actual outcomes, so treat it with caution." else "")))
    }
    if (nrow(tt)) {
      items[[length(items) + 1]] <- finding("flask", "Statistical check",
        HTML(sprintf("Of <b>%d</b> factors tested, <b>%d</b> show a statistically significant link to severity (chi-square, 5%% level). %s",
                     nrow(tt), n_sig,
                     if (n_sig == 0) "Severity looks similar across all the conditions in this dataset, so differences in the charts are small and should be read with care."
                     else "See the Severity & Stats tab for the effect sizes before drawing conclusions.")))
    }

    # dataset-level note (independent of filters)
    even <- sapply(c("city", "road_type", "weather", "cause"), function(cn) { t <- table(acc[[cn]]); max(t) / min(t) })
    if (all(even < 1.2)) {
      items[[length(items) + 1]] <- finding("magnifying-glass-chart", "Data-quality observation", kind = "finding",
        HTML(sprintf("Across the full file, city, road type, weather and cause categories are almost perfectly evenly spread (largest vs. smallest category within %.0f%%). Real accident records are normally much more uneven, so it is worth confirming how this community dataset was generated before drawing real-world conclusions.",
                     100 * (max(even) - 1))))
    }
    tagList(items)
  })

  output$recs_ui <- renderUI({
    d <- fd(); need_data()
    cs <- group_count(d, "cause") |> arrange(desc(n))
    top <- head(cs, 3)
    recs <- lapply(seq_len(nrow(top)), function(i) {
      a <- cause_actions[[as.character(top$cat[i])]]
      finding("circle-check", paste0("Address: ", top$cat[i], " (", round(top$share[i], 1), "% of records)"), a, kind = "rec")
    })
    hr <- d |> count(hour) |> complete(hour = 0:23, fill = list(n = 0))
    gap <- 100 * (max(hr$n) / max(min(hr$n), 1) - 1)
    recs[[length(recs) + 1]] <- finding("clock", "Timing of enforcement", kind = "rec",
      if (gap < 30) "Hourly counts are fairly even, so no single time window clearly stands out; spread awareness and patrols across the day rather than over-targeting one hour."
      else "Concentrate patrols and awareness around the busiest hours shown in the Time Patterns tab.")
    recs[[length(recs) + 1]] <- finding("database", "Strengthen the evidence", kind = "rec",
      "Cross-check findings with official records (e.g. MoRTH / NCRB) and add traffic-volume data so accident rates, not just counts, can be compared.")
    tagList(recs)
  })

  # ---- Data explorer ----------------------------------------------
  display_data <- reactive({
    fd() |> transmute(Date = date, City = city, State = state, Hour = hour, Weekday = weekday,
                      `Road type` = road_type, Lanes = lanes, Weather = weather,
                      Visibility = visibility, `Traffic density` = traffic_density,
                      Cause = cause, Severity = severity, Vehicles = vehicles,
                      Casualties = casualties, Festival = festival, `Risk score` = risk_score)
  })
  output$tbl <- renderDT({
    datatable(display_data(), fillContainer = FALSE, height = "auto", rownames = FALSE, filter = "top", class = "compact stripe hover",
              options = list(pageLength = 12, scrollX = TRUE, lengthMenu = c(10, 12, 25, 50, 100)))
  })
  output$dl <- downloadHandler(
    filename = function() paste0("road_accidents_filtered_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(display_data(), file))

  # ---- About -------------------------------------------------------
  output$tbl_log <- renderDT(datatable(clean_log, fillContainer = FALSE, height = "auto", rownames = FALSE, class = "compact stripe",
                                       options = list(dom = "t", pageLength = 30, ordering = FALSE)))
  output$tbl_dict <- renderDT({
    dict <- data.frame(
      Variable = names(readr::read_csv("data/indian_roads_dataset.csv", n_max = 1, show_col_types = FALSE)),
      Description = c("Unique record ID", "City where the accident was recorded", "State of the city",
                      "Latitude of the record", "Longitude of the record", "Date (dd-mm-yyyy)",
                      "Time (hh:mm)", "Hour of day (0-23)", "Day of week", "1 = Saturday/Sunday",
                      "Road type (urban / rural / highway)", "Number of lanes (1-6)",
                      "1 = traffic signal present", "Weather (clear / rain / fog)",
                      "Visibility (low / medium / high)", "Temperature (deg C)",
                      "Traffic density (low / medium / high)", "Reported cause",
                      "Severity (minor / major / fatal)", "Vehicles involved (1-5)",
                      "Casualties (0-5)", "Peak-hour flag as supplied in the file",
                      "Festival on the date, or 'None'", "Pre-computed risk score 0.1-1.0 (method not documented)"),
      stringsAsFactors = FALSE)
    datatable(dict, fillContainer = FALSE, height = "auto", rownames = FALSE, class = "compact stripe", options = list(dom = "t", pageLength = 30, ordering = FALSE))
  })
}

shinyApp(ui, server)
