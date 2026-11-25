# Example 5 of the talk: from one chart to a dashboard
# New: several results on one page that all follow the same filters (three key
# numbers and two charts), three kinds of control (a menu, a slider, buttons),
# and the value under the mouse (plotly).
#
# Try: move the slider: every number and both charts change.
#      Pass the mouse over a line or a bar.
#      Add a third chart: one more card() on the page, one more render in the server.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

DATA_PATH <- "../../data/flu/flu_cantons_weekly.csv"
ACCENT    <- "#14505C"
SOURCE    <- "Source: FOPH, mandatory reporting, laboratory-confirmed influenza (opendata.swiss)"
SMALL_N   <- 10   # never more than this many cases in a week -> warn the reader

flu <- read_csv(DATA_PATH, show_col_types = FALSE)

# October to April of the flu season that starts in the autumn of `start`
season_weeks <- function(flu, start) {
  flu |> filter(week_start >= as.Date(paste0(start, "-10-01")),
                week_start <= as.Date(paste0(start + 1, "-04-30")))
}

season_name <- function(start) paste0(start, "/", substr(start + 1, 3, 4))   # 2023 -> "2023/24"
unit_of     <- function(measure) if (measure == "cases") "cases" else "per 100 000"

theme_dashboard <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
}

# Chart 1: the weeks of the season. One canton in colour, the others in grey.
# `tip` is the text that plotly shows under the mouse.
plot_trend <- function(weekly, focus, measure) {
  d <- weekly |>
    mutate(value = .data[[measure]],
           tip = sprintf("%s<br>Week of %s<br>%s %s", canton,
                         format(week_start, "%d %b %Y"), round(value, 1), unit_of(measure)))
  national <- if (measure == "incidence") {
    nat <- d |>
      group_by(week_start) |>
      summarise(value = sum(cases) / sum(population) * 1e5) |>
      mutate(canton = "Switzerland",
             tip = sprintf("Switzerland<br>Week of %s<br>%s per 100 000",
                           format(week_start, "%d %b %Y"), round(value, 1)))
    geom_line(data = nat, colour = "grey25", linewidth = 0.8, linetype = "dashed")
  }
  ggplot(d, aes(week_start, value, group = canton, text = tip)) +
    geom_line(data = filter(d, canton != focus), colour = "grey82", linewidth = 0.4) +
    national +
    geom_line(data = filter(d, canton == focus), colour = ACCENT, linewidth = 1.2) +
    labs(x = NULL, y = NULL) +
    theme_dashboard()
}

# Chart 2: the whole season, canton by canton, sorted
plot_ranking <- function(weekly, focus, measure) {
  d <- weekly |>
    group_by(canton) |>
    summarise(cases = sum(cases), incidence = sum(cases) / mean(population) * 1e5) |>
    mutate(value = .data[[measure]],
           tip = sprintf("%s<br>%s %s", canton, round(value, 1), unit_of(measure)))
  ggplot(d, aes(value, reorder(canton, value), text = tip)) +
    geom_col(aes(fill = canton == focus), width = 0.75) +
    scale_fill_manual(values = c(`TRUE` = ACCENT, `FALSE` = "grey80"), guide = "none") +
    labs(x = NULL, y = NULL) +
    theme_dashboard() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(),
          axis.text.y = element_text(size = 8))
}

# plotly: the ggplot2 chart, with the value under the mouse
interactive <- function(chart) {
  ggplotly(chart, tooltip = "text") |>
    config(displayModeBar = FALSE) |>
    layout(showlegend = FALSE)
}

ui <- page_sidebar(
  title = "Influenza in Switzerland, by canton",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = ACCENT),
  sidebar = sidebar(
    selectInput("canton", "Canton",
                choices = sort(unique(flu$canton)), selected = "Ticino"),
    sliderInput("start", "Season starting in autumn",
                min = 2013, max = 2025, value = 2023, step = 1, sep = ""),
    radioButtons("measure", "Show",
                 c("Cases per 100 000" = "incidence", "Number of cases" = "cases")),
    downloadButton("download", "Download chart (PDF)")
  ),
  layout_columns(
    value_box("Cases in the season", textOutput("total"), theme = "primary"),
    value_box("Per 100 000 inhabitants", textOutput("rate")),
    value_box("Highest week", textOutput("peak"))
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(card_header(textOutput("trend_title")), plotlyOutput("trend"), min_height = 440),
    card(card_header("The 26 cantons, whole season"), plotlyOutput("ranking"), min_height = 440)
  ),
  tags$div(textOutput("caution"), style = "color: #9A3412; font-weight: 600;")
)

server <- function(input, output, session) {
  # The filters are applied once. Every result below reads `weekly()` or `own()`.
  weekly <- reactive(season_weeks(flu, input$start))
  own    <- reactive(filter(weekly(), canton == input$canton))

  output$total <- renderText(format(sum(own()$cases), big.mark = " "))
  output$rate  <- renderText(round(sum(own()$cases) / mean(own()$population) * 1e5))
  output$peak  <- renderText({
    top <- slice_max(own(), cases, n = 1, with_ties = FALSE)
    if (top$cases == 0) "no cases" else format(top$week_start, "%d %b")
  })

  output$trend_title <- renderText(
    paste0(input$canton, " and the other cantons, week by week, ", season_name(input$start)))
  output$trend   <- renderPlotly(interactive(plot_trend(weekly(), input$canton, input$measure)))
  output$ranking <- renderPlotly(interactive(plot_ranking(weekly(), input$canton, input$measure)))

  output$caution <- renderText({
    top <- max(own()$cases)
    if (top == 0) {
      paste(input$canton, "had no reported cases this season.")
    } else if (top < SMALL_N) {
      sprintf("Small numbers: %s never had more than %d %s in a week this season. Week-to-week changes are mostly noise.",
              input$canton, as.integer(top), if (top == 1) "case" else "cases")
    } else ""
  })

  # The file for the part on design: the same chart, with its title and its source, as PDF
  output$download <- downloadHandler(
    filename = function() paste0("flu_", gsub("[^A-Za-z]", "_", input$canton), "_",
                                 sub("/", "-", season_name(input$start)), ".pdf"),
    content  = function(file) {
      chart <- plot_trend(weekly(), input$canton, input$measure) +
        labs(title = paste(input$canton, "against the other cantons,", season_name(input$start)),
             subtitle = paste("Weekly", if (input$measure == "cases") "cases." else "cases per 100 000 inhabitants. Dashed: Switzerland.",
                              "Grey: other cantons."),
             caption = SOURCE) +
        theme(plot.title.position = "plot", plot.title = element_text(face = "bold"),
              plot.caption.position = "plot", plot.caption = element_text(colour = "grey45", hjust = 0))
      ggsave(file, chart, device = "pdf", width = 20, height = 12, units = "cm")
    }
  )
}

shinyApp(ui, server)
