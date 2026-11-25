# Example 4 of the talk, the complete app: presentable and honest
# New: a bslib theme, a download button (the chart as PDF, ready for
# post-processing), and a warning when the numbers get small.
#
# Try: change the theme (bootswatch = "minty", "lux", ...), or download the PDF
#      and finish it in Illustrator or Inkscape, in the part on design and infographics.
#      Pick Appenzell I.Rh. or Uri, then Zurich: what does the warning say?

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)

DATA_PATH <- "../../data/flu/flu_cantons_weekly.csv"
ACCENT    <- "#14505C"
SOURCE    <- "Source: FOPH, mandatory reporting, laboratory-confirmed influenza (opendata.swiss)"
SEASONS   <- paste0(2013:2025, "/", substr(2014:2026, 3, 4))
SMALL_N   <- 10   # never more than this many cases in a week -> warn the reader

flu <- read_csv(DATA_PATH, show_col_types = FALSE)

season_weeks <- function(flu, season) {
  start <- as.integer(substr(season, 1, 4))
  flu |> filter(week_start >= as.Date(paste0(start, "-10-01")),
                week_start <= as.Date(paste0(start + 1, "-04-30")))
}

national_of <- function(weekly) {
  weekly |>
    group_by(week_start) |>
    summarise(incidence = sum(cases) / sum(population) * 1e5)
}

plot_story <- function(weekly, focus) {
  ggplot(weekly, aes(week_start, incidence)) +
    geom_line(data = filter(weekly, canton != focus), aes(group = canton),
              colour = "grey82", linewidth = 0.4) +
    geom_line(data = national_of(weekly), colour = "grey25", linewidth = 0.8, linetype = "22") +
    geom_line(data = filter(weekly, canton == focus), colour = ACCENT, linewidth = 1.2) +
    labs(title = paste(focus, "against Switzerland and the other cantons"),
         subtitle = "Weekly cases per 100 000 inhabitants. Dashed: Switzerland. Grey: other cantons.",
         x = NULL, y = NULL, caption = SOURCE) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          plot.title.position = "plot", plot.title = element_text(face = "bold"),
          plot.caption.position = "plot", plot.caption = element_text(colour = "grey45", hjust = 0))
}

peak_sentence <- function(weekly, focus) {
  own <- weekly |> filter(canton == focus) |> slice_max(incidence, n = 1, with_ties = FALSE)
  if (own$incidence == 0) return(paste(focus, "had no reported cases this season."))
  nat <- national_of(weekly) |> slice_max(incidence, n = 1, with_ties = FALSE)
  gap <- as.numeric(nat$week_start - own$week_start) / 7
  weeks <- function(n) if (n == 1) "1 week" else paste(n, "weeks")
  when <- if (gap > 0) paste(weeks(gap), "before") else if (gap < 0) paste(weeks(-gap), "after") else "in the same week as"
  rate <- if (own$incidence >= 10) sprintf("%.0f", own$incidence) else sprintf("%.1f", own$incidence)
  sprintf("%s peaked in the week of %s at %s cases per 100 000, %s Switzerland as a whole.",
          focus, format(own$week_start, "%d %b %Y"), rate, when)
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = ACCENT),
  titlePanel("Influenza in Switzerland, by canton"),
  sidebarLayout(
    sidebarPanel(
      selectInput("canton", "Canton to highlight",
                  choices = sort(unique(flu$canton)), selected = "Ticino"),
      selectInput("season", "Flu season (October to April)",
                  choices = rev(SEASONS), selected = "2023/24"),
      downloadButton("download", "Download chart (PDF)")
    ),
    mainPanel(
      plotOutput("trend", height = "450px"),
      tags$div(textOutput("headline"), style = "font-size: 1.1em; margin-bottom: 0.6em;"),
      tags$div(textOutput("caution"), style = "color: #9A3412; font-weight: 600;")
    )
  )
)

server <- function(input, output, session) {
  weekly <- reactive({
    d <- season_weeks(flu, input$season)
    validate(need(nrow(d) > 0, "No data for this season."))
    d
  })

  output$trend    <- renderPlot(plot_story(weekly(), input$canton), res = 96)   # res: sharper, larger text
  output$headline <- renderText(peak_sentence(weekly(), input$canton))

  output$caution <- renderText({
    top <- weekly() |> filter(canton == input$canton) |> pull(cases) |> max()
    if (top < SMALL_N) {
      sprintf("Small numbers: %s never had more than %d %s in a week this season. Week-to-week changes are mostly noise.",
              input$canton, as.integer(top), if (top == 1) "case" else "cases")
    } else ""
  })

  output$download <- downloadHandler(
    filename = function() paste0("flu_", gsub("[^A-Za-z]", "_", input$canton), "_",
                                 sub("/", "-", input$season), ".pdf"),
    content  = function(file) {
      ggsave(file, plot_story(weekly(), input$canton), device = "pdf",
             width = 20, height = 12, units = "cm")
    }
  )
}

shinyApp(ui, server)
