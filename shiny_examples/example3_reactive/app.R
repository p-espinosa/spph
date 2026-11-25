# Example 3 of the talk: filter once, use everywhere: reactive()
# New: a second input (the season), reactive(), and a second output.
# The season's data are computed in ONE reactive; the plot and the sentence
# both read it, and it is recomputed only when the season changes.
#
# Try: pick 2020/21. What happened? Would you publish that chart?
#      Add radioButtons() to show weekly cases instead of cases per 100 000.

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

DATA_PATH <- "../../data/flu/flu_cantons_weekly.csv"
ACCENT    <- "#14505C"
SOURCE    <- "Source: FOPH, mandatory reporting, laboratory-confirmed influenza (opendata.swiss)"
SEASONS   <- paste0(2013:2025, "/", substr(2014:2026, 3, 4))   # "2013/14" ... "2025/26"

flu <- read_csv(DATA_PATH, show_col_types = FALSE)

# October to April of one flu season
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

# A sentence computed from the data: the "title states the finding" rule,
# for a chart whose content the reader changes.
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
  titlePanel("Influenza in Switzerland, by canton"),
  sidebarLayout(
    sidebarPanel(
      selectInput("canton", "Canton to highlight",
                  choices = sort(unique(flu$canton)), selected = "Ticino"),
      selectInput("season", "Flu season (October to April)",
                  choices = rev(SEASONS), selected = "2023/24")
    ),
    mainPanel(
      plotOutput("trend", height = "450px"),
      textOutput("headline")
    )
  )
)

server <- function(input, output, session) {
  weekly <- reactive({
    d <- season_weeks(flu, input$season)
    validate(need(nrow(d) > 0, "No data for this season."))   # message instead of an error
    d
  })

  output$trend    <- renderPlot(plot_story(weekly(), input$canton), res = 96)
  output$headline <- renderText(peak_sentence(weekly(), input$canton))
}

shinyApp(ui, server)
