# Example 2 of the talk: the reader chooses what to highlight
# New: sidebarLayout(), selectInput(), and input$canton inside renderPlot().
#
# Try: add a radioButtons() to switch the accent colour,
#      or a checkboxInput() that hides the grey lines of the other cantons.

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

DATA_PATH <- "../../data/flu/flu_cantons_weekly.csv"
ACCENT    <- "#14505C"
SOURCE    <- "Source: FOPH, mandatory reporting, laboratory-confirmed influenza (opendata.swiss)"

flu <- read_csv(DATA_PATH, show_col_types = FALSE)
weekly <- flu |> filter(week_start >= as.Date("2023-10-01"), week_start <= as.Date("2024-04-30"))

plot_story <- function(weekly, focus) {
  national <- weekly |>
    group_by(week_start) |>
    summarise(incidence = sum(cases) / sum(population) * 1e5)
  ggplot(weekly, aes(week_start, incidence)) +
    geom_line(data = filter(weekly, canton != focus), aes(group = canton),
              colour = "grey82", linewidth = 0.4) +
    geom_line(data = national, colour = "grey25", linewidth = 0.8, linetype = "22") +
    geom_line(data = filter(weekly, canton == focus), colour = ACCENT, linewidth = 1.2) +
    labs(title = paste(focus, "against Switzerland and the other cantons"),
         subtitle = "Weekly cases per 100 000 inhabitants. Dashed: Switzerland. Grey: other cantons.",
         x = NULL, y = NULL, caption = SOURCE) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          plot.title.position = "plot", plot.title = element_text(face = "bold"),
          plot.caption.position = "plot", plot.caption = element_text(colour = "grey45", hjust = 0))
}

ui <- fluidPage(
  titlePanel("Influenza in Switzerland, 2023/24"),
  sidebarLayout(
    sidebarPanel(
      selectInput("canton", "Canton to highlight",
                  choices = sort(unique(flu$canton)), selected = "Ticino")
    ),
    mainPanel(
      plotOutput("trend", height = "450px")
    )
  )
)

server <- function(input, output, session) {
  output$trend <- renderPlot(res = 96, {
    plot_story(weekly, input$canton)
  })
}

shinyApp(ui, server)
