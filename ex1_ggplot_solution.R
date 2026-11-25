# Exercise 1: SOLUTION
# From a correct chart to a communicating chart: four post-processing moves.
# Working directory = the materials folder (open DataViz_Workshop.Rproj).

library(ggplot2)
library(dplyr)
library(readr)

DATA_PATH <- "data/flu/flu_cantons_weekly.csv"     # FOPH, lab-confirmed influenza
ACCENT    <- "#14505C"                         # one colour, reserved for the story
FOCUS     <- "Ticino"                          # the canton the story is about
FROM      <- as.Date("2023-10-01")             # the 2023/24 flu season
TO        <- as.Date("2024-04-30")
SOURCE    <- "Source: Federal Office of Public Health (FOPH), mandatory reporting, laboratory-confirmed influenza (opendata.swiss)"

flu <- read_csv(DATA_PATH, show_col_types = FALSE)

weekly <- flu |>
  filter(week_start >= FROM, week_start <= TO)

# Switzerland as a whole = all cantons together
national <- weekly |>
  group_by(week_start) |>
  summarise(cases = sum(cases), population = sum(population)) |>
  mutate(incidence = cases / population * 1e5)


# 0. Starting point: correct, complete, and says nothing in particular -----

p0 <- ggplot(weekly, aes(week_start, incidence, colour = canton)) +
  geom_line()
p0


# 1. MESSAGE: the title states the finding --------------------------------
# Title = what the reader should conclude. Subtitle = what, where, when, unit.
# Caption = source (and caveats). Axis titles go if the subtitle says it.

story_labs <- labs(
  title    = "In the 2023/24 flu season, Ticino peaked four weeks before Switzerland",
  subtitle = "Laboratory-confirmed influenza, weekly cases per 100 000 inhabitants, October 2023 to April 2024",
  x = NULL, y = NULL, colour = NULL,
  caption  = SOURCE
)

p1 <- p0 + story_labs
p1


# 2. HIGHLIGHT and DECLUTTER: one colour for the story, grey for context ---

theme_story <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor      = element_blank(),
    panel.grid.major.x    = element_blank(),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title    = element_text(face = "bold", size = rel(1.1)),
    plot.subtitle = element_text(colour = "grey30", margin = margin(b = 10)),
    plot.caption  = element_text(colour = "grey45", hjust = 0, margin = margin(t = 10)),
    axis.text     = element_text(colour = "grey40")
  )

p2 <- ggplot(weekly, aes(week_start, incidence)) +
  geom_line(data = filter(weekly, canton != FOCUS), aes(group = canton),
            colour = "grey82", linewidth = 0.4) +
  geom_line(data = national, colour = "grey25", linewidth = 0.8, linetype = "22") +
  geom_line(data = filter(weekly, canton == FOCUS), colour = ACCENT, linewidth = 1.2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  story_labs +
  theme_story
p2


# 3. ANNOTATE: label lines directly, point at the evidence -----------------
# First find the peaks in the data (run the two lines, read the result):
weekly |> filter(canton == FOCUS) |> slice_max(incidence, n = 1)   # Ticino: week of 25 Dec 2023, 70 per 100 000
national |> slice_max(incidence, n = 1)                            # Switzerland: week of 22 Jan 2024, 26 per 100 000
# Then place each label by hand: x = a date, y = a height on the chart.

p3 <- p2 +
  # the name of each line, next to the line (so no legend is needed)
  annotate("text", x = as.Date("2023-12-21"), y = 70, label = "Ticino",
           colour = ACCENT, fontface = "bold", hjust = 1, size = 4) +
  annotate("text", x = as.Date("2024-02-15"), y = 44, label = "Switzerland (all cantons)",
           colour = "grey25", hjust = 0, size = 3.6) +
  annotate("segment", x = as.Date("2024-01-24"), xend = as.Date("2024-02-14"),
           y = 27, yend = 44, colour = "grey25", linewidth = 0.3) +
  annotate("text", x = as.Date("2024-01-27"), y = 100, label = "Other cantons",
           colour = "grey55", hjust = 0, size = 3.4) +
  # the evidence for the title: a double arrow from one peak to the other
  annotate("segment", x = as.Date("2023-12-25"), xend = as.Date("2024-01-22"),
           y = 78, yend = 78, colour = "grey25", linewidth = 0.4,
           arrow = arrow(length = unit(1.6, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = as.Date("2024-01-08"), y = 81, label = "4 weeks",
           vjust = 0, size = 3.6, colour = "grey25")
p3


# 4. EXPORT at the final size, as vector, for the finishing touches -------
# PDF / SVG keep text editable in Illustrator, Inkscape or Figma.
# Size in cm = the size it will have on the page: fonts stay readable.

dir.create("output", showWarnings = FALSE)
ggsave("output/ex1_final.pdf", p3, width = 20, height = 12, units = "cm")
ggsave("output/ex1_final.svg", p3, width = 20, height = 12, units = "cm")   # needs svglite
ggsave("output/ex1_final.png", p3, width = 20, height = 12, units = "cm", dpi = 300, bg = "white")


# EXTRA: the same arrow, computed from the data ---------------------------
# Fixed positions stay where they are when the data change. Computed positions
# follow the data: useful for a weekly report or a Shiny app.

focus_peak <- weekly |> filter(canton == FOCUS) |> slice_max(incidence, n = 1)
nat_peak   <- national |> slice_max(incidence, n = 1)
gap_weeks  <- as.numeric(nat_peak$week_start - focus_peak$week_start) / 7
middle     <- focus_peak$week_start + (nat_peak$week_start - focus_peak$week_start) / 2

p3_auto <- p2 +
  annotate("segment", x = focus_peak$week_start, xend = nat_peak$week_start,
           y = focus_peak$incidence + 8, yend = focus_peak$incidence + 8,
           colour = "grey25", linewidth = 0.4,
           arrow = arrow(length = unit(1.6, "mm"), ends = "both", type = "closed")) +
  annotate("text", x = middle, y = focus_peak$incidence + 11, vjust = 0,
           label = paste(gap_weeks, "weeks"), size = 3.6, colour = "grey25")
p3_auto


# STRETCH: a second panel for the infographic sheet -----------------------
# Same moves, different question: which age groups carry the wave?
# FOPH publishes age groups for Switzerland as a whole only, not per canton.

by_age <- read_csv("data/flu/flu_age_weekly.csv", show_col_types = FALSE) |>
  filter(week_start >= FROM, week_start <= TO) |>
  mutate(age_group = factor(age_group, levels = c("0-4", "5-14", "15-29", "30-64", "65+")),
         is_kids   = age_group == "0-4")

p4 <- ggplot(by_age, aes(week_start, incidence)) +
  geom_line(aes(colour = is_kids), linewidth = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = ACCENT, `FALSE` = "grey55")) +
  facet_wrap(~ age_group, nrow = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(title    = "Children under five had the highest flu rate in 2023/24",
       subtitle = "Switzerland, weekly cases per 100 000 inhabitants. In numbers of cases, the 65+ come first.",
       x = NULL, y = NULL,
       caption  = SOURCE) +
  theme_story +
  theme(strip.text = element_text(face = "bold", hjust = 0))
p4

ggsave("output/ex1_stretch_age.png", p4, width = 20, height = 8, units = "cm", dpi = 300, bg = "white")
