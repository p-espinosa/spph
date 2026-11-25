# Exercise 1: from a correct chart to a communicating chart (45 min)
# Four post-processing moves: MESSAGE, HIGHLIGHT + DECLUTTER, ANNOTATE, EXPORT.
#
# Working directory = the materials folder (open DataViz_Workshop.Rproj).
# Run line by line (Cmd/Ctrl + Enter). Each step builds on the previous plot.
# Got your own chart from yesterday's exercise? Use it instead: the moves are the same.

library(ggplot2)
library(dplyr)
library(readr)

DATA_PATH <- "data/flu/flu_cantons_weekly.csv"     # FOPH, lab-confirmed influenza, by canton
ACCENT    <- "#14505C"                         # one colour, reserved for the story
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

# Look at p0 for two minutes. What would you tell a cantonal health officer?
# Tip: p0 + facet_wrap(~ canton) makes each canton easier to see.
#      Which canton's wave comes earlier than the others?


# 1. MESSAGE: the title states the finding (10 min) -----------------------
# Title    = what the reader should conclude (one sentence, with a verb)
# Subtitle = what, where, when, unit
# Caption  = source and caveats
# Axis titles can go if the subtitle already says it (x = NULL, y = NULL).

story_labs <- labs(
  title    = "TODO: what should the reader conclude?",
  subtitle = "TODO: indicator, unit, place, period",
  x = NULL, y = NULL, colour = NULL,
  caption  = SOURCE
)

p1 <- p0 + story_labs
p1


# 2. HIGHLIGHT and DECLUTTER (10 min) -------------------------------------
# One colour for the story, grey for the context. Remove what does not help.

FOCUS <- "Ticino"   # TODO: the canton your title is about

# The theme. First run p2 as it is. Then select the six lines starting with
# "# theme_story", remove the # (Cmd/Ctrl + Shift + C), run them, and run p2 again.
# Change one value at a time to see what each line does.
theme_story <- theme_minimal(base_size = 12)
# theme_story <- theme_minimal(base_size = 12) +
#   theme(panel.grid.minor    = element_blank(),                          # no small grid lines
#         panel.grid.major.x  = element_blank(),                          # no vertical grid lines
#         plot.title.position = "plot",                                   # title starts at the left edge
#         plot.caption        = element_text(colour = "grey45", hjust = 0)  # caption grey, on the left
#   )

p2 <- ggplot(weekly, aes(week_start, incidence)) +
  geom_line(data = filter(weekly, canton != FOCUS), aes(group = canton),
            colour = "grey82", linewidth = 0.4) +
  geom_line(data = national, colour = "grey25", linewidth = 0.8, linetype = "22") +
  # TODO: a third geom_line() for the FOCUS canton, in ACCENT, thicker (linewidth = 1.2)
  story_labs +
  theme_story
p2


# 3. ANNOTATE (10 min) -----------------------------------------------------
# Label the lines directly instead of a legend, then point at the evidence.
#   annotate("text", x = as.Date("2023-12-20"), y = 70, label = "Ticino", colour = ACCENT)
#   annotate("segment", x = ..., xend = ..., y = ..., yend = ..., arrow = arrow())
# Tip: find the peaks from the data instead of reading them off the chart:
#   weekly |> filter(canton == FOCUS) |> slice_max(incidence, n = 1)
#   national |> slice_max(incidence, n = 1)

p3 <- p2   # TODO: + annotate(...) + annotate(...)
p3


# 4. EXPORT at the final size, as vector (10 min) -------------------------
# Width/height in cm = the size on the final page, so the fonts stay readable.

dir.create("output", showWarnings = FALSE)
# TODO: ggsave("output/my_chart.pdf", p3, width = 20, height = 12, units = "cm")
# TODO: ggsave("output/my_chart.svg", p3, width = 20, height = 12, units = "cm")
#
# Then open the PDF or SVG in Illustrator, Inkscape (free) or Figma (free) and
# make ONE change by hand that you could not easily make in R. Note what it was:
# that is the step you will have to redo every time the data change.


# STRETCH (if you finish early) --------------------------------------------
# A second panel for the same infographic: which age groups carry the wave?
# data/flu/flu_age_weekly.csv has the same columns with age_group instead of canton
# (Switzerland as a whole: FOPH does not publish age groups per canton).
# Use facet_wrap(~ age_group), highlight one group, same title logic.
