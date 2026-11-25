# prepare_flu_data.R — builds the two workshop tables from FOPH open data.
#
# Source: Federal Office of Public Health (FOPH/BAG), influenza, mandatory
# reporting system (laboratory-confirmed cases), opendata.swiss "influenza_oblig".
# Terms: open use, must provide the source.
#   https://idd.bag.admin.ch/api/v1/export/latest/INFLUENZA_oblig/csv
#
# The raw file is frozen as bag_influenza_oblig_raw_2026-11-09.csv (data up to
# week 2026-W44), so the workshop works offline and the numbers don't move.
# To refresh it: set REFRESH <- TRUE (needs internet).
#
# Output (in data/flu/):
#   flu_cantons_weekly.csv  week x canton (26 cantons), all ages and sexes
#   flu_age_weekly.csv      week x age group, Switzerland + Liechtenstein
# FOPH publishes age groups only nationally, not per canton.
#
# Run from the materials folder:  Rscript data/flu/prepare_flu_data.R

library(dplyr)
library(readr)

RAW     <- "data/flu/bag_influenza_oblig_raw_2026-11-09.csv"
REFRESH <- FALSE
if (REFRESH) {
  RAW <- paste0("data/flu/bag_influenza_oblig_raw_", Sys.Date(), ".csv")
  download.file("https://idd.bag.admin.ch/api/v1/export/latest/INFLUENZA_oblig/csv", RAW, mode = "wb")
}

raw <- read_csv(RAW, show_col_types = FALSE, guess_max = 1e5) |>
  filter(valueCategory == "cases", temporal_type == "iso_week")

# ISO week "2025-W05" -> date of its Monday
iso_monday <- function(x) {
  year <- as.integer(substr(x, 1, 4))
  week <- as.integer(substr(x, 7, 8))
  jan4 <- as.Date(paste0(year, "-01-04"))
  jan4 - (as.integer(format(jan4, "%u")) - 1) + 7 * (week - 1)
}

# Flu season = week 27 to week 26 of the next year (July to June)
season_of <- function(x) {
  year <- as.integer(substr(x, 1, 4))
  week <- as.integer(substr(x, 7, 8))
  start <- ifelse(week >= 27, year, year - 1)
  paste0(start, "/", substr(start + 1, 3, 4))
}

canton_names <- c(
  AG = "Aargau", AI = "Appenzell I.Rh.", AR = "Appenzell A.Rh.", BE = "Bern",
  BL = "Basel-Landschaft", BS = "Basel-Stadt", FR = "Fribourg", GE = "Geneva",
  GL = "Glarus", GR = "Graubuenden", JU = "Jura", LU = "Lucerne",
  NE = "Neuchatel", NW = "Nidwalden", OW = "Obwalden", SG = "St. Gallen",
  SH = "Schaffhausen", SO = "Solothurn", SZ = "Schwyz", TG = "Thurgau",
  TI = "Ticino", UR = "Uri", VD = "Vaud", VS = "Valais", ZG = "Zug", ZH = "Zurich"
)  # ASCII spellings on purpose: they survive every export device

cantons <- raw |>
  filter(georegion_type == "canton", georegion %in% names(canton_names)) |>
  transmute(week_start = iso_monday(temporal),
            iso_week   = temporal,
            season     = season_of(temporal),
            canton     = unname(canton_names[georegion]),
            canton_code = georegion,
            cases      = value,
            population = pop,
            incidence  = round(cases / population * 1e5, 2)) |>
  arrange(week_start, canton)

ages <- raw |>
  filter(georegion == "CHFL", agegroup != "all", agegroup != "unknown",
         sex == "all", type == "all") |>
  transmute(week_start = iso_monday(temporal),
            iso_week   = temporal,
            season     = season_of(temporal),
            age_group  = gsub(" ", "", agegroup),       # "0 - 4" -> "0-4"
            cases      = value,
            population = pop,
            incidence  = round(cases / population * 1e5, 2)) |>
  arrange(week_start, age_group)

write_csv(cantons, "data/flu/flu_cantons_weekly.csv")
write_csv(ages,    "data/flu/flu_age_weekly.csv")
cat("flu_cantons_weekly.csv:", nrow(cantons), "rows,", min(cantons$iso_week), "to", max(cantons$iso_week), "\n")
cat("flu_age_weekly.csv:    ", nrow(ages), "rows\n")
