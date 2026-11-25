# prepare_tbe_data.R — builds the tables of the exercise dataset from FOPH open data.
#
# Source: Federal Office of Public Health (FOPH/BAG), tick-borne encephalitis
# (TBE; in German FSME), mandatory reporting system, opendata.swiss
# "FSME - mandatory reporting system". Terms: open use, must provide the source.
#   https://idd.bag.admin.ch/api/v1/export/latest/TBE_oblig/csv
#
# The raw file is frozen as bag_tbe_oblig_raw_2026-11-19.csv (data up to
# October 2026), so the workshop works offline and the numbers don't move.
# To refresh it: set REFRESH <- TRUE (needs internet).
#
# Output (in data/tbe/):
#   tbe_cantons_yearly.csv       year x canton (26 cantons), 2013 to 2025
#   tbe_switzerland_monthly.csv  month, Switzerland and Liechtenstein, January 2013 to October 2026
#   tbe_age_10y.csv              age group: mean yearly cases per 100 000 over the last ten years
#   tbe_sex_10y.csv              sex: cases summed over the last ten years
# FOPH publishes the cantons by year only, the months for the whole country only,
# and age and sex as ten-year figures only.
#
# Run from the materials folder:  Rscript data/tbe/prepare_tbe_data.R

library(dplyr)
library(readr)

RAW     <- "data/tbe/bag_tbe_oblig_raw_2026-11-19.csv"
REFRESH <- FALSE
if (REFRESH) {
  RAW <- paste0("data/tbe/bag_tbe_oblig_raw_", Sys.Date(), ".csv")
  download.file("https://idd.bag.admin.ch/api/v1/export/latest/TBE_oblig/csv", RAW, mode = "wb")
}

raw <- read_csv(RAW, show_col_types = FALSE, guess_max = 1e5,
                col_types = cols(temporal = col_character()))

canton_names <- c(
  AG = "Aargau", AI = "Appenzell I.Rh.", AR = "Appenzell A.Rh.", BE = "Bern",
  BL = "Basel-Landschaft", BS = "Basel-Stadt", FR = "Fribourg", GE = "Geneva",
  GL = "Glarus", GR = "Graubuenden", JU = "Jura", LU = "Lucerne",
  NE = "Neuchatel", NW = "Nidwalden", OW = "Obwalden", SG = "St. Gallen",
  SH = "Schaffhausen", SO = "Solothurn", SZ = "Schwyz", TG = "Thurgau",
  TI = "Ticino", UR = "Uri", VD = "Vaud", VS = "Valais", ZG = "Zug", ZH = "Zurich"
)  # the same ASCII spellings as in the flu tables

# 1. Cantons, by year --------------------------------------------------------
cantons <- raw |>
  filter(valueCategory == "cases", temporal_type == "year",
         georegion_type == "canton", georegion %in% names(canton_names)) |>
  transmute(year        = as.integer(temporal),
            canton      = unname(canton_names[georegion]),
            canton_code = georegion,
            cases       = value,
            population  = pop,
            incidence   = round(cases / population * 1e5, 2)) |>
  arrange(year, canton)

# 2. Switzerland and Liechtenstein, by month ----------------------------------
# With the range of the ten years before (minimum, median, maximum of the same
# month), which FOPH gives from January 2022 on.
month_rows <- raw |> filter(temporal_type == "month", georegion == "CHFL")
one_value <- function(category, name) {
  d <- month_rows |> filter(valueCategory == category) |> select(temporal, value)
  names(d)[2] <- name
  d
}
months <- month_rows |>
  filter(valueCategory == "cases") |>
  select(temporal, cases = value, population = pop) |>
  left_join(one_value("min_value_10y",    "min_10y"),    by = "temporal") |>
  left_join(one_value("median_value_10y", "median_10y"), by = "temporal") |>
  left_join(one_value("max_value_10y",    "max_10y"),    by = "temporal") |>
  mutate(year  = as.integer(substr(temporal, 1, 4)),
         month = as.integer(substr(temporal, 7, 8)),
         month_start = as.Date(sprintf("%d-%02d-01", year, month))) |>
  select(month_start, year, month, cases, population, min_10y, median_10y, max_10y) |>
  arrange(month_start)

# 3. Age and sex, over the last ten complete years -----------------------------
last_year <- max(cantons$year)
period    <- paste0(last_year - 9, "-", last_year)

ages <- raw |>
  filter(valueCategory == "cases_10y_mean", temporal == as.character(last_year),
         agegroup != "all", agegroup != "unknown") |>
  transmute(period    = period,
            age_group = gsub(" ", "", agegroup),            # "0 - 16" -> "0-16"
            incidence_10y = incValue)                        # mean yearly cases per 100 000

sexes <- raw |>
  filter(valueCategory == "cases_10y_cumsum", temporal == as.character(last_year),
         sex != "all") |>
  transmute(period    = period,
            sex       = sex,
            cases_10y = value,
            share     = prct)                                # % of the cases of the ten years

write_csv(cantons, "data/tbe/tbe_cantons_yearly.csv")
write_csv(months,  "data/tbe/tbe_switzerland_monthly.csv")
write_csv(ages,    "data/tbe/tbe_age_10y.csv")
write_csv(sexes,   "data/tbe/tbe_sex_10y.csv")
cat("tbe_cantons_yearly.csv:     ", nrow(cantons), "rows,", min(cantons$year), "to", max(cantons$year), "\n")
cat("tbe_switzerland_monthly.csv:", nrow(months), "rows,", format(min(months$month_start), "%Y-%m"), "to", format(max(months$month_start), "%Y-%m"), "\n")
cat("tbe_age_10y.csv:            ", nrow(ages), "rows, period", period, "\n")
cat("tbe_sex_10y.csv:            ", nrow(sexes), "rows, period", period, "\n")
