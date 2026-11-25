# Tick-borne encephalitis in Switzerland: the data of the exercises

Tick-borne encephalitis (TBE; in German FSME, in French méningo-encéphalite à tiques) is a viral infection passed on by the bite of a tick. Doctors and laboratories must report every case to the Federal Office of Public Health (FOPH). These tables are the reported cases, as FOPH publishes them as open data.

## The four tables

The files are in this folder, `data/tbe/`. They are small: look at them before you draw.

| File                          | One row is                                        | Rows | Period                       |
| ----------------------------- | ------------------------------------------------- | ---- | ---------------------------- |
| `tbe_cantons_yearly.csv`      | one canton in one year                            | 338  | 2013 to 2025                 |
| `tbe_switzerland_monthly.csv` | one month, Switzerland and Liechtenstein together | 166  | January 2013 to October 2026 |
| `tbe_age_10y.csv`             | one age group, ten years together                 | 5    | 2016 to 2025                 |
| `tbe_sex_10y.csv`             | one sex, ten years together                       | 3    | 2016 to 2025                 |

### `tbe_cantons_yearly.csv`

| Column | Meaning |
|---|---|
| `year` | 2013 to 2025 |
| `canton`, `canton_code` | the 26 cantons, with the same spellings as in the flu table |
| `cases` | reported cases in that year |
| `population` | inhabitants of the canton |
| `incidence` | cases per 100 000 inhabitants in that year |

### `tbe_switzerland_monthly.csv`

| Column | Meaning |
|---|---|
| `month_start` | the first day of the month, as a date |
| `year`, `month` | the same, as two numbers |
| `cases` | reported cases in that month |
| `population` | inhabitants of Switzerland and Liechtenstein |
| `min_10y`, `median_10y`, `max_10y` | the lowest, the middle and the highest number of cases of the same month in the ten years before. Given from January 2022 on; empty before |

### `tbe_age_10y.csv` and `tbe_sex_10y.csv`

| Column | Meaning |
|---|---|
| `period` | 2016-2025: the ten years are counted together |
| `age_group` | 0-16, 17-44, 45-64, 65-84, 85+ |
| `incidence_10y` | cases per 100 000 people of that age, per year, on average over the ten years |
| `sex` | female, male, unknown |
| `cases_10y` | cases of the ten years together |
| `share` | per cent of the cases of the ten years |

## To read them in R

```r
library(readr)
cantons <- read_csv("data/tbe/tbe_cantons_yearly.csv")
months  <- read_csv("data/tbe/tbe_switzerland_monthly.csv")
ages    <- read_csv("data/tbe/tbe_age_10y.csv")
sexes   <- read_csv("data/tbe/tbe_sex_10y.csv")
```

These paths start at the folder of the package. A Shiny app reads its files from its own folder: from an app in a folder next to `data/`, the path is `../data/tbe/tbe_cantons_yearly.csv`.

## Context

From the FOPH page on TBE (read on 19 November 2026): the whole of Switzerland except the canton of Ticino counts as a risk area. Vaccination is recommended for all people who are exposed to ticks in a risk area, in general from the age of 3. In 5 to 15 % of infected people the central nervous system is affected, and about 1 % of the cases with neurological symptoms die. There is no treatment against the virus itself.

https://www.bag.admin.ch/de/fruehsommer-meningoenzephalitis-fsme

## The source, for your chart

> Source: Federal Office of Public Health (FOPH), tick-borne encephalitis, mandatory reporting system (opendata.swiss). Data of 19 November 2026.

