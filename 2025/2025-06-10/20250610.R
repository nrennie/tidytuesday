# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-06-10")
judges_appointments <- tuesdata$judges_appointments
judges_people <- tuesdata$judges_people


# Data wrangling ----------------------------------------------------------

nominations <- judges_appointments |>
  select(president_name, nomination_date) |>
  mutate(
    nomination_date = mdy(nomination_date)
  ) |>
  filter(nomination_date >= as.Date("1947-01-01")) |>
  mutate(
    president_name = str_extract(president_name, "[^ ]+$")
  )

new_presidential <- presidential |>
  add_row(
    name = "Truman",
    start = as.Date("1947-01-01"),
    end = as.Date("1953-01-20"),
    party = "Democratic", .before = 1
  ) |>
  filter(start <= max(nominations$nomination_date)) |>
  mutate(
    end = if_else(
      end >= max(nominations$nomination_date), max(nominations$nomination_date), end
    )
  )

plot_data <- new_presidential |>
  rowwise() |>
  mutate(n = sum(nominations$nomination_date >= start &
    nominations$nomination_date <= end)) |>
  ungroup()


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2025/2025-06-10/data.csv")
