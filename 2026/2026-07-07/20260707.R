# Load packages -----------------------------------------------------------

library(tidyverse)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-07-07")
ufc_athletes <- tuesdata$ufc_athletes
ufc_fights <- tuesdata$ufc_fights
ufc_rankings_dataset <- tuesdata$ufc_rankings_dataset
ufcstats_data <- tuesdata$ufcstats_data
ultimate_ufc_dataset <- tuesdata$ultimate_ufc_dataset


# Data wrangling ----------------------------------------------------------

ranks <- ufc_rankings_dataset |> 
  select(-date) |> 
  group_by(fighter, weightclass) |> 
  slice_min(rank, with_ties = FALSE) |> 
  mutate(sex = if_else(
    str_detect(weightclass, "Women's"), "Women", "Men"
  )) |>
  filter(sex == "Women") |> 
  mutate(weightclass = str_remove(weightclass, "Women's ")) |> 
  ungroup() |> 
  select(-sex)

plot_data <- ufc_athletes |>
  mutate(sex = if_else(
    str_detect(weight_class, "Women's"), "Women", "Men"
  )) |>
  filter(sex == "Women") |>
  mutate(weight_class = str_remove(weight_class, "Women's ")) |>
  mutate(place_of_birth = str_remove(place_of_birth, "^.*, ")) |>
  mutate(place_of_birth = if_else(
    is.na(place_of_birth), "Unknown", place_of_birth
  )) |>
  mutate(status = if_else(
    is.na(status), "Unknown", status
  )) |>
  left_join(ranks, by = c("name" = "fighter", "weight_class" = "weightclass")) |> 
  mutate(name = glue('<a href="{url}" target="_blank">{name}</a>')) |> # nolint
  select(
    `Weight class` = weight_class,
    name,
    Status = status,
    `Country of birth` = place_of_birth,
    Wins = wins,
    Draws = draws,
    Losses = losses,
    `Best rank` = rank
  ) |>
  arrange(`Weight class`, 
          `Best rank`,
          desc(Wins), desc(Draws), Losses)


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2026/2026-07-07/data.csv", na = "")
