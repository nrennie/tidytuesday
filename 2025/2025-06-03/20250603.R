

# Packages ----------------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-06-03")
gutenberg_languages <- tuesdata$gutenberg_languages


# Data wrangling ----------------------------------------------------------

plot_data <- gutenberg_languages |> 
  mutate(is_eng = if_else(language == "en", 1, 0)) |> 
  count(is_eng) |> 
  mutate(tot = sum(n),
         perc = round(100 * n / tot)) |> 
  select(is_eng, perc) |> 
  uncount(weights = perc) |>
  mutate(
    x = rep(1:10, each = 10),
    y = rep(1:10, times = 10)
  ) |>
  mutate(
    colour = case_when(
      is_eng == 1 ~ "#A30000",
      is_eng == 0 ~ "#B2BDBD"
    )
  )


# Write CSV ---------------------------------------------------------------

readr::write_csv(plot_data, "2025/2025-06-03/data.csv")

