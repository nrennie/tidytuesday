# Load packages -----------------------------------------------------------

library(tidyverse)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-05-12")
cities <- tuesdata$cities |> 
  group_by(name, country) |>
  mutate(n = n()) |> 
  ungroup() |> 
  mutate(
    name = if_else(
      n > 1, paste0(name, " (", round(lat, 3), ",", round(lng, 3), ")"), name
    )
  ) |> 
  select(-n) |> 
  group_by(name) |>
  mutate(n = n()) |> 
  ungroup() |> 
  mutate(
    name = if_else(
      n > 1, paste0(name, " (", countrycd, ")"), name
    )
  ) |> 
  select(-n)
links <- tuesdata$links


# Helper functions --------------------------------------------------------

filter_data <- function(chosen_city) {
  city_id <- cities |>
    filter(name == chosen_city) |>
    pull(id)
  
  city_col <- cities |>
    filter(name == chosen_city) |> 
    pull(continent) |> 
    unique()
  
  plot_data <- links |>
    filter(source %in% city_id | target %in% city_id) |>
    mutate(
      id = if_else(
        source %in% city_id, target, source
      )
    ) |>
    select(id) |>
    left_join(
      cities,
      by = "id"
    ) |>
    select(name, country, continent) |>
    rename(city = name) |> 
    mutate(continent_col = continent) |> 
    mutate(city = glue("<b>{city}</b>, {country}, {continent}"),
           country = glue("<b>{country}</b>, {continent}"),
           continent = glue("<b>{continent}</b>")) |> 
      arrange(continent, country) |> 
    mutate(order = row_number()) |> 
    mutate(chosen_city = chosen_city,
           city_col = city_col)
  return(plot_data)
}


# Process data ------------------------------------------------------------

cities_data <- purrr::map_df(
  .x = cities$name,
  .f = ~filter_data(.x)
)

write_csv(cities_data, "2026/2026-05-12/cities_data.csv")

