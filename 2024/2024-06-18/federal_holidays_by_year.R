
# Date processing function ------------------------------------------------

get_date <- function(year = YEAR, mon, num_day, dow) {
  if (any(is.na(c(mon, num_day, dow)))) {
    return(NA)
  }
  m <- match(mon, month.name)
  # generate dates
  dates_seq <- seq(from = ymd(paste(year, m, 1)), to = (ymd(paste(year, m + 1, 1)) - 1), by = "day")
  # filter to day of week
  day_df <- tibble(date = dates_seq) |>
    mutate(wday = wday(date, label = TRUE, abbr = FALSE)) |>
    filter(wday == dow)
  # correct type
  if (num_day == "last") {
    output <- day_df |>
      slice_tail(n = 1) |>
      pull(date)
  } else {
    output <- day_df |>
      slice(as.numeric(num_day)) |>
      pull(date)
  }
  return(output)
}


# Parameterised plot function ---------------------------------------------

federal_holidays_by_year <- function(
    YEAR = 2024,
    bg_col = "#F4F4F9",
    text_col = "#0B4F6C",
    highlight_col = "#01BAEF",
    body_font = "roboto",
    title_font = "robotoslab",
    data = federal_holidays) {
  # process data
  process_dates <- data |>
    mutate(
      month = gsub(" .*$", "", date),
      .after = 1
    ) |>
    mutate(
      num_day = case_when(
        str_detect(date_definition, "last") ~ "last",
        date_definition != "fixed date" ~ str_extract(date_definition, "(\\d)+"),
        TRUE ~ NA_character_
      ),
      .after = 1
    ) |>
    mutate(
      dow = case_when(
        date_definition != "fixed date" ~ str_to_title(str_trim(str_extract(date_definition, "\\s(.*)"))),
        TRUE ~ NA_character_
      ),
      .after = 2
    )

  # get dates
  process_dates$new_date <- NA_Date_
  for (i in 1:nrow(process_dates)) {
    if (process_dates$date_definition[i] == "fixed date") {
      process_dates$new_date[i] <- lubridate::mdy(paste(process_dates$date[i], YEAR))
    } else {
      process_dates$new_date[i] <- get_date(
        year = YEAR,
        mon = process_dates$month[i],
        num_day = process_dates$num_day[i],
        dow = process_dates$dow[i]
      )
    }
  }

  # subset
  plot_data <- process_dates |>
    select(new_date, official_name, year_established) |>
    rename(date = new_date)

  # plot prep
  social <- nrBrand::social_caption(
    bg_colour = bg_col,
    icon_colour = highlight_col,
    font_colour = text_col,
    font_family = body_font
  )
  title <- glue::glue("Federal holidays in {YEAR}")
  st <- glue::glue("There are {nrow(plot_data)} federal holidays in the USA in {YEAR}.")
  cap <- paste0(
    "**Data**: Wikipedia<br>**Graphic**: ", social
  )

  # plot
  g <- df_year(YEAR) |> 
    ggcalendar() +
    geom_tile_calendar(
      data = plot_data,
      fill = highlight_col,
      alpha = 0.25
    ) +
    labs(
      title = title,
      subtitle = st,
      caption = cap
    ) +
    theme_void() +
    theme(
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 10, t = 0),
        lineheight = 0.5,
        size = rel(1.5),
        face = "bold",
        family = title_font
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 10, t = 0),
        lineheight = 0.5,
        family = body_font
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 0, t = 10),
        family = body_font
      ),
      strip.text = element_text(margin = margin(t = 5, b = 5))
    )

  return(g)
}


# Examples ----------------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggcalendar)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load("2024-06-18")
federal_holidays <- tuesdata$federal_holidays

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()
showtext_opts(dpi = 300)

federal_holidays_by_year(2024)
federal_holidays_by_year(2025)
federal_holidays_by_year(2030)

