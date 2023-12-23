# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-19")
holiday_episodes <- tuesdata$holiday_episodes
holiday_episode_genres <- tuesdata$holiday_episode_genres


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Mountains of Christmas", "xmas")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#fafafa"
text_col <- "#000000"
highlight_col <- "#af261c"

body_font <- "roboto"
title_font <- "xmas"


# Data wrangling ----------------------------------------------------------

plot_data <- holiday_episodes |>
  filter(christmas) |>
  select(average_rating, parent_average_rating) |>
  drop_na() |>
  mutate(
    comp = case_when(
      average_rating < parent_average_rating ~ "lower",
      average_rating == parent_average_rating ~ "same",
      average_rating > parent_average_rating ~ "higher"
    )
  ) |>
  count(comp) |>
  mutate(freq = round(18 * n / sum(n)))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-12-19", "recording"),
  device = "png",
  width = 5,
  height = 7.5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Christmas TV Episodes"
st <- glue::glue("Of the {format(sum(holiday_episodes$christmas), big.mark = ',')} 
                 Christmas episodes of TV programmes listed on IMDb, most of them 
                 ({format(plot_data$n[1], big.mark = ',')}) received a 
                 <span style='color: #819a47'>higher</span> average rating 
                 than the average for the TV series as a whole. In contrast, 
                 {format(plot_data$n[2], big.mark = ',')} received a 
                 <span style='color: {highlight_col}'>lower</span> average rating.")
cap <- paste0(
  "**Data**: IMDb<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

source("2023/2023-12-19/christmas_cracker.R")

ggplot() +
  # Add Christmas crackers
  # higher (10)
  christmas_cracker(x = 0, y = 0, col = "#819a47") +
  christmas_cracker(x = 0, y = 1.05, col = "#819a47") +
  christmas_cracker(x = 0, y = 2.10, col = "#819a47") +
  christmas_cracker(x = 0.5, y = 0, col = "#819a47") +
  christmas_cracker(x = 0.5, y = 1.05, col = "#819a47") +
  christmas_cracker(x = 0.5, y = 2.10, col = "#819a47") +
  christmas_cracker(x = 1, y = 0, col = "#819a47") +
  christmas_cracker(x = 1, y = 1.05, col = "#819a47") +
  christmas_cracker(x = 1, y = 2.10, col = "#819a47") +
  christmas_cracker(x = 1.5, y = 0, col = "#819a47") +
  # same (1)
  christmas_cracker(x = 1.5, y = 1.05, col = "#efd1a5") +
  # lower (7)
  christmas_cracker(x = 1.5, y = 2.10, col = highlight_col) +
  christmas_cracker(x = 2, y = 0, col = highlight_col) +
  christmas_cracker(x = 2, y = 1.05, col = highlight_col) +
  christmas_cracker(x = 2, y = 2.10, col = highlight_col) +
  christmas_cracker(x = 2.5, y = 0, col = highlight_col) +
  christmas_cracker(x = 2.5, y = 1.05, col = highlight_col) +
  christmas_cracker(x = 2.5, y = 2.10, col = highlight_col) +
  # Styling
  scale_colour_identity() +
  scale_fill_identity() +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 30, base_family = body_font) +
  theme(
    plot.margin = margin(10, 10, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      face = "bold",
      size = 65,
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-12-19", paste0("20231219", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
