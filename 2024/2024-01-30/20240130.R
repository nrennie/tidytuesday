# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggimage)
library(cropcircles)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-01-30")
groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions


# Load fonts --------------------------------------------------------------

font_add_google("Josefin Sans", "josefin")
font_add_google("Carter One", "carter")

showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey25"
text_col <- "white"
col_palette <- c("Shadow" = "#129490",
                 "No shadow" = "#70B77E",
                 "Groundhog" = "#E0A890",
                 "Not a groundhog" = "#CE1483")
highlight_col <- col_palette[1]

body_font <- "josefin"
title_font <- "carter"


# Data wrangling ----------------------------------------------------------

grid_layout <- expand.grid(x = 1:10, y = 1:7) |>
  mutate(id = row_number())

plot_data <- predictions |>
  filter(year == 2023) |>
  select(id, shadow) |>
  left_join(groundhogs, by = "id") |>
  select(id, image, is_groundhog, shadow) |>
  drop_na() |>
  mutate(id = row_number()) |>
  left_join(grid_layout, by = "id") |>
  mutate(
    shadow = case_when(shadow ~ "Shadow", TRUE ~ "No shadow"),
    is_groundhog = case_when(is_groundhog ~ "Groundhog", TRUE ~ "Not a groundhog")
  ) |> 
  mutate(img_sq = crop_square(image))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-01-30", "recording"),
  device = "png",
  width = 6,
  height = 7,
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
title <- "2023 Groundhog Predictions"
st <- glue("If a groundhog sees its shadow and goes back into its burrow on Groundhog 
Day, that is a prediction of six more weeks of winter. Otherwise spring will 
come early.<br>The outer border colour of the images represents with a groundhog
<span style='color: {col_palette[1]};'>saw its shadow</span> or 
<span style='color: {col_palette[2]};'>did not see its shadow</span>. 
The inner border colour represents whether it was a 
<span style='color: {col_palette[3]};'>real groundhog</span> or 
<span style='color: {col_palette[4]};'>not a real groundhog</span>.")
cap <- paste0(
  "**Data**: groundhog-day.com<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = x, y = y)
) +
  geom_tile(mapping = aes(fill = shadow), width = 0.95, height = 0.95) +
  geom_tile(mapping = aes(fill = is_groundhog), width = 0.8, height = 0.8) +
  geom_image(mapping = aes(image = img_sq), size = 0.09) +
  scale_fill_manual(values = col_palette) +
  coord_fixed(expand = FALSE) +
  labs(title = title,
       subtitle = st,
       caption = cap) +
  theme_void(base_size = 34) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 15),
      lineheight = 0.5,
      size = 48,
      face = "bold",
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 25, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 15),
      lineheight = 0.5,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-01-30", paste0("20240130", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
