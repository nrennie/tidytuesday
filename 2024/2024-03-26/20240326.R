# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-03-26")
team_results <- tuesdata$`team-results`
public_picks <- tuesdata$`public-picks`


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey97"
text_col <- "black"
highlight_col <- "#1879bf"
col_palette <- monochromeR::generate_palette(highlight_col,
  modification = "go_both_ways", n_colours = 7
)
"#cc1e4c"

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

plot_data <- public_picks |>
  filter(YEAR == 2024) |>
  select(TEAM:FINALS) |>
  mutate(across(R64:FINALS, ~ as.numeric(str_remove(.x, "%")))) |>
  mutate(TEAM = reorder(TEAM, R64)) |>
  pivot_longer(-TEAM, names_to = "round", values_to = "perc") |>
  mutate(
    round = case_when(
      round == "R64" ~ "Round of 64",
      round == "R32" ~ "Round of 32",
      round == "S16" ~ "Sweet 16",
      round == "E8" ~ "Elite 8",
      round == "F4" ~ "Final 4",
      round == "FINALS" ~ "Finals",
    ),
    round = factor(round,
      levels = c(
        "Round of 64", "Round of 32", "Sweet 16",
        "Elite 8", "Final 4", "Finals"
      )
    )
  )


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-03-26", "recording"),
  device = "png",
  width = 7,
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
title <- "NCAA Men's March Madness 2024"
st <- "March Madness is the NCAA Division I basketball tournament for women and 
men. It's a single-elimination tournament of 68 teams that compete in six rounds 
for the national championship. The public make predictions about who will win 
in each round of this year's tournament. What teams are people predicting will 
do well this year?"
cap <- paste0(
  "**Data**: kaggle.com<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_raster(
    data = plot_data,
    mapping = aes(x = TEAM, y = round, fill = perc)
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap,
    x = "", y = ""
  ) +
  scale_y_discrete(position = "right") +
  scale_fill_gradientn(
    limits = c(0, 100),
    colours = col_palette,
    name = "Percentage of people who picked the team to win the game in each round."
  ) +
  guides(fill = guide_colourbar(title.position = "top")) +
  theme_minimal(base_size = 26, base_family = body_font) +
  theme(
    plot.margin = margin(5, 15, 5, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      family = title_font,
      size = rel(2)
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
      margin = margin(b = 5, t = 10),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      margin = margin(t = -5)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank(),
    legend.key.width = unit(2.95, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5, margin = margin(t = -25, b = 5))
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-03-26", paste0("20240326", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
