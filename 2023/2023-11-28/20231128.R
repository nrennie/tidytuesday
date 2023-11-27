# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggalt)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-28")
drwho_episodes <- tuesdata$drwho_episodes
drwho_directors <- tuesdata$drwho_directors
drwho_writers <- tuesdata$drwho_writers


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#dedede"
text_col <- "gray20"
highlight_col <- "#003B6F"

body_font <- "roboto"
title_font <- "roboto"


# Data wrangling ----------------------------------------------------------

# remake previous chart
plot_data <- drwho_episodes |>
  group_by(season_number) |>
  summarise(
    min_viewers = min(uk_viewers),
    max_viewers = max(uk_viewers)
  ) |>
  drop_na() |>
  mutate(season_number = factor(season_number,
    levels = 1:13,
    labels = paste(rep("Season", 13), 1:13)
  ))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-11-28", "recording"),
  device = "png",
  width = 7,
  height = 5,
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
title <- "Who is watching Doctor Who?"
st <- "The number of people watching the revival of Doctor Who has been steadily
declining in recent years. On Christmas Day 2007, Voyage of the Damned became
the most watched episode with 13.3 million people tuning in. Jodie Whittaker's
first series as the Doctor brought back an increase in viewership."
cap <- paste0(
  "**Data**: {datardis}<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

main_plot <- ggplot() +
  geom_dumbbell(
    data = plot_data,
    mapping = aes(x = min_viewers, 
                  xend = max_viewers, 
                  y = season_number),
    colour_x = highlight_col,
    colour_xend = highlight_col,
    color = alpha(text_col, 0.5),
    size = 0.9,
    size_x = 2.5,
    size_xend = 2.5
  ) +
  # Text labels
  geom_text(
    data = filter(plot_data, season_number == "Season 13"),
    mapping = aes(x = min_viewers, 
                  y = 13.5,
                  label = "Min"),
    family = body_font,
    size = 7
  ) +
  geom_text(
    data = filter(plot_data, season_number == "Season 13"),
    mapping = aes(x = max_viewers, 
                  y = 13.5,
                  label = "Max"),
    family = body_font,
    size = 7
  ) +
  # Other text
  labs(
    x = "UK Viewers (millions)",
    y = "",
    title = title,
    tag = st,
    caption = cap
  ) +
  theme_minimal(base_size = 24, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 200),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_line(colour = text_col, linewidth = 0.1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = -2.7,
      margin = margin(b = 50, t = 10),
      lineheight = 0.5,
      size = 50,
      face = "bold",
      family = body_font
    ),
    plot.tag.position = c(-0.66, 0.84),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      width = 1.3,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = -3,
      margin = margin(b = 5, t = -15),
      lineheight = 0.5,
      family = body_font
    )
  )

main_plot + inset_element(tardis, -1.8, -0.1, 0.45, 1.0)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-11-28", paste0("20231128", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
