# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-10-29")
monster_movie_genres <- tuesdata$monster_movie_genres
monster_movies <- tuesdata$monster_movies


# Load fonts --------------------------------------------------------------

font_add_google("Shadows Into Light", "shadows")
font_add_google("Creepster", "creepster")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "black"
text_col <- "white"
highlight_col <- "#FF9F1C"

body_font <- "shadows"
title_font <- "creepster"


# Data wrangling ----------------------------------------------------------

plot_data <- monster_movies |>
  filter(title_type == "movie") |>
  select(year, average_rating, genres)

highest_rated <- monster_movies |>
  filter(title_type == "movie") |>
  slice_max(average_rating, n = 1, with_ties = FALSE)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-10-29", "recording"),
  device = "png",
  width = 4,
  height = 5.5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  twitter = NA
)
title <- "Is horror better when it's real?"
st <- glue("The top four highest rated horror movies on IMDb are all
           <span style='color:{highlight_col}'>documentaries</span>,
           which also tend to be rated higher on average.")
cap <- paste0(
  "**Data**: IMDb<br>**Graphic**: ", social
)

# Plot --------------------------------------------------------------------

ggplot() +
  geom_point(
    data = filter(plot_data, str_detect(genres, "Documentary", negate = TRUE)),
    mapping = aes(
      x = year,
      y = average_rating
    ),
    colour = text_col,
    fill = text_col,
    pch = 21,
    alpha = 0.3
  ) +
  geom_point(
    data = filter(plot_data, str_detect(genres, "Documentary")),
    mapping = aes(x = year, y = average_rating),
    colour = highlight_col,
    fill = highlight_col,
    pch = 21,
    alpha = 0.55
  ) +
  with_outer_glow(
    geom_point(
      data = highest_rated,
      mapping = aes(x = year, y = average_rating),
      colour = highlight_col,
      fill = highlight_col,
      pch = 21,
      alpha = 1,
      size = 1.3
    ),
    colour = highlight_col,
    sigma = 10,
    expand = 8
  ) +
  annotate(
    "curve",
    x = 1955, y = 8.7,
    xend = 2018, yend = 9.6,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    colour = text_col,
    linewidth = 0.3,
    curvature = -0.2
  ) +
  annotate(
    "text",
    x = 1925, y = 8.5,
    label = str_wrap(glue("The highest rated horror movie is
                          {highest_rated$primary_title}, which was
                          released in {highest_rated$year} and has
                          an average rating of {highest_rated$average_rating}."), 26),
    colour = text_col,
    family = body_font,
    lineheight = 0.4,
    size = 11
  ) +
  scale_x_continuous(limits = c(1900, 2025)) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(
    x = "", y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_size = 32, base_family = body_font) +
  theme(
    axis.text = element_text(colour = text_col),
    axis.text.x = element_text(margin = margin(t = -5)),
    axis.text.y = element_text(margin = margin(r = -5)),
    panel.grid.major = element_line(
      linewidth = 0.5,
      colour = alpha(text_col, 0.3)
    ),
    panel.grid.minor = element_line(
      linewidth = 0.3,
      colour = alpha(text_col, 0.2)
    ),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      family = title_font,
      size = rel(1.8)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 15),
      lineheight = 0.4,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      lineheight = 0.4,
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-10-29", paste0("20241029", ".png")),
  bg = bg_col,
  width = 4,
  height = 5.5
)

gg_playback(
  name = file.path("2024", "2024-10-29", paste0("20241029", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
