# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load fonts --------------------------------------------------------------

font_add_google("Zalando Sans", "Zalando", db_cache = FALSE)
font_add_google("DM Serif Display", "DM")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours and fonts-------------------------------------------------

bg_col <- "grey90"
text_col <- "black"

body_font <- "Zalando"
title_font <- "DM"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2025", "2025-11-11", "recording"),
  device = "png",
  width = 6,
  height = 7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "The History of the Akimel O’odham"
st <- "and the *Pima Indians Diabetes Database*"
cap <- paste0(
  "**Graphic**: ", social
)


# Load data ---------------------------------------------------------------

history <- read_csv("2025/2025-11-11/history.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- history |>
  mutate(
    label = glue(
      "**{year_label}**<br>{note}"
    )
  ) |>
  mutate(
    id = row_number(),
    hjust = if_else(
      usefunc::is_odd(id), 0, 1
    )
  ) |> 
  select(hjust, year, label)


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data
) +
  geom_point(
    mapping = aes(
      x = 0, y = year
    ),
    size = 3
  ) +
  geom_textbox(
    mapping = aes(
      x = 0, y = year,
      label = label,
      hjust = hjust,
      halign = hjust
    ),
    fill = "transparent",
    box.colour = "transparent",
    box.padding = unit(c(-3, 7.5, 5.5, 7.5), "pt"),
    vjust = 1,
    valign = 1,
    minwidth = unit(3, "in"),
    family = body_font,
    size = 3
  ) +
  geom_vline(xintercept = 0) +
  scale_y_reverse() +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    )
  )


# Save gif ----------------------------------------------------------------

ggsave(
  filename = file.path("2025", "2025-11-11", paste0("20251111", ".png")),
  width = 6,
  height = 7,
  bg = bg_col,
  units = "in",
  dpi = 300
)

gg_playback(
  name = file.path("2025", "2025-11-11", paste0("20251111", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
