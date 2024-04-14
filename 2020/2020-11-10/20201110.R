# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(scales)


# Load data ---------------------------------------------------------------

mobile <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "gray5"
text_col <- "#F8FFE5"
highlight_col <- "#1B9AAA"

body_font <- "roboto"
title_font <- "roboto_slab"


# Data wrangling ----------------------------------------------------------

set.seed(123)
plot_data <- mobile |>
  filter(entity == "Germany")


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "recording"),
  device = "png",
  width = 2,
  height = 4,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  twitter = NA,
  mastodon = NA,
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)
title <- "Mobile phone usage in Germany"
st <- "Mobile technology has spread rapidly around the globe. People in advanced
economies are more likely to have mobile phones and, in 2006, Germany exceeded
an average of one mobile subscription per person. Subscriptions have remained above
this threshold since then."
cap <- paste0(
  st,
  "<br><br>**Data**: Our World in Data<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  geom_area(
    mapping = aes(
      x = year,
      y = mobile_subs
    ),
    fill = highlight_col,
    colour = highlight_col,
    alpha = 0.5,
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 100,
    colour = text_col
  ) +
  annotate(
    "text",
    x = 1990, y = 105,
    label = "1 mobile per person",
    colour = text_col,
    hjust = 0,
    size = 6
  ) +
  scale_y_continuous(
    limits = c(0, 150),
    breaks = seq(0, 150, 50),
    minor_breaks = seq(0, 150, 25)
  ) +
  labs(
    title = title,
    caption = cap,
    subtitle = "Mobile subscriptions per 100 people",
    y = NULL
  ) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 21, base_family = body_font) +
  theme(
    plot.margin = margin(10, 7, 5, 7),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.2)
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
      family = body_font,
      size = rel(0.9)
    ),
    axis.text = element_text(
      colour = text_col
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      colour = text_col
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major = element_line(
      linewidth = 0.5,
      colour = alpha(text_col, 0.2)
    ),
    panel.grid.minor.y = element_line(
      linewidth = 0.5,
      colour = alpha(text_col, 0.2)
    ),
    panel.grid.minor.x = element_blank()
  )


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "viz", "gifs", paste0("day_11", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)

unlink("2024/recording/", recursive = TRUE)
