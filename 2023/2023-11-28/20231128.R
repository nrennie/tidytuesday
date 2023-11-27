
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-28")


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""

body_font <- "roboto"
title_font <- "roboto"


# Data wrangling ----------------------------------------------------------



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
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", social
)


# Plot --------------------------------------------------------------------


theme(
  plot.margin = margin(5, 5, 5, 5),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
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
  name = file.path("2023", "2023-11-28", paste0("20231128", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
