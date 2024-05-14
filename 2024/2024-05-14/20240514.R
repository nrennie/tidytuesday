# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(statebins)


# Load data ---------------------------------------------------------------

coffee_survey <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv")


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()

sysfonts::font_add(family = "Font Awesome 6",
                   regular = "fonts/Font Awesome 6 Free-Solid-900.otf")
showtext::showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "#B0A084"
text_col <- "#60492C"
highlight_col <- "#E7E2DA"

body_font <- "ubuntu"
title_font <- "ubuntu"


# Data wrangling ----------------------------------------------------------

coffee_icon <- glue::glue(
  "<span style='font-family:\"Font Awesome 6\"; color:{highlight_col};'>&#xf0f4;</span>"
)

plot_data <- coffee_survey |>
  select(prefer_overall) |>
  drop_na() |>
  count(prefer_overall) |>
  mutate(perc = n / sum(n)) |> 
  mutate(
    x = 1:4, 
    y = rep(1, 4),
    icon = rep(coffee_icon, 4),
    text = c(
      "<br>**Kenyan beans**<br>*Light roast*",
      "<br>**Kenyan beans**<br>*Medium roast*",
      "<br>**Kenyan beans**<br>*Dark roast*",
      "<br>**Columbian beans**<br>*Experimental processing*"
    )
  ) |> 
  mutate(
    height = perc * (0.34+0.1) + (y - 0.1)
  )

# y - 0.1, y + 0.34

# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-05-14", "recording"),
  device = "png",
  width = 7,
  height = 4,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = "black",
  font_family = body_font
)
title <- "The Great American Coffee Taste Test"
st <- glue("In October 2023, world champion barista James Hoffmann and coffee 
           company Cometeer held the **Great American Coffee Taste Test** on 
           YouTube, during which viewers were asked to fill out a survey about 
           4 coffees they ordered from Cometeer for the tasting. Of 
           the {format(sum(plot_data$n), big.mark = ',')} people who
           answered 'what was your favorite overall coffee?', almost 37% of 
           them chose coffee D - the Columbian beans.")
cap <- paste0(
  "**Data**: Cometeer<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

ggplot(plot_data) +
  geom_textbox(
    mapping = aes(
      x = x, y = y, label = icon
    ),
    box.colour = NA,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    size = 90
  ) +
  # text
  geom_textbox(
    mapping = aes(x = x, y = y - 0.3, 
                  label = paste0(prefer_overall, text)),
    family = body_font,
    box.colour = NA,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    vjust = 1,
    size = 8
  ) +
  geom_textbox(
    mapping = aes(x = x, y = y + 0.45, 
                  label = paste0(round(100*perc, 1), "%")),
    family = body_font,
    box.colour = NA,
    fill = NA,
    hjust = 0.5,
    halign = 0.5,
    fontface = "bold",
    lineheight = 0.5,
    size = 10
  ) +
  # fill
  statebins:::geom_rrect(
    mapping = aes(
      xmin = x - 0.29,
      xmax = x + 0.2,
      ymin = y - 0.1,
      ymax = y + 0.34,
    ),
    fill = text_col,
    radius = grid::unit(12, "pt")
  ) +
  geom_rect(
    mapping = aes(
      xmin = x - 0.29,
      xmax = x + 0.2,
      ymin = height,
      ymax = y + 0.34,
    ),
    fill = "#9E9076"
  ) +
  scale_x_continuous(limits = c(0.5, 4.5)) +
  scale_y_continuous(limits = c(0.1, 1.8)) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_fixed() +
  theme_void(base_size = 24, base_family = body_font) +
  theme(
  plot.margin = margin(5, 10, 5, 10),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  plot.title = element_textbox_simple(
    colour = "black",
    hjust = 0.5,
    halign = 0.5,
    margin = margin(b = 10, t = 10),
    lineheight = 0.5,
    size = rel(2),
    face = "bold",
    family = title_font
  ),
  plot.subtitle = element_textbox_simple(
    colour = "black",
    hjust = 0.5,
    halign = 0.5,
    margin = margin(b = -10, t = 10, r = 10, l = 10),
    lineheight = 0.5,
    family = body_font
  ),
  plot.caption = element_textbox_simple(
    colour = "black",
    hjust = 0.5,
    halign = 0.5,
    margin = margin(b = 0, t = 0),
    lineheight = 0.5,
    family = body_font
  )
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-05-14", paste0("20240514", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
