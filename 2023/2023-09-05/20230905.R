
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(gghighlight)
library(geofacet)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- "#FAFAFA"
text_col <- "grey10"
highlight_col <- "#b20e10"


# Data wrangling ----------------------------------------------------------

plot_data <- states |> 
  filter(sector == "Total") |> 
  select(state_abbreviation, year, p_members) |> 
  mutate(p_members = 100*p_members)


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2023", "2023-09-05", "recording"),
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "roboto"
)
title <- "Union Membership in the United States"
st <- "The percentage of employed workers who are union members has generally 
decreased across most states since the early 1990s. Notable exceptions are Vermont 
and New Hampshire which have maintained steady levels of union membership. South 
Carolina has consistently shown the lowest percentage of union membership."
cap <- paste0(
  "**Data**: unionstats.com © 2023 by Barry T. Hirsch, David A. Macpherson, and William E. Even<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot(data = plot_data) +
  geom_line(mapping = aes(x = year, y = p_members, colour = state_abbreviation)) +
  gghighlight(use_direct_label = FALSE) +
  geom_text(data = filter(plot_data, year == 2017),
            mapping = aes(x = 2017, y = 28, label = state_abbreviation),
            family = "robotoslab",
            fontface = "bold",
            size = 8) +
  facet_geo(~state_abbreviation, grid = "us_state_grid1") +
  labs(title = title,
       subtitle = st,
       caption = cap,
       y = "Percent of employed workers who are union members") +
  scale_x_continuous(breaks = c(1990, 2010)) +
  scale_colour_manual(values = rep(highlight_col, length(unique(plot_data$state_abbreviation)))) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 28, base_family = "roboto") +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      colour = text_col,
      face = "bold",
      family = "robotoslab",
      lineheight = 0.5,
      size = 54,
      margin = margin(b = 20)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      maxwidth = 0.45,
      halign = 0,
      hjust = 0,
      lineheight = 0.5,
      family = "roboto",
      margin = margin(b = -30)
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      lineheight = 0.5,
      family = "roboto",
      margin = margin(b = 5, t = 15)
    ),
    panel.grid.major = element_line(linewidth = 0.4),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(hjust = 0),
    axis.text.x = element_text(margin = margin(t = -5)),
    axis.text.y = element_text(margin = margin(r = -5)),
    strip.background = element_blank(),
    strip.text = element_blank()
  )
record_polaroid()


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2023", "2023-09-05", paste0("20230905", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
