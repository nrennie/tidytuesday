# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(sf)
library(gglgbtq)
library(cowplot)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-06-11")
pride_index <- tuesdata$pride_index
pride_index_tags <- tuesdata$pride_index_tags

# From https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
us_sf <- read_sf("2024/2024-06-11/us_states_hexgrid.geojson")


# Load fonts --------------------------------------------------------------

font_add_google("Special Elite", "elite")
font_add_google("Just Another Hand", "hand")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "grey5"
text_col <- "white"
highlight_col <- palette_lgbtq("rainbow")[3]

body_font <- "hand"
title_font <- "elite"


# Data wrangling ----------------------------------------------------------

# mean rating per state
pride_data <- pride_index |>
  separate_wider_delim(
    cols = campus_location,
    delim = ",",
    too_many = "drop",
    names = c("location", "state")
  ) |>
  mutate(state = str_trim(state)) |>
  group_by(state) |>
  summarise(
    mean_rating = mean(rating),
    students = sum(students)
  ) |>
  ungroup()

# join pride data to map
map_data <- us_sf |>
  mutate(google_name = str_remove(google_name, " \\(United States\\)")) |>
  left_join(pride_data, by = c("iso3166_2" = "state"))

# get centres
centres <- sf::st_centroid(map_data)
centres_text <- centres |>
  st_coordinates() |>
  as_tibble() |>
  mutate(iso3166_2 = centres$iso3166_2)

# reformat to a tibble
plot_data <- map_data |>
  sfheaders::sf_to_df(fill = TRUE) |>
  as_tibble()


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-06-11", "recording"),
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
  font_family = title_font
)
title <- "Campus Pride Index"
txt1 <- '"*Since 2007, the Campus Pride Index has been the premier LGBTQ
         national benchmarking tool for colleges and universities to create
         safer, more inclusive campus communities."* - Campus Pride Index'
txt2 <- glue(
  "Inclusion in the Campus Pride Index is voluntary, and some colleges have not
 provided information. The index ranges from 1 to 5 and, of the {nrow(pride_index)}
 colleges included here, the average rating was {round(mean(pride_index$rating), 2)}."
)
txt3 <- glue(
  "{length(slice_max(pride_index, rating))} of the listed colleges and universities
  scored the maximum rating of 5 out of 5. The lowest score listed was {min(pride_index$rating)}."
)
cap <- paste0(
  "**Data**: Campus Pride Index<br>**Graphic**:", social
)


# Plot --------------------------------------------------------------------

main_plot <- ggplot() +
  geom_polygon(
    data = plot_data,
    mapping = aes(x = x, y = y, group = iso3166_2, fill = mean_rating),
    colour = text_col
  ) +
  geom_text(
    data = centres_text,
    mapping = aes(x = X, y = Y, label = iso3166_2),
    colour = text_col,
    size = 13,
    family = title_font
  ) +
  geom_textbox(
    data = data.frame(x = -105, y = 54, label = txt1),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    box.colour = NA,
    family = body_font,
    lineheight = 0.4,
    hjust = 0.5,
    halign = 0.5,
    size = 14,
    fill = NA,
    maxwidth = 0.47,
    minwidth = 0.47
  ) +
  geom_textbox(
    data = data.frame(x = -71, y = 27, label = txt2),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    box.colour = NA,
    family = body_font,
    lineheight = 0.4,
    hjust = 0.5,
    halign = 0.5,
    size = 14,
    fill = NA,
    maxwidth = 0.45,
    minwidth = 0.3
  ) +
  geom_textbox(
    data = data.frame(x = -141, y = 42, label = txt3),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    box.colour = NA,
    family = body_font,
    lineheight = 0.4,
    hjust = 0.5,
    halign = 0.5,
    size = 14,
    fill = NA,
    maxwidth = 0.22,
    minwidth = 0.22
  ) +
  scale_x_continuous(limits = c(-148, -59)) +
  scale_y_continuous(limits = c(21, 56)) +
  scale_fill_gradientn(
    colours = palette_lgbtq("rainbow"),
    limits = c(1, 5),
    breaks = 1:5,
    na.value = bg_col
  ) +
  coord_map() +
  theme_void(base_family = body_font, base_size = 24) +
  labs(
    title = title,
    caption = cap
  ) +
  theme(
    # legend text
    legend.title = element_blank(),
    legend.text = element_text(
      color = text_col,
      lineheight = 0.5,
      hjust = 0.5,
      size = rel(1.2),
      margin = margin(t = 5)
    ),
    # legend size
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.2, "cm"),
    # legend position
    legend.position = "bottom",
    legend.margin = margin(-5, 5, 0, 0),
    legend.direction = "horizontal",
    legend.ticks = element_blank(),
    # plot
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10),
      lineheight = 0.5,
      size = rel(2.3),
      face = "bold",
      family = title_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = -5),
      lineheight = 0.5,
      family = title_font
    )
  )

ggdraw(main_plot) +
  annotate(
    geom = "curve",
    x = 0.8,
    y = 0.5,
    xend = 0.85,
    yend = 0.41,
    linewidth = 0.5,
    colour = text_col,
    curvature = -0.5,
    arrow = arrow(length = unit(2.5, "mm"), type = "closed")
  ) +
  annotate(
    geom = "curve",
    x = 0.65,
    y = 0.64,
    xend = 0.63,
    yend = 0.73,
    linewidth = 0.5,
    colour = text_col,
    curvature = 0.5,
    arrow = arrow(length = unit(2.5, "mm"), type = "closed")
  ) +
  annotate(
    geom = "curve",
    x = 0.23,
    y = 0.41,
    xend = 0.15,
    yend = 0.42,
    linewidth = 0.5,
    colour = text_col,
    curvature = -0.5,
    arrow = arrow(length = unit(2.5, "mm"), type = "closed")
  ) 


# Save png ----------------------------------------------------------------

ggsave("2024/2024-06-11/20240611.png", width = 7, height = 5, bg = bg_col)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-06-11", paste0("20240611", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
