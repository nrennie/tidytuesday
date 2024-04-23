# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(ggforce)
library(ggimage)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-04-23")
outer_space_objects <- tuesdata$outer_space_objects


# Load fonts --------------------------------------------------------------

font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "gray3"
text_col <- "gray97"
highlight_col <- "#6369D1"

body_font <- "ubuntu"
title_font <- "ubuntu"


# Data wrangling ----------------------------------------------------------

r <- 2

space_data <- outer_space_objects |>
  filter(Entity == "World")

plot_data <- space_data |> 
  mutate(
    theta = seq(
      to = ((pi * 90) / 180),
      from = pi * (2 + ((90) / 180)),
      length.out = nrow(space_data) + 1
    )[1:nrow(space_data)],
    x_in = r * cos(.data$theta),
    x_out = (log(num_objects) + r) * cos(.data$theta),
    y_in = r * sin(.data$theta),
    y_out = (log(num_objects) + r) * sin(.data$theta)
  ) |>
  select(-c(Entity, Code, num_objects)) |> 
  pivot_longer(cols = -c(Year, theta)) |> 
  separate(col = name, into = c("var", "series"), sep = "_") |> 
  pivot_wider(id_cols = c(Year, series, theta), names_from = var, values_from = value) |> 
  mutate(angle = 180 + 360 * (.data$theta / (2 * pi)))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-04-23", "recording"),
  device = "png",
  width = 5,
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
title <- "Objects Launched into Space"
st <- "Objects are defined here as satellites, probes, landers, crewed 
spacecrafts, and space station flight elements launched into Earth orbit or 
beyond. This data is based on national registers of launches submitted to the 
UN by participating nations. According to UN estimates, the data captures around 
88% of all objects launched. In 2023, the world launched 2,664 objects 
into space compared to just 2 in 1957."
cap <- paste0(
  "**Data**: Our World in Data | **Image**: NASA<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  # axis lines
  geom_circle(
    data = data.frame(
      x0 = c(0, 0, 0),
      y0 = c(0, 0, 0),
      rad = log(c(10, 100, 1000)) + r),
    mapping = aes(x0 = x0, y0 = y0, r = rad),
    colour = alpha(text_col, 0.5)
  ) +
  # lollipop
  geom_line(
    data = plot_data,
    mapping = aes(x = x, y = y, group = Year),
    colour = highlight_col
  ) +
  geom_point(
    data = filter(plot_data, series == "out"),
    mapping = aes(x = x, y = y),
    colour = highlight_col
  ) +
  # year labels
  geom_text(
    data = filter(plot_data, series == "out"),
    mapping = aes(x = x, y = y, label = paste(Year, " "), angle = angle),
    colour = text_col,
    family = body_font,
    size.unit = "pt",
    size = 22,
    hjust = 1.2
  ) +
  # add logo
  geom_image(
    data = slice_head(plot_data, n = 1),
    aes(
      x = 0,
      y = 0,
      image = "2024/2024-04-23/images/earth.png"
    ),
    size = 0.3
  ) +
  # limits
  scale_y_continuous(
    limits = c(-ceiling(max(plot_data$y))+0.5, ceiling(max(plot_data$y)))
  ) +
  # annotate lines
  annotate(
    "segment", x = log(10) + r, y = 0, yend = log(1000 + 500) + r,
    colour = alpha(text_col, 0.7), linetype = "dashed"
  ) +
  annotate(
    "segment", x = log(100) + r, y = 0, yend = log(1000 + 500) + r,
    colour = alpha(text_col, 0.7), linetype = "dashed"
  ) +
  annotate(
    "segment", x = log(1000) + r, y = 0, yend = log(1000 + 500) + r,
    colour = alpha(text_col, 0.7), linetype = "dashed"
  ) +
  # annotate labels
  annotate(
    "text", x = log(10) + r, y = log(1000 + 500) + r, label = "10\nobjects",
    colour = text_col, lineheight = 0.4, vjust = -0.2,
    size.unit = "pt", size = 22, family = body_font
  ) +
  annotate(
    "text", x = log(100) + r, y = log(1000 + 500) + r, label = "100\nobjects",
    colour = text_col, lineheight = 0.4, vjust = -0.2,
    size.unit = "pt", size = 22, family = body_font
  ) +
  annotate(
    "text", x = log(1000) + r, y = log(1000 + 500) + r, label = "1,000\nobjects",
    colour = text_col, lineheight = 0.4, vjust = -0.2,
    size.unit = "pt", size = 22, family = body_font
  ) +
  labs(title = title, 
       subtitle = st,
       caption = cap) +
  coord_fixed() +
  theme_void(base_size = 24) +
  theme(
  plot.margin = margin(5, 5, 5, 5),
  plot.background = element_rect(fill = bg_col, colour = bg_col),
  panel.background = element_rect(fill = bg_col, colour = bg_col),
  plot.title = element_textbox_simple(
    colour = text_col,
    hjust = 0.5,
    halign = 0.5,
    margin = margin(b = 10, t = 10),
    lineheight = 0.5,
    size = rel(1.8),
    face = "bold",
    family = title_font
  ),
  plot.subtitle = element_textbox_simple(
    colour = text_col,
    hjust = 0.5,
    halign = 0.5,
    margin = margin(b = 10, t = 10),
    lineheight = 0.5,
    family = body_font
  ),
  plot.caption = element_textbox_simple(
    colour = text_col,
    hjust = 0.5,
    halign = 0.5,
    margin = margin(b = 10, t = -10),
    lineheight = 0.5,
    family = body_font
  )
)


# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path("2024", "2024-04-23", paste0("20240423", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
