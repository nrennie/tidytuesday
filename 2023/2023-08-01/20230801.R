library(tidyverse)
library(showtext)
library(patchwork)
library(geofacet)
library(camcorder)
library(ggtext)
library(glue)

# load data
states <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv")
state_name_etymology <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/state_name_etymology.csv")

# load fonts
font_add_google("Roboto", "roboto")
font_add_google("Carter One", "carter")
showtext_auto()

# colours
col1 <- "#202A44"
col2 <- "#C41E3A"
bg_col <- "#fafafa"

# prep data
plot_data <- states |>
  select(postal_abbreviation, admission)
start <- lubridate::dmy("4 July 1776")
end <- Sys.Date()

# start recording
gg_record(
  dir = file.path("2023", "2023-08-01", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 12, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# inset plot
p_inset <- ggplot(data = filter(plot_data, postal_abbreviation == "CO")) +
  geom_rect(
    mapping = aes(
      xmin = start,
      xmax = admission,
      ymin = 0,
      ymax = 1
    ),
    colour = col1,
    fill = col1
  ) +
  geom_rect(
    mapping = aes(
      xmin = admission,
      xmax = end,
      ymin = 0,
      ymax = 1
    ),
    colour = col2,
    fill = col2
  ) +
  geom_text(
    mapping = aes(
      x = admission,
      y = 1.6,
      label = str_wrap("The date when the state was admitted to
                                   the union, or when it ratified the US
                                   Constitution.", 30)
    ),
    colour = col1,
    size = 11,
    vjust = 0,
    lineheight = 0.5,
    family = "roboto"
  ) +
  geom_text(
    mapping = aes(
      x = start,
      y = 1.6,
      label = "4 July\n1776"
    ),
    family = "roboto",
    colour = col1,
    vjust = 0,
    lineheight = 0.5,
    size = 11
  ) +
  geom_text(
    mapping = aes(
      x = end,
      y = 1.6,
      label = "Present\nday"
    ),
    family = "roboto",
    colour = col1,
    vjust = 0,
    lineheight = 0.5,
    size = 11
  ) +
  geom_segment(
    mapping = aes(
      x = admission,
      xend = admission,
      y = 1.5,
      yend = 1
    ),
    colour = col1,
    linewidth = 1,
    arrow = arrow(type = "closed", length = unit(0.1, "inches"))
  ) +
  geom_segment(
    mapping = aes(
      x = end,
      xend = end,
      y = 1.5,
      yend = 1
    ),
    colour = col1,
    linewidth = 1,
    arrow = arrow(type = "closed", length = unit(0.1, "inches"))
  ) +
  geom_segment(
    mapping = aes(
      x = start,
      xend = start,
      y = 1.5,
      yend = 1
    ),
    colour = col1,
    linewidth = 1,
    arrow = arrow(type = "closed", length = unit(0.1, "inches"))
  ) +
  geom_text(
    mapping = aes(
      x = mean(c(start, end)),
      y = 0.5,
      label = postal_abbreviation
    ),
    family = "carter",
    colour = alpha(bg_col, 0.7),
    size = 26
  ) +
  scale_x_date(expand = expansion(0.1, 0)) +
  scale_y_continuous(limits = c(0, 2.2)) +
  coord_cartesian() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col)
  )

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = col2,
  font_colour = col1,
  font_family = "roboto"
)
st <- "The United States is made up of a total of 50 states, who joined the 
union at different times. Alaska and Hawaii were the last to join, both 
in 1959."
cap <- paste0(
  st,
  "<br>",
  "**Data**: Wikipedia."
)
title <- "The United States of America"

# main plot
p_main <- ggplot(data = plot_data) +
  geom_rect(
    mapping = aes(
      xmin = start,
      xmax = admission,
      ymin = 0,
      ymax = 1
    ),
    colour = col1,
    fill = col1
  ) +
  geom_rect(
    mapping = aes(
      xmin = admission,
      xmax = end,
      ymin = 0,
      ymax = 1
    ),
    colour = col2,
    fill = col2
  ) +
  geom_text(
    mapping = aes(
      x = mean(c(start, end)),
      y = 0.5,
      label = postal_abbreviation
    ),
    family = "carter",
    colour = alpha(bg_col, 0.7),
    size = 12
  ) +
  facet_geo(~postal_abbreviation, grid = "us_state_without_DC_grid2") +
  coord_cartesian(expand = FALSE) +
  labs(title = title,
       tag = cap,
       caption = social) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(10, 10, 10, 300),
    plot.title = element_textbox_simple(
      hjust =  -1.01,
      halign = -1.01,
      colour = col1,
      face = "bold",
      family = "carter",
      lineheight = 0.5,
      size = 60,
      margin = margin(b = 20)
    ),
    plot.tag = element_textbox_simple(
      hjust =  0,
      colour = col1,
      size = 35,
      maxwidth = 0.65,
      lineheight = 0.5,
      family = "roboto",
      margin = margin(b = 20)
    ),
    plot.caption = element_textbox_simple(
      hjust = -1.01,
      halign = -1.01,
      colour = col1,
      size = 34,
      maxwidth = 0.65,
      lineheight = 0.5,
      family = "roboto",
      margin = margin(b = 5, t = 10)
    ),
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.tag.position = c(-0.51, 0.76)
  )
record_polaroid()

# combine plots
p_main + inset_element(p_inset, 0.02, 0.08, 0.32, 0.62, align_to = "full", clip = FALSE) 
record_polaroid()

# save gif
gg_playback(
  name = file.path("2023", "2023-08-01","20230801.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
