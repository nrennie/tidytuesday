library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)
library(camcorder)
library(emojifont)
library(ggtext)
library(glue)
library(nrBrand)

# load data
historical_markers <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv")
no_markers <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/no_markers.csv")

# prep data
state_data <- historical_markers |>
  select(state_or_prov, missing) |>
  group_by(state_or_prov) |>
  mutate(total_markers = n()) |>
  filter(!is.na(missing)) |>
  mutate(perc_missing = round(100 * n() / total_markers, 1)) |>
  ungroup() |>
  select(state_or_prov, perc_missing) |>
  distinct()

plot_data <- historical_markers |>
  filter(!(state_or_prov %in% state_data$state_or_prov)) |>
  select(state_or_prov) |>
  distinct() |>
  mutate(perc_missing = 0) |>
  rbind(state_data) |>
  arrange(desc(perc_missing))

overall_data <- historical_markers |>
  select(missing) |>
  mutate(total_markers = n()) |>
  filter(!is.na(missing)) |>
  reframe(perc_missing = round(100 * n() / total_markers, 0)) |>
  distinct() |>
  pull()

# start recording
gg_record(
  dir = file.path("2023", "2023-07-04", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 14, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
bg_col <- nrBrand::nr_light
dark_col <- nrBrand::nr_dark
highlight_col <- nrBrand::nr_mid

# text
title <- "Missing Markers"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = dark_col,
  font_family = "Commissioner"
)
st <- "National and global events all happened somewhere, and historical markers 
mark where many occurred. The Historical Marker Database keeps track of where
such markers are located. It includes some markers have been reported (and 
sometimes confirmed) as missing. Some states have far more markers labelled as 
missing than others."
cap <- paste0("**Data**: The Historical Marker Database<br>", social)

# additional data
plot_edges <- plot_data |>
  mutate(
    theta = seq(pi / 4, (7 / 4) * pi, length.out = nrow(plot_data)),
    x = 10 * cos(theta),
    y = 10 * sin(theta),
    labx = 10.5 * cos(theta),
    laby = 10.5 * sin(theta),
    angle = 180 + 360 * (theta / (2 * pi))
  )

waffle_data <- expand.grid(x = -11:-2, y = -16:-25) |>
  as_tibble() |>
  mutate(colour = c(rep(highlight_col, overall_data),
                    rep(dark_col, 100 - overall_data)),
         alpha = c(rep(1, overall_data),
                   rep(0.4, 100 - overall_data)))

# plot
ggplot() +
  geom_segment(
    data = filter(plot_edges, perc_missing != 0),
    mapping = aes(
      x = x, y = y, xend = 0, yend = 0, linewidth = perc_missing
    ),
    colour = highlight_col
  ) +
  geom_point(
    data = plot_edges,
    mapping = aes(
      x = x, y = y
    ),
    colour = highlight_col,
    size = 2.5
  ) +
  geom_point(
    data = data.frame(),
    mapping = aes(
      x = 0, y = 0
    ),
    colour = highlight_col,
    size = 6
  ) +
  geom_text(
    data = plot_edges,
    mapping = aes(
      x = labx, y = laby, angle = angle,
      label = paste0("(", perc_missing, "%) ", state_or_prov),
    ),
    colour = dark_col,
    family = "Commissioner",
    hjust = 1,
    size = 8
  ) +
  geom_textbox(data = data.frame(),
            mapping = aes(x = 7.2, y = -3,
                          label = "Washington D.C. has almost 15% of its listed
                          markers either reported or confirmed to be missing - more
                           than double that of West Virginia in second place."),
            size = 15,
            family = "Commissioner",
            lineheight = 0.5,
            fill = bg_col,
            box.colour = bg_col,
            width = unit(3, "inch"),
            colour = dark_col) +
  geom_text(
    data = waffle_data,
    mapping = aes(
      x = x, y = y, colour = colour,
      label = fontawesome("fa-map-marker"), alpha = alpha
    ),
    size = 16,
    family = "fontawesome-webfont"
  ) +
  scale_x_continuous(limits = c(-13, 13)) +
  scale_y_continuous(limits = c(-25, 13)) +
  scale_linewidth_continuous(range = c(0.1, 4)) +
  scale_colour_identity() +
  scale_alpha_identity() +
  coord_fixed() +
  labs(title = title,
       subtitle = st,
       caption = cap,
       tag = glue::glue("Almost <span style='color:{highlight_col}'>{overall_data}%</span> of historical markers in the USA
                      have been reported or confirmed missing. This relates to a total of <span style='color:{highlight_col}'>
                     {sum(!is.na(historical_markers$missing))}</span> objects.")) +
  theme_void(base_size = 44, base_family = "Commissioner") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(0, 20, 10, 20),
    plot.title = element_textbox_simple(
      family = "Fraunces",
      colour = dark_col,
      hjust = 0,
      size = 100,
      margin = margin(t = 20)
    ),
    plot.subtitle = element_textbox_simple(
      family = "Commissioner",
      colour = dark_col,
      hjust = 0,
      lineheight = 0.5,
      margin = margin(t = 20, b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = "Commissioner",
      colour = dark_col,
      hjust =  0,
      lineheight = 0.5,
      margin = margin(t = 10)
    ),
    plot.tag.position = c(0.55, 0.16),
    plot.tag = element_textbox_simple(
      lineheight = 0.55,
      maxwidth = 0.6,
      margin = margin(t = 0),
      colour = dark_col,
      hjust = 0,
      halign = 0,
      size = 42
    )
    )

# save gif
gg_playback(
  name = file.path("2023", "2023-07-04", "20230704.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)









