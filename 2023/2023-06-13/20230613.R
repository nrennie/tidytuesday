library(tidyverse)
library(lubridate)
library(showtext)
library(camcorder)
library(ggtext)
library(glue)
library(cowplot)
library(magick)
library(grid)

# load fonts
font_add_google("Ubuntu", "Ubuntu")
font_add_google("Chelsea Market", "Chelsea")
showtext_auto()

# read in data
safi_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv")

# prep data
plot_data <- safi_data %>%
  select(items_owned) %>%
  mutate(total = nrow(.)) %>%
  separate_longer_delim(items_owned, delim = ";") %>%
  filter(items_owned %in%
           c("mobile_phone", "bicycle", "fridge", "television")) %>%
  group_by(items_owned) %>%
  mutate(
    num_item = n(),
    perc = 100 * num_item / total,
    perc = round(perc / 5) * 5,
    num_bars = perc / 5
  ) |>
  select(items_owned, num_bars) |>
  distinct() |>
  mutate(
    items_owned = str_replace(items_owned, "_", " "),
    items_owned = str_to_title(items_owned)
  ) |>
  ungroup() |>
  arrange(desc(num_bars)) |>
  mutate(items_owned = factor(items_owned, levels = items_owned))


bg_tiles <- data.frame(
  x = rep(1:20, times = 4),
  y = 1,
  items_owned = factor(rep(levels(plot_data$items_owned), each = 20),
                       levels = levels(plot_data$items_owned))
)

fill_tiles <- data.frame(
  x = unlist(purrr::map(.x = plot_data$num_bars, .f = ~seq_len(.x))),
  y = 1,
  items_owned = factor(unlist(purrr::map2(.x = plot_data$num_bars,
                                          .y = plot_data$items_owned,
                                          .f = ~rep(.y, each = .x))),
                       levels = levels(plot_data$items_owned))
)

# start recording
gg_record(
  dir = file.path("2023", "2023-06-13", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# colours
dark_col <- "#5c3633"
highlight_col <- "#f5e60f"

title <- "SAFI Survey"
social <- nrBrand::social_caption(
  bg_colour = "#ffffff00",
  icon_colour = highlight_col,
  font_colour = highlight_col,
  font_family = "Ubuntu"
)
st <- "SAFI (Studying African Farmer-Led Irrigation) is a currently running project 
which is looking at farming and irrigation methods. The survey data was collected
through interviews, and relates to households in Tanzania and Mozambique. Information collected 
on items owned by the household shows that just over 65% of households own a mobile phone. "
cap <- paste0("**Data**: SAFI Survey <br>**Image**: safi-research.org <br>", social)

# plot
g1 <- ggplot() +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(expand = F) +
  theme_minimal(
    base_size = 33,
    base_family = "Ubuntu"
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    panel.background = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    plot.title = element_textbox_simple(
      family = "Chelsea",
      face = "bold",
      size = 58,
      margin = margin(t = 0, b = 15),
      colour = highlight_col,
      hjust = 0,
      halign = 0,
      lineheight = 0.4,
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 0.5,
      colour = highlight_col,
      margin = margin(b = 20),
      halign = 0
    ),
    plot.caption = element_textbox_simple(
      lineheight = 0.55,
      colour = highlight_col,
      hjust = 0,
      halign = 0
    ),
    axis.text.x = element_text(family = "Chelsea"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(dark_col, 0.3),
      linewidth = 0.5,
    ),
    plot.margin = margin(5, 15, 5, 10),
  )

g2 <- ggplot(plot_data) +
  geom_tile(
    data = bg_tiles,
    mapping = aes(
      x = x, y = y
    ),
    height = 1,
    width = 0.8,
    fill = alpha(dark_col, 0.4)
  ) +
  geom_tile(
    data = fill_tiles,
    mapping = aes(
      x = x, y = y
    ),
    height = 1,
    width = 0.8,
    fill = dark_col
  ) +
  facet_wrap(~items_owned, ncol = 1) +
  coord_cartesian(expand = FALSE) +
  theme_void(base_size = 36) +
  theme(
    plot.margin = margin(5, 15, 5, 10),
    strip.text = element_text(
      hjust = 0, colour = dark_col, family = "Chelsea", margin = margin(b = 2)
    )
  )


# bg image
img <- image_read("2023/2023-06-13/bg.jpg")
rect <- rectGrob(
  x = unit(0, "in"),
  y = unit(4, "in") - unit(0, "in"),
  width = unit(8, "in"),
  height = unit(4, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = dark_col, alpha = 0.78)
)
rect2 <- rectGrob(
  x = unit(4, "in"),
  y = unit(4, "in") - unit(0.25, "in"),
  width = unit(3.75, "in"),
  height = unit(3.5, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fdfacf", alpha = 0.6)
)
ggdraw() +
  draw_image(img) +
  draw_grob(rect) +
  draw_grob(rect2) +
  draw_plot(g1, .05, .05, .4, .9) +
  draw_plot(g2, .5, .08, .47, .85)

# save gif
gg_playback(
  name = file.path("2023", "2023-06-13", "20230613.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
