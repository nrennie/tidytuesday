library(tidyverse)
library(showtext)
library(camcorder)
library(sf)
library(osmdata)
library(ggmap)
library(rcartocolor)
library(cowplot)

# load data
elevators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv')

# load fonts
font_add_google("Fraunces")
font_add_google("Commissioner")
showtext_auto()

# wrangling data
plot_data <- elevators %>%
  filter(`Device Status` == "A",
         `Device Type` == "Passenger Elevator (P)") %>%
  filter(LONGITUDE >= -77.5) %>%
  select(c(DV_CAPACITY_LBS, LATITUDE, LONGITUDE)) %>%
  drop_na() %>%
  mutate(cap = cut(DV_CAPACITY_LBS,
                   breaks = seq(0, 60000, 10000),
                   labels = paste0("<", seq(10, 60, 10)),
                   include.lowest = TRUE)) %>%
  arrange(cap) %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(crs = 4326)

# start recording
gg_record(
  dir = file.path("2022", "2022-12-06", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Define geographic area
getbb("New York City")
bb <- matrix(c(-74.252, -73.707, 40.502, 40.914),
             ncol = 2,
             nrow = 2,
             byrow = TRUE,
             dimnames = list(c("x", "y"), c("min", "max")))

# Get a background map
bg_map <- get_map(bb,
                  source = "stamen",
                  maptype = "toner-hybrid",
                  color = "bw")

# plot
p <- ggmap(bg_map) +
  geom_sf(data = plot_data,
          size = 0.2,
          inherit.aes = FALSE,
          mapping = aes(colour = cap)) +
  labs(tag = "NEW YORK CITY\nELEVATORS") +
  scale_colour_carto_d(
    palette = "RedOr",
    direction = -1,
    name = "") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 145),
        plot.tag = element_text(family = "Fraunces", face = "bold",
                                size = 40, lineheight = 0.4,
                                colour = "white"),
        plot.tag.position = c(-0.26, 0.8),
        legend.position = "none",
        plot.background = element_rect(fill = "#36454f", colour = "#36454f"),
        legend.background = element_rect(fill = "#36454f", colour = "#36454f"),
        panel.background = element_rect(fill = "white", colour = "white"))

# legend
width <- 6
ybar_lim <- c(0.95, 1.25)
xbar_lim <- c(5 - width/2, 5 + width/2)
breaks <- 7
xbar_breaks <- seq(xbar_lim[1], xbar_lim[2], length.out = breaks)
scale_data <- purrr::map_df(1:(length(xbar_breaks) - 1),
                            .f = ~{
                              data.frame(x = xbar_breaks[.x],
                                         xend = xbar_breaks[.x + 1],
                                         y = ybar_lim[1],
                                         yend = ybar_lim[2],
                                         i = .x)
                            })
text_data <- scale_data %>%
  slice(-1) %>%
  mutate(label = seq(10000, 50000, by = 10000))
xlim <- c(0, 10)
ylim <- c(0, 1.7)
legend <- ggplot() +
    geom_rect(aes(xmin = xlim[1], xmax = xlim[2],
                  ymin = ylim[1], ymax = ylim[2]),
              colour = NA, fill = "#36454f") +
    geom_rect(data = scale_data,
              aes(xmin = x, xmax = xend,
                  ymin = y, ymax = yend, fill = i),
              colour = "#36454f", linewidth = 1) +
    geom_text(data = text_data,
              aes(x = x, y = yend + 0.4,
                  label = label, family = "Commissioner"),
              colour = "white", size = 7) +
    geom_text(aes(x = 10, y = 0.8),
              label = str_wrap("Capacity (lbs) of active passenger elevators in New York City", 24),
              size = 7, family = "Commissioner", lineheight = 0.4,
              colour = "white") +
    scale_x_continuous(limits = xlim, expand = c(0, 0)) +
    scale_y_continuous(limits = ylim, expand = c(0, 0)) +
    scale_fill_carto_c(palette = "RedOr", direction = -1) +
    coord_polar(start = 3.14) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0),
          legend.position = "none",
          plot.background = element_rect(fill = "#36454f", colour = "#36454f"),
          legend.background = element_rect(fill = "#36454f", colour = "#36454f"),
          panel.background = element_rect(fill = "#36454f", colour = "#36454f"))

# join together
ggdraw() +
  draw_plot(p, x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5,
            width = 1, height = 1) +
  draw_plot(legend, x = 0.16, y = 0.4, vjust = 0.5, hjust = 0.5,
            width = 0.5, height = 0.5)

# save gif
gg_playback(
  name = file.path("2022", "2022-12-06","20221206.gif"),
  first_image_duration = 4,
  last_image_duration = 25,
  frame_duration = .25
)


