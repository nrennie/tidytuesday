library(tidyverse)
library(showtext)
library(sf)
library(usefunc)
library(rcartocolor)
library(camcorder)

# load fonts
font_add_google("Cabin Sketch", "cabin")
font_add_google("Lato", "lato")
showtext_auto()

# load data
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

# % of workforce in the arts per state
workers_by_state <- artists %>%
  select(-c("artists_share", "location_quotient")) %>%
  group_by(state, type) %>%
  summarise(total_workers = sum(all_workers_n, na.rm = TRUE)) %>%
  select(-type) %>%
  distinct()

plot_data <- artists %>%
  select(-c("artists_share", "location_quotient")) %>%
  group_by(state) %>%
  summarise(total_artists = sum(artists_n, na.rm = TRUE)) %>%
  left_join(workers_by_state, by = "state") %>%
  mutate(artists_share = total_artists/total_workers,
         artists_share = 100*artists_share)

# Download the hexagons here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
hex_data <- geojson_sf("2022/2022-09-27/us_states_hexgrid.geojson")
hex_data <- hex_data %>%
  mutate(google_name = stringr::str_extract(google_name, pattern = "^[^\\(]+"),
         google_name = stringr::str_trim(google_name)) %>%
  left_join(plot_data, by = c("google_name" = "state")) %>%
  sf::st_drop_geometry()

# bin colours
artists_share_bin <- cut(hex_data$artists_share,
                         breaks = c(0, 0.5, 1, 1.5, 2.0, 2.5, Inf),
                         labels = c("0 - 0.5", "0.5 - 1", "1 - 1.5", "1.5 - 2", "2 - 2.5", "> 2.5"),
                         include.lowest = TRUE )
hex_data$artists_share_bin <- artists_share_bin

# fix map crs
us <- geojson_read("2022/2022-09-27/us_states_hexgrid.geojson",  what = "sp")
us_map <- fortify(us, region="iso3166_2")

us_map <- us_map %>%
  left_join(hex_data, by = c("id" = "iso3166_2"))

# get centres
centres <- sf::st_centroid(hex_data)
centres_text <- as.data.frame(sf::st_coordinates(centres))
centres_text$iso3166_2 <- centres$iso3166_2

# define subtitle
st <- usefunc::str_wrap_break("In the USA, the District of Columbia has the highest percentage of the workforce in the arts, followed by New York. Mississippi has the fewest.", 100)

# start recording
gg_record(
  dir = file.path("2022", "2022-09-27", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 5, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# plot
ggplot() +
  geom_map(data = us_map,
           mapping = aes(map_id = id,
                         x = long,
                         y = lat,
                         fill = artists_share_bin),
           map = us_map,
           colour = "#f5f5f2",
           size = 1.5) +
  coord_map() +
  geom_text(data = centres_text,
            mapping = aes(x = X,
                          y = Y,
                          label = iso3166_2),
            size = 12,
            family = "lato") +
  scale_fill_carto_d(palette = "SunsetDark",
                     direction = 1,
                     guide = guide_legend(keyheight = unit(3, units = "mm"),
                                          keywidth = unit(12, units = "mm"),
                                          label.position = "bottom",
                                          title.position = 'top',
                                          nrow = 1,
                                          title = "% of workers in the arts")) +
  labs(title = "Arts in the USA",
       subtitle = st,
       caption = "N. Rennie | Data: arts.gov") +
  theme(plot.background = element_rect(fill = "#f5f5f2",
                                       colour = "#f5f5f2"),
        panel.background = element_rect(fill = "#f5f5f2",
                                        colour = "#f5f5f2"),
        legend.background = element_rect(fill = "transparent",
                                         colour = "transparent"),
        plot.title = element_text(hjust = 0.5,
                                  size = 80,
                                  family = "cabin"),
        plot.subtitle = element_text(hjust = 0.5,
                                     family = "lato",
                                     size = 30,
                                     lineheight = 0.5),
        plot.caption = element_text(hjust = 0.5,
                                    family = "lato",
                                    size = 30),
        legend.title = element_text(hjust = 0.5,
                                    family = "lato",
                                    size = 30,
                                    margin = margin(b = -10)),
        legend.text = element_text(hjust = 0.5,
                                   family = "lato",
                                   size = 30,
                                   lineheight = 0.1,
                                   margin = margin(t = -10)),
        legend.position = c(0.5, 0.9),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"))

# save gif
gg_playback(
  name = file.path("2022", "2022-09-27","20220927.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)
