library(tidyverse)
library(showtext)
library(camcorder)
library(ggforce)
library(geofacet)
library(ggtext)
library(rcartocolor)

# load fonts
font_add_google("Raleway", "raleway")
font_add_google("Kelly Slab", "kelly")
showtext_auto()

# load data
state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')
station_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/station_info.csv')

# data wrangling
plot_data <- state_stations %>% 
  select(state, format) %>% 
  group_by(state) %>% 
  mutate(n = n()) %>% 
  filter(str_detect(format, pattern = "Religious|Religion")) %>% 
  mutate(n_rel = n()) %>% 
  select(-format) %>% 
  distinct() %>% 
  mutate(prop = 100*n_rel/n) %>% 
  select(state, prop) %>% 
  mutate(state = str_replace(state, "_", " ")) %>% 
  left_join(geofacet::us_state_grid2, by = c("state" = "name"))

# start recording
gg_record(
  dir = file.path("2022", "2022-11-08", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- "Montana is the US state with the highest proportion of radio<br>stations whose format contains the word *religion* or *religious*,<br>with New Jersey a close second."

# plot
ggplot(data = plot_data) +
  geom_circle(mapping = aes(x0 = col + 0,
                            y0 = row + 0,
                            r = 0.4,
                            fill = prop),
              size = 0.2) +
  geom_arc_bar(mapping = aes(x0 = col + 0,
                             y0 = row + 0,
                             r0 = 0,
                             r = 0.4,
                             start = 0,
                             end = (2*pi*0.01)*prop),
               fill = "black",
               size = 0.2) +
  geom_spoke(mapping = aes(x = col + 0, 
                           y = row + 0,
                           angle = (pi/2) - (2*pi*0.01)*prop),
             radius = 0.4,
             colour = "white",
             size = 0.3) +
  geom_text(mapping = aes(x = col + 0,
                          y = row + 0.2,
                          label = code,
                          colour = (code %in% c("NJ", "MT"))), 
            fontface = "bold",
            family = "kelly",
            size = 6) +
  scale_fill_carto_c(palette = "BrwnYl") +
  scale_colour_manual(values = c("black", "white")) +
  scale_y_reverse() +
  coord_fixed() +
  labs(title = "Religious Radio in the USA", 
       subtitle = st,
       caption = "N. Rennie | Data: Wikipedia | #TidyTuesday") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#eadeca", colour = "#eadeca"),
        panel.background = element_rect(fill = "#eadeca", colour = "#eadeca"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, margin = margin(t = 10), family = "kelly", size = 40),
        plot.subtitle = element_markdown(hjust = 0.5, margin = margin(t = 10), family = "raleway", size = 20, lineheight = 0.5),
        plot.caption = element_text(hjust = 0.5, margin = margin(b = 10), family = "raleway", size = 20))

# save gif
gg_playback(
  name = file.path("2022", "2022-11-08","20221108.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)
