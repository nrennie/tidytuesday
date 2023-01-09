library(tidyverse)
library(showtext)
library(camcorder)
library(sf)
library(spatstat)
library(raster)
library(terra)
library(tidyterra)
library(maps)

# load fonts
font_add_google("Roboto Slab", "slab")
font_add_google("Roboto", "roboto")
showtext_auto()

# load data
feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

# data wrangling
plot_data <- feederwatch |> 
  select(loc_id, latitude, longitude, how_many) |> 
  group_by(loc_id) |> 
  mutate(how_many = mean(how_many, na.rm = TRUE)) |> 
  distinct() |> 
  ungroup() |> 
  select(latitude, longitude, how_many) |> 
  drop_na()

# get bg map
usa_map <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
usa_map <- subset(counties, grepl("california", counties$ID))
bb <- st_bbox(usa_map)
usa_map$label = "Bird sightings in California"

# interpolate
obs_window <- owin(bb[c(1,3)], bb[c(2,4)])
ppp_birds <- ppp(plot_data$longitude,
                 plot_data$latitude,
                 marks = plot_data$how_many,
                 window = obs_window)
idw_birds <- idw(ppp_birds, power=0.05, at="pixels")
sp_idw_birds <- spatstat.geom::as.data.frame.im(idw_birds) |> 
  as_tibble()

# crop to area
obj_raster <- rast(sp_idw_birds)
obj_raster_mask <- crop(obj_raster, vect(usa_map)) %>%
  mask(vect(usa_map))

# points data
points_data <- feederwatch |> 
  separate(subnational1_code, into = c("country", "state"), sep = "-") |> 
  filter(state == "CA") |> 
  select(latitude, longitude) |> 
  distinct()

# start recording
gg_record(
  dir = file.path("2023", "2023-01-10", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- "This maps shows the average number of reported bird sightings for each observation site, with each site indicated by a white dot.\n\nValues are interpolated using Inverse Distance Weighting which applies a spatial smoothing of numeric values observed at a set of irregular locations. The interpolation includes observations from neighbouring states.\n\nN. Rennie | Data: Project FeederWatch"

# plot
ggplot() +
  geom_spatraster(data = obj_raster_mask) +
  geom_point(data = points_data,
             mapping = aes(x = longitude,
                           y = latitude),
             colour = "white",
             size = 0.05) +
  geom_sf(data = usa_map, fill = "transparent", linewidth = 0.1, colour = "#d9e2f1") +
  scale_fill_viridis_c(na.value = "#d9e2f1",
                       limits = c(3.79, 3.83),
                       breaks = c(3.795, 3.825),
                       labels = c("Fewer sightings", "More sightings"),
                       direction = -1
  ) +
  annotate("text", 
           x = -118,
           y = 40,
           label = str_wrap("FeederWatch is a November-April survey of birds that visit backyards, nature centers, community areas, and other locales in North America. Citizen scientists could birds in areas with plantings, habitat, water, or food that attracts birds.",
                           40),
           family = "roboto",
           lineheight = 0.4,
           size = 8.4,
           hjust = 0,
           colour = "#4a4081") +
  coord_sf(xlim = c(-133, -109.5)) +
  facet_wrap(~label) +
  labs(tag = usefunc::str_wrap_break(st, 40)) +
  guides(fill = guide_colourbar(ticks = FALSE)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#d9e2f1", colour = "#d9e2f1"),
        panel.background = element_rect(fill = "#d9e2f1", colour = "#d9e2f1"),
        strip.text = element_text(family = "slab", lineheight = 0.4, hjust = 0,
                                  colour = "#d9e2f1", size = 48,
                                  margin = margin(t = 10, 
                                                  l = 10, 
                                                  b = 10)),
        strip.background = element_rect(fill = "#4a4081", colour = "#4a4081"),
        plot.tag.position = c(0.03, 0.33),
        plot.tag = element_text(family = "roboto",lineheight = 0.4, size = 24,
                                colour = "#4a4081", hjust = 0),
        legend.text = element_text(family = "roboto", lineheight = 0.4,
                                   size = 24, hjust = 0.5,
                                   colour = "#4a4081"),
        legend.key.width = unit(1.5,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.position = c(0.6, -0.05),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(-5,0,30,0))

# save gif
gg_playback(
  name = file.path("2023", "2023-01-10","20230110.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
