library(tidyverse)
library(showtext)
library(camcorder)
library(MetBrewer)

# load fonts
font_add_google("Roboto Slab", "slab")
font_add_google("Roboto", "roboto")
showtext_auto()

# load data
pop <- readr::read_csv("2023/2023-01-03/data/population_aged_70plus_years_both_sexes_percent.csv")
doctors <- readr::read_csv("2023/2023-01-03/data/medical_doctors_per_1000_people.csv")

# data wrangling
pop <- pop |> 
  pivot_longer(-country, values_to = "pop", names_to = "year")
doctors <- doctors |> 
  pivot_longer(-country, values_to = "doctors", names_to = "year")
plot_data <- pop |> 
  full_join(doctors, by = c("country", "year")) |> 
  mutate(ratio = doctors/pop) |> 
  drop_na() |> 
  group_by(country) |> 
  slice_max(year)

# world map
world <- map_data("world")
sort(unique(world$region))
sort(unique(plot_data$country))
plot_data <- plot_data %>% #old = new
  mutate(country = 
           recode(country, 
                  "United Kingdom" = "UK", 
                  "United States" = "USA",
                  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                  "Cote d'Ivoire" = "Ivory Coast",
                  "Congo, Rep." = "Republic of Congo")) |> 
  ungroup()
map_data <- left_join(world, plot_data, by = c("region" = "country")) |> 
  filter(region != "Antarctica")
map_data$label = "Doctors in an ageing population"
  
# start recording
gg_record(
  dir = file.path("2023", "2023-01-03", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 4, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# subtitle
st <- "This map show the number of doctors per thousand people, rescaled by the percentage of the population aged over 70, revealing which countries* may be more likely to struggle in providing care for an elderly population.\n\n* using most recent available data for each country." 

# plot
ggplot(data = map_data,
       mapping = aes(long,
                     lat,
                     map_id = region,
                     fill = log10(ratio))) +
  geom_map(map = map_data,
           color = "#EADEDA",
           size = 0.05) +
  scale_y_continuous(limits = c(-60, 120)) +
  scale_fill_gradientn(colours = met.brewer("Hokusai2", n = 20),
                       limits = c(-1.8, 0.7),
                       breaks = c(-1.6, 0.5),
                       labels = c("Fewer doctors", "More doctors")
                       ) +
  facet_wrap(~label) +
  labs(tag = usefunc::str_wrap_break(st, 120),
       caption = "N. Rennie | Data: Gapminder") +
  guides(fill = guide_colourbar(ticks = FALSE)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#EADEDA", colour = "#EADEDA"),
        panel.background = element_rect(fill = "#EADEDA", colour = "#EADEDA"),
        strip.text = element_text(family = "slab", lineheight = 0.4, hjust = 0,
                                  colour = "#EADEDA", size = 48,
                                  margin = margin(t = 10, 
                                                  l = 10, 
                                                  b = 10)),
        strip.background = element_rect(fill = "#1B5681", colour = "#1B5681"),
        plot.tag.position = c(0.03, 0.78),
        plot.tag = element_text(family = "roboto",lineheight = 0.4, size = 24,
                                colour = "#0E3F62", hjust = 0),
        plot.caption = element_text(family = "roboto", lineheight = 0.4,
                                    size = 24, hjust = 0.03,
                                    colour = "#0E3F62"),
        legend.text = element_text(family = "roboto", lineheight = 0.4,
                                   size = 24, hjust = 0.5,
                                   colour = "#0E3F62"),
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.position = c(0.645, -0.005),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        plot.margin = margin(0,0,10,0))

# save gif
gg_playback(
  name = file.path("2023", "2023-01-03","20230103.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)

