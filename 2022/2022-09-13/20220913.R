library(tidyverse)
library(showtext)
library(camcorder)
library(usefunc)

# load data
bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')

# load fonts
font_add_google("Creepster", "creepster")
font_add_google("Alegreya Sans", "alegreya")
showtext_auto()

# data prep
plot_data <- bigfoot %>%
  select(moon_phase, date) %>%
  drop_na() %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(phase = case_when(
    moon_phase > 0 & moon_phase <= 0.25 ~ "Moon phase:\n0 - 0.25",
    moon_phase > 0.25 & moon_phase <= 0.5 ~ "Moon phase:\n0.25 - 0.5",
    moon_phase > 0.5 & moon_phase <= 0.75 ~ "Moon phase:\n0.5 - 0.75",
    moon_phase > 0.75 & moon_phase <= 1 ~ "Moon phase:\n0.75 - 1",
    TRUE ~ "Moon phase:\n0 - 0.25"
  )) %>%
  select(phase, year) %>%
  group_by(year, phase) %>%
  summarise(n = n())

# captions
caps <- bigfoot %>%
  select(observed) %>%
  filter(str_detect(observed, "moon")) %>%
  pull(observed)

# subtitle
st <- str_wrap_break("Bigfoot, also commonly referred to as Sasquatch, is an ape-like creature said to roam the forests of North America. Many claim to have seen Bigfoot...\n\nThe plots below show the number of bigfoot sightings per year for each phase of the moon. In recent years, the number of bigfoot sightings has dropped.", 80)

# tag
tg <- str_wrap_break('"We all three started howling at the moon. When we stopped, something howled back at us..."', 40)

# start recording
gg_record(
  dir = file.path("2022", "2022-09-13", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8, # width of saved image
  height = 8, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# plot
ggplot(data = plot_data,
       mapping = aes(x = factor(year), y = n)) +
  geom_col(fill = "white") +
  facet_wrap(~phase, nrow = 1) +
  coord_polar(theta = "y") +
  scale_x_discrete(breaks = c("1950", "1980", "2010"),
                   labels = rev) +
  labs(title = "Bigfoot Sightings",
       subtitle = st,
       tag = tg,
       caption = "N. Rennie | Data: Bigfoot Field Researchers Organization") +
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        strip.background = element_rect(fill = "black", colour = "black"),
        panel.grid = element_blank(),
        strip.text = element_text(colour = "white",
                                  family = "alegreya",
                                  hjust = 0.5,
                                  size = 36,
                                  lineheight = 0.4),
        plot.title = element_text(colour = "white",
                                  family = "creepster",
                                  size = 100,
                                  hjust = 0.5,
                                  margin = margin(t = 10, b = 30)),
        plot.subtitle = element_text(colour = "white",
                                     family = "alegreya",
                                     hjust = 0.5,
                                     size = 40,
                                     lineheight = 0.4,
                                     margin = margin(t = 10, b = 50)),
        plot.caption = element_text(colour = "white",
                                    family = "alegreya",
                                    hjust = 0.5,
                                    size = 30,
                                    margin = margin(t = 140, b = 10)),
        plot.tag = element_text(colour = "white",
                                    family = "alegreya",
                                    hjust = 0.5,
                                    size = 50,
                                    lineheight = 0.4,
                                    margin = margin(t = 10, b = 10)),
        plot.tag.position = c(0.52, 0.2),
        axis.text = element_text(colour = "#fafafa",
                                 family = "alegreya",
                                 hjust = 0.5,
                                 size = 26),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 1.5, 0.5, 0.5), unit = "cm"))

# save gif
gg_playback(
  name = file.path("2022", "2022-09-13","20220913.gif"),
  first_image_duration = 8,
  last_image_duration = 12,
  frame_duration = .25
)
