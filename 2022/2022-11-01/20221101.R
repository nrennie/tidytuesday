library(tidyverse)
library(showtext)
library(camcorder)
library(lubridate)

# load fonts
font_add_google("Creepster", "creepster")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()

# load data
horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

# data wrangling
plot_data <- horror_movies %>% 
  mutate(year = year(release_date), 
         decade = factor(floor(year / 10) * 10), 
         year = factor(year)) %>% 
  filter(decade == "1980") %>% 
  select(vote_average, release_date, decade, year) %>% 
  mutate(week = str_c(formatC(isoweek(release_date), format = "f", digits = 0, width = 2, flag = "0"))) %>% 
  mutate(week_day = wday(release_date, week_start = 1)) %>% 
  complete(week, week_day) %>% 
  filter(!is.na(year)) %>% 
  select(week, week_day, vote_average, year) %>% 
  group_by(year, week, week_day) %>% 
  summarise(vote_average = mean(vote_average))
plot_data

# start recording
gg_record(
  dir = file.path("2022", "2022-11-01", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 10, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

#subtitle
st <- "The Movie Database lists 2,563 horror movies released in the 1980s. This plot shows the average ratings of movies released each week throughout the decade with brighter colours indicating higher ratings."

# plot
ggplot(data = plot_data) +
  geom_raster(mapping = aes(x = week, y = week_day, fill = vote_average)) +
  coord_fixed() +
  facet_wrap(~year, ncol = 1) +
  scale_y_reverse(breaks = 7:1,
                  labels = rev(c("Mon", "", "Wed", "", "Fri", "", "Sun"))) +
  scale_fill_distiller(palette = "OrRd", na.value = "#707070") +
  labs(title = "1980s Horror Movies", 
       subtitle = str_wrap(st, 61), 
       caption = "N. Rennie | Data: The Movie Datbase") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 30, colour = "white", family ="ubuntu"),
        plot.title = element_text(size = 60, colour = "white", margin = margin(t = 10), family = "creepster"),
        plot.background = element_rect(colour = "black", fill = "black"),
        panel.background = element_rect(colour = "black", fill = "black"),
        strip.background = element_rect(colour = "black", fill = "black"),
        strip.text = element_text(size = 30, colour = "white", hjust = 0, family ="ubuntu"),
        plot.subtitle = element_text(size = 30, colour = "white", lineheight = 0.3, family ="ubuntu", margin = margin(t = 10, b = 5)),
        plot.caption = element_text(size = 30, colour = "white", hjust = 0, family ="ubuntu"),
        panel.grid = element_blank())

# save gif
gg_playback(
  name = file.path("2022", "2022-11-01","20221101.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)
